use std::collections::{HashMap, HashSet};

use fea_rs_ast::{
    AsFea, FeatureFile, GlyphContainer, LookupBlock, Metric, PairPosStatement, Statement,
    ToplevelItem,
};
use indexmap::IndexMap;
use smol_str::SmolStr;

use crate::{Features, Font, filters::FontFilter, layout::expand::expand_glyph_container};

use super::feature_code::{
    INSERTION_MARKER, lookup_reference_count, lookups_by_section, remove_lookup_blocks,
};

/// A filter that moves explicit FEA-based kerning rules into the kerning table
///
/// It moves the pair kerning lookups of the `kern` feature: lookups the feature
/// applies under every script and language it is registered for, holding only
/// pair adjustments of the advance. Glyph pairs become kerning pairs, class
/// pairs become kerning between kern groups, read as a shaper applies the
/// lookup (the first subtable that covers a glyph is the one that kerns it).
/// Variable values are evaluated at each master. Anything else, such as
/// contextual kerning, stays feature code.
#[derive(Default)]
pub struct MoveKerningFromFeatures;

impl MoveKerningFromFeatures {
    /// Create a new MoveKerningFromFeatures filter
    pub fn new() -> Self {
        MoveKerningFromFeatures
    }
}

/// IgnoreMarks; the only lookup flag the kerning model can stand for
const IGNORE_MARKS: u16 = 0x0008;

/// One side of a pair: a glyph, or a class of glyphs
#[derive(Clone, Debug, PartialEq)]
enum Side {
    Glyph(SmolStr),
    Class(Option<SmolStr>, Vec<SmolStr>),
}

impl Side {
    fn glyphs(&self) -> Vec<SmolStr> {
        match self {
            Side::Glyph(glyph) => vec![glyph.clone()],
            Side::Class(_, glyphs) => glyphs.clone(),
        }
    }
}

/// The kerning one lookup holds: pairs of sides with their values, in the
/// order a shaper would find them
#[derive(Default, Debug)]
struct LookupKerning {
    pairs: Vec<(Side, Side, Metric)>,
    first_glyphs: HashSet<SmolStr>,
}

struct Reader<'a> {
    font: &'a Font,
    classes: HashMap<SmolStr, Vec<SmolStr>>,
}

impl Reader<'_> {
    fn expand(&self, container: &GlyphContainer) -> Vec<SmolStr> {
        let mut glyphs = expand_glyph_container(self.font, container, &self.classes, None);
        let mut seen = HashSet::new();
        glyphs.retain(|g| seen.insert(g.clone()));
        glyphs
    }

    fn side(&self, container: &GlyphContainer) -> Side {
        match container {
            GlyphContainer::GlyphName(name) => Side::Glyph(name.name.clone()),
            GlyphContainer::GlyphClassName(name) => Side::Class(
                Some(name.trim_start_matches('@').into()),
                self.expand(container),
            ),
            _ => Side::Class(None, self.expand(container)),
        }
    }

    /// The kerning a pair states: an x advance, or, as kerning is written for a
    /// right-to-left script, an x placement and an x advance of one value
    fn advance(statement: &PairPosStatement) -> Option<Metric> {
        if statement.value_record_2.is_some() {
            return None;
        }
        let record = &statement.value_record_1;
        let unset = |metric: &Option<Metric>| match metric {
            None => true,
            Some(Metric::Scalar(v)) => *v == 0,
            Some(_) => false,
        };
        let right_to_left = record.x_placement.is_some()
            && record.x_placement == record.x_advance
            && !unset(&record.x_placement);
        if (!unset(&record.x_placement) && !right_to_left)
            || !unset(&record.y_placement)
            || !unset(&record.y_advance)
            || record.x_placement_device.is_some()
            || record.y_placement_device.is_some()
            || record.x_advance_device.is_some()
            || record.y_advance_device.is_some()
        {
            return None;
        }
        record.x_advance.clone()
    }

    /// The kerning of a lookup, or None if the kerning model cannot hold it
    fn read_lookup(&self, lookup: &LookupBlock) -> Option<LookupKerning> {
        let mut segments: Vec<Vec<&PairPosStatement>> = vec![vec![]];
        for statement in &lookup.statements {
            match statement {
                Statement::PairPos(pair) => segments.last_mut()?.push(pair),
                Statement::Subtable(_) => segments.push(vec![]),
                Statement::LookupFlag(flag) => {
                    if flag.value & !IGNORE_MARKS != 0
                        || flag.mark_attachment.is_some()
                        || flag.mark_filtering_set.is_some()
                    {
                        return None;
                    }
                }
                Statement::Comment(_) => {}
                _ => return None,
            }
        }
        let mut kerning = LookupKerning::default();
        // glyphs a class subtable has kerned: later subtables never see them
        let mut claimed: HashSet<SmolStr> = HashSet::new();
        let mut seen_pairs: HashSet<(SmolStr, SmolStr)> = HashSet::new();
        for segment in segments {
            // A compiler puts a subtable's glyph pairs ahead of its class pairs
            let (glyph_pairs, class_pairs): (Vec<_>, Vec<_>) =
                segment.into_iter().partition(|pair| {
                    pair.enumerated
                        || matches!(
                            (&pair.glyphs_1, &pair.glyphs_2),
                            (GlyphContainer::GlyphName(_), GlyphContainer::GlyphName(_))
                        )
                });
            for pair in glyph_pairs {
                let value = Self::advance(pair)?;
                for left in self.expand(&pair.glyphs_1) {
                    if claimed.contains(&left) {
                        continue;
                    }
                    for right in self.expand(&pair.glyphs_2) {
                        if seen_pairs.insert((left.clone(), right.clone())) {
                            kerning.first_glyphs.insert(left.clone());
                            kerning.pairs.push((
                                Side::Glyph(left.clone()),
                                Side::Glyph(right),
                                value.clone(),
                            ));
                        }
                    }
                }
            }
            // Each first class's row of kerning, in order
            let mut rows: Vec<(Side, Vec<(Side, Metric)>)> = vec![];
            for pair in class_pairs {
                let value = Self::advance(pair)?;
                let left = self.side(&pair.glyphs_1);
                let right = self.side(&pair.glyphs_2);
                match rows.iter_mut().find(|(l, _)| *l == left) {
                    Some((_, row)) => row.push((right, value)),
                    None => rows.push((left, vec![(right, value)])),
                }
            }
            // First classes that kern alike are one class (a compiler may split
            // a group, as class 0 of a class definition)
            let mut merged: Vec<(Side, Vec<(Side, Metric)>)> = vec![];
            for (left, row) in rows {
                match merged.iter_mut().find(|(_, r)| *r == row) {
                    Some((Side::Class(name, members), _)) => {
                        *name = None;
                        members.extend(left.glyphs());
                    }
                    Some((existing, _)) => {
                        let mut members = existing.glyphs();
                        members.extend(left.glyphs());
                        *existing = Side::Class(None, members);
                    }
                    None => merged.push((left, row)),
                }
            }
            let mut covered = HashSet::new();
            for (left, row) in merged {
                let left_glyphs = left.glyphs();
                // a glyph an earlier class subtable kerned is shadowed here; the
                // model cannot leave a group member out of a group pair
                if left_glyphs.iter().any(|g| claimed.contains(g)) {
                    return None;
                }
                covered.extend(left_glyphs.iter().cloned());
                kerning.first_glyphs.extend(left_glyphs);
                for (right, value) in row {
                    kerning.pairs.push((left.clone(), right, value));
                }
            }
            claimed.extend(covered);
        }
        Some(kerning)
    }
}

/// Kern groups by their members, named after a class's name or its first glyph
#[derive(Default)]
struct Groups {
    by_members: HashMap<Vec<SmolStr>, SmolStr>,
    by_glyph: HashMap<SmolStr, SmolStr>,
    groups: IndexMap<SmolStr, Vec<SmolStr>>,
}

impl Groups {
    fn key(&mut self, side: &Side) -> Option<SmolStr> {
        let (name, members) = match side {
            Side::Glyph(glyph) => return Some(glyph.clone()),
            Side::Class(name, members) => (name, members),
        };
        if members.len() == 1 && name.is_none() {
            return members.first().cloned();
        }
        let mut sorted = members.clone();
        sorted.sort();
        if let Some(group) = self.by_members.get(&sorted) {
            return Some(format!("@{group}").into());
        }
        // a glyph is in one group at most
        if members.iter().any(|g| self.by_glyph.contains_key(g)) {
            return None;
        }
        let base = name.clone().or_else(|| members.first().cloned())?;
        let mut group = base.clone();
        let mut n = 1;
        while self.groups.contains_key(&group) {
            group = format!("{base}.{n}").into();
            n += 1;
        }
        for glyph in members {
            self.by_glyph.insert(glyph.clone(), group.clone());
        }
        self.by_members.insert(sorted, group.clone());
        self.groups.insert(group.clone(), members.clone());
        Some(format!("@{group}").into())
    }
}

impl FontFilter for MoveKerningFromFeatures {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        let has_kern = font.features.features.iter().any(|(n, _)| n == "kern")
            || font.features.to_fea().contains("kern");
        if !has_kern {
            return Ok(());
        }
        if font.masters.iter().any(|m| !m.kerning.is_empty())
            || !font.first_kern_groups.is_empty()
            || !font.second_kern_groups.is_empty()
        {
            log::info!("The font has kerning already; its kern feature stays feature code");
            return Ok(());
        }
        let fea = font.features.to_fea();
        let glyph_names: Vec<&str> = font.glyphs.iter().map(|g| g.name.as_str()).collect();
        let file = match FeatureFile::new_from_fea(&fea, Some(&glyph_names), font.source.clone()) {
            Ok(file) => file,
            Err(e) => {
                log::warn!("Kerning stays feature code; the features do not parse: {e}");
                return Ok(());
            }
        };

        let mut reader = Reader {
            font,
            classes: HashMap::new(),
        };
        let mut lookups: HashMap<SmolStr, &LookupBlock> = HashMap::new();
        let mut kern_blocks = vec![];
        for item in file.iter() {
            match item {
                ToplevelItem::GlyphClassDefinition(definition) => {
                    let glyphs =
                        reader.expand(&GlyphContainer::GlyphClass(definition.glyphs.clone()));
                    reader
                        .classes
                        .insert(definition.name.trim_start_matches('@').into(), glyphs);
                }
                ToplevelItem::Lookup(lookup) => {
                    lookups.insert(lookup.name.clone(), lookup);
                }
                ToplevelItem::Feature(feature) if feature.name == "kern" => {
                    kern_blocks.push(feature);
                }
                _ => {}
            }
        }

        // Lookups every section of every kern block applies
        let mut candidates: Option<Vec<SmolStr>> = None;
        for block in &kern_blocks {
            let Some(sections) = lookups_by_section(&block.statements) else {
                return Ok(());
            };
            for section in sections {
                candidates = Some(match candidates {
                    None => section,
                    Some(previous) => previous
                        .into_iter()
                        .filter(|name| section.contains(name))
                        .collect(),
                });
            }
        }
        let candidates = candidates.unwrap_or_default();
        let kern_references: HashMap<&SmolStr, usize> = candidates
            .iter()
            .map(|name| {
                let count = kern_blocks
                    .iter()
                    .map(|b| lookup_reference_count(&b.as_fea(""), name))
                    .sum();
                (name, count)
            })
            .collect();

        let mut moved: Vec<(SmolStr, LookupKerning)> = vec![];
        let mut first_glyphs: HashSet<SmolStr> = HashSet::new();
        for name in &candidates {
            let Some(lookup) = lookups.get(name) else {
                continue;
            };
            // used elsewhere too (another feature, a contextual rule)
            let elsewhere = lookup_reference_count(&fea, name)
                - 1
                - kern_references.get(name).copied().unwrap_or(0);
            if elsewhere > 0 {
                continue;
            }
            let Some(kerning) = reader.read_lookup(lookup) else {
                log::info!("Lookup {name} stays feature code; the kerning model cannot hold it");
                continue;
            };
            // lookups kerning the same first glyphs add up; the model cannot
            if !kerning.first_glyphs.is_disjoint(&first_glyphs) {
                continue;
            }
            first_glyphs.extend(kerning.first_glyphs.iter().cloned());
            moved.push((name.clone(), kerning));
        }
        if moved.is_empty() {
            return Ok(());
        }

        // Pairs and groups, then the values at each master
        let mut first_groups = Groups::default();
        let mut second_groups = Groups::default();
        let mut pairs: Vec<(SmolStr, SmolStr, Metric)> = vec![];
        let mut moved_names: HashSet<SmolStr> = HashSet::new();
        for (name, kerning) in moved {
            let mut lookup_pairs = vec![];
            let mut ok = true;
            for (left, right, value) in kerning.pairs {
                match (first_groups.key(&left), second_groups.key(&right)) {
                    (Some(left), Some(right)) => lookup_pairs.push((left, right, value)),
                    _ => {
                        ok = false;
                        break;
                    }
                }
            }
            if !ok {
                log::info!("Lookup {name} stays feature code; its classes overlap");
                continue;
            }
            pairs.extend(lookup_pairs);
            moved_names.insert(name);
        }
        if moved_names.is_empty() {
            return Ok(());
        }
        let axes = font.fontdrasil_axes()?;
        let mut values: Vec<Vec<i16>> = vec![];
        for master in &font.masters {
            let mut master_values = vec![];
            for (_, _, value) in &pairs {
                let v = crate::features::metric_at(value, &master.location, &axes)?;
                master_values.push(v.round().clamp(i16::MIN as f64, i16::MAX as f64) as i16);
            }
            values.push(master_values);
        }
        // A class pair that kerns nothing anywhere says nothing
        let keep: Vec<bool> = (0..pairs.len())
            .map(|i| {
                let (left, right, _) = &pairs[i];
                !(left.starts_with('@') && right.starts_with('@'))
                    || values.iter().any(|v| v[i] != 0)
            })
            .collect();
        for (master, master_values) in font.masters.iter_mut().zip(values) {
            for (i, (left, right, _)) in pairs.iter().enumerate() {
                if keep[i] {
                    master
                        .kerning
                        .insert((left.clone(), right.clone()), master_values[i]);
                }
            }
        }
        font.first_kern_groups = first_groups.groups;
        font.second_kern_groups = second_groups.groups;
        remove_lookups(&mut font.features, &moved_names, file);
        Ok(())
    }

    fn from_str(_s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(MoveKerningFromFeatures::new())
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        clap::Arg::new("movekerningfromfeatures")
            .long("move-kerning-from-features")
            .help("Move explicit FEA-based kerning rules into the kerning table")
            .action(clap::ArgAction::SetTrue)
    }
}

/// Takes the moved lookups out of the feature code, keeping its structure
/// where each lookup is a block of its own in some prefix and the kern feature
/// refers to it, as a decompiled font's code is
fn remove_lookups(features: &mut Features, names: &HashSet<SmolStr>, file: FeatureFile) {
    if remove_lookup_blocks(features, names, &["kern"]) {
        return;
    }
    // Otherwise rewrite the feature code without them
    let mut file = file;
    file.statements.retain(|item| match item {
        ToplevelItem::Lookup(lookup) => !names.contains(&lookup.name),
        _ => true,
    });
    for item in file.statements.iter_mut() {
        if let ToplevelItem::Feature(feature) = item {
            if feature.name == "kern" {
                let first_moved = feature.statements.iter().position(|s| {
                    matches!(s, Statement::LookupReference(r) if names.contains(r.lookup_name.as_str()))
                });
                feature.statements.retain(|s| match s {
                    Statement::LookupReference(r) => !names.contains(r.lookup_name.as_str()),
                    _ => true,
                });
                if let Some(index) = first_moved {
                    feature.statements.insert(
                        index.min(feature.statements.len()),
                        Statement::Comment(fea_rs_ast::Comment::new(INSERTION_MARKER.into())),
                    );
                }
            }
        }
    }
    file.statements.retain(|item| match item {
        ToplevelItem::Feature(feature) if feature.name == "kern" => {
            feature.statements.iter().any(|s| {
                !matches!(
                    s,
                    Statement::Script(_) | Statement::Language(_) | Statement::Comment(_)
                )
            })
        }
        _ => true,
    });
    *features = Features::from_fea(&file.as_fea(""));
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::indexing_slicing)]
mod tests {
    use super::*;
    use crate::{Glyph, Master};

    fn font(fea: &str) -> Font {
        let mut font = Font::new();
        for name in ["A", "Adot", "O", "Q", "V"] {
            font.glyphs.push(Glyph::new(name));
        }
        font.masters
            .push(Master::new("Regular", "m", Default::default()));
        font.features = Features::from_fea(fea);
        font
    }

    fn kern(font: &Font, left: &str, right: &str) -> Option<i16> {
        font.masters[0]
            .kerning
            .get(&(left.into(), right.into()))
            .copied()
    }

    #[test]
    fn test_class_and_glyph_pairs_become_kerning() {
        let mut font = font(
            "lookup k { pos A Adot -20; subtable; pos [A Adot] [O Q] -50; } k;\n\
             feature kern { lookup k; } kern;",
        );
        MoveKerningFromFeatures.apply(&mut font).unwrap();
        assert_eq!(kern(&font, "A", "Adot"), Some(-20));
        assert_eq!(kern(&font, "@A", "@O"), Some(-50));
        assert_eq!(font.first_kern_groups.get("A").unwrap(), &["A", "Adot"]);
        assert_eq!(font.second_kern_groups.get("O").unwrap(), &["O", "Q"]);
        assert!(
            !font.features.to_fea().contains("pos"),
            "{}",
            font.features.to_fea()
        );
    }

    #[test]
    fn test_lookups_in_one_prefix_keep_the_others() {
        let mut font = font("");
        font.features = Features::default();
        font.features.prefixes.insert(
            "Lookups".into(),
            crate::features::PossiblyAutomaticCode::new(
                "lookup single {\n    pos O 5;\n} single;\n\
                 lookup k {\n    pos A V -10;\n} k;\n\
                 lookup ctx {\n    pos Q' lookup single A;\n} ctx;",
            ),
        );
        font.features.features.push((
            "kern".into(),
            crate::features::PossiblyAutomaticCode::new("lookup k;\nlookup ctx;"),
        ));
        MoveKerningFromFeatures.apply(&mut font).unwrap();
        assert_eq!(kern(&font, "A", "V"), Some(-10));
        let lookups = &font.features.prefixes.get("Lookups").unwrap().code;
        assert!(lookups.contains("lookup single {") && lookups.contains("lookup ctx {"));
        assert!(!lookups.contains("lookup k {"), "{lookups}");
        let kern_code = &font.features.features[0].1.code;
        assert_eq!(kern_code, "# Automatic Code\nlookup ctx;");
    }

    #[test]
    fn test_a_shadowed_glyph_pair_is_not_kerning() {
        // A is kerned by the class subtable first; the glyph pair after it never applies
        let mut font = font(
            "lookup k { pos [A Adot] [O Q] -50; subtable; pos A O -10; } k;\n\
             feature kern { lookup k; } kern;",
        );
        MoveKerningFromFeatures.apply(&mut font).unwrap();
        assert_eq!(kern(&font, "A", "O"), None);
        assert_eq!(kern(&font, "@A", "@O"), Some(-50));
    }

    #[test]
    fn test_first_classes_that_kern_alike_are_one_group() {
        // as fontc writes a group: one member in class 0, the others in class 1
        let mut font = font(
            "lookup k { pos [A] [O Q] -50; pos [Adot] [O Q] -50; pos [V] [O Q] -10; } k;\n\
             feature kern { script DFLT; lookup k; script latn; lookup k; } kern;",
        );
        MoveKerningFromFeatures.apply(&mut font).unwrap();
        assert_eq!(font.first_kern_groups.get("A").unwrap(), &["A", "Adot"]);
        assert_eq!(kern(&font, "@A", "@O"), Some(-50));
        assert_eq!(kern(&font, "V", "@O"), Some(-10));
    }

    #[test]
    fn test_named_classes_name_the_groups() {
        let mut font = font(
            "@Round = [O Q];\nlookup k { pos V @Round -30; } k;\n\
             feature kern { lookup k; } kern;",
        );
        MoveKerningFromFeatures.apply(&mut font).unwrap();
        assert_eq!(kern(&font, "V", "@Round"), Some(-30));
        assert_eq!(font.second_kern_groups.get("Round").unwrap(), &["O", "Q"]);
    }

    #[test]
    fn test_contextual_and_placement_kerning_stay() {
        let fea = "lookup single { pos O 5; } single;\n\
                   lookup ctx { pos Q' lookup single A; } ctx;\n\
                   lookup placed { pos A O <10 0 0 0>; } placed;\n\
                   feature kern { lookup ctx; lookup placed; } kern;";
        let mut font = font(fea);
        MoveKerningFromFeatures.apply(&mut font).unwrap();
        assert!(font.masters[0].kerning.is_empty());
        assert!(font.features.to_fea().contains("lookup ctx;"));
        assert!(font.features.to_fea().contains("lookup placed;"));
    }

    #[test]
    fn test_script_specific_lookups_stay() {
        let mut font = font(
            "languagesystem DFLT dflt;\nlanguagesystem latn dflt;\n\
             lookup all { pos A V -10; } all;\nlookup latin { pos V A -20; } latin;\n\
             feature kern { lookup all; script latn; lookup latin; } kern;",
        );
        MoveKerningFromFeatures.apply(&mut font).unwrap();
        assert_eq!(kern(&font, "A", "V"), Some(-10));
        assert_eq!(kern(&font, "V", "A"), None);
        let fea = font.features.to_fea();
        assert!(
            fea.contains("lookup latin;") && !fea.contains("lookup all;"),
            "{fea}"
        );
        // so that a compiler puts the kerning it generates where the lookup was
        assert!(fea.contains("# Automatic Code"), "{fea}");
    }
}

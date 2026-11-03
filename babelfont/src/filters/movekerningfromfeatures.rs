use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use fea_rs_ast::{FeatureFile, GlyphContainer, LookupBlock, Metric, PairPosStatement, Statement};
use indexmap::IndexMap;
use smol_str::SmolStr;

use crate::{Font, GlyphCategory, filters::FontFilter};

use super::feature_code::{remove_lookup_blocks, remove_lookups_from_file};
use super::layout_model::{
    COMMON, DFLT_SCRIPT, Expander, Flags, GlyphProps, IGNORE_MARKS, INHERITED, LangSys, Layout,
    MasterValues, RIGHT_TO_LEFT, ScriptList, USE_MARK_FILTERING_SET, dist_script,
    fontc_script_tags, lookup_blocks, ot_round, ot_tag_script, right_to_left_script,
};

/// A filter that moves explicit FEA-based kerning rules into the kerning table
///
/// It moves the pair kerning lookups of the `kern` feature that a compiler
/// generating kerning from the kerning table makes again as they were: with the
/// same lookup flags, the same value records for the direction of each pair's
/// script, applied under the same scripts and languages, in the same place in
/// the lookup list. Glyph pairs become kerning pairs, class pairs become
/// kerning between kern groups, read as a shaper applies the lookup (the first
/// subtable that covers a glyph is the one that kerns it). Variable values are
/// evaluated at each master. Anything else, such as contextual kerning, stays
/// feature code.
#[derive(Default)]
pub struct MoveKerningFromFeatures {
    props: Option<GlyphProps>,
}

impl MoveKerningFromFeatures {
    /// Create a new MoveKerningFromFeatures filter
    pub fn new() -> Self {
        MoveKerningFromFeatures::default()
    }

    /// The filter, knowing glyphs' Unicode properties as a compiler finds
    /// them (through the substitutions of the font the code came from)
    pub(crate) fn with_glyph_props(props: GlyphProps) -> Self {
        MoveKerningFromFeatures { props: Some(props) }
    }
}

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

    fn is_class(&self) -> bool {
        matches!(self, Side::Class(..))
    }
}

/// A pair's kerning: its value, and whether it is written as kerning for a
/// right-to-left script (an x placement and an x advance of one value)
#[derive(Clone, Debug, PartialEq)]
struct Value {
    metric: Metric,
    right_to_left: bool,
}

/// The kerning one lookup holds: pairs of sides with their values, in the
/// order a shaper would find them
#[derive(Default, Debug)]
struct LookupKerning {
    pairs: Vec<(Side, Side, Value)>,
}

/// What a compiler makes of the flags of kerning lookups
#[derive(Clone, Copy, PartialEq)]
enum Marks {
    /// IgnoreMarks: pairs with a mark never apply and are left out
    Ignored,
    /// No flags, in a font with marks: every pair must have a mark
    Kerned,
}

struct Reader<'a> {
    expander: Expander<'a>,
    marks: HashSet<SmolStr>,
    /// the flags a compiler gives kerning lookups of base glyphs
    base_flags: Flags,
}

impl Reader<'_> {
    fn expand(&self, container: &GlyphContainer, marks: Marks) -> Vec<SmolStr> {
        let mut glyphs = self.expander.expand(container);
        if marks == Marks::Ignored {
            glyphs.retain(|g| !self.marks.contains(g));
        }
        glyphs
    }

    fn side(&self, container: &GlyphContainer, marks: Marks) -> Side {
        match container {
            GlyphContainer::GlyphName(name) => Side::Glyph(name.name.clone()),
            // a decompiler's name for a pair class says nothing: name the group
            // for its first glyph
            GlyphContainer::GlyphClassName(name)
                if !name
                    .trim_start_matches('@')
                    .starts_with(sr_aef::PAIR_CLASS_PREFIX) =>
            {
                Side::Class(
                    Some(name.trim_start_matches('@').into()),
                    self.expand(container, marks),
                )
            }
            _ => Side::Class(None, self.expand(container, marks)),
        }
    }

    /// The kerning a pair states: an x advance, or, as kerning is written for a
    /// right-to-left script, an x placement and an x advance of one value
    fn advance(statement: &PairPosStatement) -> Option<Value> {
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
        Some(Value {
            metric: record.x_advance.clone().unwrap_or(Metric::Scalar(0)),
            right_to_left,
        })
    }

    /// How a compiler would treat marks in a lookup of these flags, or None if
    /// it would write other flags
    fn marks_for(&self, flags: &Flags) -> Option<Marks> {
        let value = flags.value & !RIGHT_TO_LEFT;
        let plain = Flags {
            value,
            ..flags.clone()
        };
        if plain == self.base_flags {
            return Some(Marks::Ignored);
        }
        if value == 0 && flags.attachment.is_none() && flags.filter.is_none() {
            return Some(if self.marks.is_empty() {
                Marks::Ignored
            } else {
                Marks::Kerned
            });
        }
        None
    }

    /// The kerning of a lookup, or None if the kerning model cannot hold it
    fn read_lookup(&self, lookup: &LookupBlock) -> Option<LookupKerning> {
        let mut segments: Vec<Vec<&PairPosStatement>> = vec![vec![]];
        let mut flags = Flags::default();
        for statement in &lookup.statements {
            match statement {
                Statement::PairPos(pair) => segments.last_mut()?.push(pair),
                Statement::Subtable(_) => segments.push(vec![]),
                Statement::LookupFlag(flag) => {
                    flags = Flags {
                        value: flag.value
                            | if flag.mark_filtering_set.is_some() {
                                USE_MARK_FILTERING_SET
                            } else {
                                0
                            },
                        attachment: flag
                            .mark_attachment
                            .as_ref()
                            .map(|c| self.expander.expand(c).into_iter().collect()),
                        filter: flag
                            .mark_filtering_set
                            .as_ref()
                            .map(|c| self.expander.expand(c).into_iter().collect()),
                    };
                }
                Statement::Comment(_) => {}
                _ => return None,
            }
        }
        let Some(marks) = self.marks_for(&flags) else {
            log::info!(
                "Lookup {} stays feature code; a compiler would give it other lookup flags",
                lookup.name
            );
            return None;
        };
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
                for left in self.expand(&pair.glyphs_1, marks) {
                    if claimed.contains(&left) {
                        continue;
                    }
                    for right in self.expand(&pair.glyphs_2, marks) {
                        if seen_pairs.insert((left.clone(), right.clone())) {
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
            let mut rows: Vec<(Side, Vec<(Side, Value)>)> = vec![];
            for pair in class_pairs {
                let value = Self::advance(pair)?;
                let left = self.side(&pair.glyphs_1, marks);
                let right = self.side(&pair.glyphs_2, marks);
                if left.glyphs().is_empty() || right.glyphs().is_empty() {
                    continue;
                }
                match rows.iter_mut().find(|(l, _)| *l == left) {
                    Some((_, row)) => row.push((right, value)),
                    None => rows.push((left, vec![(right, value)])),
                }
            }
            let mut covered = HashSet::new();
            for (left, row) in rows {
                let left_glyphs = left.glyphs();
                // a glyph an earlier class subtable kerned is shadowed here; the
                // model cannot leave a group member out of a group pair
                if left_glyphs.iter().any(|g| claimed.contains(g)) {
                    return None;
                }
                covered.extend(left_glyphs);
                for (right, value) in row {
                    kerning.pairs.push((left.clone(), right, value));
                }
            }
            claimed.extend(covered);
        }
        if marks == Marks::Kerned
            && kerning.pairs.iter().any(|(l, r, _)| {
                !l.glyphs().iter().all(|g| self.marks.contains(g))
                    && !r.glyphs().iter().all(|g| self.marks.contains(g))
            })
        {
            log::info!(
                "Lookup {} stays feature code; a compiler would kern its base glyphs across marks",
                lookup.name
            );
            return None;
        }
        Some(kerning)
    }
}

/// Kern groups by their members, named after a class's name or its first glyph
#[derive(Default, Clone)]
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
        let mut sorted = members.clone();
        sorted.sort();
        if let Some(group) = self.by_members.get(&sorted) {
            return Some(format!("@{group}").into());
        }
        // A class of one glyph is a group of one: a compiler writes a glyph
        // kerned by itself as glyph pairs, and a group as a class (Fustat's @B,
        // @f). Where the glyph is in a group already, it is the glyph.
        if let [glyph] = members.as_slice() {
            if name.is_none() && self.by_glyph.contains_key(glyph) {
                return Some(glyph.clone());
            }
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

/// A pair's glyphs by index, its value, and whether it kerns nothing
type PairGlyphs = (BTreeSet<usize>, BTreeSet<usize>, Value, bool);

/// A pair as a compiler splits kerning by script and direction
struct Piece {
    lookup: SmolStr,
    scripts: BTreeSet<SmolStr>,
    side1: BTreeSet<usize>,
    side2: BTreeSet<usize>,
    written_right_to_left: bool,
    zero: bool,
    is_mark: bool,
}

/// A feature and a language system a lookup is registered under
type Registration = (SmolStr, LangSys);

const KERN: &str = "kern";
const DIST: &str = "dist";

/// What fontc makes of the kerning: its pieces, the lookup each goes in
/// (by script set and whether it kerns marks), and where each lookup applies
struct Simulation {
    pieces: Vec<(Piece, Option<usize>, bool)>,
    /// each lookup's features and language systems
    registered: Vec<BTreeSet<Registration>>,
    /// lookups that cannot be moved: a compiler would drop some of their
    /// kerning
    dropped: HashSet<SmolStr>,
}

struct Kerning<'a> {
    props: &'a GlyphProps,
    layout: &'a Layout,
    glyph_scripts: Vec<BTreeSet<SmolStr>>,
    left_to_right: BTreeSet<usize>,
    right_to_left: BTreeSet<usize>,
    marks: HashSet<usize>,
}

fn script_direction(script: &str) -> u8 {
    if script == COMMON {
        0
    } else if right_to_left_script(script) {
        2
    } else {
        1
    }
}

impl<'a> Kerning<'a> {
    #[cfg(feature = "ttf")]
    fn new(
        props: &'a GlyphProps,
        layout: &'a Layout,
        marks: HashSet<SmolStr>,
    ) -> Option<Kerning<'a>> {
        use super::layout_model::unicode;
        // the scripts a compiler knows: of characters of one script, and of
        // the language systems
        let mut known: HashSet<SmolStr> = unicode::font_scripts(&props.codepoints);
        known.extend(layout.declared.iter().filter_map(|(s, _)| ot_tag_script(s)));
        let classified = props.classify(|cp, buf: &mut Vec<SmolStr>| {
            if known.is_empty() {
                buf.push(COMMON.into());
            } else {
                buf.extend(
                    unicode::script_extensions(cp)
                        .into_iter()
                        .filter(|s| s == COMMON || s == INHERITED || known.contains(s)),
                );
            }
        });
        let mut glyph_scripts = vec![BTreeSet::new(); props.codepoints.len()];
        for (script, glyphs) in classified {
            for glyph in glyphs {
                if let Some(scripts) = glyph_scripts.get_mut(glyph) {
                    scripts.insert(script.clone());
                }
            }
        }
        for scripts in glyph_scripts.iter_mut() {
            if scripts.is_empty() || scripts.contains(COMMON) || scripts.contains(INHERITED) {
                *scripts = [SmolStr::from(COMMON)].into();
            }
        }
        let bidi = props.classify(|cp, buf: &mut Vec<bool>| buf.extend(unicode::bidi(cp)));
        Some(Kerning {
            props,
            layout,
            glyph_scripts,
            left_to_right: bidi.get(&true).cloned().unwrap_or_default(),
            right_to_left: bidi.get(&false).cloned().unwrap_or_default(),
            marks: marks.iter().filter_map(|m| props.name_index(m)).collect(),
        })
    }

    #[cfg(not(feature = "ttf"))]
    fn new(_: &'a GlyphProps, _: &'a Layout, _: HashSet<SmolStr>) -> Option<Kerning<'a>> {
        // Without Unicode data the scripts a compiler finds are not known
        None
    }

    fn indices(&self, glyphs: &[SmolStr]) -> Option<BTreeSet<usize>> {
        glyphs
            .iter()
            .map(|g| {
                if !self.props.known(g) {
                    return None;
                }
                self.props.name_index(g)
            })
            .collect()
    }

    /// fontc's split of a pair by the direction of its glyphs' scripts
    fn pieces(
        &self,
        lookup: &SmolStr,
        side1: &BTreeSet<usize>,
        side2: &BTreeSet<usize>,
        value: &Value,
        zero: bool,
    ) -> Vec<Piece> {
        let by_direction = |side: &BTreeSet<usize>| {
            let mut out: BTreeMap<u8, BTreeSet<usize>> = BTreeMap::new();
            for glyph in side {
                for script in self.glyph_scripts.get(*glyph).into_iter().flatten() {
                    out.entry(script_direction(script))
                        .or_default()
                        .insert(*glyph);
                }
            }
            out
        };
        let scripts_of = |side: &BTreeSet<usize>| -> BTreeSet<SmolStr> {
            side.iter()
                .flat_map(|g| self.glyph_scripts.get(*g).into_iter().flatten().cloned())
                .collect()
        };
        let is_mark = side1.iter().chain(side2).any(|g| self.marks.contains(g));
        let mut out = vec![];
        for (d1, g1) in by_direction(side1) {
            for (d2, g2) in by_direction(side2) {
                if d1 != 0 && d2 != 0 && d1 != d2 {
                    // a compiler drops it; no run of text has both
                    continue;
                }
                let s1 = scripts_of(&g1);
                let s2 = scripts_of(&g2);
                let mut scripts: BTreeSet<SmolStr> = s1.union(&s2).cloned().collect();
                if !(s1.contains(COMMON) && s2.contains(COMMON)) {
                    scripts.remove(COMMON);
                }
                out.push(Piece {
                    lookup: lookup.clone(),
                    scripts,
                    side1: g1.clone(),
                    side2: g2,
                    written_right_to_left: value.right_to_left,
                    zero,
                    is_mark,
                });
            }
        }
        out
    }

    /// What fontc makes of these pieces of kerning
    fn simulate(&self, pieces: Vec<Piece>) -> Simulation {
        // merge script sets that share a script, as fontc does
        let mut sets: Vec<BTreeSet<SmolStr>> = vec![];
        for piece in &pieces {
            if !sets.contains(&piece.scripts) {
                sets.push(piece.scripts.clone());
            }
        }
        let mut merged = true;
        while merged {
            merged = false;
            let mut out: Vec<BTreeSet<SmolStr>> = vec![];
            for set in sets {
                match out.iter_mut().find(|o| !o.is_disjoint(&set)) {
                    Some(existing) => {
                        existing.extend(set);
                        merged = true;
                    }
                    None => out.push(set),
                }
            }
            sets = out;
        }
        // one lookup per merged script set, base pairs and mark pairs apart
        let mut lookups: Vec<(BTreeSet<SmolStr>, bool)> = vec![];
        let mut dropped = HashSet::new();
        let mut placed = vec![];
        for piece in pieces {
            let Some(set) = sets.iter().find(|s| !s.is_disjoint(&piece.scripts)) else {
                dropped.insert(piece.lookup.clone());
                placed.push((piece, None, false));
                continue;
            };
            let glyphs: BTreeSet<usize> = piece.side1.union(&piece.side2).copied().collect();
            let has_ltr = !glyphs.is_disjoint(&self.left_to_right);
            let has_rtl = !glyphs.is_disjoint(&self.right_to_left);
            if has_ltr && has_rtl {
                // a compiler drops a pair of both directions
                dropped.insert(piece.lookup.clone());
                placed.push((piece, None, false));
                continue;
            }
            let rtl_script = set.first().is_some_and(|s| script_direction(s) == 2);
            let rtl = rtl_script && !has_ltr;
            let key = (set.clone(), piece.is_mark);
            let index = match lookups.iter().position(|l| *l == key) {
                Some(i) => i,
                None => {
                    lookups.push(key);
                    lookups.len() - 1
                }
            };
            placed.push((piece, Some(index), rtl));
        }
        // where fontc registers each lookup (`assign_lookups_to_scripts`):
        // kern for most scripts, dist for Indic and USE scripts, each with the
        // common lookups; kern's defaults are the common lookups and the left
        // to right ones (else the right to left ones)
        let mut by_script: BTreeMap<SmolStr, Vec<usize>> = BTreeMap::new();
        for (i, (scripts, _)) in lookups.iter().enumerate() {
            for script in scripts {
                by_script.entry(script.clone()).or_default().push(i);
            }
        }
        let mut registered: Vec<BTreeSet<Registration>> = vec![BTreeSet::new(); lookups.len()];
        let common: Vec<usize> = by_script.get(COMMON).cloned().unwrap_or_default();
        let mut defaults = common.clone();
        let (mut ltr, mut rtl) = (vec![], vec![]);
        for (script, ids) in &by_script {
            if dist_script(script) {
                continue;
            }
            match script_direction(script) {
                1 => ltr.extend(ids.iter().copied()),
                2 => rtl.extend(ids.iter().copied()),
                _ => {}
            }
        }
        defaults.extend(if ltr.is_empty() { rtl } else { ltr });
        for language in self.layout.declared_languages(DFLT_SCRIPT) {
            for id in &defaults {
                if let Some(r) = registered.get_mut(*id) {
                    r.insert((KERN.into(), (DFLT_SCRIPT.into(), language.clone())));
                }
            }
        }
        for (script, ids) in &by_script {
            if script == COMMON || script == INHERITED {
                continue;
            }
            let feature: SmolStr = if dist_script(script) { DIST } else { KERN }.into();
            for tag in fontc_script_tags(script) {
                for language in self.layout.declared_languages(&tag) {
                    for id in ids.iter().chain(&common) {
                        if let Some(r) = registered.get_mut(*id) {
                            r.insert((feature.clone(), (tag.clone(), language.clone())));
                        }
                    }
                }
            }
        }
        Simulation {
            pieces: placed,
            registered,
            dropped,
        }
    }

    /// The lookups whose kerning a compiler would make differently, given
    /// what it makes of all of them
    fn failing(&self, simulation: &Simulation, gpos: &ScriptList) -> HashSet<SmolStr> {
        let mut failing = simulation.dropped.clone();
        // the script list after: the original's, and what the compiler adds
        let mut after = gpos.clone();
        let mut added: HashSet<LangSys> = HashSet::new();
        for systems in &simulation.registered {
            for (_, system) in systems {
                let languages = after.scripts.entry(system.0.clone()).or_default();
                if languages.insert(system.1.clone()) {
                    added.insert(system.clone());
                }
            }
        }
        let moving: HashSet<&SmolStr> = simulation
            .pieces
            .iter()
            .map(|(p, _, _)| &p.lookup)
            .collect();
        let kept_at = |system: &LangSys| {
            self.layout.features.values().any(|systems| {
                systems.get(system).is_some_and(|lookups| {
                    lookups.iter().any(|l| {
                        !moving.contains(l) && self.layout.lookups.get(l).is_some_and(|i| i.gpos)
                    })
                })
            })
        };
        let generated_at =
|system: &LangSys| {
            simulation
                .registered
                .iter()
                .any(|r| r.iter().any(|(_, s)| s == system))
        };
        // Whether text of `script` shapes alike with the font's language
        // system `before` and the compiler's `now`: `before` has nothing but
        // the moved kerning, and the compiler kerns at `now` as it did
        let same_kerning = |script: &SmolStr, before: &LangSys, now: &LangSys| {
            !kept_at(before)
                && simulation.pieces.iter().all(|(piece, lookup, _)| {
                    if !(piece.scripts.contains(script) || piece.scripts.contains(COMMON)) {
                        return true;
                    }
                    let then = self.layout.applies(&piece.lookup, before);
                    let generated = lookup.is_some_and(|l| {
                        simulation
                            .registered
                            .get(l)
                            .is_some_and(|r| r.iter().any(|(_, s)| s == now))
                    });
                    then == generated
                })
        };
        let with_language = |list: &ScriptList, script: SmolStr, language: &SmolStr| {
            let language = if list
                .scripts
                .get(&script)
                .is_some_and(|l| l.contains(language))
            {
                language.clone()
            } else {
                super::layout_model::DFLT_LANG.into()
            };
            (script, language)
        };
        // a script record or language system added changes what a shaper
        // chooses for text of that script or language
        let mut changed: BTreeSet<LangSys> = BTreeSet::new();
        // an added record stands where a shaper chose another before
        let mut stood_for: HashMap<LangSys, LangSys> = HashMap::new();
        for system in &added {
            let Some(script) = ot_tag_script(&system.0) else {
                changed.insert(system.clone());
                continue;
            };
            let new_script = !gpos.scripts.contains_key(&system.0);
            if new_script && gpos.selected(&script) == after.selected(&script) {
                continue;
            }
            let before = if new_script {
                gpos.selected(&script)
                    .map(|s| with_language(gpos, s, &system.1))
            } else {
                Some((system.0.clone(), super::layout_model::DFLT_LANG.into()))
            };
            match before {
                Some(before) if same_kerning(&script, &before, system) => {
                    stood_for.insert(system.clone(), before);
                }
                _ => {
                    changed.insert(system.clone());
                }
            }
        }
        // a language system left with no positioning goes from the table,
        // and a shaper chooses another for its script: fine where that one has
        // no positioning of the font's own and the compiler's kerning there is
        // what the lost one applied
        let mut lost: Vec<LangSys> = vec![];
        for (script, languages) in &after.scripts {
            for language in languages {
                let system: LangSys = (script.clone(), language.clone());
                if !kept_at(&system) && !generated_at(&system) {
                    lost.push(system);
                }
            }
        }
        if !lost.is_empty() {
            let mut remaining = after.clone();
            for (script, language) in &lost {
                if let Some(languages) = remaining.scripts.get_mut(script) {
                    languages.remove(language);
                }
            }
            remaining
                .scripts
                .retain(|_, languages| !languages.is_empty());
            for system in &lost {
                let script = ot_tag_script(&system.0);
                let fallback = script
                    .as_ref()
                    .and_then(|s| remaining.selected(s))
                    .map(|f| with_language(&remaining, f, &system.1));
                let fine = match (&script, &fallback) {
                    (Some(script), Some(fallback)) => {
                        !kept_at(fallback) && same_kerning(script, system, fallback)
                    }
                    _ => false,
                };
                if !fine {
                    log::info!(
                        "Kerning stays feature code; {} {} would lose its positioning",
                        system.0,
                        system.1
                    );
                    failing.extend(moving.iter().map(|l| (*l).clone()));
                    return failing;
                }
            }
        }
        for (piece, lookup, rtl) in &simulation.pieces {
            let Some(lookup) = lookup else {
                continue;
            };
            // one reason per lookup is enough
            if failing.contains(&piece.lookup) {
                continue;
            }
            let systems = simulation.registered.get(*lookup);
            let changes = |(_, s): &&Registration| changed.contains(s);
            if systems.is_some_and(|s| s.iter().any(|r| changes(&r))) {
                log::info!(
                    "Lookup {} stays feature code; a compiler would add script records {:?}",
                    piece.lookup,
                    systems
                        .into_iter()
                        .flatten()
                        .filter(changes)
                        .map(|(_, s)| s)
                        .collect::<BTreeSet<_>>()
                );
                failing.insert(piece.lookup.clone());
                continue;
            }
            if !piece.zero && piece.written_right_to_left != *rtl {
                log::info!(
                    "Lookup {} stays feature code; its kerning is written for the other direction",
                    piece.lookup
                );
                failing.insert(piece.lookup.clone());
                continue;
            }
            // kern and dist alike: a shaper applies both, in lookup order
            for system in after.relevant(&piece.scripts) {
                let then = stood_for.get(&system).unwrap_or(&system);
                let before = self.layout.applies(&piece.lookup, then);
                let generated = systems.is_some_and(|s| s.iter().any(|(_, s)| *s == system));
                if before != generated {
                    log::info!(
                        "Lookup {} stays feature code; a compiler would apply it {} {} {}",
                        piece.lookup,
                        if generated { "under" } else { "not under" },
                        system.0,
                        system.1
                    );
                    failing.insert(piece.lookup.clone());
                    break;
                }
            }
        }
        failing
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
        let Some(moved) = self.choose(font, &file)? else {
            return Ok(());
        };
        let MovedKerning {
            names,
            first,
            pairs,
            values,
            first_groups,
            second_groups,
            marker_tags,
            glyph_scripts,
            stated,
        } = moved;
        let markers = vec![(first, marker_tags.iter().map(|t| t.as_str()).collect())];
        if !remove_lookup_blocks(&mut font.features, &names, &markers) {
            font.features = remove_lookups_from_file(&file, &names, &markers);
        }
        // the decompiler's pair classes nothing refers to any more
        let fea = font.features.to_fea();
        let used: HashSet<&str> = regex::Regex::new(r"@([A-Za-z0-9_.]+)")
            .map(|p| {
                p.captures_iter(&fea)
                    .filter(|c| !c.get(0).is_some_and(|m| fea[m.end()..].starts_with(" = [")))
                    .filter_map(|c| c.get(1).map(|m| m.as_str()))
                    .collect()
            })
            .unwrap_or_default();
        let unused: Vec<SmolStr> = font
            .features
            .classes
            .keys()
            .filter(|name| {
                name.starts_with(sr_aef::PAIR_CLASS_PREFIX) && !used.contains(name.as_str())
            })
            .cloned()
            .collect();
        for name in unused {
            font.features.classes.shift_remove(&name);
        }
        // A class pair that kerns nothing anywhere says nothing
        let keep: Vec<bool> = pairs
            .iter()
            .enumerate()
            .map(|(i, (left, right, _))| {
                !(left.starts_with('@') && right.starts_with('@'))
                    || values.iter().any(|v| v.get(i).is_some_and(|v| *v != 0))
            })
            .collect();
        // Within a lookup the first rule for a pair wins: where a class of one
        // glyph and a glyph pair become the same pair (@f:@f 22 after f f 15),
        // the first value stays
        for ((master, master_values), stated) in font.masters.iter_mut().zip(values).zip(stated) {
            if !stated {
                continue;
            }
            for (i, (left, right, _)) in pairs.iter().enumerate() {
                if let (Some(true), Some(value)) = (keep.get(i), master_values.get(i)) {
                    master
                        .kerning
                        .entry((left.clone(), right.clone()))
                        .or_insert(*value);
                }
            }
        }
        font.first_kern_groups = first_groups.groups;
        font.second_kern_groups = second_groups.groups;
        super::kerning_groups::merge_split_groups(font, &glyph_scripts);
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

struct MovedKerning {
    names: HashSet<SmolStr>,
    first: SmolStr,
    pairs: Vec<(SmolStr, SmolStr, Metric)>,
    values: Vec<Vec<i16>>,
    first_groups: Groups,
    second_groups: Groups,
    marker_tags: Vec<SmolStr>,
    /// Each glyph's scripts and direction, as a compiler finds them
    glyph_scripts: super::kerning_groups::GlyphScripts,
    /// The masters the kerning is written at; elsewhere it is interpolated
    stated: Vec<bool>,
}

impl MoveKerningFromFeatures {
    /// The lookups to move and the kerning they become, if any
    fn choose(
        &self,
        font: &Font,
        file: &FeatureFile,
    ) -> Result<Option<MovedKerning>, crate::BabelfontError> {
        let mut expander = Expander {
            font,
            classes: HashMap::new(),
        };
        let layout = Layout::new(file, &mut expander);
        if layout.marked_features.contains("kern") || layout.marked_features.contains("dist") {
            log::info!("The kern feature has an insertion marker; its kerning stays");
            return Ok(None);
        }
        let marks: HashSet<SmolStr> = font
            .glyphs
            .iter()
            .filter(|g| g.category == GlyphCategory::Mark)
            .map(|g| g.name.clone())
            .collect();
        let spacing: BTreeSet<SmolStr> = font
            .glyphs
            .iter()
            .filter(|g| {
                g.category == GlyphCategory::Mark && g.layers.iter().any(|l| l.width != 0.0)
            })
            .map(|g| g.name.clone())
            .collect();
        let base_flags = if spacing.is_empty() {
            Flags {
                value: IGNORE_MARKS,
                ..Flags::default()
            }
        } else {
            Flags {
                value: USE_MARK_FILTERING_SET,
                attachment: None,
                filter: Some(spacing),
            }
        };
        let reader = Reader {
            expander,
            marks: marks.clone(),
            base_flags,
        };
        let blocks = lookup_blocks(file);

        // Candidates: pair lookups only the kern and dist features refer to,
        // in order
        let mut candidates: Vec<(&SmolStr, &LookupBlock)> = blocks
            .iter()
            .filter(|(name, _)| layout.only_in(name, &[KERN, DIST]))
            .map(|(n, b)| (n, *b))
            .collect();
        candidates.sort_by_key(|(name, _)| layout.lookups.get(*name).map(|l| l.index));
        let mut master_values = MasterValues::new(font)?;
        // each lookup's name, kerning, values at each master, and whether
        // they are whole numbers there
        type Read = (SmolStr, LookupKerning, Vec<Vec<i16>>, Vec<bool>);
        let mut read: Vec<Read> = vec![];
        // The pairs the lookups read so far kern: by first glyph, the second
        // glyphs of each kerning pair (a compiler splits one kerning by script
        // into lookups that kern the same first glyphs, never the same pair)
        let mut kerned: HashMap<SmolStr, Vec<HashSet<SmolStr>>> = HashMap::new();
        'lookups: for (name, lookup) in candidates {
            let Some(kerning) = reader.read_lookup(lookup) else {
                log::info!("Lookup {name} stays feature code; the kerning model cannot hold it");
                continue;
            };
            let mut values = vec![vec![]; master_values.master_count()];
            let mut whole = vec![true; master_values.master_count()];
            for (_, _, value) in &kerning.pairs {
                let Some(at_masters) = master_values.at_masters(&value.metric) else {
                    log::info!("Lookup {name} stays feature code; a value has no master values");
                    continue 'lookups;
                };
                if !metric_at_masters_only(&value.metric, font) {
                    log::info!(
                        "Lookup {name} stays feature code; a value varies between the masters"
                    );
                    continue 'lookups;
                }
                for ((master, v), whole) in values.iter_mut().zip(at_masters).zip(whole.iter_mut()) {
                    *whole &= (v - v.round()).abs() < 1e-6;
                    master.push(ot_round(v).clamp(i16::MIN as i32, i16::MAX as i32) as i16);
                }
            }
            // lookups kerning the same pair add up; the model holds one value
            let nonzero = |i: usize| values.iter().any(|m| m.get(i).is_some_and(|v| *v != 0));
            let these: Vec<(Vec<SmolStr>, HashSet<SmolStr>)> = kerning
                .pairs
                .iter()
                .enumerate()
                .filter(|(i, _)| nonzero(*i))
                .map(|(_, (left, right, _))| (left.glyphs(), right.glyphs().into_iter().collect()))
                .collect();
            let adds_up = these.iter().any(|(lefts, rights)| {
                lefts.iter().any(|left| {
                    kerned
                        .get(left)
                        .is_some_and(|seconds| seconds.iter().any(|s| !s.is_disjoint(rights)))
                })
            });
            if adds_up {
                log::info!(
                    "Lookup {name} stays feature code; an earlier lookup kerns some of its pairs too"
                );
                continue;
            }
            for (lefts, rights) in these {
                for left in lefts {
                    kerned.entry(left).or_default().push(rights.clone());
                }
            }
            read.push((name.clone(), kerning, values, whole));
        }
        if read.is_empty() {
            return Ok(None);
        }

        // What a compiler would make of it, until it makes all of it as it was
        let props = match &self.props {
            Some(props) => props.clone(),
            None => GlyphProps::new(font, None),
        };
        let Some(kerning_model) = Kerning::new(&props, &layout, marks) else {
            log::info!("Kerning stays feature code; the glyphs' scripts are not known");
            return Ok(None);
        };
        let gpos = layout.gpos_systems();
        let mut unknown: HashSet<SmolStr> = HashSet::new();
        let mut pieces_of: HashMap<SmolStr, Vec<PairGlyphs>> = HashMap::new();
        for (name, kerning, values, _) in &read {
            let mut out = vec![];
            for (i, (left, right, value)) in kerning.pairs.iter().enumerate() {
                let (Some(s1), Some(s2)) = (
                    kerning_model.indices(&left.glyphs()),
                    kerning_model.indices(&right.glyphs()),
                ) else {
                    unknown.insert(name.clone());
                    break;
                };
                let zero = values.iter().all(|m| m.get(i).is_none_or(|v| *v == 0));
                // a compiler leaves out a class pair kerning nothing
                if zero && left.is_class() && right.is_class() {
                    continue;
                }
                out.push((s1, s2, value.clone(), zero));
            }
            pieces_of.insert(name.clone(), out);
        }
        let mut moving: Vec<SmolStr> = read
            .iter()
            .map(|(n, _, _, _)| n.clone())
            .filter(|n| !unknown.contains(n))
            .collect();
        loop {
            let pieces: Vec<Piece> =
                moving
                    .iter()
                    .flat_map(|name| {
                        pieces_of.get(name).into_iter().flatten().flat_map(
                            |(s1, s2, value, zero)| {
                                kerning_model.pieces(name, s1, s2, value, *zero)
                            },
                        )
                    })
                    .collect();
            let simulation = kerning_model.simulate(pieces);
            let failing = kerning_model.failing(&simulation, &gpos);
            if failing.is_empty() {
                break;
            }
            moving.retain(|n| !failing.contains(n));
            if moving.is_empty() {
                return Ok(None);
            }
        }

        // Where the compiler puts the lookups: where the first one was
        let Some(first) = moving
            .iter()
            .min_by_key(|n| layout.lookups.get(*n).map(|l| l.index))
            .cloned()
        else {
            return Ok(None);
        };
        let position = layout.lookups.get(&first).map(|l| l.index).unwrap_or(0);
        let positions: HashMap<SmolStr, usize> =
            moving.iter().map(|n| (n.clone(), position)).collect();
        if !layout.order_kept(&positions) {
            log::info!("Kerning stays feature code; it would change places with other lookups");
            return Ok(None);
        }

        // Pairs and groups, then the values at each master
        let mut first_groups = Groups::default();
        let mut second_groups = Groups::default();
        let mut pairs: Vec<(SmolStr, SmolStr, Metric)> = vec![];
        let mut values: Vec<Vec<i16>> = vec![vec![]; font.masters.len()];
        let mut names: HashSet<SmolStr> = HashSet::new();
        // Kerning goes where every value is a whole number: stated there, or
        // interpolated exactly. Elsewhere a compiler interpolates it from the
        // binary's rounded deltas, and a rounded value would be off by one.
        let mut whole_at = vec![true; font.masters.len()];
        for (name, kerning, lookup_values, whole) in read {
            if !moving.contains(&name) {
                continue;
            }
            // register groups on a copy: a lookup that fails leaves none behind
            let mut trial_first = first_groups.clone();
            let mut trial_second = second_groups.clone();
            let mut lookup_pairs = vec![];
            let mut ok = true;
            for (left, right, value) in &kerning.pairs {
                match (trial_first.key(left), trial_second.key(right)) {
                    (Some(left), Some(right)) => {
                        lookup_pairs.push((left, right, value.metric.clone()))
                    }
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
            first_groups = trial_first;
            second_groups = trial_second;
            pairs.extend(lookup_pairs);
            for (all, these) in values.iter_mut().zip(lookup_values) {
                all.extend(these);
            }
            for (all, these) in whole_at.iter_mut().zip(whole) {
                *all &= these;
            }
            names.insert(name);
        }
        if names.is_empty() {
            return Ok(None);
        }
        let first = moving
            .iter()
            .filter(|n| names.contains(*n))
            .min_by_key(|n| layout.lookups.get(*n).map(|l| l.index))
            .cloned()
            .unwrap_or(first);
        // Markers for the features the compiler writes kerning to; an Indic
        // script's kerning goes to a dist feature the font may have
        let mut marker_tags = vec![SmolStr::from(KERN)];
        if layout.features.contains_key(DIST) {
            marker_tags.push(DIST.into());
        }
        let stated = whole_at;
        let name = |i: &usize| font.glyphs.get_by_index(*i).map(|g| g.name.clone());
        let glyph_scripts = super::kerning_groups::GlyphScripts {
            scripts: kerning_model
                .glyph_scripts
                .iter()
                .enumerate()
                .filter_map(|(i, scripts)| Some((name(&i)?, scripts.clone())))
                .collect(),
            left_to_right: kerning_model
                .left_to_right
                .iter()
                .filter_map(name)
                .collect(),
            right_to_left: kerning_model
                .right_to_left
                .iter()
                .filter_map(name)
                .collect(),
            marks: kerning_model.marks.iter().filter_map(name).collect(),
        };
        Ok(Some(MovedKerning {
            names,
            first,
            pairs,
            values,
            first_groups,
            second_groups,
            marker_tags,
            glyph_scripts,
            stated,
        }))
    }
}

/// Whether a value is stated at masters only: elsewhere a compiler
/// interpolates between the masters, and a value stated between them would
/// be lost
fn metric_at_masters_only(metric: &Metric, font: &Font) -> bool {
    let Metric::Variable(items) = metric else {
        return true;
    };
    let Ok(axes) = font.fontdrasil_axes() else {
        return false;
    };
    let masters: Vec<Vec<i16>> = font
        .masters
        .iter()
        .filter_map(|m| m.location.to_normalized(&axes).ok())
        .map(|l| {
            axes.iter()
                .map(|a| l.get(a.tag).unwrap_or_default().to_f2dot14().to_bits())
                .collect()
        })
        .collect();
    items.iter().all(|(location, _)| {
        let here: Vec<i16> = axes
            .iter()
            .map(|axis| {
                location
                    .get(axis.tag.to_string().as_str())
                    .map_or(axis.default, |c| fontdrasil::coords::UserCoord::new(c.0))
                    .to_normalized(&axis.converter)
                    .to_f2dot14()
                    .to_bits()
            })
            .collect();
        // a step or two away is the master (a location through avar)
        masters.iter().any(|master| {
            master
                .iter()
                .zip(&here)
                .all(|(a, b)| (*a as i32 - *b as i32).abs() <= 2)
        })
    })
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::indexing_slicing)]
mod tests {
    use super::*;
    use crate::{Features, Glyph, Master};

    fn font(fea: &str) -> Font {
        let mut font = Font::new();
        for (name, cp) in [
            ("A", 0x41),
            ("Adot", 0x1EA0),
            ("Agrave", 0xC0),
            ("kadeva", 0x915),
            ("khadeva", 0x916),
            ("O", 0x4F),
            ("Q", 0x51),
            ("V", 0x56),
        ] {
            let mut glyph = Glyph::new(name);
            glyph.codepoints = vec![cp];
            glyph.category = GlyphCategory::Base;
            font.glyphs.push(glyph);
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

    const LS: &str =
        "languagesystem DFLT dflt;\nlanguagesystem latn dflt;\nlanguagesystem grek dflt;\n";

    #[test]
    fn test_class_and_glyph_pairs_become_kerning() {
        let mut font = font(&format!(
            "{LS}lookup k {{ lookupflag IgnoreMarks; pos A Adot -20; subtable; pos [A Adot] [O Q] -50; }} k;\n\
             feature kern {{ lookup k; }} kern;"
        ));
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
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
                "languagesystem DFLT dflt;\nlanguagesystem latn dflt;\n\
                 lookup single {\n    pos O 5;\n} single;\n\
                 lookup k {\n    lookupflag IgnoreMarks;\n    pos A V -10;\n} k;\n\
                 lookup ctx {\n    pos Q' lookup single A;\n} ctx;",
            ),
        );
        font.features.features.push((
            "kern".into(),
            crate::features::PossiblyAutomaticCode::new("lookup k;\nlookup ctx;"),
        ));
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert_eq!(kern(&font, "A", "V"), Some(-10));
        let lookups = &font.features.prefixes.get("Lookups").unwrap().code;
        assert!(lookups.contains("lookup single {") && lookups.contains("lookup ctx {"));
        assert!(!lookups.contains("lookup k {"), "{lookups}");
        // the compiler generates kerning where the lookup was
        assert!(
            lookups
                .contains("} single;\nfeature kern {\n    # Automatic Code\n} kern;\nlookup ctx"),
            "{lookups}"
        );
        let kern_code = &font.features.features[0].1.code;
        assert_eq!(kern_code, "lookup ctx;");
    }

    #[test]
    fn test_a_shadowed_glyph_pair_is_not_kerning() {
        // A is kerned by the class subtable first; the glyph pair after it never applies
        let mut font = font(&format!(
            "{LS}lookup k {{ lookupflag IgnoreMarks; pos [A Adot] [O Q] -50; subtable; pos A O -10; }} k;\n\
             feature kern {{ lookup k; }} kern;"
        ));
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert_eq!(kern(&font, "A", "O"), None);
        assert_eq!(kern(&font, "@A", "@O"), Some(-50));
    }

    #[test]
    fn test_first_classes_that_kern_alike_stay_apart() {
        // two groups whose other kerning a compiler wrote as glyph pairs
        let mut font = font(&format!(
            "{LS}lookup k {{ lookupflag IgnoreMarks; pos A O -60; pos [A Agrave] [O Q] -50; pos [Adot] [O Q] -50; pos [V] [O Q] -10; }} k;\n\
             feature kern {{ lookup k; }} kern;"
        ));
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert_eq!(font.first_kern_groups.get("A").unwrap(), &["A", "Agrave"]);
        assert_eq!(kern(&font, "@A", "@O"), Some(-50));
        assert_eq!(kern(&font, "A", "O"), Some(-60));
        // a class of one glyph is a group of one
        assert_eq!(kern(&font, "@Adot", "@O"), Some(-50));
        assert_eq!(kern(&font, "@V", "@O"), Some(-10));
    }

    #[test]
    fn test_indic_kerning_in_dist_is_kerning() {
        // as fontc writes it: Latin kerning in kern, Devanagari in dist
        let mut font = font(
            "languagesystem DFLT dflt;\nlanguagesystem latn dflt;\n\
             languagesystem dev2 dflt;\nlanguagesystem deva dflt;\n\
             lookup kL { lookupflag IgnoreMarks; pos A V -30; } kL;\n\
             lookup kD { lookupflag IgnoreMarks; pos kadeva khadeva -50; } kD;\n\
             feature kern { script DFLT; lookup kL; script latn; lookup kL; } kern;\n\
             feature dist { script dev2; lookup kD; script deva; lookup kD; } dist;",
        );
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert_eq!(kern(&font, "A", "V"), Some(-30));
        assert_eq!(kern(&font, "kadeva", "khadeva"), Some(-50));
        let fea = font.features.to_fea();
        assert!(!fea.contains("lookup kL") && !fea.contains("lookup kD"), "{fea}");
    }

    #[test]
    fn test_indic_kerning_in_kern_moves_to_dist() {
        // a compiler puts it in dist, which a shaper applies as it does kern
        let mut font = font(
            "languagesystem DFLT dflt;\nlanguagesystem latn dflt;\n\
             languagesystem dev2 dflt;\nlanguagesystem deva dflt;\n\
             lookup kD { lookupflag IgnoreMarks; pos kadeva khadeva -50; } kD;\n\
             feature kern { script dev2; lookup kD; script deva; lookup kD; } kern;",
        );
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert_eq!(kern(&font, "kadeva", "khadeva"), Some(-50));
    }

    #[test]
    fn test_kerning_interpolated_to_a_fraction_is_not_stated() {
        use crate::Axis;
        use fontdrasil::coords::{DesignCoord, DesignLocation, UserCoord};
        use write_fonts::types::Tag;
        // a master between two others, from the outlines: where its kerning
        // interpolates to a fraction, a compiler must interpolate it again;
        // where to whole numbers, it is the kerning there
        for (bold, between) in [(-51, None), (-50, Some(-25))] {
            let mut font = font(&format!(
                "{LS}lookup k {{ lookupflag IgnoreMarks; pos A V (wght=100:-10 wght=900:{bold}); }} k;\n\
                 feature kern {{ lookup k; }} kern;"
            ));
            let mut axis = Axis::new("Weight", Tag::new(b"wght"));
            axis.min = Some(UserCoord::new(100.0));
            axis.default = Some(UserCoord::new(100.0));
            axis.max = Some(UserCoord::new(900.0));
            font.axes.push(axis);
            font.masters.clear();
            for (id, weight) in [("light", 100.0), ("regular", 400.0), ("bold", 900.0)] {
                let location =
                    DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(weight))]);
                font.masters.push(Master::new(id, id, location));
            }
            MoveKerningFromFeatures::new().apply(&mut font).unwrap();
            let at = |i: usize| font.masters[i].kerning.get(&("A".into(), "V".into())).copied();
            assert_eq!((at(0), at(1), at(2)), (Some(-10), between, Some(bold)));
        }
    }

    #[test]
    fn test_a_value_stated_a_step_from_a_master_is_the_masters() {
        use crate::Axis;
        use fontdrasil::coords::{DesignCoord, DesignLocation, UserCoord};
        use write_fonts::types::Tag;
        // wght=300.1 normalizes two F2Dot14 steps from the master at 300, as
        // a location through avar comes back from a binary
        let mut font = font(&format!(
            "{LS}lookup k {{ lookupflag IgnoreMarks; pos A V (wght=100:-10 wght=300.1:-20 wght=900:-30); }} k;\n\
             feature kern {{ lookup k; }} kern;"
        ));
        let mut axis = Axis::new("Weight", Tag::new(b"wght"));
        axis.min = Some(UserCoord::new(100.0));
        axis.default = Some(UserCoord::new(100.0));
        axis.max = Some(UserCoord::new(900.0));
        font.axes.push(axis);
        font.masters.clear();
        for (id, weight) in [("light", 100.0), ("regular", 300.0), ("bold", 900.0)] {
            let location =
                DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(weight))]);
            font.masters.push(Master::new(id, id, location));
        }
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        let at = |i: usize| font.masters[i].kerning.get(&("A".into(), "V".into())).copied();
        assert_eq!((at(0), at(1), at(2)), (Some(-10), Some(-20), Some(-30)));
    }

    #[test]
    fn test_named_classes_name_the_groups() {
        let mut font = font(&format!(
            "{LS}@Round = [O Q];\nlookup k {{ lookupflag IgnoreMarks; pos V @Round -30; }} k;\n\
             feature kern {{ lookup k; }} kern;"
        ));
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert_eq!(kern(&font, "V", "@Round"), Some(-30));
        assert_eq!(font.second_kern_groups.get("Round").unwrap(), &["O", "Q"]);
    }

    #[test]
    fn test_contextual_and_placement_kerning_stay() {
        let fea = format!(
            "{LS}lookup single {{ pos O 5; }} single;\n\
                   lookup ctx {{ pos Q' lookup single A; }} ctx;\n\
                   lookup placed {{ pos A O <10 0 0 0>; }} placed;\n\
                   feature kern {{ lookup ctx; lookup placed; }} kern;"
        );
        let mut font = font(&fea);
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert!(font.masters[0].kerning.is_empty());
        assert!(font.features.to_fea().contains("lookup ctx;"));
        assert!(font.features.to_fea().contains("lookup placed;"));
    }

    #[test]
    fn test_script_specific_lookups_stay() {
        let mut font = font(&format!(
            "{LS}lookup all {{ lookupflag IgnoreMarks; pos A V -10; }} all;\n\
             lookup latin {{ lookupflag IgnoreMarks; pos V A -20; }} latin;\n\
             feature kern {{ lookup all; script grek; lookup latin; }} kern;"
        ));
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
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

    #[test]
    fn test_kerning_without_ignore_marks_stays() {
        let mut font = font(&format!(
            "{LS}lookup k {{ pos A V -10; }} k;\nfeature kern {{ lookup k; }} kern;"
        ));
        let mut mark = Glyph::new("acutecomb");
        mark.codepoints = vec![0x301];
        mark.category = GlyphCategory::Mark;
        font.glyphs.push(mark);
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert!(font.masters[0].kerning.is_empty());
    }

    #[test]
    fn test_left_to_right_kerning_written_for_right_to_left_stays() {
        let mut font = font(&format!(
            "{LS}lookup k {{ lookupflag IgnoreMarks; pos A V <-10 0 -10 0>; }} k;\n\
             feature kern {{ lookup k; }} kern;"
        ));
        MoveKerningFromFeatures::new().apply(&mut font).unwrap();
        assert!(font.masters[0].kerning.is_empty());
    }
}

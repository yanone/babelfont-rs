use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use fea_rs_ast::{
    Anchor as FeaAnchor, FeatureFile, GlyphContainer, LookupBlock, Statement, ToplevelItem,
};
use smol_str::SmolStr;

use super::feature_code::{remove_lookup_blocks, remove_lookups_from_file};
use super::layout_model::{
    COMMON, Expander, GlyphProps, IGNORE_MARKS, INHERITED, LangSys, Layout, MasterValues,
    RIGHT_TO_LEFT, ScriptList, USE_MARK_FILTERING_SET, abvm_script, lookup_blocks, ot_round,
    ot_tag_script,
};
use crate::{Font, GlyphCategory, LayerType, filters::FontFilter};

/// A filter that moves mark attachment written as feature code into anchors
///
/// A font's anchors are what a compiler builds `mark`, `mkmk`, `abvm`, `blwm`
/// and `curs` from. Where feature code states the same attachment with the
/// anchors' positions written into it (as a decompiled font's does), editing
/// an anchor changes nothing: the written lookups still win. This filter takes
/// the lookups out where a compiler builds from the anchors exactly the
/// attachment they make (the same marks on the same bases at the same places,
/// with the same lookup flags, under the same scripts and languages, in the
/// same place in the lookup list), so that it builds them from the anchors.
/// Where it would build anything else, the lookups stay, and the compiler is
/// kept from building attachment of its own besides.
#[derive(Default)]
pub struct MoveMarksFromFeatures {
    props: Option<GlyphProps>,
}

impl MoveMarksFromFeatures {
    /// Create a new MoveMarksFromFeatures filter
    pub fn new() -> Self {
        MoveMarksFromFeatures::default()
    }

    /// The filter, knowing glyphs' Unicode properties as a compiler finds
    /// them (through the substitutions of the font the code came from)
    pub(crate) fn with_glyph_props(props: GlyphProps) -> Self {
        MoveMarksFromFeatures { props: Some(props) }
    }
}

/// A position at each master
type Position = Vec<(i32, i32)>;

/// What an attachment is between: a mark on a base, a mark on a ligature's
/// component, a mark on a mark, a glyph's cursive anchors
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Key {
    Base(usize, usize),
    Ligature(usize, usize, usize),
    Mark(usize, usize),
    Cursive(usize),
}

impl Key {
    /// The glyph whose script decides which language systems matter
    fn base(&self) -> usize {
        match self {
            Key::Base(b, _) | Key::Ligature(b, _, _) | Key::Mark(b, _) | Key::Cursive(b) => *b,
        }
    }

    fn writer(&self) -> Writer {
        match self {
            Key::Base(..) | Key::Ligature(..) => Writer::Mark,
            Key::Mark(..) => Writer::Mkmk,
            Key::Cursive(..) => Writer::Curs,
        }
    }
}

/// Where an attachment puts the glyphs, and what of the lookup's flags
/// matters for it
#[derive(Clone, Debug, PartialEq)]
enum Value {
    Attach(Position, Position),
    /// mark to mark: the mark filtering set
    MarkOnMark(Position, Position, Option<BTreeSet<usize>>),
    /// entry, exit, right to left
    Cursive(Option<Position>, Option<Position>, bool),
}

/// The features a compiler generates, by what gates them
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Writer {
    /// mark, with abvm and blwm for Indic and USE scripts
    Mark,
    Mkmk,
    Curs,
}

impl Writer {
    fn tags(self) -> &'static [&'static str] {
        match self {
            Writer::Mark => &["abvm", "blwm", "mark"],
            Writer::Mkmk => &["mkmk"],
            Writer::Curs => &["curs"],
        }
    }
}

/// An anchor's kind, as fontc reads its name
enum AnchorKind {
    Base(SmolStr),
    Mark(SmolStr),
    Ligature(SmolStr, usize),
    Entry,
    Exit,
    Other,
}

fn anchor_kind(name: &str) -> AnchorKind {
    if name == "entry" {
        return AnchorKind::Entry;
    }
    if name == "exit" {
        return AnchorKind::Exit;
    }
    if name.starts_with("caret_") || name.starts_with("vcaret_") {
        return AnchorKind::Other;
    }
    if let Some(suffix) = name.strip_prefix('_') {
        if suffix.is_empty() || suffix.parse::<usize>().is_ok() {
            return AnchorKind::Other;
        }
        if suffix
            .rsplit_once('_')
            .is_some_and(|(_, n)| n.parse::<usize>().is_ok())
        {
            return AnchorKind::Other;
        }
        return AnchorKind::Mark(suffix.into());
    }
    if let Some((group, Ok(index))) = name
        .rsplit_once('_')
        .map(|(group, n)| (group, n.parse::<usize>()))
    {
        if index == 0 {
            return AnchorKind::Other;
        }
        return AnchorKind::Ligature(group.into(), index);
    }
    AnchorKind::Base(name.into())
}

/// The attachment a compiler builds from the anchors, lookup by lookup in the
/// order it writes them
struct Generated {
    /// writer, then lookups in order, each its attachments
    lookups: Vec<(Writer, Vec<(Key, Value)>)>,
}

struct Model<'a> {
    font: &'a Font,
    /// glyph index, anchor name, position at each master
    anchors: Vec<BTreeMap<SmolStr, Position>>,
    /// glyphs whose anchors a compiler cannot read alike in every master
    irregular: bool,
    glyph_scripts: Vec<BTreeSet<SmolStr>>,
    left_to_right: BTreeSet<usize>,
    abvm: Option<(BTreeSet<usize>, BTreeSet<usize>)>,
}

impl<'a> Model<'a> {
    #[cfg(feature = "ttf")]
    fn new(font: &'a Font, props: &'a GlyphProps, layout: &Layout) -> Option<Self> {
        use super::layout_model::unicode;
        let mut anchors = vec![];
        let mut irregular = false;
        for glyph in font.glyphs.iter() {
            let mut out: BTreeMap<SmolStr, Position> = BTreeMap::new();
            for master in &font.masters {
                let Some(layer) = glyph
                    .layers
                    .iter()
                    .find(|l| l.master == LayerType::DefaultForMaster(master.id.clone()))
                else {
                    irregular |= glyph.layers.iter().any(|l| !l.anchors.is_empty());
                    continue;
                };
                let mut seen = HashSet::new();
                for anchor in &layer.anchors {
                    if !seen.insert(anchor.name.as_str()) {
                        irregular = true;
                    }
                    out.entry(anchor.name.as_str().into())
                        .or_default()
                        .push((ot_round(anchor.x), ot_round(anchor.y)));
                }
            }
            if out.values().any(|p| p.len() != font.masters.len()) {
                irregular = true;
            }
            anchors.push(out);
        }
        let mut known: HashSet<SmolStr> = unicode::font_scripts(&props.codepoints);
        known.extend(layout.declared.iter().filter_map(|(s, _)| ot_tag_script(s)));
        let classified = props.classify(|cp, buf: &mut Vec<SmolStr>| {
            buf.extend(
                unicode::script_extensions(cp)
                    .into_iter()
                    .filter(|s| s == COMMON || s == INHERITED || known.contains(s)),
            );
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
        let directions = props.classify(|cp, buf: &mut Vec<bool>| {
            if let Some(script) = unicode::script(cp).filter(|s| s != COMMON && s != INHERITED) {
                buf.push(!super::layout_model::right_to_left_script(&script));
            }
        });
        // Indic and USE scripts' glyphs: their attachment goes to abvm and blwm
        let fea_scripts: HashSet<SmolStr> = layout
            .declared
            .iter()
            .filter_map(|(s, _)| ot_tag_script(s))
            .collect();
        let in_abvm = |script: &SmolStr| {
            abvm_script(script) && (fea_scripts.is_empty() || fea_scripts.contains(script))
        };
        let is_abvm = |cp: u32| -> Option<bool> {
            let scripts = unicode::script_extensions(cp);
            if scripts.iter().any(|s| s == COMMON) {
                return None;
            }
            Some(scripts.iter().any(in_abvm))
        };
        let any_abvm = props
            .codepoints
            .iter()
            .flatten()
            .any(|cp| is_abvm(*cp).unwrap_or(false));
        let abvm = any_abvm.then(|| {
            let abvm_glyphs = props
                .classify(|cp, buf: &mut Vec<bool>| buf.extend(is_abvm(cp)))
                .remove(&true)
                .unwrap_or_default();
            let mut other = props
                .classify(|cp, buf: &mut Vec<bool>| {
                    buf.push(
                        unicode::script_extensions(cp)
                            .iter()
                            .any(|s| !abvm_script(s)),
                    )
                })
                .remove(&true)
                .unwrap_or_default();
            other.extend((0..font.glyphs.len()).filter(|g| !abvm_glyphs.contains(g)));
            (abvm_glyphs, other)
        });
        Some(Model {
            font,
            anchors,
            irregular,
            glyph_scripts,
            left_to_right: directions.get(&true).cloned().unwrap_or_default(),
            abvm,
        })
    }

    #[cfg(not(feature = "ttf"))]
    fn new(_: &'a Font, _: &'a GlyphProps, _: &Layout) -> Option<Self> {
        None
    }

    fn category(&self, glyph: usize) -> GlyphCategory {
        self.font
            .glyphs
            .get_by_index(glyph)
            .map(|g| g.category.clone())
            .unwrap_or_default()
    }

    /// What fontc builds from the anchors
    fn generate(&self) -> Generated {
        // anchors of groups some base and some mark have
        let mut base_groups = HashSet::new();
        let mut mark_groups = HashSet::new();
        for anchors in &self.anchors {
            for name in anchors.keys() {
                match anchor_kind(name) {
                    AnchorKind::Base(g) | AnchorKind::Ligature(g, _) => {
                        base_groups.insert(g);
                    }
                    AnchorKind::Mark(g) => {
                        mark_groups.insert(g);
                    }
                    _ => {}
                }
            }
        }
        let used: HashSet<&SmolStr> = base_groups.intersection(&mark_groups).collect();
        let used_kind = |kind: &AnchorKind| match kind {
            AnchorKind::Base(g) | AnchorKind::Mark(g) | AnchorKind::Ligature(g, _) => {
                used.contains(g)
            }
            AnchorKind::Entry | AnchorKind::Exit => true,
            AnchorKind::Other => false,
        };
        let mark_glyphs: BTreeSet<usize> = (0..self.anchors.len())
            .filter(|g| {
                self.category(*g) == GlyphCategory::Mark
                    && self.anchors.get(*g).is_some_and(|a| {
                        a.keys().any(|n| {
                            let kind = anchor_kind(n);
                            matches!(kind, AnchorKind::Mark(_)) && used_kind(&kind)
                        })
                    })
            })
            .collect();
        // groups by name: bases (with their anchors) and marks
        type Group = (Vec<(usize, Position)>, Vec<(usize, Position)>);
        type LigatureGroup = (Vec<(usize, Vec<Option<Position>>)>, Vec<(usize, Position)>);
        let mut mark_base: BTreeMap<SmolStr, Group> = BTreeMap::new();
        let mut mark_lig: BTreeMap<SmolStr, LigatureGroup> = BTreeMap::new();
        let mut mark_mark: BTreeMap<SmolStr, Group> = BTreeMap::new();
        let mut cursive: Vec<(usize, Option<Position>, Option<Position>)> = vec![];
        let (mut has_entry, mut has_exit) = (false, false);
        let mark_anchor_groups: HashSet<SmolStr> = mark_glyphs
            .iter()
            .filter_map(|g| self.anchors.get(*g))
            .flat_map(|a| a.keys())
            .filter_map(|n| match anchor_kind(n) {
                AnchorKind::Mark(g) if used.contains(&g) => Some(g),
                _ => None,
            })
            .collect();
        for (glyph, anchors) in self.anchors.iter().enumerate() {
            let is_mark = mark_glyphs.contains(&glyph);
            let treat_as_base = !is_mark && self.category(glyph) == GlyphCategory::Base;
            let is_ligature = self.category(glyph) == GlyphCategory::Ligature;
            let mut components: BTreeMap<SmolStr, Vec<Option<Position>>> = BTreeMap::new();
            let max_index = anchors
                .keys()
                .filter_map(|n| match anchor_kind(n) {
                    AnchorKind::Ligature(g, i) if used.contains(&g) => Some(i),
                    _ => None,
                })
                .max();
            let (mut entry, mut exit) = (None, None);
            for (name, position) in anchors {
                let kind = anchor_kind(name);
                if !used_kind(&kind) {
                    continue;
                }
                match kind {
                    AnchorKind::Base(g) => {
                        if treat_as_base {
                            mark_base
                                .entry(g.clone())
                                .or_default()
                                .0
                                .push((glyph, position.clone()));
                        }
                        if is_mark && mark_anchor_groups.contains(&g) {
                            mark_mark
                                .entry(g)
                                .or_default()
                                .0
                                .push((glyph, position.clone()));
                        }
                    }
                    AnchorKind::Mark(g) => {
                        if is_mark {
                            mark_base
                                .entry(g.clone())
                                .or_default()
                                .1
                                .push((glyph, position.clone()));
                        }
                    }
                    AnchorKind::Ligature(g, i) => {
                        if let (true, Some(max)) = (is_ligature, max_index) {
                            let slots = components.entry(g).or_insert_with(|| vec![None; max]);
                            if let Some(slot) = slots.get_mut(i - 1) {
                                *slot = Some(position.clone());
                            }
                        }
                    }
                    AnchorKind::Entry => {
                        has_entry = true;
                        entry = Some(position.clone());
                    }
                    AnchorKind::Exit => {
                        has_exit = true;
                        exit = Some(position.clone());
                    }
                    AnchorKind::Other => {}
                }
            }
            for (g, slots) in components {
                mark_lig.entry(g).or_default().0.push((glyph, slots));
            }
            if entry.is_some() || exit.is_some() {
                cursive.push((glyph, entry, exit));
            }
        }
        // the marks of mark-to-mark and mark-to-ligature groups
        for (glyph, anchors) in self.anchors.iter().enumerate() {
            if !mark_glyphs.contains(&glyph) {
                continue;
            }
            for (name, position) in anchors {
                if let AnchorKind::Mark(g) = anchor_kind(name) {
                    if let Some(group) = mark_mark.get_mut(&g) {
                        group.1.push((glyph, position.clone()));
                    }
                    if let Some(group) = mark_lig.get_mut(&g) {
                        group.1.push((glyph, position.clone()));
                    }
                }
            }
        }
        mark_base.retain(|_, (bases, marks)| !bases.is_empty() && !marks.is_empty());

        let everything: BTreeSet<usize> = (0..self.anchors.len()).collect();
        let (abvm_glyphs, other_glyphs) = match &self.abvm {
            Some((a, o)) => (a.clone(), o.clone()),
            None => (BTreeSet::new(), everything),
        };
        let below = |g: &SmolStr| g.starts_with("bottom") || g == "nukta";
        let mut lookups: Vec<(Writer, Vec<(Key, Value)>)> = vec![];
        let feature = |include: &BTreeSet<usize>,
                       filter: &dyn Fn(&SmolStr) -> bool,
                       with_marks: bool,
                       with_mkmk: Option<Writer>,
                       lookups: &mut Vec<(Writer, Vec<(Key, Value)>)>| {
            if with_marks {
                for (g, (bases, marks)) in &mark_base {
                    if !filter(g) {
                        continue;
                    }
                    let bases: Vec<_> = bases.iter().filter(|(b, _)| include.contains(b)).collect();
                    if bases.is_empty() || marks.is_empty() {
                        continue;
                    }
                    let mut facts = vec![];
                    for (b, bp) in &bases {
                        for (m, mp) in marks {
                            facts.push((Key::Base(*b, *m), Value::Attach(bp.clone(), mp.clone())));
                        }
                    }
                    lookups.push((Writer::Mark, facts));
                }
                for (g, (ligatures, marks)) in &mark_lig {
                    if !filter(g) {
                        continue;
                    }
                    let ligatures: Vec<_> = ligatures
                        .iter()
                        .filter(|(l, _)| include.contains(l))
                        .collect();
                    if ligatures.is_empty() || marks.is_empty() {
                        continue;
                    }
                    let mut facts = vec![];
                    for (l, slots) in &ligatures {
                        for (i, slot) in slots.iter().enumerate() {
                            let Some(lp) = slot else { continue };
                            for (m, mp) in marks {
                                facts.push((
                                    Key::Ligature(*l, i, *m),
                                    Value::Attach(lp.clone(), mp.clone()),
                                ));
                            }
                        }
                    }
                    lookups.push((Writer::Mark, facts));
                }
            }
            if let Some(writer) = with_mkmk {
                for (g, (bases, marks)) in &mark_mark {
                    if !filter(g) {
                        continue;
                    }
                    let bases: Vec<_> = bases.iter().filter(|(b, _)| include.contains(b)).collect();
                    if bases.is_empty() || marks.is_empty() {
                        continue;
                    }
                    let all_marks: HashSet<usize> = marks.iter().map(|(m, _)| *m).collect();
                    let filter_set: BTreeSet<usize> = marks
                        .iter()
                        .map(|(m, _)| *m)
                        .filter(|m| include.contains(m))
                        .chain(
                            bases
                                .iter()
                                .map(|(b, _)| *b)
                                .filter(|b| !all_marks.contains(b)),
                        )
                        .collect();
                    let mut facts = vec![];
                    for (b, bp) in &bases {
                        for (m, mp) in marks {
                            facts.push((
                                Key::Mark(*b, *m),
                                Value::MarkOnMark(bp.clone(), mp.clone(), Some(filter_set.clone())),
                            ));
                        }
                    }
                    lookups.push((writer, facts));
                }
            }
        };
        if !abvm_glyphs.is_empty() {
            feature(
                &abvm_glyphs,
                &|g| !below(g),
                true,
                Some(Writer::Mark),
                &mut lookups,
            );
            feature(&abvm_glyphs, &below, true, Some(Writer::Mark), &mut lookups);
        }
        feature(&other_glyphs, &|_| true, true, None, &mut lookups);
        feature(
            &other_glyphs,
            &|_| true,
            false,
            Some(Writer::Mkmk),
            &mut lookups,
        );
        if has_entry && has_exit {
            let (mut ltr, mut rtl) = (vec![], vec![]);
            for (glyph, entry, exit) in cursive {
                let left_to_right = self.left_to_right.contains(&glyph);
                let fact = (
                    Key::Cursive(glyph),
                    Value::Cursive(entry, exit, !left_to_right),
                );
                if left_to_right {
                    ltr.push(fact);
                } else {
                    rtl.push(fact);
                }
            }
            for facts in [ltr, rtl] {
                if !facts.is_empty() {
                    lookups.push((Writer::Curs, facts));
                }
            }
        }
        Generated { lookups }
    }
}

/// The attachment a lookup of feature code makes
struct Written {
    name: SmolStr,
    index: usize,
    facts: Vec<(Key, Value)>,
}

struct Reader<'a> {
    expander: Expander<'a>,
    props: &'a GlyphProps,
    values: MasterValues,
    mark_classes: HashMap<SmolStr, Vec<(Vec<SmolStr>, FeaAnchor)>>,
}

impl Reader<'_> {
    fn indices(&self, container: &GlyphContainer) -> Option<Vec<usize>> {
        self.expander
            .expand(container)
            .iter()
            .map(|g| self.props.name_index(g))
            .collect()
    }

    fn position(&mut self, anchor: &FeaAnchor) -> Option<Position> {
        if anchor.contourpoint.is_some()
            || anchor.x_device_table.is_some()
            || anchor.y_device_table.is_some()
        {
            return None;
        }
        let x = self.values.at_masters(&anchor.x)?;
        let y = self.values.at_masters(&anchor.y)?;
        Some(
            x.into_iter()
                .zip(y)
                .map(|(x, y)| (ot_round(x), ot_round(y)))
                .collect(),
        )
    }

    /// The marks of a class with their anchors
    fn class(&mut self, name: &SmolStr) -> Option<Vec<(usize, Position)>> {
        let definitions = self.mark_classes.get(name)?.clone();
        let mut out = vec![];
        for (marks, anchor) in definitions {
            let position = self.position(&anchor)?;
            for mark in marks {
                out.push((self.props.name_index(&mark)?, position.clone()));
            }
        }
        Some(out)
    }

    /// Attachment marks to bases (`base` makes the key)
    fn attach(
        &mut self,
        bases: &[usize],
        marks: &[(Option<FeaAnchor>, fea_rs_ast::MarkClass)],
        key: &dyn Fn(usize, usize) -> Key,
        filter: &Option<BTreeSet<usize>>,
        mark_on_mark: bool,
        facts: &mut Vec<(Key, Value)>,
    ) -> Option<()> {
        for (anchor, class) in marks {
            // a NULL anchor attaches nothing
            let Some(anchor) = anchor else { continue };
            let position = self.position(anchor)?;
            for (mark, mark_position) in self.class(&class.name)? {
                for base in bases {
                    let value = if mark_on_mark {
                        Value::MarkOnMark(position.clone(), mark_position.clone(), filter.clone())
                    } else {
                        Value::Attach(position.clone(), mark_position.clone())
                    };
                    facts.push((key(*base, mark), value));
                }
            }
        }
        Some(())
    }

    /// The attachment a lookup makes, or None if it does anything else or
    /// has flags a compiler would not write
    fn read(&mut self, lookup: &LookupBlock, index: usize) -> Option<Written> {
        let mut facts = vec![];
        let mut value = 0;
        let mut attachment = false;
        let mut filter: Option<BTreeSet<usize>> = None;
        for statement in &lookup.statements {
            match statement {
                Statement::LookupFlag(flag) => {
                    value = flag.value;
                    attachment = flag.mark_attachment.is_some();
                    filter = match &flag.mark_filtering_set {
                        Some(set) => Some(self.indices(set)?.into_iter().collect()),
                        None => None,
                    };
                    if filter.is_some() {
                        value |= USE_MARK_FILTERING_SET;
                    }
                }
                Statement::Subtable(_) | Statement::Comment(_) => {}
                Statement::MarkBasePos(pos) => {
                    if value & (IGNORE_MARKS | USE_MARK_FILTERING_SET) != 0 || attachment {
                        return None;
                    }
                    let bases = self.indices(&pos.base)?;
                    self.attach(
                        &bases,
                        &pos.marks,
                        &|b, m| Key::Base(b, m),
                        &None,
                        false,
                        &mut facts,
                    )?;
                }
                Statement::MarkLigPos(pos) => {
                    if value & (IGNORE_MARKS | USE_MARK_FILTERING_SET) != 0 || attachment {
                        return None;
                    }
                    let ligatures = self.indices(&pos.ligatures)?;
                    for (component, marks) in pos.marks.iter().enumerate() {
                        self.attach(
                            &ligatures,
                            marks,
                            &|l, m| Key::Ligature(l, component, m),
                            &None,
                            false,
                            &mut facts,
                        )?;
                    }
                }
                Statement::MarkMarkPos(pos) => {
                    if value & IGNORE_MARKS != 0 || attachment || filter.is_none() {
                        return None;
                    }
                    let bases = self.indices(&pos.base_marks)?;
                    self.attach(
                        &bases,
                        &pos.marks,
                        &|b, m| Key::Mark(b, m),
                        &filter,
                        true,
                        &mut facts,
                    )?;
                }
                Statement::CursivePos(pos) => {
                    if value & !RIGHT_TO_LEFT != IGNORE_MARKS || attachment {
                        return None;
                    }
                    let entry = match &pos.entry {
                        Some(a) => Some(self.position(a)?),
                        None => None,
                    };
                    let exit = match &pos.exit {
                        Some(a) => Some(self.position(a)?),
                        None => None,
                    };
                    for glyph in self.indices(&pos.glyphclass)? {
                        facts.push((
                            Key::Cursive(glyph),
                            Value::Cursive(entry.clone(), exit.clone(), value & RIGHT_TO_LEFT != 0),
                        ));
                    }
                }
                _ => return None,
            }
        }
        Some(Written {
            name: lookup.name.clone(),
            index,
            facts,
        })
    }
}

/// Removes `markClass` statements for classes no feature code uses any more
fn remove_unused_mark_classes(font: &mut Font, names: &HashSet<SmolStr>) {
    let fea = font.features.to_fea();
    let unused: Vec<&SmolStr> = names
        .iter()
        .filter(|name| {
            let Ok(pattern) = regex::Regex::new(&format!(r"@{}\b", regex::escape(name))) else {
                return false;
            };
            fea.lines()
                .filter(|line| !line.trim_start().starts_with("markClass"))
                .all(|line| !pattern.is_match(line))
        })
        .collect();
    for code in font.features.prefixes.values_mut() {
        code.code = code
            .code
            .lines()
            .filter(|line| {
                let line = line.trim();
                !(line.starts_with("markClass")
                    && unused
                        .iter()
                        .any(|name| line.trim_end_matches(';').ends_with(&format!("@{name}"))))
            })
            .collect::<Vec<_>>()
            .join("\n");
    }
    font.features
        .prefixes
        .retain(|_, code| !code.code.trim().is_empty());
}

/// Last-wins attachment, as a shaper applies lookups in turn
fn effective<'a>(
    lookups: impl Iterator<Item = &'a Vec<(Key, Value)>>,
) -> HashMap<&'a Key, &'a Value> {
    let mut out = HashMap::new();
    for facts in lookups {
        for (key, value) in facts {
            out.insert(key, value);
        }
    }
    out
}

/// What moves, the writers to keep from generating, and the moved lookups
type Decided = (Decision, Vec<Writer>, HashSet<SmolStr>);

struct Decision {
    /// writers that generate, with the lookups they replace, in order
    moved: Vec<(Vec<Writer>, Vec<SmolStr>)>,
}

impl MoveMarksFromFeatures {
    /// Whether the compiler builds from the anchors, for `writers`, what the
    /// lookups `moved` make, under every language system that matters
    #[allow(clippy::too_many_arguments)]
    fn equivalent(
        &self,
        model: &Model,
        layout: &Layout,
        gpos: &ScriptList,
        generated: &Generated,
        written: &[Written],
        writers: &[Writer],
        moved: &HashSet<SmolStr>,
    ) -> bool {
        let generated_lookups: Vec<&Vec<(Key, Value)>> = generated
            .lookups
            .iter()
            .filter(|(w, _)| writers.contains(w))
            .map(|(_, f)| f)
            .collect();
        let any_generated = generated_lookups.iter().any(|f| !f.is_empty());
        // the language systems after: the compiler registers what it builds
        // under every declared one
        let mut after = gpos.clone();
        if any_generated {
            for (script, language) in &layout.declared {
                let added = after
                    .scripts
                    .entry(script.clone())
                    .or_default()
                    .insert(language.clone());
                if added {
                    let fine = !gpos.scripts.contains_key(script)
                        && ot_tag_script(script)
                            .is_some_and(|s| gpos.selected(&s) == after.selected(&s));
                    if !fine {
                        log::info!(
                            "Mark attachment stays feature code; a compiler would add {script} {language}"
                        );
                        return false;
                    }
                }
            }
        }
        // a language system left with no positioning changes what a shaper
        // chooses
        for (script, languages) in &gpos.scripts {
            for language in languages {
                let system: LangSys = (script.clone(), language.clone());
                let kept = layout.features.values().any(|systems| {
                    systems.get(&system).is_some_and(|lookups| {
                        lookups.iter().any(|l| {
                            !moved.contains(l) && layout.lookups.get(l).is_some_and(|i| i.gpos)
                        })
                    })
                });
                if !kept && !(any_generated && layout.declared.contains(&system)) {
                    log::info!(
                        "Mark attachment stays feature code; {script} {language} would lose its positioning"
                    );
                    return false;
                }
            }
        }
        let generated_effective = effective(generated_lookups.into_iter());
        let mut moved_written: Vec<&Written> =
            written.iter().filter(|w| moved.contains(&w.name)).collect();
        moved_written.sort_by_key(|w| w.index);
        // language systems by which moved lookups apply there, and whether the
        // compiler registers there
        let mut signatures: BTreeMap<(Vec<usize>, bool), Vec<LangSys>> = BTreeMap::new();
        for (script, languages) in &after.scripts {
            for language in languages {
                let system: LangSys = (script.clone(), language.clone());
                let applied: Vec<usize> = moved_written
                    .iter()
                    .enumerate()
                    .filter(|(_, w)| layout.applies(&w.name, &system))
                    .map(|(i, _)| i)
                    .collect();
                let registered = any_generated && layout.declared.contains(&system);
                signatures
                    .entry((applied, registered))
                    .or_default()
                    .push(system);
            }
        }
        let mut relevant_cache: HashMap<usize, BTreeSet<LangSys>> = HashMap::new();
        for ((applied, registered), systems) in signatures {
            let systems: BTreeSet<LangSys> = systems.into_iter().collect();
            let before = effective(
                applied
                    .iter()
                    .filter_map(|i| moved_written.get(*i))
                    .map(|w| &w.facts),
            );
            let empty = HashMap::new();
            let now = if registered {
                &generated_effective
            } else {
                &empty
            };
            for key in before.keys().chain(now.keys()) {
                if before.get(key) == now.get(key) {
                    continue;
                }
                let relevant = relevant_cache.entry(key.base()).or_insert_with(|| {
                    after.relevant(
                        model
                            .glyph_scripts
                            .get(key.base())
                            .unwrap_or(&BTreeSet::new()),
                    )
                });
                if !relevant.is_disjoint(&systems) {
                    log::info!(
                        "Mark attachment stays feature code; a compiler would build {key:?} as {:?}, not {:?}",
                        now.get(key),
                        before.get(key)
                    );
                    return false;
                }
            }
        }
        // a mark attached to a base and to a mark: the compiler attaches it to
        // the base first, then to the mark
        if writers.contains(&Writer::Mark) && writers.contains(&Writer::Mkmk) {
            let mut first_on_mark: HashMap<usize, usize> = HashMap::new();
            let mut last_on_base: HashMap<usize, usize> = HashMap::new();
            for w in &moved_written {
                for (key, _) in &w.facts {
                    match key {
                        Key::Mark(_, m) => {
                            first_on_mark.entry(*m).or_insert(w.index);
                        }
                        Key::Base(_, m) | Key::Ligature(_, _, m) => {
                            last_on_base.insert(*m, w.index);
                        }
                        Key::Cursive(_) => {}
                    }
                }
            }
            if first_on_mark
                .iter()
                .any(|(m, i)| last_on_base.get(m).is_some_and(|j| j > i))
            {
                log::info!("Mark attachment stays feature code; it attaches to marks before bases");
                return false;
            }
        }
        true
    }

    fn decide(
        &self,
        font: &Font,
        file: &FeatureFile,
    ) -> Result<Option<Decided>, crate::BabelfontError> {
        let mut expander = Expander {
            font,
            classes: HashMap::new(),
        };
        let layout = Layout::new(file, &mut expander);
        if ["mark", "mkmk", "abvm", "blwm", "curs"]
            .iter()
            .any(|t| layout.marked_features.contains(*t))
        {
            log::info!("Attachment features have insertion markers; mark attachment stays");
            return Ok(None);
        }
        let props = match &self.props {
            Some(props) => props.clone(),
            None => GlyphProps::new(font, None),
        };
        let Some(model) = Model::new(font, &props, &layout) else {
            return Ok(None);
        };
        if model.irregular {
            log::info!("Mark attachment stays feature code; anchors differ between masters");
            return Ok(None);
        }
        let generated = model.generate();
        let mut mark_classes: HashMap<SmolStr, Vec<(Vec<SmolStr>, FeaAnchor)>> = HashMap::new();
        for item in file.iter() {
            if let ToplevelItem::MarkClassDefinition(definition) = item {
                mark_classes
                    .entry(definition.mark_class.name.clone())
                    .or_default()
                    .push((
                        expander.expand(&definition.glyphs),
                        definition.anchor.clone(),
                    ));
            }
        }
        let mut reader = Reader {
            expander,
            props: &props,
            values: MasterValues::new(font)?,
            mark_classes,
        };
        let blocks = lookup_blocks(file);
        let mut written: Vec<Written> = vec![];
        for (name, block) in &blocks {
            let Some(info) = layout.lookups.get(name) else {
                continue;
            };
            if !info.gpos {
                continue;
            }
            let attaches = block.statements.iter().any(|s| {
                matches!(
                    s,
                    Statement::MarkBasePos(_)
                        | Statement::MarkLigPos(_)
                        | Statement::MarkMarkPos(_)
                        | Statement::CursivePos(_)
                )
            });
            if !attaches {
                continue;
            }
            let all_features: Vec<&str> = layout.features.keys().map(|k| k.as_str()).collect();
            if !layout.only_in(name, &all_features) {
                // called from a contextual rule or a feature variation: it
                // stays, and so does what its anchors would build
                written.push(Written {
                    name: name.clone(),
                    index: info.index,
                    facts: vec![],
                });
                continue;
            }
            match reader.read(block, info.index) {
                Some(w) => written.push(w),
                None => {
                    log::info!(
                        "Lookup {name} stays feature code; a compiler would write it otherwise"
                    );
                    written.push(Written {
                        name: name.clone(),
                        index: info.index,
                        facts: vec![],
                    });
                }
            }
        }
        let readable: HashSet<&SmolStr> = written
            .iter()
            .filter(|w| {
                !w.facts.is_empty()
                    || blocks.get(&w.name).is_some_and(|b| {
                        b.statements.iter().all(|s| {
                            matches!(
                                s,
                                Statement::Comment(_)
                                    | Statement::LookupFlag(_)
                                    | Statement::Subtable(_)
                            )
                        })
                    })
            })
            .map(|w| &w.name)
            .collect();
        let unreadable: HashSet<&SmolStr> = written
            .iter()
            .filter(|w| !readable.contains(&w.name))
            .map(|w| &w.name)
            .collect();
        let writer_of =
            |w: &Written| -> BTreeSet<Writer> { w.facts.iter().map(|(k, _)| k.writer()).collect() };
        let gpos = layout.gpos_systems();
        // the lookups each try moves: those whose attachment is all of its
        // writers'; any lookup of its writers that cannot be read stops it
        let try_writers = |writers: &[Writer]| -> Option<HashSet<SmolStr>> {
            let mut moved = HashSet::new();
            for w in &written {
                let kinds = writer_of(w);
                if unreadable.contains(&w.name) {
                    continue;
                }
                if !kinds.is_empty() && kinds.iter().all(|k| writers.contains(k)) {
                    moved.insert(w.name.clone());
                }
            }
            if moved.is_empty() {
                return None;
            }
            self.equivalent(
                &model, &layout, &gpos, &generated, &written, writers, &moved,
            )
            .then_some(moved)
        };
        let mut chosen: Vec<(Vec<Writer>, Vec<SmolStr>)> = vec![];
        let mut all_moved: HashSet<SmolStr> = HashSet::new();
        let mut generate: Vec<Writer> = vec![];
        let mut add = |writers: &[Writer],
                       moved: HashSet<SmolStr>,
                       chosen: &mut Vec<(Vec<Writer>, Vec<SmolStr>)>| {
            let mut names: Vec<SmolStr> = moved.iter().cloned().collect();
            names.sort_by_key(|n| layout.lookups.get(n).map(|l| l.index));
            chosen.push((writers.to_vec(), names));
            generate.extend_from_slice(writers);
            all_moved.extend(moved);
        };
        if let Some(moved) = try_writers(&[Writer::Mark, Writer::Mkmk]) {
            add(&[Writer::Mark, Writer::Mkmk], moved, &mut chosen);
        } else {
            for writer in [Writer::Mark, Writer::Mkmk] {
                if let Some(moved) = try_writers(&[writer]) {
                    add(&[writer], moved, &mut chosen);
                }
            }
        }
        if let Some(moved) = try_writers(&[Writer::Curs]) {
            add(&[Writer::Curs], moved, &mut chosen);
        }
        // generated lookups go where each writer's first moved lookup was
        let mut positions: HashMap<SmolStr, usize> = HashMap::new();
        for (_, names) in &chosen {
            let first = names
                .iter()
                .filter_map(|n| layout.lookups.get(n).map(|l| l.index))
                .min()
                .unwrap_or(usize::MAX);
            for name in names {
                positions.insert(name.clone(), first);
            }
        }
        let mut layout = layout;
        // kerning a compiler generates: it adjusts the kerned glyphs
        let kerned: HashSet<SmolStr> = font
            .masters
            .iter()
            .flat_map(|m| m.kerning.keys())
            .flat_map(|(l, r)| [l, r])
            .flat_map(|side| match side.strip_prefix('@') {
                Some(group) => font
                    .first_kern_groups
                    .get(group)
                    .into_iter()
                    .chain(font.second_kern_groups.get(group))
                    .flatten()
                    .cloned()
                    .collect::<Vec<_>>(),
                None => vec![side.clone()],
            })
            .collect();
        for (name, info) in layout.lookups.iter_mut() {
            if name.ends_with("#generated") {
                info.kinds = [0].into();
                info.touched = kerned.clone();
            }
        }
        if !chosen.is_empty() && !layout.order_kept(&positions) {
            log::info!(
                "Mark attachment stays feature code; it would change places with other lookups"
            );
            chosen.clear();
            all_moved.clear();
            generate.clear();
        }
        // what the compiler would build for the writers that do not generate
        let mut suppress: Vec<Writer> = vec![];
        for writer in [Writer::Mark, Writer::Mkmk, Writer::Curs] {
            if !generate.contains(&writer)
                && generated
                    .lookups
                    .iter()
                    .any(|(w, facts)| *w == writer && !facts.is_empty())
            {
                suppress.push(writer);
            }
        }
        Ok(Some((Decision { moved: chosen }, suppress, all_moved)))
    }
}

impl FontFilter for MoveMarksFromFeatures {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        let has_anchors = font
            .glyphs
            .iter()
            .any(|g| g.layers.iter().any(|l| !l.anchors.is_empty()));
        if !has_anchors {
            return Ok(());
        }
        let fea = font.features.to_fea();
        let glyph_names: Vec<&str> = font.glyphs.iter().map(|g| g.name.as_str()).collect();
        let file = match FeatureFile::new_from_fea(&fea, Some(&glyph_names), font.source.clone()) {
            Ok(file) => file,
            Err(e) => {
                log::warn!("Mark attachment stays feature code; the features do not parse: {e}");
                return Ok(());
            }
        };
        let Some((decision, suppress, moved)) = self.decide(font, &file)? else {
            return Ok(());
        };
        let mark_class_names: HashSet<SmolStr> = file
            .iter()
            .filter_map(|item| match item {
                ToplevelItem::MarkClassDefinition(d) => Some(d.mark_class.name.clone()),
                _ => None,
            })
            .collect();
        if !moved.is_empty() {
            let mut markers: Vec<(SmolStr, Vec<&str>)> = vec![];
            for (writers, names) in &decision.moved {
                let Some(first) = names.first() else { continue };
                let tags: Vec<&str> = writers
                    .iter()
                    .flat_map(|w| w.tags().iter().copied())
                    .collect();
                markers.push((first.clone(), tags));
            }
            let markers_ref = &markers;
            if !remove_lookup_blocks(&mut font.features, &moved, markers_ref) {
                font.features = remove_lookups_from_file(&file, &moved, markers_ref);
            }
            remove_unused_mark_classes(font, &mark_class_names);
        }
        // keep the compiler from building attachment the code states otherwise
        let present: HashSet<String> = {
            let fea = font.features.to_fea();
            let mut out = HashSet::new();
            for tag in ["abvm", "blwm", "mark", "mkmk", "curs"] {
                if regex::Regex::new(&format!(r"\bfeature\s+{tag}\b"))
                    .is_ok_and(|p| p.is_match(&fea))
                {
                    out.insert(tag.to_string());
                }
            }
            out
        };
        for writer in suppress {
            for tag in writer.tags() {
                if !present.contains(*tag) {
                    font.features.features.push((
                        (*tag).into(),
                        crate::features::PossiblyAutomaticCode::new(
                            "# The feature code states this feature; nothing is generated",
                        ),
                    ));
                }
            }
        }
        Ok(())
    }

    fn from_str(_s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(MoveMarksFromFeatures::new())
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        clap::Arg::new("movemarksfromfeatures")
            .long("move-marks-from-features")
            .help("Move mark attachment the anchors state out of the feature code")
            .action(clap::ArgAction::SetTrue)
    }
}

#[cfg(all(test, feature = "ttf"))]
#[allow(clippy::unwrap_used, clippy::indexing_slicing)]
mod tests {
    use super::*;
    use crate::{Anchor, Axis, Features, Glyph, Layer, Master, Tag};
    use fontdrasil::coords::{DesignCoord, DesignLocation, UserCoord};

    /// A variable font of many bases and marks, attachment written into the
    /// feature code as a decompiler writes it
    fn many_marks(bases: usize, marks: usize) -> Font {
        let mut font = Font::new();
        let mut axis = Axis::new("Weight", Tag::new(b"wght"));
        axis.min = Some(UserCoord::new(100.0));
        axis.default = Some(UserCoord::new(400.0));
        axis.max = Some(UserCoord::new(900.0));
        font.axes.push(axis);
        let weights = [100.0, 400.0, 900.0];
        for w in weights {
            let mut location = DesignLocation::new();
            location.insert(Tag::new(b"wght"), DesignCoord::new(w));
            font.masters
                .push(Master::new(format!("w{w}"), format!("m{w}"), location));
        }
        let anchor = |name: &str, x: f64, y: f64| Anchor {
            id: None,
            name: name.into(),
            x,
            y,
            format_specific: Default::default(),
        };
        let metric = |v: f64| {
            format!(
                "(wght=100:{} wght=400:{} wght=900:{})",
                v,
                v + 10.0,
                v + 30.0
            )
        };
        let mut fea = String::from("languagesystem DFLT dflt;\nlanguagesystem latn dflt;\n");
        for m in 0..marks {
            let mut glyph = Glyph::new(&format!("mark{m}"));
            glyph.codepoints = vec![0x300 + m as u32];
            glyph.category = GlyphCategory::Mark;
            for (i, master) in font.masters.iter().enumerate() {
                let mut layer = Layer::new(0.0);
                layer.master = LayerType::DefaultForMaster(master.id.clone());
                layer
                    .anchors
                    .push(anchor("_top", 100.0 + [0.0, 10.0, 30.0][i], 500.0));
                glyph.layers.push(layer);
            }
            font.glyphs.push(glyph);
            fea.push_str(&format!(
                "markClass mark{m} <anchor {} 500> @top;\n",
                metric(100.0)
            ));
        }
        fea.push_str("lookup m {\n");
        for b in 0..bases {
            let mut glyph = Glyph::new(&format!("base{b}"));
            glyph.codepoints = vec![0x4E00 + b as u32];
            glyph.category = GlyphCategory::Base;
            let x = (b % 500) as f64;
            for (i, master) in font.masters.iter().enumerate() {
                let mut layer = Layer::new(600.0);
                layer.master = LayerType::DefaultForMaster(master.id.clone());
                layer
                    .anchors
                    .push(anchor("top", x + [0.0, 10.0, 30.0][i], 700.0));
                glyph.layers.push(layer);
            }
            font.glyphs.push(glyph);
            fea.push_str(&format!(
                "    pos base base{b} <anchor {} 700> mark @top;\n",
                metric(x)
            ));
        }
        fea.push_str("} m;\nfeature mark { lookup m; } mark;\n");
        font.features = Features::from_fea(&fea);
        font
    }

    #[test]
    fn test_many_marks_move_within_a_budget() {
        // 2000 bases by 60 marks: 120,000 attachments to compare, at three
        // masters. Reading each anchor's names at each master per attachment
        // took minutes on SFNS.
        let mut font = many_marks(2000, 60);
        let props = GlyphProps::new(&font, None);
        let start = std::time::Instant::now();
        MoveMarksFromFeatures::with_glyph_props(props)
            .apply(&mut font)
            .unwrap();
        let elapsed = start.elapsed();
        assert!(
            !font.features.to_fea().contains("pos base"),
            "{}",
            font.features.to_fea()
        );
        assert!(
            elapsed < std::time::Duration::from_secs(20),
            "took {elapsed:?}"
        );
    }

    #[test]
    fn test_an_anchor_off_its_attachment_keeps_it() {
        let mut font = many_marks(20, 3);
        let layer = &mut font.glyphs.get_mut("base7").unwrap().layers[2];
        layer.anchors[0].x += 1.0;
        let mut marked = many_marks(20, 3);
        let layer = &mut marked.glyphs.get_mut("mark1").unwrap().layers[2];
        layer.anchors[0].x += 1.0;
        let props = GlyphProps::new(&marked, None);
        MoveMarksFromFeatures::with_glyph_props(props)
            .apply(&mut marked)
            .unwrap();
        assert!(marked.features.to_fea().contains("pos base base7"));
        let props = GlyphProps::new(&font, None);
        MoveMarksFromFeatures::with_glyph_props(props)
            .apply(&mut font)
            .unwrap();
        assert!(font.features.to_fea().contains("pos base base7"));
    }
}

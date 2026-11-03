//! What compiled feature code does, as far as the filters that move rules out
//! of it for a compiler to regenerate need to know: which lookups apply under
//! which script and language, in what order, which glyphs they position, which
//! script record a shaper chooses for a run of text, and what scripts and
//! directions a compiler finds for each glyph.
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::Arc;

use fea_rs_ast::{
    AsFea, FeatureFile, GlyphContainer, LookupBlock, Metric, Statement, ToplevelItem,
};
use smol_str::SmolStr;

use crate::{Font, GlyphCategory, layout::expand::expand_glyph_container};

/// A script tag and a language tag
pub(crate) type LangSys = (SmolStr, SmolStr);

pub(crate) const DFLT_SCRIPT: &str = "DFLT";
pub(crate) const DFLT_LANG: &str = "dflt";

pub(crate) const RIGHT_TO_LEFT: u16 = 0x0001;
pub(crate) const IGNORE_MARKS: u16 = 0x0008;
pub(crate) const USE_MARK_FILTERING_SET: u16 = 0x0010;

/// The comment that tells a compiler where to put the lookups it generates for
/// a feature (ufo2ft's; fontc skips generating a feature the code states
/// without it)
pub(crate) const INSERTION_MARKER: &str = "# Automatic Code";

/// A lookup's flags, its glyph sets expanded
#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct Flags {
    pub value: u16,
    pub attachment: Option<BTreeSet<SmolStr>>,
    pub filter: Option<BTreeSet<SmolStr>>,
}

/// What a lookup does to glyph positions, as far as the order of lookups goes
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Kind {
    /// Single and pair adjustments: they add up, in any order
    Adjust,
    /// Cursive and mark attachment: they set positions
    Attach,
    /// Substitution, or nothing known
    Other,
}

/// A lookup as compiled: where it is in the lookup list and what it does
#[derive(Clone, Debug)]
pub(crate) struct LookupInfo {
    pub index: usize,
    pub gpos: bool,
    pub flags: Flags,
    pub kinds: BTreeSet<KindKey>,
    /// glyphs whose position the lookup sets or adjusts
    pub touched: HashSet<SmolStr>,
    /// lookups a contextual rule calls
    pub nested: Vec<SmolStr>,
}

/// [`Kind`] as an ordered key
pub(crate) type KindKey = u8;

fn kind_key(kind: Kind) -> KindKey {
    match kind {
        Kind::Adjust => 0,
        Kind::Attach => 1,
        Kind::Other => 2,
    }
}

impl LookupInfo {
    pub(crate) fn only_adjusts(&self) -> bool {
        self.kinds.iter().all(|k| *k == kind_key(Kind::Adjust))
    }
}

/// Glyph classes expanded, as feature code names them
pub(crate) struct Expander<'a> {
    pub font: &'a Font,
    pub classes: HashMap<SmolStr, Vec<SmolStr>>,
}

impl Expander<'_> {
    pub(crate) fn expand(&self, container: &GlyphContainer) -> Vec<SmolStr> {
        let mut glyphs = expand_glyph_container(self.font, container, &self.classes, None);
        let mut seen = HashSet::new();
        glyphs.retain(|g| seen.insert(g.clone()));
        glyphs
    }
}

/// Per-feature state while resolving a feature block, as fea-rs does
struct ActiveFeature<'a> {
    default_systems: &'a [LangSys],
    current: Option<LangSys>,
    script: Option<SmolStr>,
    lookups: HashMap<LangSys, Vec<SmolStr>>,
    script_defaults: HashMap<SmolStr, Vec<SmolStr>>,
}

fn default_key() -> LangSys {
    (DFLT_SCRIPT.into(), DFLT_LANG.into())
}

impl<'a> ActiveFeature<'a> {
    fn new(default_systems: &'a [LangSys]) -> Self {
        ActiveFeature {
            default_systems,
            current: None,
            script: None,
            lookups: HashMap::new(),
            script_defaults: HashMap::new(),
        }
    }

    fn set_system(&mut self, system: LangSys, exclude_dflt: bool) {
        if system.1 != DFLT_LANG {
            let mut lookups = vec![];
            if !exclude_dflt {
                let script_dflt: LangSys = (system.0.clone(), DFLT_LANG.into());
                if self.default_systems.contains(&system)
                    || (self.default_systems.contains(&script_dflt)
                        && self.script_defaults.contains_key(&system.0))
                {
                    lookups.extend(
                        self.lookups
                            .get(&default_key())
                            .into_iter()
                            .flatten()
                            .cloned(),
                    );
                }
                lookups.extend(
                    self.script_defaults
                        .get(&system.0)
                        .into_iter()
                        .flatten()
                        .cloned(),
                );
            }
            self.lookups.entry(system.clone()).or_insert(lookups);
        }
        self.current = Some(system);
    }

    fn script(&mut self, tag: &str) {
        if self.current == Some((tag.into(), DFLT_LANG.into())) {
            return;
        }
        self.script = Some(tag.into());
        self.set_system((tag.into(), DFLT_LANG.into()), false);
    }

    fn language(&mut self, tag: &str, include_dflt: bool) {
        let script = self.script.clone().unwrap_or_else(|| DFLT_SCRIPT.into());
        self.set_system((script, tag.into()), !include_dflt);
    }

    fn add(&mut self, lookup: SmolStr) {
        match &self.current {
            Some(system) if system.1 == DFLT_LANG => self
                .script_defaults
                .entry(system.0.clone())
                .or_default()
                .push(lookup),
            current => self
                .lookups
                .entry(current.clone().unwrap_or_else(default_key))
                .or_default()
                .push(lookup),
        }
    }

    fn finish(mut self) -> HashMap<LangSys, Vec<SmolStr>> {
        let defaults = self.lookups.remove(&default_key()).unwrap_or_default();
        for (script, lookups) in std::mem::take(&mut self.script_defaults) {
            let system: LangSys = (script, DFLT_LANG.into());
            let lookups = if self.default_systems.contains(&system) {
                defaults.iter().cloned().chain(lookups).collect()
            } else {
                lookups
            };
            self.lookups.insert(system, lookups);
        }
        for system in self.default_systems {
            self.lookups
                .entry(system.clone())
                .or_insert_with(|| defaults.clone());
        }
        self.lookups
    }
}

/// Which lookups each feature applies under each language system, the lookups
/// as compiled, and what refers to them
pub(crate) struct Layout {
    /// The `languagesystem` statements, or DFLT dflt
    pub declared: Vec<LangSys>,
    /// feature tag, then language system, then lookups
    pub features: BTreeMap<SmolStr, BTreeMap<LangSys, BTreeSet<SmolStr>>>,
    pub lookups: HashMap<SmolStr, LookupInfo>,
    /// the features that refer to each lookup
    pub feature_refs: HashMap<SmolStr, BTreeSet<SmolStr>>,
    /// how many contextual rules call each lookup
    pub contextual_refs: HashMap<SmolStr, usize>,
    /// lookups referred to from feature variations
    pub variation_refs: HashSet<SmolStr>,
    /// features with an insertion marker
    pub marked_features: HashSet<SmolStr>,
}

fn rule_kind(statement: &Statement) -> Option<(bool, Kind)> {
    Some(match statement {
        Statement::SingleSubst(_)
        | Statement::MultipleSubst(_)
        | Statement::AlternateSubst(_)
        | Statement::LigatureSubst(_)
        | Statement::ReverseChainSubst(_)
        | Statement::ChainedContextSubst(_)
        | Statement::IgnoreSubst(_) => (false, Kind::Other),
        Statement::SinglePos(_) | Statement::PairPos(_) => (true, Kind::Adjust),
        Statement::CursivePos(_)
        | Statement::MarkBasePos(_)
        | Statement::MarkLigPos(_)
        | Statement::MarkMarkPos(_) => (true, Kind::Attach),
        Statement::ChainedContextPos(_) | Statement::IgnorePos(_) => (true, Kind::Adjust),
        _ => return None,
    })
}

/// What a lookup's statements do: positions set, flags, nested lookups
fn describe(
    statements: &[Statement],
    expander: &Expander,
    mark_classes: &HashMap<SmolStr, Vec<SmolStr>>,
    index: usize,
) -> LookupInfo {
    let mut info = LookupInfo {
        index,
        gpos: false,
        flags: Flags::default(),
        kinds: BTreeSet::new(),
        touched: HashSet::new(),
        nested: vec![],
    };
    let marks_of = |classes: &mut dyn Iterator<Item = &SmolStr>| -> Vec<SmolStr> {
        classes
            .flat_map(|name| mark_classes.get(name).into_iter().flatten().cloned())
            .collect()
    };
    let mut seen: HashSet<String> = HashSet::new();
    for statement in statements {
        if let Statement::LookupFlag(flag) = statement {
            info.flags = Flags {
                value: flag.value
                    | if flag.mark_filtering_set.is_some() {
                        USE_MARK_FILTERING_SET
                    } else {
                        0
                    },
                attachment: flag
                    .mark_attachment
                    .as_ref()
                    .map(|c| expander.expand(c).into_iter().collect()),
                filter: flag
                    .mark_filtering_set
                    .as_ref()
                    .map(|c| expander.expand(c).into_iter().collect()),
            };
            continue;
        }
        let Some((gpos, kind)) = rule_kind(statement) else {
            continue;
        };
        info.gpos |= gpos;
        info.kinds.insert(kind_key(kind));
        // a class in many rules is expanded once
        let mut touch = |container: &GlyphContainer, info: &mut LookupInfo| {
            let key = match container {
                GlyphContainer::GlyphName(name) => {
                    info.touched.insert(name.name.clone());
                    return;
                }
                GlyphContainer::GlyphClassName(name) => name.to_string(),
                other => other.as_fea(""),
            };
            if seen.insert(key) {
                info.touched.extend(expander.expand(container));
            }
        };
        match statement {
            Statement::SinglePos(pos) => {
                for (glyphs, _) in &pos.pos {
                    touch(glyphs, &mut info);
                }
            }
            Statement::PairPos(pos) => {
                touch(&pos.glyphs_1, &mut info);
                touch(&pos.glyphs_2, &mut info);
            }
            Statement::CursivePos(pos) => touch(&pos.glyphclass, &mut info),
            Statement::MarkBasePos(pos) => {
                info.touched
                    .extend(marks_of(&mut pos.marks.iter().map(|(_, c)| &c.name)));
            }
            Statement::MarkMarkPos(pos) => {
                info.touched
                    .extend(marks_of(&mut pos.marks.iter().map(|(_, c)| &c.name)));
            }
            Statement::MarkLigPos(pos) => {
                info.touched.extend(marks_of(
                    &mut pos.marks.iter().flatten().map(|(_, c)| &c.name),
                ));
            }
            Statement::ChainedContextPos(pos) => {
                info.nested.extend(pos.lookups.iter().flatten().cloned());
            }
            Statement::ChainedContextSubst(sub) => {
                info.nested.extend(sub.lookups.iter().flatten().cloned());
            }
            _ => {}
        }
    }
    if info.flags.value & IGNORE_MARKS != 0 {
        info.touched.retain(|g| {
            expander
                .font
                .glyphs
                .get(g)
                .is_none_or(|g| g.category != GlyphCategory::Mark)
        });
    }
    info
}

impl Layout {
    pub(crate) fn new(file: &FeatureFile, expander: &mut Expander) -> Self {
        let mut declared: Vec<LangSys> = vec![];
        let mut mark_classes: HashMap<SmolStr, Vec<SmolStr>> = HashMap::new();
        let mut lookups: HashMap<SmolStr, LookupInfo> = HashMap::new();
        let mut index = 0;
        // Definitions first: classes, mark classes, lookups (lookups in
        // feature blocks among them, in the order they compile)
        for item in file.iter() {
            match item {
                ToplevelItem::LanguageSystem(ls) => {
                    let system: LangSys = (ls.script.as_str().into(), ls.language.as_str().into());
                    if !declared.contains(&system) {
                        declared.push(system);
                    }
                }
                ToplevelItem::GlyphClassDefinition(definition) => {
                    let glyphs =
                        expander.expand(&GlyphContainer::GlyphClass(definition.glyphs.clone()));
                    expander
                        .classes
                        .insert(definition.name.trim_start_matches('@').into(), glyphs);
                }
                ToplevelItem::MarkClassDefinition(definition) => {
                    let marks = expander.expand(&definition.glyphs);
                    mark_classes
                        .entry(definition.mark_class.name.clone())
                        .or_default()
                        .extend(marks);
                }
                ToplevelItem::Lookup(lookup) => {
                    lookups.insert(
                        lookup.name.clone(),
                        describe(&lookup.statements, expander, &mark_classes, index),
                    );
                    index += 1;
                }
                ToplevelItem::Feature(feature) => {
                    let mut anonymous = 0;
                    let mut run: Vec<Statement> = vec![];
                    let mut flush = |run: &mut Vec<Statement>,
                                     lookups: &mut HashMap<SmolStr, LookupInfo>,
                                     index: &mut usize| {
                        if run.iter().any(|s| rule_kind(s).is_some()) {
                            anonymous += 1;
                            lookups.insert(
                                format!("{}#{anonymous}", feature.name).into(),
                                describe(run, expander, &mark_classes, *index),
                            );
                            *index += 1;
                        }
                        run.clear();
                    };
                    for statement in &feature.statements {
                        match statement {
                            Statement::LookupBlock(lookup) => {
                                flush(&mut run, &mut lookups, &mut index);
                                lookups.insert(
                                    lookup.name.clone(),
                                    describe(&lookup.statements, expander, &mark_classes, index),
                                );
                                index += 1;
                            }
                            Statement::Script(_)
                            | Statement::Language(_)
                            | Statement::LookupReference(_)
                            | Statement::Subtable(_) => flush(&mut run, &mut lookups, &mut index),
                            Statement::Comment(comment)
                                if comment.text.contains(INSERTION_MARKER) =>
                            {
                                flush(&mut run, &mut lookups, &mut index);
                                // generated lookups go here
                                lookups.insert(
                                    format!("{}#generated", feature.name).into(),
                                    LookupInfo {
                                        index,
                                        gpos: true,
                                        flags: Flags::default(),
                                        kinds: BTreeSet::new(),
                                        touched: HashSet::new(),
                                        nested: vec![],
                                    },
                                );
                            }
                            other => run.push(other.clone()),
                        }
                    }
                    flush(&mut run, &mut lookups, &mut index);
                }
                _ => {}
            }
        }
        if declared.is_empty() {
            declared.push(default_key());
        }
        // Contextual rules: their kinds and positions are their nested lookups'
        let names: Vec<SmolStr> = lookups.keys().cloned().collect();
        let mut contextual_refs: HashMap<SmolStr, usize> = HashMap::new();
        for name in &names {
            let nested = lookups
                .get(name)
                .map(|l| l.nested.clone())
                .unwrap_or_default();
            for called in &nested {
                *contextual_refs.entry(called.clone()).or_default() += 1;
            }
            let mut kinds = BTreeSet::new();
            let mut touched = HashSet::new();
            let mut gpos = false;
            for called in &nested {
                if let Some(info) = lookups.get(called) {
                    kinds.extend(info.kinds.iter().copied());
                    touched.extend(info.touched.iter().cloned());
                    gpos |= info.gpos;
                }
            }
            if let Some(info) = lookups.get_mut(name) {
                info.kinds.extend(kinds);
                info.touched.extend(touched);
                info.gpos |= gpos;
            }
        }

        // Then what each feature applies where
        let mut features: BTreeMap<SmolStr, BTreeMap<LangSys, BTreeSet<SmolStr>>> = BTreeMap::new();
        let mut feature_refs: HashMap<SmolStr, BTreeSet<SmolStr>> = HashMap::new();
        let mut variation_refs = HashSet::new();
        let mut marked_features = HashSet::new();
        for item in file.iter() {
            match item {
                ToplevelItem::Feature(feature) => {
                    let mut active = ActiveFeature::new(&declared);
                    let mut anonymous = 0;
                    let mut in_run = false;
                    for statement in &feature.statements {
                        let mut reference: Option<SmolStr> = None;
                        match statement {
                            Statement::Script(script) => {
                                in_run = false;
                                active.script(&script.tag);
                            }
                            Statement::Language(language) => {
                                in_run = false;
                                active.language(&language.tag, language.include_dflt);
                            }
                            Statement::LookupReference(r) => {
                                in_run = false;
                                reference = Some(r.lookup_name.as_str().into());
                            }
                            Statement::LookupBlock(lookup) => {
                                in_run = false;
                                reference = Some(lookup.name.clone());
                            }
                            Statement::Subtable(_) => in_run = false,
                            Statement::Comment(comment) => {
                                if comment.text.contains(INSERTION_MARKER) {
                                    marked_features.insert(feature.name.clone());
                                    in_run = false;
                                }
                            }
                            other if rule_kind(other).is_some() && !in_run => {
                                anonymous += 1;
                                in_run = true;
                                reference = Some(format!("{}#{anonymous}", feature.name).into());
                            }
                            _ => {}
                        }
                        if let Some(name) = reference {
                            feature_refs
                                .entry(name.clone())
                                .or_default()
                                .insert(feature.name.clone());
                            active.add(name);
                        }
                    }
                    let entry = features.entry(feature.name.clone()).or_default();
                    for (system, lookups) in active.finish() {
                        entry.entry(system).or_default().extend(lookups);
                    }
                }
                ToplevelItem::VariationBlock(block) => {
                    for statement in &block.statements {
                        if let Statement::LookupReference(r) = statement {
                            variation_refs.insert(SmolStr::from(r.lookup_name.as_str()));
                        }
                    }
                }
                _ => {}
            }
        }
        Layout {
            declared,
            features,
            lookups,
            feature_refs,
            contextual_refs,
            variation_refs,
            marked_features,
        }
    }

    /// Whether a lookup is applied by a feature at a language system
    pub(crate) fn applies(&self, lookup: &str, system: &LangSys) -> bool {
        self.features
            .values()
            .any(|systems| systems.get(system).is_some_and(|l| l.contains(lookup)))
    }

    /// The language systems of the GPOS table, by script: where some feature
    /// with positioning lookups is registered
    pub(crate) fn gpos_systems(&self) -> ScriptList {
        let mut scripts: BTreeMap<SmolStr, BTreeSet<SmolStr>> = BTreeMap::new();
        for systems in self.features.values() {
            for (system, lookups) in systems {
                if lookups
                    .iter()
                    .any(|l| self.lookups.get(l).is_some_and(|i| i.gpos))
                {
                    scripts
                        .entry(system.0.clone())
                        .or_default()
                        .insert(system.1.clone());
                }
            }
        }
        ScriptList { scripts }
    }

    /// The lookups a feature refers to that nothing else refers to: no other
    /// feature, contextual rule or feature variation
    pub(crate) fn only_in(&self, lookup: &str, tags: &[&str]) -> bool {
        self.feature_refs
            .get(lookup)
            .is_some_and(|f| !f.is_empty() && f.iter().all(|t| tags.contains(&t.as_str())))
            && !self.contextual_refs.contains_key(lookup)
            && !self.variation_refs.contains(lookup)
    }

    /// Languages declared for a script, or dflt
    pub(crate) fn declared_languages(&self, script: &str) -> Vec<SmolStr> {
        let languages: Vec<SmolStr> = self
            .declared
            .iter()
            .filter(|(s, _)| s == script)
            .map(|(_, l)| l.clone())
            .collect();
        if languages.is_empty() {
            vec![DFLT_LANG.into()]
        } else {
            languages
        }
    }

    /// Whether moving lookups to where a compiler generates them keeps every
    /// pair of lookups whose order matters in its order. `moved` maps each
    /// moved lookup to the position its generated lookups take (the index of
    /// the lookup the insertion marker comes before; `usize::MAX` for the end
    /// of the list).
    pub(crate) fn order_kept(&self, moved: &HashMap<SmolStr, usize>) -> bool {
        let referenced: Vec<(&SmolStr, &LookupInfo)> = self
            .lookups
            .iter()
            .filter(|(name, info)| {
                info.gpos && (self.feature_refs.contains_key(*name) || name.ends_with("#generated"))
            })
            .collect();
        let position = |name: &SmolStr, info: &LookupInfo| -> (usize, usize) {
            match moved.get(name) {
                Some(p) => (*p, 0),
                None => (info.index, 1),
            }
        };
        for (a, ai) in &referenced {
            for (b, bi) in &referenced {
                if ai.index >= bi.index || (!moved.contains_key(*a) && !moved.contains_key(*b)) {
                    continue;
                }
                // generated lookups come before the lookup they are inserted at
                if position(a, ai) <= position(b, bi) {
                    continue;
                }
                let commute =
                    (ai.only_adjusts() && bi.only_adjusts()) || ai.touched.is_disjoint(&bi.touched);
                if !commute {
                    log::info!("Lookups {a} and {b} would change order");
                    return false;
                }
            }
        }
        true
    }
}

/// The scripts and languages of a GPOS table
#[derive(Clone, Debug, Default)]
pub(crate) struct ScriptList {
    pub scripts: BTreeMap<SmolStr, BTreeSet<SmolStr>>,
}

impl ScriptList {
    /// The script record a shaper uses for a run of a script (HarfBuzz's
    /// choice: the script's tags, then DFLT, dflt and latn)
    pub(crate) fn selected(&self, script: &str) -> Option<SmolStr> {
        hb_script_tags(script)
            .into_iter()
            .chain([DFLT_SCRIPT.into(), DFLT_LANG.into(), "latn".into()])
            .find(|tag| self.scripts.contains_key(tag))
    }

    /// The language systems a shaper may use for a run of text in any of these
    /// scripts (any language); every one of them for text of no script in
    /// particular
    pub(crate) fn relevant(&self, scripts: &BTreeSet<SmolStr>) -> BTreeSet<LangSys> {
        let records: Vec<SmolStr> =
            if scripts.is_empty() || scripts.iter().any(|s| s == COMMON || s == INHERITED) {
                self.scripts.keys().cloned().collect()
            } else {
                scripts.iter().filter_map(|s| self.selected(s)).collect()
            };
        records
            .into_iter()
            .flat_map(|record| {
                self.scripts
                    .get(&record)
                    .into_iter()
                    .flatten()
                    .map(move |l| (record.clone(), l.clone()))
            })
            .collect()
    }
}

pub(crate) const COMMON: &str = "Zyyy";
pub(crate) const INHERITED: &str = "Zinh";

const NEW_SCRIPT_TAGS: &[(&str, &str)] = &[
    ("Beng", "bng2"),
    ("Deva", "dev2"),
    ("Gujr", "gjr2"),
    ("Guru", "gur2"),
    ("Knda", "knd2"),
    ("Mlym", "mlm2"),
    ("Mymr", "mym2"),
    ("Orya", "ory2"),
    ("Taml", "tml2"),
    ("Telu", "tel2"),
];

const SCRIPT_EXCEPTIONS: &[(&str, &str)] = &[
    ("Hira", "kana"),
    ("Hrkt", "kana"),
    ("Laoo", "lao "),
    ("Nkoo", "nko "),
    ("Vaii", "vai "),
    ("Yiii", "yi  "),
    ("Zinh", "DFLT"),
    ("Zmth", "math"),
    ("Zyyy", "DFLT"),
    ("Zzzz", "DFLT"),
];

/// The OpenType tags fontc registers generated features under for a Unicode
/// script
pub(crate) fn fontc_script_tags(script: &str) -> Vec<SmolStr> {
    if let Some((_, tag)) = SCRIPT_EXCEPTIONS.iter().find(|(s, _)| *s == script) {
        return vec![(*tag).into()];
    }
    NEW_SCRIPT_TAGS
        .iter()
        .filter(|(s, _)| *s == script)
        .map(|(_, t)| SmolStr::from(*t))
        .chain(std::iter::once(script.to_ascii_lowercase().into()))
        .collect()
}

/// The OpenType tags HarfBuzz tries for a Unicode script, in order
pub(crate) fn hb_script_tags(script: &str) -> Vec<SmolStr> {
    if let Some((_, tag)) = SCRIPT_EXCEPTIONS.iter().find(|(s, _)| *s == script) {
        if *tag == DFLT_SCRIPT {
            return vec![];
        }
        return vec![(*tag).into()];
    }
    let mut tags = vec![];
    if let Some((_, new)) = NEW_SCRIPT_TAGS.iter().find(|(s, _)| *s == script) {
        let mut three = new.to_string();
        three.pop();
        three.push('3');
        tags.push(three.into());
        tags.push((*new).into());
    }
    tags.push(script.to_ascii_lowercase().into());
    tags
}

/// The Unicode script an OpenType script tag stands for
pub(crate) fn ot_tag_script(tag: &str) -> Option<SmolStr> {
    if tag == DFLT_SCRIPT {
        return None;
    }
    if tag == "math" {
        return Some("Zmth".into());
    }
    if let Some((script, _)) = NEW_SCRIPT_TAGS.iter().find(|(_, t)| *t == tag) {
        return Some((*script).into());
    }
    if let Some((script, _)) = SCRIPT_EXCEPTIONS
        .iter()
        .find(|(s, t)| *t == tag && !s.starts_with('Z') && *s != "Hira")
    {
        return Some((*script).into());
    }
    let mut chars: Vec<char> = tag.trim_end().chars().collect();
    let first = chars.first_mut()?;
    *first = first.to_ascii_uppercase();
    while chars.len() < 4 {
        let last = *chars.last()?;
        chars.push(last);
    }
    Some(chars.into_iter().collect::<String>().into())
}

pub(crate) const INDIC_SCRIPTS: &[&str] = &[
    "Beng", "Deva", "Gujr", "Guru", "Knda", "Mlym", "Orya", "Sinh", "Taml", "Telu",
];

pub(crate) const USE_SCRIPTS: &[&str] = &[
    "Adlm", "Ahom", "Bali", "Batk", "Bhks", "Brah", "Bugi", "Buhd", "Cakm", "Cham", "Chrs", "Cpmn",
    "Diak", "Dogr", "Dupl", "Egyp", "Elym", "Gong", "Gonm", "Gran", "Hano", "Hmng", "Hmnp", "Java",
    "Kali", "Kawi", "Khar", "Khoj", "Kits", "Kthi", "Lana", "Lepc", "Limb", "Mahj", "Maka", "Mand",
    "Mani", "Marc", "Medf", "Modi", "Mong", "Mtei", "Mult", "Nagm", "Nand", "Newa", "Nkoo", "Ougr",
    "Phag", "Phlp", "Plrd", "Rjng", "Rohg", "Saur", "Shrd", "Sidd", "Sind", "Sogd", "Sogo", "Soyo",
    "Sund", "Sylo", "Tagb", "Takr", "Tale", "Tavt", "Tfng", "Tglg", "Tibt", "Tirh", "Tnsa", "Toto",
    "Vith", "Wcho", "Yezi", "Zanb",
];

/// Scripts whose kerning fontc puts in `dist`
pub(crate) fn dist_script(script: &str) -> bool {
    INDIC_SCRIPTS.contains(&script)
        || USE_SCRIPTS.contains(&script)
        || script == "Khmr"
        || script == "Mymr"
}

/// Scripts whose mark attachment fontc puts in `abvm` and `blwm`
pub(crate) fn abvm_script(script: &str) -> bool {
    INDIC_SCRIPTS.contains(&script) || USE_SCRIPTS.contains(&script) || script == "Khmr"
}

/// Whether text of a script runs right to left, as fontc has it
pub(crate) fn right_to_left_script(script: &str) -> bool {
    matches!(
        script,
        "Arab"
            | "Hebr"
            | "Syrc"
            | "Thaa"
            | "Cprt"
            | "Khar"
            | "Phnx"
            | "Nkoo"
            | "Lydi"
            | "Avst"
            | "Armi"
            | "Phli"
            | "Prti"
            | "Sarb"
            | "Orkh"
            | "Samr"
            | "Mand"
            | "Merc"
            | "Mero"
            | "Mani"
            | "Mend"
            | "Nbat"
            | "Narb"
            | "Palm"
            | "Phlp"
            | "Hatr"
            | "Hung"
            | "Adlm"
            | "Rohg"
            | "Sogo"
            | "Sogd"
            | "Elym"
            | "Chrs"
            | "Yezi"
            | "Ougr"
    )
}

/// Runs a substitution closure: adds every glyph substitutions can reach from
/// the glyphs (by index)
pub(crate) type Closure = Arc<dyn Fn(&mut BTreeSet<usize>) + Send + Sync>;

/// Unicode properties of glyphs as a compiler finds them: from the codepoints
/// each glyph has, carried to the glyphs substitutions reach from them
#[derive(Clone)]
pub(crate) struct GlyphProps {
    /// glyph name to index
    pub index: HashMap<SmolStr, usize>,
    pub codepoints: Vec<Vec<u32>>,
    /// None where the substitutions cannot be followed: glyphs without a
    /// codepoint have no properties known
    pub closure: Option<Closure>,
}

/// A glyph property's values, by glyph index, as fontc's `classify` finds them
pub(crate) type Classified<T> = BTreeMap<T, BTreeSet<usize>>;

impl GlyphProps {
    pub(crate) fn new(font: &Font, closure: Option<Closure>) -> Self {
        GlyphProps {
            index: font
                .glyphs
                .iter()
                .enumerate()
                .map(|(i, g)| (g.name.clone(), i))
                .collect(),
            codepoints: font.glyphs.iter().map(|g| g.codepoints.clone()).collect(),
            closure,
        }
    }

    /// Whether a glyph's properties are known: it has a codepoint, or the
    /// substitutions that reach it are
    pub(crate) fn known(&self, glyph: &str) -> bool {
        self.closure.is_some()
            || self
                .index
                .get(glyph)
                .and_then(|i| self.codepoints.get(*i))
                .is_some_and(|c| !c.is_empty())
    }

    /// Glyphs by the values `props` gives their codepoints; glyphs reached
    /// from a glyph by substitution take its values, unless reachable from
    /// glyphs without any
    pub(crate) fn classify<T: Ord + Clone>(
        &self,
        props: impl Fn(u32, &mut Vec<T>),
    ) -> Classified<T> {
        let mut sets: Classified<T> = BTreeMap::new();
        let mut neutral: BTreeSet<usize> = BTreeSet::new();
        let mut buf = vec![];
        for (gid, codepoints) in self.codepoints.iter().enumerate() {
            for cp in codepoints {
                props(*cp, &mut buf);
                if buf.is_empty() {
                    neutral.insert(gid);
                }
                for value in buf.drain(..) {
                    sets.entry(value).or_default().insert(gid);
                }
            }
        }
        if let Some(closure) = &self.closure {
            closure(&mut neutral);
            for glyphs in sets.values_mut() {
                let mut reached: BTreeSet<usize> = glyphs.union(&neutral).copied().collect();
                closure(&mut reached);
                glyphs.extend(reached.difference(&neutral).copied());
            }
        }
        sets
    }

    pub(crate) fn name_index(&self, glyph: &str) -> Option<usize> {
        self.index.get(glyph).copied()
    }
}

#[cfg(feature = "ttf")]
pub(crate) mod unicode {
    //! Unicode script and bidi properties of codepoints, as fontc reads them
    use icu_properties::{
        CodePointMapData, PropertyNamesShort,
        props::{BidiClass, Script},
        script::ScriptWithExtensions,
    };
    use smol_str::SmolStr;

    fn short_name(script: Script) -> Option<SmolStr> {
        PropertyNamesShort::<Script>::new()
            .get(script)
            .map(SmolStr::from)
    }

    /// A codepoint's scripts and script extensions, Hiragana and Katakana as
    /// Hrkt
    pub(crate) fn script_extensions(cp: u32) -> Vec<SmolStr> {
        let mut out: Vec<SmolStr> = vec![];
        for script in ScriptWithExtensions::new()
            .get_script_extensions_val32(cp)
            .iter()
        {
            let Some(name) = short_name(script) else {
                continue;
            };
            let name: SmolStr = if name == "Hira" || name == "Kana" {
                "Hrkt".into()
            } else {
                name
            };
            if !out.contains(&name) {
                out.push(name);
            }
        }
        out
    }

    /// The scripts a compiler takes a font to be in, by its codepoints: those
    /// of characters of one script alone. A character of several (a middle
    /// dot, Arabic digits) names none: fontc's `guess_font_scripts`, to which
    /// it adds the language systems the feature code declares
    pub(crate) fn font_scripts<'a>(
        codepoints: impl IntoIterator<Item = &'a Vec<u32>>,
    ) -> std::collections::HashSet<SmolStr> {
        codepoints
            .into_iter()
            .flatten()
            .filter_map(|cp| {
                let scripts = script_extensions(*cp);
                match scripts.as_slice() {
                    [only] => Some(only.clone()),
                    _ => None,
                }
            })
            .collect()
    }

    /// A codepoint's script
    pub(crate) fn script(cp: u32) -> Option<SmolStr> {
        short_name(ScriptWithExtensions::new().get_script_val32(cp))
    }

    /// Left to right (true) or right to left (false), for strong characters
    pub(crate) fn bidi(cp: u32) -> Option<bool> {
        match CodePointMapData::<BidiClass>::new().get32(cp) {
            BidiClass::RightToLeft | BidiClass::ArabicLetter => Some(false),
            BidiClass::LeftToRight | BidiClass::ArabicNumber | BidiClass::EuropeanNumber => {
                Some(true)
            }
            _ => None,
        }
    }
}

/// A feature-code value at each master, the variation models a compiler
/// would build kept for the values that share their locations
pub(crate) struct MasterValues {
    axes: fontdrasil::types::Axes,
    masters: Vec<fontdrasil::coords::NormalizedLocation>,
    models: HashMap<Vec<Vec<(String, i16)>>, fontdrasil::variations::VariationModel>,
    cache: HashMap<Metric, Option<Vec<f64>>>,
}

impl MasterValues {
    pub(crate) fn new(font: &Font) -> Result<Self, crate::BabelfontError> {
        let axes = font.fontdrasil_axes()?;
        let masters = font
            .masters
            .iter()
            .map(|m| m.location.to_normalized(&axes))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(MasterValues {
            axes,
            masters,
            models: HashMap::new(),
            cache: HashMap::new(),
        })
    }

    pub(crate) fn master_count(&self) -> usize {
        self.masters.len()
    }

    /// A value at each master, or None where it has none (a Glyphs number)
    pub(crate) fn at_masters(&mut self, metric: &Metric) -> Option<Vec<f64>> {
        if let Metric::Scalar(v) = metric {
            return Some(vec![*v as f64; self.masters.len()]);
        }
        if let Some(values) = self.cache.get(metric) {
            return values.clone();
        }
        let values = self.compute(metric);
        self.cache.insert(metric.clone(), values.clone());
        values
    }

    fn compute(&mut self, metric: &Metric) -> Option<Vec<f64>> {
        use fontdrasil::{
            coords::{NormalizedLocation, UserCoord},
            variations::{RoundingBehaviour, VariationModel},
        };
        let Metric::Variable(items) = metric else {
            return None;
        };
        let near = |a: &NormalizedLocation, b: &NormalizedLocation| {
            self.axes.iter().all(|axis| {
                let bits = |l: &NormalizedLocation| {
                    l.get(axis.tag).unwrap_or_default().to_f2dot14().to_bits() as i32
                };
                (bits(a) - bits(b)).abs() <= 2
            })
        };
        let stated: Vec<(NormalizedLocation, f64)> = items
            .iter()
            .map(|(user_location, value)| {
                let normalized: NormalizedLocation = self
                    .axes
                    .iter()
                    .map(|axis| {
                        let user = user_location
                            .get(axis.tag.to_string().as_str())
                            .map_or(axis.default, |coord| UserCoord::new(coord.0));
                        (axis.tag, user.to_normalized(&axis.converter))
                    })
                    .collect();
                (normalized, *value as f64)
            })
            .collect();
        // A location a feature file states in user units comes back a step
        // or two of F2Dot14 away from the master it is (an axis mapped
        // through avar: Big Shoulders' wght=333.6): it is that master's,
        // where no other stated location is as near it
        let mut values: HashMap<NormalizedLocation, Vec<f64>> = HashMap::new();
        for (normalized, value) in &stated {
            let master = self.masters.iter().find(|master| near(master, normalized));
            let alone = master.is_some_and(|master| {
                stated
                    .iter()
                    .filter(|(other, _)| near(master, other))
                    .count()
                    == 1
            });
            let location = match master {
                Some(master) if alone => master.clone(),
                _ => normalized.clone(),
            };
            values.insert(location, vec![*value]);
        }
        let key_of = |location: &NormalizedLocation, axes: &fontdrasil::types::Axes| {
            axes.iter()
                .map(|a| {
                    (
                        a.tag.to_string(),
                        location
                            .get(a.tag)
                            .unwrap_or_default()
                            .to_f2dot14()
                            .to_bits(),
                    )
                })
                .collect::<Vec<_>>()
        };
        let mut key: Vec<Vec<(String, i16)>> =
            values.keys().map(|l| key_of(l, &self.axes)).collect();
        key.sort();
        let axis_order = self.axes.axis_order();
        let model = self.models.entry(key).or_insert_with(|| {
            VariationModel::new(values.keys().cloned().collect::<HashSet<_>>(), axis_order)
        });
        let deltas = model
            .deltas_with_rounding(&values, RoundingBehaviour::None)
            .ok()?;
        let mut out = vec![];
        for master in &self.masters {
            let here = key_of(master, &self.axes);
            let exact = values
                .iter()
                .find(|(l, _)| key_of(l, &self.axes) == here)
                .and_then(|(_, v)| v.first().copied());
            out.push(match exact {
                Some(v) => v,
                None => model
                    .interpolate_from_deltas(master, &deltas)
                    .first()
                    .copied()
                    .unwrap_or_default(),
            });
        }
        Some(out)
    }
}

/// Rounds as OpenType does
pub(crate) fn ot_round(v: f64) -> i32 {
    (v + 0.5).floor() as i32
}

/// The lookup blocks of a file by name
pub(crate) fn lookup_blocks(file: &FeatureFile) -> HashMap<SmolStr, &LookupBlock> {
    let mut out = HashMap::new();
    for item in file.iter() {
        match item {
            ToplevelItem::Lookup(lookup) => {
                out.insert(lookup.name.clone(), lookup);
            }
            ToplevelItem::Feature(feature) => {
                for statement in &feature.statements {
                    if let Statement::LookupBlock(lookup) = statement {
                        out.insert(lookup.name.clone(), lookup);
                    }
                }
            }
            _ => {}
        }
    }
    out
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    fn layout(fea: &str) -> Layout {
        let mut font = Font::new();
        for name in ["A", "V"] {
            font.glyphs.push(crate::Glyph::new(name));
        }
        let file = FeatureFile::new_from_fea(fea, Some(&["A", "V"]), None::<&str>).unwrap();
        let mut expander = Expander {
            font: &font,
            classes: HashMap::new(),
        };
        Layout::new(&file, &mut expander)
    }

    fn at(layout: &Layout, script: &str, language: &str) -> BTreeSet<SmolStr> {
        layout
            .features
            .get("kern")
            .and_then(|s| s.get(&(script.into(), language.into())))
            .cloned()
            .unwrap_or_default()
    }

    fn names(names: &[&str]) -> BTreeSet<SmolStr> {
        names.iter().map(|n| SmolStr::from(*n)).collect()
    }

    const LOOKUPS: &str = "languagesystem DFLT dflt; languagesystem latn dflt;
        languagesystem latn TRK;
        lookup k { pos A V -10; } k; lookup t { pos V A -10; } t; lookup s { pos A A 5; } s;";

    #[test]
    fn test_a_language_takes_its_scripts_lookups() {
        let layout = layout(&format!(
            "{LOOKUPS} feature kern {{ lookup k; script latn; lookup s; language TRK; lookup t; }} kern;"
        ));
        assert_eq!(at(&layout, "DFLT", "dflt"), names(&["k"]));
        assert_eq!(at(&layout, "latn", "dflt"), names(&["k", "s"]));
        assert_eq!(at(&layout, "latn", "TRK"), names(&["k", "s", "t"]));
    }

    #[test]
    fn test_exclude_dflt() {
        let layout = layout(&format!(
            "{LOOKUPS} feature kern {{ lookup k; script latn; language TRK exclude_dflt; lookup t; }} kern;"
        ));
        assert_eq!(at(&layout, "latn", "dflt"), names(&["k"]));
        assert_eq!(at(&layout, "latn", "TRK"), names(&["t"]));
    }

    #[test]
    fn test_default_lookups_apply_under_every_declared_system() {
        let layout = layout(&format!("{LOOKUPS} feature kern {{ lookup k; }} kern;"));
        for (s, l) in [("DFLT", "dflt"), ("latn", "dflt"), ("latn", "TRK")] {
            assert_eq!(at(&layout, s, l), names(&["k"]), "{s} {l}");
        }
    }

    #[test]
    fn test_hb_script_selection() {
        let mut list = ScriptList::default();
        list.scripts
            .insert("DFLT".into(), [SmolStr::from("dflt")].into());
        list.scripts
            .insert("dev2".into(), [SmolStr::from("dflt")].into());
        assert_eq!(list.selected("Deva").as_deref(), Some("dev2"));
        assert_eq!(list.selected("Latn").as_deref(), Some("DFLT"));
        assert_eq!(ot_tag_script("dev2").as_deref(), Some("Deva"));
        assert_eq!(ot_tag_script("latn").as_deref(), Some("Latn"));
        assert_eq!(ot_tag_script("lao ").as_deref(), Some("Laoo"));
        assert_eq!(ot_tag_script("yi  ").as_deref(), Some("Yiii"));
    }
}

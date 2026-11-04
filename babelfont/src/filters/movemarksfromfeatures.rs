use std::collections::{HashMap, HashSet};

use fea_rs_ast::{
    Anchor as FeaAnchor, AsFea, FeatureFile, GlyphContainer, LookupBlock, MarkClass, Statement,
    ToplevelItem,
};
use smol_str::SmolStr;

use super::feature_code::{lookup_reference_count, lookups_by_section, remove_lookup_blocks};
use crate::{
    Font, GlyphCategory, LayerType, filters::FontFilter, layout::expand::expand_glyph_container,
};

/// A filter that moves mark attachment written as feature code into anchors
///
/// A font's anchors are what a compiler builds `mark`, `mkmk` and `curs` from.
/// Where feature code states the same attachment with the anchors' positions
/// written into it (as a decompiled font's does), editing an anchor changes
/// nothing: the written lookups still win. This filter takes out each lookup
/// of those features that the glyphs' anchors state in full, in every master,
/// so that the compiler builds it from them; a lookup they do not state stays.
#[derive(Default)]
pub struct MoveMarksFromFeatures;

impl MoveMarksFromFeatures {
    /// Create a new MoveMarksFromFeatures filter
    pub fn new() -> Self {
        MoveMarksFromFeatures
    }
}

/// The features a compiler builds from anchors
const ATTACHMENT_FEATURES: [&str; 3] = ["mark", "mkmk", "curs"];

/// How near, in font units, an anchor must be to where the feature code puts it
const TOLERANCE: f64 = 0.01;

struct Checker<'a> {
    font: &'a Font,
    axes: fontdrasil::types::Axes,
    classes: HashMap<SmolStr, Vec<SmolStr>>,
    mark_classes: HashMap<SmolStr, Vec<(Vec<SmolStr>, FeaAnchor)>>,
}

impl Checker<'_> {
    fn expand(&self, container: &GlyphContainer) -> Vec<SmolStr> {
        expand_glyph_container(self.font, container, &self.classes, None)
    }

    /// The anchor names a glyph has at a feature-code anchor, in every master
    fn names_at(&self, glyph: &str, anchor: &FeaAnchor) -> HashSet<String> {
        if anchor.contourpoint.is_some()
            || anchor.x_device_table.is_some()
            || anchor.y_device_table.is_some()
        {
            return HashSet::new();
        }
        let Some(glyph) = self.font.glyphs.get(glyph) else {
            return HashSet::new();
        };
        let mut names: Option<HashSet<String>> = None;
        for master in &self.font.masters {
            let Some(layer) = glyph
                .layers
                .iter()
                .find(|l| l.master == LayerType::DefaultForMaster(master.id.clone()))
            else {
                return HashSet::new();
            };
            let (Ok(x), Ok(y)) = (
                crate::features::metric_at(&anchor.x, &master.location, &self.axes),
                crate::features::metric_at(&anchor.y, &master.location, &self.axes),
            ) else {
                return HashSet::new();
            };
            let here: HashSet<String> = layer
                .anchors
                .iter()
                .filter(|a| (a.x - x).abs() < TOLERANCE && (a.y - y).abs() < TOLERANCE)
                .map(|a| a.name.clone())
                .collect();
            names = Some(match names {
                None => here,
                Some(names) => names.intersection(&here).cloned().collect(),
            });
        }
        names.unwrap_or_default()
    }

    fn category(&self, glyph: &str) -> Option<GlyphCategory> {
        self.font.glyphs.get(glyph).map(|g| g.category.clone())
    }

    /// The anchor name the marks of a class attach by, if their anchors all
    /// state one where the feature code puts them
    fn mark_class_name(&self, class: &MarkClass) -> Option<HashSet<String>> {
        let definitions = self.mark_classes.get(&class.name)?;
        let mut names: Option<HashSet<String>> = None;
        for (marks, anchor) in definitions {
            for mark in marks {
                if self.category(mark) != Some(GlyphCategory::Mark) {
                    return None;
                }
                let here: HashSet<String> = self
                    .names_at(mark, anchor)
                    .into_iter()
                    .filter_map(|n| n.strip_prefix('_').map(str::to_string))
                    .collect();
                names = Some(match names {
                    None => here,
                    Some(names) => names.intersection(&here).cloned().collect(),
                });
            }
        }
        names.filter(|n| !n.is_empty())
    }

    /// Whether bases have, at each anchor, the anchor its mark class attaches
    /// by (with `suffix` for a ligature's component)
    fn attaches(
        &self,
        bases: &[SmolStr],
        marks: &[(Option<FeaAnchor>, MarkClass)],
        suffix: &str,
    ) -> bool {
        marks.iter().all(|(anchor, class)| {
            // a NULL anchor attaches nothing, as a missing anchor does
            let Some(anchor) = anchor else {
                return true;
            };
            let Some(names) = self.mark_class_name(class) else {
                return false;
            };
            bases.iter().all(|base| {
                let here = self.names_at(base, anchor);
                names
                    .iter()
                    .any(|name| here.contains(&format!("{name}{suffix}")))
            })
        })
    }

    /// Whether the glyphs' anchors state a lookup in full
    fn states(&self, lookup: &LookupBlock) -> bool {
        lookup.statements.iter().all(|statement| match statement {
            Statement::MarkBasePos(pos) => {
                let bases = self.expand(&pos.base);
                bases
                    .iter()
                    .all(|b| self.category(b).is_some_and(|c| c != GlyphCategory::Mark))
                    && self.attaches(&bases, &pos.marks, "")
            }
            Statement::MarkMarkPos(pos) => {
                let bases = self.expand(&pos.base_marks);
                bases
                    .iter()
                    .all(|b| self.category(b) == Some(GlyphCategory::Mark))
                    && self.attaches(&bases, &pos.marks, "")
            }
            Statement::MarkLigPos(pos) => {
                let ligatures = self.expand(&pos.ligatures);
                ligatures
                    .iter()
                    .all(|l| self.category(l) == Some(GlyphCategory::Ligature))
                    && pos.marks.iter().enumerate().all(|(index, component)| {
                        self.attaches(&ligatures, component, &format!("_{}", index + 1))
                    })
            }
            Statement::CursivePos(pos) => self.expand(&pos.glyphclass).iter().all(|glyph| {
                [(&pos.entry, "entry"), (&pos.exit, "exit")]
                    .into_iter()
                    .all(|(anchor, name)| {
                        anchor
                            .as_ref()
                            .is_none_or(|anchor| self.names_at(glyph, anchor).contains(name))
                    })
            }),
            Statement::LookupFlag(_) | Statement::Subtable(_) | Statement::Comment(_) => true,
            _ => false,
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

impl FontFilter for MoveMarksFromFeatures {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        if !font
            .features
            .features
            .iter()
            .any(|(n, _)| ATTACHMENT_FEATURES.contains(&n.as_str()))
        {
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
        let mut checker = Checker {
            font,
            axes: font.fontdrasil_axes()?,
            classes: HashMap::new(),
            mark_classes: HashMap::new(),
        };
        let mut lookups: Vec<&LookupBlock> = vec![];
        let mut blocks = vec![];
        for item in file.iter() {
            match item {
                ToplevelItem::GlyphClassDefinition(definition) => {
                    let glyphs =
                        checker.expand(&GlyphContainer::GlyphClass(definition.glyphs.clone()));
                    checker
                        .classes
                        .insert(definition.name.trim_start_matches('@').into(), glyphs);
                }
                ToplevelItem::MarkClassDefinition(definition) => {
                    let marks = checker.expand(&definition.glyphs);
                    checker
                        .mark_classes
                        .entry(definition.mark_class.name.clone())
                        .or_default()
                        .push((marks, definition.anchor.clone()));
                }
                ToplevelItem::Lookup(lookup) => lookups.push(lookup),
                ToplevelItem::Feature(feature)
                    if ATTACHMENT_FEATURES.contains(&feature.name.as_str()) =>
                {
                    blocks.push(feature)
                }
                _ => {}
            }
        }

        // Lookups every section of their feature applies
        let mut candidates: HashSet<SmolStr> = HashSet::new();
        for tag in ATTACHMENT_FEATURES {
            let mut applied: Option<Vec<SmolStr>> = None;
            for block in blocks.iter().filter(|b| b.name == tag) {
                let Some(sections) = lookups_by_section(&block.statements) else {
                    return Ok(());
                };
                for section in sections {
                    applied = Some(match applied {
                        None => section,
                        Some(previous) => previous
                            .into_iter()
                            .filter(|name| section.contains(name))
                            .collect(),
                    });
                }
            }
            candidates.extend(applied.unwrap_or_default());
        }
        let feature_references = |name: &str| -> usize {
            blocks
                .iter()
                .map(|b| lookup_reference_count(&b.as_fea(""), name))
                .sum()
        };
        let moved: HashSet<SmolStr> = lookups
            .iter()
            .filter(|lookup| candidates.contains(&lookup.name))
            // used elsewhere too (another feature, a contextual rule)
            .filter(|lookup| {
                lookup_reference_count(&fea, &lookup.name) == 1 + feature_references(&lookup.name)
            })
            .filter(|lookup| checker.states(lookup))
            .map(|lookup| lookup.name.clone())
            .collect();
        if moved.is_empty() {
            return Ok(());
        }
        let mark_class_names: HashSet<SmolStr> = checker.mark_classes.keys().cloned().collect();
        if remove_lookup_blocks(&mut font.features, &moved, &ATTACHMENT_FEATURES) {
            remove_unused_mark_classes(font, &mark_class_names);
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

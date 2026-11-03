use std::sync::LazyLock;

use serde::Deserialize;

use crate::filters::FontFilter;

#[derive(Debug, Deserialize)]
pub(crate) struct GlyphInfo {
    /// The codepoint, in hex
    pub(crate) unicode: Option<String>,
    /// The Unicode character name, or a description of the glyph
    pub(crate) description: Option<String>,
    /// The script, lowercase (latin, arabic, …)
    pub(crate) script: Option<String>,
    /// Other names the glyph went by, comma-separated
    alt_names: Option<String>,
    pub(crate) name: String,
    category: String,
    sub_category: Option<String>,
    // case: Option<String>,
    // direction: Option<String>,
    // script: Option<String>,
    production: Option<String>,
    // alt_names: Vec<String>,
}

const GLYPHS_DATA_STR: &str = include_str!(concat!(env!("OUT_DIR"), "/glyphsdata.json"));
#[allow(clippy::expect_used)]
static GLYPHS_DATA: LazyLock<Vec<GlyphInfo>> =
    LazyLock::new(|| serde_json::from_str(GLYPHS_DATA_STR).expect("Failed to parse glyphs data"));

/// A name compared without case, hyphens and underscores, as legacy names
/// differ from Glyphs' (dotcenterar for dotcenter-ar)
fn loose_key(name: &str) -> String {
    name.chars()
        .filter(|c| *c != '-' && *c != '_')
        .flat_map(char::to_lowercase)
        .collect()
}

struct GlyphsDataIndex {
    by_name: std::collections::HashMap<&'static str, &'static GlyphInfo>,
    by_alt_name: std::collections::HashMap<&'static str, &'static GlyphInfo>,
    // None where two glyphs share a loose name
    by_loose_name: std::collections::HashMap<String, Option<&'static GlyphInfo>>,
    by_codepoint: std::collections::HashMap<u32, &'static GlyphInfo>,
}

static GLYPHS_DATA_INDEX: LazyLock<GlyphsDataIndex> = LazyLock::new(|| {
    let mut index = GlyphsDataIndex {
        by_name: Default::default(),
        by_alt_name: Default::default(),
        by_loose_name: Default::default(),
        by_codepoint: Default::default(),
    };
    for info in GLYPHS_DATA.iter() {
        index.by_name.insert(info.name.as_str(), info);
        let alt_names = info
            .alt_names
            .as_deref()
            .unwrap_or_default()
            .split(',')
            .map(str::trim)
            .filter(|n| !n.is_empty());
        for alt_name in alt_names {
            index.by_alt_name.entry(alt_name).or_insert(info);
        }
        index
            .by_loose_name
            .entry(loose_key(&info.name))
            .and_modify(|found| *found = None)
            .or_insert(Some(info));
        if let Some(codepoint) = info
            .unicode
            .as_deref()
            .and_then(|hex| u32::from_str_radix(hex, 16).ok())
        {
            index.by_codepoint.entry(codepoint).or_insert(info);
        }
    }
    index
});

/// Glyphs.app's data on a glyph, by its name, a name it went by, or its name
/// compared loosely; a name with a suffix (`.case`) as the glyph it varies
pub(crate) fn glyph_info(name: &str) -> Option<&'static GlyphInfo> {
    let index = &*GLYPHS_DATA_INDEX;
    let base_name = name.split('.').next().unwrap_or(name);
    [name, base_name].into_iter().find_map(|name| {
        index
            .by_name
            .get(name)
            .or_else(|| index.by_alt_name.get(name))
            .copied()
            .or_else(|| index.by_loose_name.get(&loose_key(name)).copied().flatten())
    })
}

/// Glyphs.app's data on the glyph for a codepoint
pub(crate) fn glyph_info_for_codepoint(codepoint: u32) -> Option<&'static GlyphInfo> {
    GLYPHS_DATA_INDEX.by_codepoint.get(&codepoint).copied()
}

/// A filter that adds Glyphs.app glyph metadata to the font
pub struct GlyphsData;

impl FontFilter for GlyphsData {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        log::info!("Applying GlyphsData filter");
        let index = &*GLYPHS_DATA_INDEX;
        for glyph in font.glyphs.iter_mut() {
            let exact = index.by_name.get(glyph.name.as_str()).copied();
            if let Some(production_name) = exact.and_then(|info| info.production.as_ref()) {
                if glyph.production_name.is_none() {
                    glyph.production_name = Some(production_name.into());
                }
            }
            if glyph.category != crate::GlyphCategory::Unknown {
                continue;
            }
            // a glyph with a suffix (acutecomb.case) is of the category of
            // the glyph it varies, as Glyphs has it
            let Some(info) = exact.or_else(|| {
                let base = glyph.name.split('.').next()?;
                (base != glyph.name.as_str())
                    .then(|| {
                        index
                            .by_name
                            .get(base)
                            .or_else(|| index.by_alt_name.get(base))
                            .copied()
                    })
                    .flatten()
            }) else {
                continue;
            };
            match info.category.as_str() {
                // a mark only if it attaches: a spacing accent or a
                // modifier letter (acute, apostrophemod) is a base, as
                // glyphsLib and fontc have it (category_for_glyph_preliminary)
                "Mark" => {
                    glyph.category = if matches!(
                        info.sub_category.as_deref(),
                        Some("Nonspacing" | "Spacing Combining")
                    ) {
                        crate::GlyphCategory::Mark
                    } else {
                        crate::GlyphCategory::Base
                    }
                }
                "Base" => glyph.category = crate::GlyphCategory::Base,
                "Ligature" => glyph.category = crate::GlyphCategory::Ligature,
                // "Component" => glyph.category = crate::GlyphCategory::Component,
                _ => {}
            }
        }
        Ok(())
    }

    fn from_str(_s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(GlyphsData)
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        clap::Arg::new("glyphsdata")
            .long("glyphs-data")
            .help("Add Glyphs.app glyph metadata to the font")
            .action(clap::ArgAction::SetTrue)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::{Font, Glyph, GlyphCategory};

    #[test]
    fn test_only_attaching_marks_are_marks() {
        // Glyphs' data has all three as Mark: Modifier, Spacing and
        // Nonspacing. A shaper zeroes a mark's advance, so glyphsLib and fontc
        // take only a nonspacing or spacing combining one for a mark.
        let mut font = Font::new();
        for name in ["apostrophemod", "acute", "acutecomb"] {
            font.glyphs.push(Glyph::new(name));
        }
        GlyphsData.apply(&mut font).unwrap();
        let category = |name: &str| font.glyphs.get(name).unwrap().category.clone();
        assert_eq!(category("apostrophemod"), GlyphCategory::Base);
        assert_eq!(category("acute"), GlyphCategory::Base);
        assert_eq!(category("acutecomb"), GlyphCategory::Mark);
    }

    #[test]
    fn test_a_variant_is_of_its_glyphs_category() {
        // acutecomb.case is a mark as acutecomb is, though Glyphs' data names
        // only acutecomb; its production name is its own to make
        let mut font = Font::new();
        for name in ["acutecomb.case", "dotbelowcomb.case", "acute.case"] {
            font.glyphs.push(Glyph::new(name));
        }
        GlyphsData.apply(&mut font).unwrap();
        let glyph = |name: &str| font.glyphs.get(name).unwrap();
        assert_eq!(glyph("acutecomb.case").category, GlyphCategory::Mark);
        assert_eq!(glyph("dotbelowcomb.case").category, GlyphCategory::Mark);
        // a spacing accent's variant is a base, as the accent is
        assert_eq!(glyph("acute.case").category, GlyphCategory::Base);
        assert_eq!(glyph("acutecomb.case").production_name, None);
    }
}

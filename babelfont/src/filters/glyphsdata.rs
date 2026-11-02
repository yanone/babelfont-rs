use std::sync::LazyLock;

use serde::Deserialize;

use crate::filters::FontFilter;

#[derive(Debug, Deserialize)]
pub(crate) struct GlyphInfo {
    /// The codepoint, in hex
    pub(crate) unicode: Option<String>,
    /// The Unicode character name, or a description of the glyph
    pub(crate) description: Option<String>,
    pub(crate) name: String,
    category: String,
    // sub_category: Option<String>,
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

static GLYPHS_DATA_BY_NAME: LazyLock<std::collections::HashMap<&'static str, &'static GlyphInfo>> =
    LazyLock::new(|| {
        GLYPHS_DATA
            .iter()
            .map(|info| (info.name.as_str(), info))
            .collect()
    });

/// Glyphs.app's data on a glyph, by its name
pub(crate) fn glyph_info(name: &str) -> Option<&'static GlyphInfo> {
    GLYPHS_DATA_BY_NAME.get(name).copied()
}

/// A filter that adds Glyphs.app glyph metadata to the font
pub struct GlyphsData;

impl FontFilter for GlyphsData {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        log::info!("Applying GlyphsData filter");
        for glyph_info in GLYPHS_DATA.iter() {
            if let Some(glyph) = font.glyphs.get_mut(&glyph_info.name) {
                if let Some(production_name) = &glyph_info.production {
                    if glyph.production_name.is_none() {
                        glyph.production_name = Some(production_name.into());
                    }
                }
                if glyph.category != crate::GlyphCategory::Unknown {
                    continue;
                }
                match glyph_info.category.as_str() {
                    "Mark" => glyph.category = crate::GlyphCategory::Mark,
                    "Base" => glyph.category = crate::GlyphCategory::Base,
                    "Ligature" => glyph.category = crate::GlyphCategory::Ligature,
                    // "Component" => glyph.category = crate::GlyphCategory::Component,
                    _ => {}
                }
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

use std::sync::LazyLock;

use crate::{filters::FontFilter, GlyphCategory};
use regex::Regex;
use smol_str::SmolStr;

/// A filter that sets the category of conjunct glyphs without ligature anchors from ligature to base
pub struct CorrectConjunctCategory(Vec<SmolStr>);

impl CorrectConjunctCategory {
    /// Create a new CorrectConjunctCategory filter.
    /// If `glyph_names` is empty, all glyphs are processed.
    pub fn new(glyph_names: Vec<String>) -> Self {
        CorrectConjunctCategory(glyph_names.into_iter().map(SmolStr::from).collect())
    }
}

#[allow(clippy::unwrap_used)]
static LIGATURE_ANCHOR: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"_\d+$").unwrap());

fn has_ligature_anchor(layer: &crate::Layer) -> bool {
    layer
        .anchors
        .iter()
        .any(|anchor| LIGATURE_ANCHOR.is_match(&anchor.name))
}

impl FontFilter for CorrectConjunctCategory {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        let filter_list = &self.0;
        for glyph in font.glyphs.iter_mut() {
            if !filter_list.is_empty() && !filter_list.contains(&glyph.name) {
                continue;
            }
            if glyph.category == GlyphCategory::Ligature
                && !glyph.layers.iter().any(has_ligature_anchor)
            {
                log::debug!(
                    "Correcting category of glyph {} from Ligature to Base",
                    glyph.name
                );
                glyph.category = GlyphCategory::Base;
                glyph
                    .format_specific
                    .insert_json_non_null("subcategory", &"Conjunct".to_string());
            }
        }
        Ok(())
    }

    fn from_str(s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(CorrectConjunctCategory(super::parse_glyph_list(s)))
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        super::glyph_filter_arg(
            "correctconjunctcategory",
            "correct-conjunct-category",
            "Set the category of conjunct glyphs without ligature anchors from ligature to base",
        )
    }
}

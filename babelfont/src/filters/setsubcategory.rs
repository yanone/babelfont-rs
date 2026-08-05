use crate::{filters::FontFilter, GlyphCategory};
use smol_str::SmolStr;

/// A filter that sets the subcategory of mark glyphs to Nonspacing for Glyphs export
pub struct SetSubcategory(Vec<SmolStr>);

impl SetSubcategory {
    /// Create a new SetSubcategory filter.
    /// If `glyph_names` is empty, all glyphs are processed.
    pub fn new(glyph_names: Vec<String>) -> Self {
        SetSubcategory(glyph_names.into_iter().map(SmolStr::from).collect())
    }
}

fn has_underscore_anchor(layer: &crate::Layer) -> bool {
    layer
        .anchors
        .iter()
        .any(|anchor| anchor.name.starts_with('_'))
}

impl FontFilter for SetSubcategory {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        let filter_list = &self.0;
        for glyph in font.glyphs.iter_mut() {
            if !filter_list.is_empty() && !filter_list.contains(&glyph.name) {
                continue;
            }
            if glyph.category == GlyphCategory::Mark
                && glyph.layers.iter().any(has_underscore_anchor)
            {
                glyph
                    .format_specific
                    .insert_json_non_null("subcategory", &"Nonspacing".to_string());
            }
        }
        Ok(())
    }

    fn from_str(s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(SetSubcategory(super::parse_glyph_list(s)))
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        super::glyph_filter_arg(
            "setsubcategory",
            "set-subcategory",
            "Set the subcategory of mark glyphs to Nonspacing for Glyphs export",
        )
    }
}

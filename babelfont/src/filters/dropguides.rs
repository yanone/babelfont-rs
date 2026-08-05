use crate::filters::FontFilter;
use smol_str::SmolStr;

#[derive(Default)]
/// A filter that drops all guides from a font
pub struct DropGuides(Vec<SmolStr>);

impl DropGuides {
    /// Create a new DropGuides filter.
    /// If `glyph_names` is empty, all glyphs are processed.
    pub fn new(glyph_names: Vec<String>) -> Self {
        DropGuides(glyph_names.into_iter().map(SmolStr::from).collect())
    }
}

impl FontFilter for DropGuides {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        let filter_list = &self.0;
        log::info!("Dropping all guides from font");
        for master in font.masters.iter_mut() {
            master.guides.clear();
        }
        for glyph in font.glyphs.iter_mut() {
            if !filter_list.is_empty() && !filter_list.contains(&glyph.name) {
                continue;
            }
            for layer in glyph.layers.iter_mut() {
                layer.guides.clear();
            }
        }
        Ok(())
    }

    fn from_str(s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(DropGuides(super::parse_glyph_list(s)))
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        super::glyph_filter_arg("dropguides", "drop-guides", "Drop all guides from the font")
    }
}

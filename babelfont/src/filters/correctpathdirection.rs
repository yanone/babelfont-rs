use crate::filters::FontFilter;

/// A filter that normalises contour direction to the PostScript convention.
///
/// Source formats disagree about which way an outer contour should run. UFO and
/// Glyphs are PostScript-convention formats: the outer contour runs
/// counter-clockwise. TrueType is the other way round, and compilers reverse
/// every contour on the way out to `glyf` (fontc does this unconditionally
/// unless `--keep-direction`, matching fontmake and ufo2ft).
///
/// That reversal is only correct if the source really is in PostScript
/// convention. Some source formats store TrueType-convention outlines instead --
/// FontForge SFD and FontLab VFB both do -- and convertors that copy the
/// directions verbatim therefore produce fonts whose outer contours come out
/// counter-clockwise. The fill is unaffected, because the whole glyph flips
/// together and the nonzero winding rule is direction-agnostic, but the result
/// is a convention error that font QA reports (the Google Fonts
/// `outline_direction` check, fontbakery #2056).
///
/// This filter fixes it at the source end, where it applies to every convertor
/// rather than to one format: for each layer it takes the contour with the
/// largest enclosed area -- the outer contour -- and, if that contour runs
/// clockwise, reverses every closed contour in the layer.
///
/// Reversing the whole layer rather than each contour individually is what keeps
/// counters correct. The relative direction of outer contours and their holes is
/// already consistent in these sources, so the only thing wrong is the global
/// sense; flipping everything preserves the relationship.
///
/// Open contours are left alone. They are not filled, so they have no direction
/// to correct.
#[derive(Default)]
pub struct CorrectPathDirection;

impl CorrectPathDirection {
    /// Create a new CorrectPathDirection filter
    pub fn new() -> Self {
        CorrectPathDirection
    }
}

impl FontFilter for CorrectPathDirection {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        for glyph in font.glyphs.iter_mut() {
            for layer in &mut glyph.layers {
                // Find the outer contour: the closed path enclosing the most area.
                let mut outer_area = 0.0f64;
                for path in layer.shapes.iter().filter_map(|s| s.as_path()) {
                    if !path.closed {
                        continue;
                    }
                    let area = path.signed_area()?;
                    if area.abs() > outer_area.abs() {
                        outer_area = area;
                    }
                }

                // Negative area is clockwise, which is the convention we do not
                // want. Zero means there is nothing to judge by.
                if outer_area >= 0.0 {
                    continue;
                }

                for path in layer.shapes.iter_mut().filter_map(|s| s.as_path_mut()) {
                    path.reverse();
                }
            }
        }
        Ok(())
    }

    fn from_str(_s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(CorrectPathDirection::new())
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        clap::Arg::new("correctpathdirection")
            .long("correct-path-direction")
            .help(
                "Normalise contour direction to the PostScript convention \
                 (counter-clockwise outer contours)",
            )
            .action(clap::ArgAction::SetTrue)
    }
}

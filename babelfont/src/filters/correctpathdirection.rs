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

#[allow(clippy::unwrap_used, clippy::expect_used)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::Node;

    /// A closed square, drawn counter-clockwise, with straight sides.
    fn ccw_square() -> Path {
        Path {
            nodes: vec![
                Node::new_line(0.0, 0.0),
                Node::new_line(100.0, 0.0),
                Node::new_line(100.0, 100.0),
                Node::new_line(0.0, 100.0),
            ],
            closed: true,
            ..Default::default()
        }
    }

    #[test]
    fn signed_area_sign_matches_direction() {
        let ccw = ccw_square();
        assert!(signed_area(&ccw).unwrap() > 0.0);

        let mut cw = ccw_square();
        reverse_path(&mut cw);
        assert!(signed_area(&cw).unwrap() < 0.0);
    }

    #[test]
    fn reversing_twice_is_the_identity() {
        let original = ccw_square();
        let mut roundtripped = ccw_square();
        reverse_path(&mut roundtripped);
        reverse_path(&mut roundtripped);

        let area_before = signed_area(&original).unwrap();
        let area_after = signed_area(&roundtripped).unwrap();
        assert!((area_before - area_after).abs() < 1e-9);

        let points_before: Vec<(f64, f64)> = original.nodes.iter().map(|n| (n.x, n.y)).collect();
        let points_after: Vec<(f64, f64)> = roundtripped.nodes.iter().map(|n| (n.x, n.y)).collect();
        assert_eq!(points_before.len(), points_after.len());
        // The starting point may rotate, but the cycle must be the same.
        assert!(points_after
            .iter()
            .cycle()
            .take(points_after.len() * 2)
            .collect::<Vec<_>>()
            .windows(points_before.len())
            .any(|w| w.iter().copied().copied().collect::<Vec<_>>() == points_before));
    }

    #[test]
    fn reversal_preserves_mixed_line_and_curve_segments() {
        // A shape with one cubic side and one straight side. Reversing it must
        // keep exactly one cubic segment: the type has to travel with the
        // segment, not stay on the node.
        let mut path = Path {
            nodes: vec![
                Node::new_line(0.0, 0.0),
                Node::new_offcurve(30.0, 60.0),
                Node::new_offcurve(70.0, 60.0),
                Node::new_curve(100.0, 0.0),
            ],
            closed: true,
            ..Default::default()
        };
        let area_before = signed_area(&path).unwrap();

        reverse_path(&mut path);

        // Still one cubic and one line, and still a valid path.
        assert_eq!(
            path.nodes
                .iter()
                .filter(|n| n.nodetype == NodeType::OffCurve)
                .count(),
            2
        );
        assert_eq!(
            path.nodes
                .iter()
                .filter(|n| n.nodetype == NodeType::Curve)
                .count(),
            1
        );
        assert_eq!(
            path.nodes
                .iter()
                .filter(|n| n.nodetype == NodeType::Line)
                .count(),
            1
        );

        // Same shape, opposite direction.
        let area_after = signed_area(&path).unwrap();
        assert!((area_before + area_after).abs() < 1e-6);
    }

    /// Three on-curve nodes, three different segment kinds, and off-curve runs of
    /// three different lengths. Every segment kind must stay attached to its own
    /// segment, which means each on-curve node's off-curve run has to come out
    /// the right length for its new type.
    ///
    /// A contour with only two on-curve nodes cannot catch a swapped shift
    /// direction, because its successor and predecessor are the same node.
    #[test]
    fn reversal_keeps_segment_kinds_with_their_segments() {
        // A (line) -> B via cubic, B -> C via quadratic, C -> A via line.
        let mut path = Path {
            nodes: vec![
                Node::new_line(0.0, 0.0),
                Node::new_offcurve(10.0, 40.0),
                Node::new_offcurve(40.0, 60.0),
                Node::new_curve(60.0, 60.0),
                Node::new_offcurve(90.0, 30.0),
                Node::new_qcurve(90.0, 0.0),
            ],
            closed: true,
            ..Default::default()
        };
        let area_before = signed_area(&path).unwrap();

        reverse_path(&mut path);

        // to_kurbo is the real consistency check: it errors if an on-curve node's
        // type disagrees with the number of off-curves in front of it, which is
        // exactly the corruption a wrong shift direction produces.
        let kurbo = path.to_kurbo().expect("reversed path must be well-formed");
        assert!(!kurbo.elements().is_empty());

        // One of each segment kind survives.
        for kind in [NodeType::Line, NodeType::Curve, NodeType::QCurve] {
            assert_eq!(
                path.nodes.iter().filter(|n| n.nodetype == kind).count(),
                1,
                "expected exactly one {kind:?} node after reversal"
            );
        }
        assert_eq!(
            path.nodes
                .iter()
                .filter(|n| n.nodetype == NodeType::OffCurve)
                .count(),
            3
        );

        // Same shape, opposite direction.
        let area_after = signed_area(&path).unwrap();
        assert!(
            (area_before + area_after).abs() < 1e-6,
            "area {area_before} -> {area_after}"
        );
    }

    #[test]
    fn open_paths_are_left_alone() {
        let mut path = Path {
            nodes: vec![Node::new_move(0.0, 0.0), Node::new_line(100.0, 0.0)],
            closed: false,
            ..Default::default()
        };
        let before = path.clone();
        reverse_path(&mut path);
        assert_eq!(before, path);
    }
}

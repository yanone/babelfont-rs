//! Non-destructive outline boolean subtraction (`fip001-boolean`).
//!
//! Paths marked `format_specific["fip001-boolean"] = "subtraction"` cut
//! everything below them in path order. Later paths are drawn on top.
//! Glyphs whose component tree contains a subtraction path are decomposed
//! first (from an original-font snapshot) so accent cutters apply after
//! base components in the flattened shape list.

use std::collections::HashSet;

use kurbo::{BezPath, Rect};
use linesweeper::{binary_op, BinaryOp, FillRule};
use smol_str::SmolStr;

use crate::{
    filters::{DecomposeComponentReferences, FontFilter},
    shape::Shape,
    BabelfontError, Font, Glyph, Layer, Path,
};

/// `format_specific` key for boolean path operations.
pub const FIP001_BOOLEAN_KEY: &str = "fip001-boolean";
/// Value that marks a path as a subtraction cutter.
pub const FIP001_BOOLEAN_SUBTRACTION: &str = "subtraction";

/// Apply FIP001 boolean subtraction during compile.
#[derive(Debug, Clone, Copy, Default)]
pub struct Fip001Boolean;

impl Fip001Boolean {
    /// Create a new Fip001Boolean filter.
    pub fn new() -> Self {
        Fip001Boolean
    }
}

/// True when the path is marked as a subtraction cutter.
pub fn path_is_subtraction(path: &Path) -> bool {
    let is_subtraction_value = |value: Option<&serde_json::Value>| {
        value
            .and_then(|value| value.as_str())
            .is_some_and(|value| value == FIP001_BOOLEAN_SUBTRACTION)
    };
    is_subtraction_value(path.format_specific.get(FIP001_BOOLEAN_KEY))
        || is_subtraction_value(
            path.format_specific
                .get("com.schriftgestalt.Glyphs.attr")
                .and_then(|value| value.get(FIP001_BOOLEAN_KEY)),
        )
}

/// Apply boolean subtraction to a layer that already has components flattened.
pub fn apply_boolean_to_layer(layer: &mut Layer) -> Result<(), BabelfontError> {
    if !layer
        .shapes
        .iter()
        .filter_map(Shape::as_path)
        .any(path_is_subtraction)
    {
        return Ok(());
    }
    let paths: Vec<Path> = layer
        .shapes
        .iter()
        .filter_map(Shape::as_path)
        .cloned()
        .collect();
    let result = apply_boolean_to_paths(paths)?;
    layer.shapes = result.into_iter().map(Shape::Path).collect();
    Ok(())
}

/// Walk paths in order and apply sequential NonZero difference.
pub fn apply_boolean_to_paths(paths: Vec<Path>) -> Result<Vec<Path>, BabelfontError> {
    if !paths.iter().any(path_is_subtraction) {
        return Ok(paths);
    }

    let mut acc: Vec<Path> = Vec::new();
    let mut pending: Vec<Path> = Vec::new();

    for path in paths {
        if path_is_subtraction(&path) {
            if !path.closed {
                continue;
            }
            acc = subtract_cutter_from_below(acc, pending, &path)?;
            pending = Vec::new();
            continue;
        }
        pending.push(path);
    }
    acc.extend(pending);
    Ok(acc)
}

fn subtract_cutter_from_below(
    acc: Vec<Path>,
    pending: Vec<Path>,
    cutter: &Path,
) -> Result<Vec<Path>, BabelfontError> {
    let mut kept = Vec::new();
    let mut intersecting = Vec::new();
    for path in acc.into_iter().chain(pending) {
        if !path.closed {
            kept.push(path);
            continue;
        }
        if bounds_overlap(&path, cutter) {
            intersecting.push(path);
        } else {
            kept.push(path);
        }
    }
    if intersecting.is_empty() {
        return Ok(kept);
    }

    let subject = paths_to_compound_bez(&intersecting)?;
    let cutter_bez = cutter.to_kurbo()?;
    let contours = binary_op(
        &subject,
        &cutter_bez,
        FillRule::NonZero,
        BinaryOp::Difference,
    )
    .map_err(|err| {
        BabelfontError::FilterError(format!("FIP001 boolean subtraction failed: {err:?}"))
    })?;

    for contour in contours.contours() {
        kept.push(path_from_linesweeper_contour(contour)?);
    }
    Ok(kept)
}

fn bounds_overlap(a: &Path, b: &Path) -> bool {
    match (a.control_box(), b.control_box()) {
        (Some(a_box), Some(b_box)) => rects_overlap(a_box, b_box),
        _ => false,
    }
}

fn rects_overlap(a: Rect, b: Rect) -> bool {
    a.min_x() <= b.max_x()
        && b.min_x() <= a.max_x()
        && a.min_y() <= b.max_y()
        && b.min_y() <= a.max_y()
}

fn paths_to_compound_bez(paths: &[Path]) -> Result<BezPath, BabelfontError> {
    let mut compound = BezPath::new();
    for path in paths {
        for element in path.to_kurbo()?.elements() {
            compound.push(*element);
        }
    }
    Ok(compound)
}

fn path_from_linesweeper_contour(
    contour: &linesweeper::topology::Contour,
) -> Result<Path, BabelfontError> {
    let mut path = Path::from(contour.path.clone());
    if !path.closed && !path.nodes.is_empty() {
        path.closed = true;
    }
    let area = path.signed_area().unwrap_or(0.0);
    // Linesweeper marks `outer` with the set on the left in y-down space.
    // Font outlines are y-up PostScript: outer CCW (positive area), holes CW.
    if contour.outer {
        if area < 0.0 {
            path.reverse();
        }
    } else if area > 0.0 {
        path.reverse();
    }
    Ok(path)
}

fn glyph_has_subtraction_path(glyph: &Glyph) -> bool {
    glyph.layers.iter().any(|layer| {
        !layer.is_background
            && layer
                .shapes
                .iter()
                .filter_map(Shape::as_path)
                .any(path_is_subtraction)
    })
}

fn tree_has_subtraction(font: &Font, glyph_name: &str, visiting: &mut HashSet<SmolStr>) -> bool {
    if !visiting.insert(glyph_name.into()) {
        return false;
    }
    let Some(glyph) = font.glyphs.get(glyph_name) else {
        return false;
    };
    if glyph_has_subtraction_path(glyph) {
        return true;
    }
    for layer in &glyph.layers {
        if layer.is_background {
            continue;
        }
        for shape in &layer.shapes {
            if let Shape::Component(component) = shape {
                if tree_has_subtraction(font, &component.reference, visiting) {
                    return true;
                }
            }
        }
    }
    false
}

fn glyphs_needing_boolean(font: &Font) -> Vec<SmolStr> {
    font.glyphs
        .iter()
        .filter(|glyph| tree_has_subtraction(font, &glyph.name, &mut HashSet::new()))
        .map(|glyph| glyph.name.clone())
        .collect()
}

fn apply_boolean_to_glyph(glyph: &mut Glyph) -> Result<(), BabelfontError> {
    for layer in &mut glyph.layers {
        if layer.is_background {
            continue;
        }
        apply_boolean_to_layer(layer)?;
    }
    Ok(())
}

impl FontFilter for Fip001Boolean {
    fn apply(&self, font: &mut Font) -> Result<(), BabelfontError> {
        let names = glyphs_needing_boolean(font);
        if names.is_empty() {
            return Ok(());
        }

        let mut working = font.clone();
        DecomposeComponentReferences::flatten_glyphs(&mut working, names.iter().cloned())?;
        for name in &names {
            let Some(glyph) = working.glyphs.get_mut(name) else {
                continue;
            };
            apply_boolean_to_glyph(glyph)?;
        }
        for name in names {
            if let (Some(src), Some(dst)) = (working.glyphs.get(&name), font.glyphs.get_mut(&name))
            {
                dst.layers = src.layers.clone();
            }
        }
        Ok(())
    }

    fn from_str(_s: &str) -> Result<Self, BabelfontError>
    where
        Self: Sized,
    {
        Ok(Fip001Boolean)
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        clap::Arg::new("fip001boolean")
            .long("fip001-boolean")
            .help(
                "Apply FIP001 path boolean subtraction (format_specific \
                 fip001-boolean=subtraction) after decomposing affected glyphs",
            )
            .action(clap::ArgAction::SetTrue)
    }
}

#[allow(clippy::unwrap_used, clippy::expect_used)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{common::FormatSpecific, shape::Component, Glyph, Layer, Node};

    fn rect_path(x0: f64, y0: f64, x1: f64, y1: f64) -> Path {
        Path {
            id: None,
            nodes: vec![
                Node::new_line(x0, y0),
                Node::new_line(x1, y0),
                Node::new_line(x1, y1),
                Node::new_line(x0, y1),
            ],
            closed: true,
            format_specific: FormatSpecific::default(),
        }
    }

    fn reverse_path(mut path: Path) -> Path {
        path.reverse();
        path
    }

    fn mark_subtraction(mut path: Path) -> Path {
        path.format_specific.insert(
            FIP001_BOOLEAN_KEY.to_string(),
            serde_json::Value::String(FIP001_BOOLEAN_SUBTRACTION.to_string()),
        );
        path
    }

    fn layer_with(paths: Vec<Path>) -> Layer {
        Layer {
            width: 1000.0,
            shapes: paths.into_iter().map(Shape::Path).collect(),
            ..Layer::default()
        }
    }

    fn filled_area(paths: &[Path]) -> f64 {
        paths
            .iter()
            .filter(|path| path.closed)
            .map(|path| path.signed_area().unwrap())
            .sum()
    }

    #[test]
    fn apply_transform_preserves_path_format_specific() {
        let path = mark_subtraction(rect_path(0.0, 0.0, 10.0, 10.0));
        let transformed = Shape::Path(path).apply_transform(Default::default());
        let out = transformed.as_path().unwrap();
        assert!(path_is_subtraction(out));
    }

    #[test]
    fn counters_survive_nonzero_unify() {
        let outer = rect_path(0.0, 0.0, 100.0, 100.0);
        let hole = reverse_path(rect_path(25.0, 25.0, 75.0, 75.0));
        let cutter = mark_subtraction(rect_path(40.0, -10.0, 60.0, 110.0));
        let result = apply_boolean_to_paths(vec![outer, hole, cutter]).unwrap();
        assert!(
            result.len() >= 2,
            "expected outer plus hole, got {}",
            result.len()
        );
        let area = filled_area(&result);
        assert!(
            area > 1000.0 && area < 8000.0,
            "counter should remain; area was {area}"
        );
    }

    #[test]
    fn non_intersecting_path_stays_byte_identical() {
        let distant = rect_path(0.0, 0.0, 10.0, 10.0);
        let cutter = mark_subtraction(rect_path(100.0, 100.0, 120.0, 120.0));
        let result = apply_boolean_to_paths(vec![distant.clone(), cutter]).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].nodes, distant.nodes);
        assert_eq!(result[0].closed, distant.closed);
    }

    #[test]
    fn later_path_covers_the_hole() {
        let subject = rect_path(0.0, 0.0, 100.0, 100.0);
        let cutter = mark_subtraction(rect_path(40.0, 40.0, 60.0, 60.0));
        let cover = rect_path(40.0, 40.0, 60.0, 60.0);
        let result = apply_boolean_to_paths(vec![subject, cutter, cover.clone()]).unwrap();
        let last = result.last().unwrap();
        assert_eq!(last.nodes, cover.nodes);
        let area = filled_area(&result);
        assert!(
            (area - 10_000.0).abs() < 50.0,
            "cover should fill the cut; area was {area}"
        );
    }

    #[test]
    fn two_cutters_apply_in_sequence() {
        let subject = rect_path(0.0, 0.0, 100.0, 100.0);
        let cutter_a = mark_subtraction(rect_path(10.0, 40.0, 30.0, 60.0));
        let cutter_b = mark_subtraction(rect_path(70.0, 40.0, 90.0, 60.0));
        let result = apply_boolean_to_paths(vec![subject, cutter_a, cutter_b]).unwrap();
        let area = filled_area(&result);
        assert!(
            (area - 9200.0).abs() < 50.0,
            "two 20x20 holes; area was {area}"
        );
    }

    #[test]
    fn component_flatten_then_subtract() {
        let mut base = Glyph::new("A");
        base.layers
            .push(layer_with(vec![rect_path(0.0, 0.0, 100.0, 100.0)]));

        let mut accent = Glyph::new("acutecomb");
        accent
            .layers
            .push(layer_with(vec![mark_subtraction(rect_path(
                40.0, 40.0, 60.0, 60.0,
            ))]));

        let mut composite = Glyph::new("Aacute");
        composite.layers.push(Layer {
            width: 1000.0,
            shapes: vec![
                Shape::Component(Component {
                    id: None,
                    reference: "A".into(),
                    transform: Default::default(),
                    location: Default::default(),
                    format_specific: Default::default(),
                }),
                Shape::Component(Component {
                    id: None,
                    reference: "acutecomb".into(),
                    transform: Default::default(),
                    location: Default::default(),
                    format_specific: Default::default(),
                }),
            ],
            ..Layer::default()
        });

        let mut font = Font::new();
        font.glyphs.0.extend([base, accent, composite]);

        Fip001Boolean.apply(&mut font).unwrap();

        let aacute = font.glyphs.get("Aacute").unwrap();
        assert!(
            aacute.layers[0]
                .shapes
                .iter()
                .all(|shape| shape.as_path().is_some()),
            "composite should be flattened"
        );
        let area = filled_area(&aacute.layers[0].paths().cloned().collect::<Vec<_>>());
        assert!(
            (area - 9600.0).abs() < 50.0,
            "accent cutter should punch the base; area was {area}"
        );

        let accent_out = font.glyphs.get("acutecomb").unwrap();
        assert!(
            accent_out.layers[0].paths().count() == 0
                || filled_area(&accent_out.layers[0].paths().cloned().collect::<Vec<_>>()).abs()
                    < 1.0,
            "a cutter-only glyph should compile to nothing"
        );
    }
}

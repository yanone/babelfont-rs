use kurbo::{cubics_to_quadratic_splines, BezPath, CubicBez, PathEl};
use smol_str::SmolStr;

use crate::{filters::FontFilter, Node, Path};

use super::curve_filter_common::{
    apply_interpolatable_path_filter, mark_closed_and_normalize, path_el_kind,
};

/// A filter that converts cubic Bézier curves to quadratic Bézier curves in all glyphs of a font, attempting to keep corresponding paths across layers consistent for better interpolation results. This filter requires the `kurbo` feature to be enabled.
#[derive(Debug, Clone, Default)]
pub struct CubicToQuadratic(Vec<SmolStr>);

const TOLERANCE: f64 = 0.5;

fn convert_bezpaths_in_parallel(paths: Vec<&BezPath>) -> Result<Vec<Path>, crate::BabelfontError> {
    if paths.is_empty() {
        return Ok(Vec::new());
    }

    let mut new_paths = vec![Path::default(); paths.len()];
    let mut all_elements = Vec::with_capacity(paths.len());

    for path in &paths {
        all_elements.push(path.elements());
    }

    if all_elements.iter().all(|elements| elements.is_empty()) {
        return Ok(new_paths);
    }
    if all_elements.iter().any(|elements| elements.is_empty()) {
        return Err(crate::BabelfontError::FilterError(
            "Parallel conversion requires all paths to be either empty or non-empty".to_string(),
        ));
    }

    let mut last_points = Vec::with_capacity(paths.len());
    for elements in &all_elements {
        let last_point = if let Some(PathEl::MoveTo(p)) = elements.first() {
            Some(*p)
        } else {
            // Closed contours may not start with a move command.
            elements.last().and_then(|el| el.end_point())
        };
        let Some(last_point) = last_point else {
            return Err(crate::BabelfontError::FilterError(
                "Cannot determine starting point for path during parallel conversion".to_string(),
            ));
        };
        last_points.push(last_point);
    }

    let first_len = all_elements[0].len();
    if all_elements
        .iter()
        .any(|elements| elements.len() != first_len)
    {
        return Err(crate::BabelfontError::FilterError(
            "Parallel conversion requires all paths to have the same number of elements"
                .to_string(),
        ));
    }

    for el_ix in 0..first_len {
        let expected_kind = path_el_kind(&all_elements[0][el_ix]);
        if all_elements
            .iter()
            .any(|elements| path_el_kind(&elements[el_ix]) != expected_kind)
        {
            return Err(crate::BabelfontError::FilterError(format!(
                "Parallel conversion requires matching element kinds at index {}",
                el_ix
            )));
        }

        match &all_elements[0][el_ix] {
            PathEl::MoveTo(_) => {
                for (path_ix, elements) in all_elements.iter().enumerate() {
                    let PathEl::MoveTo(p) = &elements[el_ix] else {
                        unreachable!("element kind checked above")
                    };
                    new_paths[path_ix].nodes.push(Node::new_move(p.x, p.y));
                    last_points[path_ix] = *p;
                }
            }
            PathEl::LineTo(_) => {
                for (path_ix, elements) in all_elements.iter().enumerate() {
                    let PathEl::LineTo(p) = &elements[el_ix] else {
                        unreachable!("element kind checked above")
                    };
                    new_paths[path_ix].nodes.push(Node::new_line(p.x, p.y));
                    last_points[path_ix] = *p;
                }
            }
            PathEl::QuadTo(_, _) => {
                for (path_ix, elements) in all_elements.iter().enumerate() {
                    let PathEl::QuadTo(p1, p2) = &elements[el_ix] else {
                        unreachable!("element kind checked above")
                    };
                    new_paths[path_ix]
                        .nodes
                        .push(Node::new_offcurve(p1.x, p1.y));
                    new_paths[path_ix].nodes.push(Node::new_qcurve(p2.x, p2.y));
                    last_points[path_ix] = *p2;
                }
            }
            PathEl::CurveTo(_, _, _) => {
                let mut cubics = Vec::with_capacity(all_elements.len());
                let mut end_points = Vec::with_capacity(all_elements.len());
                for (path_ix, elements) in all_elements.iter().enumerate() {
                    let PathEl::CurveTo(p1, p2, p3) = &elements[el_ix] else {
                        unreachable!("element kind checked above")
                    };
                    cubics.push(CubicBez::new(last_points[path_ix], *p1, *p2, *p3));
                    end_points.push(*p3);
                }

                let Some(quadsplines) = cubics_to_quadratic_splines(&cubics, TOLERANCE) else {
                    return Err(crate::BabelfontError::FilterError(format!(
                        "Failed to convert cubic segments to quadratic splines at element index {}",
                        el_ix
                    )));
                };
                if quadsplines.len() != new_paths.len() {
                    return Err(crate::BabelfontError::FilterError(format!(
                        "Parallel conversion produced {} splines for {} input paths",
                        quadsplines.len(),
                        new_paths.len()
                    )));
                }

                for (path_ix, spline) in quadsplines.iter().enumerate() {
                    // spline.points is [start, offcurves..., end]
                    let points = spline.points();
                    for (i, point) in points.iter().enumerate() {
                        if i == 0 {
                            // Start point equals the previous segment endpoint.
                        } else if i == points.len() - 1 {
                            new_paths[path_ix]
                                .nodes
                                .push(Node::new_qcurve(point.x, point.y));
                        } else {
                            new_paths[path_ix]
                                .nodes
                                .push(Node::new_offcurve(point.x, point.y));
                        }
                    }
                }

                for (path_ix, end_point) in end_points.into_iter().enumerate() {
                    last_points[path_ix] = end_point;
                }
            }
            PathEl::ClosePath => {
                mark_closed_and_normalize(&mut new_paths);
            }
        }
    }

    Ok(new_paths)
}

fn convert_bezpath_independently(path: &BezPath) -> Result<Path, crate::BabelfontError> {
    let converted = convert_bezpaths_in_parallel(vec![path])?;
    Ok(converted.into_iter().next().unwrap_or_default())
}

impl CubicToQuadratic {
    /// Create a new CubicToQuadratic filter.
    /// If `glyph_names` is empty, all glyphs are processed.
    pub fn new(glyph_names: Vec<String>) -> Self {
        CubicToQuadratic(glyph_names.into_iter().map(SmolStr::from).collect())
    }
}

impl FontFilter for CubicToQuadratic {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        apply_interpolatable_path_filter(
            font,
            "CubicToQuadratic",
            &self.0,
            convert_bezpath_independently,
            convert_bezpaths_in_parallel,
        )
    }

    fn from_str(s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(CubicToQuadratic(super::parse_glyph_list(s)))
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        super::glyph_filter_arg(
            "cubic2quadratic",
            "cubic-to-quadratic",
            "Convert cubic Bézier curves to quadratic Bézier curves",
        )
    }
}

#[allow(clippy::unwrap_used)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_final_segment() {
        let path: Path = serde_json::from_str(r#"
            {
              "nodes": "394 173 o 467 246 o 467 337 cs 467 427 o 394 500 o 304 500 cs 213 500 o 140 427 o 140 337 cs 140 246 o 213 173 o 304 173 cs",
              "closed": true
            }
"#).unwrap();
        let kurbo = path.to_kurbo().unwrap();
        let converted = convert_bezpath_independently(&kurbo).unwrap();
        assert!(converted.closed);
        // There should be no move node
        assert!(!converted
            .nodes
            .iter()
            .any(|n| n.nodetype == crate::NodeType::Move));
    }
}

//! Anchor inheritance matching the editor's `Layer.collectComputedAnchors`.
//!
//! A layer that already stores an incoming attachment anchor (`_top`, `_bottom`,
//! …) contributes nothing further. Otherwise each component's anchors are
//! transformed by that component's matrix, nested components are walked in
//! order, and a stored anchor wins over an inherited name. Later components
//! overwrite earlier names. The result does not include the queried layer's
//! own stored anchors.

use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::{Anchor, Font, Glyph, Layer, LayerType};
use fontdrasil::coords::DesignCoord;
use fontdrasil::types::Tag;

/// Inherited anchor name to rounded `(x, y)` in layer space.
pub type ComputedAnchorMap = BTreeMap<String, (i32, i32)>;

const COMPILE_FILTERS_KEY: &str = "com.counterpunch.compile-filters";

/// Inherited anchors for one layer, in that layer's coordinate space.
pub fn computed_anchors(font: &Font, glyph_name: &str, layer_id: Option<&str>) -> ComputedAnchorMap {
    let Some(glyph) = font.glyphs.get(glyph_name) else {
        return ComputedAnchorMap::new();
    };
    let Some(layer) = find_layer(glyph, layer_id) else {
        return ComputedAnchorMap::new();
    };
    let layer = resolution_layer(glyph, layer);
    walk(font, glyph, layer, &BTreeSet::new())
}

/// Copy inherited anchors onto `layer` for names it does not already store.
pub fn fill_missing_inherited_anchors(layer: &mut Layer, inherited: &ComputedAnchorMap) {
    let stored: HashSet<String> = layer
        .anchors
        .iter()
        .map(|anchor| anchor.name.clone())
        .collect();
    for (name, (x, y)) in inherited {
        if stored.contains(name) {
            continue;
        }
        layer.anchors.push(Anchor {
            name: name.clone(),
            x: f64::from(*x),
            y: f64::from(*y),
            ..Anchor::default()
        });
    }
}

/// Fill missing inherited anchors on every compilable layer.
///
/// Background layers and non-exporting glyphs are left alone.
pub fn fill_font_missing_inherited_anchors(font: &mut Font) {
    let names: Vec<String> = font
        .glyphs
        .iter()
        .filter(|glyph| glyph.exported)
        .map(|glyph| glyph.name.to_string())
        .collect();
    for name in names {
        let layer_count = font
            .glyphs
            .get(&name)
            .map(|glyph| glyph.layers.len())
            .unwrap_or(0);
        for index in 0..layer_count {
            let (layer_id, is_background) = {
                let Some(glyph) = font.glyphs.get(&name) else {
                    continue;
                };
                let Some(layer) = glyph.layers.get(index) else {
                    continue;
                };
                (layer.id.clone(), layer.is_background)
            };
            if is_background {
                continue;
            }
            let inherited = computed_anchors(font, &name, layer_id.as_deref());
            if inherited.is_empty() {
                continue;
            }
            if let Some(layer) = font
                .glyphs
                .get_mut(&name)
                .and_then(|glyph| glyph.layers.get_mut(index))
            {
                fill_missing_inherited_anchors(layer, &inherited);
            }
        }
    }
}

/// Plugin ids stored at `format_specific["com.counterpunch.compile-filters"]`.
pub fn linked_compile_filter_ids(font: &Font) -> Vec<String> {
    let Some(value) = font.format_specific.get(COMPILE_FILTERS_KEY) else {
        return Vec::new();
    };
    let Some(ids) = value.as_array() else {
        return Vec::new();
    };
    ids.iter()
        .filter_map(|id| id.as_str().map(str::to_string))
        .filter(|id| !id.is_empty())
        .collect()
}

fn walk(
    font: &Font,
    glyph: &Glyph,
    layer: &Layer,
    visited: &BTreeSet<String>,
) -> ComputedAnchorMap {
    let glyph_name = glyph.name.to_string();
    if visited.contains(&glyph_name) {
        return ComputedAnchorMap::new();
    }
    if has_incoming_attachment(layer) {
        return ComputedAnchorMap::new();
    }
    let mut next_visited = visited.clone();
    next_visited.insert(glyph_name);
    let mut computed = ComputedAnchorMap::new();
    for component in layer.components() {
        let Some(referenced) = font.glyphs.get(component.reference.as_ref()) else {
            continue;
        };
        let Some(child) = matching_layer(font, layer, referenced) else {
            continue;
        };
        let child = resolution_layer(referenced, child);
        let mut nested = walk(font, referenced, child, &next_visited);
        for (name, point) in own_anchors(child) {
            nested.insert(name, point);
        }
        let affine = component.transform.as_affine().as_coeffs();
        for (name, (x, y)) in nested {
            let px = affine[0] * f64::from(x) + affine[2] * f64::from(y) + affine[4];
            let py = affine[1] * f64::from(x) + affine[3] * f64::from(y) + affine[5];
            computed.insert(name, (px.round() as i32, py.round() as i32));
        }
    }
    computed
}

fn own_anchors(layer: &Layer) -> ComputedAnchorMap {
    let mut anchors = ComputedAnchorMap::new();
    for anchor in &layer.anchors {
        if anchor.name.is_empty() {
            continue;
        }
        anchors.insert(anchor.name.clone(), (anchor.x.round() as i32, anchor.y.round() as i32));
    }
    anchors
}

fn has_incoming_attachment(layer: &Layer) -> bool {
    layer
        .anchors
        .iter()
        .any(|anchor| anchor.name.starts_with('_'))
}

fn find_layer<'a>(glyph: &'a Glyph, layer_id: Option<&str>) -> Option<&'a Layer> {
    if let Some(layer_id) = layer_id {
        if let Some(layer) = glyph.layers.iter().find(|layer| layer.id.as_deref() == Some(layer_id))
        {
            return Some(layer);
        }
    }
    glyph.layers.first()
}

fn resolution_layer<'a>(glyph: &'a Glyph, layer: &'a Layer) -> &'a Layer {
    if !layer.is_background {
        return layer;
    }
    if let Some(id) = layer.id.as_deref() {
        if let Some(partner) = glyph.layers.iter().find(|other| {
            !other.is_background && other.background_layer_id.as_deref() == Some(id)
        }) {
            return partner;
        }
    }
    layer
}

fn matching_layer<'a>(font: &Font, source: &Layer, glyph: &'a Glyph) -> Option<&'a Layer> {
    let source_location = effective_location(font, source)?;
    for layer in &glyph.layers {
        let Some(location) = effective_location(font, layer) else {
            continue;
        };
        if locations_match(&source_location, &location, font) {
            return Some(layer);
        }
    }
    glyph.layers.first()
}

fn effective_location(font: &Font, layer: &Layer) -> Option<BTreeMap<String, f64>> {
    if let Some(location) = &layer.location {
        let mapped = location_map(location);
        if !mapped.is_empty() {
            return Some(mapped);
        }
    }
    let master_id = layer_master_id(layer)?;
    let master = font.masters.iter().find(|master| master.id == master_id)?;
    Some(location_map(&master.location))
}

fn layer_master_id(layer: &Layer) -> Option<&str> {
    match &layer.master {
        LayerType::DefaultForMaster(id) | LayerType::AssociatedWithMaster(id) => Some(id.as_str()),
        LayerType::FreeFloating => None,
    }
}

fn location_map(location: &fontdrasil::coords::DesignLocation) -> BTreeMap<String, f64> {
    location
        .iter()
        .map(|(tag, coord): (&Tag, &DesignCoord)| (tag.to_string(), coord.to_f64()))
        .collect()
}

fn locations_match(left: &BTreeMap<String, f64>, right: &BTreeMap<String, f64>, font: &Font) -> bool {
    let mut tags = BTreeSet::new();
    for axis in &font.axes {
        tags.insert(axis.tag.to_string());
    }
    tags.extend(left.keys().cloned());
    tags.extend(right.keys().cloned());
    for tag in tags {
        let left_value = left.get(&tag).copied().unwrap_or(0.0);
        let right_value = right.get(&tag).copied().unwrap_or(0.0);
        if left_value != right_value {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Component, Glyph, LayerType, Master, Shape};
    use fontdrasil::coords::DesignLocation;

    fn master(id: &str) -> Master {
        Master::new(id, id, DesignLocation::new())
    }

    fn layer(id: &str, master_id: &str, anchors: Vec<(&str, f64, f64)>) -> Layer {
        let mut layer = Layer::new(600.0);
        layer.id = Some(id.to_string());
        layer.master = LayerType::DefaultForMaster(master_id.to_string());
        layer.anchors = anchors
            .into_iter()
            .map(|(name, x, y)| Anchor {
                name: name.to_string(),
                x,
                y,
                ..Anchor::default()
            })
            .collect();
        layer
    }

    fn glyph(name: &str, layer: Layer) -> Glyph {
        Glyph {
            name: name.into(),
            layers: vec![layer],
            ..Glyph::default()
        }
    }

    #[test]
    fn composite_inherits_transformed_top_and_stored_anchor_wins() {
        let mut dot = layer("dot", "m", vec![("top", 100.0, 40.0), ("_top", 100.0, 0.0)]);
        dot.anchors[0].y = 40.0;
        let mut base = layer("base", "m", vec![("top", 200.0, 500.0)]);
        let mut component = Component::default();
        component.reference = "dot".into();
        component.transform.translation = (10.0, 20.0);
        base.shapes.push(Shape::Component(component));

        let mut font = Font::new();
        font.masters.push(master("m"));
        font.glyphs.0.push(glyph("dot", dot));
        font.glyphs.0.push(glyph("base", base));

        let inherited = computed_anchors(&font, "base", Some("base"));
        assert_eq!(inherited.get("top"), Some(&(110, 60)));
        assert_eq!(inherited.get("_top"), Some(&(110, 20)));

        let glyph = font.glyphs.get_mut("base").unwrap();
        fill_missing_inherited_anchors(&mut glyph.layers[0], &inherited);
        let names: Vec<&str> = glyph.layers[0]
            .anchors
            .iter()
            .map(|anchor| anchor.name.as_str())
            .collect();
        assert!(names.contains(&"top"));
        assert!(names.contains(&"_top"));
        let top = glyph.layers[0]
            .anchors
            .iter()
            .find(|anchor| anchor.name == "top")
            .unwrap();
        assert_eq!(top.y, 500.0);
    }

    #[test]
    fn incoming_attachment_anchor_stops_the_walk() {
        let mut mark = layer("mark", "m", vec![("_top", 0.0, 0.0)]);
        let mut nested = Component::default();
        nested.reference = "dot".into();
        mark.shapes.push(Shape::Component(nested));
        let dot = layer("dot", "m", vec![("top", 5.0, 9.0)]);
        let mut font = Font::new();
        font.masters.push(master("m"));
        font.glyphs.0.push(glyph("dot", dot));
        font.glyphs.0.push(glyph("mark", mark));
        assert!(computed_anchors(&font, "mark", None).is_empty());
    }
}

use fea_rs_ast::LanguageSystemStatement;
use fontdrasil::{
    coords::{
        ConvertSpace, DesignCoord, DesignLocation, NormalizedCoord, NormalizedSpace, UserCoord,
    },
    types::Axes,
};
use icu_properties::{
    CodePointMapData, CodePointSetData,
    props::{DefaultIgnorableCodePoint, GeneralCategory},
};
use indexmap::IndexMap;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use skrifa::{
    GlyphId, GlyphNames, MetadataProvider,
    outline::DrawSettings,
    prelude::{LocationRef, Size},
    raw::{
        TableProvider,
        tables::{glyf, kern},
    },
    string::StringId,
};
use smol_str::SmolStr;
use sr_aef::fea_rs_ast::AsFea;
use std::collections::{HashMap, HashSet};
use write_fonts::types::F2Dot14;

use crate::layout::expand::expand_glyph_container;

use crate::{
    Anchor, BabelfontError, Features, Font, FormatSpecific, Glyph, GlyphCategory, Instance, Layer,
    LayerType, MetricType, Tag, features::PossiblyAutomaticCode,
};

/// Load a TTF font from a file path
pub fn load<T: AsRef<std::path::Path>>(path: T) -> Result<Font, BabelfontError> {
    let binary = std::fs::read(path.as_ref()).map_err(|e| BabelfontError::IO(e.to_string()))?;
    load_bytes(&binary)
}

/// Load a TTF font from its bytes
pub fn load_bytes(binary: &[u8]) -> Result<Font, BabelfontError> {
    let mut font = Font::new();
    let fontref =
        skrifa::FontRef::new(binary).map_err(|e| BabelfontError::BinaryFontRead(e.to_string()))?;
    check_outline_format(&fontref)?;
    font.upm = fontref.head()?.units_per_em();
    font.version = font_version(&fontref)?;
    font.date = font_date(&fontref)?;

    load_axes(&fontref, &mut font)?;
    load_axis_mappings(&fontref, &mut font)?;
    load_cross_axis_mappings(&fontref, &mut font)?;
    load_names(&fontref, &mut font)?;
    load_custom_ot_values(&fontref, &mut font)?;
    load_instances(&fontref, &mut font)?;
    load_masters(&fontref, &mut font)?;
    load_glyphs(&fontref, &mut font)?;
    let provisional = load_features(&fontref, &mut font)?;
    load_stat(&fontref, &mut font)?;
    load_legacy_kerning(&fontref, &mut font)?;
    name_unnamed_glyphs(&fontref, &mut font)?;
    load_variation_sequences(&fontref, &mut font);
    name_placeholder_anchors(&mut font, &provisional);
    {
        use crate::filters::FontFilter;
        crate::filters::DropImpliedOnCurves.apply(&mut font)?;
        mark_smooth_points(&mut font);
        let props = glyph_props(binary, &font);
        crate::filters::MoveKerningFromFeatures::with_glyph_props(props.clone())
            .apply(&mut font)?;
        crate::filters::MoveMarksFromFeatures::with_glyph_props(props).apply(&mut font)?;
    }
    Ok(font)
}

/// Only TrueType outlines can be read; any other font is refused up front,
/// saying what it has instead
fn check_outline_format(fontref: &skrifa::FontRef) -> Result<(), BabelfontError> {
    if fontref.glyf().is_ok() && fontref.loca(None).is_ok() {
        return Ok(());
    }
    let has = |tag: &[u8; 4]| fontref.table_data(Tag::new(tag)).is_some();
    let instead = if has(b"CFF ") || has(b"CFF2") {
        "this font has CFF outlines"
    } else if has(b"sbix") || has(b"CBDT") || has(b"EBDT") || has(b"bdat") {
        "this font has bitmap glyphs only"
    } else {
        "this font has no glyf table"
    };
    Err(BabelfontError::BinaryFontRead(format!(
        "Only TrueType-outline (glyf) fonts can be opened; {instead}"
    )))
}

/// Glyphs' codepoints, and the substitutions of the font's GSUB to carry their
/// properties to the glyphs they reach, as a compiler of the same feature code
/// follows them
fn glyph_props(binary: &[u8], font: &Font) -> crate::filters::layout_model::GlyphProps {
    use skrifa::raw::collections::IntSet;
    let data: std::sync::Arc<[u8]> = binary.into();
    let closure: crate::filters::layout_model::Closure =
        std::sync::Arc::new(move |glyphs: &mut std::collections::BTreeSet<usize>| {
            let Ok(fontref) = skrifa::FontRef::new(&data) else {
                return;
            };
            let Ok(gsub) = fontref.gsub() else {
                return;
            };
            let Ok(lookups) = gsub.collect_lookups(&IntSet::all()) else {
                return;
            };
            let mut set: IntSet<GlyphId> = glyphs.iter().map(|g| GlyphId::new(*g as u32)).collect();
            if gsub.closure_glyphs(&lookups, &mut set).is_ok() {
                glyphs.extend(set.iter().map(|g| g.to_u32() as usize));
            }
        });
    crate::filters::layout_model::GlyphProps::new(font, Some(closure))
}

/// Names each glyph that the post table leaves unnamed and the cmap encodes for
/// its codepoint, as the Adobe Glyph List Specification does (uniXXXX, uXXXXX),
/// wherever the glyph is used.
fn name_unnamed_glyphs(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    use crate::filters::FontFilter;
    let names = GlyphNames::new(fontref);
    let mut taken: HashSet<SmolStr> = font.glyphs.iter().map(|g| g.name.clone()).collect();
    let mut renames = HashMap::new();
    // glyph 0 is .notdef
    for (gid, glyph) in font.glyphs.iter().enumerate().skip(1) {
        if !names
            .get(GlyphId::new(gid as u32))
            .is_some_and(|name| name.is_synthesized())
        {
            continue;
        }
        let Some(codepoint) = glyph.codepoints.iter().min() else {
            continue;
        };
        let name: SmolStr = if *codepoint <= 0xFFFF {
            format!("uni{codepoint:04X}").into()
        } else {
            format!("u{codepoint:05X}").into()
        };
        if taken.insert(name.clone()) {
            renames.insert(glyph.name.to_string(), name.to_string());
        }
    }
    if !renames.is_empty() {
        crate::filters::RenameGlyphs::new(renames).apply(font)?;
    }
    Ok(())
}

/// How far from a straight line, as the sine of the angle, a smooth point's
/// neighbours may be
const SMOOTH_TOLERANCE: f64 = 0.005;

/// Whether an on-curve point runs on along its neighbours' tangent: they lie on
/// one line through it, on either side, and one of them is an off-curve (a
/// point between two lines is a corner)
fn runs_smoothly(path: &crate::Path, index: usize) -> bool {
    let nodes = &path.nodes;
    let count = nodes.len();
    let Some(node) = nodes.get(index) else {
        return false;
    };
    if node.nodetype == crate::NodeType::OffCurve || count < 3 {
        return false;
    }
    if !path.closed && (index == 0 || index + 1 == count) {
        return false;
    }
    let previous = &nodes[(index + count - 1) % count];
    let next = &nodes[(index + 1) % count];
    if previous.nodetype != crate::NodeType::OffCurve && next.nodetype != crate::NodeType::OffCurve
    {
        return false;
    }
    let (ax, ay) = (previous.x - node.x, previous.y - node.y);
    let (bx, by) = (next.x - node.x, next.y - node.y);
    let (a, b) = ((ax * ax + ay * ay).sqrt(), (bx * bx + by * by).sqrt());
    if a == 0.0 || b == 0.0 {
        return false;
    }
    let sine = (ax * by - ay * bx) / (a * b);
    let opposite = ax * bx + ay * by < 0.0;
    opposite && sine.abs() <= SMOOTH_TOLERANCE
}

/// Marks smooth points, which TrueType does not store: on-curve points that
/// run smoothly in every layer of their glyph that interpolates
fn mark_smooth_points(font: &mut Font) {
    for glyph in font.glyphs.iter_mut() {
        let layers: Vec<usize> = (0..glyph.layers.len())
            .filter(|&i| !glyph.layers[i].is_background)
            .collect();
        let shapes = |layer: &Layer| -> Vec<Vec<crate::NodeType>> {
            layer
                .paths()
                .map(|p| p.nodes.iter().map(|n| n.nodetype).collect())
                .collect()
        };
        let Some(first) = layers.first().map(|&i| shapes(&glyph.layers[i])) else {
            continue;
        };
        if layers.iter().any(|&i| shapes(&glyph.layers[i]) != first) {
            continue;
        }
        let smooth: Vec<Vec<bool>> = first
            .iter()
            .enumerate()
            .map(|(path_index, types)| {
                (0..types.len())
                    .map(|node_index| {
                        layers.iter().all(|&i| {
                            glyph.layers[i]
                                .paths()
                                .nth(path_index)
                                .is_some_and(|p| runs_smoothly(p, node_index))
                        })
                    })
                    .collect()
            })
            .collect();
        for &i in &layers {
            let paths = glyph.layers[i].shapes.iter_mut().filter_map(|s| match s {
                crate::Shape::Path(p) => Some(p),
                _ => None,
            });
            for (path, flags) in paths.zip(&smooth) {
                for (node, &flag) in path.nodes.iter_mut().zip(flags) {
                    node.smooth = flag;
                }
            }
        }
    }
}

/// The prefix of the names sr-aef gives anchors it found no name for
const PLACEHOLDER_ANCHOR_PREFIX: &str = "mark_class_";

/// The anchor a mark attaches by, as what the mark is says: from its Unicode
/// combining class, else from its description. Arabic harakat have classes of
/// their own (27-35), above but for kasra and kasratan.
fn anchor_name_for_mark(codepoint: Option<u32>, description: Option<&str>) -> Option<&'static str> {
    use icu_properties::props::CanonicalCombiningClass;
    let description = description.unwrap_or_default().to_ascii_uppercase();
    if description.contains("OGONEK") {
        return Some("ogonek");
    }
    if let Some(c) = codepoint.and_then(char::from_u32) {
        let class = CodePointMapData::<CanonicalCombiningClass>::new()
            .get(c)
            .to_icu4c_value();
        let name = match class {
            1 => Some("center"),
            202 | 220 | 29 | 32 => Some("bottom"),
            218 => Some("bottomleft"),
            222 => Some("bottomright"),
            214 | 230 | 27 | 28 | 30 | 31 | 33 | 34 | 35 => Some("top"),
            228 => Some("topleft"),
            232 => Some("topright"),
            _ => None,
        };
        if name.is_some() {
            return name;
        }
    }
    if description.contains("OVERLAY") {
        Some("center")
    } else if description.contains(" BELOW") {
        Some("bottom")
    } else if description.contains(" ABOVE") {
        Some("top")
    } else {
        None
    }
}

/// What a mark glyph is: its codepoint and description, from the font's cmap,
/// else from Glyphs.app's glyph data for its name (or its name before a suffix)
fn mark_identity(glyph: &Glyph) -> (Option<u32>, Option<String>) {
    let codepoint = glyph.codepoints.iter().min().copied().or_else(|| {
        // a production name states its codepoint
        glyph
            .name
            .strip_prefix("uni")
            .filter(|hex| hex.len() == 4)
            .and_then(|hex| u32::from_str_radix(hex, 16).ok())
    });
    #[cfg(feature = "glyphs")]
    {
        use crate::filters::glyphsdata::glyph_info;
        if let Some(info) = glyph_info(&glyph.name) {
            let data_codepoint = info
                .unicode
                .as_deref()
                .and_then(|hex| u32::from_str_radix(hex, 16).ok());
            return (codepoint.or(data_codepoint), info.description.clone());
        }
    }
    (codepoint, None)
}

/// The script a glyph writes, as Glyphs.app's glyph data has it, found by its
/// codepoint, else its name. None for marks and symbols of every script.
#[cfg(feature = "glyphs")]
fn glyph_script(glyph: &Glyph) -> Option<String> {
    use crate::filters::glyphsdata::{glyph_info, glyph_info_for_codepoint};
    glyph
        .codepoints
        .iter()
        .find_map(|&c| glyph_info_for_codepoint(c))
        .or_else(|| glyph_info(&glyph.name))
        .and_then(|info| info.script.clone())
}

/// The glyphs with an anchor of a name, as bases (`name`) and as marks (`_name`)
fn anchor_family(font: &Font, name: &str) -> (HashSet<SmolStr>, HashSet<SmolStr>) {
    let mark_name = format!("_{name}");
    let mut bases = HashSet::new();
    let mut marks = HashSet::new();
    for glyph in font.glyphs.iter() {
        for anchor in glyph.layers.iter().flat_map(|l| l.anchors.iter()) {
            if anchor.name == name {
                bases.insert(glyph.name.clone());
            } else if anchor.name == mark_name {
                marks.insert(glyph.name.clone());
            }
        }
    }
    (bases, marks)
}

/// Whether two anchor families may share a name: no glyph is in both, their
/// bases write different scripts, each known, and each family's marks are of
/// its bases' script only
fn may_share_name(
    font: &Font,
    (bases, marks): &(HashSet<SmolStr>, HashSet<SmolStr>),
    (other_bases, other_marks): &(HashSet<SmolStr>, HashSet<SmolStr>),
) -> bool {
    let glyphs: HashSet<&SmolStr> = bases.iter().chain(marks).collect();
    if other_bases
        .iter()
        .chain(other_marks)
        .any(|g| glyphs.contains(g))
    {
        return false;
    }
    #[cfg(feature = "glyphs")]
    {
        let scripts = |names: &HashSet<SmolStr>| -> Option<HashSet<String>> {
            names
                .iter()
                .map(|name| font.glyphs.get(name).and_then(glyph_script))
                .collect()
        };
        // marks of every script (combining accents) attach to bases of any:
        // a shared name would attach them across scripts
        let marks_match = |marks: &HashSet<SmolStr>, bases: &HashSet<String>| {
            scripts(marks).is_some_and(|m| !m.is_empty() && m.is_subset(bases))
        };
        match (scripts(bases), scripts(other_bases)) {
            (Some(a), Some(b)) => {
                !a.is_empty()
                    && !b.is_empty()
                    && a.is_disjoint(&b)
                    && marks_match(marks, &a)
                    && marks_match(other_marks, &b)
            }
            _ => false,
        }
    }
    #[cfg(not(feature = "glyphs"))]
    {
        let _ = font;
        false
    }
}

/// Names the anchors sr-aef left as placeholders after the marks that attach
/// by them, where every mark that says what it is agrees on a name. The name
/// is the class's own if no other anchor has it, and the mark class in the
/// feature code is renamed with it. It is shared with another class only where
/// no glyph has both and their bases write different scripts; the feature code
/// then keeps its class names apart.
fn name_placeholder_anchors(font: &mut Font, provisional: &HashSet<String>) {
    let all_anchor_names = |font: &Font| -> HashSet<String> {
        font.glyphs
            .iter()
            .flat_map(|g| g.layers.iter())
            .flat_map(|l| l.anchors.iter().map(|a| a.name.to_string()))
            .collect()
    };
    let mut placeholders: Vec<String> = all_anchor_names(font)
        .iter()
        .filter_map(|name| name.strip_prefix('_'))
        .filter(|name| name.starts_with(PLACEHOLDER_ANCHOR_PREFIX) || provisional.contains(*name))
        .map(str::to_string)
        .collect();
    placeholders.sort();
    for placeholder in placeholders {
        let family = anchor_family(font, &placeholder);
        let names: HashSet<&'static str> = family
            .1
            .iter()
            .filter_map(|name| font.glyphs.get(name))
            .filter_map(|g| {
                let (codepoint, description) = mark_identity(g);
                anchor_name_for_mark(codepoint, description.as_deref())
            })
            .collect();
        let mut names = names.into_iter();
        let (Some(name), None) = (names.next(), names.next()) else {
            continue;
        };
        let taken = all_anchor_names(font)
            .iter()
            .any(|existing| existing.trim_start_matches('_') == name);
        let fea_class_taken = font.features.to_fea().contains(&format!("@{name}"));
        if taken {
            if !may_share_name(font, &family, &anchor_family(font, name)) {
                continue;
            }
        } else if fea_class_taken {
            continue;
        }
        rename_anchor_family(font, &placeholder, name, !taken);
    }
}

/// Renames anchors `from` and `_from` to `to` and `_to`, and with
/// `rename_class` the mark class `@from` in the feature code
fn rename_anchor_family(font: &mut Font, from: &str, to: &str, rename_class: bool) {
    let mark_from = format!("_{from}");
    for layer in font.glyphs.iter_mut().flat_map(|g| g.layers.iter_mut()) {
        for anchor in layer.anchors.iter_mut() {
            if anchor.name == from {
                anchor.name = to.to_string();
            } else if anchor.name == mark_from {
                anchor.name = format!("_{to}");
            }
        }
    }
    if !rename_class {
        return;
    }
    let Ok(pattern) = regex::Regex::new(&format!(r"@{}\b", regex::escape(from))) else {
        return;
    };
    let rename = |code: &mut String| {
        *code = pattern.replace_all(code, format!("@{to}")).into_owned();
    };
    for code in font.features.prefixes.values_mut() {
        rename(&mut code.code);
    }
    for (_, code) in font.features.features.iter_mut() {
        rename(&mut code.code);
    }
}

/// head.fontRevision as major and minor versions, the minor in thousandths as
/// Glyphs and UFO state it: 0x00003333 (0.19999) is 0.200
fn font_version(fontref: &skrifa::FontRef) -> Result<(u16, u16), BabelfontError> {
    let revision = fontref.head()?.font_revision().to_f64();
    let thousandths = (revision * 1000.0).round();
    Ok((
        (thousandths / 1000.0).floor() as u16,
        (thousandths % 1000.0) as u16,
    ))
}

/// The language key of a name record, as the SFD reader keys `LangName` (#118):
/// US English is the default; other Windows languages are their layout tag, else
/// their BCP-47 tag. A Macintosh language is keyed as the Windows language it
/// names, English as the default.
fn name_record_language(platform_id: u16, language_id: u16) -> Option<String> {
    use crate::names::{LANGUAGE_ID_TO_BCP47, ot_lang_id_to_iso_tag, ot_lang_id_to_name_language};
    match platform_id {
        3 if language_id == 0x0409 => Some(crate::i18ndictionary::DFLT.to_string()),
        3 => ot_lang_id_to_name_language(language_id).map(str::to_string),
        1 if language_id == 0 => Some(crate::i18ndictionary::DFLT.to_string()),
        1 => {
            let bcp47 = ot_lang_id_to_iso_tag(language_id)?;
            let windows = LANGUAGE_ID_TO_BCP47
                .iter()
                .filter(|&&(id, tag)| {
                    id >= 0x0400 && (tag == bcp47 || tag.starts_with(&format!("{bcp47}-")))
                })
                .map(|&(id, _)| id)
                .min();
            Some(
                windows
                    .and_then(|id| name_record_language(3, id))
                    .unwrap_or_else(|| bcp47.to_string()),
            )
        }
        _ => None,
    }
}

/// A name's strings by language. A Macintosh record is read only where no Windows
/// record states that name in its language.
fn name_id_to_i18n(fontref: &skrifa::FontRef, name_id: StringId) -> crate::I18NDictionary {
    let mut dict = crate::I18NDictionary::new();
    let Ok(name) = fontref.name() else {
        return dict;
    };
    let mut mac = vec![];
    for record in name.name_record().iter().filter(|r| r.name_id() == name_id) {
        let Some(lang) = name_record_language(record.platform_id(), record.language_id()) else {
            continue;
        };
        // a record that does not decode reads as empty
        let value = record
            .string(name.string_data())
            .map(|s| s.to_string())
            .unwrap_or_default();
        if value.is_empty() {
            continue;
        }
        if record.platform_id() == 3 {
            dict.insert(lang, value);
        } else {
            mac.push((lang, value));
        }
    }
    for (lang, value) in mac {
        if !dict.0.contains_key(&lang) {
            dict.insert(lang, value);
        }
    }
    dict
}

fn load_axes(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    for axis in fontref.axes().iter() {
        let name = name_id_to_i18n(fontref, axis.name_id());
        let tag = axis.tag();
        let min = axis.min_value();
        let default = axis.default_value();
        let max = axis.max_value();
        font.axes.push(crate::Axis {
            name,
            tag: crate::Tag::from_be_bytes(tag.to_be_bytes()), // hateful
            min: Some(UserCoord::new(min as f64)),
            default: Some(UserCoord::new(default as f64)),
            max: Some(UserCoord::new(max as f64)),
            hidden: axis.is_hidden(),
            ..Default::default()
        });
    }
    Ok(())
}

fn load_axis_mappings(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    // Skrifa doesn't support axis mappings, handle it ourselves
    if let Ok(avar) = fontref.avar() {
        for (axis, segmap) in font.axes.iter_mut().zip(avar.axis_segment_maps().iter()) {
            let converter = axis._converter()?;
            let segmap = segmap.map_err(|e| BabelfontError::BinaryFontRead(e.to_string()))?;
            if segmap.axis_value_maps().len() == 3 {
                // -1/0/1, default
                continue;
            }
            let mut map_vec = vec![];
            for map in segmap.axis_value_maps() {
                map_vec.push((
                    user_at(map.from_coordinate(), &converter),
                    design_at(map.to_coordinate(), &converter),
                ));
            }
            axis.map = Some(map_vec);
        }
    }
    Ok(())
}

/// The value with the fewest decimals, up to three, that `quantize` stores as
/// it stores `value`: what a source said before a binary rounded it
fn shortest<T: PartialEq>(value: f64, quantize: impl Fn(f64) -> T) -> f64 {
    let stored = quantize(value);
    (0..4)
        .map(|digits| {
            let scale = 10f64.powi(digits);
            (value * scale).round() / scale
        })
        .find(|rounded| quantize(*rounded) == stored)
        .unwrap_or(value)
}

/// The user coordinate of a normalized coordinate before avar, as a source states
/// it: avar stores user 600 as an F2Dot14 that reads back as 600.0122
fn user_at(coord: F2Dot14, converter: &fontdrasil::coords::CoordConverter) -> UserCoord {
    let user = NormalizedCoord::new(coord.to_f32() as f64)
        .to_user(converter)
        .to_f64();
    UserCoord::new(shortest(user, |u| {
        UserCoord::new(u).to_normalized(converter).to_f2dot14()
    }))
}

/// The design coordinate of a normalized coordinate (after avar), as a source
/// states it
fn design_at(coord: F2Dot14, converter: &fontdrasil::coords::CoordConverter) -> DesignCoord {
    let design = NormalizedCoord::new(coord.to_f32() as f64)
        .to_design(converter)
        .to_f64();
    DesignCoord::new(shortest(design, |d| {
        DesignCoord::new(d).to_normalized(converter).to_f2dot14()
    }))
}

/// The peaks of a variation store's regions that move anything
fn variation_store_peaks(
    store: &skrifa::raw::tables::variations::ItemVariationStore,
) -> Result<Vec<Vec<F2Dot14>>, BabelfontError> {
    let regions = store.variation_region_list()?.variation_regions();
    let mut peaks: Vec<Vec<F2Dot14>> = vec![];
    for data in store.item_variation_data().iter().flatten() {
        let Ok(data) = data else { continue };
        for (column, region_index) in data.region_indexes().iter().enumerate() {
            let moves = (0..data.item_count())
                .any(|item| data.delta_set(item).nth(column).is_some_and(|d| d != 0));
            let Ok(region) = regions.get(region_index.get() as usize) else {
                continue;
            };
            let peak: Vec<F2Dot14> = region
                .region_axes()
                .iter()
                .map(|axis| axis.peak_coord())
                .collect();
            if moves && !peaks.contains(&peak) {
                peaks.push(peak);
            }
        }
    }
    Ok(peaks)
}

/// avar version 2's mappings between locations: each region of its variation
/// store peaks where a mapping's input is; its output is the input moved by
/// the store's deltas there
fn load_cross_axis_mappings(
    fontref: &skrifa::FontRef,
    font: &mut Font,
) -> Result<(), BabelfontError> {
    let Ok(avar) = fontref.avar() else {
        return Ok(());
    };
    let Some(Ok(store)) = avar.var_store() else {
        return Ok(());
    };
    // without an axis index map, axis i's deltas are item i of the store's
    // first item variation data (fontTools leaves out a map that says so)
    let index_map = match avar.axis_index_map() {
        Some(Ok(map)) => Some(map),
        Some(Err(e)) => return Err(BabelfontError::BinaryFontRead(e.to_string())),
        None => None,
    };
    let delta_index = |axis: usize| match &index_map {
        Some(map) => map.get(axis as u32).ok(),
        None => u16::try_from(axis)
            .ok()
            .map(|inner| skrifa::raw::tables::variations::DeltaSetIndex { outer: 0, inner }),
    };
    let converters = font
        .axes
        .iter()
        .map(|axis| axis._converter())
        .collect::<Result<Vec<_>, _>>()?;
    for peak in variation_store_peaks(&store)? {
        let mut input = DesignLocation::new();
        let mut output = DesignLocation::new();
        let mut moved = false;
        for (index, ((axis, converter), coord)) in
            font.axes.iter().zip(&converters).zip(&peak).enumerate()
        {
            let delta = delta_index(index)
                .and_then(|delta_index| store.compute_delta(delta_index, &peak).ok())
                .unwrap_or(0);
            moved |= delta != 0;
            // avar2 clamps a mapped coordinate to -1..1
            let mapped =
                F2Dot14::from_bits((coord.to_bits() as i32 + delta).clamp(-0x4000, 0x4000) as i16);
            input.insert(axis.tag, design_at(*coord, converter));
            output.insert(axis.tag, design_at(mapped, converter));
        }
        if moved {
            font.cross_axis_mappings
                .push(crate::axis::CrossAxisMapping {
                    description: None,
                    input,
                    output,
                    active: true,
                });
        }
    }
    Ok(())
}

/// A string as feature code writes it, for Windows: other than printable
/// ASCII as `\XXXX` escapes
fn fea_string(text: &str) -> String {
    let mut out = String::new();
    for unit in text.encode_utf16() {
        match char::from_u32(unit as u32) {
            Some(c) if c.is_ascii() && !c.is_ascii_control() && c != '"' && c != '\\' => {
                out.push(c)
            }
            _ => out.push_str(&format!("\\{unit:04x}")),
        }
    }
    out
}

/// The STAT table, as feature code: a compiler that finds it there writes it
/// as it is, rather than the simpler one it builds itself
fn load_stat(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    use skrifa::raw::tables::stat::AxisValue;
    let Ok(stat) = fontref.stat() else {
        return Ok(());
    };
    // a name's statements: its default, then its other languages (Windows)
    let name = |id: StringId| -> String {
        let names = name_id_to_i18n(fontref, id);
        let mut statements = vec![format!(
            "name \"{}\";",
            fea_string(names.get_default().map(String::as_str).unwrap_or_default())
        )];
        for (language, value) in names.0.iter() {
            match crate::names::name_language_to_ot_lang_id(language) {
                Some(0x0409) | None => {}
                Some(lang_id) => statements.push(format!(
                    "name 3 1 0x{lang_id:04X} \"{}\";",
                    fea_string(value)
                )),
            }
        }
        statements.join(" ")
    };
    let design_axes = stat.design_axes()?;
    let tag_of = |index: u16| design_axes.get(index as usize).map(|a| a.axis_tag());
    let mut lines = vec!["table STAT {".to_string()];
    // by its name: a compiler knows only the names feature code states
    if let Some(id) = stat.elided_fallback_name_id() {
        lines.push(format!("    ElidedFallbackName {{ {} }};", name(id)));
    }
    for axis in design_axes {
        lines.push(format!(
            "    DesignAxis {} {} {{ {} }};",
            axis.axis_tag(),
            axis.axis_ordering(),
            name(axis.axis_name_id())
        ));
    }
    let number = |value: skrifa::raw::types::Fixed| {
        let value = value.to_f64();
        if value.fract() == 0.0 {
            format!("{value:.0}")
        } else {
            format!("{value}")
        }
    };
    if let Some(Ok(values)) = stat.offset_to_axis_values() {
        for value in values.axis_values().iter().flatten() {
            let (flags, name_id, locations) = match &value {
                AxisValue::Format1(v) => (
                    v.flags(),
                    v.value_name_id(),
                    vec![format!(
                        "{} {}",
                        tag_of(v.axis_index()).unwrap_or_default(),
                        number(v.value())
                    )],
                ),
                AxisValue::Format2(v) => (
                    v.flags(),
                    v.value_name_id(),
                    vec![format!(
                        "{} {} {} {}",
                        tag_of(v.axis_index()).unwrap_or_default(),
                        number(v.nominal_value()),
                        number(v.range_min_value()),
                        number(v.range_max_value())
                    )],
                ),
                AxisValue::Format3(v) => (
                    v.flags(),
                    v.value_name_id(),
                    vec![format!(
                        "{} {} {}",
                        tag_of(v.axis_index()).unwrap_or_default(),
                        number(v.value()),
                        number(v.linked_value())
                    )],
                ),
                AxisValue::Format4(v) => (
                    v.flags(),
                    v.value_name_id(),
                    v.axis_values()
                        .iter()
                        .map(|record| {
                            format!(
                                "{} {}",
                                tag_of(record.axis_index()).unwrap_or_default(),
                                number(record.value())
                            )
                        })
                        .collect(),
                ),
            };
            lines.push("    AxisValue {".to_string());
            for location in locations {
                lines.push(format!("        location {location};"));
            }
            lines.push(format!("        {}", name(name_id)));
            let mut flag_names = vec![];
            if flags.bits() & 0x1 != 0 {
                flag_names.push("OlderSiblingFontAttribute");
            }
            if flags.bits() & 0x2 != 0 {
                flag_names.push("ElidableAxisValueName");
            }
            if !flag_names.is_empty() {
                lines.push(format!("        flag {};", flag_names.join(" ")));
            }
            lines.push("    };".to_string());
        }
    }
    lines.push("} STAT;".to_string());
    font.features
        .prefixes
        .insert("STAT".into(), PossiblyAutomaticCode::new(lines.join("\n")));
    Ok(())
}

fn load_names(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    for name_id in fontref
        .name()?
        .name_record()
        .iter()
        .map(|record| record.name_id())
        .filter(|id| id.to_u16() < 256)
        .collect::<Vec<_>>()
    {
        if let Some(record) = font.names.get_mut(name_id) {
            *record = name_id_to_i18n(fontref, name_id);
        }
    }
    // IDs 1 and 2 as they are: a compiler that derives them from the family and
    // style names joins a style other than the four style-linked ones to the
    // family ("Big Caslon Medium"), and a TTF's need not be those four (Hoefler
    // Text Ornaments' style is "Ornaments"). As UFO states them (see
    // `ufo::KEY_STYLE_MAP_FAMILY_NAME`).
    for (name_id, key) in [
        (StringId::FAMILY_NAME, "ufo.styleMapFamilyName"),
        (StringId::SUBFAMILY_NAME, "ufo.styleMapStyleName"),
    ] {
        if let Some(name) = name_id_to_i18n(fontref, name_id).get_default() {
            font.format_specific
                .insert(key.into(), serde_json::Value::String(name.clone()));
        }
    }
    Ok(())
}

fn load_custom_ot_values(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    let ot = &mut font.custom_ot_values;
    ot.head_lowest_rec_ppem = Some(fontref.head()?.lowest_rec_ppem());
    // Old Apple fonts have no OS/2 table; a compiler writes its defaults
    let Ok(os2) = fontref.os2() else {
        log::warn!("The font has no OS/2 table; its values are left to the compiler");
        return Ok(());
    };
    ot.os2_vendor_id = Some(os2.ach_vend_id());
    ot.os2_fs_type = Some(os2.fs_type());
    ot.os2_family_class = Some(os2.s_family_class() as u16);
    ot.os2_panose = os2.panose_10().try_into().ok();
    // all of it: a binary's ITALIC, BOLD and REGULAR need not follow its style
    // name (Bradley Hand Bold is not BOLD)
    ot.os2_fs_selection = Some(os2.fs_selection().bits());
    ot.os2_unicode_range1 = Some(os2.ul_unicode_range_1());
    ot.os2_unicode_range2 = Some(os2.ul_unicode_range_2());
    ot.os2_unicode_range3 = Some(os2.ul_unicode_range_3());
    ot.os2_unicode_range4 = Some(os2.ul_unicode_range_4());
    // OS/2 version 0 has no code page ranges
    ot.os2_code_page_range1 = os2.ul_code_page_range_1();
    ot.os2_code_page_range2 = os2.ul_code_page_range_2();
    if let Ok(post) = fontref.post() {
        ot.post_is_fixed_pitch = Some(post.is_fixed_pitch() != 0);
        ot.post_italic_angle = Some(shortest(post.italic_angle().to_f64(), |angle| {
            skrifa::raw::types::Fixed::from_f64(angle)
        }));
    }
    // A compiler derives a variable font's weight and width classes from its
    // wght and wdth axes' defaults; without the axis, it needs to be told
    let has_axis = |tag: &[u8; 4]| font.axes.iter().any(|axis| axis.tag == Tag::new(tag));
    if !has_axis(b"wght") {
        ot.os2_us_weight_class = Some(os2.us_weight_class());
    }
    if !has_axis(b"wdth") {
        ot.os2_us_width_class = Some(os2.us_width_class());
    }
    // a symbol font's Windows character map is symbol-encoded, not Unicode (see
    // `fontir::symbol_cmap::KEY_SYMBOL_ENCODING`)
    let windows_encodings: Vec<u16> = fontref
        .cmap()
        .map(|cmap| {
            cmap.encoding_records()
                .iter()
                .filter(|record| {
                    record.platform_id() == skrifa::raw::tables::cmap::PlatformId::Windows
                })
                .map(|record| record.encoding_id())
                .collect()
        })
        .unwrap_or_default();
    if windows_encodings.contains(&0) && !windows_encodings.iter().any(|e| [1, 10].contains(e)) {
        font.format_specific.insert(
            "com.counterpunch.cmap.symbolEncoding".into(),
            serde_json::Value::Bool(true),
        );
    }
    Ok(())
}

/// A static font's style as an application reads it: the typographic
/// subfamily, else the subfamily.
fn style_name(fontref: &skrifa::FontRef) -> crate::I18NDictionary {
    for name_id in [
        StringId::TYPOGRAPHIC_SUBFAMILY_NAME,
        StringId::SUBFAMILY_NAME,
    ] {
        let name = name_id_to_i18n(fontref, name_id);
        if name.get_default().is_some() {
            return name;
        }
    }
    "Regular".into()
}

fn load_instances(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    if font.axes.is_empty() {
        // A static font is the one instance of its one master
        font.instances.push(Instance {
            id: "0".to_string(),
            name: style_name(fontref),
            ..Default::default()
        });
        return Ok(());
    }
    for (id, instance) in fontref.named_instances().iter().enumerate() {
        let name = name_id_to_i18n(fontref, instance.subfamily_name_id());
        // fvar states an instance in user coordinates, as 16.16 fixed; avar and
        // avar2 move where it is drawn, not where it is
        let location: Vec<(crate::Tag, DesignCoord)> = font
            .axes
            .iter()
            .zip(instance.user_coords())
            .flat_map(|(axis, user)| {
                let user = shortest(user as f64, skrifa::raw::types::Fixed::from_f64);
                let design_coord = UserCoord::new(user).to_design(&axis._converter().ok()?);
                Some((axis.tag, design_coord))
            })
            .collect();
        let mut custom_names = crate::Names::default();
        if let Some(id) = instance.postscript_name_id() {
            custom_names.postscript_name = name_id_to_i18n(fontref, id);
        }
        font.instances.push(Instance {
            id: id.to_string(),
            name,
            location: location.into(),
            custom_names,
            ..Default::default()
        })
    }
    Ok(())
}

/// head.created, counted in seconds from 1904
fn font_date(fontref: &skrifa::FontRef) -> Result<chrono::DateTime<chrono::Utc>, BabelfontError> {
    const SECONDS_1904_TO_1970: i64 = 2_082_844_800;
    let seconds = fontref.head()?.created().as_secs() - SECONDS_1904_TO_1970;
    Ok(chrono::DateTime::from_timestamp(seconds, 0).unwrap_or_default())
}

/// cmap format 14's Unicode variation sequences, by selector and codepoint; a
/// sequence that uses the default glyph names the glyph the codepoint maps to
fn load_variation_sequences(fontref: &skrifa::FontRef, font: &mut Font) {
    let charmap = fontref.charmap();
    let name = |gid: GlyphId| {
        font.glyphs
            .get_by_index(gid.to_u32() as usize)
            .map(|g| g.name.clone())
    };
    let sequences: Vec<((u32, u32), SmolStr)> = charmap
        .variant_mappings()
        .filter_map(|(codepoint, selector, variant)| {
            let gid = match variant {
                skrifa::raw::tables::cmap::MapVariant::UseDefault => charmap.map(codepoint)?,
                skrifa::raw::tables::cmap::MapVariant::Variant(gid) => gid,
            };
            Some(((selector, codepoint), name(gid)?))
        })
        .collect();
    font.variation_sequences.extend(sequences);
}

/// The peaks the font's masters stand at. A peak of gvar tuples is a master's
/// when most glyphs that vary have one there; a peak only a few glyphs have is
/// theirs alone (an intermediate layer), even where fontTools shared the tuple.
/// A peak of the GDEF variation store (anchors and kerning) that moves anything
/// is a master's too: kerning has a place only at masters.
fn master_peaks(fontref: &skrifa::FontRef) -> Result<Vec<Vec<F2Dot14>>, BabelfontError> {
    let mut peaks: Vec<Vec<F2Dot14>> = vec![];
    if let Ok(gvar) = fontref.gvar() {
        let mut counts: HashMap<Vec<F2Dot14>, usize> = HashMap::new();
        let mut varying = 0;
        for gid in 0..fontref.maxp()?.num_glyphs() {
            let Some(data) = gvar.glyph_variation_data(GlyphId::new(gid as u32))? else {
                continue;
            };
            varying += 1;
            let glyph_peaks: HashSet<Vec<F2Dot14>> = data
                .tuples()
                .map(|tuple| tuple.peak().values().iter().map(|v| v.get()).collect())
                .collect();
            for peak in glyph_peaks {
                *counts.entry(peak).or_default() += 1;
            }
        }
        let mut glyph_peaks: Vec<Vec<F2Dot14>> = counts
            .into_iter()
            .filter(|(_, count)| count * 2 > varying)
            .map(|(peak, _)| peak)
            .collect();
        glyph_peaks.sort();
        peaks.extend(glyph_peaks);
    }
    if let Some(store) = fontref
        .gdef()
        .ok()
        .and_then(|gdef| gdef.item_var_store())
        .transpose()?
    {
        let regions = store.variation_region_list()?.variation_regions();
        for data in store.item_variation_data().iter().flatten() {
            let Ok(data) = data else { continue };
            for (column, region_index) in data.region_indexes().iter().enumerate() {
                let moves = (0..data.item_count())
                    .any(|item| data.delta_set(item).nth(column).is_some_and(|d| d != 0));
                let Ok(region) = regions.get(region_index.get() as usize) else {
                    continue;
                };
                let peak: Vec<F2Dot14> = region
                    .region_axes()
                    .iter()
                    .map(|axis| axis.peak_coord())
                    .collect();
                if moves && !peaks.contains(&peak) {
                    peaks.push(peak);
                }
            }
        }
    }
    Ok(peaks)
}

fn load_masters(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    let mut master_locations: HashSet<DesignLocation> = HashSet::new();
    for peak in master_peaks(fontref)? {
        let loc = peak
            .iter()
            .zip(font.axes.iter())
            .flat_map(|(coord, axis)| Some((axis.tag, design_at(*coord, &axis._converter().ok()?))))
            .collect::<Vec<(Tag, DesignCoord)>>();
        master_locations.insert(DesignLocation::from(loc));
    }
    // in a stable order, as the axes run
    let mut master_locations: Vec<DesignLocation> = master_locations.into_iter().collect();
    master_locations.sort_by(|a, b| {
        let key = |loc: &DesignLocation| {
            font.axes
                .iter()
                .map(|axis| OrderedFloat(loc.get(axis.tag).map_or(0.0, |c| c.to_f64())))
                .collect::<Vec<_>>()
        };
        key(a).cmp(&key(b))
    });
    for loc in master_locations {
        let name = instance_name_at(font, &loc).unwrap_or_else(|| {
            loc.iter()
                .map(|(tag, coord)| format!("{}={}", tag, coord.to_f64()))
                .collect::<Vec<String>>()
                .join(", ")
                .into()
        });

        let metrics = load_metrics(fontref, font, &loc)?;
        // More here, check use_typo_metrics
        font.masters.push(crate::Master {
            name,
            id: uuid::Uuid::new_v4().to_string(),
            location: loc,
            metrics,
            ..Default::default()
        });
    }
    // Those were the variations (or there were none), add a default one
    let metrics = load_metrics(fontref, font, &fontdrasil::coords::Location::default())?;
    let mut default_location = DesignLocation::new();
    for axis in &font.axes {
        default_location.insert(
            axis.tag,
            axis.default
                .unwrap_or(UserCoord::new(0.0))
                .to_design(&axis._converter()?),
        );
    }

    font.masters.push(crate::Master {
        name: if font.axes.is_empty() {
            style_name(fontref)
        } else {
            instance_name_at(font, &default_location).unwrap_or_else(|| "Default".into())
        },
        id: uuid::Uuid::new_v4().to_string(),
        location: default_location,
        metrics,
        ..Default::default()
    });
    Ok(())
}

/// The name of the named instance at a location, if there is one
fn instance_name_at(font: &Font, location: &DesignLocation) -> Option<crate::I18NDictionary> {
    font.instances
        .iter()
        .find(|instance| {
            font.axes.iter().all(|axis| {
                let at = |loc: &DesignLocation| loc.get(axis.tag).map(|c| c.to_f64());
                match (at(&instance.location), at(location)) {
                    (Some(a), Some(b)) => (a - b).abs() < 0.01,
                    _ => false,
                }
            })
        })
        .map(|instance| instance.name.clone())
}

/// The peaks of a glyph's own gvar tuples that no master stands at: the glyph's
/// intermediate masters
fn intermediate_locations(
    fontref: &skrifa::FontRef,
    axes: &[crate::Axis],
    // the masters' normalized coordinates
    masters: &[Vec<F2Dot14>],
    gid: GlyphId,
) -> Result<Vec<DesignLocation>, BabelfontError> {
    let Ok(gvar) = fontref.gvar() else {
        return Ok(vec![]);
    };
    let Some(data) = gvar.glyph_variation_data(gid)? else {
        return Ok(vec![]);
    };
    let at_master = |coords: &[F2Dot14]| masters.iter().any(|master| master == coords);
    let mut seen: Vec<Vec<F2Dot14>> = vec![];
    let mut locations = vec![];
    for tuple in data.tuples() {
        let peak: Vec<F2Dot14> = tuple.peak().values().iter().map(|v| v.get()).collect();
        if at_master(&peak) || seen.contains(&peak) {
            continue;
        }
        seen.push(peak.clone());
        let mut location = DesignLocation::new();
        for (axis, coord) in axes.iter().zip(&peak) {
            location.insert(
                axis.tag,
                NormalizedCoord::new(coord.to_f32() as f64).to_design(&axis._converter()?),
            );
        }
        locations.push(location);
    }
    Ok(locations)
}

fn load_metrics(
    fontref: &skrifa::FontRef<'_>,
    font: &mut Font,
    loc: &fontdrasil::coords::Location<fontdrasil::coords::DesignSpace>,
) -> Result<IndexMap<MetricType, i32>, BabelfontError> {
    let coords = fontdrasil_location_to_skrifa_location(loc.clone(), &font.axes)?;
    let location = LocationRef::new(&coords);
    let skrifa_metrics = fontref.metrics(Size::unscaled(), location);
    let mut metrics = IndexMap::new();
    metrics.insert(MetricType::Ascender, skrifa_metrics.ascent as i32);
    metrics.insert(MetricType::Descender, skrifa_metrics.descent as i32);
    // OS/2 before version 2 has no sxHeight or sCapHeight; the specification
    // defines them as the tops of U+0078 and U+0048
    let glyph_metrics = fontref.glyph_metrics(Size::unscaled(), location);
    let outline_top = |c: char| {
        fontref
            .charmap()
            .map(c)
            .and_then(|gid| glyph_metrics.bounds(gid))
            .map(|bounds| bounds.y_max)
    };
    if let Some(x_height) = skrifa_metrics.x_height.or_else(|| outline_top('x')) {
        metrics.insert(MetricType::XHeight, x_height as i32);
    }
    if let Some(cap_height) = skrifa_metrics.cap_height.or_else(|| outline_top('H')) {
        metrics.insert(MetricType::CapHeight, cap_height as i32);
    }
    if let Some(strikeout) = skrifa_metrics.strikeout {
        metrics.insert(MetricType::StrikeoutPosition, strikeout.offset as i32);
        metrics.insert(MetricType::StrikeoutSize, strikeout.thickness as i32);
    }
    if let Some(underline) = skrifa_metrics.underline {
        metrics.insert(MetricType::UnderlinePosition, underline.offset as i32);
        metrics.insert(MetricType::UnderlineThickness, underline.thickness as i32);
    }
    // Some things skrifa doesn't know yet; MVAR varies them
    let mvar = fontref.mvar().ok();
    let mvar_delta = |tag: &[u8; 4]| -> f64 {
        mvar.as_ref()
            .and_then(|mvar| mvar.metric_delta(Tag::new(tag), &coords).ok())
            .map_or(0.0, |delta| delta.to_f64())
    };
    // (without OS/2, a compiler's defaults)
    let os2 = fontref.os2().ok();
    macro_rules! copy_os2_metric {
        ($metric_type:ident, $method:ident, $tag:literal) => {
            if let Some(os2) = &os2 {
                metrics.insert(
                    MetricType::$metric_type,
                    (os2.$method() as f64 + mvar_delta($tag)).round() as i32,
                );
            }
        };
    }
    copy_os2_metric!(SuperscriptXSize, y_superscript_x_size, b"spxs");
    copy_os2_metric!(SuperscriptYSize, y_superscript_y_size, b"spys");
    copy_os2_metric!(SuperscriptXOffset, y_superscript_x_offset, b"spxo");
    copy_os2_metric!(SuperscriptYOffset, y_superscript_y_offset, b"spyo");
    copy_os2_metric!(SubscriptXSize, y_subscript_x_size, b"sbxs");
    copy_os2_metric!(SubscriptYSize, y_subscript_y_size, b"sbys");
    copy_os2_metric!(SubscriptXOffset, y_subscript_x_offset, b"sbxo");
    copy_os2_metric!(SubscriptYOffset, y_subscript_y_offset, b"sbyo");
    copy_os2_metric!(WinAscent, us_win_ascent, b"hcla");
    copy_os2_metric!(WinDescent, us_win_descent, b"hcld");
    copy_os2_metric!(TypoLineGap, s_typo_line_gap, b"hlgp");
    copy_os2_metric!(TypoAscender, s_typo_ascender, b"hasc");
    copy_os2_metric!(TypoDescender, s_typo_descender, b"hdsc");

    metrics.insert(
        MetricType::HheaAscender,
        fontref.hhea()?.ascender().to_i16() as i32,
    );
    metrics.insert(
        MetricType::HheaDescender,
        fontref.hhea()?.descender().to_i16() as i32,
    );
    metrics.insert(
        MetricType::HheaLineGap,
        fontref.hhea()?.line_gap().to_i16() as i32,
    );
    // the caret's slant and offset, which MVAR varies
    let hhea = fontref.hhea()?;
    for (metric, value, tag) in [
        (
            MetricType::HheaCaretSlopeRise,
            hhea.caret_slope_rise(),
            b"hcrs",
        ),
        (
            MetricType::HheaCaretSlopeRun,
            hhea.caret_slope_run(),
            b"hcrn",
        ),
        (MetricType::HheaCaretOffset, hhea.caret_offset(), b"hcof"),
    ] {
        metrics.insert(metric, (value as f64 + mvar_delta(tag)).round() as i32);
    }
    if let Ok(vhea) = fontref.vhea() {
        let vertical = |value: i16, tag: &[u8; 4]| (value as f64 + mvar_delta(tag)).round() as i32;
        metrics.insert(
            MetricType::VheaAscender,
            vertical(vhea.ascender().to_i16(), b"vasc"),
        );
        metrics.insert(
            MetricType::VheaDescender,
            vertical(vhea.descender().to_i16(), b"vdsc"),
        );
        metrics.insert(
            MetricType::VheaLineGap,
            vertical(vhea.line_gap().to_i16(), b"vlgp"),
        );
        for (metric, value, tag) in [
            (
                MetricType::VheaCaretSlopeRise,
                vhea.caret_slope_rise(),
                b"vcrs",
            ),
            (
                MetricType::VheaCaretSlopeRun,
                vhea.caret_slope_run(),
                b"vcrn",
            ),
            (MetricType::VheaCaretOffset, vhea.caret_offset(), b"vcof"),
        ] {
            metrics.insert(metric, vertical(value, tag));
        }
    }

    // post stores the angle negative for a right lean; babelfont stores the
    // Glyphs convention, the opposite sign.
    metrics.insert(
        MetricType::ItalicAngle,
        -fontref
            .post()
            .map_or(0.0, |post| post.italic_angle().to_f64().round()) as i32,
    );
    Ok(metrics)
}

fn load_glyphs(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    // the names the uncompiled features use
    let names = sr_aef::unique_glyph_names(fontref);
    for name in &names {
        let mut glyph = Glyph::new(name.as_str());
        glyph.exported = true;
        font.glyphs.push(glyph);
    }

    // Now do it again and load layers
    let default_master_id = font.masters.last().map(|m| m.id.clone());
    let master_coords = font
        .masters
        .iter()
        .map(|m| fontdrasil_location_to_skrifa_location(m.location.clone(), &font.axes))
        .collect::<Result<Vec<_>, _>>()?;
    for (gid, glyph) in font.glyphs.iter_mut().enumerate() {
        let gid = GlyphId::new(gid as u32);
        // One glyph that cannot be read is left empty, rather than failing
        // the font
        let read_layer = |location: &DesignLocation, layer_type: crate::LayerType| {
            load_layer(
                &font.axes,
                fontref,
                gid,
                location,
                layer_type.clone(),
                &names,
            )
            .unwrap_or_else(|e| {
                log::warn!(
                    "Glyph {} cannot be read, so it is left empty: {e}",
                    glyph.name
                );
                unread_layer(&font.axes, fontref, gid, location, layer_type)
            })
        };
        for master in &font.masters {
            let layer = read_layer(
                &master.location,
                crate::LayerType::DefaultForMaster(master.id.clone()),
            );
            glyph.layers.push(layer);
        }
        let Some(default_master_id) = &default_master_id else {
            continue;
        };
        let intermediates = intermediate_locations(fontref, &font.axes, &master_coords, gid)
            .unwrap_or_else(|e| {
                log::warn!("Glyph {}'s variations cannot be read: {e}", glyph.name);
                vec![]
            });
        for location in intermediates {
            let mut layer = read_layer(
                &location,
                crate::LayerType::AssociatedWithMaster(default_master_id.clone()),
            );
            layer.name = Some(format!(
                "{{{}}}",
                location.iter().map(|(_, c)| c.to_f64()).join(", ")
            ));
            layer.id = Some(uuid::Uuid::new_v4().to_string());
            layer.location = Some(location);
            glyph.layers.push(layer);
        }
    }
    //cmap
    for (unicode, glyphid) in fontref.charmap().mappings() {
        let Some(glyph) = font.glyphs.get_by_index_mut(glyphid.to_u32() as usize) else {
            log::warn!(
                "The cmap maps U+{unicode:04X} to glyph ID {}, which the font does not have; left out",
                glyphid.to_u32()
            );
            continue;
        };
        glyph.codepoints.push(unicode);
    }
    load_categories(fontref, font)
}

/// A layer for a glyph that cannot be read: its advance and nothing else
fn unread_layer(
    axes: &[crate::Axis],
    fontref: &skrifa::FontRef<'_>,
    gid: skrifa::GlyphId,
    location: &DesignLocation,
    layer_type: crate::LayerType,
) -> Layer {
    let coords = fontdrasil_location_to_skrifa_location(location.clone(), axes).unwrap_or_default();
    let width = fontref
        .glyph_metrics(Size::unscaled(), LocationRef::new(&coords))
        .advance_width(gid)
        .unwrap_or_default();
    let mut layer = Layer::new(width);
    layer.master = layer_type;
    layer
}

/// Each glyph's category from GDEF's glyph classes, so that a compiler does not
/// guess one from its name; a mark with an advance is Spacing Combining.
/// Without glyph classes a shaper takes a glyph for a mark by its character
/// (and a glyph substituted for it keeps that), while a compiler writes glyph
/// classes from the categories: so a glyph a shaper takes for a mark is one,
/// and so is a glyph substitutions reach from marks only.
fn load_categories(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    let class_def = match fontref.gdef() {
        Ok(gdef) => gdef.glyph_class_def().transpose()?,
        Err(_) => None,
    };
    let shaper_marks: HashSet<usize> = if class_def.is_none() {
        use skrifa::raw::collections::IntSet;
        let mut marks: IntSet<GlyphId> = IntSet::empty();
        let mut bases: IntSet<GlyphId> = IntSet::empty();
        for (gid, glyph) in font.glyphs.iter().enumerate() {
            if glyph.codepoints.is_empty() {
                continue;
            }
            let gid = GlyphId::new(gid as u32);
            if shaper_mark(glyph) {
                marks.insert(gid);
            } else {
                bases.insert(gid);
            }
        }
        if let Ok(gsub) = fontref.gsub() {
            if let Ok(lookups) = gsub.collect_lookups(&IntSet::all()) {
                let _ = gsub.closure_glyphs(&lookups, &mut marks);
                let _ = gsub.closure_glyphs(&lookups, &mut bases);
            }
        }
        marks
            .iter()
            .filter(|g| !bases.contains(*g))
            .map(|g| g.to_u32() as usize)
            .collect()
    } else {
        HashSet::new()
    };
    let metrics = fontref.glyph_metrics(Size::unscaled(), LocationRef::default());
    for (gid, glyph) in font.glyphs.iter_mut().enumerate() {
        let gid = GlyphId::new(gid as u32);
        let class = match &class_def {
            Some(class_def) => class_def.get(gid),
            None if shaper_marks.contains(&(gid.to_u32() as usize)) => 3,
            None => 1,
        };
        glyph.category = match class {
            2 => GlyphCategory::Ligature,
            3 => GlyphCategory::Mark,
            _ => GlyphCategory::Base,
        };
        let subcategory = match class {
            2 => Some("Ligature"),
            3 if metrics.advance_width(gid).unwrap_or_default() == 0.0 => Some("Nonspacing"),
            3 => Some("Spacing Combining"),
            _ => None,
        };
        if let Some(subcategory) = subcategory {
            glyph.format_specific.insert(
                "subcategory".to_string(),
                serde_json::Value::String(subcategory.to_string()),
            );
        }
    }
    Ok(())
}

/// Whether a shaper without glyph classes takes a glyph for a mark: when one of
/// its codepoints is a nonspacing mark and not default-ignorable.
fn shaper_mark(glyph: &Glyph) -> bool {
    let general_category = CodePointMapData::<GeneralCategory>::new();
    let default_ignorable = CodePointSetData::new::<DefaultIgnorableCodePoint>();
    glyph
        .codepoints
        .iter()
        .filter_map(|&codepoint| char::from_u32(codepoint))
        .any(|c| {
            general_category.get(c) == GeneralCategory::NonspacingMark
                && !default_ignorable.contains(c)
        })
}

// This is only sound for mark class definitions
/// The anchor names the decompiler made up are returned
fn load_features(
    fontref: &skrifa::FontRef,
    font: &mut Font,
) -> Result<HashSet<String>, BabelfontError> {
    let axes = font.fontdrasil_axes()?;
    // the scripts a compiler kerns the font's characters in, and so adds
    // script records for (fontc's guess_font_scripts)
    let codepoints: Vec<u32> = fontref.charmap().mappings().map(|(cp, _)| cp).collect();
    let generated_scripts: std::collections::HashSet<skrifa::Tag> =
        crate::filters::layout_model::unicode::font_scripts(&[codepoints])
            .iter()
            .flat_map(|script| crate::filters::layout_model::fontc_script_tags(script))
            .filter_map(|tag| skrifa::Tag::new_checked(format!("{tag:<4}").as_bytes()).ok())
            .collect();
    let mut uncompile_context = sr_aef::uncompile_context_with_scripts(fontref, &generated_scripts)
        .map_err(|e| BabelfontError::BinaryFontRead(e.to_string()))?;
    let provisional: HashSet<String> = uncompile_context
        .provisional_anchor_names
        .iter()
        .map(|name| name.to_string())
        .collect();
    // ligature carets and attachment points; glyph classes are categories
    let gdef = uncompile_context
        .gdef_without_glyph_classes()
        .map_err(|e| BabelfontError::BinaryFontRead(e.to_string()))?;
    let master_ids = font
        .masters
        .iter()
        .map(|m| (m.id.clone(), m))
        .collect::<HashMap<_, _>>();
    let mut features = Features::default();
    let gdef = load_ligature_carets(&mut font.glyphs, gdef, &master_ids, &axes)?;
    for (anchor_name, anchors) in uncompile_context.anchors {
        for (glyph_name, anchor) in anchors {
            if let Some(glyph) = font.glyphs.get_mut(&glyph_name) {
                for layer in glyph.layers.iter_mut() {
                    let Some(location) = layer_location(layer, &master_ids) else {
                        continue;
                    };
                    let (x, y) = get_x_y_location_for_anchor(location, &anchor, &axes)?;
                    layer.anchors.push(Anchor {
                        id: None,
                        name: anchor_name.clone().into(),
                        x,
                        y,
                        format_specific: FormatSpecific::default(),
                    });
                }
            } else {
                log::warn!(
                    "Glyph {} from anchor {} does not exist in the font",
                    glyph_name,
                    anchor_name
                );
            }
        }
    }

    // Do the same but for mark classes.
    // The lookups name the mark classes, so they are defined too
    let mark_class_definitions = uncompile_context
        .mark_classes
        .values()
        .flatten()
        .map(|definition| definition.as_fea(""))
        .join("\n");
    for (anchor_name, classdefs) in uncompile_context.mark_classes {
        let anchor_name = format!("_{}", anchor_name);
        for definition in classdefs.iter() {
            let anchor = &definition.anchor;
            let glyphs = &definition.glyphs;
            let glyphset = expand_glyph_container(font, glyphs, &HashMap::new(), None);
            for glyph_name in glyphset {
                if let Some(glyph) = font.glyphs.get_mut(&glyph_name) {
                    for layer in glyph.layers.iter_mut() {
                        let Some(location) = layer_location(layer, &master_ids) else {
                            continue;
                        };
                        let (x, y) = get_x_y_location_for_anchor(location, anchor, &axes)?;
                        layer.anchors.push(Anchor {
                            id: None,
                            name: anchor_name.clone(),
                            x,
                            y,
                            format_specific: FormatSpecific::default(),
                        });
                    }
                } else {
                    log::warn!(
                        "Glyph {} from anchor {} does not exist in the font",
                        glyph_name,
                        anchor_name
                    );
                }
            }
        }
    }

    for (class_name, glyphs) in uncompile_context.named_classes.iter() {
        // A class's code is its members; the writers add the brackets
        let members = glyphs.glyphs.iter().map(|g| g.as_fea("")).join(" ");
        features
            .classes
            .insert(class_name.clone(), PossiblyAutomaticCode::new(members));
    }
    let mut language_systems = vec![];
    for (script_tag, lang_sys_tags) in &uncompile_context.declared_language_systems {
        for lang_sys_tag in lang_sys_tags {
            let lss =
                LanguageSystemStatement::new(script_tag.to_string(), lang_sys_tag.to_string());
            language_systems.push(lss.as_fea(""));
        }
    }
    // One prefix, in the order FEA needs: language systems, mark classes, then
    // lookups as they are defined (a lookup after the lookups it calls).
    // Separate prefixes may lose their order: a JSON object read into a sorted
    // map, for one.
    let lookups = uncompile_context
        .lookups
        .values()
        .map(|statements| statements.as_fea(""));
    // Feature variations: condition sets, then the lookups each feature
    // takes in their place (Outfit's rvrn swaps a glyph from wght 668 on)
    let feature_variations =
        uncompile_context
            .feature_variations
            .iter()
            .flat_map(|(condition_set, variations)| {
                std::iter::once(condition_set.as_fea(""))
                    .chain(variations.iter().map(|variation| variation.as_fea("")))
            });
    let code = std::iter::once(language_systems.join("\n"))
        .chain(std::iter::once(gdef))
        .chain(std::iter::once(mark_class_definitions))
        .chain(lookups)
        .chain(feature_variations)
        .filter(|block| !block.is_empty())
        .join("\n");
    if !code.is_empty() {
        features
            .prefixes
            .insert("Decompiled".into(), PossiblyAutomaticCode::new(code));
    }
    for (feature_name, lookups) in uncompile_context.features.iter() {
        features.features.push((
            feature_name.clone(),
            PossiblyAutomaticCode::new(lookups.iter().map(|l| l.as_fea("")).join("\n")),
        ));
    }
    font.features = features;
    Ok(provisional)
}

/// Ligature carets that vary become `caret_N` anchors, which a compiler
/// writes the carets from (a feature file states no varying caret); the
/// other GDEF statements, carets and attachment points, stay feature code.
fn load_ligature_carets(
    glyphs: &mut crate::GlyphList,
    statements: Vec<sr_aef::fea_rs_ast::GdefStatement>,
    master_ids: &HashMap<String, &crate::Master>,
    axes: &Axes,
) -> Result<String, BabelfontError> {
    use sr_aef::fea_rs_ast::{GdefStatement, GlyphContainer, Metric};
    let varies = statements.iter().any(|s| {
        matches!(s, GdefStatement::LigatureCaretByPos(c)
            if c.carets.iter().any(|m| matches!(m, Metric::Variable(_))))
    });
    let mut kept = vec![];
    for statement in statements {
        match statement {
            GdefStatement::LigatureCaretByPos(caret) if varies => {
                let GlyphContainer::GlyphName(name) = &caret.glyphs else {
                    continue;
                };
                let Some(glyph) = glyphs.get_mut(&name.name) else {
                    continue;
                };
                for layer in glyph.layers.iter_mut() {
                    let Some(location) = layer_location(layer, master_ids) else {
                        continue;
                    };
                    for (i, metric) in caret.carets.iter().enumerate() {
                        let x = crate::features::metric_at(metric, &location, axes)?;
                        layer.anchors.push(Anchor {
                            id: None,
                            name: format!("caret_{}", i + 1),
                            x,
                            y: 0.0,
                            format_specific: FormatSpecific::default(),
                        });
                    }
                }
            }
            GdefStatement::LigatureCaretByIndex(_) if varies => {
                // a compiler writes the carets from anchors only where the
                // feature code states none
                log::warn!("A ligature caret by contour point is lost beside varying carets");
            }
            other => kept.push(other.as_fea("")),
        }
    }
    if kept.is_empty() {
        return Ok(String::new());
    }
    Ok(format!("table GDEF {{\n{}\n}} GDEF;", kept.join("\n")))
}

/// A kern table's horizontal format 0 pairs as the default master's kerning,
/// read as a shaper applies them: only without a GPOS kern feature, summed
/// across subtables, and never across a mark.
fn load_legacy_kerning(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    let Ok(kern) = fontref.kern() else {
        return Ok(());
    };
    if let Ok(gpos) = fontref.gpos() {
        if gpos
            .feature_list()?
            .feature_records()
            .iter()
            .any(|record| record.feature_tag() == Tag::new(b"kern"))
        {
            return Ok(());
        }
    }
    let has_classes = fontref
        .gdef()
        .is_ok_and(|gdef| gdef.glyph_class_def().is_some());
    let is_mark = |glyph: &Glyph| {
        if has_classes {
            glyph.category == GlyphCategory::Mark
        } else {
            shaper_mark(glyph)
        }
    };
    let mut pairs: IndexMap<(SmolStr, SmolStr), i32> = IndexMap::new();
    for subtable in kern.subtables() {
        let subtable = subtable?;
        if !subtable.is_horizontal() || subtable.is_cross_stream() || subtable.is_variable() {
            continue;
        }
        let Ok(kern::SubtableKind::Format0(format0)) = subtable.kind() else {
            log::warn!("Only format 0 kern subtables are read");
            continue;
        };
        for pair in format0.pairs() {
            let left = font.glyphs.get_by_index(pair.left().to_u32() as usize);
            let right = font.glyphs.get_by_index(pair.right().to_u32() as usize);
            if let (Some(left), Some(right)) = (left, right) {
                if is_mark(left) || is_mark(right) {
                    continue;
                }
                *pairs
                    .entry((left.name.clone(), right.name.clone()))
                    .or_default() += pair.value() as i32;
            }
        }
    }
    // The default master is the last one pushed
    if let Some(master) = font.masters.last_mut() {
        for (pair, value) in pairs {
            master
                .kerning
                .insert(pair, value.clamp(i16::MIN as i32, i16::MAX as i32) as i16);
        }
    }
    Ok(())
}

/// Where a layer stands: its own location, else its master's
fn layer_location(
    layer: &Layer,
    masters: &HashMap<String, &crate::Master>,
) -> Option<DesignLocation> {
    if let Some(location) = &layer.location {
        return Some(location.clone());
    }
    match &layer.master {
        LayerType::DefaultForMaster(id) => masters.get(id).map(|m| m.location.clone()),
        _ => None,
    }
}

fn get_x_y_location_for_anchor(
    location: DesignLocation,
    anchor: &sr_aef::fea_rs_ast::Anchor,
    axes: &Axes,
) -> Result<(f64, f64), BabelfontError> {
    Ok((
        crate::features::metric_at(&anchor.x, &location, axes)?,
        crate::features::metric_at(&anchor.y, &location, axes)?,
    ))
}

fn fontdrasil_location_to_skrifa_location<Space: ConvertSpace<NormalizedSpace>>(
    loc: fontdrasil::coords::Location<Space>,
    axis_order: &[crate::Axis],
) -> Result<Vec<F2Dot14>, BabelfontError> {
    let mut coords = vec![];

    for axis in axis_order {
        let converter = axis._converter()?;
        let coord = loc
            .get(axis.tag)
            .map(|c| F2Dot14::from_f32(c.to_normalized(&converter).to_f64() as f32))
            .unwrap_or(F2Dot14::from_f32(0.0));
        coords.push(coord);
    }

    Ok(coords)
}

/// A glyph's vmtx advance, and its vertical origin: the top of its bounding box
/// (0 for an empty glyph) plus its top side bearing. At a location, VVAR varies
/// both; without VVAR, gvar's top and bottom phantom points do.
fn load_vertical_metrics(
    fontref: &skrifa::FontRef<'_>,
    gid: skrifa::GlyphId,
    coords: &[F2Dot14],
    layer: &mut Layer,
) -> Result<(), BabelfontError> {
    let Ok(vmtx) = fontref.vmtx() else {
        return Ok(());
    };
    let (Some(advance), Some(tsb)) = (vmtx.advance(gid), vmtx.side_bearing(gid)) else {
        return Ok(());
    };
    let glyf = fontref.glyf()?;
    let loca = fontref.loca(None)?;
    let y_max = loca.get_glyf(gid, &glyf)?.map_or(0, |glyph| glyph.y_max()) as f64;
    let (mut advance, mut origin) = (advance as f64, y_max + tsb as f64);
    if coords.iter().any(|c| *c != F2Dot14::ZERO) {
        if let Ok(vvar) = fontref.vvar() {
            advance += vvar
                .advance_height_delta(gid, coords)
                .map_or(0.0, |d| d.to_f64());
            let y_max = fontref
                .glyph_metrics(Size::unscaled(), LocationRef::new(coords))
                .bounds(gid)
                .map_or(y_max, |bounds| bounds.y_max as f64);
            origin = y_max + tsb as f64 + vvar.tsb_delta(gid, coords).map_or(0.0, |d| d.to_f64());
        } else if let Ok(gvar) = fontref.gvar() {
            if let Some([_, _, top, bottom]) = gvar
                .phantom_point_deltas(&glyf, &loca, coords, gid)
                .ok()
                .flatten()
            {
                advance += (top.y - bottom.y).to_f64();
                origin += top.y.to_f64();
            }
        }
    }
    layer.vertical_advance = Some(advance.round() as f32);
    layer.vertical_origin = Some(origin.round() as f32);
    Ok(())
}

/// How far a TrueType rasterizer shifts a glyph's stored outline when drawing
/// it: by lsb - xMin, or for a composite with a USE_MY_METRICS component, by that
/// component's shift. An empty glyph's xMin is 0. (At the default location, and
/// without component deltas.)
fn drawn_shift(
    fontref: &skrifa::FontRef<'_>,
    glyf: &glyf::Glyf<'_>,
    gid: skrifa::GlyphId,
    depth: usize,
) -> Result<f64, BabelfontError> {
    if depth > COMPOSITE_RECURSION_LIMIT {
        return Err(BabelfontError::BinaryFontRead(format!(
            "Glyph ID {} nests components too deeply",
            gid.to_u32()
        )));
    }
    let glyph = fontref.loca(None)?.get_glyf(gid, glyf)?;
    let x_min = glyph.as_ref().map_or(0, |glyph| glyph.x_min()) as f64;
    let lsb = fontref
        .hmtx()?
        .side_bearing(gid)
        .map_or(x_min, |lsb| lsb as f64);
    let mut shift = lsb - x_min;
    if let Some(glyf::Glyph::Composite(composite)) = glyph {
        for component in composite.components() {
            if component
                .flags
                .contains(glyf::CompositeGlyphFlags::USE_MY_METRICS)
            {
                shift = drawn_shift(fontref, glyf, component.glyph.into(), depth + 1)?;
            }
        }
    }
    Ok(shift)
}

/// The shift a component's outline was read with (an empty one has none)
fn read_shift(
    fontref: &skrifa::FontRef<'_>,
    glyf: &glyf::Glyf<'_>,
    gid: skrifa::GlyphId,
) -> Result<f64, BabelfontError> {
    if fontref.loca(None)?.get_glyf(gid, glyf)?.is_none() {
        return Ok(0.0);
    }
    drawn_shift(fontref, glyf, gid, 0)
}

const COMPOSITE_RECURSION_LIMIT: usize = 32;

/// A pen that keeps each contour as skrifa draws it: its start, the move, is
/// its first node, as a line, and nothing is merged or dropped. skrifa draws a
/// glyf contour with the same calls wherever it is drawn (they follow the
/// point flags, which every master shares), so the nodes are the same in every
/// master, even where points coincide in some of them.
#[derive(Default)]
struct ContoursPen {
    contours: Vec<crate::Path>,
}

impl ContoursPen {
    fn push(&mut self, node: crate::Node) {
        if let Some(contour) = self.contours.last_mut() {
            contour.nodes.push(node);
        }
    }
}

impl skrifa::outline::OutlinePen for ContoursPen {
    fn move_to(&mut self, x: f32, y: f32) {
        self.contours.push(crate::Path {
            nodes: vec![crate::Node::new_line(x, y)],
            closed: true,
            ..Default::default()
        });
    }
    fn line_to(&mut self, x: f32, y: f32) {
        self.push(crate::Node::new_line(x, y));
    }
    fn quad_to(&mut self, cx0: f32, cy0: f32, x: f32, y: f32) {
        self.push(crate::Node::new_offcurve(cx0, cy0));
        self.push(crate::Node::new_qcurve(x, y));
    }
    fn curve_to(&mut self, cx0: f32, cy0: f32, cx1: f32, cy1: f32, x: f32, y: f32) {
        self.push(crate::Node::new_offcurve(cx0, cy0));
        self.push(crate::Node::new_offcurve(cx1, cy1));
        self.push(crate::Node::new_curve(x, y));
    }
    fn close(&mut self) {}
}

/// A glyph's contours as skrifa draws them, in glyf order and direction, each
/// starting with the point it starts at (see [`ContoursPen`])
fn draw_contours(
    fontref: &skrifa::FontRef<'_>,
    gid: skrifa::GlyphId,
    location: LocationRef,
) -> Result<Vec<crate::Path>, BabelfontError> {
    let Some(outline) = fontref.outline_glyphs().get(gid) else {
        return Ok(vec![]);
    };
    let mut pen = ContoursPen::default();
    outline
        .draw(DrawSettings::unhinted(Size::unscaled(), location), &mut pen)
        .map_err(|e| BabelfontError::BinaryFontRead(e.to_string()))?;
    Ok(pen.contours)
}

/// For each contour of a simple glyph, whether skrifa draws a segment back to
/// the point the contour starts at, so that the start is drawn twice: as the
/// move and as that segment's end. It does when the last point it reaches
/// before closing is an off-curve: the contour's last point, or the one before
/// it when a contour starting with an off-curve starts at its last point. This
/// follows the flags alone, never the coordinates, so it is the same in every
/// master.
fn closes_with_a_segment(simple: &glyf::SimpleGlyph) -> Vec<bool> {
    let on_curve: Vec<bool> = simple.points().map(|p| p.on_curve).collect();
    let mut start = 0;
    simple
        .end_pts_of_contours()
        .iter()
        .map(|end| {
            let end = end.get() as usize;
            let contour = on_curve.get(start..=end).unwrap_or_default();
            start = end + 1;
            let reached_last = match contour {
                [false, .., before_last, true] => *before_last,
                [.., last] => *last,
                [] => true,
            };
            !reached_last
        })
        .collect()
}

/// Reverses a contour drawn from a glyf, keeping the node at `start` (the
/// point the glyf contour starts at) as its start, the last node, as a
/// compiler that reverses it back keeps it. The start is found by its index,
/// never by its coordinates, which other points may share in some masters.
fn reverse_keeping_start(path: &mut crate::Path, start: usize) {
    if start < path.nodes.len() {
        path.nodes.rotate_left(start);
    }
    // the start, an on-curve, is now first; reversed, it is last, and
    // reversing leaves it there since no off-curves trail it
    path.reverse();
}

/// A coordinate skrifa computed in f32, rounded to the integer it stands for
fn snap(value: f64) -> f64 {
    if (value - value.round()).abs() < 1e-3 {
        value.round()
    } else {
        value
    }
}

fn load_layer(
    axes: &[crate::Axis],
    fontref: &skrifa::FontRef<'_>,
    gid: skrifa::GlyphId,
    location: &DesignLocation,
    layer_type: crate::LayerType,
    names: &[SmolStr],
) -> Result<crate::Layer, BabelfontError> {
    let loc = fontdrasil_location_to_skrifa_location(location.clone(), axes)?;
    let locationref = LocationRef::new(&loc);
    let width = fontref.glyph_metrics(Size::unscaled(), locationref);
    let mut layer = Layer::new(width.advance_width(gid).unwrap_or_default());
    layer.master = layer_type;
    load_vertical_metrics(fontref, gid, &loc, &mut layer)?;

    // Skrifa pens don't support components. Parse component glyphs manually.
    let glyf = fontref.glyf()?;
    let Some(glyph) = fontref.loca(None)?.get_glyf(gid, &glyf)? else {
        // We're done
        return Ok(layer);
    };

    match glyph {
        glyf::Glyph::Simple(simple) => {
            let closing = closes_with_a_segment(&simple);
            // glyf contours run clockwise, a source's counter-clockwise
            layer.shapes = draw_contours(fontref, gid, locationref)?
                .into_iter()
                .enumerate()
                .map(|(index, mut path)| {
                    // a start drawn twice is one point: keep the segment's end
                    let start =
                        if closing.get(index).copied().unwrap_or(false) && path.nodes.len() > 1 {
                            path.nodes.remove(0);
                            path.nodes.len() - 1
                        } else {
                            0
                        };
                    reverse_keeping_start(&mut path, start);
                    crate::Shape::Path(path)
                })
                .collect();
        }
        glyf::Glyph::Composite(c) => {
            // This gets complex. :-/
            //
            // Simple glyphs are read as drawn (see drawn_shift). A composite is
            // drawn from its components' stored outlines and then shifted, so
            // each offset gains the composite's shift and loses the one its
            // component was read with, transformed as the component is.
            //
            // A composite that cannot be drawn (a component cycle, components
            // nested too deeply, a broken component) keeps its components at
            // their stored offsets.
            let shift = drawn_shift(fontref, &glyf, gid, 0).unwrap_or_else(|e| {
                log::warn!(
                    "Glyph ID {}, its components keep their stored offsets: {e}",
                    gid.to_u32()
                );
                0.0
            });
            // Where skrifa draws each component's contours, with offset deltas
            // and point matching applied
            let drawn = draw_contours(fontref, gid, locationref).unwrap_or_else(|e| {
                log::warn!(
                    "Glyph ID {} cannot be drawn, its components keep their stored offsets: {e}",
                    gid.to_u32()
                );
                vec![]
            });
            let mut contour_index = 0;
            for component in c.components() {
                let gid = component.glyph;
                let own = if drawn.is_empty() {
                    vec![]
                } else {
                    draw_contours(fontref, gid.into(), locationref).unwrap_or_default()
                };
                let placed = drawn.get(contour_index);
                contour_index += own.len();
                let anchor = match component.anchor {
                    glyf::Anchor::Offset { x, y } => (x as f64, y as f64),
                    glyf::Anchor::Point {
                        base: _,
                        component: _,
                    } => (0.0, 0.0),
                };
                // x' = xx * x + xy * y, y' = yx * x + yy * y
                let xx = component.transform.xx.to_f32() as f64;
                let yx = component.transform.yx.to_f32() as f64;
                let xy = component.transform.xy.to_f32() as f64;
                let yy = component.transform.yy.to_f32() as f64;
                let (dx, dy) = match (
                    own.first().and_then(|p| p.nodes.first()),
                    placed.and_then(|p| p.nodes.first()),
                ) {
                    // the offset that puts the component's first point where
                    // it is drawn
                    (Some(p), Some(q)) => (
                        snap(q.x - (xx * p.x + xy * p.y)),
                        snap(q.y - (yx * p.x + yy * p.y)),
                    ),
                    // an empty component: its stored offset
                    _ => {
                        let component_shift =
                            read_shift(fontref, &glyf, gid.into()).unwrap_or_default();
                        (
                            anchor.0 + shift - xx * component_shift,
                            anchor.1 - yx * component_shift,
                        )
                    }
                };
                // kurbo::Affine::new expects [xx, yx, xy, yy, dx, dy]
                let our_affine = kurbo::Affine::new([xx, yx, xy, yy, dx, dy]);

                layer.shapes.push(crate::Shape::Component(crate::Component {
                    id: None,
                    reference: names
                        .get(gid.to_u32() as usize)
                        .ok_or(BabelfontError::BinaryFontRead(format!(
                            "Component glyph ID {} is not in the font",
                            gid.to_u32()
                        )))?
                        .clone(),
                    transform: our_affine.into(),
                    location: IndexMap::new(), // We don't have per-component locations in TTF, so leave this empty
                    format_specific: FormatSpecific::default(),
                }));
            }
        }
    }

    Ok(layer)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::indexing_slicing)]
mod tests {
    use super::*;
    use crate::PathBuilder;
    use kurbo::Shape as _;

    const STATIC_LEGACY: &str = "resources/ttf/StaticLegacy.ttf";
    const GDEF_CLASSES: &str = "resources/ttf/GdefClasses.ttf";
    const DUPLICATE_NAMES: &str = "resources/ttf/DuplicateNames.ttf";
    const MARK_NO_GDEF: &str = "resources/ttf/MarkNoGdef.ttf";
    const UNNAMED: &str = "resources/ttf/Unnamed.ttf";
    const QUADRATIC: &str = "resources/ttf/Quadratic.ttf";
    const VARIABLE: &str = "resources/ttf/Variable.ttf";
    const VARIABLE_NO_HVAR: &str = "resources/ttf/VariableNoHvar.ttf";
    const ANCHORS: &str = "resources/ttf/Anchors.ttf";
    const AVAR2: &str = "resources/ttf/Avar2.ttf";
    const AVAR2_NO_MAP: &str = "resources/ttf/Avar2NoMap.ttf";
    const METADATA: &str = "resources/ttf/Metadata.ttf";
    const RTL_KERNING: &str = "resources/ttf/RtlKerning.ttf";
    const COMPOSITES: &str = "resources/ttf/Composites.ttf";
    const REDDIT_SANS: &str = "resources/ttf/RedditSans-Subset.ttf";
    const COINCIDENT: &str = "resources/ttf/Coincident.ttf";
    const INTER: &str = "resources/ttf/Inter-Subset.ttf";
    const MALFORMED: &str = "resources/ttf/Malformed.ttf";
    const CFF: &str = "resources/ttf/Cff.otf";
    const SHAPING_VARIABLE: &str = "resources/ttf/ShapingVariable.ttf";
    const SHAPING_MKMK: &str = "resources/ttf/ShapingMkmk.ttf";

    fn master<'a>(font: &'a Font, name: &str) -> &'a crate::Master {
        font.masters
            .iter()
            .find(|m| m.name.get_default().map(String::as_str) == Some(name))
            .unwrap_or_else(|| panic!("no master {name}"))
    }

    fn layer<'a>(font: &'a Font, glyph: &str, master_name: &str) -> &'a Layer {
        let id = &master(font, master_name).id;
        font.glyphs
            .get(glyph)
            .unwrap()
            .layers
            .iter()
            .find(|l| l.master == LayerType::DefaultForMaster(id.clone()))
            .unwrap()
    }

    fn node_types(layer: &Layer) -> Vec<Vec<crate::NodeType>> {
        layer
            .paths()
            .map(|p| p.nodes.iter().map(|n| n.nodetype).collect())
            .collect()
    }

    /// The kerning a shaper applies between two glyphs at a master
    fn kern_value(font: &Font, master: &crate::Master, left: &str, right: &str) -> i16 {
        let group = |groups: &IndexMap<SmolStr, Vec<SmolStr>>, glyph: &str| {
            groups
                .iter()
                .find(|(_, members)| members.iter().any(|g| g == glyph))
                .map(|(name, _)| SmolStr::from(format!("@{name}")))
        };
        let lefts = [Some(left.into()), group(&font.first_kern_groups, left)];
        let rights = [Some(right.into()), group(&font.second_kern_groups, right)];
        for l in lefts.iter().flatten() {
            for r in rights.iter().flatten() {
                if let Some(value) = master.kerning.get(&(l.clone(), r.clone())) {
                    return *value;
                }
            }
        }
        0
    }

    fn assert_category(
        font: &Font,
        glyph: &str,
        category: GlyphCategory,
        subcategory: Option<&str>,
    ) {
        let glyph = font.glyphs.get(glyph).unwrap();
        assert_eq!(glyph.category, category, "{}", glyph.name);
        assert_eq!(
            glyph
                .format_specific
                .get_optionstring("subcategory")
                .as_deref(),
            subcategory,
            "{}",
            glyph.name
        );
    }

    fn component_coeffs(font: &Font, glyph: &str) -> Vec<[f64; 6]> {
        font.glyphs.get(glyph).unwrap().layers[0]
            .components()
            .map(|c| c.transform.as_affine().as_coeffs())
            .collect()
    }

    fn assert_close(a: &[f64], b: &[f64]) {
        assert!(
            a.iter().zip(b).all(|(x, y)| (x - y).abs() < 1e-6),
            "{a:?} != {b:?}"
        );
    }

    #[test]
    fn test_component_transform_order() {
        // barrot turns bar 90 degrees: x' = -y, y' = x
        let font = load(STATIC_LEGACY).unwrap();
        let coeffs = component_coeffs(&font, "barrot");
        assert_close(&coeffs[0][..4], &[0.0, 1.0, -1.0, 0.0]);
    }
    #[test]
    fn test_glyphs_read_as_drawn() {
        // every glyph, simple or composite, comes out where skrifa draws it
        let binary = std::fs::read(STATIC_LEGACY).unwrap();
        let fontref = skrifa::FontRef::new(&binary).unwrap();
        let font = load(STATIC_LEGACY).unwrap();
        for (gid, glyph) in font.glyphs.iter().enumerate() {
            let mut pen = PathBuilder::new();
            fontref
                .outline_glyphs()
                .get(GlyphId::new(gid as u32))
                .unwrap()
                .draw(
                    DrawSettings::unhinted(Size::unscaled(), LocationRef::default()),
                    &mut pen,
                )
                .unwrap();
            let drawn = pen
                .build()
                .iter()
                .map(|p| p.to_kurbo().unwrap().bounding_box())
                .reduce(|a, b| a.union(b))
                .unwrap_or_default();
            let layer = &glyph.layers[0];
            let mut paths: Vec<crate::Path> = layer.paths().cloned().collect();
            for component in layer.components() {
                paths.extend(component.decompose(None, |name, _| {
                    let layer = &font.glyphs.get(name)?.layers[0];
                    Some(crate::shape::ResolvedShapes {
                        paths: layer.paths().cloned().collect(),
                        components: layer.components().cloned().collect(),
                    })
                }));
            }
            let read = paths
                .iter()
                .map(|p| p.to_kurbo().unwrap().bounding_box())
                .reduce(|a, b| a.union(b))
                .unwrap_or_default();
            assert_close(
                &[read.x0, read.y0, read.x1, read.y1],
                &[drawn.x0, drawn.y0, drawn.x1, drawn.y1],
            );
        }
        assert_close(&component_coeffs(&font, "barflip")[0][4..], &[310.0, 600.0]);
        let bars = component_coeffs(&font, "bars");
        assert_close(&[bars[0][4], bars[1][4]], &[-5.0, 245.0]);
    }
    #[test]
    fn test_contours_read_counter_clockwise() {
        // x is a clockwise rectangle in the binary
        let font = load(STATIC_LEGACY).unwrap();
        let path = font.glyphs.get("x").unwrap().layers[0]
            .paths()
            .next()
            .unwrap();
        let n = path.nodes.len();
        let area: f64 = (0..n)
            .map(|i| {
                let (a, b) = (&path.nodes[i], &path.nodes[(i + 1) % n]);
                a.x * b.y - b.x * a.y
            })
            .sum();
        assert!(area > 0.0, "{:?}", path.nodes);
    }
    #[test]
    fn test_static_font_master_and_instance() {
        // named after the style: ID 17, not ID 2
        let font = load(STATIC_LEGACY).unwrap();
        assert_eq!(font.masters.len(), 1);
        assert_eq!(font.masters[0].name.get_default().unwrap(), "Medium");
        assert_eq!(font.instances.len(), 1);
        assert_eq!(font.instances[0].name.get_default().unwrap(), "Medium");
    }
    #[test]
    fn test_os2_values() {
        let ot = load(STATIC_LEGACY).unwrap().custom_ot_values;
        assert_eq!(ot.os2_us_weight_class, Some(500));
        assert_eq!(ot.os2_us_width_class, Some(6));
        assert_eq!(ot.os2_fs_type, Some(0));
        assert_eq!(ot.os2_panose, Some([2, 0, 6, 3, 0, 0, 0, 0, 0, 0]));
        // REGULAR | USE_TYPO_METRICS
        assert_eq!(ot.os2_fs_selection, Some(1 << 6 | 1 << 7));
        assert_eq!(ot.head_lowest_rec_ppem, Some(9));
    }

    #[test]
    fn test_metadata() {
        let font = load(METADATA).unwrap();
        let ot = &font.custom_ot_values;
        assert_eq!(ot.os2_us_weight_class, Some(700));
        assert_eq!(ot.os2_us_width_class, Some(3));
        assert_eq!(ot.os2_fs_type, Some(8));
        assert_eq!(ot.os2_family_class, Some(0x0805));
        assert_eq!(ot.os2_fs_selection, Some(1 << 5 | 1 << 7 | 1 << 8));
        assert_eq!(ot.os2_unicode_range2, Some(1 << 28));
        assert_eq!(ot.os2_code_page_range1, Some(1 << 31));
        assert_eq!(ot.post_is_fixed_pitch, Some(true));
        // as the source said it, not as 16.16 stores it (-16.332993)
        assert_eq!(ot.post_italic_angle, Some(-16.333));
        assert_eq!(
            font.masters[0].metrics.get(&MetricType::ItalicAngle),
            Some(&16)
        );
        assert_eq!(
            font.format_specific
                .get("com.counterpunch.cmap.symbolEncoding"),
            Some(&serde_json::Value::Bool(true))
        );
        let names = &font.names;
        assert_eq!(
            names.family_name.0.get("DEU").map(String::as_str),
            Some("Fixture Metadaten")
        );
        assert_eq!(
            names.copyright.0.get("JPN").map(String::as_str),
            Some("\u{8ab0}\u{3082}")
        );
    }

    #[test]
    fn test_metadata_without_an_axis_for_it() {
        // Variable.ttf has wght, not wdth: a compiler derives the weight class
        // from wght's default, but must be told the width class
        let ot = load(VARIABLE).unwrap().custom_ot_values;
        assert_eq!(ot.os2_us_weight_class, None);
        assert_eq!(ot.os2_us_width_class, Some(5));
    }

    /// The tables' values a font designer sets, by name
    #[cfg(feature = "fontir")]
    fn designer_metadata(binary: &[u8]) -> Vec<(String, String)> {
        let fontref = skrifa::FontRef::new(binary).unwrap();
        let os2 = fontref.os2().unwrap();
        let post = fontref.post().unwrap();
        let head = fontref.head().unwrap();
        let mut values: Vec<(String, String)> = vec![
            ("usWeightClass".into(), os2.us_weight_class().to_string()),
            ("usWidthClass".into(), os2.us_width_class().to_string()),
            ("fsType".into(), os2.fs_type().to_string()),
            ("fsSelection".into(), os2.fs_selection().bits().to_string()),
            ("sFamilyClass".into(), os2.s_family_class().to_string()),
            ("panose".into(), format!("{:?}", os2.panose_10())),
            (
                "ulUnicodeRange".into(),
                format!(
                    "{:?}",
                    [
                        os2.ul_unicode_range_1(),
                        os2.ul_unicode_range_2(),
                        os2.ul_unicode_range_3(),
                        os2.ul_unicode_range_4()
                    ]
                ),
            ),
            (
                "ulCodePageRange".into(),
                format!(
                    "{:?}",
                    [os2.ul_code_page_range_1(), os2.ul_code_page_range_2()]
                ),
            ),
            ("macStyle".into(), head.mac_style().bits().to_string()),
            ("lowestRecPPEM".into(), head.lowest_rec_ppem().to_string()),
            ("isFixedPitch".into(), post.is_fixed_pitch().to_string()),
            (
                "italicAngle".into(),
                post.italic_angle().to_f64().to_string(),
            ),
            (
                "cmap".into(),
                format!(
                    "{:?}",
                    fontref
                        .cmap()
                        .unwrap()
                        .encoding_records()
                        .iter()
                        .filter(
                            |r| r.platform_id() == skrifa::raw::tables::cmap::PlatformId::Windows
                        )
                        .map(|r| r.encoding_id())
                        .collect::<Vec<_>>()
                ),
            ),
        ];
        // the Windows names, but those a compiler writes where a source has none
        let name = fontref.name().unwrap();
        let mut names: Vec<(String, String)> = name
            .name_record()
            .iter()
            .filter(|r| r.platform_id() == 3 && ![3, 4, 5, 6].contains(&r.name_id().to_u16()))
            .map(|r| {
                (
                    format!("name {} 0x{:04X}", r.name_id().to_u16(), r.language_id()),
                    r.string(name.string_data()).unwrap().to_string(),
                )
            })
            .collect();
        names.sort();
        values.extend(names);
        values
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_metadata_round_trips() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        for path in [METADATA, STATIC_LEGACY] {
            let original = std::fs::read(path).unwrap();
            let compiled = BabelfontIrSource::compile(
                load_bytes(&original).unwrap(),
                CompilationOptions::default(),
            )
            .unwrap();
            let (a, b) = (designer_metadata(&original), designer_metadata(&compiled));
            assert_eq!(b, a, "{path}");
        }
    }

    #[cfg(all(feature = "fontir", feature = "glyphs"))]
    #[test]
    fn test_composites_round_trip_through_glyphs() {
        // Abar's bar is scaled and slanted: Glyphs reads its fields in its own
        // order, and its slant in degrees
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let font = load(COMPOSITES).unwrap();
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("Composites.glyphs");
        font.save(&path).unwrap();
        let again = crate::load(&path).unwrap();
        let compile = |f: &Font| {
            BabelfontIrSource::compile(f.clone(), CompilationOptions::default()).unwrap()
        };
        let (a, b) = (compile(&font), compile(&again));
        let drawn = |binary: &[u8], glyph: &str, wght: f32, wdth: f32| {
            let fontref = skrifa::FontRef::new(binary).unwrap();
            let location = fontref.axes().location([("wght", wght), ("wdth", wdth)]);
            let coords: Vec<F2Dot14> = location.coords().to_vec();
            let (gid, _) = fontref
                .glyph_names()
                .iter()
                .find(|(_, name)| name.as_str() == glyph)
                .unwrap();
            let mut points: Vec<(f64, f64)> =
                draw_contours(&fontref, gid, LocationRef::new(&coords))
                    .unwrap()
                    .iter()
                    .flat_map(|p| p.nodes.iter().map(|n| (n.x.round(), n.y.round())))
                    .collect();
            points.sort_by(|p, q| p.partial_cmp(q).unwrap());
            points
        };
        let names: Vec<String> = font.glyphs.iter().map(|g| g.name.to_string()).collect();
        assert!(names.iter().any(|n| n == "Abar"));
        for glyph in &names {
            for (wght, wdth) in [(400.0, 100.0), (100.0, 75.0), (900.0, 100.0), (650.0, 87.5)] {
                assert_eq!(
                    drawn(&b, glyph, wght, wdth),
                    drawn(&a, glyph, wght, wdth),
                    "{glyph} at wght {wght} wdth {wdth}"
                );
            }
        }
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_stat_names_keep_their_languages() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let original = std::fs::read(AVAR2_NO_MAP).unwrap();
        let compiled = BabelfontIrSource::compile(
            load_bytes(&original).unwrap(),
            CompilationOptions::default(),
        )
        .unwrap();
        let fontref = skrifa::FontRef::new(&compiled).unwrap();
        let stat = fontref.stat().unwrap();
        let wght = stat.design_axes().unwrap()[0].axis_name_id();
        let names = name_id_to_i18n(&fontref, wght);
        assert_eq!(names.get_default().map(String::as_str), Some("Weight"));
        assert_eq!(names.0.get("DEU").map(String::as_str), Some("Gewicht"));
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_vertical_metrics_round_trip() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let original = std::fs::read(GDEF_CLASSES).unwrap();
        let compiled = BabelfontIrSource::compile(
            load_bytes(&original).unwrap(),
            CompilationOptions::default(),
        )
        .unwrap();
        let vertical = |binary: &[u8]| {
            let fontref = skrifa::FontRef::new(binary).unwrap();
            let vhea = fontref.vhea().unwrap();
            let vmtx = fontref.vmtx().unwrap();
            let names: Vec<String> = fontref
                .glyph_names()
                .iter()
                .map(|(_, n)| n.to_string())
                .collect();
            let mut glyphs: Vec<(String, Option<u16>, Option<i16>)> = names
                .iter()
                .enumerate()
                .map(|(gid, n)| {
                    let gid = GlyphId::new(gid as u32);
                    (n.clone(), vmtx.advance(gid), vmtx.side_bearing(gid))
                })
                .collect();
            glyphs.sort();
            (
                vhea.ascender().to_i16(),
                vhea.descender().to_i16(),
                vhea.line_gap().to_i16(),
                glyphs,
            )
        };
        assert_eq!(vertical(&compiled), vertical(&original));
    }
    #[test]
    fn test_font_revision() {
        // head.fontRevision 2.005 (0x00020148)
        assert_eq!(load(STATIC_LEGACY).unwrap().version, (2, 5));
    }
    #[test]
    fn test_heights_without_os2_v2() {
        let font = load(STATIC_LEGACY).unwrap();
        let metrics = &font.masters[0].metrics;
        assert_eq!(metrics.get(&MetricType::XHeight), Some(&480));
        assert_eq!(metrics.get(&MetricType::CapHeight), Some(&700));
    }
    #[test]
    fn test_hhea_line_gap() {
        let font = load(STATIC_LEGACY).unwrap();
        let metrics = &font.masters[0].metrics;
        assert_eq!(metrics.get(&MetricType::HheaLineGap), Some(&25));
    }
    #[test]
    fn test_vertical_metrics() {
        // vhea 900/-100/0; a: advance height 1000, tsb 100, yMax 500
        let font = load(GDEF_CLASSES).unwrap();
        let metrics = &font.masters[0].metrics;
        assert_eq!(metrics.get(&MetricType::VheaAscender), Some(&900));
        assert_eq!(metrics.get(&MetricType::VheaDescender), Some(&-100));
        assert_eq!(metrics.get(&MetricType::VheaLineGap), Some(&0));
        let layer = &font.glyphs.get("a").unwrap().layers[0];
        assert_eq!(layer.vertical_advance, Some(1000.0));
        assert_eq!(layer.vertical_origin, Some(600.0));
    }
    #[test]
    fn test_name_record_languages_are_keyed_as_sfd_names() {
        assert_eq!(name_record_language(3, 0x0409).as_deref(), Some("dflt"));
        assert_eq!(name_record_language(3, 0x0403).as_deref(), Some("CAT"));
        assert_eq!(name_record_language(3, 0x080A).as_deref(), Some("es-MX"));
        // Macintosh English and Japanese
        assert_eq!(name_record_language(1, 0).as_deref(), Some("dflt"));
        assert_eq!(name_record_language(1, 11).as_deref(), Some("JPN"));
    }

    #[test]
    fn test_macintosh_names_the_windows_records_state_are_not_repeated() {
        let font = load(STATIC_LEGACY).unwrap();
        let langs: Vec<&String> = font.names.family_name.0.keys().collect();
        assert_eq!(langs, ["dflt", "JPN"]);
        assert_eq!(
            font.names.family_name.get_default().map(String::as_str),
            Some("Fixture Medium")
        );
    }

    #[test]
    fn test_undecodable_name_left_out() {
        // the MacJapanese record does not decode; the Windows one does
        let font = load(STATIC_LEGACY).unwrap();
        let ja: Vec<&String> = font
            .names
            .family_name
            .0
            .iter()
            .filter(|(lang, _)| lang.as_str() == "JPN")
            .map(|(_, name)| name)
            .collect();
        assert_eq!(ja, ["\u{3057}\u{3063}\u{307d}\u{308a}"]);
    }
    #[test]
    fn test_duplicate_glyph_names() {
        // the post table names glyphs 2 and 3 both "dupe"; Acomp uses glyph 3
        let font = load(DUPLICATE_NAMES).unwrap();
        let names: Vec<&str> = font.glyphs.iter().map(|g| g.name.as_str()).collect();
        assert_eq!(names, [".notdef", "A", "dupe", "dupe.1", "Acomp"]);
        let component = font.glyphs.get("Acomp").unwrap().layers[0]
            .components()
            .next()
            .unwrap();
        assert_eq!(component.reference, "dupe.1");
    }
    #[test]
    fn test_named_class_members() {
        let font = load(GDEF_CLASSES).unwrap();
        let (_, members) = font.features.classes.first().unwrap();
        assert_eq!(members.code, "a gravecomb acutecomb tildecomb f_f");
        assert!(
            font.features
                .to_fea()
                .contains("= [a gravecomb acutecomb tildecomb f_f\n];")
        );
    }
    #[test]
    fn test_mark_attachment_is_anchors() {
        // the anchors state the mark feature in full: it is theirs to build
        let font = load(GDEF_CLASSES).unwrap();
        let fea = font.features.to_fea();
        assert!(
            !fea.contains("pos base") && !fea.contains("markClass"),
            "{fea}"
        );
        assert!(!font.features.features.iter().any(|(tag, _)| tag == "mark"));
        let anchor = |glyph: &str, name: &str| {
            font.glyphs.get(glyph).unwrap().layers[0]
                .anchors
                .iter()
                .find(|a| a.name == name)
                .map(|a| (a.x, a.y))
        };
        assert_eq!(anchor("a", "top"), Some((250.0, 500.0)));
        assert_eq!(anchor("acutecomb", "_top"), Some((-100.0, 600.0)));
    }
    /// A static font of these glyphs (name, codepoint, category, anchors),
    /// with kerning, compiled by fontc and read back
    #[cfg(feature = "fontir")]
    fn compiled_and_read(
        glyphs: &[(&str, u32, GlyphCategory, &[(&str, f64, f64)])],
        kerning: &[(&str, &str, i16)],
    ) -> Font {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        use crate::{Anchor, Glyph, Layer, LayerType, Master};
        let mut font = Font::new();
        let mut master = Master::new("Regular", "m", Default::default());
        for (left, right, value) in kerning {
            master
                .kerning
                .insert(((*left).into(), (*right).into()), *value);
        }
        font.masters.push(master);
        for (name, codepoint, category, anchors) in glyphs {
            let mut glyph = Glyph::new(*name);
            glyph.codepoints = vec![*codepoint];
            glyph.category = category.clone();
            glyph.exported = true;
            let width = if *category == GlyphCategory::Mark {
                0.0
            } else {
                600.0
            };
            let mut layer = Layer::new(width);
            layer.master = LayerType::DefaultForMaster("m".into());
            if *category != GlyphCategory::Mark {
                // a box, for anchor names by where they are on it
                use skrifa::outline::OutlinePen;
                let mut pen = PathBuilder::new();
                pen.move_to(0.0, 0.0);
                pen.line_to(0.0, 700.0);
                pen.line_to(600.0, 700.0);
                pen.line_to(600.0, 0.0);
                pen.close();
                layer
                    .shapes
                    .extend(pen.build().into_iter().map(crate::Shape::Path));
            }
            for (anchor, x, y) in anchors.iter() {
                layer.anchors.push(Anchor {
                    id: None,
                    name: (*anchor).into(),
                    x: *x,
                    y: *y,
                    format_specific: Default::default(),
                });
            }
            glyph.layers.push(layer);
            font.glyphs.push(glyph);
        }
        let options = CompilationOptions {
            dont_use_production_names: true,
            ..Default::default()
        };
        let binary = BabelfontIrSource::compile(font, options).unwrap();
        load_bytes(&binary).unwrap()
    }

    #[test]
    #[cfg(feature = "fontir")]
    fn test_compiled_attachment_of_two_scripts_is_anchors() {
        // fontc writes abvm, mark to mark included, before mark, and the
        // kerning before the mark attachment: a mark attached to a mark in
        // abvm before a base in mark is as it builds it
        let base = GlyphCategory::Base;
        let mark = GlyphCategory::Mark;
        let font = compiled_and_read(
            &[
                ("A", 0x41, base.clone(), &[("top", 300.0, 700.0)]),
                ("V", 0x56, base.clone(), &[]),
                ("ka-deva", 0x915, base.clone(), &[("top", 280.0, 720.0)]),
                (
                    "acutecomb",
                    0x301,
                    mark.clone(),
                    &[("_top", 0.0, 500.0), ("top", 0.0, 700.0)],
                ),
                (
                    "candrabindu-deva",
                    0x901,
                    mark.clone(),
                    &[("_top", 0.0, 520.0), ("top", 0.0, 760.0)],
                ),
            ],
            &[("A", "V", -50)],
        );
        let fea = font.features.to_fea();
        for code in ["pos base", "pos mark", "markClass"] {
            assert!(!fea.contains(code), "{code} stays:\n{fea}");
        }
        let anchors = |glyph: &str| -> Vec<String> {
            let mut names: Vec<String> = font.glyphs.get(glyph).unwrap().layers[0]
                .anchors
                .iter()
                .map(|a| a.name.to_string())
                .collect();
            names.sort();
            names
        };
        // (named by where they are on glyphs without outlines) base and mark
        // anchors pair up, and the marks attach to marks
        let [on_base] = anchors("A").try_into().unwrap();
        assert!(anchors("acutecomb").contains(&format!("_{on_base}")));
        assert_eq!(anchors("candrabindu-deva").len(), 2);
        assert_eq!(anchors("ka-deva").len(), 1);
    }

    #[test]
    #[cfg(feature = "fontir")]
    fn test_anchor_names_keep_the_order_of_their_lookups() {
        // two groups attach tildecomb to A: fontc writes a lookup per group in
        // the order of their names, and the later one wins (FleurDeLeah's top
        // and topviet). Read back, the names keep that order, though by where
        // they are the first is topright, the second topleft.
        let font = compiled_and_read(
            &[
                (
                    "A",
                    0x41,
                    GlyphCategory::Base,
                    &[("top", 560.0, 700.0), ("topviet", 40.0, 690.0)],
                ),
                (
                    "tildecomb",
                    0x303,
                    GlyphCategory::Mark,
                    &[("_top", 0.0, 500.0), ("_topviet", 10.0, 480.0)],
                ),
            ],
            &[],
        );
        let fea = font.features.to_fea();
        assert!(!fea.contains("pos base"), "{fea}");
        let layer = &font.glyphs.get("A").unwrap().layers[0];
        let name_at = |x: f64| {
            layer
                .anchors
                .iter()
                .find(|a| a.x == x)
                .map(|a| a.name.clone())
                .unwrap()
        };
        assert!(name_at(560.0) < name_at(40.0), "{:?}", layer.anchors);
        for anchor in &layer.anchors {
            assert!(
                !anchor.name.starts_with("mark_class"),
                "{:?}",
                layer.anchors
            );
        }
    }

    #[test]
    #[cfg(feature = "fontir")]
    fn test_anchor_names_keep_abvm_and_blwm_apart() {
        // fontc puts a Devanagari group in blwm by its name (bottom*, nukta)
        let font = compiled_and_read(
            &[
                (
                    "ka-deva",
                    0x915,
                    GlyphCategory::Base,
                    &[("top", 280.0, 720.0), ("bottom", 280.0, -40.0)],
                ),
                (
                    "candrabindu-deva",
                    0x901,
                    GlyphCategory::Mark,
                    &[("_top", 0.0, 520.0)],
                ),
                (
                    "nukta-deva",
                    0x93C,
                    GlyphCategory::Mark,
                    &[("_bottom", 0.0, 0.0)],
                ),
            ],
            &[],
        );
        let fea = font.features.to_fea();
        assert!(!fea.contains("pos base"), "{fea}");
        let names: Vec<String> = font.glyphs.get("ka-deva").unwrap().layers[0]
            .anchors
            .iter()
            .map(|a| a.name.to_string())
            .collect();
        let below: Vec<&String> = names.iter().filter(|n| n.starts_with("bottom")).collect();
        assert_eq!(below.len(), 1, "{names:?}");
    }

    #[test]
    fn test_feature_code_does_not_depend_on_prefix_order() {
        // the calt rule calls a lookup: everything it needs is in the one
        // prefix, before it
        let font = load(GDEF_CLASSES).unwrap();
        assert_eq!(font.features.prefixes.len(), 1);
        let code = &font.features.prefixes.get("Decompiled").unwrap().code;
        let at = |text: &str| code.find(text).unwrap_or_else(|| panic!("{text}: {code}"));
        assert!(at("languagesystem") < at("lookup gsub_single"));
        assert!(at("lookup gsub_single") < at("lookup gsub_chain_contextual"));
    }
    #[test]
    fn test_categories_from_gdef() {
        let font = load(GDEF_CLASSES).unwrap();
        // a base in GDEF, though U+0300 is a nonspacing mark
        assert_category(&font, "gravecomb", GlyphCategory::Base, None);
        assert_category(&font, "acutecomb", GlyphCategory::Mark, Some("Nonspacing"));
        assert_category(
            &font,
            "tildecomb",
            GlyphCategory::Mark,
            Some("Spacing Combining"),
        );
        assert_category(&font, "f_f", GlyphCategory::Ligature, Some("Ligature"));
    }

    #[test]
    fn test_categories_without_gdef() {
        // a shaper takes gravecomb for a mark by its character: so does the
        // compiled font's GDEF, and its advance is zeroed alike
        let font = load(STATIC_LEGACY).unwrap();
        assert_category(&font, "gravecomb", GlyphCategory::Mark, Some("Nonspacing"));
        for glyph in ["commaaccent", "x"] {
            assert_category(&font, glyph, GlyphCategory::Base, None);
        }
    }

    #[test]
    fn test_categories_without_gdef_attaching_marks() {
        let font = load(MARK_NO_GDEF).unwrap();
        assert_category(&font, "gravecomb", GlyphCategory::Mark, Some("Nonspacing"));
        // U+034F is a default-ignorable nonspacing mark
        for glyph in ["a", "commaaccent", "uni034F"] {
            assert_category(&font, glyph, GlyphCategory::Base, None);
        }
    }
    #[test]
    fn test_legacy_kerning() {
        // two subtables; x x is in both
        let font = load(STATIC_LEGACY).unwrap();
        let kerning = &font.masters[0].kerning;
        assert_eq!(kerning.get(&("x".into(), "H".into())), Some(&-30));
        assert_eq!(kerning.get(&("H".into(), "x".into())), Some(&-20));
        assert_eq!(kerning.get(&("x".into(), "x".into())), Some(&10));
        // a shaper skips marks applying the table
        assert_eq!(kerning.get(&("x".into(), "gravecomb".into())), None);
        // and ignores it when GPOS has a kern feature: the GPOS kerning is the
        // font's kerning
        let font = load(GDEF_CLASSES).unwrap();
        let kerning = &font.masters[0].kerning;
        assert_eq!(kerning.get(&("a".into(), "f_f".into())), None);
        // (the GPOS kerning has no IgnoreMarks: it stays feature code)
        assert!(font.features.to_fea().contains("pos a a -10"));
    }
    #[test]
    fn test_unnamed_glyphs() {
        // post format 3: encoded glyphs are named for their codepoints everywhere
        let font = load(UNNAMED).unwrap();
        let names: Vec<&str> = font.glyphs.iter().map(|g| g.name.as_str()).collect();
        assert_eq!(names, [".notdef", "uni0041", "uni00C5", "glyph00003"]);
        let component = font.glyphs.get("uni00C5").unwrap().layers[0]
            .components()
            .next()
            .unwrap();
        assert_eq!(component.reference, "uni0041");
        assert_eq!(
            font.masters[0]
                .kerning
                .get(&("uni0041".into(), "uni00C5".into())),
            Some(&-50)
        );
        assert!(
            font.features
                .to_fea()
                .contains("sub uni0041 by glyph00003;")
        );
    }

    #[test]
    fn test_load_bytes() {
        let font = load_bytes(&std::fs::read(QUADRATIC).unwrap()).unwrap();
        assert_eq!(font.glyphs.len(), 7);
    }
    #[test]
    fn test_off_curve_only_contour() {
        use crate::NodeType::OffCurve;
        let font = load(QUADRATIC).unwrap();
        let layer = &font.glyphs.get("o").unwrap().layers[0];
        assert_eq!(node_types(layer), vec![vec![OffCurve; 4]]);
        let bbox = layer.paths().next().unwrap().to_kurbo().unwrap();
        let bbox = kurbo::Shape::bounding_box(&bbox);
        // the curves pass a quarter of the way from the implied on-curves to
        // the off-curves
        assert_close(
            &[bbox.x0, bbox.y0, bbox.x1, bbox.y1],
            &[62.5, 62.5, 437.5, 437.5],
        );
    }
    #[test]
    fn test_implied_on_curves_stay_implied() {
        use crate::NodeType::QCurve as Q;
        // glyf: 9 points, 4 explicit on-curves; (250, 700) and (0, 350) are
        // midpoints, and so is the one between (450, 0) and (500, 200)
        let font = load(QUADRATIC).unwrap();
        let layer = &font.glyphs.get("O").unwrap().layers[0];
        // the glyf contour's start, (250, 0), stays its start
        let last = layer.paths().next().unwrap().nodes.last().unwrap();
        assert_eq!((last.x, last.y), (250.0, 0.0));
        assert_eq!(
            node_types(layer)[0].iter().filter(|t| **t == Q).count(),
            2,
            "{:?}",
            layer.paths().next().unwrap().nodes
        );
    }
    /// The contours of a glyph's master layers, as (node type, x, y), by the
    /// masters' first coordinate
    fn master_nodes(font: &Font, glyph: &str) -> Vec<Vec<Vec<(crate::NodeType, f64, f64)>>> {
        let mut masters: Vec<&crate::Master> = font.masters.iter().collect();
        masters.sort_by_key(|m| OrderedFloat(m.location.iter().next().unwrap().1.to_f64()));
        masters
            .iter()
            .map(|m| {
                font.glyphs
                    .get(glyph)
                    .unwrap()
                    .layers
                    .iter()
                    .find(|l| l.master == LayerType::DefaultForMaster(m.id.clone()))
                    .unwrap()
                    .paths()
                    .map(|p| p.nodes.iter().map(|n| (n.nodetype, n.x, n.y)).collect())
                    .collect()
            })
            .collect()
    }
    #[test]
    fn test_coincident_points_keep_their_place() {
        use crate::NodeType::{Line, OffCurve, QCurve};
        // the glyf points coincide at 400 only; every master has every point,
        // in the same order, starting where the glyf contour starts
        let font = load(COINCIDENT).unwrap();
        let [regular, bold] = &master_nodes(&font, "A")[..] else {
            panic!()
        };
        // the last point, on the start at 400, stays a point of its own
        let types = |c: &Vec<(crate::NodeType, f64, f64)>| c.iter().map(|n| n.0).collect_vec();
        assert_eq!(types(&regular[0]), vec![Line; 5]);
        assert_eq!(types(&regular[0]), types(&bold[0]));
        assert_eq!(regular[0].last().unwrap(), &(Line, 0.0, 0.0));
        assert_eq!(bold[0].last().unwrap(), &(Line, 0.0, 0.0));
        assert_eq!(bold[0].first().unwrap(), &(Line, 40.0, 0.0));
        // the start, passed again at 400, is the glyf's start in every master
        let [regular, bold] = &master_nodes(&font, "B")[..] else {
            panic!()
        };
        assert_eq!(regular[0].last().unwrap(), &(Line, 250.0, 250.0));
        assert_eq!(bold[0].last().unwrap(), &(Line, 290.0, 250.0));
        assert_eq!(regular[0][2], (Line, 250.0, 250.0));
        assert_eq!(bold[0][2], (Line, 250.0, 250.0));
        // a curve closes C back to its start; the on-curve on the start at
        // 400 is a point of its own
        let [regular, bold] = &master_nodes(&font, "C")[..] else {
            panic!()
        };
        assert_eq!(types(&regular[0]), vec![OffCurve, QCurve, Line, Line, Line]);
        assert_eq!(types(&regular[0]), types(&bold[0]));
        assert_eq!(regular[0][1], (QCurve, 0.0, 0.0));
        assert_eq!(bold[0][1], (QCurve, 400.0, 0.0));
        assert_eq!(regular[0][4], (Line, 0.0, 0.0));
        assert_eq!(bold[0][4], (Line, 0.0, 0.0));
        // single-point contours stay
        for master in master_nodes(&font, "D") {
            assert_eq!(master.iter().map(|c| c.len()).collect_vec(), vec![4, 1, 1]);
        }
    }
    #[test]
    fn test_malformed_glyphs_and_tables_do_not_fail_the_font() {
        let font = load(MALFORMED).unwrap();
        // no OS/2: the compiler's defaults
        assert_eq!(font.custom_ot_values.os2_vendor_id, None);
        assert!(!font.masters[0].metrics.contains_key(&MetricType::WinAscent));
        // a component cycle and too deep a nesting keep their components
        let references = |name: &str| -> Vec<String> {
            font.glyphs.get(name).unwrap().layers[0]
                .components()
                .map(|c| c.reference.to_string())
                .collect()
        };
        assert_eq!(references("B"), vec!["C"]);
        assert_eq!(references("C"), vec!["B"]);
        assert_eq!(references("n39"), vec!["n38"]);
        assert_eq!(references("D"), vec!["A"]);
        // the glyphs around them read as usual
        assert_eq!(font.glyphs.get("A").unwrap().layers[0].paths().count(), 1);
        // the cmap entry for a glyph the font does not have is left out
        assert!(
            font.glyphs
                .iter()
                .all(|g| !g.codepoints.contains(&('Z' as u32)))
        );
        assert_eq!(font.glyphs.get("A").unwrap().codepoints, vec!['A' as u32]);
    }
    #[test]
    fn test_cff_font_is_refused() {
        let error = load(CFF).unwrap_err().to_string();
        assert!(
            error.contains(
                "Only TrueType-outline (glyf) fonts can be opened; this font has CFF outlines"
            ),
            "{error}"
        );
    }
    #[test]
    fn test_point_matched_component() {
        // the dot's point 0 (0, 0) sits on O's point 2 (500, 200)
        let font = load(QUADRATIC).unwrap();
        assert_close(&component_coeffs(&font, "dotO")[1][4..], &[500.0, 200.0]);
    }
    #[test]
    fn test_anchor_name_for_mark() {
        // from the combining class: above, below, overlay, attached below
        assert_eq!(anchor_name_for_mark(Some(0x0301), None), Some("top"));
        assert_eq!(anchor_name_for_mark(Some(0x0323), None), Some("bottom"));
        assert_eq!(anchor_name_for_mark(Some(0x0335), None), Some("center"));
        assert_eq!(anchor_name_for_mark(Some(0x0327), None), Some("bottom"));
        // Arabic harakat: fatha above, kasra below
        assert_eq!(anchor_name_for_mark(Some(0x064E), None), Some("top"));
        assert_eq!(anchor_name_for_mark(Some(0x0650), None), Some("bottom"));
        // the ogonek, attached below like a cedilla, by its name
        assert_eq!(
            anchor_name_for_mark(Some(0x0328), Some("COMBINING OGONEK")),
            Some("ogonek")
        );
        // without a codepoint, from the description
        assert_eq!(
            anchor_name_for_mark(None, Some("ARABIC THREE DOTS BELOW")),
            Some("bottom")
        );
        assert_eq!(anchor_name_for_mark(None, Some("ARABIC DOT CENTER")), None);
    }

    #[cfg(feature = "glyphs")]
    #[test]
    fn test_placeholder_anchors_named_by_their_marks() {
        let anchored = |name: &str, codepoints: Vec<u32>, anchors: &[&str]| {
            let mut glyph = Glyph::new(name);
            glyph.codepoints = codepoints;
            let mut layer = Layer::new(0.0);
            for anchor in anchors {
                layer.anchors.push(Anchor {
                    id: None,
                    name: anchor.to_string(),
                    x: 0.0,
                    y: 0.0,
                    format_specific: FormatSpecific::default(),
                });
            }
            glyph.layers.push(layer);
            glyph
        };
        let mut font = Font::new();
        font.glyphs.push(anchored(
            "o",
            vec![0x6F],
            &["top", "mark_class_0x1", "mark_class_0x2", "mark_class_0x3"],
        ));
        // overlay marks: one by Unicode, one by its Glyphs name (U+0336)
        font.glyphs
            .push(anchored("uni0335", vec![0x0335], &["_mark_class_0x1"]));
        font.glyphs.push(anchored(
            "strokelongcomb.case",
            vec![],
            &["_mark_class_0x1"],
        ));
        // above, but top is taken
        font.glyphs
            .push(anchored("acutecomb", vec![0x0301], &["_top"]));
        font.glyphs
            .push(anchored("gravecomb.alt", vec![], &["_mark_class_0x2"]));
        // nothing says what it is
        font.glyphs
            .push(anchored("blob", vec![], &["_mark_class_0x3"]));
        // an Arabic top: does not share top with the Latin one, whose
        // acutecomb is a mark of every script (it would attach to beh)
        font.glyphs
            .push(anchored("behar", vec![], &["mark_class_0x4"]));
        font.glyphs
            .push(anchored("fatha-ar", vec![0x064E], &["_mark_class_0x4"]));
        font.features.prefixes.insert(
            "Decompiled".into(),
            PossiblyAutomaticCode::new(
                "markClass uni0335 <anchor 0 0> @mark_class_0x1;\n\
                 markClass gravecomb.alt <anchor 0 0> @mark_class_0x2;\n\
                 markClass fatha-ar <anchor 0 0> @mark_class_0x4;",
            ),
        );
        name_placeholder_anchors(&mut font, &HashSet::new());
        let names = |glyph: &str| -> Vec<String> {
            font.glyphs.get(glyph).unwrap().layers[0]
                .anchors
                .iter()
                .map(|a| a.name.clone())
                .collect()
        };
        assert_eq!(
            names("o"),
            ["top", "center", "mark_class_0x2", "mark_class_0x3"]
        );
        assert_eq!(names("uni0335"), ["_center"]);
        assert_eq!(names("strokelongcomb.case"), ["_center"]);
        assert_eq!(names("gravecomb.alt"), ["_mark_class_0x2"]);
        assert_eq!(names("blob"), ["_mark_class_0x3"]);
        assert_eq!(names("behar"), ["mark_class_0x4"]);
        assert_eq!(names("fatha-ar"), ["_mark_class_0x4"]);
        let code = &font.features.prefixes.get("Decompiled").unwrap().code;
        assert!(
            code.contains("@center;") && code.contains("@mark_class_0x2;"),
            "{code}"
        );
        assert!(code.contains("@mark_class_0x4;"), "{code}");
    }

    #[test]
    fn test_anchor_names() {
        let font = load(ANCHORS).unwrap();
        let anchors = |glyph: &str| -> Vec<(String, f64, f64)> {
            let mut anchors: Vec<_> = font.glyphs.get(glyph).unwrap().layers[0]
                .anchors
                .iter()
                .map(|a| (a.name.to_string(), a.x, a.y))
                .collect();
            anchors.sort_by(|a, b| a.0.cmp(&b.0));
            anchors
        };
        let names = |glyph: &str| -> Vec<String> {
            anchors(glyph)
                .into_iter()
                .map(|(name, _, _)| name)
                .collect()
        };
        assert_eq!(names("a"), ["bottom", "ogonek", "top"]);
        assert_eq!(names("uni0328"), ["_ogonek"]);
        assert_eq!(names("dotbelowcomb"), ["_bottom"]);
        // mark-to-mark uses the same class as mark-to-base: one pair of names
        assert_eq!(
            anchors("acutecomb"),
            [
                ("_top".to_string(), -100.0, 500.0),
                ("top".to_string(), -100.0, 750.0)
            ]
        );
        // a ligature's anchors are numbered by component
        assert_eq!(
            anchors("f_f"),
            [
                ("top_1".to_string(), 250.0, 700.0),
                ("top_2".to_string(), 750.0, 700.0)
            ]
        );
    }
    #[cfg(feature = "fontir")]
    #[test]
    fn test_moved_anchor_moves_the_attachment() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let mut font = load(SHAPING_MKMK).unwrap();
        let fea = font.features.to_fea();
        assert!(!fea.contains("markClass"), "{fea}");
        let anchors = |font: &Font| -> Vec<(String, String, f64, f64)> {
            let mut anchors: Vec<_> = font
                .glyphs
                .iter()
                .flat_map(|g| {
                    g.layers[0]
                        .anchors
                        .iter()
                        .map(|a| (g.name.to_string(), a.name.clone(), a.x, a.y))
                })
                .collect();
            anchors.sort_by(|a, b| (&a.0, &a.1).cmp(&(&b.0, &b.1)));
            anchors
        };
        let compile = |font: &Font| {
            let bytes =
                BabelfontIrSource::compile(font.clone(), CompilationOptions::default()).unwrap();
            load_bytes(&bytes).unwrap()
        };
        // the compiler builds the same attachment from the anchors
        assert_eq!(anchors(&compile(&font)), anchors(&font));
        // and a moved anchor moves it
        let top = font.glyphs.get_mut("A").unwrap().layers[0]
            .anchors
            .iter_mut()
            .find(|a| a.name == "top")
            .unwrap();
        top.x += 30.0;
        let again = compile(&font);
        let moved = again.glyphs.get("A").unwrap().layers[0]
            .anchors
            .iter()
            .find(|a| a.name == "top")
            .map(|a| a.x);
        assert_eq!(moved, Some(330.0));
    }
    /// STAT's axis values: format, location, flags and name
    fn stat_values(binary: &[u8]) -> Vec<String> {
        use skrifa::raw::tables::stat::AxisValue;
        let fontref = skrifa::FontRef::new(binary).unwrap();
        let stat = fontref.stat().unwrap();
        let name = |id: StringId| {
            name_id_to_i18n(&fontref, id)
                .get_default()
                .cloned()
                .unwrap_or_default()
        };
        stat.offset_to_axis_values()
            .unwrap()
            .unwrap()
            .axis_values()
            .iter()
            .flatten()
            .map(|value| match value {
                AxisValue::Format1(v) => {
                    format!(
                        "1 {} {} {}",
                        v.value(),
                        v.flags().bits(),
                        name(v.value_name_id())
                    )
                }
                AxisValue::Format2(v) => format!(
                    "2 {} {} {} {} {}",
                    v.nominal_value(),
                    v.range_min_value(),
                    v.range_max_value(),
                    v.flags().bits(),
                    name(v.value_name_id())
                ),
                AxisValue::Format3(v) => format!(
                    "3 {} {} {} {}",
                    v.value(),
                    v.linked_value(),
                    v.flags().bits(),
                    name(v.value_name_id())
                ),
                AxisValue::Format4(v) => format!("4 {}", name(v.value_name_id())),
            })
            .collect()
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_stat_round_trips() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let original = std::fs::read(VARIABLE).unwrap();
        let font = load_bytes(&original).unwrap();
        let code = &font.features.prefixes.get("STAT").unwrap().code;
        assert!(code.contains("location wght 900 800 900;"), "{code}");
        assert!(code.contains("location wght 400 700;"), "{code}");
        assert!(code.contains("flag ElidableAxisValueName;"), "{code}");
        let compiled = BabelfontIrSource::compile(font, CompilationOptions::default()).unwrap();
        let values = stat_values(&original);
        assert_eq!(values.len(), 3);
        assert_eq!(stat_values(&compiled), values);
    }

    #[test]
    fn test_avar2_mappings() {
        let font = load(AVAR2).unwrap();
        assert_eq!(font.cross_axis_mappings.len(), 1);
        let mapping = &font.cross_axis_mappings[0];
        let at = |loc: &DesignLocation, tag: &[u8; 4]| {
            loc.get(Tag::new(tag))
                .map(|c| (c.to_f64() * 10.0).round() / 10.0)
        };
        assert_eq!(at(&mapping.input, b"wght"), Some(900.0));
        assert_eq!(at(&mapping.input, b"wdth"), Some(75.0));
        assert_eq!(at(&mapping.output, b"wght"), Some(850.0));
        assert_eq!(at(&mapping.output, b"wdth"), Some(75.0));
    }

    #[test]
    fn test_avar2_without_axis_index_map() {
        // the implicit map: axis i's deltas are item i of the first data
        let font = load(AVAR2_NO_MAP).unwrap();
        assert_eq!(font.cross_axis_mappings.len(), 1);
        let mapping = &font.cross_axis_mappings[0];
        let at = |loc: &DesignLocation, tag: &[u8; 4]| loc.get(Tag::new(tag)).map(|c| c.to_f64());
        assert_eq!(at(&mapping.input, b"wght"), Some(900.0));
        assert_eq!(at(&mapping.input, b"wdth"), Some(75.0));
        assert_eq!(at(&mapping.output, b"wght"), Some(850.0));
        assert_eq!(at(&mapping.output, b"wdth"), Some(75.0));
    }

    #[test]
    fn test_avar2_instances_stand_where_fvar_says() {
        // not where avar2 draws them: wght 900 at wdth 75 is drawn as 850
        let font = load(AVAR2_NO_MAP).unwrap();
        let locations: Vec<(String, Vec<f64>)> = font
            .instances
            .iter()
            .map(|i| {
                (
                    i.name.get_default().unwrap().clone(),
                    font.axes
                        .iter()
                        .map(|a| i.location.get(a.tag).unwrap().to_f64())
                        .collect(),
                )
            })
            .collect();
        assert_eq!(
            locations,
            [
                ("Black Condensed".to_string(), vec![900.0, 75.0]),
                ("Thin".to_string(), vec![100.0, 100.0])
            ]
        );
    }

    #[test]
    fn test_coordinates_as_the_source_states_them() {
        // avar and gvar store F2Dot14s: user 600 reads back as 600.0122; they
        // are the shortest values stored as they are. (A binary does not know
        // the source's design coordinates: they are user coordinates mapped.)
        let font = load(VARIABLE).unwrap();
        let axis = &font.axes[0];
        let map: Vec<(f64, f64)> = axis
            .map
            .as_ref()
            .unwrap()
            .iter()
            .map(|(u, d)| (u.to_f64(), d.to_f64()))
            .collect();
        assert_eq!(
            map,
            [
                (100.0, 100.0),
                (400.0, 400.0),
                (600.0, 608.34),
                (900.0, 900.0)
            ]
        );
        let mut masters: Vec<f64> = font
            .masters
            .iter()
            .map(|m| m.location.get(axis.tag).unwrap().to_f64())
            .collect();
        masters.sort_by(f64::total_cmp);
        assert_eq!(masters, [100.0, 400.0, 900.0]);
        // an instance at user 333, which F2Dot14 stores as 333.0017
        let converter = axis._converter().unwrap();
        let stored = UserCoord::new(333.0).to_normalized(&converter).to_f2dot14();
        assert_ne!(
            NormalizedCoord::new(stored.to_f32() as f64)
                .to_user(&converter)
                .to_f64(),
            333.0
        );
        assert_eq!(user_at(stored, &converter).to_f64(), 333.0);
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_avar2_round_trips() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        // drawn at user locations, through avar and avar2
        let drawn = |binary: &[u8], wght: f32, wdth: f32| {
            let fontref = skrifa::FontRef::new(binary).unwrap();
            let location = fontref.axes().location([("wght", wght), ("wdth", wdth)]);
            let coords: Vec<F2Dot14> = location.coords().to_vec();
            let gid = fontref.charmap().map('I').unwrap();
            draw_contours(&fontref, gid, LocationRef::new(&coords))
                .unwrap()
                .iter()
                .flat_map(|p| p.nodes.iter().map(|n| (n.x.round(), n.y.round())))
                .collect::<Vec<_>>()
        };
        let instances = |binary: &[u8]| {
            let fontref = skrifa::FontRef::new(binary).unwrap();
            fontref
                .named_instances()
                .iter()
                .map(|i| i.user_coords().collect::<Vec<_>>())
                .collect::<Vec<_>>()
        };
        for path in [AVAR2, AVAR2_NO_MAP] {
            let original = std::fs::read(path).unwrap();
            let compiled = BabelfontIrSource::compile(
                load_bytes(&original).unwrap(),
                CompilationOptions::default(),
            )
            .unwrap();
            for (wght, wdth) in [(900.0, 75.0), (500.0, 75.0), (900.0, 90.0), (300.0, 100.0)] {
                assert_eq!(
                    drawn(&compiled, wght, wdth),
                    drawn(&original, wght, wdth),
                    "{path} wght {wght} wdth {wdth}"
                );
            }
            // the mapping does something: wght 900 draws as 850 there
            assert_ne!(
                drawn(&original, 900.0, 75.0),
                drawn(&original, 900.0, 100.0)
            );
            assert_eq!(instances(&compiled), instances(&original), "{path}");
        }
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_right_to_left_kerning() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        // <v 0 v 0>, as kerning is written for Arabic, is kerning
        let font = load(RTL_KERNING).unwrap();
        let kerning = |font: &Font| font.masters[0].kerning.clone();
        let expected = kerning(&font);
        assert_eq!(
            expected.get(&("alef-ar".into(), "beh-ar".into())),
            Some(&-50)
        );
        // @ALEFS = [alef-ar], a group of one
        assert_eq!(
            expected.get(&("@beh-ar".into(), "@alef-ar".into())),
            Some(&-30)
        );
        assert!(
            !font.features.to_fea().contains("pos "),
            "{}",
            font.features.to_fea()
        );
        // and a compiler writes it so again
        let options = CompilationOptions {
            dont_use_production_names: true,
            ..Default::default()
        };
        let compiled = BabelfontIrSource::compile(font.clone(), options).unwrap();
        let again = load_bytes(&compiled).unwrap();
        for (left, right) in [
            ("alef-ar", "beh-ar"),
            ("beh-ar", "alef-ar"),
            ("teh-ar", "alef-ar"),
        ] {
            assert_eq!(
                kern_value(&again, &again.masters[0], left, right),
                kern_value(&font, &font.masters[0], left, right),
                "{left} {right}"
            );
        }
        let fontref = skrifa::FontRef::new(&compiled).unwrap();
        let fea = sr_aef::uncompile(&fontref, true).unwrap().as_fea("");
        assert!(fea.contains("<-50 0 -50 0>"), "{fea}");
    }

    #[test]
    fn test_smooth_points() {
        let font = load(QUADRATIC).unwrap();
        let smooth = |glyph: &str| -> Vec<(f64, f64)> {
            let mut points: Vec<(f64, f64)> = font.glyphs.get(glyph).unwrap().layers[0]
                .paths()
                .flat_map(|p| p.nodes.iter().filter(|n| n.smooth).map(|n| (n.x, n.y)))
                .collect();
            points.sort_by(|a, b| a.partial_cmp(b).unwrap());
            points
        };
        // lines running into curves along their tangents; the top corners are not
        assert_eq!(smooth("u"), [(0.0, 250.0), (500.0, 250.0)]);
        // two curves meeting at an angle
        assert_eq!(smooth("v"), []);
        // on-curves between off-curves on one line
        assert_eq!(smooth("O"), [(250.0, 0.0), (500.0, 350.0)]);
    }

    #[test]
    fn test_smooth_only_where_smooth_in_every_master() {
        // Q's top on-curve is halfway, hence smooth, in Thin and Regular; in
        // Black it is not halfway but still on the line between its off-curves
        let font = load(VARIABLE).unwrap();
        for name in ["Thin", "Regular", "Black"] {
            let smooth = layer(&font, "A", name)
                .paths()
                .flat_map(|p| p.nodes.iter())
                .any(|n| n.smooth);
            assert!(!smooth, "A is all corners in {name}");
        }
        let count = |name: &str| {
            layer(&font, "Q", name)
                .paths()
                .flat_map(|p| p.nodes.iter())
                .filter(|n| n.smooth)
                .count()
        };
        assert_eq!(count("Thin"), count("Black"));
        assert!(count("Thin") > 0);
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_font_metadata_round_trips() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let original = std::fs::read(VARIABLE).unwrap();
        let font = load_bytes(&original).unwrap();

        // head.created
        assert_eq!(font.date.to_rfc3339(), "2024-01-01T00:00:00+00:00");
        // variation sequences survive JSON, nested by selector
        let json = serde_json::to_value(&font).unwrap();
        assert_eq!(
            json["variation_sequences"],
            serde_json::json!({"65024": {"65": "Adot", "79": "O"}})
        );
        let from_json: Font = serde_json::from_value(json).unwrap();
        assert_eq!(from_json.variation_sequences, font.variation_sequences);
        // variation sequences: a variant glyph, and the default one
        let sequences: Vec<_> = font.variation_sequences.iter().collect();
        assert_eq!(
            sequences,
            [
                (&(0xFE00, 0x41), &SmolStr::from("Adot")),
                (&(0xFE00, 0x4F), &SmolStr::from("O"))
            ]
        );
        // the named instances' PostScript names
        let postscript: Vec<&str> = font
            .instances
            .iter()
            .map(|i| {
                i.custom_names
                    .postscript_name
                    .get_default()
                    .unwrap()
                    .as_str()
            })
            .collect();
        assert_eq!(
            postscript,
            [
                "FixtureVariable-Thin",
                "FixtureVariable-Regular",
                "FixtureVariable-Black"
            ]
        );
        // hhea's caret, varied by MVAR
        for (name, run, offset) in [
            ("Thin", 100, -5),
            ("Regular", 150, -10),
            ("Black", 200, -15),
        ] {
            let metrics = &master(&font, name).metrics;
            assert_eq!(metrics.get(&MetricType::HheaCaretSlopeRise), Some(&1000));
            assert_eq!(
                metrics.get(&MetricType::HheaCaretSlopeRun),
                Some(&run),
                "{name}"
            );
            assert_eq!(
                metrics.get(&MetricType::HheaCaretOffset),
                Some(&offset),
                "{name}"
            );
        }

        // and a compiler writes them again
        let compiled =
            BabelfontIrSource::compile(font.clone(), CompilationOptions::default()).unwrap();
        let (a, b) = (
            skrifa::FontRef::new(&original).unwrap(),
            skrifa::FontRef::new(&compiled).unwrap(),
        );
        assert_eq!(b.head().unwrap().created(), a.head().unwrap().created());
        let variants = |f: &skrifa::FontRef| {
            f.charmap()
                .variant_mappings()
                .map(|(c, s, v)| {
                    let gid = match v {
                        skrifa::raw::tables::cmap::MapVariant::UseDefault => None,
                        skrifa::raw::tables::cmap::MapVariant::Variant(gid) => {
                            f.charmap().map(c).map(|default| default != gid)
                        }
                    };
                    (c, s, gid)
                })
                .collect::<Vec<_>>()
        };
        assert_eq!(variants(&b), variants(&a));
        let postscript_ids = |f: &skrifa::FontRef| {
            f.named_instances()
                .iter()
                .map(|i| {
                    i.postscript_name_id()
                        .map(|id| name_id_to_i18n(f, id).get_default().cloned())
                })
                .collect::<Vec<_>>()
        };
        assert_eq!(postscript_ids(&b), postscript_ids(&a));
        let again = load_bytes(&compiled).unwrap();
        for name in ["Thin", "Regular", "Black"] {
            for metric in [MetricType::HheaCaretSlopeRun, MetricType::HheaCaretOffset] {
                assert_eq!(
                    master(&again, name).metrics.get(&metric),
                    master(&font, name).metrics.get(&metric),
                    "{name} {metric:?}"
                );
            }
        }
    }

    #[test]
    fn test_master_names_from_instances() {
        let font = load(VARIABLE).unwrap();
        let names: Vec<&str> = font
            .masters
            .iter()
            .map(|m| m.name.get_default().unwrap().as_str())
            .collect();
        assert_eq!(names, ["Thin", "Black", "Regular"]);
    }
    #[test]
    fn test_master_locations_through_avar() {
        // avar maps user 600 to the design location the sparse master sits at
        let font = load(VARIABLE).unwrap();
        let axis = &font.axes[0];
        let converter = axis._converter().unwrap();
        for (name, user) in [("Thin", 100.0), ("Regular", 400.0), ("Black", 900.0)] {
            let design = master(&font, name).location.get(axis.tag).unwrap();
            assert_close(&[design.to_user(&converter).to_f64()], &[user]);
        }
    }
    #[test]
    fn test_component_offsets_vary() {
        let font = load(VARIABLE).unwrap();
        for (name, x) in [("Thin", 150.0), ("Regular", 200.0), ("Black", 300.0)] {
            let dot = layer(&font, "Adot", name).components().nth(1).unwrap();
            assert_close(&dot.transform.as_affine().as_coeffs()[4..], &[x, 750.0]);
        }
    }
    #[test]
    fn test_advances_with_and_without_hvar() {
        for path in [VARIABLE, VARIABLE_NO_HVAR] {
            let font = load(path).unwrap();
            for (name, width) in [("Thin", 450.0), ("Regular", 550.0), ("Black", 700.0)] {
                assert_eq!(layer(&font, "A", name).width, width, "{path} {name}");
            }
        }
    }
    #[test]
    fn test_glyph_intermediate_master() {
        let font = load(VARIABLE).unwrap();
        let brace = font.glyphs.get("brace").unwrap();
        let intermediate: Vec<&Layer> = brace
            .layers
            .iter()
            .filter(|l| l.location.is_some())
            .collect();
        assert_eq!(intermediate.len(), 1);
        let layer = intermediate[0];
        let axis = &font.axes[0];
        let user = layer.location.as_ref().unwrap().get(axis.tag).unwrap();
        // as near 600 as F2Dot14 gets
        let user = user.to_user(&axis._converter().unwrap()).to_f64();
        assert!((user - 600.0).abs() < 0.05, "{user}");
        assert_eq!(
            layer.master,
            LayerType::AssociatedWithMaster(master(&font, "Regular").id.clone())
        );
        assert_eq!(layer.width, 550.0);
        let bbox = kurbo::Shape::bounding_box(&layer.paths().next().unwrap().to_kurbo().unwrap());
        assert_close(&[bbox.x1], &[450.0]);
        // parenleft has one too: fontTools shared the tuple, but two glyphs do
        // not make a master
        let others: Vec<&str> = font
            .glyphs
            .iter()
            .filter(|g| g.layers.iter().any(|l| l.location.is_some()))
            .map(|g| g.name.as_str())
            .collect();
        assert_eq!(others, ["brace", "parenleft"]);
        assert_eq!(font.masters.len(), 3);
    }
    #[test]
    fn test_metrics_vary_with_the_masters() {
        // MVAR varies the typo ascender, x-height and subscript size
        let font = load(VARIABLE).unwrap();
        for (name, ascender, x_height, subscript) in [
            ("Thin", 780, 480, 600),
            ("Regular", 800, 500, 650),
            ("Black", 840, 530, 700),
        ] {
            let metrics = &master(&font, name).metrics;
            assert_eq!(
                metrics.get(&MetricType::TypoAscender),
                Some(&ascender),
                "{name}"
            );
            assert_eq!(metrics.get(&MetricType::XHeight), Some(&x_height), "{name}");
            assert_eq!(
                metrics.get(&MetricType::SubscriptYSize),
                Some(&subscript),
                "{name}"
            );
        }
    }
    #[test]
    fn test_vertical_metrics_vary_with_the_masters() {
        // VVAR varies A's advance height; its origin stays at the ascender
        let font = load(VARIABLE).unwrap();
        for (name, height) in [("Thin", 1000.0), ("Regular", 1050.0), ("Black", 1100.0)] {
            let layer = layer(&font, "A", name);
            assert_eq!(layer.vertical_advance, Some(height), "{name}");
            assert_eq!(layer.vertical_origin, Some(900.0), "{name}");
        }
    }
    #[test]
    fn test_implied_only_where_every_master_implies_it() {
        // Q's top on-curve is a midpoint in Thin and Regular, not in Black
        let font = load(VARIABLE).unwrap();
        for name in ["Thin", "Regular", "Black"] {
            let o: usize = node_types(layer(&font, "O", name))
                .iter()
                .map(Vec::len)
                .sum();
            let q: usize = node_types(layer(&font, "Q", name))
                .iter()
                .map(Vec::len)
                .sum();
            assert_eq!((o, q), (7, 8), "{name}");
        }
    }
    /// Each kerned glyph pair's value at a master: a pair of glyphs, else of
    /// a glyph and a group, of a group and a glyph, then of groups
    fn effective_kerning(
        kerning: &IndexMap<(SmolStr, SmolStr), i16>,
        first_groups: &HashMap<SmolStr, Vec<SmolStr>>,
        second_groups: &HashMap<SmolStr, Vec<SmolStr>>,
    ) -> PairKerning {
        let group_of = |groups: &HashMap<SmolStr, Vec<SmolStr>>| {
            let mut of: HashMap<SmolStr, SmolStr> = HashMap::new();
            for (name, members) in groups {
                for glyph in members {
                    of.insert(glyph.clone(), format!("@{name}").into());
                }
            }
            of
        };
        let (first_of, second_of) = (group_of(first_groups), group_of(second_groups));
        let side = |key: &SmolStr, groups: &HashMap<SmolStr, Vec<SmolStr>>| -> Vec<SmolStr> {
            match key.strip_prefix('@') {
                Some(group) => groups.get(group).cloned().unwrap_or_default(),
                None => vec![key.clone()],
            }
        };
        let mut out = HashMap::new();
        for (left, right) in kerning.keys() {
            for l in side(left, first_groups) {
                for r in side(right, second_groups) {
                    let lg = first_of.get(&l).cloned().unwrap_or_default();
                    let rg = second_of.get(&r).cloned().unwrap_or_default();
                    let value = [(&l, &r), (&l, &rg), (&lg, &r), (&lg, &rg)]
                        .into_iter()
                        .find_map(|(a, b)| kerning.get(&(a.clone(), b.clone())));
                    if let Some(value) = value {
                        out.insert((l.clone(), r.clone()), *value);
                    }
                }
            }
        }
        out.retain(|_, v| *v != 0);
        out
    }

    /// A master's place: its normalized location, rounded as a binary stores it
    type Place = Vec<(String, i16)>;
    /// Kerning by glyph pair
    type PairKerning = HashMap<(SmolStr, SmolStr), i16>;

    fn master_place(font: &Font, master: &crate::Master) -> Place {
        let axes = font.fontdrasil_axes().unwrap();
        let mut place: Vec<(String, i16)> = master
            .location
            .to_normalized(&axes)
            .unwrap()
            .iter()
            .map(|(tag, coord)| (tag.to_string(), coord.to_f2dot14().to_bits()))
            .collect();
        place.sort();
        place
    }

    /// A Glyphs source's kerning at each master, by its place, as glyphsLib
    /// and fontc's Glyphs reader have it (`derive_kern_groups`,
    /// `get_glyphs_with_rtl_kerning`): a glyph in right-to-left kerning kerns
    /// first by its left group and second by its right group, and the
    /// right-to-left kerning (`@MMK_R_` first, `@MMK_L_` second) is added to
    /// the kerning
    #[cfg(feature = "glyphs")]
    fn glyphs_source_kerning(font: &Font) -> HashMap<Place, PairKerning> {
        use std::collections::BTreeMap;
        let group = |glyph: &crate::Glyph, key: &str| -> Option<SmolStr> {
            let group = glyph.format_specific.get_string(key);
            (!group.is_empty()).then(|| group.into())
        };
        let rtl: BTreeMap<String, BTreeMap<String, BTreeMap<String, f64>>> = font
            .format_specific
            .get_parse_or("com.schriftgestalt.Glyphs.kerningRTL", BTreeMap::new());
        // the glyphs in right-to-left kerning: named, or in a group named
        let mut rtl_glyphs: HashSet<SmolStr> = HashSet::new();
        let mut rtl_right: HashSet<SmolStr> = HashSet::new();
        let mut rtl_left: HashSet<SmolStr> = HashSet::new();
        for (left, rights) in rtl.values().flatten() {
            for key in std::iter::once(left).chain(rights.keys()) {
                if let Some(g) = key.strip_prefix("@MMK_L_") {
                    rtl_right.insert(g.into());
                } else if let Some(g) = key.strip_prefix("@MMK_R_") {
                    rtl_left.insert(g.into());
                } else {
                    rtl_glyphs.insert(key.into());
                }
            }
        }
        for glyph in font.glyphs.iter() {
            if group(glyph, "kern_right").is_some_and(|g| rtl_right.contains(&g))
                || group(glyph, "kern_left").is_some_and(|g| rtl_left.contains(&g))
            {
                rtl_glyphs.insert(glyph.name.clone());
            }
        }
        let mut first: HashMap<SmolStr, Vec<SmolStr>> = HashMap::new();
        let mut second: HashMap<SmolStr, Vec<SmolStr>> = HashMap::new();
        for glyph in font.glyphs.iter() {
            let (side1, side2) = if rtl_glyphs.contains(&glyph.name) {
                (group(glyph, "kern_left"), group(glyph, "kern_right"))
            } else {
                (group(glyph, "kern_right"), group(glyph, "kern_left"))
            };
            if let Some(g) = side1 {
                first.entry(g).or_default().push(glyph.name.clone());
            }
            if let Some(g) = side2 {
                second.entry(g).or_default().push(glyph.name.clone());
            }
        }
        font.masters
            .iter()
            .map(|master| {
                let mut kerning = master.kerning.clone();
                for (left, rights) in rtl.get(&master.id).into_iter().flatten() {
                    for (right, value) in rights {
                        let key = |k: &str, prefix: &str| -> SmolStr {
                            k.strip_prefix(prefix)
                                .map(|g| format!("@{g}").into())
                                .unwrap_or_else(|| k.into())
                        };
                        kerning.insert(
                            (key(left, "@MMK_R_"), key(right, "@MMK_L_")),
                            value.round() as i16,
                        );
                    }
                }
                let out = effective_kerning(&kerning, &first, &second);
                (master_place(font, master), out)
            })
            .collect()
    }

    #[test]
    #[cfg(feature = "fontir")]
    fn test_kerning_at_an_intermediate_master_comes_back() {
        // A pair whose kerning at an intermediate master is the default's
        // has no delta there: read back, it is still interpolated between all
        // of the kerning masters, as for the pairs that have one (Big
        // Shoulders)
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        use crate::{Axis, Glyph, Layer, LayerType, Master};
        use fontdrasil::coords::{DesignCoord, DesignLocation, UserCoord};
        let mut font = Font::new();
        let mut axis = Axis::new("Weight", Tag::new(b"wght"));
        axis.min = Some(UserCoord::new(100.0));
        axis.default = Some(UserCoord::new(400.0));
        axis.max = Some(UserCoord::new(900.0));
        font.axes.push(axis);
        let masters = [
            ("light", 100.0),
            ("semilight", 250.0),
            ("regular", 400.0),
            ("bold", 900.0),
        ];
        for (name, cp) in [("A", 0x41), ("V", 0x56), ("O", 0x4F)] {
            let mut glyph = Glyph::new(name);
            glyph.codepoints = vec![cp];
            glyph.exported = true;
            for (id, _) in masters {
                let mut layer = Layer::new(500.0);
                layer.master = LayerType::DefaultForMaster(id.to_string());
                glyph.layers.push(layer);
            }
            font.glyphs.push(glyph);
        }
        // A V has no delta at semilight, A O has one
        let kerning = [(-10, 0, 0, -20), (-10, -6, 0, -20)];
        for (i, (id, weight)) in masters.iter().enumerate() {
            let location =
                DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(*weight))]);
            let mut master = Master::new(*id, *id, location);
            let at = |k: &(i16, i16, i16, i16)| [k.0, k.1, k.2, k.3][i];
            master
                .kerning
                .insert(("A".into(), "V".into()), at(&kerning[0]));
            master
                .kerning
                .insert(("A".into(), "O".into()), at(&kerning[1]));
            font.masters.push(master);
        }
        let binary =
            BabelfontIrSource::compile(font.clone(), CompilationOptions::default()).unwrap();
        let read = load_bytes(&binary).unwrap();
        for (id, weight) in masters {
            let master = read
                .masters
                .iter()
                .find(|m| m.location.get(Tag::new(b"wght")).map(|c| c.to_f64()) == Some(weight))
                .unwrap_or_else(|| panic!("no master at {weight}"));
            let source = font.masters.iter().find(|m| m.id == id).unwrap();
            for pair in [("A", "V"), ("A", "O")] {
                let key = (pair.0.into(), pair.1.into());
                assert_eq!(
                    master.kerning.get(&key),
                    source.kerning.get(&key),
                    "{pair:?} at {id}"
                );
            }
        }
    }

    #[test]
    #[cfg(all(feature = "fontir", feature = "glyphs"))]
    fn test_compiled_glyphs_kerning_comes_back() {
        use std::collections::BTreeSet;
        // A source compiled by fontc, read back: every glyph pair kerns as the
        // source kerns it, at every master, the kerning is all kerning, and
        // every group the source kerns with comes back whole.
        // Fustat's kerning is split by script into lookups that kern the same
        // first glyphs, has a class of one glyph with an exception (@f:f 15,
        // @f:@f 22), right-to-left kerning, and values varying by master
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        // Nunito's kerning varies on two axes, some pairs' values only on one:
        // each is read back at every kerning location
        for path in [
            "resources/Fustat.glyphs",
            "resources/G3RTLKerning.glyphs",
            "resources/Nunito3.glyphs",
        ] {
            let source = crate::load(path).unwrap();
            let expected = glyphs_source_kerning(&source);
            // the groups the source kerns another group with, by their
            // members (a compiler writes a group's pairs with glyphs as
            // glyph pairs)
            let kerned_groups = |first: bool| -> BTreeSet<BTreeSet<SmolStr>> {
                let groups = if first {
                    &source.first_kern_groups
                } else {
                    &source.second_kern_groups
                };
                source
                    .masters
                    .iter()
                    .flat_map(|m| m.kerning.iter())
                    .filter(|((l, r), value)| {
                        **value != 0 && l.starts_with('@') && r.starts_with('@')
                    })
                    .filter_map(|((l, r), _)| if first { l } else { r }.strip_prefix('@'))
                    .filter_map(|name| groups.get(name))
                    .map(|members| members.iter().cloned().collect())
                    .collect()
            };
            let (first_groups, second_groups) = (kerned_groups(true), kerned_groups(false));
            let options = CompilationOptions {
                dont_use_production_names: true,
                ..Default::default()
            };
            let binary = BabelfontIrSource::compile(source, options).unwrap();
            let font = load_bytes(&binary).unwrap();
            // the kern feature refers to no lookup: a compiler writes it all
            for (tag, code) in &font.features.features {
                assert!(
                    tag != "kern" || !code.code.contains("lookup "),
                    "{path}: kerning stays feature code:\n{}",
                    code.code
                );
            }
            let first: HashMap<SmolStr, Vec<SmolStr>> = font
                .first_kern_groups
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            let second: HashMap<SmolStr, Vec<SmolStr>> = font
                .second_kern_groups
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            // each group comes back whole, though a compiler split it by script
            for (wanted, got) in [(&first_groups, &first), (&second_groups, &second)] {
                let got: BTreeSet<BTreeSet<SmolStr>> = got
                    .values()
                    .map(|members| members.iter().cloned().collect())
                    .collect();
                let missing: Vec<_> = wanted.iter().filter(|g| !got.contains(*g)).collect();
                assert!(
                    missing.is_empty(),
                    "{path}: groups not back whole: {missing:?}"
                );
            }
            for master in &font.masters {
                let name = master.name.get_default().cloned().unwrap_or_default();
                let Some(want) = expected.get(&master_place(&font, master)) else {
                    panic!("{path}: no master where {name} is in the source");
                };
                let got = effective_kerning(&master.kerning, &first, &second);
                let mut differ: Vec<_> = want
                    .keys()
                    .chain(got.keys())
                    .filter(|k| want.get(*k) != got.get(*k))
                    .map(|k| (k.clone(), want.get(k), got.get(k)))
                    .collect();
                differ.sort();
                differ.dedup();
                assert!(
                    differ.is_empty(),
                    "{path} {name}: {} pairs differ, e.g. {:?}",
                    differ.len(),
                    &differ[..differ.len().min(5)]
                );
            }
        }
    }

    #[test]
    fn test_gpos_kerning_is_kerning() {
        let font = load(SHAPING_VARIABLE).unwrap();
        for (name, pair, class) in [
            ("wght=100", -60, -10),
            ("Default", -40, -20),
            ("wght=900", -120, -30),
        ] {
            let m = master(&font, name);
            assert_eq!(kern_value(&font, m, "A", "V"), pair, "{name}");
            assert_eq!(kern_value(&font, m, "A", "O"), class, "{name}");
            assert_eq!(kern_value(&font, m, "V", "O"), class, "{name}");
        }
        let fea = font.features.to_fea();
        assert!(!fea.contains("pos A V"), "{fea}");
    }

    #[test]
    fn test_ligature_carets_and_attachment_points_round_trip() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let font = load("resources/ttf/GdefCarets.ttf").unwrap();
        let fea = font.features.to_fea();
        assert!(fea.contains("LigatureCaretByPos f_f 400;"), "{fea}");
        assert!(fea.contains("Attach f 1 2;"), "{fea}");
        // the glyph classes are the glyphs' categories, not feature code
        assert!(!fea.contains("GlyphClassDef"), "{fea}");
        let bytes = BabelfontIrSource::compile(font, CompilationOptions::default()).unwrap();
        let fontref = skrifa::FontRef::new(&bytes).unwrap();
        let gdef = fontref.gdef().unwrap();
        let carets = gdef.lig_caret_list().unwrap().unwrap();
        assert_eq!(carets.lig_glyph_count(), 1);
        let attach = gdef.attach_list().unwrap().unwrap();
        assert_eq!(attach.glyph_count(), 1);
        let classes = gdef.glyph_class_def().unwrap().unwrap();
        assert_eq!(classes.get(skrifa::GlyphId16::new(2)), 2);
    }

    #[test]
    fn test_varying_ligature_carets_are_anchors() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        // sr-aef's fixture: a caret of a varying at 100, 150 and 200, and one
        // at 300 (a feature file states no varying caret)
        let font = load("../sr-aef/resources/varcarets.ttf").unwrap();
        assert!(!font.features.to_fea().contains("LigatureCaret"));
        let carets = |font: &Font| -> Vec<Vec<(String, f64)>> {
            font.glyphs
                .get("a")
                .unwrap()
                .layers
                .iter()
                .map(|l| {
                    l.anchors
                        .iter()
                        .filter(|a| a.name.starts_with("caret_"))
                        .map(|a| (a.name.clone(), a.x))
                        .collect()
                })
                .collect()
        };
        let before = carets(&font);
        assert!(
            before
                .iter()
                .any(|c| c == &[("caret_1".into(), 150.0), ("caret_2".into(), 300.0)])
        );
        let bytes = BabelfontIrSource::compile(font, CompilationOptions::default()).unwrap();
        assert_eq!(carets(&load_bytes(&bytes).unwrap()), before);
    }

    #[test]
    fn test_kerning_across_marks_stays_feature_code() {
        // the lookup has no IgnoreMarks, and the font has marks: a compiler
        // kerning from the kerning table would kern across them
        let font = load(VARIABLE).unwrap();
        assert!(font.masters.iter().all(|m| m.kerning.is_empty()));
        let fea = font.features.to_fea();
        assert!(fea.contains("pos A Adot"), "{fea}");
        assert!(fea.contains("pos Q' lookup"), "{fea}");
    }
    /// Outlines and advances of two fonts, drawn by skrifa at a location
    /// Every point a glyph is drawn with, in order
    #[derive(Default)]
    struct PointsPen(Vec<(f64, f64)>);

    impl skrifa::outline::OutlinePen for PointsPen {
        fn move_to(&mut self, x: f32, y: f32) {
            self.0.push((x as f64, y as f64));
        }
        fn line_to(&mut self, x: f32, y: f32) {
            self.0.push((x as f64, y as f64));
        }
        fn quad_to(&mut self, cx0: f32, cy0: f32, x: f32, y: f32) {
            self.0
                .extend([(cx0 as f64, cy0 as f64), (x as f64, y as f64)]);
        }
        fn curve_to(&mut self, cx0: f32, cy0: f32, cx1: f32, cy1: f32, x: f32, y: f32) {
            self.0.extend([
                (cx0 as f64, cy0 as f64),
                (cx1 as f64, cy1 as f64),
                (x as f64, y as f64),
            ]);
        }
        fn close(&mut self) {}
    }

    fn drawn_at(binary: &[u8], glyph: &str, coords: &[F2Dot14]) -> (Vec<(f64, f64)>, f32) {
        let fontref = skrifa::FontRef::new(binary).unwrap();
        let names = sr_aef::unique_glyph_names(&fontref);
        let gid = GlyphId::new(names.iter().position(|n| n == glyph).unwrap() as u32);
        let location = LocationRef::new(coords);
        // at the font's own size in pixels, which keeps the fractions an
        // unscaled draw rounds away at every component
        let upem = fontref.head().unwrap().units_per_em() as f32;
        let mut pen = PointsPen::default();
        fontref
            .outline_glyphs()
            .get(gid)
            .unwrap()
            .draw(DrawSettings::unhinted(Size::new(upem), location), &mut pen)
            .unwrap();
        let points = pen.0;
        let advance = fontref
            .glyph_metrics(Size::unscaled(), location)
            .advance_width(gid)
            .unwrap();
        (points, advance)
    }

    /// Each contour's bounding box, sorted
    fn contour_boxes(paths: &[crate::Path]) -> Vec<[f64; 4]> {
        let mut boxes: Vec<[f64; 4]> = paths
            .iter()
            .map(|p| {
                let b = kurbo::Shape::bounding_box(&p.to_kurbo().unwrap());
                [b.x0, b.y0, b.x1, b.y1]
            })
            .collect();
        boxes.sort_by(|a, b| a.partial_cmp(b).unwrap());
        boxes
    }

    /// Every layer of every glyph, masters and intermediate layers, its
    /// components decomposed at the layer's location, is the glyph as skrifa
    /// draws it there
    fn assert_layers_read_as_drawn(path: &str) {
        let binary = std::fs::read(path).unwrap();
        let fontref = skrifa::FontRef::new(&binary).unwrap();
        let font = load_bytes(&binary).unwrap();
        let mut layers_checked = 0;
        for (gid, glyph) in font.glyphs.iter().enumerate() {
            for layer in &glyph.layers {
                let location = match (&layer.location, &layer.master) {
                    (Some(location), _) => location.clone(),
                    (None, LayerType::DefaultForMaster(id)) => font
                        .masters
                        .iter()
                        .find(|m| &m.id == id)
                        .unwrap()
                        .location
                        .clone(),
                    _ => continue,
                };
                let coords =
                    fontdrasil_location_to_skrifa_location(location.clone(), &font.axes).unwrap();
                let drawn = draw_contours(
                    &fontref,
                    GlyphId::new(gid as u32),
                    LocationRef::new(&coords),
                )
                .unwrap();
                let mut paths: Vec<crate::Path> = layer.paths().cloned().collect();
                for component in layer.components() {
                    paths.extend(component.decompose(None, |name, _| {
                        let layer = font.interpolate_glyph(name, &location).ok()?;
                        Some(crate::shape::ResolvedShapes {
                            paths: layer.paths().cloned().collect(),
                            components: layer.components().cloned().collect(),
                        })
                    }));
                }
                let (read, drawn) = (contour_boxes(&paths), contour_boxes(&drawn));
                assert_eq!(read.len(), drawn.len(), "{} at {location:?}", glyph.name);
                for (a, b) in read.iter().zip(&drawn) {
                    assert!(
                        a.iter().zip(b).all(|(x, y)| (x - y).abs() <= 0.5),
                        "{} at {location:?}: {a:?} != {b:?}",
                        glyph.name
                    );
                }
                layers_checked += 1;
            }
        }
        assert!(
            layers_checked > font.glyphs.len(),
            "{path}: {layers_checked}"
        );
    }

    #[test]
    fn test_variable_layers_read_as_drawn() {
        for path in [
            VARIABLE,
            VARIABLE_NO_HVAR,
            AVAR2,
            COMPOSITES,
            REDDIT_SANS,
            COINCIDENT,
            INTER,
        ] {
            assert_layers_read_as_drawn(path);
        }
    }

    /// Normalized locations: every axis on its own at every step, and a grid
    /// over all axes, as fine as keeps it to 41 x 41 locations (every step
    /// for up to two axes)
    fn locations(axis_count: usize, steps: i32) -> Vec<Vec<F2Dot14>> {
        let along = |k: i32| -> Vec<F2Dot14> {
            (-k..=k)
                .map(|s| F2Dot14::from_f32(s as f32 / k as f32))
                .collect()
        };
        let mut k = steps;
        while k > 1
            && (2 * k as usize + 1)
                .checked_pow(axis_count as u32)
                .is_none_or(|n| n > 41 * 41)
        {
            k -= 1;
        }
        let mut grid: Vec<Vec<F2Dot14>> = vec![vec![]];
        for _ in 0..axis_count {
            grid = grid
                .into_iter()
                .flat_map(|location| {
                    along(k).into_iter().map(move |c| {
                        let mut location = location.clone();
                        location.push(c);
                        location
                    })
                })
                .collect();
        }
        for axis in 0..axis_count {
            for c in along(steps) {
                let mut location = vec![F2Dot14::ZERO; axis_count];
                location[axis] = c;
                if !grid.contains(&location) {
                    grid.push(location);
                }
            }
        }
        grid
    }

    #[test]
    fn test_locations_cover_every_axis() {
        assert_eq!(locations(1, 20).len(), 41);
        assert_eq!(locations(2, 20).len(), 41 * 41);
        let three = locations(3, 20);
        // an 11 x 11 x 11 grid, and the 30 further steps along each axis
        assert_eq!(three.len(), 11 * 11 * 11 + 3 * 30);
        assert!(three.iter().all(|l| l.len() == 3));
        let one = F2Dot14::ONE;
        assert!(three.contains(&vec![one, -one, one]));
        assert!(three.contains(&vec![F2Dot14::ZERO, F2Dot14::ZERO, F2Dot14::from_f32(0.05)]));
    }

    /// The recompiled font draws every glyph, and advances it, as the original
    /// does: at the masters and everywhere between them
    #[cfg(feature = "fontir")]
    fn assert_interpolates_as_original(path: &str, steps: i32) {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let original = std::fs::read(path).unwrap();
        let font = load_bytes(&original).unwrap();
        let options = CompilationOptions {
            dont_use_production_names: true,
            ..Default::default()
        };
        let compiled = BabelfontIrSource::compile(font.clone(), options).unwrap();
        for coords in locations(font.axes.len(), steps) {
            for glyph in font.glyphs.iter().skip(1) {
                let (points, advance) = drawn_at(&original, &glyph.name, &coords);
                let (again, again_advance) = drawn_at(&compiled, &glyph.name, &coords);
                assert!(
                    (advance - again_advance).abs() <= 1.0,
                    "{path} {} advance at {coords:?}: {advance} != {again_advance}",
                    glyph.name
                );
                assert_eq!(
                    points.len(),
                    again.len(),
                    "{path} {} at {coords:?}",
                    glyph.name
                );
                // The masters read are rounded to whole units, and a compiler
                // leaves out deltas it can infer to within half a unit; a
                // rasterizer also rounds each component's offset, in each
                // font. So a point may be up to a unit and a half from where
                // the original draws it; a wrong outline or component is off
                // by far more.
                for (a, b) in points.iter().zip(&again) {
                    assert!(
                        (a.0 - b.0).abs() <= 1.5 && (a.1 - b.1).abs() <= 1.5,
                        "{path} {} at {coords:?}: {a:?} != {b:?}",
                        glyph.name
                    );
                }
            }
        }
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_variable_fonts_round_trip_between_masters() {
        assert_interpolates_as_original(VARIABLE, 20);
        assert_interpolates_as_original(VARIABLE_NO_HVAR, 20);
        assert_interpolates_as_original(AVAR2, 8);
        assert_interpolates_as_original(COMPOSITES, 8);
        assert_interpolates_as_original(REDDIT_SANS, 20);
        assert_interpolates_as_original(COINCIDENT, 20);
        assert_interpolates_as_original(INTER, 10);
    }

    #[test]
    fn test_composite_intermediate_masters() {
        let font = load(COMPOSITES).unwrap();
        let intermediates = |glyph: &str| -> Vec<&Layer> {
            font.glyphs
                .get(glyph)
                .unwrap()
                .layers
                .iter()
                .filter(|l| l.location.is_some())
                .collect()
        };
        // Acomp's own master in the middle of the design space, both axes
        let acomp = intermediates("Acomp");
        assert_eq!(acomp.len(), 1);
        let location = acomp[0].location.as_ref().unwrap();
        let user = |tag: &[u8; 4]| {
            let axis = font.axes.iter().find(|a| a.tag == Tag::new(tag)).unwrap();
            let design = location.get(axis.tag).unwrap();
            (design.to_user(&axis._converter().unwrap()).to_f64() * 10.0).round() / 10.0
        };
        assert_eq!((user(b"wght"), user(b"wdth")), (650.0, 87.5));
        // the accent there is off the line between the masters around it
        let accent = acomp[0].components().nth(1).unwrap();
        let on_the_line = font.interpolate_glyph("Acomp", location);
        assert_eq!(accent.reference, "acutecomb");
        let interpolated_without = {
            let mut without = font.clone();
            without
                .glyphs
                .get_mut("Acomp")
                .unwrap()
                .layers
                .retain(|l| l.location.is_none());
            without.interpolate_glyph("Acomp", location).unwrap()
        };
        let x = |layer: &Layer| {
            layer
                .components()
                .nth(1)
                .unwrap()
                .transform
                .as_affine()
                .as_coeffs()[4]
        };
        assert!((x(acomp[0]) - x(&interpolated_without)).abs() > 30.0);
        assert!(on_the_line.is_ok());
        assert_eq!(intermediates("B").len(), 1);
        // nothing else has a master of its own
        let others: Vec<&str> = font
            .glyphs
            .iter()
            .filter(|g| g.layers.iter().any(|l| l.location.is_some()))
            .map(|g| g.name.as_str())
            .filter(|n| !["Acomp", "B", "A", "acutecomb"].contains(n))
            .collect();
        assert!(others.is_empty(), "{others:?}");
        assert_eq!(font.masters.len(), 6);
    }

    #[test]
    fn test_point_matched_component_varies() {
        // Odot's dot sits on O's point 2, which moves with the masters
        let font = load(COMPOSITES).unwrap();
        let offsets: Vec<f64> = font
            .masters
            .iter()
            .map(|m| {
                let layer = font
                    .glyphs
                    .get("Odot")
                    .unwrap()
                    .layers
                    .iter()
                    .find(|l| l.master == LayerType::DefaultForMaster(m.id.clone()))
                    .unwrap();
                layer
                    .components()
                    .nth(1)
                    .unwrap()
                    .transform
                    .as_affine()
                    .as_coeffs()[4]
            })
            .collect();
        let distinct: HashSet<i64> = offsets.iter().map(|x| x.round() as i64).collect();
        assert!(distinct.len() > 2, "{offsets:?}");
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_variable_font_round_trips() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let font = load(VARIABLE).unwrap();
        // the variable anchors state the mark feature: the compiler builds it
        assert!(!font.features.to_fea().contains("pos base"));
        let bytes =
            BabelfontIrSource::compile(font.clone(), CompilationOptions::default()).unwrap();
        let again = load_bytes(&bytes).unwrap();
        assert_eq!(again.masters.len(), font.masters.len());
        for m in &font.masters {
            let name = m.name.get_default().unwrap();
            let other = master(&again, name);
            assert_eq!(other.location, m.location, "{name}");
            for glyph in font.glyphs.iter() {
                let (a, b) = (
                    layer(&font, &glyph.name, name),
                    layer(&again, &glyph.name, name),
                );
                assert_eq!(a.width, b.width, "{} {name}", glyph.name);
                assert_eq!(
                    a.paths().collect::<Vec<_>>(),
                    b.paths().collect::<Vec<_>>(),
                    "{} {name}",
                    glyph.name
                );
                let components = |l: &Layer| {
                    l.components()
                        .map(|c| (c.reference.clone(), c.transform.as_affine().as_coeffs()))
                        .collect::<Vec<_>>()
                };
                assert_eq!(components(a), components(b), "{} {name}", glyph.name);
                let anchors = |l: &Layer| {
                    let mut anchors: Vec<_> = l
                        .anchors
                        .iter()
                        .map(|a| (a.name.clone(), a.x, a.y))
                        .collect();
                    anchors.sort_by(|a, b| a.0.cmp(&b.0));
                    anchors
                };
                assert_eq!(anchors(a), anchors(b), "{} {name}", glyph.name);
            }
            for (l, r) in [("A", "Adot"), ("A", "O"), ("Adot", "Q"), ("O", "A")] {
                assert_eq!(
                    kern_value(&again, other, l, r),
                    kern_value(&font, m, l, r),
                    "{l} {r} {name}"
                );
            }
        }
        assert_eq!(again.first_kern_groups, font.first_kern_groups);
        assert_eq!(again.second_kern_groups, font.second_kern_groups);
        let brace = again.glyphs.get("brace").unwrap();
        assert_eq!(
            brace.layers.iter().filter(|l| l.location.is_some()).count(),
            1
        );
        assert!(again.features.to_fea().contains("pos Q' lookup"));
    }
}

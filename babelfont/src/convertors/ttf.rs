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
        tables::{glyf, gpos::PositionSubtables, kern},
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
    LayerType, MetricType, PathBuilder, Tag, features::PossiblyAutomaticCode,
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
    font.upm = fontref.head()?.units_per_em();
    font.version = font_version(&fontref)?;

    load_axes(&fontref, &mut font)?;
    load_axis_mappings(&fontref, &mut font)?;
    load_names(&fontref, &mut font)?;
    load_custom_ot_values(&fontref, &mut font)?;
    load_instances(&fontref, &mut font)?;
    load_masters(&fontref, &mut font)?;
    load_glyphs(&fontref, &mut font)?;
    load_features(&fontref, &mut font)?;
    load_legacy_kerning(&fontref, &mut font)?;
    name_unnamed_glyphs(&fontref, &mut font)?;
    name_placeholder_anchors(&mut font);
    {
        use crate::filters::FontFilter;
        crate::filters::DropImpliedOnCurves.apply(&mut font)?;
        crate::filters::MoveKerningFromFeatures.apply(&mut font)?;
    }
    Ok(font)
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

/// Whether two anchor families may share a name: no glyph is in both, and
/// their bases write different scripts, each known
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
        match (scripts(bases), scripts(other_bases)) {
            (Some(a), Some(b)) => !a.is_empty() && !b.is_empty() && a.is_disjoint(&b),
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
fn name_placeholder_anchors(font: &mut Font) {
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
        .filter(|name| name.starts_with(PLACEHOLDER_ANCHOR_PREFIX))
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
                let from = NormalizedCoord::new(map.from_coordinate().to_f32() as f64);
                let to = NormalizedCoord::new(map.to_coordinate().to_f32() as f64);
                map_vec.push((from.to_user(&converter), to.to_design(&converter)));
            }
            axis.map = Some(map_vec);
        }
    }
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
    Ok(())
}

/// The fsSelection bits a source states; a compiler sets ITALIC, BOLD and
/// REGULAR from the style.
const SOURCE_FS_SELECTION_BITS: u16 = !(1 << 0 | 1 << 5 | 1 << 6);

fn load_custom_ot_values(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    let os2 = fontref.os2()?;
    let ot = &mut font.custom_ot_values;
    ot.os2_vendor_id = Some(os2.ach_vend_id());
    ot.os2_fs_type = Some(os2.fs_type());
    ot.os2_family_class = Some(os2.s_family_class() as u16);
    ot.os2_panose = os2.panose_10().try_into().ok();
    ot.os2_fs_selection = Some(os2.fs_selection().bits() & SOURCE_FS_SELECTION_BITS);
    ot.head_lowest_rec_ppem = Some(fontref.head()?.lowest_rec_ppem());
    // A variable font's weight and width classes describe its default instance only
    if font.axes.is_empty() {
        ot.os2_us_weight_class = Some(os2.us_weight_class());
        ot.os2_us_width_class = Some(os2.us_width_class());
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
        let coordinates = instance.location();
        let location: Vec<(crate::Tag, DesignCoord)> = font
            .axes
            .iter()
            .zip(coordinates.coords().iter())
            .flat_map(|(axis, coord)| {
                let normalized = NormalizedCoord::new(coord.to_f32() as f64);
                let design_coord = normalized.to_design(&axis._converter().ok()?);
                Some((axis.tag, design_coord))
            })
            .collect();
        font.instances.push(Instance {
            id: id.to_string(),
            name,
            location: location.into(),
            ..Default::default()
        })
    }
    Ok(())
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
            .flat_map(|(coord, axis)| {
                let normalized = NormalizedCoord::new(coord.to_f32() as f64);
                let design_coord = normalized.to_design(&axis._converter().ok()?);
                Some((axis.tag, design_coord))
            })
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
    masters: &[crate::Master],
    gid: GlyphId,
) -> Result<Vec<DesignLocation>, BabelfontError> {
    let Ok(gvar) = fontref.gvar() else {
        return Ok(vec![]);
    };
    let Some(data) = gvar.glyph_variation_data(gid)? else {
        return Ok(vec![]);
    };
    let at_master = |coords: &[F2Dot14]| {
        masters.iter().any(|master| {
            fontdrasil_location_to_skrifa_location(master.location.clone(), axes)
                .is_ok_and(|master_coords| master_coords == coords)
        })
    };
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
    macro_rules! copy_os2_metric {
        ($metric_type:ident, $method:ident, $tag:literal) => {
            metrics.insert(
                MetricType::$metric_type,
                (fontref.os2()?.$method() as f64 + mvar_delta($tag)).round() as i32,
            );
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
    }

    // post stores the angle negative for a right lean; babelfont stores the
    // Glyphs convention, the opposite sign.
    metrics.insert(
        MetricType::ItalicAngle,
        -fontref.post()?.italic_angle().to_f32() as i32,
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
    for (gid, glyph) in font.glyphs.iter_mut().enumerate() {
        let gid = GlyphId::new(gid as u32);
        for master in &font.masters {
            glyph.layers.push(load_layer(
                &font.axes,
                fontref,
                gid,
                &master.location,
                crate::LayerType::DefaultForMaster(master.id.clone()),
                &names,
            )?);
        }
        let Some(default_master_id) = &default_master_id else {
            continue;
        };
        for location in intermediate_locations(fontref, &font.axes, &font.masters, gid)? {
            let mut layer = load_layer(
                &font.axes,
                fontref,
                gid,
                &location,
                crate::LayerType::AssociatedWithMaster(default_master_id.clone()),
                &names,
            )?;
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
        let glyph = font
            .glyphs
            .get_by_index_mut(glyphid.to_u32() as usize)
            .ok_or(BabelfontError::BinaryFontRead(format!(
                "Glyph ID {} from charmap does not exist in the font",
                glyphid.to_u32()
            )))?;
        glyph.codepoints.push(unicode);
    }
    load_categories(fontref, font)
}

/// Each glyph's category from GDEF's glyph classes, so that a compiler does not
/// guess one from its name; a mark with an advance is Spacing Combining. Without
/// glyph classes every glyph is a base, so that the compiled font has none either
/// and a shaper guesses them as it does for the binary. But a font that attaches
/// marks gets glyph classes from a compiler anyway, so there a glyph a shaper
/// takes for a mark is one.
fn load_categories(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    let class_def = match fontref.gdef() {
        Ok(gdef) => gdef.glyph_class_def().transpose()?,
        Err(_) => None,
    };
    let marks_by_codepoint = class_def.is_none() && attaches_marks(fontref)?;
    let metrics = fontref.glyph_metrics(Size::unscaled(), LocationRef::default());
    for (gid, glyph) in font.glyphs.iter_mut().enumerate() {
        let gid = GlyphId::new(gid as u32);
        let class = match &class_def {
            Some(class_def) => class_def.get(gid),
            None if marks_by_codepoint && shaper_mark(glyph) => 3,
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

/// Whether the font's GPOS attaches marks (to bases, ligatures or marks).
fn attaches_marks(fontref: &skrifa::FontRef) -> Result<bool, BabelfontError> {
    let Ok(gpos) = fontref.gpos() else {
        return Ok(false);
    };
    for lookup in gpos.lookup_list()?.lookups().iter() {
        if matches!(
            lookup?.subtables()?,
            PositionSubtables::MarkToBase(_)
                | PositionSubtables::MarkToLig(_)
                | PositionSubtables::MarkToMark(_)
        ) {
            return Ok(true);
        }
    }
    Ok(false)
}

// This is only sound for mark class definitions
fn load_features(fontref: &skrifa::FontRef, font: &mut Font) -> Result<(), BabelfontError> {
    let axes = font.fontdrasil_axes()?;
    let uncompile_context = sr_aef::uncompile_context(fontref)
        .map_err(|e| BabelfontError::BinaryFontRead(e.to_string()))?;
    let master_ids = font
        .masters
        .iter()
        .map(|m| (m.id.clone(), m))
        .collect::<HashMap<_, _>>();
    let mut features = Features::default();
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
    for (script_tag, lang_sys_tags) in &uncompile_context.language_systems {
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
    let code = std::iter::once(language_systems.join("\n"))
        .chain(std::iter::once(mark_class_definitions))
        .chain(lookups)
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
    Ok(())
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

/// A contour and the point it starts at
type ContourWithStart = (crate::Path, (f64, f64));

/// A pen that notes where each contour starts
struct StartsPen {
    builder: PathBuilder,
    starts: Vec<(f64, f64)>,
}

impl skrifa::outline::OutlinePen for StartsPen {
    fn move_to(&mut self, x: f32, y: f32) {
        self.starts.push((x as f64, y as f64));
        skrifa::outline::OutlinePen::move_to(&mut self.builder, x, y);
    }
    fn line_to(&mut self, x: f32, y: f32) {
        skrifa::outline::OutlinePen::line_to(&mut self.builder, x, y);
    }
    fn quad_to(&mut self, cx0: f32, cy0: f32, x: f32, y: f32) {
        skrifa::outline::OutlinePen::quad_to(&mut self.builder, cx0, cy0, x, y);
    }
    fn curve_to(&mut self, cx0: f32, cy0: f32, cx1: f32, cy1: f32, x: f32, y: f32) {
        skrifa::outline::OutlinePen::curve_to(&mut self.builder, cx0, cy0, cx1, cy1, x, y);
    }
    fn close(&mut self) {
        skrifa::outline::OutlinePen::close(&mut self.builder);
    }
}

/// A glyph's contours as skrifa draws them, in glyf order and direction, with
/// the point each one starts at
fn draw_contours_with_starts(
    fontref: &skrifa::FontRef<'_>,
    gid: skrifa::GlyphId,
    location: LocationRef,
) -> Result<Vec<ContourWithStart>, BabelfontError> {
    let Some(outline) = fontref.outline_glyphs().get(gid) else {
        return Ok(vec![]);
    };
    let mut pen = StartsPen {
        builder: PathBuilder::new(),
        starts: vec![],
    };
    outline
        .draw(DrawSettings::unhinted(Size::unscaled(), location), &mut pen)
        .map_err(|e| BabelfontError::BinaryFontRead(e.to_string()))?;
    Ok(pen.builder.build().into_iter().zip(pen.starts).collect())
}

fn draw_contours(
    fontref: &skrifa::FontRef<'_>,
    gid: skrifa::GlyphId,
    location: LocationRef,
) -> Result<Vec<crate::Path>, BabelfontError> {
    Ok(draw_contours_with_starts(fontref, gid, location)?
        .into_iter()
        .map(|(path, _)| path)
        .collect())
}

/// Reverses a contour drawn from a glyf, keeping the point it starts at as its
/// start (the last node), as a compiler that reverses it back keeps it
fn reverse_keeping_start(path: &mut crate::Path, start: (f64, f64)) {
    path.reverse();
    if let Some(index) = path
        .nodes
        .iter()
        .position(|n| n.nodetype != crate::NodeType::OffCurve && (n.x, n.y) == start)
    {
        path.nodes.rotate_left(index + 1);
    }
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
        glyf::Glyph::Simple(_) => {
            // glyf contours run clockwise, a source's counter-clockwise
            layer.shapes = draw_contours_with_starts(fontref, gid, locationref)?
                .into_iter()
                .map(|(mut path, start)| {
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
            let shift = drawn_shift(fontref, &glyf, gid, 0)?;
            // Where skrifa draws each component's contours, with offset deltas
            // and point matching applied
            let drawn = draw_contours(fontref, gid, locationref)?;
            let mut contour_index = 0;
            for component in c.components() {
                let gid = component.glyph;
                let own = draw_contours(fontref, gid.into(), locationref)?;
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
                        let component_shift = read_shift(fontref, &glyf, gid.into())?;
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
        // USE_TYPO_METRICS; REGULAR is left to the compiler
        assert_eq!(ot.os2_fs_selection, Some(1 << 7));
        assert_eq!(ot.head_lowest_rec_ppem, Some(9));
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
    fn test_mark_classes_defined() {
        // every mark class a lookup names has a markClass statement
        let fea = load(GDEF_CLASSES).unwrap().features.to_fea();
        let used: Vec<&str> = fea
            .split("mark @")
            .skip(1)
            .map(|rest| {
                rest.split(|c: char| c == ';' || c.is_whitespace())
                    .next()
                    .unwrap()
            })
            .collect();
        assert!(!used.is_empty(), "{fea}");
        for class in used {
            assert!(fea.contains(&format!("> @{class};")), "@{class}: {fea}");
        }
    }
    #[test]
    fn test_feature_code_does_not_depend_on_prefix_order() {
        // the calt rule calls a lookup and the mark lookup uses a mark class:
        // everything they need is in the one prefix, before them
        let font = load(GDEF_CLASSES).unwrap();
        assert_eq!(font.features.prefixes.len(), 1);
        let code = &font.features.prefixes.get("Decompiled").unwrap().code;
        let at = |text: &str| code.find(text).unwrap_or_else(|| panic!("{text}: {code}"));
        assert!(at("languagesystem") < at("markClass"));
        assert!(at("markClass") < at("lookup gpos_mark_to_base"));
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
        let font = load(STATIC_LEGACY).unwrap();
        for glyph in ["gravecomb", "commaaccent", "x"] {
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
        assert_eq!(kerning.get(&("a".into(), "a".into())), Some(&-10));
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
        // an Arabic top: shares top with the Latin one, the scripts being apart
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
        name_placeholder_anchors(&mut font);
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
        // behar is beh-ar, loosely
        assert_eq!(names("behar"), ["top"]);
        assert_eq!(names("fatha-ar"), ["_top"]);
        let code = &font.features.prefixes.get("Decompiled").unwrap().code;
        assert!(
            code.contains("@center;") && code.contains("@mark_class_0x2;"),
            "{code}"
        );
        // a shared name leaves the classes apart in the feature code
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
    #[test]
    fn test_gpos_kerning_is_kerning() {
        let font = load(VARIABLE).unwrap();
        for (name, pair, class) in [
            ("Thin", -10, -40),
            ("Regular", -20, -50),
            ("Black", -30, -80),
        ] {
            let m = master(&font, name);
            assert_eq!(kern_value(&font, m, "A", "Adot"), pair, "{name}");
            assert_eq!(kern_value(&font, m, "A", "O"), class, "{name}");
            assert_eq!(kern_value(&font, m, "Adot", "Q"), class, "{name}");
        }
        let groups: Vec<&Vec<SmolStr>> = font.first_kern_groups.values().collect();
        assert_eq!(groups, [&vec![SmolStr::from("A"), "Adot".into()]]);
        // the contextual rule stays, with the lookup it calls
        let fea = font.features.to_fea();
        assert!(fea.contains("pos Q' lookup"), "{fea}");
        assert!(!fea.contains("pos A Adot"), "{fea}");
    }
    /// Outlines and advances of two fonts, drawn by skrifa at a location
    fn drawn_at(binary: &[u8], glyph: &str, coords: &[F2Dot14]) -> (Vec<(f64, f64)>, f32) {
        let fontref = skrifa::FontRef::new(binary).unwrap();
        let names = sr_aef::unique_glyph_names(&fontref);
        let gid = GlyphId::new(names.iter().position(|n| n == glyph).unwrap() as u32);
        let location = LocationRef::new(coords);
        let points = draw_contours(&fontref, gid, location)
            .unwrap()
            .iter()
            .flat_map(|p| p.nodes.iter().map(|n| (n.x, n.y)))
            .collect();
        let advance = fontref
            .glyph_metrics(Size::unscaled(), location)
            .advance_width(gid)
            .unwrap();
        (points, advance)
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_variable_font_round_trips_between_masters() {
        // the recompiled font interpolates as the original does everywhere,
        // not only at its masters
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let original = std::fs::read(VARIABLE).unwrap();
        let font = load_bytes(&original).unwrap();
        let compiled = BabelfontIrSource::compile(font, CompilationOptions::default()).unwrap();
        for step in -20..=20 {
            let coords = [F2Dot14::from_f32(step as f32 / 20.0)];
            for glyph in ["A", "Adot", "O", "Q", "brace", "parenleft", "acutecomb"] {
                let (points, advance) = drawn_at(&original, glyph, &coords);
                let (again, again_advance) = drawn_at(&compiled, glyph, &coords);
                assert_eq!(advance, again_advance, "{glyph} advance at {step}/20");
                assert_eq!(points.len(), again.len(), "{glyph} at {step}/20");
                // A rasterizer rounds an interpolated point; an on-curve point
                // that stays implied is the midpoint of rounded off-curves, so
                // it may be half a unit off where the source stored it.
                for (a, b) in points.iter().zip(&again) {
                    assert!(
                        (a.0 - b.0).abs() <= 0.5 && (a.1 - b.1).abs() <= 0.5,
                        "{glyph} at {step}/20: {a:?} != {b:?}"
                    );
                }
            }
        }
    }

    #[cfg(feature = "fontir")]
    #[test]
    fn test_variable_font_round_trips() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        let font = load(VARIABLE).unwrap();
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

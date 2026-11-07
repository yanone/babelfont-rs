use crate::{
    common::decomposition::DecomposedAffine, features::Features, glyph::GlyphCategory,
    BabelfontError, Component, Font, Glyph, Layer, LayerType, Master, MetricType, Node, Path,
    Shape,
};
use chrono::{DateTime, NaiveDateTime, TimeZone, Utc};
use fontdrasil::{coords::Location, types::Tag};
use indexmap::IndexMap;
use paste::paste;
use serde::{Deserialize, Serialize};
use smol_str::{SmolStr, ToSmolStr};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fs,
    path::{Path as FsPath, PathBuf},
    str::FromStr,
    sync::Mutex,
    time::SystemTime,
};

/// Key for storing norad lib data in FormatSpecific
pub const KEY_LIB: &str = "norad.lib";
/// Key for storing norad groups in FormatSpecific
pub const KEY_GROUPS: &str = "norad.groups";
// These aren't pub because we just use them to get stuff out of the ufo's lib, we don't store them ourselves
const KEY_CATEGORIES: &str = "public.openTypeCategories";
const KEY_PSNAMES: &str = "public.postscriptNames";
const KEY_SKIP_EXPORT: &str = "public.skipExportGlyphs";
// Format-specific names
/// Key for storing style map family name in FormatSpecific
pub const KEY_STYLE_MAP_FAMILY_NAME: &str = "ufo.styleMapFamilyName";
/// Key for storing style map style name in FormatSpecific
pub const KEY_STYLE_MAP_STYLE_NAME: &str = "ufo.styleMapStyleName";
/// Key for storing style name in FormatSpecific
pub const KEY_STYLE_NAME: &str = "ufo.styleName";
/// Key for storing identifier in FormatSpecific
pub const KEY_IDENTIFIER: &str = "ufo.identifier";
/// Key for storing original guide type in FormatSpecific, used to roundtrip horizontal and vertical guides which UFO can either represent as such, or as angled guides with specific angles. Values are "horizontal" or "vertical".
pub const KEY_ORIGINAL_GUIDE: &str = "ufo.originalGuide";
/// The glyph lib key holding the y coordinate of a glyph's vertical origin
pub const KEY_VERTICAL_ORIGIN: &str = "public.verticalOrigin";
/// Key for a UFO's feature files as authored, see [`FeatureSources`].
pub const KEY_FEATURE_SOURCES: &str = "norad.featureSources";
const KEY_GLYPH_ORDER: &str = "public.glyphOrder";

/// Key for the `fontinfo.plist` of the UFO a master comes from. Saving starts
/// from it, so that the fields babelfont does not model survive.
pub const KEY_FONTINFO: &str = "norad.fontinfo";

/// The id of a UFO's one master. It is fixed, so that loading a UFO again
/// gives its master and layers the same ids.
pub const UFO_MASTER_ID: &str = "ufo-master";

/// A UFO's `features.fea` and the files it `include()`s, as written.
///
/// Babelfont works on the flattened features. Saving writes these files back
/// unchanged, includes and all, as long as the font's features still flatten
/// to what was loaded; once they are edited, the flattened code is written.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct FeatureSources {
    root: String,
    #[serde(default)]
    includes: BTreeMap<String, String>,
    fingerprint: String,
}

/// A stable fingerprint of the flattened feature code (FNV-1a, 64 bit).
fn features_fingerprint(features: &Features) -> String {
    let mut hash: u64 = 0xcbf29ce484222325;
    for byte in features.to_fea().bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{hash:016x}")
}

/// A UFO's feature files as authored (see [`FeatureSources`]), fingerprinted
/// against `features`, the font's flattened features they belong with.
pub(crate) fn feature_sources_for(
    ufo: &norad::Font,
    features: &Features,
) -> Option<serde_json::Value> {
    if ufo.features.is_empty() && ufo.feature_files.is_empty() {
        return None;
    }
    serde_json::to_value(FeatureSources {
        root: ufo.features.clone(),
        includes: ufo
            .feature_files
            .iter()
            .map(|(path, text)| (path.to_string_lossy().replace('\\', "/"), text.clone()))
            .collect(),
        fingerprint: features_fingerprint(features),
    })
    .ok()
}

/// The directory and file names of an existing UFO, read from its
/// `layercontents.plist` and each layer's `contents.plist`, so that saving
/// over it keeps them instead of deriving new ones.
#[derive(Debug, Default, Clone)]
pub struct UfoLayout {
    /// Layer name and directory, in `layercontents.plist` order.
    layers: Vec<(String, String)>,
    /// Glyph name to `.glif` file name, per layer directory.
    files: HashMap<String, BTreeMap<String, String>>,
}

impl UfoLayout {
    /// Reads the layout from UFO file entries keyed by path relative to the
    /// UFO. Only `layercontents.plist` and the layers' `contents.plist` are
    /// used; without a `layercontents.plist` the layout is empty.
    pub fn from_entries(entries: &HashMap<String, String>) -> Result<Self, BabelfontError> {
        let Some(layer_contents) = entries.get("layercontents.plist") else {
            return Ok(Self::default());
        };
        let layers: Vec<(String, String)> = plist::from_bytes(layer_contents.as_bytes())
            .map_err(|e| BabelfontError::UfoLoad(format!("layercontents.plist: {e}")))?;
        let mut files = HashMap::new();
        for (_, dir) in &layers {
            let contents_path = format!("{dir}/contents.plist");
            let contents: BTreeMap<String, String> = match entries.get(&contents_path) {
                Some(text) => plist::from_bytes(text.as_bytes())
                    .map_err(|e| BabelfontError::UfoLoad(format!("{contents_path}: {e}")))?,
                None => BTreeMap::new(),
            };
            files.insert(dir.clone(), contents);
        }
        Ok(Self { layers, files })
    }

    /// The layers' names and directories, in `layercontents.plist` order.
    pub fn layers(&self) -> impl Iterator<Item = (&str, &str)> {
        self.layers
            .iter()
            .map(|(name, dir)| (name.as_str(), dir.as_str()))
    }

    pub(crate) fn file_name(&self, dir: &str, glyph: &str) -> Option<&str> {
        self.files.get(dir)?.get(glyph).map(String::as_str)
    }

    fn has_layer_dir(&self, dir: &str) -> bool {
        self.files.contains_key(dir)
    }

    /// The `.glif` paths of `glyph` in every layer, relative to the UFO.
    pub fn glyph_paths(&self, glyph: &str) -> Vec<String> {
        self.layers
            .iter()
            .filter_map(|(_, dir)| Some(format!("{dir}/{}", self.file_name(dir, glyph)?)))
            .collect()
    }

    /// The glyph stored at `path` (`<layer dir>/<file>.glif`), if any.
    pub fn glyph_at(&self, path: &str) -> Option<&str> {
        let (dir, file) = path.split_once('/')?;
        self.files
            .get(dir)?
            .iter()
            .find(|(_, f)| f.as_str() == file)
            .map(|(glyph, _)| glyph.as_str())
    }
}

/// Glyphs number names (font) and values (master), as babelfont keeps them
/// for a Glyphs source. A UFO has no such thing; they ride in its lib so
/// that feature code using `$name` compiles the same.
const KEY_NUMBER_NAMES: &str = "com.schriftgestalt.Glyphs.numberNames";
const KEY_NUMBER_VALUES: &str = "com.schriftgestalt.Glyphs.numberValues";

/// Writes the font's number names and `master`'s values into a UFO lib.
pub(crate) fn save_number_values(lib: &mut norad::Plist, font: &Font, master: &Master) {
    let as_plist = |value: Option<&serde_json::Value>| {
        value
            .filter(|v| v.as_array().is_some_and(|a| !a.is_empty()))
            .and_then(|v| serde_json::from_value::<plist::Value>(v.clone()).ok())
    };
    match as_plist(font.format_specific.get(KEY_NUMBER_NAMES)) {
        Some(names) => {
            lib.insert(KEY_NUMBER_NAMES.into(), names);
            match as_plist(master.format_specific.get(KEY_NUMBER_VALUES)) {
                Some(values) => lib.insert(KEY_NUMBER_VALUES.into(), values),
                None => lib.remove(KEY_NUMBER_VALUES),
            };
        }
        None => {
            lib.remove(KEY_NUMBER_NAMES);
            lib.remove(KEY_NUMBER_VALUES);
        }
    }
}

/// Reads number names and values back from a UFO lib: the names into the
/// font, if `font` is given, and the values into the master.
pub(crate) fn load_number_values(
    lib: &norad::Plist,
    font: Option<&mut crate::common::FormatSpecific>,
    master: &mut Master,
) {
    let json = |key: &str| lib.get(key).and_then(|v| serde_json::to_value(v).ok());
    if let (Some(font), Some(names)) = (font, json(KEY_NUMBER_NAMES)) {
        font.insert(KEY_NUMBER_NAMES.into(), names);
    }
    if let Some(values) = json(KEY_NUMBER_VALUES) {
        master
            .format_specific
            .insert(KEY_NUMBER_VALUES.into(), values);
    }
}

pub(crate) fn stash_lib(lib: Option<&norad::Plist>) -> crate::common::FormatSpecific {
    let mut fs = crate::common::FormatSpecific::default();
    if let Some(lib) = lib {
        if lib.is_empty() {
            return fs;
        }
        fs.insert(
            KEY_LIB.into(),
            serde_json::to_value(lib).unwrap_or_default(),
        );
    }
    fs
}

pub(crate) fn stat(path: &std::path::Path) -> Option<DateTime<chrono::Local>> {
    fs::metadata(path)
        .and_then(|x| x.created())
        .ok()
        .and_then(|x| {
            DateTime::from_timestamp(
                x.duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap_or(std::time::Duration::new(0, 0))
                    .as_secs() as i64,
                0,
            )
        })
        .map(DateTime::<chrono::Local>::from)
}

/// Load a UFO font from a file path
pub fn load<T: AsRef<std::path::Path>>(path: T) -> Result<Font, BabelfontError> {
    let created_time: Option<DateTime<Utc>> = stat(path.as_ref()).map(DateTime::<Utc>::from);
    let ufo = norad::Font::load(&path)?;
    font_from_norad(path.as_ref(), created_time, ufo)
}

/// Load a UFO font from in-memory entries keyed by relative path.
///
/// `path` is the virtual UFO root path (for example `MyFont.ufo` or `sources/MyFont.ufo`).
pub fn load_entries(
    path: PathBuf,
    entries: &HashMap<String, String>,
) -> Result<Font, BabelfontError> {
    let ufo = load_norad_from_entries(&path, entries)?;
    font_from_norad(&path, None, ufo)
}

pub(crate) fn load_norad_from_entries(
    path: &FsPath,
    entries: &HashMap<String, String>,
) -> Result<norad::Font, BabelfontError> {
    let normalized_entries: HashMap<String, String> = entries
        .iter()
        .map(|(k, v)| {
            (
                normalize_virtual_path(k).unwrap_or_else(|| k.clone()),
                v.clone(),
            )
        })
        .collect();

    let root = normalize_virtual_path(&path.to_string_lossy()).unwrap_or_default();
    let root = root.trim_end_matches('/').to_string();

    let source = |requested: &FsPath| -> Option<Result<Vec<u8>, std::io::Error>> {
        let requested = normalize_virtual_path(&requested.to_string_lossy())?;
        let found = normalized_entries.get(&requested).or_else(|| {
            if !root.is_empty() {
                let prefixed = format!("{}/{}", root, requested);
                normalized_entries.get(&prefixed)
            } else {
                None
            }
        });
        found.map(|v| Ok(v.clone().into_bytes()))
    };

    norad::Font::load_from_source(&norad::DataRequest::all(), &source).map_err(Into::into)
}

pub(crate) fn font_from_norad(
    path: &FsPath,
    created_time: Option<DateTime<Utc>>,
    ufo: norad::Font,
) -> Result<Font, BabelfontError> {
    let mut font = Font::new();
    font.format_specific = stash_lib(Some(&ufo.lib));
    load_glyphs(&mut font, &ufo);
    let info = &ufo.font_info;
    load_font_info(&mut font, info, created_time);
    let mut master = Master::new(
        info.family_name
            .as_ref()
            .unwrap_or(&"Unnamed master".to_string()),
        UFO_MASTER_ID,
        Location::new(),
    );
    load_master_info(&mut master, info);
    stash_font_info(&mut master, info);
    load_number_values(&ufo.lib, Some(&mut font.format_specific), &mut master);
    load_kerning(&mut master, &ufo.kerning);
    (font.first_kern_groups, font.second_kern_groups) = load_kern_groups(&ufo.groups);

    for layer in ufo.iter_layers() {
        for g in font.glyphs.iter_mut() {
            if let Some(norad_glyph) = layer.get_glyph(g.name.as_str()) {
                g.layers
                    .push(norad_glyph_to_babelfont_layer(norad_glyph, layer, &master))
            }
        }
    }
    let expanded_features = ufo.features_expanded()?;
    font.features = Features::from_fea(&expanded_features);
    if let Some(sources) = feature_sources_for(&ufo, &font.features) {
        font.format_specific
            .insert(KEY_FEATURE_SOURCES.into(), sources);
    }
    font.features.include_paths.push(
        path.parent()
            .unwrap_or_else(|| std::path::Path::new("."))
            .to_path_buf(),
    );
    // Potentially also within the UFO itself? I think this unclear.

    font.masters.push(master);

    Ok(font)
}

fn normalize_virtual_path(path: &str) -> Option<String> {
    let mut parts: Vec<String> = Vec::new();
    for component in FsPath::new(path).components() {
        match component {
            std::path::Component::Normal(p) => parts.push(p.to_string_lossy().to_string()),
            std::path::Component::CurDir | std::path::Component::RootDir => {}
            std::path::Component::ParentDir => {
                if parts.pop().is_none() {
                    return None;
                }
            }
            std::path::Component::Prefix(_) => return None,
        }
    }
    Some(parts.join("/"))
}

/// Convert a Babelfont Font to a norad UFO font
///
/// This is currently unfinished and may not preserve all data.
pub fn as_norad(font: &Font, master_ix: usize) -> Result<norad::Font, BabelfontError> {
    as_norad_with_layout(font, master_ix, &[], &UfoLayout::default(), None)
}

/// Where a converted glyph goes in the UFO.
enum TargetLayer {
    Default,
    Named(String),
}

/// [`as_norad`], keeping the directory and `.glif` file names of `layout`.
///
/// The UFO holds master `master_ix` in its default layer and each master of
/// `sparse_masters` in the layer named with it, as a Designspace source with a
/// `layer` has it. A master that keeps its own UFO's lib, features or
/// fontinfo (a Designspace master other than the default) writes those. With
/// `only_glyphs`, the layers hold just those glyphs.
pub(crate) fn as_norad_with_layout(
    font: &Font,
    master_ix: usize,
    sparse_masters: &[(usize, String)],
    layout: &UfoLayout,
    only_glyphs: Option<&HashSet<&str>>,
) -> Result<norad::Font, BabelfontError> {
    let mut ufo = norad::Font::new();
    let first_master = font
        .masters
        .first()
        .ok_or(BabelfontError::NoDefaultMaster)?;
    let target_master = font.masters.get(master_ix).unwrap_or(first_master);
    let plist = |value: Option<&serde_json::Value>| {
        value.and_then(|x| serde_json::from_value::<norad::Plist>(x.clone()).ok())
    };
    // Move some things into lib key before serializing it:
    // exports
    // categories
    let font_lib = plist(font.format_specific.get(KEY_LIB)).unwrap_or_default();
    let updates = glyph_lib_updates(font, &font_lib);
    match plist(target_master.format_specific.get(KEY_LIB)) {
        // A master's own UFO lib only changes where the font's glyph
        // metadata did.
        Some(master_lib) => ufo.lib = master_lib,
        None => {
            ufo.lib = font_lib;
            save_uvs_sequences(font, &mut ufo.lib);
        }
    }
    apply_lib_updates(&mut ufo.lib, updates);
    save_number_values(&mut ufo.lib, font, target_master);

    // Keep the existing layers, in their order and directories.
    for (name, dir) in &layout.layers {
        if dir == "glyphs" {
            if name != ufo.default_layer().name().as_str() {
                let default_name = ufo.default_layer().name().to_string();
                ufo.layers.rename_layer(&default_name, name, false)?;
            }
        } else {
            ufo.layers.new_layer_at(name, dir.as_str())?;
        }
    }

    let mut placed: Vec<(TargetLayer, norad::Glyph)> = vec![];
    let masters: Vec<(&Master, TargetLayer)> =
        std::iter::once((target_master, TargetLayer::Default))
            .chain(sparse_masters.iter().filter_map(|(ix, layer)| {
                Some((font.masters.get(*ix)?, TargetLayer::Named(layer.clone())))
            }))
            .collect();
    for g in font.glyphs.iter() {
        if only_glyphs.is_some_and(|only| !only.contains(g.name.as_str())) {
            continue;
        }
        for (layer_master, default_target) in &masters {
            let is_main = std::ptr::eq(*layer_master, target_master);
            for layer in g.layers.iter() {
                // Backgrounds belong to whichever layer points at them.
                let owner = if layer.is_background {
                    g.layers.iter().find(|l| {
                        l.background_layer_id.is_some() && l.background_layer_id == layer.id
                    })
                } else {
                    Some(layer)
                };
                let Some(owner) = owner else { continue };
                let belongs_to_master = match &owner.master {
                    LayerType::DefaultForMaster(id) => id == &layer_master.id,
                    // Older UFO imports kept the master's id as the layer id.
                    LayerType::AssociatedWithMaster(id) => {
                        is_main
                            && (id == &layer_master.id
                                || owner.id.as_deref() == Some(layer_master.id.as_str()))
                    }
                    LayerType::FreeFloating => is_main && master_ix == 0 && !layer.is_background,
                };
                // A sparse master's layer holds its outlines alone.
                if !belongs_to_master || (!is_main && layer.is_background) {
                    continue;
                }
                let mut norad_layer = babelfont_layer_to_norad_glyph(g, layer)?;
                let target = if layer.is_background {
                    TargetLayer::Named("public.background".to_string())
                } else if matches!(owner.master, LayerType::DefaultForMaster(_)) {
                    match default_target {
                        TargetLayer::Default => TargetLayer::Default,
                        TargetLayer::Named(name) => TargetLayer::Named(name.clone()),
                    }
                } else {
                    TargetLayer::Named(
                        layer
                            .name
                            .as_deref()
                            .unwrap_or("public.default")
                            .to_string(),
                    )
                };
                // Codepoints belong to the glyph, which its default layer states.
                if !matches!(target, TargetLayer::Default) {
                    norad_layer.codepoints = norad::Codepoints::default();
                }
                placed.push((target, norad_layer));
            }
        }
    }
    // Glyphs that keep an existing file name go in first, so that a name
    // derived for a new glyph never takes one of theirs.
    let (mut ordered, derived): (Vec<_>, Vec<_>) =
        placed.into_iter().partition(|(target, glyph)| {
            let dir = match target {
                TargetLayer::Default => Some("glyphs".to_string()),
                TargetLayer::Named(name) => ufo.layers.get(name).map(|l| path_string(l.path())),
            };
            dir.is_some_and(|dir| layout.file_name(&dir, glyph.name()).is_some())
        });
    ordered.extend(derived);
    for (target, glyph) in ordered {
        let layer = match target {
            TargetLayer::Default => ufo.default_layer_mut(),
            TargetLayer::Named(name) => ufo.layers.get_or_create_layer(&name)?,
        };
        let dir = path_string(layer.path());
        match layout.file_name(&dir, glyph.name()) {
            Some(file) => layer.insert_glyph_at(glyph, file)?,
            None => layer.insert_glyph(glyph),
        }
    }

    let merged: IndexMap<(SmolStr, SmolStr), i16> = font
        .merged_kerning_for_master(target_master)
        .into_iter()
        .collect();
    save_kerning(&mut ufo.kerning, &merged)?;
    ufo.font_info = target_master
        .format_specific
        .get(KEY_FONTINFO)
        .and_then(|v| serde_json::from_value(v.clone()).ok())
        .unwrap_or_default();
    save_info(&mut ufo.font_info, font, target_master);
    let (swapped_first, swapped_second) = font.kern_groups_with_rtl_swaps();
    save_kern_groups(&mut ufo.groups, &swapped_first, &swapped_second)?;
    // Insert non-kerning groups into ufo.groups
    for (group_name, glyphs) in font
        .format_specific
        .get(KEY_GROUPS)
        .and_then(|x| x.as_object())
        .unwrap_or(&serde_json::Map::new())
    {
        let norad_group_name = norad::Name::new(group_name.as_str())?;
        if let Some(glyphs) = glyphs.as_array() {
            let norad_glyph_names: Vec<norad::Name> = glyphs
                .iter()
                .flat_map(|g| g.as_str())
                .map(norad::Name::new)
                .collect::<Result<_, _>>()?;
            ufo.groups.insert(norad_group_name, norad_glyph_names);
        }
    }

    match target_master
        .format_specific
        .get(KEY_FEATURE_SOURCES)
        .or_else(|| font.format_specific.get(KEY_FEATURE_SOURCES))
        .and_then(|v| serde_json::from_value::<FeatureSources>(v.clone()).ok())
    {
        Some(sources) if sources.fingerprint == features_fingerprint(&font.features) => {
            ufo.features = sources.root;
            ufo.feature_files = sources
                .includes
                .into_iter()
                .map(|(path, text)| (PathBuf::from(path), text))
                .collect();
        }
        _ => ufo.features = font.features.to_fea(),
    }
    Ok(ufo)
}

fn path_string(path: &FsPath) -> String {
    path.to_string_lossy().replace('\\', "/")
}

/// Writes the glyph order, OpenType categories, production names and
/// non-exporting glyphs into the font lib, where UFO keeps them. A key is only
/// rewritten when the font differs from what loading the lib gives, so an
/// untouched lib is saved as it was.
fn glyph_lib_updates(font: &Font, lib: &norad::Plist) -> Vec<(&'static str, serde_json::Value)> {
    let as_json = |key: &str| lib.get(key).and_then(|v| serde_json::to_value(v).ok());
    let string_map =
        |value: Option<serde_json::Value>| -> serde_json::Map<String, serde_json::Value> {
            value
                .and_then(|v| v.as_object().cloned())
                .unwrap_or_default()
        };
    let string_list = |value: Option<serde_json::Value>| -> Vec<String> {
        value
            .and_then(|v| v.as_array().cloned())
            .unwrap_or_default()
            .iter()
            .filter_map(|v| v.as_str().map(str::to_string))
            .collect()
    };
    let old_order = string_list(as_json(KEY_GLYPH_ORDER));
    let old_categories = string_map(as_json(KEY_CATEGORIES));
    let old_names = string_map(as_json(KEY_PSNAMES));
    let old_skipped: HashSet<String> = string_list(as_json(KEY_SKIP_EXPORT)).into_iter().collect();

    let names: Vec<String> = font.glyphs.iter().map(|g| g.name.to_string()).collect();
    let mut updates: Vec<(&'static str, serde_json::Value)> = vec![];
    if load_glyph_order(&old_order, &names) != names {
        updates.push((KEY_GLYPH_ORDER, serde_json::json!(names)));
    }

    let unknown_category = |name: &str| {
        #[cfg(feature = "glyphs")]
        let category = crate::filters::glyphsdata::data_category(name);
        #[cfg(not(feature = "glyphs"))]
        let category = None;
        category.unwrap_or(GlyphCategory::Base)
    };
    let loaded_category = |name: &str| match old_categories.get(name).and_then(|v| v.as_str()) {
        Some("mark") => GlyphCategory::Mark,
        Some("ligature") => GlyphCategory::Ligature,
        _ => GlyphCategory::Base,
    };
    if font
        .glyphs
        .iter()
        .any(|g| g.category != loaded_category(&g.name))
    {
        let mut categories = serde_json::Map::new();
        for glyph in font.glyphs.iter() {
            let existing = old_categories
                .get(glyph.name.as_str())
                .and_then(|v| v.as_str());
            // A UFO has no unknown category: loading makes Base of a missing
            // entry. Write the one compiling would give such a glyph.
            let resolved;
            let category = match &glyph.category {
                GlyphCategory::Unknown => {
                    resolved = unknown_category(&glyph.name);
                    &resolved
                }
                category => category,
            };
            let category = match category {
                GlyphCategory::Mark => Some("mark"),
                GlyphCategory::Ligature => Some("ligature"),
                // Only these are UFO categories; a Glyphs one such as
                // "Letter" is a base.
                GlyphCategory::Custom(custom)
                    if matches!(
                        custom.as_str(),
                        "base" | "ligature" | "mark" | "component" | "unassigned"
                    ) =>
                {
                    Some(custom.as_str())
                }
                // Loading makes Base of a missing entry and of "component",
                // so keep those.
                GlyphCategory::Base | GlyphCategory::Custom(_) => match existing {
                    Some("mark") | Some("ligature") => Some("base"),
                    other => other,
                },
                GlyphCategory::Unknown => None,
            };
            if let Some(category) = category {
                categories.insert(glyph.name.to_string(), category.into());
            }
        }
        updates.push((KEY_CATEGORIES, serde_json::Value::Object(categories)));
    }

    if font.glyphs.iter().any(|g| {
        g.production_name.as_deref() != old_names.get(g.name.as_str()).and_then(|v| v.as_str())
    }) {
        let production_names: serde_json::Map<String, serde_json::Value> = font
            .glyphs
            .iter()
            .filter_map(|g| {
                Some((
                    g.name.to_string(),
                    g.production_name.as_ref()?.to_string().into(),
                ))
            })
            .collect();
        updates.push((KEY_PSNAMES, serde_json::Value::Object(production_names)));
    }

    if font
        .glyphs
        .iter()
        .any(|g| g.exported == old_skipped.contains(g.name.as_str()))
    {
        let skipped: Vec<String> = font
            .glyphs
            .iter()
            .filter(|g| !g.exported)
            .map(|g| g.name.to_string())
            .collect();
        updates.push((KEY_SKIP_EXPORT, serde_json::json!(skipped)));
    }
    updates
}

/// Write [`glyph_lib_updates`] into a lib.
fn apply_lib_updates(lib: &mut norad::Plist, updates: Vec<(&'static str, serde_json::Value)>) {
    for (key, value) in updates {
        let is_empty = value.as_array().is_some_and(Vec::is_empty)
            || value.as_object().is_some_and(serde_json::Map::is_empty);
        if is_empty {
            lib.remove(key);
            continue;
        }
        if let Ok(mut entry) =
            serde_json::from_value::<norad::Plist>(serde_json::json!({ key: value }))
        {
            if let Some(value) = entry.remove(key) {
                lib.insert(key.to_string(), value);
            }
        }
    }
}

/// The glyph order loading gives for glyphs `names` and a lib order of
/// `lib_order`: `.notdef` first, then the lib's order, then the rest sorted.
fn load_glyph_order(lib_order: &[String], names: &[String]) -> Vec<String> {
    let mut rest: std::collections::BTreeSet<&str> = names.iter().map(String::as_str).collect();
    let mut order = vec![];
    if rest.remove(".notdef") {
        order.push(".notdef".to_string());
    }
    for name in lib_order {
        if rest.remove(name.as_str()) {
            order.push(name.clone());
        }
    }
    order.extend(rest.into_iter().map(str::to_string));
    order
}

pub(crate) fn save_ufo<T: AsRef<std::path::Path>>(
    font: &Font,
    path: T,
) -> Result<(), BabelfontError> {
    // We can't save more than one master
    if font.masters.len() > 1 {
        return Err(BabelfontError::MultipleMastersNotSupported);
    }
    let ufo = as_norad(font, 0)?;
    ufo.save(path).map_err(|e| e.into())
}

/// What [`save_entries`] writes.
#[derive(Debug, Default, Clone, Copy)]
pub struct UfoSaveRequest<'a> {
    /// The layout of the UFO being saved over; its names are kept.
    pub layout: Option<&'a UfoLayout>,
    /// Write only the `.glif` files of these glyphs, plus the font-level
    /// files when `font_level` is set. `None` writes the whole UFO.
    pub glyphs: Option<&'a [String]>,
    /// In a partial save, also write the top-level files: `metainfo.plist`,
    /// `fontinfo.plist`, `lib.plist`, `groups.plist`, `kerning.plist` and the
    /// feature files.
    pub font_level: bool,
}

/// UFO files as text, by path relative to the UFO.
#[derive(Debug, Default, Clone, Serialize)]
pub struct UfoEntries {
    /// Files to write.
    pub files: BTreeMap<String, String>,
    /// In a partial save with `font_level`, the top-level files the font no
    /// longer has, which should be removed. A full save lists none: anything
    /// it does not write is stale.
    pub removed: Vec<String>,
}

/// The result of [`save_entries`].
#[derive(Debug, Clone)]
pub enum UfoSave {
    /// The files to write.
    Entries(UfoEntries),
    /// A partial save cannot express the change, because it would alter the
    /// UFO's glyph set or layers (and so its `contents.plist` files); save
    /// the whole UFO instead.
    NeedsFullSave(String),
}

/// Save a UFO font to in-memory entries keyed by relative path.
///
/// Only single-master fonts can be saved.
pub fn save_entries(font: &Font, request: UfoSaveRequest) -> Result<UfoSave, BabelfontError> {
    if font.masters.len() > 1 {
        return Err(BabelfontError::MultipleMastersNotSupported);
    }
    save_master_entries(font, 0, &[], request)
}

/// [`save_entries`] for the UFO of master `master_ix`, with `sparse_masters`
/// in its named layers (see [`as_norad_with_layout`]).
pub(crate) fn save_master_entries(
    font: &Font,
    master_ix: usize,
    sparse_masters: &[(usize, String)],
    request: UfoSaveRequest,
) -> Result<UfoSave, BabelfontError> {
    let default_layout = UfoLayout::default();
    let layout = request.layout.unwrap_or(&default_layout);
    let only: Option<HashSet<&str>> = request
        .glyphs
        .map(|names| names.iter().map(String::as_str).collect());
    if let Some(only) = &only {
        if let Some(missing) = only.iter().find(|name| font.glyphs.get(name).is_none()) {
            return Ok(UfoSave::NeedsFullSave(format!(
                "glyph {missing} was removed"
            )));
        }
    }
    let ufo = as_norad_with_layout(font, master_ix, sparse_masters, layout, only.as_ref())?;

    let mut glyph_files = HashSet::new();
    if let Some(only) = &only {
        for layer in ufo.iter_layers() {
            let dir = path_string(layer.path());
            if !layout.has_layer_dir(&dir) && !layer.is_empty() {
                return Ok(UfoSave::NeedsFullSave(format!(
                    "new layer {}",
                    layer.name()
                )));
            }
            for name in only {
                match (layer.get_path(name), layout.file_name(&dir, name)) {
                    (Some(path), Some(_)) => {
                        glyph_files.insert(format!("{dir}/{}", path_string(path)));
                    }
                    (None, None) => {}
                    _ => {
                        return Ok(UfoSave::NeedsFullSave(format!(
                            "glyph {name} changed layers"
                        )))
                    }
                }
            }
        }
    }
    let layer_dirs: HashSet<String> = ufo.iter_layers().map(|l| path_string(l.path())).collect();

    let written = Mutex::new(BTreeMap::<String, Vec<u8>>::new());
    let sink = |path: &FsPath, data: &[u8]| -> Result<(), std::io::Error> {
        written
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .insert(path_string(path), data.to_vec());
        Ok(())
    };
    ufo.save_to_sink(&sink, &norad::WriteOptions::default())?;

    let mut entries = UfoEntries::default();
    for (path, bytes) in written.into_inner().unwrap_or_else(|e| e.into_inner()) {
        if only.is_some() {
            let in_layer = path
                .split_once('/')
                .is_some_and(|(dir, _)| layer_dirs.contains(dir));
            let wanted = if in_layer {
                glyph_files.contains(&path)
            } else {
                request.font_level && path != "layercontents.plist"
            };
            if !wanted {
                continue;
            }
        }
        let text = String::from_utf8(bytes)
            .map_err(|_| BabelfontError::UfoSave(format!("{path} is not UTF-8")))?;
        entries.files.insert(path, text);
    }
    if only.is_some() && request.font_level {
        for file in [
            "fontinfo.plist",
            "lib.plist",
            "groups.plist",
            "kerning.plist",
            "features.fea",
        ] {
            if !entries.files.contains_key(file) {
                entries.removed.push(file.to_string());
            }
        }
    }
    Ok(UfoSave::Entries(entries))
}

/// Loads glyphs from their `.glif` files alone, to pick up a change to a few
/// files without reading the whole UFO.
///
/// `layout` says which files hold each glyph; `glifs` holds those files by
/// path relative to the UFO. Layers belong to the master `master_id`. Only the
/// glyph data stored in `.glif` files is meaningful in the result: layers and
/// codepoints.
pub fn load_glyphs_from_glifs(
    layout: &UfoLayout,
    glifs: &HashMap<String, String>,
    glyph_names: &[String],
    master_id: &str,
) -> Result<Vec<Glyph>, BabelfontError> {
    let master = Master::new(master_id, master_id, Location::new());
    let mut layers = norad::LayerContents::default();
    for (name, dir) in &layout.layers {
        if dir == "glyphs" {
            let default_name = layers.default_layer().name().to_string();
            if name != &default_name {
                layers.rename_layer(&default_name, name, false)?;
            }
        } else {
            layers.new_layer_at(name, dir.as_str())?;
        }
    }
    let mut glyphs = vec![];
    for name in glyph_names {
        let mut glyph = Glyph::new(name);
        for layer in layers.iter() {
            let dir = path_string(layer.path());
            let Some(file) = layout.file_name(&dir, name) else {
                continue;
            };
            let path = format!("{dir}/{file}");
            let text = glifs
                .get(&path)
                .ok_or_else(|| BabelfontError::UfoLoad(format!("missing {path}")))?;
            let norad_glyph = norad::Glyph::parse_raw(text.as_bytes())
                .map_err(|e| BabelfontError::UfoLoad(format!("{path}: {e}")))?;
            if layer.is_default() {
                glyph.codepoints = norad_glyph.codepoints.iter().map(|c| c as u32).collect();
            }
            glyph
                .layers
                .push(norad_glyph_to_babelfont_layer(&norad_glyph, layer, &master));
        }
        glyphs.push(glyph);
    }
    Ok(glyphs)
}

fn babelfont_layer_to_norad_glyph(
    glyph: &Glyph,
    layer: &Layer,
) -> Result<norad::Glyph, BabelfontError> {
    let mut norad_glyph = norad::Glyph::new(glyph.name.as_str());
    norad_glyph.width = layer.width as f64;
    if let Some(advance) = layer.vertical_advance {
        norad_glyph.height = advance as f64;
    }
    norad_glyph.codepoints =
        norad::Codepoints::new(glyph.codepoints.iter().flat_map(|&x| char::from_u32(x)));

    for shape in &layer.shapes {
        match shape {
            Shape::Path(p) => {
                norad_glyph.contours.push(save_path(p)?);
            }
            Shape::Component(c) => {
                norad_glyph.components.push(save_component(c)?);
            }
            Shape::FormatSpecific(_) => {
                // UFO has no representation for opaque shapes.
                log::warn!("Skipping opaque shape when saving UFO");
            }
        }
    }
    norad_glyph.anchors = layer
        .anchors
        .iter()
        .map(norad::Anchor::try_from)
        .collect::<Result<Vec<_>, BabelfontError>>()?;
    if !layer.guides.is_empty() {
        norad_glyph.guidelines = layer
            .guides
            .iter()
            .map(norad::Guideline::try_from)
            .collect::<Result<Vec<_>, BabelfontError>>()?;
    }
    if let Some(lib) = layer.format_specific.get(KEY_LIB) {
        norad_glyph.lib = serde_json::from_value::<norad::Plist>(lib.clone()).unwrap_or_default();
    }
    // In a UFO the vertical origin is a glyph lib entry, and the native field
    // is authoritative.
    let mut lib_value = serde_json::to_value(&norad_glyph.lib).unwrap_or_default();
    if let Some(map) = lib_value.as_object_mut() {
        match layer.vertical_origin {
            Some(origin) => {
                // A UFO vertical origin is usually a whole number; write it as
                // one rather than turning an 880 into 880.0.
                let value = if origin.fract() == 0.0 {
                    serde_json::json!(origin as i64)
                } else {
                    serde_json::json!(origin)
                };
                map.insert(KEY_VERTICAL_ORIGIN.to_string(), value);
            }
            None => {
                map.remove(KEY_VERTICAL_ORIGIN);
            }
        }
    }
    norad_glyph.lib = serde_json::from_value::<norad::Plist>(lib_value).unwrap_or_default();
    Ok(norad_glyph)
}

pub(crate) fn norad_glyph_to_babelfont_layer(
    glyph: &norad::Glyph,
    layer: &norad::Layer,
    master: &Master,
) -> Layer {
    glyph_to_layer(
        glyph,
        (!layer.is_default()).then(|| layer.name().as_str()),
        master,
    )
}

/// A babelfont layer from a `.glif` in the named UFO layer of `master`, or in
/// its default layer for `None`.
pub(crate) fn glyph_to_layer(
    glyph: &norad::Glyph,
    layer_name: Option<&str>,
    master: &Master,
) -> Layer {
    let master_id = master.id.as_str();
    let mut l = Layer::new(glyph.width as f32);
    match layer_name {
        None => {
            l.name = None;
            l.master = LayerType::DefaultForMaster(master_id.to_string());
            l.id = Some(master_id.to_string());
        }
        Some(name) => {
            l.name = Some(name.to_string());
            l.master = LayerType::AssociatedWithMaster(master_id.to_string());
            // Unique within the glyph, and the same on every load.
            l.id = Some(format!("{master_id}.{name}"));
        }
    }
    if !glyph.lib.is_empty() {
        l.format_specific = stash_lib(Some(&glyph.lib));
    }
    // A glif without a height has a height of zero, which norad cannot tell
    // apart from an explicit zero, so only a non-zero height is an advance.
    if glyph.height != 0.0 {
        l.vertical_advance = Some(glyph.height as f32);
    }
    // Move `public.verticalOrigin` out of the stashed lib into the native field,
    // so that it is not also written back from the stash.
    if let Some(serde_json::Value::Object(mut lib)) = l.format_specific.remove(KEY_LIB) {
        if let Some(origin) = lib.remove(KEY_VERTICAL_ORIGIN).and_then(|v| v.as_f64()) {
            l.vertical_origin = Some(origin as f32);
        }
        if !lib.is_empty() {
            l.format_specific
                .insert(KEY_LIB.to_string(), serde_json::Value::Object(lib));
        }
    }

    l.guides = glyph.guidelines.iter().map(|x| x.into()).collect();
    l.anchors = glyph.anchors.iter().map(|x| x.into()).collect();
    for comp in &glyph.components {
        l.shapes.push(Shape::Component(load_component(comp)));
    }
    for contour in &glyph.contours {
        l.shapes.push(Shape::Path(load_path(contour)));
    }
    l
}

pub(crate) fn load_component(c: &norad::Component) -> Component {
    let t = c.transform;
    // norad uses the convention:
    //   x' = x_scale * x + xy_scale * y + x_offset
    //   y' = yx_scale * x + y_scale * y + y_offset
    // kurbo::Affine::new expects [xx, yx, xy, yy, dx, dy]
    let affine = kurbo::Affine::new([
        t.x_scale, t.yx_scale, t.xy_scale, t.y_scale, t.x_offset, t.y_offset,
    ]);
    // UFO uses the same transform representation as Fontra
    let decomposed: DecomposedAffine = affine.into();
    Component {
        id: None,
        reference: c.base.to_smolstr(),
        transform: decomposed,
        format_specific: stash_lib(c.lib()),
        location: IndexMap::new(),
    }
}

pub(crate) fn save_component(c: &Component) -> Result<norad::Component, BabelfontError> {
    let affine = c.transform.as_affine();
    let mut t = affine.as_coeffs();
    // Clamp floating point noise to zero to avoid epsilon drift in roundtrips
    for v in t.iter_mut() {
        if v.abs() < 1e-7 {
            *v = 0.0;
        }
    }
    let mut comp = norad::Component::new(
        norad::Name::new(c.reference.as_str())?,
        norad::AffineTransform {
            // Map back from kurbo coefficients [xx, yx, xy, yy, dx, dy]
            x_scale: t[0],
            xy_scale: t[2],
            yx_scale: t[1],
            y_scale: t[3],
            x_offset: t[4],
            y_offset: t[5],
        },
        None,
    );
    if let Some(lib) = c
        .format_specific
        .get(KEY_LIB)
        .and_then(|x| serde_json::from_value(x.clone()).ok())
    {
        comp.replace_lib(lib);
    }
    Ok(comp)
}

pub(crate) fn load_path(c: &norad::Contour) -> Path {
    let mut nodes: Vec<Node> = c.points.iter().map(|p| p.into()).collect();
    // See https://github.com/simoncozens/rust-font-tools/issues/3
    nodes.rotate_left(1);
    Path {
        id: None,
        nodes,
        closed: c
            .points
            .first()
            .is_none_or(|v| v.typ != norad::PointType::Move),
        format_specific: stash_lib(c.lib()),
    }
}

pub(crate) fn save_path(p: &Path) -> Result<norad::Contour, BabelfontError> {
    let mut points: Vec<norad::ContourPoint> = p
        .nodes
        .iter()
        .map(|n| n.try_into())
        .collect::<Result<Vec<_>, BabelfontError>>()?;
    // See https://github.com/simoncozens/rust-font-tools/issues/3
    points.rotate_right(1);
    let mut c = norad::Contour::new(points, None);
    if let Some(lib) = p
        .format_specific
        .get(KEY_LIB)
        .and_then(|x| serde_json::from_value(x.clone()).ok())
    {
        c.replace_lib(lib);
    }
    Ok(c)
}

pub(crate) fn save_kerning(
    norad_kerning: &mut norad::Kerning,
    babelfont_kerning: &IndexMap<(SmolStr, SmolStr), i16>,
) -> Result<(), BabelfontError> {
    for ((left, right), value) in babelfont_kerning.iter() {
        let left_key = if left.starts_with('@') {
            "public.kern1.".to_string() + left.trim_start_matches('@')
        } else {
            left.to_string()
        };
        let right_key = if right.starts_with('@') {
            "public.kern2.".to_string() + right.trim_start_matches('@')
        } else {
            right.to_string()
        };
        norad_kerning
            .entry(norad::Name::new(left_key.as_str())?)
            .or_default()
            .insert(norad::Name::new(right_key.as_str())?, *value as f64);
    }
    Ok(())
}

pub(crate) fn save_kern_groups(
    norad_groups: &mut norad::Groups,
    first: &IndexMap<SmolStr, Vec<SmolStr>>,
    second: &IndexMap<SmolStr, Vec<SmolStr>>,
) -> Result<(), BabelfontError> {
    // Prefix with public.kern1. and public.kern2.
    for (group_name, glyphs) in first.iter() {
        let norad_group_name = norad::Name::new(&format!("public.kern1.{}", group_name))?;
        let norad_glyph_names: Vec<norad::Name> = glyphs
            .iter()
            .map(|g| norad::Name::new(g.as_str()))
            .collect::<Result<_, _>>()?;
        norad_groups.insert(norad_group_name, norad_glyph_names);
    }
    for (group_name, glyphs) in second.iter() {
        let norad_group_name = norad::Name::new(&format!("public.kern2.{}", group_name))?;
        let norad_glyph_names: Vec<norad::Name> = glyphs
            .iter()
            .map(|g| norad::Name::new(g.as_str()))
            .collect::<Result<_, _>>()?;
        norad_groups.insert(norad_group_name, norad_glyph_names);
    }
    Ok(())
}

/// Keep a UFO's style names, which belong to the font for a UFO and to each
/// master in a designspace.
pub(crate) fn stash_style_names(
    format_specific: &mut crate::common::FormatSpecific,
    info: &norad::FontInfo,
) {
    if let Some(smfn) = &info.style_map_family_name {
        format_specific.insert(
            KEY_STYLE_MAP_FAMILY_NAME.into(),
            serde_json::to_value(smfn).unwrap_or_default(),
        );
    }
    if let Some(smsn) = &info.style_map_style_name {
        format_specific.insert(
            KEY_STYLE_MAP_STYLE_NAME.into(),
            match smsn {
                norad::fontinfo::StyleMapStyle::Regular => "Regular",
                norad::fontinfo::StyleMapStyle::Bold => "Bold",
                norad::fontinfo::StyleMapStyle::Italic => "Italic",
                norad::fontinfo::StyleMapStyle::BoldItalic => "Bold Italic",
            }
            .into(),
        );
    }
    if let Some(stylename) = &info.style_name {
        format_specific.insert(KEY_STYLE_NAME.into(), stylename.clone().into());
    }
}

/// Keep a UFO's `fontinfo.plist` on the master it gives, see [`KEY_FONTINFO`].
pub(crate) fn stash_font_info(master: &mut Master, info: &norad::FontInfo) {
    if let Ok(value) = serde_json::to_value(info) {
        master.format_specific.insert(KEY_FONTINFO.into(), value);
    }
}

/// Write the font's and `master`'s info into a UFO's fontinfo.
pub(crate) fn save_info(info: &mut norad::FontInfo, font: &Font, master: &Master) {
    let get_metric = |mt: MetricType| master.metrics.get(&mt).map(|&v| v as f64);
    info.ascender = get_metric(MetricType::Ascender);
    info.cap_height = get_metric(MetricType::CapHeight);
    info.copyright = font
        .names
        .copyright
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.descender = get_metric(MetricType::Descender);
    info.family_name = font
        .names
        .family_name
        .get_default_or_fallback()
        .map(|x| x.to_string());
    let guides: Vec<_> = master.guides.iter().flat_map(|g| g.try_into()).collect();
    info.guidelines = (!guides.is_empty()).then_some(guides);
    // Written back in the UFO's post convention: see load_master_info.
    info.italic_angle = get_metric(MetricType::ItalicAngle).map(|v| -v);
    // macintoshFONDName, yey
    info.note = font.note.clone();
    // gasp range records
    info.open_type_head_created = font.date.format("%Y/%m/%d %H:%M:%S").to_string().into();
    info.open_type_head_flags = font.custom_ot_values.head_flags.map(to_bitarray);
    // lowest rec ppem
    info.open_type_hhea_ascender = get_metric(MetricType::HheaAscender).map(|x| x as i32);
    info.open_type_hhea_caret_offset = get_metric(MetricType::HheaCaretOffset).map(|x| x as i32);
    info.open_type_hhea_caret_slope_rise =
        get_metric(MetricType::HheaCaretSlopeRise).map(|x| x as i32);
    info.open_type_hhea_caret_slope_run =
        get_metric(MetricType::HheaCaretSlopeRun).map(|x| x as i32);
    info.open_type_hhea_descender = get_metric(MetricType::HheaDescender).map(|x| x as i32);
    info.open_type_hhea_line_gap = get_metric(MetricType::HheaLineGap).map(|x| x as i32);
    info.open_type_vhea_vert_typo_ascender = get_metric(MetricType::VheaAscender).map(|x| x as i32);
    info.open_type_vhea_vert_typo_descender =
        get_metric(MetricType::VheaDescender).map(|x| x as i32);
    info.open_type_vhea_vert_typo_line_gap = get_metric(MetricType::VheaLineGap).map(|x| x as i32);
    info.open_type_vhea_caret_slope_rise =
        get_metric(MetricType::VheaCaretSlopeRise).map(|x| x as i32);
    info.open_type_vhea_caret_slope_run =
        get_metric(MetricType::VheaCaretSlopeRun).map(|x| x as i32);
    info.open_type_vhea_caret_offset = get_metric(MetricType::VheaCaretOffset).map(|x| x as i32);
    // opentype name compatible full name
    info.open_type_name_description = font
        .names
        .description
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_designer_url = font
        .names
        .designer_url
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_designer = font
        .names
        .designer
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_license = font
        .names
        .license
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_license_url = font
        .names
        .license_url
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_manufacturer = font
        .names
        .manufacturer
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_manufacturer_url = font
        .names
        .manufacturer_url
        .get_default_or_fallback()
        .map(|x| x.to_string());
    // UFO's "preferred" names are the typographic ones (IDs 16 and 17)
    info.open_type_name_preferred_family_name = font
        .names
        .typographic_family
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_preferred_subfamily_name = font
        .names
        .typographic_subfamily
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_wws_family_name = font
        .names
        .wws_family_name
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_wws_subfamily_name = font
        .names
        .wws_subfamily_name
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_compatible_full_name = font
        .names
        .compatible_full_name
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_sample_text = font
        .names
        .sample_text
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_unique_id = font
        .names
        .unique_id
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.open_type_name_version = font
        .names
        .version
        .get_default_or_fallback()
        .map(|x| x.to_string());
    // XXX lots more
    info.postscript_font_name = font
        .names
        .postscript_name
        .get_default_or_fallback()
        .map(|x| x.to_string());
    // and more
    let codepage_ranges: Vec<u8> =
        to_bitarray(font.custom_ot_values.os2_code_page_range1.unwrap_or(0))
            .iter()
            .copied()
            .chain(
                to_bitarray(font.custom_ot_values.os2_code_page_range2.unwrap_or(0))
                    .iter()
                    .map(|&bit| bit + 32),
            )
            .collect();
    if !codepage_ranges.is_empty() {
        info.open_type_os2_code_page_ranges = Some(codepage_ranges);
    }
    info.open_type_os2_selection = font.custom_ot_values.os2_fs_selection.map(to_bitarray);
    info.open_type_os2_type = font.custom_ot_values.os2_fs_type.map(to_bitarray);
    info.open_type_os2_typo_ascender = get_metric(MetricType::TypoAscender).map(|x| x as i32);
    info.open_type_os2_typo_descender = get_metric(MetricType::TypoDescender).map(|x| x as i32);
    info.open_type_os2_typo_line_gap = get_metric(MetricType::TypoLineGap).map(|x| x as i32);
    info.open_type_os2_strikeout_position =
        get_metric(MetricType::StrikeoutPosition).map(|x| x as i32);
    info.open_type_os2_strikeout_size = get_metric(MetricType::StrikeoutSize).map(|x| x as i32);
    info.open_type_os2_subscript_x_offset =
        get_metric(MetricType::SubscriptXOffset).map(|x| x as i32);
    info.open_type_os2_subscript_x_size = get_metric(MetricType::SubscriptXSize).map(|x| x as i32);
    info.open_type_os2_subscript_y_offset =
        get_metric(MetricType::SubscriptYOffset).map(|x| x as i32);
    info.open_type_os2_subscript_y_size = get_metric(MetricType::SubscriptYSize).map(|x| x as i32);
    info.open_type_os2_superscript_x_offset =
        get_metric(MetricType::SuperscriptXOffset).map(|x| x as i32);
    info.open_type_os2_superscript_x_size =
        get_metric(MetricType::SuperscriptXSize).map(|x| x as i32);
    info.open_type_os2_superscript_y_offset =
        get_metric(MetricType::SuperscriptYOffset).map(|x| x as i32);
    info.open_type_os2_superscript_y_size =
        get_metric(MetricType::SuperscriptYSize).map(|x| x as i32);
    let mut bit_array = vec![];
    let unicode_range_1 = font.custom_ot_values.os2_unicode_range1.unwrap_or(0) as u64;
    bit_array.extend(to_bitarray(unicode_range_1));
    let unicode_range_2 = font.custom_ot_values.os2_unicode_range2.unwrap_or(0) as u64;
    bit_array.extend(to_bitarray(unicode_range_2).iter().map(|&bit| bit + 32));
    let unicode_range_3 = font.custom_ot_values.os2_unicode_range3.unwrap_or(0) as u64;
    bit_array.extend(to_bitarray(unicode_range_3).iter().map(|&bit| bit + 64));
    let unicode_range_4 = font.custom_ot_values.os2_unicode_range4.unwrap_or(0) as u64;
    bit_array.extend(to_bitarray(unicode_range_4).iter().map(|&bit| bit + 96));
    if !bit_array.is_empty() {
        info.open_type_os2_unicode_ranges = Some(bit_array);
    }
    info.open_type_os2_panose =
        font.custom_ot_values
            .os2_panose
            .map(|x| norad::fontinfo::Os2Panose {
                family_type: x[0].into(),
                serif_style: x[1].into(),
                weight: x[2].into(),
                proportion: x[3].into(),
                contrast: x[4].into(),
                stroke_variation: x[5].into(),
                arm_style: x[6].into(),
                letterform: x[7].into(),
                midline: x[8].into(),
                x_height: x[9].into(),
            });
    info.open_type_os2_vendor_id = font.custom_ot_values.os2_vendor_id.map(|x| x.to_string());
    info.open_type_os2_win_ascent = get_metric(MetricType::WinAscent).map(|x| x as u32);
    info.open_type_os2_win_descent = get_metric(MetricType::WinDescent).map(|x| x as u32);
    info.postscript_underline_position = get_metric(MetricType::UnderlinePosition);
    info.postscript_underline_thickness = get_metric(MetricType::UnderlineThickness);
    info.postscript_other_blues = font.custom_ot_values.cff_other_blues.clone();
    info.postscript_blue_values = font.custom_ot_values.cff_blue_values.clone();
    info.postscript_family_blues = font.custom_ot_values.cff_family_blues.clone();
    info.postscript_family_other_blues = font.custom_ot_values.cff_family_other_blues.clone();
    info.postscript_stem_snap_h = font.custom_ot_values.cff_stem_snap_h.clone();
    info.postscript_stem_snap_v = font.custom_ot_values.cff_stem_snap_v.clone();
    // A master's own style names (a designspace master's) win.
    let style_key = |key: &str| {
        master
            .format_specific
            .get(key)
            .or_else(|| font.format_specific.get(key))
    };
    info.style_map_family_name = style_key(KEY_STYLE_MAP_FAMILY_NAME)
        .and_then(|x| x.as_str())
        .map(|x| x.to_string());
    info.style_map_style_name = style_key(KEY_STYLE_MAP_STYLE_NAME)
        .and_then(|x| x.as_str())
        .map(|x| match x {
            "Regular" => norad::fontinfo::StyleMapStyle::Regular,
            "Bold" => norad::fontinfo::StyleMapStyle::Bold,
            "Italic" => norad::fontinfo::StyleMapStyle::Italic,
            "Bold Italic" => norad::fontinfo::StyleMapStyle::BoldItalic,
            _ => norad::fontinfo::StyleMapStyle::Regular,
        });
    info.style_name = style_key(KEY_STYLE_NAME)
        .and_then(|x| x.as_str())
        .map(|x| x.to_string());
    info.trademark = font
        .names
        .trademark
        .get_default_or_fallback()
        .map(|x| x.to_string());
    info.units_per_em = Some((font.upm as u32).into());
    info.version_major = Some(font.version.0 as i32);
    info.version_minor = Some(font.version.1 as u32);
    // XXX WOFF
    info.x_height = get_metric(MetricType::XHeight);
}

macro_rules! load_metric {
    ($info:ident, $metrics:ident, $field:ident, $metric_type:expr) => {
        if let Some(v) = $info.$field {
            $metrics.insert($metric_type, v as i32);
        }
    };
}

pub(crate) fn load_master_info(master: &mut Master, info: &norad::FontInfo) {
    let metrics = &mut master.metrics;
    load_metric!(info, metrics, ascender, MetricType::Ascender);
    load_metric!(info, metrics, cap_height, MetricType::CapHeight);
    load_metric!(info, metrics, descender, MetricType::Descender);
    // A UFO stores the italic angle in the post convention, negative for a
    // right lean; babelfont stores the Glyphs convention, the opposite sign
    // (compare glyphsLib masters.py, `italic_angle = -master.italicAngle`).
    if let Some(v) = info.italic_angle {
        metrics.insert(MetricType::ItalicAngle, -v as i32);
    }
    load_metric!(info, metrics, x_height, MetricType::XHeight);
    load_metric!(
        info,
        metrics,
        open_type_hhea_ascender,
        MetricType::HheaAscender
    );
    load_metric!(
        info,
        metrics,
        open_type_hhea_descender,
        MetricType::HheaDescender
    );
    load_metric!(
        info,
        metrics,
        open_type_hhea_line_gap,
        MetricType::HheaLineGap
    );
    load_metric!(
        info,
        metrics,
        open_type_hhea_caret_offset,
        MetricType::HheaCaretOffset
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_typo_ascender,
        MetricType::TypoAscender
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_typo_descender,
        MetricType::TypoDescender
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_typo_line_gap,
        MetricType::TypoLineGap
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_win_ascent,
        MetricType::WinAscent
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_win_descent,
        MetricType::WinDescent
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_strikeout_position,
        MetricType::StrikeoutPosition
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_strikeout_size,
        MetricType::StrikeoutSize
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_subscript_x_offset,
        MetricType::SubscriptXOffset
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_subscript_x_size,
        MetricType::SubscriptXSize
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_subscript_y_offset,
        MetricType::SubscriptYOffset
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_subscript_y_size,
        MetricType::SubscriptYSize
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_superscript_x_offset,
        MetricType::SuperscriptXOffset
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_superscript_x_size,
        MetricType::SuperscriptXSize
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_superscript_y_offset,
        MetricType::SuperscriptYOffset
    );
    load_metric!(
        info,
        metrics,
        open_type_os2_superscript_y_size,
        MetricType::SuperscriptYSize
    );
    load_metric!(
        info,
        metrics,
        postscript_underline_position,
        MetricType::UnderlinePosition
    );
    load_metric!(
        info,
        metrics,
        postscript_underline_thickness,
        MetricType::UnderlineThickness
    );
    load_metric!(
        info,
        metrics,
        open_type_vhea_vert_typo_ascender,
        MetricType::VheaAscender
    );
    load_metric!(
        info,
        metrics,
        open_type_vhea_vert_typo_descender,
        MetricType::VheaDescender
    );
    load_metric!(
        info,
        metrics,
        open_type_vhea_vert_typo_line_gap,
        MetricType::VheaLineGap
    );
    load_metric!(
        info,
        metrics,
        open_type_vhea_caret_slope_rise,
        MetricType::VheaCaretSlopeRise
    );
    load_metric!(
        info,
        metrics,
        open_type_vhea_caret_slope_run,
        MetricType::VheaCaretSlopeRun
    );
    load_metric!(
        info,
        metrics,
        open_type_vhea_caret_offset,
        MetricType::VheaCaretOffset
    );
    if let Some(v) = &info.guidelines {
        for g in v.iter() {
            master.guides.push(g.into())
        }
    }
}

macro_rules! copy_name {
    ($font:ident, $ufo:ident, $field:ident) => {
        paste! {
            if let Some(v) = &$ufo.[<open_type_name_ $field>] {
                $font.names.$field = v.into();
            }
        }
    };
}

// The distinction between this and load_master_info is that this is font-wide info;
// in a .designspace loader, this would be called once per font, while load_master_info
// would be called once per source.
pub(crate) fn load_font_info(
    font: &mut Font,
    info: &norad::FontInfo,
    created: Option<DateTime<Utc>>,
) {
    if let Some(v) = &info.copyright {
        font.names.copyright = v.into();
    }
    if let Some(v) = &info.family_name {
        font.names.family_name = v.into();
    }
    if let Some(v) = &info.note {
        font.note = Some(v.clone());
    }
    if let Some(v) = &info.open_type_head_created {
        if let Ok(Some(date)) = NaiveDateTime::parse_from_str(v, "%Y/%m/%d %H:%M:%S")
            .map(|x| chrono::Utc.from_local_datetime(&x).single())
        {
            font.date = date;
        } else {
            font.date = created.unwrap_or_else(chrono::Utc::now);
        }
    }
    font.custom_ot_values.head_flags = info.open_type_head_flags.as_ref().map(|x| from_bitarray(x));
    font.custom_ot_values.head_lowest_rec_ppem =
        info.open_type_head_lowest_rec_ppem.map(|x| x as u16);
    font.custom_ot_values.os2_fs_selection = info
        .open_type_os2_selection
        .as_ref()
        .map(|x| from_bitarray(x));
    font.custom_ot_values.os2_fs_type = info.open_type_os2_type.as_ref().map(|x| from_bitarray(x));
    if let Some(v) = &info
        .open_type_os2_code_page_ranges
        .as_ref()
        .map(|x| from_bitarray(x))
    {
        let v: u64 = *v;
        // Split into top and bottom 32 bits
        font.custom_ot_values.os2_code_page_range1 = Some(v as u32);
        font.custom_ot_values.os2_code_page_range2 = Some((v >> 32) as u32);
    }
    if let Some(v) = &info.open_type_os2_unicode_ranges {
        // This one's a bit trickier since there are 128 bits split over four u32s.
        let mut ur1 = 0;
        let mut ur2 = 0;
        let mut ur3 = 0;
        let mut ur4 = 0;
        for bit in v.iter() {
            match bit {
                0..=31 => ur1 |= 1 << bit,
                32..=63 => ur2 |= 1 << (bit - 32),
                64..=95 => ur3 |= 1 << (bit - 64),
                96..=127 => ur4 |= 1 << (bit - 96),
                _ => {}
            }
        }
        font.custom_ot_values.os2_unicode_range1 = Some(ur1);
        font.custom_ot_values.os2_unicode_range2 = Some(ur2);
        font.custom_ot_values.os2_unicode_range3 = Some(ur3);
        font.custom_ot_values.os2_unicode_range4 = Some(ur4);
    }
    font.custom_ot_values.cff_blue_values = info.postscript_blue_values.clone();
    font.custom_ot_values.cff_other_blues = info.postscript_other_blues.clone();
    font.custom_ot_values.cff_family_blues = info.postscript_family_blues.clone();
    font.custom_ot_values.cff_family_other_blues = info.postscript_family_other_blues.clone();
    font.custom_ot_values.cff_stem_snap_h = info.postscript_stem_snap_h.clone();
    font.custom_ot_values.cff_stem_snap_v = info.postscript_stem_snap_v.clone();
    font.custom_ot_values.os2_vendor_id = info
        .open_type_os2_vendor_id
        .as_ref()
        .and_then(|x| Tag::from_str(x).ok());
    // XXX and much more
    if let Some(v) = &info.trademark {
        font.names.trademark = v.into();
    }

    if let Some(v) = info.units_per_em {
        font.upm = v.as_f64() as u16;
    }
    if let Some(v) = info.version_major {
        font.version.0 = v as u16;
    }
    if let Some(v) = info.version_minor {
        font.version.1 = v as u16;
    }
    if let Some(p) = &info.postscript_font_name {
        font.names.postscript_name = p.into();
    }
    copy_name!(font, info, description);
    copy_name!(font, info, designer_url);
    copy_name!(font, info, designer);
    copy_name!(font, info, license);
    copy_name!(font, info, license_url);
    copy_name!(font, info, manufacturer);
    copy_name!(font, info, manufacturer_url);
    copy_name!(font, info, sample_text);
    copy_name!(font, info, unique_id);
    copy_name!(font, info, version);
    stash_style_names(&mut font.format_specific, info);
    // UFO's "preferred" names are the typographic ones (IDs 16 and 17), not
    // WWS names or the legacy subfamily (ID 2)
    if let Some(v) = info.open_type_name_preferred_family_name.as_ref() {
        font.names.typographic_family = v.into();
    }
    if let Some(v) = info.open_type_name_preferred_subfamily_name.as_ref() {
        font.names.typographic_subfamily = v.into();
    }
    if let Some(v) = info.open_type_name_wws_family_name.as_ref() {
        font.names.wws_family_name = v.into();
    }
    if let Some(v) = info.open_type_name_wws_subfamily_name.as_ref() {
        font.names.wws_subfamily_name = v.into();
    }
    if let Some(v) = info.open_type_name_compatible_full_name.as_ref() {
        font.names.compatible_full_name = v.into();
    }
    if let Some(panose) = &info.open_type_os2_panose {
        font.custom_ot_values.os2_panose = Some([
            panose.family_type.try_into().unwrap_or(0),
            panose.serif_style.try_into().unwrap_or(0),
            panose.weight.try_into().unwrap_or(0),
            panose.proportion.try_into().unwrap_or(0),
            panose.contrast.try_into().unwrap_or(0),
            panose.stroke_variation.try_into().unwrap_or(0),
            panose.arm_style.try_into().unwrap_or(0),
            panose.letterform.try_into().unwrap_or(0),
            panose.midline.try_into().unwrap_or(0),
            panose.x_height.try_into().unwrap_or(0),
        ]);
    }
}

pub(crate) fn load_kerning(master: &mut Master, kerning: &norad::Kerning) {
    for (left, right_dict) in kerning.iter() {
        for (right, value) in right_dict.iter() {
            let left_maybe_group = if let Some(group) = left.strip_prefix("public.kern1.") {
                format!("@{:}", group)
            } else {
                left.to_string()
            };
            let right_maybe_group = if let Some(group) = right.strip_prefix("public.kern2.") {
                format!("@{:}", group)
            } else {
                right.to_string()
            };
            master.kerning.insert(
                (left_maybe_group.into(), right_maybe_group.into()),
                *value as i16,
            );
        }
    }
}

fn from_bitarray<T>(v: &[u8]) -> T
where
    T: num_traits::PrimInt + num_traits::FromPrimitive,
{
    let mut result = T::zero();
    for bit in v.iter() {
        result = result | (T::one() << usize::from(*bit));
    }
    result
}

fn to_bitarray<T>(v: T) -> Vec<u8>
where
    T: num_traits::PrimInt,
{
    let mut bits = vec![];
    let mut bit_index = 0;
    let mut value = v;
    while !value.is_zero() {
        if (value & T::one()) == T::one() {
            bits.push(bit_index);
        }
        value = value >> 1;
        bit_index += 1;
    }
    bits
}
pub(crate) fn load_kern_groups(
    groups: &norad::Groups,
) -> (
    IndexMap<SmolStr, Vec<SmolStr>>,
    IndexMap<SmolStr, Vec<SmolStr>>,
) {
    let mut first: IndexMap<SmolStr, Vec<SmolStr>> = IndexMap::new();
    let mut second: IndexMap<SmolStr, Vec<SmolStr>> = IndexMap::new();
    for (name, members) in groups.iter() {
        if let Some(first_name) = name.strip_prefix("public.kern1.") {
            first.insert(
                SmolStr::from(first_name),
                members.iter().map(|x| SmolStr::from(x.as_str())).collect(),
            );
        } else if let Some(second_name) = name.strip_prefix("public.kern2.") {
            second.insert(
                SmolStr::from(second_name),
                members.iter().map(|x| SmolStr::from(x.as_str())).collect(),
            );
        }
    }
    (first, second)
}

pub(crate) fn load_glyphs(font: &mut Font, ufo: &norad::Font) {
    let categories = ufo.lib.get(KEY_CATEGORIES).and_then(|x| x.as_dictionary());
    let psnames = ufo.lib.get(KEY_PSNAMES).and_then(|x| x.as_dictionary());
    let skipped: HashSet<String> = ufo
        .lib
        .get(KEY_SKIP_EXPORT)
        .and_then(|x| x.as_array())
        .cloned()
        .unwrap_or_default()
        .iter()
        .flat_map(|x| x.as_string())
        .map(|x| x.to_string())
        .collect();
    let glyphorder: Vec<String> = ufo
        .lib
        .get("public.glyphOrder")
        .and_then(|x| x.as_array())
        .unwrap_or(&vec![])
        .iter()
        .flat_map(|x| x.as_string())
        .map(|x| x.to_string())
        .collect();
    let mut order: Vec<String> = vec![];
    let mut ufo_names: Vec<String> = ufo.iter_names().map(|x| x.to_string()).collect();
    if ufo_names.contains(&".notdef".to_string()) {
        order.push(".notdef".to_string());
        ufo_names.retain(|x| x != ".notdef");
    }
    for name in glyphorder {
        if !ufo_names.contains(&name) {
            continue;
        }
        ufo_names.retain(|x| x != &name);
        order.push(name);
    }
    order.append(&mut ufo_names);

    for glyphname in order {
        if let Some(glyph) = ufo.get_glyph(glyphname.as_str()) {
            let cat = if let Some(cats) = categories {
                match cats.get(&glyphname).and_then(|x| x.as_string()) {
                    Some("base") => GlyphCategory::Base,
                    Some("mark") => GlyphCategory::Mark,
                    Some("ligature") => GlyphCategory::Ligature,
                    _ => GlyphCategory::Base,
                }
            } else {
                GlyphCategory::Base
            };
            let production_name = psnames
                .and_then(|x| x.get(&glyphname))
                .and_then(|x| x.as_string())
                .map(|x| x.into());
            font.glyphs.push(Glyph {
                name: SmolStr::from(glyphname.as_str()),
                category: cat,
                production_name,
                codepoints: glyph.codepoints.iter().map(|x| x as u32).collect(),
                layers: vec![],
                exported: !skipped.contains(&glyphname),
                direction: None,
                format_specific: Default::default(),
                component_axes: Default::default(),
            })
        }
    }
    add_uvs_sequences(font, ufo);
}

/// The font's Unicode variation sequences as UFO's lib states them: selector →
/// codepoint → glyph name, the codepoints in hexadecimal
fn save_uvs_sequences(font: &Font, lib: &mut norad::Plist) {
    if font.variation_sequences.is_empty() {
        return;
    }
    let mut nested = serde_json::Map::new();
    for (&(selector, codepoint), glyph) in &font.variation_sequences {
        if let serde_json::Value::Object(records) = nested
            .entry(format!("{selector:04X}"))
            .or_insert_with(|| serde_json::Value::Object(serde_json::Map::new()))
        {
            records.insert(
                format!("{codepoint:04X}"),
                serde_json::Value::String(glyph.to_string()),
            );
        }
    }
    let entry = serde_json::json!({ "public.unicodeVariationSequences": nested });
    if let Ok(entry) = serde_json::from_value::<norad::Plist>(entry) {
        for (key, value) in entry {
            lib.insert(key, value);
        }
    }
}

fn add_uvs_sequences(font: &mut Font, ufo: &norad::Font) {
    if let Some(uvs) = ufo
        .lib
        .get("public.unicodeVariationSequences")
        .and_then(|x| x.as_dictionary())
    {
        // Lasciate ogne speranza, voi ch'intrate
        for (selector_s, records_plist) in uvs.iter() {
            if let Ok(selector) = u32::from_str_radix(selector_s, 16) {
                if let Some(records) = records_plist.as_dictionary() {
                    for (codepoint_s, glyphname_plist) in records {
                        if let Ok(codepoint) = u32::from_str_radix(codepoint_s, 16) {
                            if let Some(glyphname) = glyphname_plist.as_string() {
                                font.variation_sequences
                                    .insert((selector, codepoint), glyphname.into());
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    #![allow(clippy::unwrap_used)]

    use super::*;
    use pretty_assertions::assert_eq;

    const GLYPH_ORDER_KEY: &str = "public.glyphOrder";

    pub(crate) fn ufo_semantic_test(ufo1: &norad::Font, ufo2: &norad::Font, do_metadata: bool) {
        assert_eq!(
            ufo1.default_layer().len(),
            ufo2.default_layer().len(),
            "Number of glyphs differs"
        );
        assert_eq!(
            ufo1.layers.len(),
            ufo2.layers.len(),
            "Number of layers differs. Layer names 1: {:?}, Layer names 2: {:?}",
            ufo1.layers
                .iter()
                .map(|layer| layer.name())
                .collect::<Vec<_>>(),
            ufo2.layers
                .iter()
                .map(|layer| layer.name())
                .collect::<Vec<_>>()
        );
        for (layer1, layer2) in ufo1.layers.iter().zip(ufo2.layers.iter()) {
            assert_eq!(
                layer1.len(),
                layer2.len(),
                "Number of glyphs in layer {} differs",
                layer1.name()
            );
        }
        for name in ufo1.iter_names().map(|name| name.to_string()) {
            let glyph1 = ufo1.get_glyph(&name).unwrap();
            let glyph2 = ufo2.get_glyph(&name).unwrap();
            assert_eq!(glyph1, glyph2, "Glyph {} differs", name);
        }

        let skipped1: HashSet<String> = ufo1
            .lib
            .get(KEY_SKIP_EXPORT)
            .and_then(|value| value.as_array())
            .cloned()
            .unwrap_or_default()
            .iter()
            .flat_map(|value| value.as_string())
            .map(|value| value.to_string())
            .collect();
        let skipped2: HashSet<String> = ufo2
            .lib
            .get(KEY_SKIP_EXPORT)
            .and_then(|value| value.as_array())
            .cloned()
            .unwrap_or_default()
            .iter()
            .flat_map(|value| value.as_string())
            .map(|value| value.to_string())
            .collect();
        assert_eq!(skipped1, skipped2, "skipExportGlyphs differs");
        assert_eq!(ufo1.kerning, ufo2.kerning);
        if !do_metadata {
            return;
        }

        let order1: Vec<String> = ufo1
            .lib
            .get(GLYPH_ORDER_KEY)
            .and_then(|value| value.as_array())
            .unwrap_or(&vec![])
            .iter()
            .flat_map(|value| value.as_string())
            .map(|value| value.to_string())
            .filter(|name| !skipped1.contains(name))
            .collect();
        let order2: Vec<String> = ufo2
            .lib
            .get(GLYPH_ORDER_KEY)
            .and_then(|value| value.as_array())
            .unwrap_or(&vec![])
            .iter()
            .flat_map(|value| value.as_string())
            .map(|value| value.to_string())
            .filter(|name| !skipped2.contains(name))
            .collect();
        assert_eq!(order1, order2, "Glyph order differs");

        let categories1 = ufo1
            .lib
            .get(KEY_CATEGORIES)
            .and_then(|value| value.as_dictionary())
            .map(|dictionary| {
                dictionary
                    .iter()
                    .map(|(key, value)| {
                        (
                            key.to_string(),
                            value.as_string().unwrap_or_default().to_string(),
                        )
                    })
                    .collect::<HashMap<_, _>>()
            })
            .unwrap_or_default();
        let categories2 = ufo2
            .lib
            .get(KEY_CATEGORIES)
            .and_then(|value| value.as_dictionary())
            .map(|dictionary| {
                dictionary
                    .iter()
                    .map(|(key, value)| {
                        (
                            key.to_string(),
                            value.as_string().unwrap_or_default().to_string(),
                        )
                    })
                    .collect::<HashMap<_, _>>()
            })
            .unwrap_or_default();
        let base = "base".to_string();
        for (glyph, category) in &categories1 {
            let category2 = categories2.get(glyph).unwrap_or(&base);
            assert_eq!(category, category2, "Category for glyph {} differs", glyph);
        }

        let all_keys = ufo1
            .lib
            .keys()
            .chain(ufo2.lib.keys())
            .filter(|key| {
                *key != KEY_SKIP_EXPORT && *key != GLYPH_ORDER_KEY && *key != KEY_CATEGORIES
            })
            .collect::<HashSet<_>>();
        for key in all_keys {
            let value1 = ufo1
                .lib
                .get(key)
                .unwrap_or_else(|| panic!("Lib key {} is missing in first UFO", key));
            let value2 = ufo2
                .lib
                .get(key)
                .unwrap_or_else(|| panic!("Lib key {} is missing in second UFO", key));
            assert_eq!(value1, value2, "Lib value for key {} differs", key);
        }
        assert_eq!(ufo1.groups, ufo2.groups);
        assert_eq!(ufo1.features.trim(), ufo2.features.trim());
        assert_eq!(ufo1.data, ufo2.data);
        assert_eq!(ufo1.images, ufo2.images);
        assert_eq!(ufo1.font_info, ufo2.font_info);
    }

    #[test]
    fn test_italic_angle_conventions() {
        // A UFO stores the angle in the post convention, negative for a right
        // lean; babelfont stores the Glyphs convention, the opposite sign, the
        // same as glyphsLib's `-master.italicAngle`. Ibarra Real Nova Italic
        // says -22 in fontinfo, and its shipped binary says -22 in post; kept
        // raw, both of babelfont's compile paths shipped +22, a back-slant.
        let font = crate::load("resources/IbarraRealNova-Italic.ufo").unwrap();
        assert_eq!(
            font.masters[0].metrics.get(&crate::MetricType::ItalicAngle),
            Some(&22),
            "internally the Glyphs convention: right lean is positive"
        );

        // And back out in the UFO's own convention, unchanged.
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().join("ital.ufo");
        font.save(out.clone()).unwrap();
        let reloaded = crate::load(out).unwrap();
        assert_eq!(
            reloaded.masters[0]
                .metrics
                .get(&crate::MetricType::ItalicAngle),
            Some(&22),
            "a UFO round-trip must preserve the angle"
        );
    }

    #[test]
    fn test_preferred_names_are_typographic() {
        let mut ufo = norad::Font::load("resources/IbarraRealNova-Italic.ufo").unwrap();
        ufo.font_info.open_type_name_preferred_family_name = Some("Ibarra".into());
        ufo.font_info.open_type_name_preferred_subfamily_name = Some("Real Italic".into());
        ufo.font_info.open_type_name_wws_family_name = Some("Ibarra WWS".into());
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("names.ufo");
        ufo.save(&path).unwrap();
        let font = crate::load(&path).unwrap();
        let name = |n: &crate::I18NDictionary| n.get_default().cloned();
        assert_eq!(
            name(&font.names.typographic_family).as_deref(),
            Some("Ibarra")
        );
        assert_eq!(
            name(&font.names.typographic_subfamily).as_deref(),
            Some("Real Italic")
        );
        assert_eq!(
            name(&font.names.wws_family_name).as_deref(),
            Some("Ibarra WWS")
        );
        assert_eq!(name(&font.names.preferred_subfamily_name), None);
        // and back
        let info = as_norad(&font, 0).unwrap().font_info;
        assert_eq!(
            info.open_type_name_preferred_family_name.as_deref(),
            Some("Ibarra")
        );
        assert_eq!(
            info.open_type_name_preferred_subfamily_name.as_deref(),
            Some("Real Italic")
        );
        assert_eq!(
            info.open_type_name_wws_family_name.as_deref(),
            Some("Ibarra WWS")
        );
    }

    #[test]
    fn test_variation_sequences_round_trip() {
        let mut font = crate::load("resources/IbarraRealNova-Italic.ufo").unwrap();
        font.variation_sequences
            .insert((0xFE00, 0x41), smol_str::SmolStr::from("A"));
        font.variation_sequences
            .insert((0xE0100, 0x1F600), smol_str::SmolStr::from("B"));
        let ufo = as_norad(&font, 0).unwrap();
        let lib = ufo
            .lib
            .get("public.unicodeVariationSequences")
            .and_then(|v| v.as_dictionary())
            .unwrap();
        let fe00 = lib.get("FE00").and_then(|v| v.as_dictionary()).unwrap();
        assert_eq!(fe00.get("0041").and_then(|v| v.as_string()), Some("A"));
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().join("uvs.ufo");
        font.save(out.clone()).unwrap();
        let reloaded = crate::load(out).unwrap();
        assert_eq!(reloaded.variation_sequences, font.variation_sequences);
    }

    #[test]
    fn test_roundtrip_1() {
        let there = crate::load("resources/NotoSans-LightItalic.ufo").unwrap();
        assert!(there.masters.len() == 1);
        let backagain = as_norad(&there, 0).unwrap();
        let once_more = norad::Font::load("resources/NotoSans-LightItalic.ufo").unwrap();
        assert_eq!(there.glyphs.len(), backagain.default_layer().len());
        assert_eq!(
            backagain.default_layer().len(),
            once_more.default_layer().len()
        );
        let backagain_layer = backagain.default_layer();
        let once_more_layer = once_more.default_layer();
        for name in there.glyphs.iter().map(|x| x.name.as_str()) {
            let g1 = backagain_layer.get_glyph(name).unwrap();
            let g2 = once_more_layer.get_glyph(name).unwrap();
            assert_eq!(g1, g2, "Glyph {} differs", name);
        }

        assert_eq!(
            there.custom_ot_values.os2_unicode_range2,
            Some(
                // 0,1,2,3,4,13,30 should be set
                1 << 0 | 1 << 1 | 1 << 2 | 1 << 3 | 1 << 4 | 1 << 13 | 1 << 30
            )
        );

        assert_eq!(backagain.lib, once_more.lib);
        assert_eq!(backagain.groups, once_more.groups);
        assert_eq!(backagain.kerning, once_more.kerning);
        assert_eq!(backagain.features.trim(), once_more.features.trim());
        assert_eq!(backagain.data, once_more.data);
        assert_eq!(backagain.images, once_more.images);
        assert_eq!(backagain.font_info, once_more.font_info);
    }

    /// Every text file below `root`, by path relative to it.
    pub(crate) fn read_tree(root: &str) -> HashMap<String, String> {
        fn walk(root: &FsPath, dir: &FsPath, out: &mut HashMap<String, String>) {
            for entry in fs::read_dir(dir).unwrap().flatten() {
                let path = entry.path();
                if path.is_dir() {
                    walk(root, &path, out);
                } else if let Ok(text) = fs::read_to_string(&path) {
                    let relative = path.strip_prefix(root).unwrap();
                    out.insert(path_string(relative), text);
                }
            }
        }
        let mut out = HashMap::new();
        walk(FsPath::new(root), FsPath::new(root), &mut out);
        out
    }

    fn full_save(font: &Font, layout: &UfoLayout) -> UfoEntries {
        let request = UfoSaveRequest {
            layout: Some(layout),
            ..Default::default()
        };
        match save_entries(font, request).unwrap() {
            UfoSave::Entries(entries) => entries,
            UfoSave::NeedsFullSave(reason) => panic!("full save refused: {reason}"),
        }
    }

    fn as_map(entries: &UfoEntries) -> HashMap<String, String> {
        entries.files.clone().into_iter().collect()
    }

    const BOLD: &str = "resources/IbarraRealNova-Bold.ufo";

    #[test]
    fn save_entries_round_trip_keeps_file_names_and_is_stable() {
        let original = read_tree(BOLD);
        let layout = UfoLayout::from_entries(&original).unwrap();
        let font = load_entries(PathBuf::from("Bold.ufo"), &original).unwrap();
        let saved = full_save(&font, &layout);

        // Every glyph file, background layer included, keeps its path.
        for path in original.keys().filter(|p| p.ends_with(".glif")) {
            assert!(saved.files.contains_key(path), "{path} was renamed");
        }
        assert_eq!(
            saved.files.keys().filter(|p| p.ends_with(".glif")).count(),
            original.keys().filter(|p| p.ends_with(".glif")).count()
        );

        // Reloading gives the same font...
        let reloaded = load_entries(PathBuf::from("Bold.ufo"), &as_map(&saved)).unwrap();
        assert_eq!(reloaded.glyphs, font.glyphs);
        assert_eq!(reloaded.features.to_fea(), font.features.to_fea());
        assert_eq!(reloaded.masters[0].kerning, font.masters[0].kerning);
        assert_eq!(reloaded.first_kern_groups, font.first_kern_groups);
        assert_eq!(reloaded.names, font.names);

        // ...and saving that again writes the same bytes.
        let layout = UfoLayout::from_entries(&as_map(&saved)).unwrap();
        assert_eq!(full_save(&reloaded, &layout).files, saved.files);
    }

    #[test]
    fn a_glyphs_layers_have_their_own_ids() {
        let font = load_entries(PathBuf::from("Bold.ufo"), &read_tree(BOLD)).unwrap();
        let glyph = font.glyphs.iter().find(|g| g.layers.len() > 1).unwrap();
        let ids: HashSet<_> = glyph.layers.iter().map(|l| l.id.clone()).collect();
        assert_eq!(ids.len(), glyph.layers.len(), "{}", glyph.name);
        for layer in &glyph.layers {
            let master = match &layer.master {
                LayerType::DefaultForMaster(id) | LayerType::AssociatedWithMaster(id) => id,
                LayerType::FreeFloating => panic!("{} has a free layer", glyph.name),
            };
            assert_eq!(master, UFO_MASTER_ID);
        }
    }

    #[test]
    fn untouched_lib_and_features_are_saved_as_they_were() {
        let original = read_tree(BOLD);
        let layout = UfoLayout::from_entries(&original).unwrap();
        let font = load_entries(PathBuf::from("Bold.ufo"), &original).unwrap();
        let saved = full_save(&font, &layout);
        assert_eq!(saved.files["features.fea"], original["features.fea"]);
        let lib = |text: &str| plist::from_bytes::<norad::Plist>(text.as_bytes());
        assert_eq!(
            lib(&saved.files["lib.plist"]).unwrap(),
            lib(&original["lib.plist"]).unwrap()
        );
    }

    #[test]
    fn glyph_metadata_edits_reach_the_lib() {
        let original = read_tree(BOLD);
        let layout = UfoLayout::from_entries(&original).unwrap();
        let mut font = load_entries(PathBuf::from("Bold.ufo"), &original).unwrap();
        // Not .notdef, which loading always puts first.
        let first = font.glyphs[1].name.clone();
        let glyph = font.glyphs.get_mut(&first).unwrap();
        glyph.production_name = Some("uniE000".into());
        glyph.exported = false;
        glyph.category = GlyphCategory::Mark;
        let moved = font.glyphs.remove(1);
        font.glyphs.push(moved);

        let saved = full_save(&font, &layout);
        let reloaded = load_entries(PathBuf::from("Bold.ufo"), &as_map(&saved)).unwrap();
        let glyph = reloaded.glyphs.get(&first).unwrap();
        assert_eq!(glyph.production_name.as_deref(), Some("uniE000"));
        assert!(!glyph.exported);
        assert_eq!(glyph.category, GlyphCategory::Mark);
        assert_eq!(reloaded.glyphs.last().unwrap().name, first);
    }

    #[test]
    fn partial_save_writes_only_the_glyph_and_matches_a_full_save() {
        let original = read_tree(BOLD);
        let layout = UfoLayout::from_entries(&original).unwrap();
        let mut font = load_entries(PathBuf::from("Bold.ufo"), &original).unwrap();
        let name = "A".to_string();
        font.glyphs.get_mut("A").unwrap().layers[0].width += 10.0;

        let full = full_save(&font, &layout);
        let request = UfoSaveRequest {
            layout: Some(&layout),
            glyphs: Some(std::slice::from_ref(&name)),
            font_level: false,
        };
        let UfoSave::Entries(partial) = save_entries(&font, request).unwrap() else {
            panic!("partial save refused");
        };
        let mut expected: Vec<String> = layout.glyph_paths("A");
        expected.sort();
        assert_eq!(partial.files.keys().cloned().collect::<Vec<_>>(), expected);
        for (path, text) in &partial.files {
            assert_eq!(text, &full.files[path]);
        }
        assert!(partial.removed.is_empty());

        // With font-level files, the top-level files come too, as in a full save.
        let request = UfoSaveRequest {
            font_level: true,
            ..request
        };
        let UfoSave::Entries(partial) = save_entries(&font, request).unwrap() else {
            panic!("partial save refused");
        };
        for file in [
            "metainfo.plist",
            "fontinfo.plist",
            "lib.plist",
            "features.fea",
        ] {
            assert_eq!(partial.files[file], full.files[file], "{file}");
        }
        assert!(!partial.files.contains_key("layercontents.plist"));
    }

    #[test]
    fn partial_save_refuses_glyph_set_changes() {
        let original = read_tree(BOLD);
        let layout = UfoLayout::from_entries(&original).unwrap();
        let mut font = load_entries(PathBuf::from("Bold.ufo"), &original).unwrap();
        let mut added = font.glyphs.get("A").unwrap().clone();
        added.name = "A.new".into();
        font.glyphs.push(added);
        for name in ["A.new", "no-such-glyph"] {
            let names = [name.to_string()];
            let request = UfoSaveRequest {
                layout: Some(&layout),
                glyphs: Some(&names),
                font_level: false,
            };
            assert!(matches!(
                save_entries(&font, request).unwrap(),
                UfoSave::NeedsFullSave(_)
            ));
        }
    }

    #[test]
    fn feature_includes_survive_until_the_features_are_edited() {
        let mut entries = read_tree(BOLD);
        entries.insert(
            "features.fea".into(),
            "include(features/classes.fea);\nfeature liga {\n    sub f i by fi;\n} liga;\n".into(),
        );
        entries.insert(
            "features/classes.fea".into(),
            "@letters = [a b c];\n".into(),
        );
        let layout = UfoLayout::from_entries(&entries).unwrap();
        let mut font = load_entries(PathBuf::from("Bold.ufo"), &entries).unwrap();
        assert!(font.features.to_fea().contains("@letters"));

        let saved = full_save(&font, &layout);
        assert_eq!(saved.files["features.fea"], entries["features.fea"]);
        assert_eq!(
            saved.files["features/classes.fea"],
            entries["features/classes.fea"]
        );

        font.features = Features::from_fea("@letters = [a b];\n");
        let saved = full_save(&font, &layout);
        assert!(!saved.files["features.fea"].contains("include"));
        assert!(saved.files["features.fea"].contains("@letters = [a b]"));
        assert!(!saved.files.contains_key("features/classes.fea"));
    }

    #[test]
    fn glyphs_load_from_their_glifs_alone() {
        let original = read_tree(BOLD);
        let layout = UfoLayout::from_entries(&original).unwrap();
        let font = load_entries(PathBuf::from("Bold.ufo"), &original).unwrap();
        let master_id = font.masters[0].id.clone();
        let with_background = font
            .glyphs
            .iter()
            .find(|g| g.layers.len() > 1)
            .unwrap()
            .name
            .to_string();
        let names = vec!["A".to_string(), with_background];
        let glifs: HashMap<String, String> = names
            .iter()
            .flat_map(|name| layout.glyph_paths(name))
            .map(|path| (path.clone(), original[&path].clone()))
            .collect();
        let glyphs = load_glyphs_from_glifs(&layout, &glifs, &names, &master_id).unwrap();
        for glyph in glyphs {
            let full = font.glyphs.get(&glyph.name).unwrap();
            assert_eq!(glyph.layers, full.layers, "{}", glyph.name);
            assert_eq!(glyph.codepoints, full.codepoints);
        }
        assert_eq!(layout.glyph_at(&layout.glyph_paths("A")[0]), Some("A"));
    }
}

use crate::{
    common::FormatSpecific,
    glyph::{self, glyphs::glyph_to_glyphs},
    i18ndictionary::I18NDictionary,
    names::Names,
    Axis, BabelfontError, CustomOTValues, Font, GlyphList, Layer, LayerType, Master, Tag,
};
use fontdrasil::coords::{DesignCoord, DesignLocation, UserCoord};
use glyphslib::glyphs3::{self, Property};
use indexmap::IndexMap;
use serde_json::json;
use smol_str::SmolStr;
use std::{
    collections::{BTreeMap, HashMap},
    fs,
    path::PathBuf,
    str::FromStr,
};

mod customparameters;

use customparameters::{enabled_cp_value, export_font_level_cps, interpret_custom_parameters};

pub(crate) type UserData = BTreeMap<SmolStr, glyphslib::Plist>;

pub(crate) const KEY_ALIGNMENT: &str = "com.schriftgestalt.Glyphs.alignment";
pub(crate) const KEY_ANCHOR_LOCKED: &str = "com.schriftgestalt.Glyphs.anchorLocked";
pub(crate) const KEY_ANCHOR_ORIENTATION: &str = "com.schriftgestalt.Glyphs.anchorOrientation";
pub(crate) const KEY_ANNOTATIONS: &str = "com.schriftgestalt.Glyphs.annotations";
pub(crate) const KEY_APP_VERSION: &str = "com.schriftgestalt.Glyphs.appVersion";
pub(crate) const KEY_ATTR: &str = "com.schriftgestalt.Glyphs.attr";
pub(crate) const KEY_FIP001_BOOLEAN: &str = "fip001-boolean";
pub(crate) const KEY_COLOR_LABEL: &str = "com.schriftgestalt.Glyphs.colorLabel";
pub(crate) const KEY_COMPONENT_ANCHOR: &str = "com.schriftgestalt.Glyphs.componentAnchor";
pub(crate) const KEY_COMPONENT_ANCHOR_TO: &str = "com.schriftgestalt.Glyphs.componentAnchorTo";
pub(crate) const KEY_COMPONENT_MASTER_ID: &str = "com.schriftgestalt.Glyphs.componentMasterId";
pub(crate) const KEY_COMPONENT_ORIENTATION: &str = "com.schriftgestalt.Glyphs.componentOrientation";
pub(crate) const KEY_COMPONENT_TRAVERSE_ANCHORS: &str =
    "com.schriftgestalt.Glyphs.componentTraverseAnchors";
pub(crate) const KEY_COMPONENT_ALIGNMENT_EXPLICIT: &str =
    "com.schriftgestalt.Glyphs.componentAlignmentExplicit";
pub(crate) const KEY_COMPONENT_LOCKED: &str = "com.schriftgestalt.Glyphs.componentLocked";
pub(crate) const KEY_CUSTOM_PARAMETERS: &str = "com.schriftgestalt.Glyphs.customParameters.";
pub(crate) const KEY_DISPLAY_STRINGS: &str = "com.schriftgestalt.Glyphs.displayStrings";
pub(crate) const KEY_FORMAT_VERSION: &str = "com.schriftgestalt.Glyphs.formatVersion";
/// The Glyphs file format version to save in, when it is not the one the
/// font was read in ([`KEY_FORMAT_VERSION`]): 3 or 4.
pub const KEY_SAVE_FORMAT_VERSION: &str = "com.schriftgestalt.Glyphs.saveFormatVersion";
pub(crate) const KEY_ICON_NAME: &str = "com.schriftgestalt.Glyphs.iconName";
pub(crate) const KEY_INSTANCE_EXPORTS: &str = "com.schriftgestalt.Glyphs.exports";
pub(crate) const KEY_INSTANCE_EXPORT_TYPE: &str = "com.schriftgestalt.Glyphs.instance.type";
pub(crate) const KEY_INSTANCE_INTERPOLATIONS: &str =
    "com.schriftgestalt.Glyphs.instanceInterpolations";
pub(crate) const KEY_INSTANCE_MANUAL_INTERPOLATION: &str =
    "com.schriftgestalt.Glyphs.manualInterpolation";
pub(crate) const KEY_IS_BOLD: &str = "com.schriftgestalt.Glyphs.isBold";
pub(crate) const KEY_IS_ITALIC: &str = "com.schriftgestalt.Glyphs.isItalic";
pub(crate) const KEY_KERNING_CONTEXT: &str = "com.schriftgestalt.Glyphs.kerningContext";
pub(crate) const KEY_KERNING_RTL: &str = "com.schriftgestalt.Glyphs.kerningRTL";
pub(crate) const KEY_KERNING_VERTICAL: &str = "com.schriftgestalt.Glyphs.kerningVertical";
pub(crate) const KEY_LAYER_HINTS: &str = "com.schriftgestalt.Glyphs.layerHints";
pub(crate) const KEY_LAYER_IMAGE: &str = "com.schriftgestalt.Glyphs.layerBackgroundImage";
pub(crate) const KEY_MASTER_VISIBLE: &str = "com.schriftgestalt.Glyphs.visible";
pub(crate) const KEY_METRIC_BOTTOM: &str = "com.schriftgestalt.Glyphs.metricBottom";
pub(crate) const KEY_METRIC_LEFT: &str = "com.schriftgestalt.Glyphs.metricLeft";
pub(crate) const KEY_METRIC_RIGHT: &str = "com.schriftgestalt.Glyphs.metricRight";
pub(crate) const KEY_METRIC_TOP: &str = "com.schriftgestalt.Glyphs.metricTop";
pub(crate) const KEY_METRIC_VERT_ORIGIN: &str = "com.schriftgestalt.Glyphs.metricVertOrigin";
pub(crate) const KEY_METRIC_VERT_WIDTH: &str = "com.schriftgestalt.Glyphs.metricVertWidth";
pub(crate) const KEY_METRIC_WIDTH: &str = "com.schriftgestalt.Glyphs.metricWidth";
pub(crate) const KEY_NODE_LOCKED: &str = "com.schriftgestalt.Glyphs.node.locked";
pub(crate) const KEY_NODE_TANGENT: &str = "com.schriftgestalt.Glyphs.node.tangent";
pub(crate) const KEY_NODE_ORIENTATION: &str = "com.schriftgestalt.Glyphs.node.orientation";
pub(crate) const KEY_NODE_HOI: &str = "com.schriftgestalt.Glyphs.node.hoi";
pub(crate) const KEY_NUMBER_NAMES: &str = "com.schriftgestalt.Glyphs.numberNames";
pub(crate) const KEY_NUMBER_VALUES: &str = "com.schriftgestalt.Glyphs.numberValues";
pub(crate) const KEY_SETTINGS: &str = "com.schriftgestalt.Glyphs.settings";
/// The order of a font's or instance's properties as read, which a Glyphs 3
/// file keeps (Glyphs 4 sorts them).
const KEY_PROPERTY_ORDER: &str = "com.schriftgestalt.Glyphs.propertyOrder";
/// A font's classes and feature prefixes as read, when a name is used more
/// than once: the font's features keep one per name.
const KEY_REPEATED_CLASSES: &str = "com.schriftgestalt.Glyphs.repeatedClasses";
const KEY_REPEATED_PREFIXES: &str = "com.schriftgestalt.Glyphs.repeatedPrefixes";

/// Stores `entries` as read when a name repeats.
fn stash_repeats<T: serde::Serialize>(
    format_specific: &mut FormatSpecific,
    key: &str,
    entries: &[T],
    name: impl Fn(&T) -> &str,
) {
    let mut seen = std::collections::HashSet::new();
    if entries.iter().any(|entry| !seen.insert(name(entry))) {
        format_specific.insert_json(key, &entries);
    }
}

/// The entries as read, if the font's features still hold what they did
/// (the last entry of each name), else `current`.
fn restore_repeats<T: serde::de::DeserializeOwned + PartialEq + Clone>(
    format_specific: &FormatSpecific,
    key: &str,
    current: Vec<T>,
    name: impl Fn(&T) -> &str,
) -> Vec<T> {
    let Some(stashed) = format_specific
        .get(key)
        .and_then(|v| serde_json::from_value::<Vec<T>>(v.clone()).ok())
    else {
        return current;
    };
    let mut last: Vec<T> = Vec::new();
    for entry in &stashed {
        match last.iter_mut().find(|l| name(l) == name(entry)) {
            Some(slot) => *slot = entry.clone(),
            None => last.push(entry.clone()),
        }
    }
    if last == current {
        stashed
    } else {
        current
    }
}

/// Properties babelfont does not model, such as
/// `variationsPostScriptNamePrefix`, kept to be written back.
const KEY_OTHER_PROPERTIES: &str = "com.schriftgestalt.Glyphs.otherProperties";
/// The names of metrics that have a type as well (a filtered x-height named
/// `smallCapHeight`), by the metric's key in the masters.
const KEY_METRIC_NAMES: &str = "com.schriftgestalt.Glyphs.metricNames";

/// A Glyphs metric's key in the masters' metrics (see `load_master`).
fn metric_key(metric: &glyphs3::Metric) -> Option<String> {
    let typ = crate::MetricType::from(&metric.metric_type?);
    Some(match &metric.filter {
        Some(filter) => format!("{} (filter {})", typ.as_str(), filter),
        None => typ.as_str().to_string(),
    })
}

/// Records the order of the properties as read, and the ones babelfont does
/// not model.
fn stash_property_order(format_specific: &mut FormatSpecific, properties: &[Property]) {
    let order: Vec<String> = properties.iter().filter_map(Property::key_name).collect();
    if !order.is_empty() {
        format_specific.insert_json(KEY_PROPERTY_ORDER, &order);
    }
    let other: Vec<&glyphslib::Plist> = properties
        .iter()
        .filter_map(|p| match p {
            Property::Junk(plist) => Some(plist),
            _ => None,
        })
        .collect();
    if !other.is_empty() {
        format_specific.insert_json(KEY_OTHER_PROPERTIES, &other);
    }
}

/// Puts properties back as they were read: in their order, new ones after,
/// in the form they had (`postscriptFullNames`, localized, or the singular
/// `postscriptFullName`), with the ones babelfont does not model.
fn restore_property_order(
    format_specific: &FormatSpecific,
    names: &Names,
    properties: &mut Vec<Property>,
) {
    let order: Vec<String> = format_specific.get_parse_or(KEY_PROPERTY_ORDER, Vec::new());
    if order.iter().any(|key| key == "postscriptFullNames") {
        for property in properties.iter_mut() {
            if matches!(
                property,
                Property::SingularProperty {
                    key: glyphs3::SingularPropertyKey::PostscriptFullName,
                    ..
                }
            ) {
                *property = Property::LocalizedProperty {
                    key: glyphs3::LocalizedPropertyKey::PostscriptFullNames,
                    values: (&names.full_name).into(),
                };
            }
        }
    }
    let other: Vec<glyphslib::Plist> =
        format_specific.get_parse_or(KEY_OTHER_PROPERTIES, Vec::new());
    properties.extend(other.into_iter().map(Property::Junk));
    if order.is_empty() {
        return;
    }
    properties.sort_by_key(|p| {
        p.key_name()
            .and_then(|key| order.iter().position(|o| *o == key))
            .unwrap_or(usize::MAX)
    });
}
/// Whether a Glyphs 4 axis listed its (localized) `names`.
const KEY_AXIS_NAMES: &str = "com.schriftgestalt.Glyphs.axisNames";
pub(crate) const KEY_STEM_VALUES: &str = "com.schriftgestalt.Glyphs.stemValues";
pub(crate) const KEY_STEMS: &str = "com.schriftgestalt.Glyphs.stems";
pub(crate) const KEY_USER_DATA: &str = "com.schriftgestalt.Glyphs.userData";
pub(crate) const KEY_SHAPE_TYPE: &str = "com.schriftgestalt.Glyphs.shapeType";
pub(crate) const KEY_WEIGHT_CLASS: &str = "com.schriftgestalt.Glyphs.weightClass";
pub(crate) const KEY_WIDTH_CLASS: &str = "com.schriftgestalt.Glyphs.widthClass";
pub(crate) const KEY_STYLISTIC_SET_LABEL: &str = "com.schriftgestalt.Glyphs.labels";

fn copy_custom_parameters(
    format_specific: &mut FormatSpecific,
    custom_parameters: &[glyphslib::common::CustomParameter],
) {
    for cp in custom_parameters.iter() {
        if let Ok(value) = serde_json::to_value(&cp.value) {
            // A parameter can be there more than once (several Filter ones):
            // a repeat gets a key of its own, `Filter#2`, in its place.
            let mut key = format!("{}{}", KEY_CUSTOM_PARAMETERS, cp.name);
            let mut repeat = 1;
            while format_specific.contains_key(&key) {
                repeat += 1;
                key = format!("{}{}#{}", KEY_CUSTOM_PARAMETERS, cp.name, repeat);
            }
            format_specific.insert(
                key,
                json!({
                            "value": value,
                            "disabled": cp.disabled,
                }),
            );
        }
    }
}

/// The name of the custom parameter stored under `name`: a repeat stored as
/// `Filter#2` is a `Filter` one.
fn custom_parameter_name(format_specific: &FormatSpecific, name: &str) -> String {
    if let Some((base, repeat)) = name.rsplit_once('#') {
        if repeat.parse::<u32>().is_ok_and(|n| n >= 2)
            && format_specific.contains_key(&format!("{}{}", KEY_CUSTOM_PARAMETERS, base))
        {
            return base.to_string();
        }
    }
    name.to_string()
}

fn get_cp<'a>(format_specific: &'a FormatSpecific, name: &str) -> Option<&'a serde_json::Value> {
    format_specific.get(&format!("{}{}", KEY_CUSTOM_PARAMETERS, name))
}

fn serialize_custom_parameters(
    format_specific: &FormatSpecific,
) -> Vec<glyphslib::common::CustomParameter> {
    format_specific
        .iter()
        .filter_map(|(key, value)| {
            if key.starts_with(KEY_CUSTOM_PARAMETERS) {
                let name = custom_parameter_name(
                    format_specific,
                    key.trim_start_matches(KEY_CUSTOM_PARAMETERS),
                );
                Some(glyphslib::common::CustomParameter {
                    name,
                    value: value
                        .as_object()
                        .and_then(|d| d.get("value").cloned())
                        .and_then(|v| serde_json::from_value(v).ok())
                        .unwrap_or_default(),
                    disabled: value
                        .as_object()
                        .and_then(|d| d.get("disabled"))
                        .and_then(|v| v.as_bool())
                        .unwrap_or(false),
                })
            } else {
                None
            }
        })
        .collect()
}

/// Parse a Glyphs colour from the JSON we stored it as. Handles all three
/// representations: a colour label index, a colour tuple, and a Glyphs 4
/// palette reference (`["p", index, alpha]`).
pub(crate) fn color_from_json(value: &serde_json::Value) -> Option<glyphslib::common::Color> {
    if let Some(index) = value.as_i64() {
        return Some(glyphslib::common::Color::ColorInt(index as u8));
    }
    let array = value.as_array()?;
    // A palette reference. Its alpha is stored like a tuple's components,
    // as Glyphs 3 writes it (0..255); fonts stored before that hold 0..1.
    if array.first().and_then(|v| v.as_str()) == Some("p") {
        let alpha = array.get(2).and_then(|v| v.as_f64()).unwrap_or(255.0);
        return Some(glyphslib::common::Color::ColorPaletteIndex {
            index: array.get(1).and_then(|v| v.as_u64()).unwrap_or(0) as u16,
            alpha: if alpha > 1.0 { alpha / 255.0 } else { alpha },
        });
    }
    // A colour tuple. It was stored scaled to 0..255, so unscale it.
    Some(glyphslib::common::Color::ColorTuple(
        array
            .iter()
            .filter_map(|v| v.as_f64())
            .map(|v| v / 255.0)
            .collect(),
    ))
}

pub(crate) fn copy_user_data(
    format_specific: &mut FormatSpecific,
    user_data: &BTreeMap<SmolStr, glyphslib::Plist>,
) {
    if !user_data.is_empty() {
        if let Ok(value) = serde_json::to_value(user_data) {
            format_specific.insert(KEY_USER_DATA.into(), value);
        }
    }
}

/// Load a Glyphs font from a file path
pub fn load(path: PathBuf) -> Result<Font, BabelfontError> {
    if path.extension().and_then(|x| x.to_str()) == Some("glyphspackage") {
        return _load(
            &glyphslib::Font::load(&path).map_err(|x| BabelfontError::PlistParse(x.to_string()))?,
            path,
        );
    }
    let s = fs::read_to_string(&path)?;
    load_str(&s, path.clone())
}

/// Load a Glyphs font from a string
pub fn load_str(s: &str, path: PathBuf) -> Result<Font, BabelfontError> {
    let glyphs_font =
        glyphslib::Font::load_str(s).map_err(|x| BabelfontError::PlistParse(x.to_string()))?;
    _load(&glyphs_font, path)
}

/// Load a Glyphs package font from in-memory package entries
pub fn load_package_entries(
    path: PathBuf,
    entries: &HashMap<String, String>,
) -> Result<Font, BabelfontError> {
    let glyphs_font = glyphslib::Font::load_package_entries(entries)
        .map_err(|x| BabelfontError::PlistParse(x.to_string()))?;
    _load(&glyphs_font, path)
}

/// What [`save_package_entries`] writes.
#[derive(Debug, Default, Clone, Copy)]
pub struct PackageSaveRequest<'a> {
    /// Write only the files of these glyphs, plus the font-level files when
    /// `font_level` is set. A named glyph the font no longer has is listed
    /// in [`PackageEntries::removed`]. `None` writes the whole package.
    pub glyphs: Option<&'a [String]>,
    /// In a partial save, also write `fontinfo.plist`, `order.plist` and
    /// `UIState.plist`, and in a Glyphs 4 package `kerning.plist`, `note.md`
    /// and `features/*.fea`.
    pub font_level: bool,
}

/// `.glyphspackage` files as text, by path relative to the package.
#[derive(Debug, Default, Clone, serde::Serialize)]
pub struct PackageEntries {
    /// Files to write.
    pub files: BTreeMap<String, String>,
    /// In a partial save, files that should no longer exist. A full save
    /// lists none: anything it does not write is stale.
    pub removed: Vec<String>,
}

/// Save a font as `.glyphspackage` file entries, the whole package or the
/// files of some glyphs. A glyph file comes out the same either way.
pub fn save_package_entries(
    font: &Font,
    request: PackageSaveRequest,
) -> Result<PackageEntries, BabelfontError> {
    let package = font.as_glyphslib()?;
    let to_error = |e: Box<dyn std::error::Error>| BabelfontError::General(e.to_string());
    let mut entries = PackageEntries::default();
    let Some(names) = request.glyphs else {
        entries.files = package.to_package_entries().map_err(to_error)?;
        return Ok(entries);
    };
    for name in names {
        match package.package_glyph_entry(name).map_err(to_error)? {
            Some((path, text)) => {
                entries.files.insert(path, text);
            }
            None => entries.removed.push(glyphslib::package_glyph_path(name)),
        }
    }
    if request.font_level {
        for (path, text) in package.to_package_entries().map_err(to_error)? {
            if !path.starts_with("glyphs/") {
                entries.files.insert(path, text);
            }
        }
        // Files a package only has when there is something to put in them.
        for file in ["UIState.plist", "kerning.plist", "note.md"] {
            if !entries.files.contains_key(file) {
                entries.removed.push(file.to_string());
            }
        }
    }
    Ok(entries)
}

/// Loads glyphs from their `.glyphspackage` files alone, with the font
/// context of the package's `fontinfo.plist`, to pick up a change to a few
/// glyph files without reading the whole package.
///
/// `glyph_files` holds `glyphs/<file>.glyph` contents by path. The result is
/// a font with just those glyphs, and kerning groups only for them.
pub fn load_package_glyphs(
    fontinfo: &str,
    glyph_files: &HashMap<String, String>,
) -> Result<Font, BabelfontError> {
    let mut names = vec![];
    for (path, text) in glyph_files {
        let plist = glyphslib::Plist::parse(text)
            .map_err(|e| BabelfontError::PlistParse(format!("{path}: {e}")))?;
        let name = plist
            .get("glyphname")
            .and_then(|n| n.as_str())
            .ok_or_else(|| BabelfontError::PlistParse(format!("{path} has no glyphname")))?;
        names.push(glyphslib::Plist::String(name.to_string()));
    }
    let mut entries: HashMap<String, String> = glyph_files.clone();
    entries.insert("fontinfo.plist".to_string(), fontinfo.to_string());
    entries.insert(
        "order.plist".to_string(),
        glyphslib::Plist::Array(names).to_string(),
    );
    load_package_entries(PathBuf::from("glyphs.glyphspackage"), &entries)
}

fn _load(glyphs_font: &glyphslib::Font, path: PathBuf) -> Result<Font, BabelfontError> {
    let mut font = Font::new();
    let glyphs2 = glyphs_font.as_glyphs2();
    // Remember which Glyphs file format version we came from, so that
    // `as_glyphs3` can write the same version back out again. A Glyphs 2 source
    // is upgraded to Glyphs 3 for our in-memory model, so we record "2" but will
    // never write anything lower than 3.
    let source_format_version = if glyphs_font.as_glyphs2().is_some() {
        2
    } else {
        glyphs_font
            .as_glyphs3()
            .map(|g| g.format_version)
            .unwrap_or(3)
    };
    font.format_specific.insert(
        KEY_FORMAT_VERSION.into(),
        serde_json::json!(source_format_version),
    );
    let mut upgraded = glyphs_font.clone();
    if glyphs_font.as_glyphs2().is_some() {
        upgraded.upgrade_in_place();
    }
    // Smart Glyph properties are read the Glyphs 3 way.
    if let glyphslib::Font::Glyphs3(glyphs3) = &mut upgraded {
        glyphs3.convert_smart_glyphs(3);
    }
    let glyphs_font = upgraded
        .as_glyphs3()
        .ok_or(BabelfontError::WrongConvertor { path })?;
    // App version
    font.format_specific.insert(
        KEY_APP_VERSION.into(),
        serde_json::Value::String(glyphs_font.app_version.clone()),
    );
    // Display strings
    font.format_specific.insert(
        KEY_DISPLAY_STRINGS.into(),
        serde_json::to_value(&glyphs_font.display_strings).unwrap_or(serde_json::Value::Null),
    );
    // Copy axes
    font.axes = glyphs_font
        .axes
        .iter()
        .map(|axis| {
            let mut out = Axis {
                tag: Tag::from_str(&axis.tag).unwrap_or_else(|_| Tag::new(b"????")),
                name: axis.name.clone().into(),
                hidden: axis.hidden,
                ..Default::default()
            };
            // Glyphs 4 can name an axis in several languages.
            if !axis.names.is_empty() {
                for name in &axis.names {
                    out.name.insert(name.language.clone(), name.value.clone());
                }
                out.format_specific
                    .insert(KEY_AXIS_NAMES.into(), serde_json::Value::Bool(true));
            }
            out
        })
        .collect();
    let axes_order = font.axes.iter().map(|a| a.tag).collect::<Vec<Tag>>();
    // Classes
    font.features.classes = glyphs_font
        .classes
        .iter()
        .map(|c| (SmolStr::new(&c.name), c.into()))
        .collect();
    stash_repeats(
        &mut font.format_specific,
        KEY_REPEATED_CLASSES,
        &glyphs_font.classes,
        |c| &c.name,
    );
    stash_repeats(
        &mut font.format_specific,
        KEY_REPEATED_PREFIXES,
        &glyphs_font.feature_prefixes,
        |p| &p.name,
    );
    // Custom parameters
    copy_custom_parameters(&mut font.format_specific, &glyphs_font.custom_parameters);
    // Date
    font.date = glyphs_font
        .date
        .parse()
        .unwrap_or_else(|_| chrono::Utc::now());
    // Family name
    font.names.family_name = glyphs_font.family_name.clone().into();
    // Feature prefixes
    for prefix in glyphs_font.feature_prefixes.iter() {
        font.features
            .prefixes
            .insert(SmolStr::new(&prefix.name), prefix.into());
    }
    // Features
    for feature in glyphs_font.features.iter() {
        font.features
            .features
            .push((SmolStr::new(&feature.tag), feature.into()));
    }
    // Names of metrics that have a type as well
    let metric_names: BTreeMap<String, String> = glyphs_font
        .metrics
        .iter()
        .filter(|m| !m.name.is_empty())
        .filter_map(|m| Some((metric_key(m)?, m.name.clone())))
        .collect();
    if !metric_names.is_empty() {
        font.format_specific
            .insert_json(KEY_METRIC_NAMES, &metric_names);
    }
    // Masters
    font.masters = glyphs_font
        .masters
        .iter()
        .map(|master| load_master(master, glyphs_font, &font))
        .collect();
    // Glyphs
    font.glyphs = GlyphList(
        glyphs_font
            .glyphs
            .iter()
            .map(|g| glyph::glyphs::from_glyphs(g, &axes_order))
            .collect::<Result<Vec<_>, BabelfontError>>()?,
    );
    if let Some(glyphs2) = glyphs2 {
        restore_glyphs2_component_transforms(&mut font, glyphs2);
    }
    // Instances
    font.instances = glyphs_font
        .instances
        .iter()
        .map(|i| load_instance(&font, i))
        .collect();
    // Handle kerning when we do masters
    // Metrics
    // Note
    font.note = Some(glyphs_font.note.clone());
    // Numbers
    font.format_specific.insert(
        KEY_NUMBER_NAMES.into(),
        glyphs_font.numbers.iter().map(|n| n.name.clone()).collect(),
    );
    // Properties
    load_properties(
        &mut font.names,
        &mut font.custom_ot_values,
        &glyphs_font.properties,
    );
    stash_property_order(&mut font.format_specific, &glyphs_font.properties);
    // Settings, all of them: some are only written to Glyphs 4 files.
    font.format_specific.insert(KEY_SETTINGS.into(), {
        let _all_settings = glyphslib::common::with_format_version(4);
        serde_json::to_value(&glyphs_font.settings).unwrap_or(serde_json::Value::Null)
    });

    // Stems
    if !glyphs_font.stems.is_empty() {
        font.format_specific.insert(
            KEY_STEMS.into(),
            serde_json::to_value(&glyphs_font.stems).unwrap_or(serde_json::Value::Null),
        );
    }
    // UPM
    font.upm = glyphs_font.units_per_em as u16;

    // User data
    copy_user_data(&mut font.format_specific, &glyphs_font.user_data);
    // Version
    font.version = (
        glyphs_font.version.major as u16,
        glyphs_font.version.minor as u16,
    );

    // Vertical kerning stays format-specific. RTL kerning goes onto the
    // masters below, once they exist.
    font.format_specific
        .insert_json_non_null(KEY_KERNING_VERTICAL, &glyphs_font.kerning_vertical);
    font.format_specific
        .insert_json_non_null(KEY_KERNING_RTL, &glyphs_font.kerning_rtl);
    font.move_rtl_kerning_to_masters();

    // Copy masters
    // Copy instances
    // Copy kern groups
    for glyph in font.glyphs.iter() {
        // The *left side* of the glyph is relevant when the glyph is
        // on the *right side* (second item) of a kerning pair.
        let left_group = glyph.format_specific.get_string("kern_left");
        if !left_group.is_empty() {
            font.second_kern_groups
                .entry(left_group.into())
                .or_default()
                .push(glyph.name.clone());
        }

        // The *right side* of the glyph is relevant when the glyph is
        // on the *left side* (first item) of a kerning pair.
        let right_group = glyph.format_specific.get_string("kern_right");
        if !right_group.is_empty() {
            font.first_kern_groups
                .entry(right_group.into())
                .or_default()
                .push(glyph.name.clone());
        }
    }

    // Interpret metrics
    // Interpret axes
    interpret_axes(&mut font)?;
    // Interpret font-level custom parameters
    interpret_custom_parameters(&mut font)?;
    // Glyphs states a vertical origin as an offset from the master ascender;
    // now that the metrics are interpreted, store the absolute y instead.
    glyphs_vertical_origin_to_babelfont(&mut font);

    // Bake in Glyphs data ??? When is best to do this?
    // GlyphsData.apply(&mut font)?;

    Ok(font)
}

fn load_instance(font: &Font, instance: &glyphs3::Instance) -> crate::Instance {
    let designspace_to_location = |numbers: &[f32]| -> DesignLocation {
        numbers
            .iter()
            .zip(font.axes.iter())
            .map(|(number, axis)| (axis.tag, DesignCoord::new(*number as f64)))
            .collect()
    };
    let mut format_specific = FormatSpecific::default();
    copy_custom_parameters(&mut format_specific, &instance.custom_parameters);
    copy_user_data(&mut format_specific, &instance.user_data);
    format_specific.insert_some_json(KEY_WEIGHT_CLASS, &instance.weight_class.as_ref());
    format_specific.insert_some_json(KEY_WIDTH_CLASS, &instance.width_class.as_ref());
    format_specific.insert_if_ne_json(KEY_INSTANCE_EXPORTS, &instance.exports, &true);
    format_specific.insert_if_ne_json(KEY_IS_BOLD, &instance.is_bold, &false);
    format_specific.insert_if_ne_json(KEY_IS_ITALIC, &instance.is_italic, &false);
    if !instance.instance_interpolations.is_empty() {
        format_specific.insert_json(
            KEY_INSTANCE_INTERPOLATIONS,
            &instance.instance_interpolations,
        );
    }
    format_specific.insert_if_ne_json(
        KEY_INSTANCE_MANUAL_INTERPOLATION,
        &instance.manual_interpolation,
        &false,
    );
    format_specific.insert_if_ne_json(
        KEY_INSTANCE_EXPORT_TYPE,
        &instance.export_type,
        &glyphslib::glyphs3::ExportType::default(),
    );
    let mut names = Names::new();
    let mut custom_ot_values = CustomOTValues::default();
    load_properties(
        &mut names,
        &mut custom_ot_values, // XXX
        &instance.properties,
    );
    stash_property_order(&mut format_specific, &instance.properties);
    // Glyphs 4 keeps the instance's name as the dflt style name; it is the
    // name, not a localization of it.
    if names.typographic_subfamily.0.len() == 1
        && names.typographic_subfamily.get_default() == Some(&instance.name)
        && font
            .format_specific
            .get(KEY_FORMAT_VERSION)
            .and_then(|v| v.as_i64())
            .is_some_and(|v| v >= 4)
    {
        names.typographic_subfamily = Default::default();
    }
    // There may be a familyName custom parameter
    if names.family_name.is_empty() {
        if let Some(family_name) =
            enabled_cp_value(&format_specific, "familyName").and_then(|x| x.as_str())
        {
            names.family_name = family_name.to_string().into();
        }
    }
    crate::Instance {
        id: instance.id.as_ref().unwrap_or(&instance.name).clone(),
        name: I18NDictionary::from(&instance.name),
        location: designspace_to_location(&instance.axes_values),
        custom_names: names,
        variable: instance.export_type == glyphslib::glyphs3::ExportType::Variable,
        linked_style: instance.link_style.clone(),
        format_specific,
    }
}

/// A Glyphs 4 instance id: the instance's own, unless it is just the
/// instance's name (as for an instance from Glyphs 3, a designspace or a
/// UFO). Then a UUID, as Glyphs makes them, derived from the name and the
/// instance's position, so that it is unique and the same on every save.
fn glyphs4_instance_id(index: usize, instance: &crate::Instance) -> String {
    let id = instance.id.as_str();
    if instance.name.get_default().map(String::as_str) != Some(id) {
        return id.to_string();
    }
    use std::hash::{Hash, Hasher};
    let mut bytes = [0u8; 16];
    for (half, salt) in [(0usize, "babelfont"), (8, "instance")] {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        (salt, index, id).hash(&mut hasher);
        bytes[half..half + 8].copy_from_slice(&hasher.finish().to_le_bytes());
    }
    uuid::Builder::from_random_bytes(bytes)
        .into_uuid()
        .to_string()
        .to_uppercase()
}

fn save_instance(
    index: usize,
    instance: &crate::Instance,
    axes: &[Axis],
    weight_class: Option<i32>,
    width_class: Option<i32>,
    format_version: i32,
) -> glyphs3::Instance {
    let mut axes_values = vec![];
    if !instance.variable {
        for axis in axes {
            axes_values.push(
                instance
                    .location
                    .get(axis.tag)
                    .map(|x| x.to_f64())
                    .map(|x| x as f32)
                    .unwrap_or(0.0),
            );
        }
    }
    let format_specific = &instance.format_specific;
    glyphs3::Instance {
        id: if format_version > 3 {
            Some(glyphs4_instance_id(index, instance))
        } else {
            None
        },
        name: instance
            .name
            .get_default()
            .map(|x| x.to_string())
            .unwrap_or_default(),
        axes_values,
        // An instance's own weightClass/widthClass wins; otherwise the
        // font-level OS/2 classes the source stated (custom_ot_values) are
        // serialized here, because the instance field is where the Glyphs
        // format carries them.
        weight_class: format_specific
            .get(KEY_WEIGHT_CLASS)
            .and_then(|x| x.as_i64())
            .map(|x| x as i32)
            .or(weight_class),
        width_class: format_specific
            .get(KEY_WIDTH_CLASS)
            .and_then(|x| x.as_i64())
            .map(|x| x as i32)
            .or(width_class),
        exports: format_specific
            .get(KEY_INSTANCE_EXPORTS)
            .and_then(|x| x.as_bool())
            .unwrap_or(true),
        custom_parameters: serialize_custom_parameters(&instance.format_specific),
        user_data: format_specific
            .get(KEY_USER_DATA)
            .and_then(|x| serde_json::from_value::<UserData>(x.clone()).ok())
            .unwrap_or_default(),
        link_style: instance.linked_style.clone(),
        instance_interpolations: format_specific
            .get_parse_opt::<BTreeMap<String, glyphslib::common::InstanceFactors>>(
                KEY_INSTANCE_INTERPOLATIONS,
            )
            .unwrap_or_default(),
        is_bold: instance.format_specific.get_bool_or(KEY_IS_BOLD, false),
        is_italic: instance.format_specific.get_bool_or(KEY_IS_ITALIC, false),
        manual_interpolation: format_specific.get_bool(KEY_INSTANCE_MANUAL_INTERPOLATION),
        properties: {
            let mut properties =
                save_properties(&instance.custom_names, &CustomOTValues::default(), true);
            // A family name that came from the familyName custom parameter
            // stays there alone.
            let from_parameter = enabled_cp_value(format_specific, "familyName")
                .and_then(|v| v.as_str())
                .is_some_and(|name| {
                    instance.custom_names.family_name.is_single()
                        && instance
                            .custom_names
                            .family_name
                            .get_default()
                            .map(String::as_str)
                            == Some(name)
                });
            if from_parameter {
                properties.retain(|p| {
                    !matches!(
                        p,
                        glyphs3::Property::LocalizedProperty {
                            key: glyphs3::LocalizedPropertyKey::FamilyNames,
                            ..
                        }
                    )
                });
            }
            restore_property_order(format_specific, &instance.custom_names, &mut properties);
            // Glyphs 3 has the name; style names that only repeat it go, as
            // Glyphs 4 leaves them out of a Glyphs 3 file.
            if format_version < 4 {
                let name = instance.name.get_default().cloned().unwrap_or_default();
                for property in properties.iter_mut() {
                    if let glyphs3::Property::LocalizedProperty {
                        key: glyphs3::LocalizedPropertyKey::StyleNames,
                        values,
                    } = property
                    {
                        values.retain(|v| v.value != name);
                    }
                }
                properties.retain(|p| {
                    !matches!(p, glyphs3::Property::LocalizedProperty { values, .. } if values.is_empty())
                });
            }
            properties
        },
        export_type: format_specific
            .get_parse_opt::<glyphslib::glyphs3::ExportType>(KEY_INSTANCE_EXPORT_TYPE)
            .unwrap_or(if instance.variable {
                glyphslib::glyphs3::ExportType::Variable
            } else {
                glyphslib::glyphs3::ExportType::Single
            }),
    }
}

fn load_properties(
    names: &mut Names,
    custom_ot_values: &mut CustomOTValues,
    glyphs_properties: &[Property],
) {
    for property in glyphs_properties.iter() {
        match property {
            glyphs3::Property::SingularProperty { key, value } => match key {
                glyphs3::SingularPropertyKey::Designer => {
                    names.designer = I18NDictionary::from(value)
                }
                glyphs3::SingularPropertyKey::Manufacturer => {
                    names.manufacturer = I18NDictionary::from(value)
                }
                glyphs3::SingularPropertyKey::DesignerUrl => {
                    names.designer_url = I18NDictionary::from(value)
                }
                glyphs3::SingularPropertyKey::ManufacturerUrl => {
                    names.manufacturer_url = I18NDictionary::from(value);
                }
                glyphs3::SingularPropertyKey::LicenseUrl => {
                    names.license_url = I18NDictionary::from(value)
                }
                glyphs3::SingularPropertyKey::PostscriptFullName => {
                    names.full_name = I18NDictionary::from(value)
                }
                glyphs3::SingularPropertyKey::PostscriptFontName => {
                    names.postscript_name = I18NDictionary::from(value)
                }
                glyphs3::SingularPropertyKey::WwsFamilyName => {
                    names.wws_family_name = I18NDictionary::from(value)
                }
                glyphs3::SingularPropertyKey::VersionString => {
                    names.version = I18NDictionary::from(value)
                }
                glyphs3::SingularPropertyKey::VendorID => {
                    if let Ok(x) = Tag::from_str(value) {
                        custom_ot_values.os2_vendor_id = Some(x);
                    }
                }
                glyphs3::SingularPropertyKey::UniqueID => {
                    names.unique_id = I18NDictionary::from(value)
                }
            },
            glyphs3::Property::LocalizedProperty { key, values } => {
                let mut value = I18NDictionary::new();
                for localized_value in values.iter() {
                    value.insert(
                        localized_value.language.clone(),
                        localized_value.value.clone(),
                    );
                }
                match key {
                    // Added to the name from `familyName`, which is the default.
                    glyphs3::LocalizedPropertyKey::FamilyNames => {
                        for (language, name) in value.0 {
                            names.family_name.insert(language, name);
                        }
                    }
                    glyphs3::LocalizedPropertyKey::Copyrights => names.copyright = value,
                    glyphs3::LocalizedPropertyKey::Designers => names.designer = value,
                    glyphs3::LocalizedPropertyKey::Manufacturers => names.manufacturer = value,
                    glyphs3::LocalizedPropertyKey::Licenses => names.license = value,
                    glyphs3::LocalizedPropertyKey::Trademarks => names.trademark = value,
                    glyphs3::LocalizedPropertyKey::Descriptions => names.description = value,
                    glyphs3::LocalizedPropertyKey::SampleTexts => names.sample_text = value,
                    glyphs3::LocalizedPropertyKey::CompatibleFullNames => {
                        names.compatible_full_name = value
                    }
                    glyphs3::LocalizedPropertyKey::StyleNames => {
                        names.typographic_subfamily = value;
                    }
                    glyphs3::LocalizedPropertyKey::PostscriptFullNames => names.full_name = value,
                }
            }
            // Kept as read (see `stash_property_order`).
            glyphs3::Property::Junk(_) => {}
        }
    }
}

/// The Glyphs properties of `names`; an instance's are `for_instance`.
fn save_properties(
    names: &Names,
    custom_ot_values: &CustomOTValues,
    for_instance: bool,
) -> Vec<glyphs3::Property> {
    let mut properties: Vec<glyphs3::Property> = vec![];

    // Macro for singular-only properties (no localized variant). These
    // cannot carry a localization, so a name that exists only under a
    // language tag (an SFD's LangName strings live under ENG) must fall
    // back to it rather than be dropped.
    macro_rules! push_singular {
        ($field:expr, $key:expr) => {
            if let Some(value) = $field.get_default_or_fallback() {
                properties.push(glyphs3::Property::SingularProperty {
                    key: $key,
                    value: value.clone(),
                });
            }
        };
    }

    // Macro for localized-only properties (no singular variant)
    macro_rules! push_localized {
        ($field:expr, $key:expr) => {
            if !$field.is_empty() {
                let values: Vec<glyphslib::glyphs3::LocalizedValue> = $field
                    .0
                    .iter()
                    .map(|(language, value)| glyphslib::glyphs3::LocalizedValue {
                        language: language.clone(),
                        value: value.clone(),
                    })
                    .collect();
                properties.push(glyphs3::Property::LocalizedProperty { key: $key, values });
            }
        };
    }

    push_localized!(
        names.compatible_full_name,
        glyphs3::LocalizedPropertyKey::CompatibleFullNames
    );

    push_localized!(names.copyright, glyphs3::LocalizedPropertyKey::Copyrights);

    // Glyphs 3 always emits the localized form.
    push_localized!(names.designer, glyphs3::LocalizedPropertyKey::Designers);

    push_singular!(
        names.designer_url,
        glyphs3::SingularPropertyKey::DesignerUrl
    );

    // Localized-only properties
    // The font's family name has a key of its own (familyName), so only its
    // other languages go here; an instance's lives only here.
    if for_instance {
        push_localized!(
            names.family_name,
            glyphs3::LocalizedPropertyKey::FamilyNames
        );
    } else {
        let mut others = names.family_name.clone();
        others.0.shift_remove("dflt");
        push_localized!(others, glyphs3::LocalizedPropertyKey::FamilyNames);
    }

    push_localized!(names.license, glyphs3::LocalizedPropertyKey::Licenses);
    push_singular!(names.license_url, glyphs3::SingularPropertyKey::LicenseUrl);

    // Glyphs 3 always emits the localized form.
    push_localized!(
        names.manufacturer,
        glyphs3::LocalizedPropertyKey::Manufacturers
    );

    push_singular!(
        names.manufacturer_url,
        glyphs3::SingularPropertyKey::ManufacturerUrl
    );
    // Glyphs stores the full name as postscriptFullName and the PostScript
    // name (name ID 6) as postscriptFontName. It has no property for the
    // PostScript CID findfont name (name ID 20).
    push_singular!(
        names.full_name,
        glyphs3::SingularPropertyKey::PostscriptFullName
    );
    push_singular!(
        names.postscript_name,
        glyphs3::SingularPropertyKey::PostscriptFontName
    );
    push_singular!(
        names.wws_family_name,
        glyphs3::SingularPropertyKey::WwsFamilyName
    );

    push_localized!(names.trademark, glyphs3::LocalizedPropertyKey::Trademarks);
    push_localized!(
        names.description,
        glyphs3::LocalizedPropertyKey::Descriptions
    );
    push_localized!(
        names.sample_text,
        glyphs3::LocalizedPropertyKey::SampleTexts
    );
    push_localized!(
        names.typographic_subfamily,
        glyphs3::LocalizedPropertyKey::StyleNames
    );
    push_singular!(names.unique_id, glyphs3::SingularPropertyKey::UniqueID);

    // Output vendor ID from custom OT values if present
    if let Some(vendor_id) = custom_ot_values.os2_vendor_id {
        properties.push(glyphs3::Property::SingularProperty {
            key: glyphs3::SingularPropertyKey::VendorID,
            // Glyphs writes it without the padding to four characters.
            value: vendor_id.to_string().trim_end().to_string(),
        });
    }
    push_singular!(names.version, glyphs3::SingularPropertyKey::VersionString);

    properties
}

fn load_master(master: &glyphs3::Master, glyphs_font: &glyphs3::Glyphs3, font: &Font) -> Master {
    let designspace_to_location = |numbers: &[f32]| -> DesignLocation {
        numbers
            .iter()
            .zip(font.axes.iter())
            .map(|(number, axis)| (axis.tag, DesignCoord::new(*number as f64)))
            .collect()
    };
    let mut m = Master::new(
        master.name.clone(),
        master.id.clone(),
        designspace_to_location(&master.axes_values),
    );
    m.active = master.active;
    m.guides = master.guides.iter().map(Into::into).collect();
    for (i, metric_value) in master.metric_values.iter().enumerate() {
        let metric_name = if i < glyphs_font.metrics.len() {
            let glyphs_metric = &glyphs_font.metrics[i];
            if let Some(known_type) = glyphs_font.metrics[i].metric_type {
                let typ = crate::MetricType::from(&known_type);
                if let Some(filter) = &glyphs_metric.filter {
                    crate::MetricType::Custom(format!("{} (filter {})", typ.as_str(), filter))
                } else {
                    typ
                }
            } else {
                crate::MetricType::Custom(glyphs_font.metrics[i].name.clone())
            }
        } else {
            crate::MetricType::Custom(format!("Metric {}", i))
        };
        m.metrics
            .insert(metric_name.clone(), metric_value.pos as i32);
        let overshoot_metric_name =
            crate::MetricType::Custom(format!("{} overshoot", metric_name.as_str()));
        m.metrics
            .insert(overshoot_metric_name, metric_value.over as i32);
    }
    copy_custom_parameters(&mut m.format_specific, &master.custom_parameters);
    m.kerning = glyphs_font
        .kerning
        .get(&m.id)
        .map(|kerndict| {
            let mut kerns = IndexMap::new();
            for (first, items) in kerndict {
                // Replace "@MMK_L_"/"@MMK_R_" prefix in group names with "@"
                let first = if let Some(stripped) = first.strip_prefix("@MMK_L_") {
                    format!("@{}", stripped)
                } else {
                    first.clone()
                };
                for (second, kern) in items {
                    let second = if let Some(stripped) = second.strip_prefix("@MMK_R_") {
                        format!("@{}", stripped)
                    } else {
                        second.clone()
                    };

                    kerns.insert(
                        (SmolStr::from(&first), SmolStr::from(&second)),
                        *kern as i16,
                    );
                }
            }
            kerns
        })
        .unwrap_or_default();
    m.format_specific.insert(
        KEY_STEM_VALUES.into(),
        serde_json::to_value(&master.stem_values).unwrap_or(serde_json::Value::Null),
    );
    m.format_specific.insert(
        KEY_NUMBER_VALUES.into(),
        serde_json::to_value(&master.number_values).unwrap_or(serde_json::Value::Null),
    );
    m.format_specific.insert(
        KEY_ICON_NAME.into(),
        serde_json::Value::String(master.icon_name.clone()),
    );
    m.format_specific.insert(
        KEY_USER_DATA.into(),
        serde_json::to_value(&master.user_data).unwrap_or(serde_json::Value::Null),
    );
    m.format_specific.insert(
        KEY_MASTER_VISIBLE.into(),
        serde_json::Value::Bool(master.visible),
    );

    m
}

fn find_base_style(masters: &[Master]) -> String {
    if masters.is_empty() {
        return String::new();
    }
    let first_name = masters[0]
        .name
        .get_default()
        .map(|s| s.as_str())
        .unwrap_or("");
    let mut base_style: Vec<&str> = first_name.split_whitespace().collect();
    for master in masters.iter().skip(1) {
        let name = master.name.get_default().map(|s| s.as_str()).unwrap_or("");
        let style: Vec<&str> = name.split_whitespace().collect();
        base_style.retain(|s| style.contains(s));
    }
    base_style.join(" ")
}

fn get_origin_master(font: &Font) -> Option<&Master> {
    if font.masters.is_empty() {
        return None;
    }
    // The current Glyphs spec uses "Variable Font Origin" (matched by master ID).
    if let Some(id) =
        enabled_cp_value(&font.format_specific, "Variable Font Origin").and_then(|x| x.as_str())
    {
        if let Some(master) = font.masters.iter().find(|m| m.id == id) {
            return Some(master);
        }
    }
    // Older name: "Variation Font Origin" (matched by master name).
    if let Some(name) =
        enabled_cp_value(&font.format_specific, "Variation Font Origin").and_then(|x| x.as_str())
    {
        if let Some(master) = font
            .masters
            .iter()
            .find(|m| m.name.get_default().map(|n| n.as_str()) == Some(name))
        {
            return Some(master);
        }
    }
    // Fall back to the base style shared by all masters (default "Regular").
    let base_style = find_base_style(&font.masters);
    let base_style = if base_style.is_empty() {
        "Regular".to_string()
    } else {
        base_style
    };
    if let Some(master) = font
        .masters
        .iter()
        .find(|m| m.name.get_default().map(|n| n.as_str()) == Some(base_style.as_str()))
    {
        return Some(master);
    }
    // Second attempt: master whose name with "Regular" stripped matches base_style.
    if let Some(master) = font.masters.iter().find(|m| {
        m.name.get_default().is_some_and(|name| {
            let without_regular = name
                .split_whitespace()
                .filter(|&w| w != "Regular")
                .collect::<Vec<_>>()
                .join(" ");
            without_regular == base_style
        })
    }) {
        return Some(master);
    }
    font.masters.first()
}

/// The OS/2 width class is a 1–9 enumeration, not a user-space value; the
/// user-space width Glyphs exposes is a percentage of the normal width. See
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#uswidthclass>.
/// This mirrors glyphsLib's `WIDTH_CLASS_TO_VALUE`.
const WIDTH_CLASS_TO_VALUE: [f64; 9] = [
    50.0,  // Ultra-condensed
    62.5,  // Extra-condensed
    75.0,  // Condensed
    87.5,  // Semi-condensed
    100.0, // Medium (normal)
    112.5, // Semi-expanded
    125.0, // Expanded
    150.0, // Extra-expanded
    200.0, // Ultra-expanded
];

/// Translate an OS/2 width class (1–9) into the user-space percentage Glyphs
/// uses for the width axis.
fn width_class_to_user_loc(class: i64) -> Option<f64> {
    let index = usize::try_from(class).ok()?;
    if index == 0 || index > WIDTH_CLASS_TO_VALUE.len() {
        return None;
    }
    WIDTH_CLASS_TO_VALUE.get(index - 1).copied()
}

/// Which family an axis belongs to. This decides the default user location and
/// whether the OS/2 weight/width classes are used to derive one. Mirrors
/// glyphsLib's `AxisDefinition`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum AxisKind {
    Weight,
    Width,
    Other,
}

impl AxisKind {
    fn of(tag: Tag) -> Self {
        if tag == Tag::new(b"wght") {
            AxisKind::Weight
        } else if tag == Tag::new(b"wdth") {
            AxisKind::Width
        } else {
            AxisKind::Other
        }
    }

    /// The user location used when nothing else supplies one.
    fn default_user_loc(self) -> f64 {
        match self {
            AxisKind::Weight => 400.0,
            AxisKind::Width => 100.0,
            AxisKind::Other => 0.0,
        }
    }
}

/// A user → design mapping being assembled for one axis.
#[derive(Default, Clone)]
struct AxisMapping {
    pairs: Vec<(f64, f64)>,
}

impl AxisMapping {
    /// Add a mapping, overwriting (with a warning) any previous mapping for the
    /// same user location — glyphsLib's behaviour, rather than erroring.
    fn insert(&mut self, user: f64, design: f64, axis_name: &str) {
        if let Some(existing) = self.pairs.iter_mut().find(|(u, _)| *u == user) {
            if existing.1 != design {
                log::warn!(
                    "Axis {axis_name}: redefining the mapping for user location {user} \
                     from {} to {design}",
                    existing.1
                );
            }
            existing.1 = design;
        } else {
            self.pairs.push((user, design));
        }
    }

    fn is_empty(&self) -> bool {
        self.pairs.is_empty()
    }

    /// Whether every entry maps a user location to the identical design
    /// location, so there is nothing to record.
    fn is_identity(&self) -> bool {
        self.pairs.iter().all(|(user, design)| user == design)
    }

    /// Mapping entries sorted by user location.
    fn sorted(&self) -> Vec<(f64, f64)> {
        let mut pairs = self.pairs.clone();
        pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
        pairs
    }
}

/// The computed range, default and mapping for one axis.
#[derive(Default)]
struct ComputedAxis {
    min: Option<f64>,
    max: Option<f64>,
    default: Option<f64>,
    map: Option<Vec<(f64, f64)>>,
}

/// The per-object inputs used to derive a user location along one axis.
struct UserLocSource<'a> {
    format_specific: &'a FormatSpecific,
    design_loc: Option<f64>,
    weight_class: Option<f64>,
    width_class: Option<f64>,
}

impl<'a> UserLocSource<'a> {
    /// A master: Glyphs masters have no weight/width class of their own (their
    /// `weight`/`width` keys are legacy name fragments, not locations).
    fn master(format_specific: &'a FormatSpecific, design_loc: Option<f64>) -> Self {
        Self {
            format_specific,
            design_loc,
            weight_class: None,
            width_class: None,
        }
    }

    /// An instance, whose `weightClass`/`widthClass` attributes are OS/2
    /// classes used to derive a user location.
    fn instance(instance: &'a crate::Instance, design_loc: Option<f64>) -> Self {
        Self {
            format_specific: &instance.format_specific,
            design_loc,
            weight_class: instance_weight_class(instance),
            width_class: instance_width_class(instance),
        }
    }
}

/// Resolve the user location of a master or instance along one axis, following
/// glyphsLib's precedence: the default/design location is overridden by the
/// `weightClass`/`widthClass` attributes, and finally by the `Axis Location`
/// custom parameter (which is already in user space).
fn get_user_loc(
    kind: AxisKind,
    default_user_loc: f64,
    tag: Tag,
    axis_name: &str,
    source: UserLocSource<'_>,
) -> Option<f64> {
    let UserLocSource {
        format_specific,
        design_loc,
        weight_class,
        width_class,
    } = source;
    // For weight the user location defaults to 400 regardless of the design
    // location; for every other axis it defaults to the design location.
    let mut user_loc = match kind {
        AxisKind::Weight => Some(default_user_loc),
        _ => design_loc,
    };

    match kind {
        AxisKind::Weight => {
            if let Some(class) = weight_class {
                // An OS/2 weight class *is* a user location.
                user_loc = Some(class);
            }
        }
        AxisKind::Width => {
            if let Some(class) = width_class.and_then(|c| width_class_to_user_loc(c.round() as i64))
            {
                user_loc = Some(class);
            }
        }
        AxisKind::Other => {}
    }

    // The Axis Location custom parameter wins over everything else.
    if let Some(location) = axis_location_value(format_specific, tag, axis_name) {
        user_loc = Some(location);
    }

    user_loc
}

/// The enabled `Axis Location` custom parameter of a master or instance, if
/// any.
fn axis_location_cp(format_specific: &FormatSpecific) -> Option<&serde_json::Value> {
    enabled_cp_value(format_specific, "Axis Location")
}

/// The user-space location for one axis from an `Axis Location` custom
/// parameter. Glyphs stores this as a list of `{Axis, Location}` dicts; some
/// versions use a dict keyed by axis name or tag.
fn axis_location_value(format_specific: &FormatSpecific, tag: Tag, axis_name: &str) -> Option<f64> {
    let value = axis_location_cp(format_specific)?;
    if let Some(list) = value.as_array() {
        for entry in list {
            if let Some(entry) = entry.as_object() {
                if entry.get("Axis").and_then(|axis| axis.as_str()) == Some(axis_name) {
                    return entry.get("Location").and_then(parse_number);
                }
            }
        }
    } else if let Some(map) = value.as_object() {
        let tag_string = tag.to_string();
        for key in [axis_name, tag_string.as_str()] {
            if let Some(location) = map.get(key).and_then(parse_number) {
                return Some(location);
            }
        }
    }
    None
}

/// A JSON number, or a string holding one (Glyphs sometimes writes
/// `Location = "200"`).
fn parse_number(value: &serde_json::Value) -> Option<f64> {
    value
        .as_f64()
        .or_else(|| value.as_str().and_then(|s| s.trim().parse::<f64>().ok()))
}

/// Instances that contribute to the axis mapping: exported, non-variable ones
/// (glyphsLib's `is_instance_active` plus its `InstanceType.VARIABLE` filter).
fn mapping_instances(font: &Font) -> impl Iterator<Item = &crate::Instance> {
    font.instances.iter().filter(|instance| {
        !instance.variable
            && instance
                .format_specific
                .get(KEY_INSTANCE_EXPORTS)
                .and_then(|exports| exports.as_bool())
                .unwrap_or(true)
    })
}

fn instance_weight_class(instance: &crate::Instance) -> Option<f64> {
    instance
        .format_specific
        .get(KEY_WEIGHT_CLASS)
        .and_then(|class| class.as_f64())
}

fn instance_width_class(instance: &crate::Instance) -> Option<f64> {
    instance
        .format_specific
        .get(KEY_WIDTH_CLASS)
        .and_then(|class| class.as_f64())
}

/// Invert a user → design mapping at `design`, interpolating between known
/// points and extrapolating beyond the ends (as fontTools' `piecewiseLinearMap`
/// does).
fn design_to_user(pairs: &[(f64, f64)], design: f64) -> Option<f64> {
    if pairs.is_empty() {
        return None;
    }
    let mut by_design: Vec<(f64, f64)> = pairs
        .iter()
        .map(|(user, design)| (*design, *user))
        .collect();
    by_design.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));

    if let Some((_, user)) = by_design.iter().find(|(d, _)| *d == design) {
        return Some(*user);
    }
    let (first_design, first_user) = *by_design.first()?;
    if design < first_design {
        return Some(design + first_user - first_design);
    }
    let (last_design, last_user) = *by_design.last()?;
    if design > last_design {
        return Some(design + last_user - last_design);
    }
    for window in by_design.windows(2) {
        if let [(d0, u0), (d1, u1)] = window {
            if design >= *d0 && design <= *d1 {
                if d1 == d0 {
                    return Some(*u0);
                }
                let t = (design - d0) / (d1 - d0);
                return Some(u0 + t * (u1 - u0));
            }
        }
    }
    None
}

/// The user location in `pairs` closest to `value`. `Axis`'s converter requires
/// the default to be one of the mapping's user coordinates, so an interpolated
/// default is snapped to the nearest entry.
fn nearest_user_loc(pairs: &[(f64, f64)], value: f64) -> Option<f64> {
    pairs.iter().map(|(user, _)| *user).min_by(|a, b| {
        (a - value)
            .abs()
            .partial_cmp(&(b - value).abs())
            .unwrap_or(std::cmp::Ordering::Equal)
    })
}

/// Turn a mapping into the axis's range/default/map, following glyphsLib: the
/// min/max are the mapping's extremes, the default is derived from the regular
/// master, and an identity mapping is elided.
fn finish_axis(mapping: AxisMapping, regular_design: Option<f64>, axis_name: &str) -> ComputedAxis {
    if mapping.is_empty() {
        return ComputedAxis::default();
    }
    let pairs = mapping.sorted();
    let (Some(min), Some(max)) = (
        pairs.first().map(|(user, _)| *user),
        pairs.last().map(|(user, _)| *user),
    ) else {
        return ComputedAxis::default();
    };

    if mapping.is_identity() {
        // user == design, so there is nothing to map; the default is just the
        // regular master's location, clamped into range.
        let default = regular_design.map(|d| d.clamp(min, max)).unwrap_or(min);
        return ComputedAxis {
            min: Some(min),
            max: Some(max),
            default: Some(default),
            map: None,
        };
    }

    let default = regular_design
        .and_then(|design| design_to_user(&pairs, design))
        .map(|user| user.clamp(min, max))
        .and_then(|user| nearest_user_loc(&pairs, user))
        .or(Some(min));
    if default.is_none() {
        log::warn!("Axis {axis_name}: could not determine a default location");
    }
    ComputedAxis {
        min: Some(min),
        max: Some(max),
        default,
        map: Some(pairs),
    }
}

/// An identity mapping built from the masters' design locations — glyphsLib's
/// `master_mapping`, used when the instances don't provide a usable mapping.
fn mapping_from_masters(master_locs: &[f64], axis_name: &str) -> AxisMapping {
    let mut mapping = AxisMapping::default();
    for design in master_locs {
        mapping.insert(*design, *design, axis_name);
    }
    mapping
}

/// Parse the `Axis Mappings` custom parameter for one axis. The parameter is a
/// dict of tag → mapping, where each mapping is either a list of `[user,
/// design]` pairs or a dict of user → design.
fn explicit_axis_mapping(mappings: &serde_json::Value, tag: Tag) -> Option<Vec<(f64, f64)>> {
    let map = mappings.as_object()?.get(&tag.to_string())?;
    let mut pairs = Vec::new();
    if let Some(array) = map.as_array() {
        for item in array {
            if let Some(pair) = item.as_array() {
                if let (Some(user), Some(design)) = (
                    pair.first().and_then(parse_number),
                    pair.get(1).and_then(parse_number),
                ) {
                    pairs.push((user, design));
                }
            }
        }
    } else if let Some(object) = map.as_object() {
        for (user, design) in object {
            if let (Ok(user), Some(design)) = (user.parse::<f64>(), parse_number(design)) {
                pairs.push((user, design));
            }
        }
    }
    Some(pairs)
}

/// Compute the range, default and mapping for one axis, following glyphsLib's
/// three-way strategy:
///
/// * an explicit `Axis Mappings` custom parameter is authoritative;
/// * otherwise, if every master carries an `Axis Location` parameter, the
///   mapping comes from the masters (instances contributing only their own
///   `Axis Location`);
/// * otherwise the mapping is derived from the instances, falling back to the
///   masters' design locations when that yields nothing useful.
fn compute_axis(
    font: &Font,
    axis: &Axis,
    origin_id: Option<&str>,
    explicit_mappings: Option<&serde_json::Value>,
    uses_axis_locations: bool,
) -> ComputedAxis {
    let tag = axis.tag;
    let kind = AxisKind::of(tag);
    let axis_name = axis.name();
    let default_user_loc = kind.default_user_loc();

    let master_locs: Vec<f64> = font
        .masters
        .iter()
        .filter_map(|master| master.location.get(tag).map(|coord| coord.to_f64()))
        .collect();
    let regular_design = origin_id
        .and_then(|id| font.masters.iter().find(|master| master.id == id))
        .and_then(|master| master.location.get(tag))
        .map(|coord| coord.to_f64())
        .or_else(|| master_locs.first().copied());

    // Strategy A: an explicit "Axis Mappings" custom parameter is authoritative.
    if let Some(mappings) = explicit_mappings {
        if let Some(pairs) = explicit_axis_mapping(mappings, tag) {
            if !pairs.is_empty() {
                let mut mapping = AxisMapping::default();
                for (user, design) in pairs {
                    mapping.insert(user, design, &axis_name);
                }
                return finish_axis(mapping, regular_design, &axis_name);
            }
        }
        log::warn!(
            "Font has an Axis Mappings custom parameter but no mapping for axis {tag}; \
             leaving it unmapped"
        );
        return finish_axis(
            mapping_from_masters(&master_locs, &axis_name),
            regular_design,
            &axis_name,
        );
    }

    // Strategy B: every master states its user location with an "Axis Location"
    // parameter. Instances then only contribute their own "Axis Location".
    if uses_axis_locations {
        let mut mapping = AxisMapping::default();
        for master in font.masters.iter() {
            let Some(design) = master.location.get(tag).map(|coord| coord.to_f64()) else {
                continue;
            };
            if let Some(user) = get_user_loc(
                kind,
                default_user_loc,
                tag,
                &axis_name,
                UserLocSource::master(&master.format_specific, Some(design)),
            ) {
                mapping.insert(user, design, &axis_name);
            }
        }
        for instance in mapping_instances(font) {
            let Some(design) = instance.location.get(tag).map(|coord| coord.to_f64()) else {
                continue;
            };
            if let Some(user) = axis_location_value(&instance.format_specific, tag, &axis_name) {
                mapping.insert(user, design, &axis_name);
            }
        }
        return finish_axis(mapping, regular_design, &axis_name);
    }

    // Strategy C: derive the mapping from the instances; if that yields nothing
    // useful, fall back to the masters' design locations.
    let mut instance_mapping = AxisMapping::default();
    for instance in mapping_instances(font) {
        let Some(design) = instance.location.get(tag).map(|coord| coord.to_f64()) else {
            continue;
        };
        if let Some(user) = get_user_loc(
            kind,
            default_user_loc,
            tag,
            &axis_name,
            UserLocSource::instance(instance, Some(design)),
        ) {
            instance_mapping.insert(user, design, &axis_name);
        }
    }
    let mapping = if !instance_mapping.is_empty() && !instance_mapping.is_identity() {
        instance_mapping
    } else {
        mapping_from_masters(&master_locs, &axis_name)
    };
    finish_axis(mapping, regular_design, &axis_name)
}

fn interpret_axes(font: &mut Font) -> Result<(), BabelfontError> {
    let origin_id = get_origin_master(font).map(|m| m.id.clone());
    let explicit_mappings = enabled_cp_value(&font.format_specific, "Axis Mappings");
    let uses_axis_locations = !font.axes.is_empty()
        && !font.masters.is_empty()
        && font
            .masters
            .iter()
            .all(|master| axis_location_cp(&master.format_specific).is_some());

    let computed: Vec<ComputedAxis> = font
        .axes
        .iter()
        .map(|axis| {
            compute_axis(
                font,
                axis,
                origin_id.as_deref(),
                explicit_mappings,
                uses_axis_locations,
            )
        })
        .collect();

    for (axis, computed) in font.axes.iter_mut().zip(computed) {
        axis.min = computed.min.map(UserCoord::new);
        axis.max = computed.max.map(UserCoord::new);
        axis.default = computed.default.map(UserCoord::new);
        axis.map = computed.map.map(|pairs| {
            pairs
                .into_iter()
                .map(|(user, design)| (UserCoord::new(user), DesignCoord::new(design)))
                .collect()
        });
    }

    // The mapping now lives in `axis.map`. The custom parameter stays, to be
    // written back as it was while it still says the same (see
    // `stashed_axis_mappings_hold`).

    Ok(())
}

/// The first Glyphs build that reads `Axis Mappings` as written.
const FIRST_BUILD_KEEPING_AXIS_MAPPINGS: u32 = 3168;

/// The `.appVersion` to write. Glyphs swaps the keys and values of `Axis
/// Mappings` when it opens a file whose build is 3167 or earlier, or is
/// empty or not a number. Babelfont, like fontc and glyphsLib, reads the
/// mapping as written, so a file that carries one gets a build at which
/// Glyphs reads it the same way. Comparing Glyphs 4.1.1's saves of 119 old
/// Glyphs 3 sources with and without a raised build showed no other change.
fn app_version_for_axis_mappings(
    app_version: String,
    custom_parameters: &[glyphslib::common::CustomParameter],
) -> String {
    let has_axis_mappings = custom_parameters
        .iter()
        .any(|cp| cp.name == "Axis Mappings");
    let old_build = app_version
        .trim()
        .parse::<u32>()
        .map_or(true, |build| build < FIRST_BUILD_KEEPING_AXIS_MAPPINGS);
    if has_axis_mappings && old_build {
        FIRST_BUILD_KEEPING_AXIS_MAPPINGS.to_string()
    } else {
        app_version
    }
}

/// Whether the font's `Axis Mappings` custom parameter, as read, still says
/// what the axes' maps say, or is disabled and so says nothing. Then it is
/// written back unchanged, in its place.
fn stashed_axis_mappings_hold(font: &Font) -> bool {
    let key = format!("{}{}", KEY_CUSTOM_PARAMETERS, "Axis Mappings");
    let Some(stashed) = font.format_specific.get(&key) else {
        return false;
    };
    let Some(mappings) = enabled_cp_value(&font.format_specific, "Axis Mappings") else {
        // Disabled: it does not decide the mapping.
        return stashed.is_object();
    };
    let sorted = |mut pairs: Vec<(f64, f64)>| {
        pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
        pairs
    };
    let is_identity = |pairs: &[(f64, f64)]| pairs.iter().all(|(u, d)| u == d);
    font.axes.iter().all(|axis| {
        let ours = sorted(
            axis.map
                .iter()
                .flatten()
                .map(|(user, design)| (user.to_f64(), design.to_f64()))
                .collect(),
        );
        let theirs = sorted(explicit_axis_mapping(mappings, axis.tag).unwrap_or_default());
        if is_identity(&ours) && is_identity(&theirs) {
            return true;
        }
        ours.len() == theirs.len()
            && ours
                .iter()
                .zip(&theirs)
                .all(|(a, b)| (a.0 - b.0).abs() < 1e-6 && (a.1 - b.1).abs() < 1e-6)
    })
}

/// If the axis mapping is not derivable from the masters and instances,
/// we need to store it as a custom parameter.
fn axis_mappings_custom_parameter(font: &Font) -> Option<glyphslib::common::CustomParameter> {
    let origin_id = get_origin_master(font).map(|master| master.id.as_str());
    let uses_axis_locations = !font.axes.is_empty()
        && !font.masters.is_empty()
        && font
            .masters
            .iter()
            .all(|master| axis_location_cp(&master.format_specific).is_some());

    let mut mappings: BTreeMap<SmolStr, glyphslib::Plist> = BTreeMap::new();
    for axis in &font.axes {
        let Some(map) = axis.map.as_ref() else {
            continue;
        };
        // Empty and identity mappings are elided on reload, so there is nothing
        // to record for them.
        if map.is_empty()
            || map
                .iter()
                .all(|(user, design)| user.to_f64() == design.to_f64())
        {
            continue;
        }

        // The mapping the loader would derive if we wrote nothing here.
        let derived = compute_axis(font, axis, origin_id, None, uses_axis_locations);
        if let Some(derived_pairs) = derived.map {
            let mut ours: Vec<(f64, f64)> = map
                .iter()
                .map(|(user, design)| (user.to_f64(), design.to_f64()))
                .collect();
            ours.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
            if derived_pairs.len() == ours.len()
                && derived_pairs
                    .into_iter()
                    .zip(ours)
                    .all(|((user, design), (u, d))| user == u && design == d)
            {
                continue;
            }
        }

        let mut axis_mapping: BTreeMap<SmolStr, glyphslib::Plist> = BTreeMap::new();
        for (user, design) in map {
            axis_mapping.insert(
                SmolStr::new(user.to_f64().to_string()),
                glyphslib::Plist::Float(design.to_f64()),
            );
        }
        mappings.insert(
            SmolStr::new(axis.tag.to_string()),
            glyphslib::Plist::Dictionary(axis_mapping),
        );
    }

    if mappings.is_empty() {
        return None;
    }

    Some(glyphslib::common::CustomParameter {
        name: "Axis Mappings".to_string(),
        value: glyphslib::Plist::Dictionary(mappings),
        disabled: false,
    })
}

/// Express babelfont's native vertical metrics as Glyphs `vertWidth` and
/// `vertOrigin`.
///
/// babelfont stores the vertical origin as an absolute y coordinate; Glyphs
/// stores it as an offset down from the master's (typo) ascender. Following
/// glyphsLib, a `vertWidth` equal to the ascender-minus-descender default is
/// left out, as is a `vertOrigin` of zero.
//
// This is deliberately *not* gated on the master's `vhea` metrics: a UFO whose
// master states no vertical metrics may therefore gain a `vertWidth`, and so
// when you compile the Glyphs source, you might get an unexpected `vmtx` table.
// But if you didn't want a `vmtx` table, why did you populate your font with
// vertical advance and origins for all the glyphs? What did you *think* was
// going to happen? Oh well. If that ever becomes a problem, what I'd like to
// do is to add some kind of flag like "emit vmtx". I don't really want babelfont
// convertors to start caring about what kind of source the font originally came from,
// because that goes against the whole idea of babelfont as a neutral broker format.
fn babelfont_vertical_metrics_to_glyphs(font: &mut Font) {
    let extents = vertical_metric_extents(font);
    for layer in font.glyphs.iter_mut().flat_map(|g| g.layers.iter_mut()) {
        let Some((ascender, descender)) = extents.get(layer_master_id(layer)).copied() else {
            continue;
        };
        if let Some(advance) = layer.vertical_advance {
            if (advance as f64) == ascender - descender {
                layer.vertical_advance = None;
            }
        }
        if let Some(origin) = layer.vertical_origin {
            // Absolute y -> offset down from the ascender.
            layer.vertical_origin = Some((ascender - origin as f64) as f32);
        }
    }
}

/// The inverse of [`babelfont_vertical_metrics_to_glyphs`]: convert a Glyphs
/// `vertOrigin` (an offset down from the ascender) into babelfont's absolute y.
fn glyphs_vertical_origin_to_babelfont(font: &mut Font) {
    let extents = vertical_metric_extents(font);
    for layer in font.glyphs.iter_mut().flat_map(|g| g.layers.iter_mut()) {
        let Some((ascender, _)) = extents.get(layer_master_id(layer)).copied() else {
            continue;
        };
        if let Some(offset) = layer.vertical_origin {
            layer.vertical_origin = Some((ascender - offset as f64) as f32);
        }
    }
}

/// The ascender and descender for each master that states them.
fn vertical_metric_extents(font: &Font) -> HashMap<String, (f64, f64)> {
    font.masters
        .iter()
        .filter_map(|master| {
            master
                .vertical_metric_extents()
                .map(|extents| (master.id.clone(), extents))
        })
        .collect()
}

/// The master a layer belongs to, if it has one.
fn layer_master_id(layer: &Layer) -> &str {
    match &layer.master {
        LayerType::DefaultForMaster(id) | LayerType::AssociatedWithMaster(id) => id,
        LayerType::FreeFloating => "",
    }
}

/// A Glyphs 2 component's transform is a matrix. Upgrading to Glyphs 3 keeps
/// only a rotation and scale read from it, losing a slant (and misreading the
/// rotation where there is one): decompose the matrix itself, as Glyphs 3 states
/// it. The upgrade keeps each layer's components first, in their order.
fn restore_glyphs2_component_transforms(font: &mut Font, glyphs2: &glyphslib::glyphs2::Glyphs2) {
    for source in &glyphs2.glyphs {
        let Some(glyph) = font.glyphs.get_mut(&source.name) else {
            continue;
        };
        for source_layer in &source.layers {
            let Some(layer) = glyph
                .layers
                .iter_mut()
                .find(|l| !l.is_background && l.id.as_deref() == Some(&source_layer.layer_id))
            else {
                continue;
            };
            let components = layer.shapes.iter_mut().filter_map(|shape| match shape {
                crate::Shape::Component(component) => Some(component),
                _ => None,
            });
            for (component, source_component) in components.zip(&source_layer.components) {
                if component.reference == source_component.component_glyph {
                    component.transform = (&source_component.transform).into();
                }
            }
        }
    }
}

pub(crate) fn as_glyphs3(font: &Font) -> Result<glyphs3::Glyphs3, BabelfontError> {
    // Do some cleanups.
    let mut font = font.clone();
    // The Glyphs file format version to write: the one asked for, else the
    // version we read (see `_load`); fonts we build ourselves default to
    // Glyphs 3. Glyphs 2 is never written, because a Glyphs 2 font is upgraded
    // on load.
    let version_of = |key: &str| {
        font.format_specific
            .get(key)
            .and_then(|v| {
                v.as_i64()
                    .or_else(|| v.as_str().and_then(|s| s.parse().ok()))
            })
            .map(|v| (v as i32).max(3))
    };
    let read_version = version_of(KEY_FORMAT_VERSION).unwrap_or(3);
    let format_version = version_of(KEY_SAVE_FORMAT_VERSION).unwrap_or(read_version);
    // Express the native vertical metrics in Glyphs terms.
    babelfont_vertical_metrics_to_glyphs(&mut font);
    // #[allow(clippy::unwrap_used)] // Surely this can't fail
    // DropSparseMasters.apply(&mut font).unwrap();
    // println!("Exporting {} masters", font.masters.len());

    let axes = font
        .axes
        .iter()
        .map(|ax| glyphs3::Axis {
            default: None,
            hidden: ax.hidden,
            name: ax.name(),
            tag: ax.tag.to_string(),
            // Glyphs 4 lists them where the font did, or where there is more
            // than the name itself.
            names: if format_version > 3
                && (ax.format_specific.get_bool_or(KEY_AXIS_NAMES, false) || !ax.name.is_single())
            {
                (&ax.name).into()
            } else {
                vec![]
            },
            user_data: ax
                .format_specific
                .get(KEY_USER_DATA)
                .and_then(|x| serde_json::from_value::<UserData>(x.clone()).ok())
                .unwrap_or_default(),
        })
        .collect();

    let mut our_metrics: Vec<crate::MetricType> = vec![];
    for master in font.masters.iter() {
        for key in master.metrics.keys() {
            if key.as_str().ends_with(" overshoot") {
                continue;
            }
            // Some metrics live in customParameters, not in the metrics array;
            // append_master_metric_custom_parameters emits those. A metrics entry
            // is keyed by its type, so one that only names such a metric would
            // carry its value nowhere.
            if customparameters::is_custom_parameter_metric(key) {
                continue;
            }
            if !our_metrics.contains(key) {
                our_metrics.push(key.clone());
            }
        }
    }
    let app_version = font
        .format_specific
        .get(KEY_APP_VERSION)
        .and_then(|x| x.as_str())
        .unwrap_or("")
        .to_string();
    let display_strings: Vec<String> = font
        .format_specific
        .get_parse_or(KEY_DISPLAY_STRINGS, Vec::new());
    let classes = restore_repeats(
        &font.format_specific,
        KEY_REPEATED_CLASSES,
        font.features
            .classes
            .iter()
            .map(|(name, members)| members.to_featureclass(name))
            .collect(),
        |c: &glyphslib::common::FeatureClass| &c.name,
    );
    let feature_prefixes = restore_repeats(
        &font.format_specific,
        KEY_REPEATED_PREFIXES,
        font.features
            .prefixes
            .iter()
            .map(|(name, code)| code.to_featureprefix(name))
            .collect(),
        |p: &glyphslib::common::FeaturePrefix| &p.name,
    );
    let features = font
        .features
        .features
        .iter()
        .map(|(tag, code)| code.to_feature(tag))
        .collect();

    // This serializes the ones we have in .format_specific
    let mut custom_parameters = serialize_custom_parameters(&font.format_specific);
    // Now overlay any font-level custom parameters in the font structure (custom_ot_values etc)
    export_font_level_cps(&mut custom_parameters, &mut font)?;
    // The font's Axis Mappings as read, while it still says what axis.map
    // says; otherwise one from the authoritative axis.map, if it is needed.
    if !stashed_axis_mappings_hold(&font) {
        custom_parameters.retain(|cp| cp.name != "Axis Mappings");
        if let Some(axis_mappings) = axis_mappings_custom_parameter(&font) {
            custom_parameters.push(axis_mappings);
        }
    }

    let family_name = font
        .names
        .family_name
        .get_default()
        .map(|x| x.to_string())
        .unwrap_or_default();

    let masters: Vec<glyphs3::Master> = font
        .masters
        .iter()
        .map(|x| save_master(x, &font.axes, &our_metrics, &font.format_specific))
        .collect();

    let settings: glyphs3::Settings = font
        .format_specific
        .get_parse_or(KEY_SETTINGS, glyphs3::Settings::default());
    let stems: Vec<glyphs3::Stem> = font.format_specific.get_parse_or(KEY_STEMS, Vec::new());

    let numbers: Vec<glyphslib::glyphs3::Number> = font
        .format_specific
        .get_parse_or::<Vec<String>>(KEY_NUMBER_NAMES, Vec::new())
        .into_iter()
        .map(|x| glyphslib::glyphs3::Number { name: x })
        .collect();

    let kerning = font
        .masters
        .iter()
        .filter(|m| !m.is_sparse(&font))
        .flat_map(|m| {
            let expanded_kerning: BTreeMap<String, BTreeMap<String, f32>> =
                m.kerning
                    .iter()
                    .fold(BTreeMap::new(), |mut acc, ((first, second), value)| {
                        let first = if let Some(stripped) = first.strip_prefix("@") {
                            SmolStr::from(format!("@MMK_L_{}", stripped))
                        } else {
                            first.clone()
                        };
                        let second = if let Some(stripped) = second.strip_prefix("@") {
                            SmolStr::from(format!("@MMK_R_{}", stripped))
                        } else {
                            second.clone()
                        };
                        acc.entry(first.to_string())
                            .or_default()
                            .insert(second.to_string(), *value as f32);
                        acc
                    });

            (!expanded_kerning.is_empty()).then(|| (m.id.clone(), expanded_kerning))
        })
        .collect();

    let axes_order = font.axes.iter().map(|a| a.tag).collect::<Vec<_>>();

    let mut properties = save_properties(&font.names, &font.custom_ot_values, false);
    restore_property_order(&font.format_specific, &font.names, &mut properties);
    let mut glyph_to_first_kern_group = HashMap::new();
    let mut glyph_to_second_kern_group = HashMap::new();
    for (group, glyph) in font.first_kern_groups.iter() {
        for glyph_name in glyph.iter() {
            if glyph_to_first_kern_group.contains_key(glyph_name) {
                log::warn!(
                    "Glyph {} is in multiple first kerning groups, skipping assignment",
                    glyph_name
                );
                continue;
            }
            glyph_to_first_kern_group.insert(glyph_name.clone(), group.clone());
        }
    }
    for (group, glyph) in font.second_kern_groups.iter() {
        for glyph_name in glyph.iter() {
            if glyph_to_second_kern_group.contains_key(glyph_name) {
                log::warn!(
                    "Glyph {} is in multiple second kerning groups, skipping assignment",
                    glyph_name
                );
                continue;
            }
            glyph_to_second_kern_group.insert(glyph_name.clone(), group.clone());
        }
    }
    let app_version = app_version_for_axis_mappings(app_version, &custom_parameters);
    let mut glyphs_font = glyphs3::Glyphs3 {
        app_version,
        format_version,
        display_strings,
        axes,
        classes,
        custom_parameters,
        date: font.date.format("%Y-%m-%d %H:%M:%S +0000").to_string(),

        family_name,
        feature_prefixes,
        features,
        glyphs: font
            .glyphs
            .iter()
            .map(|g| {
                glyph_to_glyphs(
                    g,
                    &axes_order,
                    glyph_to_second_kern_group.get(&g.name), // Glyph groups are backwards in Glyphs
                    glyph_to_first_kern_group.get(&g.name),
                )
            })
            .collect(),
        instances: font
            .instances
            .iter()
            .enumerate()
            .map(|(index, x)| {
                save_instance(
                    index,
                    x,
                    &font.axes,
                    font.custom_ot_values.os2_us_weight_class.map(|w| w as i32),
                    font.custom_ot_values.os2_us_width_class.map(|w| w as i32),
                    format_version,
                )
            })
            .collect(),
        kerning,
        kerning_context: font
            .format_specific
            .get_parse_or(KEY_KERNING_CONTEXT, BTreeMap::new()),
        kerning_rtl: font.glyphs_rtl_kerning(|_| false),
        kerning_vertical: font
            .format_specific
            .get_parse_or(KEY_KERNING_VERTICAL, BTreeMap::new()),
        masters,
        metrics: {
            let names: BTreeMap<String, String> = font
                .format_specific
                .get_parse_or(KEY_METRIC_NAMES, BTreeMap::new());
            our_metrics
                .iter()
                .map(|m| {
                    let mut metric: glyphs3::Metric = m.into();
                    if let Some(name) = metric_key(&metric).and_then(|key| names.get(&key)) {
                        metric.name = name.clone();
                    }
                    metric
                })
                .collect()
        },
        note: font.note.clone().unwrap_or_default(),
        numbers,
        properties,
        settings,
        stems,
        units_per_em: font.upm.into(),
        version: glyphslib::common::Version {
            major: font.version.0.into(),
            minor: font.version.1.into(),
        },
        user_data: font
            .format_specific
            .get(KEY_USER_DATA)
            .and_then(|x| serde_json::from_value::<UserData>(x.clone()).ok())
            .unwrap_or_default(),
    };
    // Path attributes are kept as read; some mean something else in the
    // other version.
    glyphs_font.convert_format_version(read_version, format_version);
    // The Smart Glyph properties, kept the Glyphs 3 way (see `_load`).
    if format_version >= 4 {
        glyphs_font.convert_smart_glyphs(4);
    }
    // Glyphs 4 states the color space of a font it upgrades.
    if read_version < 4 && format_version >= 4 && glyphs_font.settings.color_space.is_none() {
        glyphs_font.settings.color_space = Some("apple-rgb".to_string());
    }
    Ok(glyphs_font)
}

fn save_master(
    master: &Master,
    axes: &[Axis],
    metrics: &[crate::MetricType],
    font_format_specific: &FormatSpecific,
) -> glyphs3::Master {
    let mut axes_values = vec![];
    for axis in axes {
        axes_values.push(
            master
                .location
                .get(axis.tag)
                .map(|x| x.to_f64())
                .map(|x| x as f32)
                .unwrap_or(0.0),
        );
    }

    let mut metric_values: Vec<glyphs3::MetricValue> = vec![];
    for metric in metrics {
        let position = master.metrics.get(metric).copied().unwrap_or(0);
        let over = master
            .metrics
            .get(&crate::MetricType::Custom(format!(
                "{} overshoot",
                metric.as_str()
            )))
            .copied()
            .unwrap_or(0);
        metric_values.push(glyphs3::MetricValue {
            over: over as f32,
            pos: position as f32,
        });
    }

    let mut custom_parameters = serialize_custom_parameters(&master.format_specific);
    // Metrics that Glyphs keeps in customParameters, not in the metrics array.
    customparameters::append_master_metric_custom_parameters(
        &mut custom_parameters,
        master,
        font_format_specific,
    );

    glyphs3::Master {
        id: master.id.clone(),
        active: master.active,
        name: master
            .name
            .get_default()
            .map(|x| x.to_string())
            .unwrap_or_default(),
        axes_values,
        guides: master.guides.iter().map(Into::into).collect(),
        metric_values,
        custom_parameters,
        stem_values: master
            .format_specific
            .get_parse_or::<Vec<f32>>(KEY_STEM_VALUES, Vec::new()),
        number_values: master
            .format_specific
            .get_parse_or::<Vec<f32>>(KEY_NUMBER_VALUES, Vec::new()),
        icon_name: master
            .format_specific
            .get(KEY_ICON_NAME)
            .and_then(|x| x.as_str())
            .unwrap_or("")
            .to_string(),
        user_data: master
            .format_specific
            .get(KEY_USER_DATA)
            .and_then(|x| serde_json::from_value::<UserData>(x.clone()).ok())
            .unwrap_or_default(),
        visible: master
            .format_specific
            .get(KEY_MASTER_VISIBLE)
            .and_then(|x| x.as_bool())
            .unwrap_or(true),
        properties: vec![], // Wait what?
        temp_data: Default::default(),
    }
}

#[cfg(test)]
mod package_entry_tests {
    #![allow(clippy::unwrap_used, clippy::indexing_slicing)]
    use super::*;
    use crate::convertors::ufo::tests::read_tree;

    const PACKAGE: &str = "resources/NotoSans.glyphspackage";

    fn load(entries: &HashMap<String, String>) -> Font {
        load_package_entries(PathBuf::from("NotoSans.glyphspackage"), entries).unwrap()
    }

    #[test]
    fn package_round_trips_and_is_stable() {
        let original = read_tree(PACKAGE);
        let font = load(&original);
        let saved = save_package_entries(&font, PackageSaveRequest::default()).unwrap();
        assert!(saved.removed.is_empty());
        for path in original.keys().filter(|p| p.starts_with("glyphs/")) {
            assert!(saved.files.contains_key(path), "{path} missing");
        }
        let reloaded = load(&saved.files.clone().into_iter().collect());
        assert_eq!(reloaded.glyphs.len(), font.glyphs.len());
        for (reloaded, glyph) in reloaded.glyphs.iter().zip(font.glyphs.iter()) {
            assert_eq!(reloaded, glyph, "{} differs", glyph.name);
        }
        assert_eq!(reloaded.masters, font.masters);
        assert_eq!(reloaded.first_kern_groups, font.first_kern_groups);
        assert_eq!(reloaded.features.to_fea(), font.features.to_fea());
        let again = save_package_entries(&reloaded, PackageSaveRequest::default()).unwrap();
        assert_eq!(again.files, saved.files);
    }

    #[test]
    fn partial_package_save_matches_a_full_save() {
        let mut font = load(&read_tree(PACKAGE));
        font.glyphs.get_mut("A").unwrap().layers[0].width += 10.0;
        let full = save_package_entries(&font, PackageSaveRequest::default()).unwrap();
        let names = ["A".to_string(), "no-such-glyph".to_string()];
        let partial = save_package_entries(
            &font,
            PackageSaveRequest {
                glyphs: Some(&names),
                font_level: false,
            },
        )
        .unwrap();
        assert_eq!(
            partial.files.keys().collect::<Vec<_>>(),
            ["glyphs/A_.glyph"]
        );
        assert_eq!(
            partial.files["glyphs/A_.glyph"],
            full.files["glyphs/A_.glyph"]
        );
        assert_eq!(partial.removed, ["glyphs/no-such-glyph.glyph"]);

        let partial = save_package_entries(
            &font,
            PackageSaveRequest {
                glyphs: Some(&names[..1]),
                font_level: true,
            },
        )
        .unwrap();
        for file in ["fontinfo.plist", "order.plist"] {
            assert_eq!(partial.files[file], full.files[file], "{file}");
        }
    }

    #[test]
    fn glyphs4_package_round_trips() {
        let mut glyphs = glyphslib::Font::load_package_entries(&read_tree(PACKAGE)).unwrap();
        if let glyphslib::Font::Glyphs3(glyphs3) = &mut glyphs {
            glyphs3.format_version = 4;
            glyphs3.note = "A note".to_string();
        }
        let package: HashMap<String, String> =
            glyphs.to_package_entries().unwrap().into_iter().collect();
        assert!(package.keys().any(|p| p.starts_with("features/")));
        assert!(package.contains_key("kerning.plist"));

        let font = load(&package);
        assert_eq!(font.note.as_deref(), Some("A note"));
        let saved = save_package_entries(&font, PackageSaveRequest::default()).unwrap();
        let mut saved_paths: Vec<_> = saved.files.keys().collect();
        let mut package_paths: Vec<_> = package.keys().collect();
        saved_paths.sort();
        package_paths.sort();
        assert_eq!(saved_paths, package_paths);
        for path in saved
            .files
            .keys()
            .filter(|p| !p.ends_with("fontinfo.plist"))
        {
            assert_eq!(saved.files[path], package[path], "{path} differs");
        }
        let reloaded = load(&saved.files.clone().into_iter().collect());
        assert_eq!(reloaded.glyphs, font.glyphs);
        assert_eq!(reloaded.features.to_fea(), font.features.to_fea());
        assert_eq!(reloaded.masters, font.masters);
    }

    #[test]
    fn glyphs_load_from_their_package_files_alone() {
        let original = read_tree(PACKAGE);
        let font = load(&original);
        let files: HashMap<String, String> = ["glyphs/A_.glyph", "glyphs/a.glyph"]
            .iter()
            .map(|p| (p.to_string(), original[*p].clone()))
            .collect();
        let partial = load_package_glyphs(&original["fontinfo.plist"], &files).unwrap();
        assert_eq!(partial.glyphs.len(), 2);
        for glyph in partial.glyphs.iter() {
            assert_eq!(glyph, font.glyphs.get(&glyph.name).unwrap());
        }
    }
}

#[allow(clippy::unwrap_used, clippy::indexing_slicing)]
#[cfg(test)]
mod tests {
    #[test]
    fn english_only_singular_properties_are_not_dropped() {
        // An SFD's LangName strings populate the names under the ENG tag,
        // not the default; the singular Glyphs properties (the URLs among
        // them) must fall back to that instead of vanishing. Verified
        // against the Google Fonts corpus: 119 of 121 shipped fonts carry a
        // license URL that the converted .glyphs lost this way.
        let mut font = crate::Font::new();
        font.names
            .designer_url
            .insert("ENG".to_string(), "http://example.com/d".to_string());
        font.names
            .license_url
            .insert("ENG".to_string(), "http://scripts.sil.org/OFL".to_string());
        font.names
            .manufacturer_url
            .insert("ENG".to_string(), "http://example.com/m".to_string());
        let props = super::save_properties(&font.names, &font.custom_ot_values, false);
        let singulars: Vec<_> = props
            .iter()
            .filter_map(|p| match p {
                glyphslib::glyphs3::Property::SingularProperty { key, value } => {
                    Some((format!("{:?}", key), value.clone()))
                }
                _ => None,
            })
            .collect();
        for (key, value) in [
            ("DesignerUrl", "http://example.com/d"),
            ("LicenseUrl", "http://scripts.sil.org/OFL"),
            ("ManufacturerUrl", "http://example.com/m"),
        ] {
            assert!(
                singulars.iter().any(|(k, v)| k == key && v == value),
                "missing singular property {key}: got {singulars:?}"
            );
        }
    }

    fn singular_property(
        props: &[glyphslib::glyphs3::Property],
        wanted: glyphslib::glyphs3::SingularPropertyKey,
    ) -> Option<&str> {
        props.iter().find_map(|p| match p {
            glyphslib::glyphs3::Property::SingularProperty { key, value } if *key == wanted => {
                Some(value.as_str())
            }
            _ => None,
        })
    }

    #[test]
    fn postscript_names_are_written_to_their_glyphs_properties() {
        // postscriptFontName holds the PostScript name (name ID 6) and
        // postscriptFullName the full name, at font and instance level.
        // Name ID 20 has no Glyphs property and is not written.
        use glyphslib::glyphs3::SingularPropertyKey::{PostscriptFontName, PostscriptFullName};
        let mut font = crate::Font::new();
        font.names.postscript_name.set_default("Test".to_string());
        font.names.full_name.set_default("Test Font".to_string());
        font.names
            .postscript_cid_name
            .set_default("TestCID".to_string());
        let mut inst = crate::Instance::default();
        inst.name.set_default("Bold".to_string());
        inst.custom_names
            .postscript_name
            .set_default("Test-Bold".to_string());
        inst.custom_names
            .full_name
            .set_default("Test Font Bold".to_string());
        font.instances.push(inst);
        let g = super::as_glyphs3(&font).unwrap();

        assert_eq!(
            singular_property(&g.properties, PostscriptFontName),
            Some("Test")
        );
        assert_eq!(
            singular_property(&g.properties, PostscriptFullName),
            Some("Test Font")
        );
        assert!(!format!("{:?}", g.properties).contains("TestCID"));
        let inst_props = &g.instances[0].properties;
        assert_eq!(
            singular_property(inst_props, PostscriptFontName),
            Some("Test-Bold")
        );
        assert_eq!(
            singular_property(inst_props, PostscriptFullName),
            Some("Test Font Bold")
        );
    }

    #[test]
    fn postscript_names_are_read_from_their_glyphs_properties() {
        use glyphslib::glyphs3::{
            LocalizedPropertyKey, LocalizedValue, Property, SingularPropertyKey,
        };
        let mut names = crate::names::Names::new();
        super::load_properties(
            &mut names,
            &mut crate::CustomOTValues::default(),
            &[
                Property::SingularProperty {
                    key: SingularPropertyKey::PostscriptFontName,
                    value: "Test".to_string(),
                },
                Property::SingularProperty {
                    key: SingularPropertyKey::PostscriptFullName,
                    value: "Test Font".to_string(),
                },
            ],
        );
        assert_eq!(names.postscript_name.get_default().unwrap(), "Test");
        assert_eq!(names.full_name.get_default().unwrap(), "Test Font");
        assert!(names.postscript_cid_name.is_empty());

        // The localized form of postscriptFullName is also the full name.
        let mut names = crate::names::Names::new();
        super::load_properties(
            &mut names,
            &mut crate::CustomOTValues::default(),
            &[Property::LocalizedProperty {
                key: LocalizedPropertyKey::PostscriptFullNames,
                values: vec![LocalizedValue {
                    language: "ENG".to_string(),
                    value: "Test Font".to_string(),
                }],
            }],
        );
        assert_eq!(names.full_name.0.get("ENG").unwrap(), "Test Font");
        assert!(names.postscript_name.is_empty());
    }

    #[test]
    fn font_level_os2_classes_reach_the_exported_instance() {
        // The Glyphs format carries usWeightClass/usWidthClass on instances.
        // A font-level value read from another format (custom_ot_values) is
        // serialized there, unless the instance states its own.
        let mut font = crate::Font::new();
        font.custom_ot_values.os2_us_weight_class = Some(700);
        font.custom_ot_values.os2_us_width_class = Some(3);
        let mut inst = crate::Instance::default();
        inst.name.set_default("Bold".to_string());
        font.instances.push(inst);
        let g = super::as_glyphs3(&font).unwrap();
        assert_eq!(g.instances[0].weight_class, Some(700));
        assert_eq!(g.instances[0].width_class, Some(3));

        // An instance's own value wins over the font level.
        let mut font2 = crate::Font::new();
        font2.custom_ot_values.os2_us_weight_class = Some(700);
        let mut inst2 = crate::Instance::default();
        inst2.name.set_default("Own".to_string());
        inst2
            .format_specific
            .insert(super::KEY_WEIGHT_CLASS.to_string(), serde_json::json!(500));
        font2.instances.push(inst2);
        let g2 = super::as_glyphs3(&font2).unwrap();
        assert_eq!(g2.instances[0].weight_class, Some(500));
    }

    use crate::{GlyphCategory, Shape};
    use fontdrasil::coords::Location;
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use similar::TextDiff;

    use super::*;

    #[test]
    fn test_transform() {
        let f = load("resources/RadioCanadaDisplay.glyphs".into()).unwrap();
        let shape = f
            .glyphs
            .iter()
            .find(|g| g.name == "eacute")
            .unwrap()
            .layers
            .first()
            .unwrap()
            .shapes
            .get(1)
            .unwrap();
        if let Shape::Component(p) = shape {
            assert_eq!(p.reference, "acutecomb");
            assert_eq!(
                p.transform.as_affine(),
                kurbo::Affine::new([1.0, 0.0, 0.0, 1.0, 152.0, 0.0])
            );
        } else {
            panic!("Expected a component shape");
        }
    }

    #[test]
    fn test_default_master() {
        let font = load("resources/Nunito3.glyphs".into()).unwrap();
        assert_eq!(font.masters.len(), 6);
        assert_eq!(
            font.masters[0].location,
            Location::from(vec![
                (Tag::new(b"wght"), DesignCoord::new(42.0)),
                (Tag::new(b"ital"), DesignCoord::new(0.0)),
            ])
        );
        assert_eq!(font.axes.len(), 2);
        assert_eq!(font.axes[0].tag, Tag::new(b"wght"));
        assert_eq!(font.axes[0].min.unwrap().to_f64(), 200.0);
        assert_eq!(font.axes[0].max.unwrap().to_f64(), 1000.0);
        assert_eq!(font.axes[0].default.unwrap().to_f64(), 200.0);
        // Every master states an "Axis Location", so the mapping comes from
        // those (with instances contributing their own Axis Location). This
        // matches glyphsLib's designspace output for the same file.
        assert_eq!(
            axis_map(&font.axes[0]),
            vec![
                (200.0, 42.0),
                (300.0, 61.0),
                (400.0, 81.0),
                (600.0, 101.0),
                (700.0, 125.0),
                (800.0, 151.0),
                (900.0, 178.0),
                (1000.0, 208.0),
            ]
        );
        // The italic axis is unmapped (user == design) and runs 0..9.
        assert_eq!(font.axes[1].tag, Tag::new(b"ital"));
        assert_eq!(font.axes[1].min.unwrap().to_f64(), 0.0);
        assert_eq!(font.axes[1].max.unwrap().to_f64(), 9.0);
        assert_eq!(font.axes[1].default.unwrap().to_f64(), 0.0);
        assert!(font.axes[1].map.is_none());
        assert!(font.default_master().is_some());
    }

    #[rstest]
    fn test_roundtrip(
        #[files("resources/*glyphs")]
        // Weird instance configuration means we can't find the default master
        #[exclude("GlyphsFileFormatv3.glyphs")]
        // Small but insignificant difference in property serialization.
        // Glyphs sometimes stores a singular property as a localized property
        // with only one entry, and vice versa.
        #[exclude("RadioCanadaDisplay.glyphs")]
        #[exclude("RadioCanadaDisplay-Italic.glyphs")]
        #[exclude("Nunito3.glyphs")]
        #[exclude("NotoSansGrantha-SmartComponent.glyphs")]
        #[exclude("Fustat.glyphs")]
        // RTL kerning is merged into LTR on load, so roundtrip output differs structurally
        #[exclude("G3RTLKerning.glyphs")]
        path: PathBuf,
    ) {
        let there = load(path.clone()).unwrap();
        let backagain = glyphslib::Font::Glyphs3(as_glyphs3(&there).unwrap());
        let orig = glyphslib::Font::load_str(&fs::read_to_string(path).unwrap()).unwrap();

        let old_string = orig.to_string().unwrap();
        let new_string = backagain.to_string().unwrap();
        let diff = TextDiff::from_lines(&old_string, &new_string);
        let text_diff = diff.unified_diff().to_string();
        println!("Diff between original and roundtrip:\n{}", text_diff);
        if diff.ratio() < 1.0 {
            panic!("Roundtrip produced different output");
        }
    }

    /// Fustat, cut down to a few glyphs, as Glyphs 3 and Glyphs 4 saved it.
    /// Saving one in the other's file format version gives what Glyphs wrote,
    /// apart from the app version and the ids Glyphs made up for instances.
    #[rstest]
    #[case(
        "resources/FustatG4-subset.glyphs",
        3,
        "resources/Fustat-subset.glyphs"
    )]
    #[case(
        "resources/Fustat-subset.glyphs",
        4,
        "resources/FustatG4-subset.glyphs"
    )]
    fn test_save_in_other_format_version(
        #[case] source: &str,
        #[case] version: i64,
        #[case] expected: &str,
    ) {
        let mut font = load(source.into()).unwrap();
        font.format_specific
            .insert(KEY_SAVE_FORMAT_VERSION.into(), serde_json::json!(version));
        let saved = glyphslib::Font::Glyphs3(as_glyphs3(&font).unwrap());
        assert_eq!(saved.as_glyphs3().unwrap().format_version, version as i32);
        let normalize = |text: String| -> Vec<String> {
            text.lines()
                .filter(|l| !l.starts_with(".appVersion"))
                // Glyphs 3 wrote some of these as strings.
                .map(|l| l.replace("strokePos = \"1\";", "strokePos = 1;"))
                // Instance ids Glyphs 4 made up; ours are derived.
                .filter(|l| !(l.starts_with("id = \"") && l.len() == 44))
                .collect()
        };
        let expected = glyphslib::Font::load_str(&fs::read_to_string(expected).unwrap()).unwrap();
        let ours = normalize(saved.to_string().unwrap());
        let theirs = normalize(expected.to_string().unwrap());
        let (theirs, ours) = (theirs.join("\n"), ours.join("\n"));
        let diff = TextDiff::from_lines(&theirs, &ours);
        assert!(
            diff.ratio() == 1.0,
            "differs from what Glyphs wrote:\n{}",
            diff.unified_diff()
        );
        // Every instance has its own id in Glyphs 4.
        if version >= 4 {
            let ids: std::collections::HashSet<_> = saved
                .as_glyphs3()
                .unwrap()
                .instances
                .iter()
                .map(|i| i.id.clone().unwrap())
                .collect();
            assert_eq!(ids.len(), saved.as_glyphs3().unwrap().instances.len());
        }
    }

    /// Excerpts of fonts from Google Fonts' sources as Glyphs 4.1.1 saved
    /// them in both file format versions (`-3`, `-4`), and its own conversion
    /// of the Glyphs 4 file back to version 3 (`-4to3`). Together they hold
    /// what the versions write differently: Smart Glyphs (VT323, aoboshi,
    /// Marmelad), ghost and cap hints (Yuyu, aoboshi), gradients, colors and
    /// color palettes (KalniaGlaze), user data of components and guides
    /// (Estedad, RedHatMono-Italic), stroke positions (BBH-Bogle), localized
    /// and instance names (aoboshi, Gabarito, Yuyu), repeated classes and
    /// `:true` settings (NotoSansSamaritan), line caps, which the versions
    /// number differently (VendSans), node names (Kufam) and a master's
    /// color palettes (CoralPixels).
    #[rstest]
    fn test_save_in_other_format_version_like_glyphs(
        #[values(
            "VT323",
            "aoboshi",
            "Yuyu",
            "KalniaGlaze",
            "Estedad",
            "RedHatMono-Italic",
            "NotoSansSamaritan",
            "Marmelad",
            "BBH-Bogle",
            "Gabarito",
            "VendSans",
            "Kufam_Latin_Italic_Master",
            "CoralPixels"
        )]
        font: &str,
        #[values(("3", 4, "4"), ("4", 3, "4to3"), ("3", 3, "3"), ("4", 4, "4"))] case: (
            &str,
            i64,
            &str,
        ),
    ) {
        let (source, version, expected) = case;
        let path = |tag: &str| format!("resources/glyphs-versions/{font}-{tag}.glyphs");
        let mut ours = load(path(source).into()).unwrap();
        ours.format_specific
            .insert(KEY_SAVE_FORMAT_VERSION.into(), serde_json::json!(version));
        let ours = glyphslib::Font::Glyphs3(as_glyphs3(&ours).unwrap());
        let theirs =
            glyphslib::Font::load_str(&fs::read_to_string(path(expected)).unwrap()).unwrap();
        // What Glyphs makes up anew on every conversion, and a master's
        // icon, which Glyphs leaves out where it matches an icon it derives
        // (`GSFontMaster.defaultIconName`).
        let made_up = |l: &&str| {
            l.starts_with(".appVersion")
                || l.starts_with("lastChange = ")
                || l.starts_with("iconName = ")
                || (l.starts_with("id = \"") && l.len() == 44)
        };
        let normalize = |font: &glyphslib::Font| -> String {
            font.to_string()
                .unwrap()
                .lines()
                .filter(|l| !made_up(l))
                .collect::<Vec<_>>()
                .join("\n")
        };
        let (mut theirs, ours) = (normalize(&theirs), normalize(&ours));
        // Glyphs 4 gives Yuyu's instance an English style name of its own.
        if font == "Yuyu" && source == "3" && version == 4 {
            theirs = theirs.replace("{\nlanguage = ENG;\nvalue = Regular;\n},\n", "");
        }
        let diff = TextDiff::from_lines(&theirs, &ours);
        assert!(
            diff.ratio() == 1.0,
            "{font} {source} -> {version} differs from what Glyphs wrote:\n{}",
            diff.unified_diff()
        );
    }

    #[test]
    fn test_load_open_shape() {
        let font = load("resources/GlyphsFileFormatv3.glyphs".into()).unwrap();
        let shape = &font.glyphs.get("A").unwrap().layers[1].shapes[0];
        match shape {
            Shape::Path(p) => assert!(!p.closed),
            _ => panic!("Expected a path shape"),
        }
    }

    /// The v4-specific bits of a Glyphs 4 file survive a load (the byte-exact
    /// roundtrip is covered by `test_roundtrip` above).
    #[test]
    fn test_glyphs_file_format_v4() {
        let font = load("resources/GlyphsFileFormatv4.glyphs".into()).unwrap();

        // We remember the file format version so we can write v4 back out.
        assert_eq!(
            font.format_specific
                .get(KEY_FORMAT_VERSION)
                .and_then(|v| v.as_i64()),
            Some(4)
        );

        let glyph = font.glyphs.get("A").unwrap();
        let layer = &glyph.layers[0];

        // The Glyphs vertOrigin (an offset from the ascender) becomes an
        // absolute y: vertOrigin 20 with an ascender of 800 is y=780.
        assert_eq!(layer.vertical_advance, Some(900.0));
        assert_eq!(layer.vertical_origin, Some(780.0));

        // Images and shape groups are carried as opaque shapes.
        let opaque = layer
            .shapes
            .iter()
            .filter(|s| matches!(s, Shape::FormatSpecific(_)))
            .count();
        assert_eq!(opaque, 2, "expected an image and a shape group");

        // The component keeps its Glyphs 4 fields.
        let component = layer.shapes.iter().find_map(|s| s.as_component()).unwrap();
        assert_eq!(
            component
                .format_specific
                .get(KEY_COMPONENT_TRAVERSE_ANCHORS),
            Some(&serde_json::json!(false))
        );
        assert_eq!(
            component
                .format_specific
                .get(KEY_COMPONENT_MASTER_ID)
                .and_then(|v| v.as_str()),
            Some("m1")
        );
        assert_eq!(
            component
                .format_specific
                .get(KEY_COMPONENT_ANCHOR_TO)
                .and_then(|v| v.as_str()),
            Some("top")
        );

        // Node higher-order interpolation rides in the node's format-specific data.
        let path = layer.paths().next().unwrap();
        assert!(path.nodes.iter().any(|n| n
            .format_specific
            .get(KEY_NODE_HOI)
            .is_some_and(|v| !v.is_null())));

        // Writing v4 back out keeps those, plus the palette colour, the
        // glyph-specific axis metadata and the instance types.
        let out = as_glyphs3(&font).unwrap();
        assert_eq!(out.format_version, 4);

        let out_a = out.glyphs.iter().find(|g| g.name == "A").unwrap();
        assert!(matches!(
            out_a.color,
            Some(glyphslib::common::Color::ColorPaletteIndex { index: 5, .. })
        ));
        assert_eq!(out_a.axes[0].tag, "sz01");
        assert!(out_a.axes[0].hidden);

        use glyphslib::glyphs3::ExportType;
        assert!(out
            .instances
            .iter()
            .any(|i| i.export_type == ExportType::Variable));
        assert!(out
            .instances
            .iter()
            .any(|i| i.export_type == ExportType::Icon));
        assert!(out
            .instances
            .iter()
            .any(|i| i.export_type == ExportType::Single));
    }

    #[test]
    fn test_designspace() {
        let font = load("resources/Designspace.glyphs".into()).unwrap();
        assert_eq!(font.axes.len(), 1);
        assert_eq!(font.axes[0].name.get_default().unwrap(), "Weight");
        assert_eq!(font.axes[0].tag, Tag::new(b"wght"));

        assert!(font.axes[0].map.is_some());
        // Axes values are in userspace units
        assert_eq!(font.axes[0].min.unwrap().to_f64(), 100.0);
        assert_eq!(font.axes[0].max.unwrap().to_f64(), 600.0);
        assert_eq!(font.axes[0].default.unwrap().to_f64(), 100.0);
        // Every master carries an "Axis Location", so the mapping comes from
        // those and not from the instances' weightClass values. Verified
        // against glyphsLib's own output for this file.
        assert_eq!(axis_map(&font.axes[0]), vec![(100.0, 1.0), (600.0, 199.0)]);
        // Master locations are in designspace units
        assert_eq!(
            font.masters[0]
                .location
                .get(Tag::new(b"wght"))
                .unwrap()
                .to_f64(),
            1.0
        );
        assert_eq!(
            font.masters[1]
                .location
                .get(Tag::new(b"wght"))
                .unwrap()
                .to_f64(),
            199.0
        );
        // Instance locations are in designspace units
        assert_eq!(
            font.instances[0]
                .location
                .get(Tag::new(b"wght"))
                .unwrap()
                .to_f64(),
            1.0
        );
        assert_eq!(
            font.instances[1]
                .location
                .get(Tag::new(b"wght"))
                .unwrap()
                .to_f64(),
            7.0
        );
    }

    #[test]
    fn test_kern_groups() {
        let font = load("resources/KernGroupTest.glyphs".into()).unwrap();
        // *Left* side of D looks like an H, so is used when D is the *second*
        // glyph in a pair.
        let h_group = font.second_kern_groups.get("H").unwrap();
        assert_eq!(h_group.len(), 2);
        assert!(h_group.contains(&"D".into()));
        assert!(h_group.contains(&"Dcaron".into()));
        // *Right* side of D looks like an O, so is used when D is the *first*
        // glyph in a pair.
        let o_group = font.first_kern_groups.get("O").unwrap();
        assert_eq!(o_group.len(), 3);
        assert!(o_group.contains(&"D".into()));
        assert!(o_group.contains(&"Dcaron".into()));
        assert!(o_group.contains(&"Dcroat".into()));
    }

    #[test]
    fn test_timezone() {
        let font_date = "2024-05-08 05:56:55 +0000";
        let date = font_date.parse().unwrap_or_else(|_| chrono::Utc::now());
        let formatted_date = date.format("%Y-%m-%d %H:%M:%S +0000").to_string();
        assert_eq!(formatted_date, font_date);
    }

    #[test]
    fn test_smart_components() {
        let font = load("resources/NotoSansGrantha-SmartComponent.glyphs".into()).unwrap();
        let glyph = font.glyphs.get("_part.iMatra").unwrap();
        assert_eq!(glyph.component_axes.len(), 5);
        assert_eq!(glyph.component_axes[0].name.get_default().unwrap(), "Width");
        assert_eq!(glyph.component_axes[0].min, Some(UserCoord::new(0.0)));
        assert_eq!(glyph.component_axes[0].max, Some(UserCoord::new(800.0)));
        assert_eq!(glyph.component_axes[1].name.get_default().unwrap(), "Dip");
        assert_eq!(glyph.component_axes[1].min, Some(UserCoord::new(0.0)));
        assert_eq!(glyph.component_axes[1].max, Some(UserCoord::new(100.0)));
        // Each layer should have some location in those axes
        // Layer one is called Wide
        assert_eq!(glyph.layers[1].name.as_ref().unwrap(), "Wide");
        let layer_location = &glyph.layers[1].smart_component_location;
        assert!(!layer_location.is_empty());
        println!("Layer location: {:?}", layer_location);
        assert_eq!(layer_location.len(), 1);
        assert_eq!(
            layer_location.get("Width").unwrap(),
            &DesignCoord::new(800.0)
        );

        // Test the user end
        let layer = font
            .glyphs
            .get("ny_ji_gran")
            .unwrap()
            .layers
            .first()
            .unwrap();
        let shapes = &layer.shapes;
        let Shape::Component(first) = shapes.first().unwrap() else {
            panic!("Expected component shape");
        };
        assert_eq!(first.reference, "ny_ja_gran");
        assert_eq!(first.location, IndexMap::<String, DesignCoord>::default());
        // But the second is a smart component
        let Shape::Component(smart) = &shapes[1] else {
            panic!("Expected component shape");
        };
        assert_eq!(smart.reference, "_part.iMatra");
        assert_eq!(smart.location.len(), 3);
        assert_eq!(
            smart.location.get("Width").unwrap(),
            &DesignCoord::new(520.0)
        );
    }

    #[test]
    fn test_load_marks() {
        let font = load("resources/NotoSansLimbu.glyphs".into()).unwrap();
        /*
        category = Mark;
        glyphname = uni1938;
        subCategory = Spacing;
        */
        let uni1938 = font.glyphs.get("uni1938").unwrap();
        assert_eq!(uni1938.category, GlyphCategory::Base);
        // When we send it back to Glyphs, it should get Mark/Spacing again
        let glyphs_glyph = glyph_to_glyphs(
            uni1938,
            &font.axes.iter().map(|a| a.tag).collect::<Vec<_>>(),
            None,
            None,
        );
        assert_eq!(glyphs_glyph.category, Some("Mark".to_string()));
        assert_eq!(glyphs_glyph.subcategory, Some("Spacing".to_string()));
    }

    /// G3RTLKerning.glyphs with these features
    fn with_features(features: &str) -> Font {
        let source = std::fs::read_to_string("resources/G3RTLKerning.glyphs").unwrap();
        let source = source.replacen(
            "fontMaster = (",
            &format!("features = (\n{features}\n);\nfontMaster = ("),
            1,
        );
        load_str(&source, "resources/G3RTLKerning.glyphs".into()).unwrap()
    }

    fn kern_code(font: &Font) -> String {
        font.features
            .features
            .iter()
            .find(|(tag, _)| tag == "kern")
            .map(|(_, code)| code.code.clone())
            .unwrap()
    }

    #[test]
    fn test_a_kern_feature_of_its_own_gets_the_kerning_after_it() {
        // Glyphs puts its kerning after a kern feature's code; glyphsLib and
        // fontc put an insertion marker at its end (Estedad)
        let font = with_features("{\ncode = \"pos A' 5 T;\";\ntag = kern;\n}");
        assert_eq!(kern_code(&font), "pos A' 5 T;\n# Automatic Code");
        // and it goes again on the way back
        let glyphs = as_glyphs3(&font).unwrap();
        let kern = glyphs.features.iter().find(|f| f.tag == "kern").unwrap();
        assert_eq!(kern.code, "pos A' 5 T;");
    }

    #[test]
    fn test_kern_features_with_a_marker_or_automatic_get_none() {
        let font = with_features("{\ncode = \"# Automatic Code\npos A' 5 T;\";\ntag = kern;\n}");
        assert_eq!(kern_code(&font), "# Automatic Code\npos A' 5 T;");
        let font = with_features("{\nautomatic = 1;\ncode = \"pos A T -5;\";\ntag = kern;\n}");
        assert_eq!(kern_code(&font), "pos A T -5;");
    }

    #[test]
    #[cfg(feature = "fontir")]
    fn test_a_kern_feature_of_its_own_compiles_with_the_kerning() {
        use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};
        use skrifa::raw::{tables::gpos::PositionSubtables, TableProvider};
        let font = with_features("{\ncode = \"pos A' 5 T;\";\ntag = kern;\n}");
        let binary = BabelfontIrSource::compile(font, CompilationOptions::default()).unwrap();
        let gpos = skrifa::FontRef::new(&binary).unwrap().gpos().unwrap();
        let lookups = gpos.lookup_list().unwrap();
        let kinds: Vec<&str> = gpos
            .feature_list()
            .unwrap()
            .feature_records()
            .iter()
            .filter(|r| r.feature_tag() == skrifa::Tag::new(b"kern"))
            .take(1)
            .flat_map(|r| {
                r.feature(gpos.feature_list().unwrap().offset_data())
                    .unwrap()
                    .lookup_list_indices()
                    .iter()
                    .map(|i| {
                        match lookups
                            .lookups()
                            .get(i.get() as usize)
                            .unwrap()
                            .subtables()
                            .unwrap()
                        {
                            PositionSubtables::Pair(_) => "pair",
                            PositionSubtables::ChainContextual(_) => "context",
                            _ => "other",
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
        // the feature's own lookup, then the kerning
        assert_eq!(kinds.first(), Some(&"context"), "{kinds:?}");
        assert!(kinds[1..].contains(&"pair"), "{kinds:?}");
    }

    #[test]
    fn test_load_ligatures() {
        // a ligature by its subcategory, as glyphsLib and fontc have it; a
        // category other than Letter goes back as it came
        for category in ["Letter", "Number"] {
            let glyphs_glyph = glyphslib::glyphs3::Glyph {
                name: "one_half".into(),
                category: Some(category.into()),
                subcategory: Some("Ligature".into()),
                ..Default::default()
            };
            let glyph = crate::glyph::glyphs::from_glyphs(&glyphs_glyph, &[]).unwrap();
            assert_eq!(glyph.category, GlyphCategory::Ligature);
            let back = glyph_to_glyphs(&glyph, &[], None, None);
            assert_eq!(back.category, Some(category.to_string()));
            assert_eq!(back.subcategory, Some("Ligature".to_string()));
        }
    }

    #[test]
    fn test_load_spacing_combining_marks() {
        // a mark that attaches and advances is a mark, as glyphsLib and
        // fontc have it
        let glyphs_glyph = glyphslib::glyphs3::Glyph {
            name: "matra".into(),
            category: Some("Mark".into()),
            subcategory: Some("Spacing Combining".into()),
            ..Default::default()
        };
        let glyph = crate::glyph::glyphs::from_glyphs(&glyphs_glyph, &[]).unwrap();
        assert_eq!(glyph.category, GlyphCategory::Mark);
    }

    #[test]
    fn test_save_classes_without_subcategory() {
        let to_glyphs = |category, width| {
            let glyph = crate::Glyph {
                name: "g".into(),
                category,
                layers: vec![Layer {
                    width,
                    ..Default::default()
                }],
                ..Default::default()
            };
            glyph_to_glyphs(&glyph, &[], None, None)
        };
        let mark = to_glyphs(GlyphCategory::Mark, 0.0);
        assert_eq!(mark.category, Some("Mark".to_string()));
        assert_eq!(mark.subcategory, Some("Nonspacing".to_string()));
        // Glyphs zeroes a Nonspacing mark's advance, so a mark that advances is Spacing Combining
        let spacing_mark = to_glyphs(GlyphCategory::Mark, 300.0);
        assert_eq!(spacing_mark.category, Some("Mark".to_string()));
        assert_eq!(
            spacing_mark.subcategory,
            Some("Spacing Combining".to_string())
        );
        let ligature = to_glyphs(GlyphCategory::Ligature, 600.0);
        assert_eq!(ligature.subcategory, Some("Ligature".to_string()));
        assert_eq!(to_glyphs(GlyphCategory::Base, 600.0).subcategory, None);
    }

    #[test]
    fn all_the_axes() {
        let font = load("resources/RadioCanadaDisplay-Italic.glyphs".into()).unwrap();
        assert_eq!(font.axes[0].tag, Tag::new(b"wght"));
        assert_eq!(
            font.masters[0].location,
            DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(400.0))])
        );
        assert_eq!(font.axes[0].min, Some(UserCoord::new(400.0)));
        // No instance supplies a distinct user location, so the axis is
        // unmapped and spans the masters. Matches glyphsLib's output.
        assert_eq!(font.axes[0].max, Some(UserCoord::new(700.0)));
        assert_eq!(font.axes[0].default, Some(UserCoord::new(400.0)));
        assert!(font.axes[0].map.is_none());
    }

    fn load_test_font(source: &str) -> Font {
        load_str(source, PathBuf::from("test.glyphs")).unwrap()
    }

    fn axis_map(axis: &Axis) -> Vec<(f64, f64)> {
        axis.map
            .as_ref()
            .map(|map| {
                map.iter()
                    .map(|(user, design)| (user.to_f64(), design.to_f64()))
                    .collect()
            })
            .unwrap_or_default()
    }

    fn user(axis: &Axis, which: fn(&Axis) -> Option<UserCoord>) -> Option<f64> {
        which(axis).map(|coord| coord.to_f64())
    }

    /// A two-master width-axis font. The design locations are deliberately not
    /// the user-space percentages, so a raw class leaking through would show up.
    const WIDTH_CLASS_FONT: &str = r#"{
.formatVersion = 3;
.appVersion = "3243";
familyName = WidthClassTest;
date = "2024-05-08 05:56:55 +0000";
unitsPerEm = 1000;
versionMajor = 1;
versionMinor = 0;
axes = (
{
name = Width;
tag = wdth;
}
);
metrics = ();
fontMaster = (
{
id = m01;
name = Condensed;
axesValues = (0);
metricValues = ();
},
{
id = m02;
name = Regular;
axesValues = (1000);
metricValues = ();
}
);
glyphs = ();
instances = (
{
name = Condensed;
axesValues = (0);
widthClass = 3;
},
{
name = SemiCondensed;
axesValues = (500);
widthClass = 4;
},
{
name = Regular;
axesValues = (1000);
widthClass = 5;
}
);
}
"#;

    #[test]
    fn width_class_is_a_percentage_not_a_raw_class() {
        // An OS/2 width class of 4 means "semi-condensed", i.e. 87.5% of the
        // normal width — the user-space value 4 is wrong (as is 3, "condensed").
        let font = load_test_font(WIDTH_CLASS_FONT);
        let axis = &font.axes[0];
        assert_eq!(axis.tag, Tag::new(b"wdth"));
        assert_eq!(
            axis_map(axis),
            vec![(75.0, 0.0), (87.5, 500.0), (100.0, 1000.0)]
        );
        assert_eq!(user(axis, |a| a.min), Some(75.0));
        assert_eq!(user(axis, |a| a.max), Some(100.0));
        // Default comes from the "Regular" master, inverted through the mapping.
        assert_eq!(user(axis, |a| a.default), Some(100.0));
    }

    #[test]
    fn instance_axis_location_overrides_the_width_class() {
        // The "Axis Location" custom parameter is already in user space, so it
        // wins over the width class (and, before this was fixed, it was read
        // from the wrong place and ignored entirely).
        let font = load_test_font(&WIDTH_CLASS_FONT.replace(
            r#"name = Condensed;
axesValues = (0);
widthClass = 3;"#,
            r#"name = Condensed;
axesValues = (0);
widthClass = 3;
customParameters = (
{
name = "Axis Location";
value = (
{
Axis = Width;
Location = 80;
}
);
}
);"#,
        ));
        let axis = &font.axes[0];
        assert_eq!(
            axis_map(axis),
            vec![(80.0, 0.0), (87.5, 500.0), (100.0, 1000.0)]
        );
        assert_eq!(user(axis, |a| a.min), Some(80.0));
    }

    #[test]
    fn axis_location_on_masters_builds_the_mapping_from_masters() {
        // When every master has an "Axis Location", that is the mapping; the
        // instances' weight classes must not contribute extra points.
        let font = load_test_font(
            r#"{
.formatVersion = 3;
.appVersion = "3243";
familyName = MasterAxisLocationTest;
date = "2024-05-08 05:56:55 +0000";
unitsPerEm = 1000;
versionMajor = 1;
versionMinor = 0;
axes = (
{
name = Weight;
tag = wght;
}
);
metrics = ();
fontMaster = (
{
id = m01;
name = Thin;
axesValues = (1);
customParameters = (
{
name = "Axis Location";
value = (
{
Axis = Weight;
Location = 100;
}
);
}
);
metricValues = ();
},
{
id = m02;
name = Bold;
axesValues = (199);
customParameters = (
{
name = "Axis Location";
value = (
{
Axis = Weight;
Location = 700;
}
);
}
);
metricValues = ();
}
);
glyphs = ();
instances = (
{
name = Thin;
axesValues = (1);
weightClass = 100;
},
{
name = Regular;
axesValues = (100);
weightClass = 400;
},
{
name = Bold;
axesValues = (199);
weightClass = 700;
}
);
}
"#,
        );
        let axis = &font.axes[0];
        assert_eq!(axis_map(axis), vec![(100.0, 1.0), (700.0, 199.0)]);
        assert_eq!(user(axis, |a| a.min), Some(100.0));
        assert_eq!(user(axis, |a| a.max), Some(700.0));
        assert_eq!(user(axis, |a| a.default), Some(100.0));
    }

    #[test]
    fn explicit_axis_mappings_take_precedence_over_instances() {
        let font = load_test_font(
            r#"{
.formatVersion = 3;
.appVersion = "3243";
familyName = ExplicitMappingTest;
date = "2024-05-08 05:56:55 +0000";
unitsPerEm = 1000;
versionMajor = 1;
versionMinor = 0;
axes = (
{
name = Weight;
tag = wght;
}
);
customParameters = (
{
name = "Axis Mappings";
value = {
wght = {
300 = 1;
900 = 199;
};
};
}
);
metrics = ();
fontMaster = (
{
id = m01;
name = Light;
axesValues = (1);
metricValues = ();
},
{
id = m02;
name = Black;
axesValues = (199);
metricValues = ();
}
);
glyphs = ();
instances = (
{
name = Regular;
axesValues = (100);
weightClass = 400;
}
);
}
"#,
        );
        let axis = &font.axes[0];
        // The instance's weightClass would map 400 -> 100 if it were consulted.
        assert_eq!(axis_map(axis), vec![(300.0, 1.0), (900.0, 199.0)]);
        assert_eq!(user(axis, |a| a.min), Some(300.0));
        assert_eq!(user(axis, |a| a.max), Some(900.0));
    }

    #[test]
    fn identity_mapping_is_elided() {
        let font = load_test_font(
            r#"{
.formatVersion = 3;
.appVersion = "3243";
familyName = IdentityTest;
date = "2024-05-08 05:56:55 +0000";
unitsPerEm = 1000;
versionMajor = 1;
versionMinor = 0;
axes = (
{
name = Weight;
tag = wght;
}
);
metrics = ();
fontMaster = (
{
id = m01;
name = Regular;
axesValues = (400);
metricValues = ();
}
);
glyphs = ();
instances = (
{
name = Regular;
axesValues = (400);
weightClass = 400;
}
);
}
"#,
        );
        let axis = &font.axes[0];
        assert!(axis.map.is_none(), "an identity mapping must be elided");
        assert_eq!(user(axis, |a| a.min), Some(400.0));
        assert_eq!(user(axis, |a| a.max), Some(400.0));
        assert_eq!(user(axis, |a| a.default), Some(400.0));
    }

    #[test]
    fn instance_without_a_weight_class_defaults_to_400() {
        // An instance with no `weightClass` is Glyphs' "Regular", i.e. 400 in
        // user space — not its design location. (This replaces the old
        // name-matching heuristic.)
        let font = load_test_font(
            r#"{
.formatVersion = 3;
.appVersion = "3243";
familyName = DefaultWeightTest;
date = "2024-05-08 05:56:55 +0000";
unitsPerEm = 1000;
versionMajor = 1;
versionMinor = 0;
axes = (
{
name = Weight;
tag = wght;
}
);
metrics = ();
fontMaster = (
{
id = m01;
name = Regular;
axesValues = (94);
metricValues = ();
},
{
id = m02;
name = Bold;
axesValues = (152);
metricValues = ();
}
);
glyphs = ();
instances = (
{
name = Regular;
axesValues = (94);
},
{
name = Bold;
axesValues = (152);
weightClass = 700;
}
);
}
"#,
        );
        let axis = &font.axes[0];
        assert_eq!(axis_map(axis), vec![(400.0, 94.0), (700.0, 152.0)]);
        assert_eq!(user(axis, |a| a.min), Some(400.0));
        assert_eq!(user(axis, |a| a.max), Some(700.0));
        assert_eq!(user(axis, |a| a.default), Some(400.0));
    }

    #[test]
    fn non_derivable_axis_map_survives_a_glyphs_round_trip() {
        // A map with more points than the masters can supply has to be written
        // as an explicit "Axis Mappings" custom parameter, otherwise the
        // reloader collapses it to the points it can re-derive (here a single
        // master-derived identity pair). This is what a designspace `<map>`
        // loses when it round-trips through Glyphs without the parameter.
        let mut font = Font::new();

        let mut weight = Axis::new("Weight", Tag::new(b"wght"));
        weight.min = Some(UserCoord::new(100.0));
        weight.max = Some(UserCoord::new(900.0));
        weight.default = Some(UserCoord::new(400.0));
        weight.map = Some(vec![
            (UserCoord::new(100.0), DesignCoord::new(0.0)),
            (UserCoord::new(400.0), DesignCoord::new(94.0)),
            (UserCoord::new(700.0), DesignCoord::new(152.0)),
            (UserCoord::new(900.0), DesignCoord::new(200.0)),
        ]);
        font.axes.push(weight);

        // Two masters whose design locations only pin down two of those points.
        font.masters.push(Master::new(
            "Light",
            "m01",
            DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(94.0))]),
        ));
        font.masters.push(Master::new(
            "Black",
            "m02",
            DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(152.0))]),
        ));

        let glyphs = as_glyphs3(&font).unwrap();
        // The writer must have decided the map can't be re-derived and emitted
        // an explicit parameter rather than dropping it.
        assert!(
            glyphs
                .custom_parameters
                .iter()
                .any(|cp| cp.name == "Axis Mappings"),
            "expected an Axis Mappings custom parameter to preserve the map"
        );
        let serialized = glyphslib::Font::Glyphs3(glyphs).to_string().unwrap();
        let reloaded = load_str(&serialized, PathBuf::from("roundtrip.glyphs")).unwrap();

        let axis = &reloaded.axes[0];
        assert_eq!(axis.tag, Tag::new(b"wght"));
        assert_eq!(
            axis_map(axis),
            vec![(100.0, 0.0), (400.0, 94.0), (700.0, 152.0), (900.0, 200.0),]
        );
        assert_eq!(user(axis, |a| a.min), Some(100.0));
        assert_eq!(user(axis, |a| a.max), Some(900.0));
        assert_eq!(user(axis, |a| a.default), Some(400.0));
    }

    #[test]
    fn derivable_axis_map_needs_no_custom_parameter() {
        // The same map the loader re-derives from the instances' weight classes
        // must not be written back out as an explicit "Axis Mappings"
        // parameter; that would be noise, not preservation.
        let mut font = Font::new();

        let mut weight = Axis::new("Weight", Tag::new(b"wght"));
        weight.min = Some(UserCoord::new(400.0));
        weight.max = Some(UserCoord::new(700.0));
        weight.default = Some(UserCoord::new(400.0));
        weight.map = Some(vec![
            (UserCoord::new(400.0), DesignCoord::new(94.0)),
            (UserCoord::new(700.0), DesignCoord::new(152.0)),
        ]);
        font.axes.push(weight);

        font.masters.push(Master::new(
            "Regular",
            "m01",
            DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(94.0))]),
        ));
        font.masters.push(Master::new(
            "Bold",
            "m02",
            DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(152.0))]),
        ));

        for (name, design, weight_class) in [("Regular", 94.0, 400), ("Bold", 152.0, 700)] {
            let mut instance = crate::Instance::default();
            instance.name.set_default(name.to_string());
            instance.location =
                DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(design))]);
            instance.format_specific.insert(
                super::KEY_WEIGHT_CLASS.to_string(),
                serde_json::json!(weight_class),
            );
            font.instances.push(instance);
        }

        let glyphs = as_glyphs3(&font).unwrap();
        assert!(
            !glyphs
                .custom_parameters
                .iter()
                .any(|cp| cp.name == "Axis Mappings"),
            "a derivable mapping must not be written as an explicit parameter"
        );
    }

    #[test]
    fn axis_mappings_custom_parameter_is_consumed_on_load() {
        let font = load_test_font(
            r#"{
.formatVersion = 3;
familyName = MappingsConsumedTest;
date = "2024-05-08 05:56:55 +0000";
unitsPerEm = 1000;
versionMajor = 1;
versionMinor = 0;
axes = (
{
name = Weight;
tag = wght;
}
);
customParameters = (
{
name = "Axis Mappings";
value = {
wght = {
300 = 1;
900 = 199;
};
};
}
);
metrics = ();
fontMaster = (
{
id = m01;
name = Light;
axesValues = (1);
metricValues = ();
},
{
id = m02;
name = Black;
axesValues = (199);
metricValues = ();
}
);
glyphs = ();
instances = ();
}
"#,
        );

        // The mapping is folded into the native field...
        assert_eq!(axis_map(&font.axes[0]), vec![(300.0, 1.0), (900.0, 199.0)]);
        // ...and the custom parameter stays, to be written back as it was
        // while `axis.map` says the same.
        assert!(super::get_cp(&font.format_specific, "Axis Mappings").is_some());
        let saved = as_glyphs3(&font).unwrap();
        let written: Vec<_> = saved
            .custom_parameters
            .iter()
            .filter(|cp| cp.name == "Axis Mappings")
            .collect();
        assert_eq!(written.len(), 1);
        assert_eq!(
            serde_json::to_value(&written[0].value).unwrap(),
            super::get_cp(&font.format_specific, "Axis Mappings").unwrap()["value"]
        );
    }

    /// A Glyphs 3 file with the given `.appVersion` line (or none) and,
    /// optionally, an `Axis Mappings` custom parameter.
    fn app_version_test_source(app_version_line: &str, axis_mappings: bool) -> String {
        let custom_parameters = if axis_mappings {
            "customParameters = (\n{\nname = \"Axis Mappings\";\nvalue = {\nwght = {\n300 = 1;\n900 = 199;\n};\n};\n}\n);\n"
        } else {
            ""
        };
        format!(
            "{{\n.formatVersion = 3;\n{app_version_line}familyName = AppVersionTest;\n\
             date = \"2024-05-08 05:56:55 +0000\";\nunitsPerEm = 1000;\nversionMajor = 1;\n\
             versionMinor = 0;\naxes = (\n{{\nname = Weight;\ntag = wght;\n}}\n);\n\
             {custom_parameters}metrics = ();\nfontMaster = (\n{{\nid = m01;\nname = Light;\n\
             axesValues = (1);\nmetricValues = ();\n}},\n{{\nid = m02;\nname = Black;\n\
             axesValues = (199);\nmetricValues = ();\n}}\n);\nglyphs = ();\ninstances = ();\n}}\n"
        )
    }

    fn written_app_version_and_mapping(source: &str) -> (String, Option<serde_json::Value>) {
        let saved = as_glyphs3(&load_test_font(source)).unwrap();
        let mapping = saved
            .custom_parameters
            .iter()
            .find(|cp| cp.name == "Axis Mappings")
            .map(|cp| serde_json::to_value(&cp.value).unwrap());
        (saved.app_version, mapping)
    }

    #[test]
    fn axis_mappings_from_an_old_build_are_written_for_a_build_that_keeps_them() {
        // Glyphs swaps the keys and values of Axis Mappings in files from build
        // 3167 or earlier, or without a build number. Babelfont reads the
        // mapping as written, so the file it writes must say a build at which
        // Glyphs reads it the same way.
        for line in [".appVersion = \"3151\";\n", ".appVersion = \"\";\n", ""] {
            let (app_version, mapping) =
                written_app_version_and_mapping(&app_version_test_source(line, true));
            assert_eq!(app_version, "3168", "for {line:?}");
            assert_eq!(
                mapping,
                Some(serde_json::json!({ "wght": { "300": 1, "900": 199 } })),
                "the mapping must be written as read, for {line:?}"
            );
        }
    }

    #[test]
    fn app_version_is_kept_when_glyphs_reads_the_file_as_written() {
        // A new enough build, with a mapping.
        let (app_version, _) = written_app_version_and_mapping(&app_version_test_source(
            ".appVersion = \"3260\";\n",
            true,
        ));
        assert_eq!(app_version, "3260");
        // An old build without a mapping: nothing for Glyphs to convert.
        let (app_version, mapping) = written_app_version_and_mapping(&app_version_test_source(
            ".appVersion = \"3151\";\n",
            false,
        ));
        assert_eq!(app_version, "3151");
        assert_eq!(mapping, None);
    }

    #[test]
    fn a_font_from_another_format_gets_a_build_that_keeps_its_axis_mappings() {
        // A font that never was a Glyphs file has no build number, which
        // Glyphs reads as old.
        let mut font = Font::new();
        let mut weight = Axis::new("Weight", Tag::new(b"wght"));
        weight.min = Some(UserCoord::new(300.0));
        weight.max = Some(UserCoord::new(900.0));
        weight.default = Some(UserCoord::new(300.0));
        weight.map = Some(vec![
            (UserCoord::new(300.0), DesignCoord::new(1.0)),
            (UserCoord::new(900.0), DesignCoord::new(199.0)),
        ]);
        font.axes.push(weight);
        for (name, id, value) in [("Light", "m01", 1.0), ("Black", "m02", 199.0)] {
            font.masters.push(Master::new(
                name,
                id,
                DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(value))]),
            ));
        }
        let glyphs = as_glyphs3(&font).unwrap();
        assert!(glyphs
            .custom_parameters
            .iter()
            .any(|cp| cp.name == "Axis Mappings"));
        assert_eq!(glyphs.app_version, "3168");
    }

    #[test]
    fn edited_axis_map_overrides_a_stashed_mapping_on_export() {
        let mut font = Font::new();

        let mut weight = Axis::new("Weight", Tag::new(b"wght"));
        weight.min = Some(UserCoord::new(300.0));
        weight.max = Some(UserCoord::new(900.0));
        weight.default = Some(UserCoord::new(300.0));
        weight.map = Some(vec![
            (UserCoord::new(300.0), DesignCoord::new(1.0)),
            (UserCoord::new(900.0), DesignCoord::new(199.0)),
        ]);
        font.axes.push(weight);

        font.masters.push(Master::new(
            "Light",
            "m01",
            DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(1.0))]),
        ));
        font.masters.push(Master::new(
            "Black",
            "m02",
            DesignLocation::from(vec![(Tag::new(b"wght"), DesignCoord::new(199.0))]),
        ));

        // A stale stash left behind by an older load must not shadow the edited
        // native map, and must not be written out alongside a regenerated one.
        font.format_specific.insert(
            format!("{}{}", super::KEY_CUSTOM_PARAMETERS, "Axis Mappings"),
            serde_json::json!({
                "value": { "wght": { "400": 94.0, "700": 152.0 } },
                "disabled": false,
            }),
        );

        let glyphs = as_glyphs3(&font).unwrap();
        let axis_mappings: Vec<_> = glyphs
            .custom_parameters
            .iter()
            .filter(|cp| cp.name == "Axis Mappings")
            .collect();
        assert_eq!(
            axis_mappings.len(),
            1,
            "the stale mapping must be dropped, not duplicated"
        );

        let serialized = glyphslib::Font::Glyphs3(glyphs).to_string().unwrap();
        let reloaded = load_str(&serialized, PathBuf::from("roundtrip.glyphs")).unwrap();
        assert_eq!(
            axis_map(&reloaded.axes[0]),
            vec![(300.0, 1.0), (900.0, 199.0)]
        );
    }

    #[cfg(feature = "ufo")]
    #[test]
    fn ufo_vertical_metrics_become_vert_width_and_origin() {
        let glyphs_vert = |font: &Font, name: &str| {
            let glyphs = as_glyphs3(font).unwrap();
            let layer = &glyphs
                .glyphs
                .iter()
                .find(|g| g.name == name)
                .unwrap()
                .layers[0];
            (layer.vert_width, layer.vert_origin)
        };
        let font = crate::load("resources/Test1.ufo").unwrap();
        // The native fields hold the UFO's own values: the glif `height` and the
        // absolute `public.verticalOrigin`.
        let layer = |name: &str| &font.glyphs.get(name).unwrap().layers[0];
        assert_eq!(layer("A").vertical_advance, Some(1000.0));
        assert_eq!(layer("A").vertical_origin, Some(880.0));
        assert_eq!(layer("B").vertical_advance, Some(1200.0));
        assert_eq!(layer("B").vertical_origin, None);
        // A glif with no height states no vertical advance, which is different
        // from an explicit zero.
        assert_eq!(layer("i").vertical_advance, None);

        // The typo ascender is 900 and the typo descender -300, so Glyphs.app's
        // defaults are a vertWidth of 1200 and a vertical origin at y=900.
        assert_eq!(glyphs_vert(&font, "A"), (Some(1000.0), Some(20.0)));
        // B's advance equals the default, so it is left out; it has no origin.
        assert_eq!(glyphs_vert(&font, "B"), (None, None));
        assert_eq!(glyphs_vert(&font, "i"), (None, None));
    }

    /// Glyphs 3.4.1 wrote the same slanted component as Glyphs 3 fields (angle
    /// 20, scale 0.8, slant 10 degrees) and as a Glyphs 2 matrix (glyphs-reader's
    /// read_component_slant_2_and_3): this matrix
    const SLANTED: [f64; 4] = [0.8, 0.2736, -0.1411, 0.7518];

    const SLANTED_GLYPHS3: &str = r#"{
.formatVersion = 3;
familyName = Slant;
fontMaster = ({id = m01; name = Regular;});
glyphs = (
{glyphname = A; layers = ({layerId = m01; shapes = ({ref = B; angle = 20; scale = (0.8,0.8); slant = (10,0);}); width = 600;});},
{glyphname = B; layers = ({layerId = m01; shapes = ({closed = 1; nodes = ((0,0,l),(100,0,l),(100,100,l));}); width = 600;});}
);
unitsPerEm = 1000;
}
"#;

    const SLANTED_GLYPHS2: &str = r#"{
.appVersion = "1342";
familyName = Slant;
fontMaster = ({id = m01;});
glyphs = (
{glyphname = A; layers = ({layerId = m01; components = ({name = B; transform = "{0.8, 0.27362, -0.14106, 0.75175, 0, 0}";}); width = 600;});},
{glyphname = B; layers = ({layerId = m01; paths = ({closed = 1; nodes = ("0 0 LINE","100 0 LINE","100 100 LINE");}); width = 600;});}
);
unitsPerEm = 1000;
}
"#;

    fn first_component(font: &Font, glyph: &str) -> crate::Component {
        font.glyphs.get(glyph).unwrap().layers[0]
            .components()
            .next()
            .unwrap()
            .clone()
    }

    fn assert_coeffs(affine: kurbo::Affine, expected: [f64; 4], tolerance: f64) {
        let coeffs = affine.as_coeffs();
        assert!(
            coeffs[..4]
                .iter()
                .zip(expected)
                .all(|(a, b)| (a - b).abs() < tolerance),
            "{coeffs:?} is not {expected:?}"
        );
    }

    /// The matrix Glyphs makes of its fields: translate · slant · rotate ·
    /// scale, the slant's tangent and the angle in degrees
    fn glyphs_matrix(component: &glyphslib::glyphs3::Component) -> kurbo::Affine {
        let (sx, sy) = component.slant;
        kurbo::Affine::translate((component.position.0 as f64, component.position.1 as f64))
            * kurbo::Affine::skew(
                (sx as f64).to_radians().tan(),
                (sy as f64).to_radians().tan(),
            )
            * kurbo::Affine::rotate((component.angle as f64).to_radians())
            * kurbo::Affine::scale_non_uniform(component.scale.0 as f64, component.scale.1 as f64)
    }

    #[test]
    fn component_slant_is_degrees() {
        let font = load_test_font(SLANTED_GLYPHS3);
        let component = first_component(&font, "A");
        // the model's skew is radians
        assert!((component.transform.skew.0 - 10f64.to_radians()).abs() < 1e-6);
        assert_coeffs(component.transform.as_affine(), SLANTED, 1e-4);
        // and written back in degrees
        let glyphs = as_glyphs3(&font).unwrap();
        let glyphslib::glyphs3::Shape::Component(written) = &glyphs.glyphs[0].layers[0].shapes[0]
        else {
            panic!("not a component");
        };
        assert!((written.slant.0 - 10.0).abs() < 1e-4, "{:?}", written.slant);
        assert!((written.angle - 20.0).abs() < 1e-4, "{}", written.angle);
    }

    #[test]
    fn glyphs2_component_matrix_keeps_its_slant() {
        let font = load_test_font(SLANTED_GLYPHS2);
        let component = first_component(&font, "A");
        assert_coeffs(component.transform.as_affine(), SLANTED, 1e-4);
        assert!((component.transform.skew.0.to_degrees() - 10.0).abs() < 1e-2);
        assert!((component.transform.rotation.to_degrees() - 20.0).abs() < 1e-2);
    }

    #[test]
    fn transforms_in_the_other_order_are_written_as_glyphs_reads_them() {
        use crate::common::decomposition::{DecomposedAffine, TransformOrder};
        let mut font = load_test_font(SLANTED_GLYPHS3);
        for (rotation, scale, skew) in [
            (30f64, (1.2, 0.7), (15f64, 0f64)),
            (-75.0, (-0.8, 1.1), (-20.0, 0.0)),
            (120.0, (0.5, 0.9), (5.0, 0.0)),
        ] {
            let transform = DecomposedAffine {
                translation: (10.0, 20.0),
                scale,
                rotation: rotation.to_radians(),
                skew: (skew.0.to_radians(), skew.1.to_radians()),
                order: TransformOrder::RestOfTheWorld,
            };
            if let Some(crate::Shape::Component(component)) =
                font.glyphs.get_mut("A").unwrap().layers[0]
                    .shapes
                    .get_mut(0)
            {
                component.transform = transform;
            }
            let glyphs = as_glyphs3(&font).unwrap();
            let glyphslib::glyphs3::Shape::Component(written) =
                &glyphs.glyphs[0].layers[0].shapes[0]
            else {
                panic!("not a component");
            };
            let expected = transform.as_affine().as_coeffs();
            let got = glyphs_matrix(written).as_coeffs();
            assert!(
                got.iter().zip(expected).all(|(a, b)| (a - b).abs() < 1e-3),
                "{got:?} is not {expected:?}"
            );
        }
    }

    const RTL_SAMPLE: &str = "resources/G3RTLKerning.glyphs";
    const RTL_MASTER: &str = "3E7589AA-8194-470F-8E2F-13C1C581BE24";

    fn rtl_pair(master: &crate::Master, first: &str, second: &str) -> Option<i16> {
        master
            .kerning_rtl
            .get(&(first.into(), second.into()))
            .copied()
    }

    #[test]
    fn rtl_kerning_moves_onto_the_masters_on_import() {
        let font = crate::load(RTL_SAMPLE).unwrap();
        assert!(
            !font
                .format_specific
                .contains_key(crate::Font::KEY_KERNING_RTL),
            "kerningRTL is left in format_specific"
        );
        let master = font.masters.iter().find(|m| m.id == RTL_MASTER).unwrap();
        assert_eq!(rtl_pair(master, "@reh", "@alef"), Some(-90));
        assert_eq!(rtl_pair(master, "@reh", "@reh"), Some(-40));
        let other = font.masters.iter().find(|m| m.id != RTL_MASTER).unwrap();
        assert_eq!(rtl_pair(other, "@reh", "@alef"), Some(-60));
        // Right-to-left pairs stay out of the left-to-right kerning.
        assert!(master
            .kerning
            .keys()
            .all(|(first, _)| first.as_str() != "@reh"));
    }

    #[test]
    fn rtl_kerning_is_written_back_on_export() {
        let source =
            glyphslib::Font::load_str(&std::fs::read_to_string(RTL_SAMPLE).unwrap()).unwrap();
        let source = source.as_glyphs3().unwrap();
        let font = crate::load(RTL_SAMPLE).unwrap();
        let saved = super::as_glyphs3(&font).unwrap();
        // Every master's, also one the sample's odd `.notdef` makes look
        // sparse (and whose left-to-right kerning is not saved for that).
        assert_eq!(saved.kerning_rtl, source.kerning_rtl);
    }

    #[test]
    fn babelfont_json_keeps_rtl_kerning_flat_per_master() {
        let font = crate::load(RTL_SAMPLE).unwrap();
        let json = serde_json::to_value(&font).unwrap();
        let master = json["masters"]
            .as_array()
            .unwrap()
            .iter()
            .find(|m| m["id"] == RTL_MASTER)
            .unwrap();
        assert_eq!(master["kerning_rtl"]["@reh:@alef"], -90);
        assert!(json["format_specific"]
            .get(crate::Font::KEY_KERNING_RTL)
            .is_none());
        let back: crate::Font = serde_json::from_value(json).unwrap();
        assert_eq!(back.masters, font.masters);
    }

    #[test]
    fn rtl_kerning_saved_the_old_way_is_moved_and_unknown_masters_kept() {
        let mut font = crate::Font::new();
        font.masters
            .push(crate::Master::new("Regular", "m1", Default::default()));
        font.masters[0]
            .kerning_rtl
            .insert(("@reh".into(), "@alef".into()), -10);
        font.format_specific.insert(
            crate::Font::KEY_KERNING_RTL.to_string(),
            serde_json::json!({
                "m1": {
                    "@MMK_R_reh": { "@MMK_L_alef": -90, "@MMK_L_reh": -40 },
                    "beh": { "alef": -5 }
                },
                "gone": { "@MMK_R_reh": { "@MMK_L_alef": -70 } }
            }),
        );

        assert!(font.move_rtl_kerning_to_masters());
        let master = &font.masters[0];
        // A pair the master already has keeps its value.
        assert_eq!(rtl_pair(master, "@reh", "@alef"), Some(-10));
        assert_eq!(rtl_pair(master, "@reh", "@reh"), Some(-40));
        assert_eq!(rtl_pair(master, "beh", "alef"), Some(-5));
        // Kerning for a master the font does not have is kept as it was.
        assert_eq!(
            font.format_specific.get(crate::Font::KEY_KERNING_RTL),
            Some(&serde_json::json!({
                "gone": { "@MMK_R_reh": { "@MMK_L_alef": -70.0 } }
            }))
        );
        let written = font.glyphs_rtl_kerning(|_| false);
        assert_eq!(written["gone"]["@MMK_R_reh"]["@MMK_L_alef"], -70.0);
        assert_eq!(written["m1"]["@MMK_R_reh"]["@MMK_L_reh"], -40.0);
        assert_eq!(written["m1"]["beh"]["alef"], -5.0);
        // Moving again changes nothing.
        assert!(!font.move_rtl_kerning_to_masters());
    }

    #[test]
    fn a_stale_kerning_rtl_copy_for_a_known_master_is_not_exported() {
        // Fonts the editor saved before masters had `kerning_rtl` keep both
        // copies; a pair deleted from the master since must stay deleted.
        let mut font = crate::Font::new();
        font.masters
            .push(crate::Master::new("Regular", "m1", Default::default()));
        font.masters[0]
            .kerning_rtl
            .insert(("@reh".into(), "@alef".into()), -90);
        font.format_specific.insert(
            crate::Font::KEY_KERNING_RTL.to_string(),
            serde_json::json!({
                "m1": { "@MMK_R_reh": { "@MMK_L_alef": -90, "@MMK_L_reh": -40 } }
            }),
        );
        let written = font.glyphs_rtl_kerning(|_| false);
        assert_eq!(written["m1"]["@MMK_R_reh"].len(), 1);
        assert_eq!(written["m1"]["@MMK_R_reh"]["@MMK_L_alef"], -90.0);
        assert_eq!(
            font.merged_kerning_for_master(&font.masters[0]),
            vec![(("@reh".into(), "@alef".into()), -90)]
        );
    }

    #[test]
    fn renaming_a_glyph_renames_its_rtl_pairs() {
        use crate::filters::FontFilter;
        let mut font = crate::Font::new();
        font.masters
            .push(crate::Master::new("Regular", "m1", Default::default()));
        font.masters[0]
            .kerning_rtl
            .insert(("beh-ar".into(), "alef-ar".into()), -20);
        let renames = std::collections::HashMap::from([("beh-ar".to_string(), "beh".to_string())]);
        crate::filters::RenameGlyphs::new(renames)
            .apply(&mut font)
            .unwrap();
        assert_eq!(rtl_pair(&font.masters[0], "beh", "alef-ar"), Some(-20));
        assert_eq!(rtl_pair(&font.masters[0], "beh-ar", "alef-ar"), None);
    }
}

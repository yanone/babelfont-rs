use crate::{
    convertors::ufo::{
        self, glyph_to_layer, load_kerning, load_master_info, stash_font_info, stash_lib,
        UfoEntries, UfoLayout, UfoSave, UfoSaveRequest, KEY_FEATURE_SOURCES, KEY_LIB,
    },
    names::Names,
    Axis, BabelfontError, Font, I18NDictionary, Instance, Layer, LayerType, Master, Tag,
};
use fontdrasil::coords::{DesignCoord, DesignLocation};
use norad::designspace::{
    DesignSpaceDocument, Dimension, Instance as DSInstance, Source, VariableFont,
};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Component, Path as FsPath, PathBuf};

/// Key to store the designspace format version in the font's format_specific
pub const FORMAT_KEY: &str = "norad.designspace.format";
/// Key to store the master filename in the instance's format_specific
pub const FILENAME_KEY: &str = "norad.designspace.filename";
/// Key to store the master style name in the master's format_specific
pub const STYLENAME_KEY: &str = "norad.designspace.style";
/// Key for the designspace document as read. Saving starts from it, so that
/// what babelfont does not model (rules, axis mappings, the document lib,
/// STAT labels, localised names, user locations) survives.
pub const DOCUMENT_KEY: &str = "norad.designspace.document";
/// Key for the UFO layer a sparse master lives in (a source's `layer`).
pub const LAYER_KEY: &str = "norad.designspace.layer";

const BACKGROUND_LAYER: &str = "public.background";

/// Load a DesignSpace document and all referenced UFOs into a Babelfont Font
pub fn load(path: PathBuf) -> Result<Font, BabelfontError> {
    let text = std::fs::read_to_string(&path)?;
    let base = path
        .parent()
        .unwrap_or_else(|| FsPath::new(""))
        .to_path_buf();
    load_document(&text, &|filename| {
        Ok(norad::Font::load(base.join(filename))?)
    })
}

/// Load a DesignSpace document and referenced UFOs from in-memory entries.
///
/// `path` is the virtual designspace path (for example `My.designspace` or `sources/My.designspace`).
pub fn load_entries(
    path: PathBuf,
    entries: &HashMap<String, String>,
) -> Result<Font, BabelfontError> {
    let normalized_entries: HashMap<String, String> = entries
        .iter()
        .map(|(k, v)| {
            (
                normalize_virtual_path(k).unwrap_or_else(|| k.clone()),
                v.clone(),
            )
        })
        .collect();

    let ds_path = normalize_virtual_path(&path.to_string_lossy()).ok_or_else(|| {
        BabelfontError::General(format!("Invalid designspace path: {}", path.display()))
    })?;
    let ds_contents = normalized_entries
        .get(&ds_path)
        .or_else(|| {
            FsPath::new(&ds_path)
                .file_name()
                .and_then(|name| normalized_entries.get(&name.to_string_lossy().to_string()))
        })
        .ok_or_else(|| {
            BabelfontError::General(format!(
                "Designspace file not found in entries: {}",
                ds_path
            ))
        })?;
    let ds_base = FsPath::new(&ds_path)
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_default();
    load_document(ds_contents, &|filename| {
        let ufo_path = resolve_virtual_reference(&ds_base, filename)?;
        ufo::load_norad_from_entries(FsPath::new(&ufo_path), &normalized_entries)
    })
}

fn parse_document(text: &str) -> Result<DesignSpaceDocument, BabelfontError> {
    Ok(DesignSpaceDocument::load_from_reader(text.as_bytes())?)
}

/// The UFOs a designspace document's sources live in, each once, in order.
pub fn source_files(text: &str) -> Result<Vec<String>, BabelfontError> {
    let ds = parse_document(text)?;
    let mut seen = HashSet::new();
    Ok(ds
        .sources
        .iter()
        .filter(|s| seen.insert(s.filename.clone()))
        .map(|s| s.filename.clone())
        .collect())
}

/// One id per source, the same on every load: its name, else its file and
/// layer.
fn master_ids(sources: &[Source]) -> Vec<String> {
    let mut seen = HashSet::new();
    sources
        .iter()
        .map(|source| {
            let base = source.name.clone().unwrap_or_else(|| match &source.layer {
                Some(layer) => format!("{}/{layer}", source.filename),
                None => source.filename.clone(),
            });
            let mut id = base.clone();
            let mut counter = 1;
            while !seen.insert(id.clone()) {
                counter += 1;
                id = format!("{base} {counter}");
            }
            id
        })
        .collect()
}

/// For each source, the UFO layers other sources use, which its own layers
/// must not include.
fn layers_used_by_sources(sources: &[Source]) -> HashMap<&str, HashSet<&str>> {
    let mut used: HashMap<&str, HashSet<&str>> = HashMap::new();
    for source in sources {
        if let Some(layer) = &source.layer {
            used.entry(source.filename.as_str())
                .or_default()
                .insert(layer.as_str());
        }
    }
    used
}

/// What building a glyph's layers needs from a designspace's UFOs.
trait SourceGlyphs {
    /// The names of a UFO's layers other than its default layer, in order.
    fn layer_names(&self, ufo: &str) -> Vec<String>;
    /// A glyph from a UFO's named layer, or its default layer for `None`.
    fn glyph(&self, ufo: &str, layer: Option<&str>, name: &str) -> Option<norad::Glyph>;
}

struct LoadedUfos<'a>(&'a HashMap<String, norad::Font>);

impl SourceGlyphs for LoadedUfos<'_> {
    fn layer_names(&self, ufo: &str) -> Vec<String> {
        self.0
            .get(ufo)
            .map(|font| {
                font.iter_layers()
                    .filter(|l| !l.is_default())
                    .map(|l| l.name().to_string())
                    .collect()
            })
            .unwrap_or_default()
    }

    fn glyph(&self, ufo: &str, layer: Option<&str>, name: &str) -> Option<norad::Glyph> {
        let font = self.0.get(ufo)?;
        let layer = match layer {
            Some(layer) => font.layers.get(layer)?,
            None => font.default_layer(),
        };
        layer.get_glyph(name).cloned()
    }
}

/// A glyph's layers across all masters: each master's outline, the
/// background of a master with its own UFO, and that UFO's other layers.
fn glyph_layers(
    name: &str,
    sources: &[Source],
    masters: &[Master],
    ufos: &dyn SourceGlyphs,
) -> Vec<Layer> {
    let used_layers = layers_used_by_sources(sources);
    let mut layers = vec![];
    for (source, master) in sources.iter().zip(masters) {
        let ufo = source.filename.as_str();
        let Some(glyph) = ufos.glyph(ufo, source.layer.as_deref(), name) else {
            continue;
        };
        // A source's layer is the default layer of its master.
        let mut layer = glyph_to_layer(&glyph, None, master);
        if source.layer.is_some() {
            layers.push(layer);
            continue;
        }
        let background = ufos.glyph(ufo, Some(BACKGROUND_LAYER), name).map(|bg| {
            let mut background = glyph_to_layer(&bg, None, master);
            background.master = LayerType::FreeFloating;
            background.id = Some(format!("{}.{BACKGROUND_LAYER}", master.id));
            background.is_background = true;
            background
        });
        layer.background_layer_id = background.as_ref().and_then(|b| b.id.clone());
        layers.push(layer);
        layers.extend(background);
        let others = used_layers.get(ufo);
        for layer_name in ufos.layer_names(ufo) {
            if layer_name == BACKGROUND_LAYER
                || others.is_some_and(|used| used.contains(layer_name.as_str()))
            {
                continue;
            }
            if let Some(glyph) = ufos.glyph(ufo, Some(&layer_name), name) {
                layers.push(glyph_to_layer(&glyph, Some(&layer_name), master));
            }
        }
    }
    layers
}

fn load_document(
    text: &str,
    read_ufo: &dyn Fn(&str) -> Result<norad::Font, BabelfontError>,
) -> Result<Font, BabelfontError> {
    let ds = parse_document(text)?;
    let axes: Vec<Axis> = ds
        .axes
        .iter()
        .filter_map(|dsax| dsax.try_into().ok())
        .collect();
    #[allow(clippy::unwrap_used)] // We put a default there
    let axis_name_tag_map: HashMap<String, Tag> = axes
        .iter()
        .map(|ax| (ax.name.get_default().unwrap().clone(), ax.tag))
        .collect();
    let default_source = default_master(&ds, &axes).ok_or(BabelfontError::NoDefaultMaster)?;

    let mut ufos = HashMap::new();
    for source in &ds.sources {
        if !ufos.contains_key(&source.filename) {
            ufos.insert(source.filename.clone(), read_ufo(&source.filename)?);
        }
    }
    let default_ufo = ufos
        .get(&default_source.filename)
        .ok_or(BabelfontError::NoDefaultMaster)?;
    let mut font = ufo::font_from_norad(
        FsPath::new(&default_source.filename),
        None,
        default_ufo.clone(),
    )?;
    font.axes = axes;
    load_instances(&mut font, &axis_name_tag_map, &ds.instances)?;
    load_variable_fonts(&mut font, &ds.variable_fonts);

    let mut masters = vec![];
    for (source, id) in ds.sources.iter().zip(master_ids(&ds.sources)) {
        let ufo = &ufos[&source.filename];
        let mut master = Master::new(
            source.name.as_ref().unwrap_or(&source.filename),
            id,
            source_location(&source.location, &axis_name_tag_map)?,
        );
        if let Some(stylename) = &source.stylename {
            master
                .format_specific
                .insert(STYLENAME_KEY.into(), stylename.clone().into());
        }
        master
            .format_specific
            .insert(FILENAME_KEY.into(), source.filename.clone().into());
        load_master_info(&mut master, &ufo.font_info);
        ufo::load_number_values(
            &ufo.lib,
            (source.filename == default_source.filename).then_some(&mut font.format_specific),
            &mut master,
        );
        load_kerning(&mut master, &ufo.kerning);
        match &source.layer {
            Some(layer) => {
                master
                    .format_specific
                    .insert(LAYER_KEY.into(), layer.clone().into());
            }
            None => {
                stash_font_info(&mut master, &ufo.font_info);
                if source.filename != default_source.filename {
                    ufo::stash_style_names(&mut master.format_specific, &ufo.font_info);
                    // Its own UFO's lib and features, written back while the
                    // font's are unchanged.
                    // Even an empty lib, so that it stays empty.
                    master.format_specific.insert(
                        KEY_LIB.into(),
                        serde_json::to_value(&ufo.lib).unwrap_or_default(),
                    );
                    if let Some(sources) = ufo::feature_sources_for(ufo, &font.features) {
                        master
                            .format_specific
                            .insert(KEY_FEATURE_SOURCES.into(), sources);
                    }
                }
            }
        }
        masters.push(master);
    }

    let loaded = LoadedUfos(&ufos);
    for glyph in font.glyphs.iter_mut() {
        glyph.layers = glyph_layers(&glyph.name, &ds.sources, &masters, &loaded);
    }
    font.masters = masters;
    font.format_specific.insert(
        FORMAT_KEY.to_string(),
        serde_json::Number::from_f64(ds.format as f64)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
    );
    font.format_specific
        .insert(DOCUMENT_KEY.to_string(), text.to_string().into());
    Ok(font)
}

fn source_location(
    location: &[Dimension],
    axis_name_tag_map: &HashMap<String, Tag>,
) -> Result<DesignLocation, BabelfontError> {
    Ok(DesignLocation::from(
        location
            .iter()
            .map(|dimension| {
                let tag = axis_name_tag_map.get(&dimension.name).ok_or_else(|| {
                    BabelfontError::General(format!("Axis name not found: {}", dimension.name))
                })?;
                Ok((*tag, DesignCoord::new(dimension.xvalue.unwrap_or_default())))
            })
            .collect::<Result<Vec<_>, BabelfontError>>()?,
    ))
}

/// A designspace's variable fonts are what a Glyphs font calls variable
/// instances: an export setting, not a location.
fn load_variable_fonts(font: &mut Font, variable_fonts: &[VariableFont]) {
    for variable_font in variable_fonts {
        let mut instance = Instance {
            id: variable_font.name.clone(),
            name: (&variable_font.name).into(),
            location: Default::default(),
            custom_names: Names::new(),
            variable: true,
            format_specific: stash_lib(Some(&variable_font.lib)),
            linked_style: None,
        };
        if let Some(filename) = &variable_font.filename {
            instance
                .format_specific
                .insert(FILENAME_KEY.to_string(), filename.clone().into());
        }
        font.instances.push(instance);
    }
}

pub(crate) fn load_instances(
    font: &mut Font,
    axis_name_tag_map: &HashMap<String, Tag>,
    instances: &[DSInstance],
) -> Result<(), BabelfontError> {
    for instance in instances {
        let mut custom_names = Names::new();
        if let Some(familyname) = &instance.familyname {
            custom_names.family_name = familyname.into();
        }
        if let Some(stylename) = &instance.stylename {
            custom_names.preferred_subfamily_name = stylename.into(); // ?
        }
        if let Some(psname) = &instance.postscriptfontname {
            custom_names.postscript_name = psname.into();
        }
        if let Some(stylemapfamilyname) = &instance.stylemapfamilyname {
            custom_names.typographic_family = stylemapfamilyname.into(); // ???
        }
        if let Some(stylemapstylename) = &instance.stylemapstylename {
            custom_names.typographic_subfamily = stylemapstylename.into(); // ???
        }
        let mut inst = Instance {
            id: instance
                .name
                .as_ref()
                .unwrap_or(&"Unnamed instance".to_string())
                .clone(),
            name: instance
                .name
                .as_ref()
                .unwrap_or(&"Unnamed instance".to_string())
                .into(),
            location: source_location(&instance.location, axis_name_tag_map)?,
            custom_names,
            variable: false,
            format_specific: stash_lib(Some(&instance.lib)),
            linked_style: None,
        };
        if let Some(filename) = &instance.filename {
            inst.format_specific.insert(
                FILENAME_KEY.to_string(),
                serde_json::Value::String(filename.into()),
            );
        }
        font.instances.push(inst);
    }
    Ok(())
}

fn resolve_virtual_reference(base_dir: &str, relative: &str) -> Result<String, BabelfontError> {
    let combined = if base_dir.is_empty() {
        relative.to_string()
    } else {
        format!("{}/{}", base_dir.trim_end_matches('/'), relative)
    };
    normalize_virtual_path(&combined).ok_or_else(|| {
        BabelfontError::General(format!(
            "Designspace source path escapes project root or is invalid: {}",
            relative
        ))
    })
}

fn normalize_virtual_path(path: &str) -> Option<String> {
    let mut parts: Vec<String> = Vec::new();
    for component in FsPath::new(path).components() {
        match component {
            Component::Normal(p) => parts.push(p.to_string_lossy().to_string()),
            Component::CurDir | Component::RootDir => {}
            Component::ParentDir => {
                if parts.pop().is_none() {
                    return None;
                }
            }
            Component::Prefix(_) => return None,
        }
    }
    Some(parts.join("/"))
}

fn default_master<'a>(ds: &'a DesignSpaceDocument, axes: &[Axis]) -> Option<&'a Source> {
    #[allow(clippy::unwrap_used)]
    // We know the axes are well defined because we constructed them from the DS
    let defaults: BTreeMap<&String, DesignCoord> = axes
        .iter()
        .map(|ax| {
            (
                ax.name.get_default().unwrap(),
                ax.default
                    .map(|x| ax.userspace_to_designspace(x).unwrap())
                    .unwrap_or_default(),
            )
        })
        .collect();
    ds.sources.iter().find(|source| {
        source.layer.is_none()
            && source
                .location
                .iter()
                .all(|loc| defaults.get(&loc.name) == loc.xvalue.map(DesignCoord::new).as_ref())
    })
}

/// Loads glyphs from their `.glif` files alone, to pick up a change to a few
/// files without reading the whole designspace.
///
/// `layouts` gives each UFO's file names, by the UFO's path relative to the
/// document; `glifs` holds the `.glif` files by path relative to the
/// document (`Font-Bold.ufo/glyphs/A_.glif`). Only layers and codepoints are
/// meaningful in the result.
pub fn load_glyphs_from_glifs(
    document: &str,
    layouts: &HashMap<String, UfoLayout>,
    glifs: &HashMap<String, String>,
    glyph_names: &[String],
) -> Result<Vec<crate::Glyph>, BabelfontError> {
    struct Glifs<'a> {
        layouts: &'a HashMap<String, UfoLayout>,
        glifs: &'a HashMap<String, String>,
    }
    impl SourceGlyphs for Glifs<'_> {
        fn layer_names(&self, ufo: &str) -> Vec<String> {
            self.layouts
                .get(ufo)
                .map(|layout| {
                    layout
                        .layers()
                        .filter(|(_, dir)| *dir != "glyphs")
                        .map(|(name, _)| name.to_string())
                        .collect()
                })
                .unwrap_or_default()
        }

        fn glyph(&self, ufo: &str, layer: Option<&str>, name: &str) -> Option<norad::Glyph> {
            let layout = self.layouts.get(ufo)?;
            let dir = match layer {
                None => "glyphs",
                Some(layer) => layout.layers().find(|(n, _)| *n == layer)?.1,
            };
            let text = self
                .glifs
                .get(&format!("{ufo}/{dir}/{}", layout.file_name(dir, name)?))?;
            norad::Glyph::parse_raw(text.as_bytes()).ok()
        }
    }

    let ds = parse_document(document)?;
    let masters: Vec<Master> = master_ids(&ds.sources)
        .into_iter()
        .map(|id| Master::new(&id, &id, DesignLocation::new()))
        .collect();
    let default_ufo = ds
        .sources
        .iter()
        .find(|s| s.layer.is_none())
        .map(|s| s.filename.clone())
        .unwrap_or_default();
    let source = Glifs { layouts, glifs };
    Ok(glyph_names
        .iter()
        .map(|name| {
            let mut glyph = crate::Glyph::new(name);
            glyph.layers = glyph_layers(name, &ds.sources, &masters, &source);
            if let Some(default) = source.glyph(&default_ufo, None, name) {
                glyph.codepoints = default.codepoints.iter().map(|c| c as u32).collect();
            }
            glyph
        })
        .collect())
}

/// The UFO layouts of a designspace project, from its UFOs'
/// `layercontents.plist` and `contents.plist` files keyed by path relative to
/// the document (`Font-Bold.ufo/glyphs/contents.plist`).
pub fn layouts_from_entries(
    document: &str,
    entries: &HashMap<String, String>,
) -> Result<HashMap<String, UfoLayout>, BabelfontError> {
    layouts_by_ufo(&source_files(document)?, entries)
}

/// [`layouts_from_entries`] for the UFOs a font's masters live in.
pub fn layouts_for_font(
    font: &Font,
    entries: &HashMap<String, String>,
) -> Result<HashMap<String, UfoLayout>, BabelfontError> {
    let mut seen = HashSet::new();
    let ufo_files: Vec<String> = master_files(font)
        .into_iter()
        .map(|(f, _)| f)
        .filter(|f| seen.insert(f.clone()))
        .collect();
    layouts_by_ufo(&ufo_files, entries)
}

fn layouts_by_ufo(
    ufo_files: &[String],
    entries: &HashMap<String, String>,
) -> Result<HashMap<String, UfoLayout>, BabelfontError> {
    let mut layouts = HashMap::new();
    for ufo_file in ufo_files {
        let prefix = format!("{ufo_file}/");
        let ufo_entries: HashMap<String, String> = entries
            .iter()
            .filter_map(|(path, text)| {
                Some((path.strip_prefix(&prefix)?.to_string(), text.clone()))
            })
            .collect();
        layouts.insert(ufo_file.clone(), UfoLayout::from_entries(&ufo_entries)?);
    }
    Ok(layouts)
}

/// Each glyph's `.glif` paths, and the paths that are no glyph's file.
pub type GlyphFiles = (BTreeMap<String, Vec<String>>, Vec<String>);

/// The `.glif` files of the glyphs that changed files belong to, across all
/// UFOs and layers, by path relative to the document; and the changed paths
/// that are no known glyph's file.
pub fn glyph_files_for_paths(
    document: &str,
    layouts: &HashMap<String, UfoLayout>,
    paths: &[String],
) -> Result<GlyphFiles, BabelfontError> {
    let ufo_files = source_files(document)?;
    let mut glyphs = BTreeMap::new();
    let mut unknown = vec![];
    for path in paths {
        let found = ufo_files.iter().find_map(|ufo_file| {
            let inner = path.strip_prefix(&format!("{ufo_file}/"))?;
            layouts.get(ufo_file)?.glyph_at(inner)
        });
        match found {
            Some(name) => {
                glyphs.entry(name.to_string()).or_insert_with(|| {
                    ufo_files
                        .iter()
                        .flat_map(|ufo_file| {
                            layouts
                                .get(ufo_file)
                                .map(|layout| layout.glyph_paths(name))
                                .unwrap_or_default()
                                .into_iter()
                                .map(move |p| format!("{ufo_file}/{p}"))
                        })
                        .collect()
                });
            }
            None => unknown.push(path.clone()),
        }
    }
    Ok((glyphs, unknown))
}

/// What [`save_entries`] writes.
#[derive(Debug, Clone, Copy)]
pub struct DesignspaceSaveRequest<'a> {
    /// The document's file name, relative to the project folder.
    pub document: &'a str,
    /// The layouts of the UFOs being saved over, by their path relative to
    /// the document; their names are kept.
    pub layouts: &'a HashMap<String, UfoLayout>,
    /// Write only the `.glif` files of these glyphs, plus the document and
    /// the UFOs' font-level files when `font_level` is set. `None` writes
    /// the whole project.
    pub glyphs: Option<&'a [String]>,
    /// In a partial save, also write the document and font-level files.
    pub font_level: bool,
}

/// The UFO file name of each master, and the layer of a sparse one.
fn master_files(font: &Font) -> Vec<(String, Option<String>)> {
    font.masters
        .iter()
        .map(|m| {
            let filename = m
                .format_specific
                .get(FILENAME_KEY)
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
                .unwrap_or(format!(
                    "{}-{}.ufo",
                    font.names
                        .family_name
                        .get_default()
                        .unwrap_or(&"Unnamed font".to_string())
                        .replace(" ", ""),
                    m.name
                        .get_default()
                        .map(|x| x.as_str())
                        .unwrap_or(m.id.as_str())
                        .replace(" ", "")
                ));
            let layer = m
                .format_specific
                .get(LAYER_KEY)
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            (filename, layer)
        })
        .collect()
}

/// Save a designspace project into file entries keyed by path relative to the
/// document's folder: the document and every master's UFO. A master with a
/// source `layer` goes into that layer of its UFO. See [`ufo::save_entries`]
/// for what a partial save writes in each UFO.
pub fn save_entries(
    font: &Font,
    request: DesignspaceSaveRequest,
) -> Result<UfoSave, BabelfontError> {
    let files = master_files(font);
    let mut entries = UfoEntries::default();
    let mut ufo_files: Vec<&str> = vec![];
    for (filename, _) in &files {
        if !ufo_files.contains(&filename.as_str()) {
            ufo_files.push(filename);
        }
    }
    let no_layout = UfoLayout::default();
    for ufo_file in ufo_files {
        let main = files
            .iter()
            .position(|(f, layer)| f == ufo_file && layer.is_none())
            .ok_or_else(|| {
                BabelfontError::General(format!("No master has {ufo_file} as its own UFO"))
            })?;
        let sparse: Vec<(usize, String)> = files
            .iter()
            .enumerate()
            .filter(|(_, (f, _))| f == ufo_file)
            .filter_map(|(ix, (_, layer))| Some((ix, layer.clone()?)))
            .collect();
        let ufo_request = UfoSaveRequest {
            layout: Some(request.layouts.get(ufo_file).unwrap_or(&no_layout)),
            glyphs: request.glyphs,
            font_level: request.font_level,
        };
        match ufo::save_master_entries(font, main, &sparse, ufo_request)? {
            UfoSave::Entries(ufo_entries) => {
                for (path, text) in ufo_entries.files {
                    entries.files.insert(format!("{ufo_file}/{path}"), text);
                }
                for path in ufo_entries.removed {
                    entries.removed.push(format!("{ufo_file}/{path}"));
                }
            }
            needs_full @ UfoSave::NeedsFullSave(_) => return Ok(needs_full),
        }
    }
    if request.glyphs.is_none() || request.font_level {
        let document = build_document(font, &files)?;
        let mut bytes = vec![];
        document
            .save_to_writer(&mut bytes)
            .map_err(|e| BabelfontError::General(format!("Failed to write designspace: {e}")))?;
        entries.files.insert(
            request.document.to_string(),
            String::from_utf8(bytes)
                .map_err(|_| BabelfontError::General("designspace is not UTF-8".into()))?,
        );
    }
    Ok(UfoSave::Entries(entries))
}

/// Save a Babelfont Font as a DesignSpace document and referenced UFOs
pub fn save_designspace(font: &Font, path: &PathBuf) -> Result<(), BabelfontError> {
    let base = path.parent().unwrap_or_else(|| FsPath::new("."));
    let document = path
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "font.designspace".to_string());
    let layouts = HashMap::new();
    let UfoSave::Entries(entries) = save_entries(
        font,
        DesignspaceSaveRequest {
            document: &document,
            layouts: &layouts,
            glyphs: None,
            font_level: true,
        },
    )?
    else {
        return Err(BabelfontError::General(
            "A whole save is always possible".into(),
        ));
    };
    // Replace each UFO, as saving a UFO does.
    for (ufo_file, _) in master_files(font) {
        let ufo_path = base.join(&ufo_file);
        if ufo_path.exists() {
            std::fs::remove_dir_all(&ufo_path)?;
        }
    }
    for (relative, text) in entries.files {
        let file = base.join(relative);
        if let Some(parent) = file.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(file, text)?;
    }
    Ok(())
}

/// The designspace document for the font, starting from the one it was read
/// from, so that what babelfont does not model is kept.
fn build_document(
    font: &Font,
    files: &[(String, Option<String>)],
) -> Result<DesignSpaceDocument, BabelfontError> {
    let original = font
        .format_specific
        .get(DOCUMENT_KEY)
        .and_then(|v| v.as_str())
        .and_then(|text| parse_document(text).ok());
    let original_sources: HashMap<String, Source> = original
        .as_ref()
        .map(|ds| {
            master_ids(&ds.sources)
                .into_iter()
                .zip(ds.sources.iter().cloned())
                .collect()
        })
        .unwrap_or_default();
    let original_instances: HashMap<String, DSInstance> = original
        .as_ref()
        .map(|ds| {
            ds.instances
                .iter()
                .filter_map(|i| Some((i.name.clone()?, i.clone())))
                .collect()
        })
        .unwrap_or_default();

    let axis_tag_name_map: HashMap<Tag, String> = font
        .axes
        .iter()
        .map(|ax| {
            (
                ax.tag,
                ax.name
                    .get_default()
                    .unwrap_or(&"Unnamed axis".to_string())
                    .to_string(),
            )
        })
        .collect();
    let axis_order = font
        .axes
        .iter()
        .flat_map(|ax| ax.name.get_default())
        .map(|x| x.as_str())
        .collect::<Vec<_>>();
    let location = |location: &DesignLocation, original: &[Dimension]| {
        let mut dimensions: Vec<Dimension> = location
            .iter()
            .map(|(tag, coord)| Dimension {
                name: axis_tag_name_map
                    .get(tag)
                    .cloned()
                    .unwrap_or_else(|| tag.to_string()),
                xvalue: Some(coord.to_f64()),
                uservalue: None,
                yvalue: None,
            })
            .collect();
        dimensions.sort_by_key(|d| {
            axis_order
                .iter()
                .position(|x| x == &d.name)
                .unwrap_or(usize::MAX)
        });
        // Unchanged, the location is written as it was, user values and all.
        let unchanged = dimensions.len() == original.len()
            && dimensions.iter().all(|d| {
                original
                    .iter()
                    .any(|o| o.name == d.name && o.xvalue == d.xvalue)
            });
        if unchanged {
            original.to_vec()
        } else {
            dimensions
        }
    };

    let mut ds = original.clone().unwrap_or_default();
    ds.format = font
        .format_specific
        .get(FORMAT_KEY)
        .and_then(|v| v.as_f64())
        .unwrap_or(5.0) as f32;
    ds.axes = font.axes.iter().map(|x| x.into()).collect();
    ds.sources = font
        .masters
        .iter()
        .zip(files)
        .map(|(master, (filename, layer))| {
            let mut source = original_sources
                .get(&master.id)
                .cloned()
                .unwrap_or_default();
            if source.familyname.is_some() || original.is_none() {
                source.familyname = font.names.family_name.get_default().map(|x| x.to_string());
            }
            source.stylename = master
                .format_specific
                .get(STYLENAME_KEY)
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            source.name = master.name.get_default().map(|x| x.to_string());
            source.filename = filename.clone();
            source.layer = layer.clone();
            source.location = location(&master.location, &source.location);
            source
        })
        .collect();
    let original_variable_fonts: HashMap<String, VariableFont> = original
        .as_ref()
        .map(|ds| {
            ds.variable_fonts
                .iter()
                .map(|v| (v.name.clone(), v.clone()))
                .collect()
        })
        .unwrap_or_default();
    ds.variable_fonts = font
        .instances
        .iter()
        .filter(|instance| instance.variable)
        .map(|instance| {
            let name = instance
                .name
                .get_default()
                .cloned()
                .unwrap_or_else(|| instance.id.clone());
            // A new one covers every axis.
            let mut out = original_variable_fonts
                .get(&name)
                .cloned()
                .unwrap_or_default();
            out.name = name;
            out.filename = instance
                .format_specific
                .get(FILENAME_KEY)
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            out.lib = serde_json::from_value(
                instance
                    .format_specific
                    .get(KEY_LIB)
                    .cloned()
                    .unwrap_or_default(),
            )
            .unwrap_or_default();
            out
        })
        .collect();
    ds.instances = font
        .instances
        .iter()
        .filter(|instance| !instance.variable)
        .map(|instance| {
            let name = |x: &I18NDictionary| x.get_default().map(|y| y.to_string());
            let mut out = instance
                .name
                .get_default()
                .and_then(|n| original_instances.get(n))
                .cloned()
                .unwrap_or_default();
            out.familyname = name(&instance.custom_names.family_name);
            out.stylename = name(&instance.custom_names.preferred_subfamily_name);
            out.name = name(&instance.name);
            out.filename = instance
                .format_specific
                .get(FILENAME_KEY)
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            out.postscriptfontname = name(&instance.custom_names.postscript_name);
            out.stylemapfamilyname = name(&instance.custom_names.typographic_family);
            out.stylemapstylename = name(&instance.custom_names.typographic_subfamily);
            out.location = location(&instance.location, &out.location);
            out.lib = serde_json::from_value(
                instance
                    .format_specific
                    .get(KEY_LIB)
                    .cloned()
                    .unwrap_or_default(),
            )
            .unwrap_or_default();
            out
        })
        .collect();
    Ok(ds)
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]

    use std::path::PathBuf;

    use crate::filters::{FontFilter, RetainGlyphs};

    use super::*;
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    fn test_roundtrip(#[files("resources/*.designspace")] path: PathBuf) {
        let there = crate::load(&path).unwrap();
        let tempdir = tempfile::tempdir().unwrap();
        let temp_path = tempdir.path().join("temp.designspace");
        save_designspace(&there, &temp_path).unwrap();
        println!("Saved to temp path: {:?}", temp_path);

        // Perform a norad load on both designspace files and check for equivalence
        let relative = path.parent();
        let ds1 = norad::designspace::DesignSpaceDocument::load(&path).unwrap();
        let ds2 = norad::designspace::DesignSpaceDocument::load(&temp_path).unwrap();
        assert_eq!(ds1, ds2);
        let index_of_default = there.default_master_index().unwrap();

        // Now check each UFO in turn.
        for (ix, source) in ds1.sources.iter().enumerate() {
            use crate::convertors::ufo::tests::ufo_semantic_test;

            let relative_path_to_master = if let Some(r) = relative {
                r.join(source.filename.clone())
            } else {
                source.filename.clone().into()
            };
            let font1 = norad::Font::load(relative_path_to_master).unwrap();

            let source2 = ds2
                .sources
                .iter()
                .find(|s| s.filename == source.filename)
                .unwrap();
            let relative_path_to_master2 = tempdir.path().join(source2.filename.clone());
            println!(
                "Testing master: {}, loading from {:?}",
                source.filename,
                relative_path_to_master2.display()
            );
            let font2 = norad::Font::load(relative_path_to_master2).unwrap();
            ufo_semantic_test(&font1, &font2, ix == index_of_default);
        }
    }

    #[test]
    fn test_background() {
        let mut font = crate::load("resources/IbarraRealNova.designspace").unwrap();
        let glyph = font.glyphs.iter().find(|g| g.name == "A").unwrap();
        // Each layer in the glyph should have a background layer, and they should be linked
        assert_eq!(glyph.layers.len(), 2 * font.masters.len());
        // Slim the font just to the A glyph for simplicity
        RetainGlyphs::new(vec!["A".to_string()])
            .apply(&mut font)
            .unwrap();
        // Convert master 1 to UFO
        let ufo = ufo::as_norad(&font, 0).unwrap();
        // Check it has two layers
        assert_eq!(ufo.layers.len(), 2);
    }

    const DOCUMENT: &str = "IbarraRealNova.designspace";

    /// The designspace document and its UFOs' files, by path relative to the
    /// document.
    fn project_entries() -> HashMap<String, String> {
        let base = FsPath::new("resources");
        let text = std::fs::read_to_string(base.join(DOCUMENT)).unwrap();
        let mut entries = HashMap::new();
        for ufo_file in source_files(&text).unwrap() {
            let root = base.join(&ufo_file);
            for (path, contents) in crate::convertors::ufo::tests::read_tree(root.to_str().unwrap())
            {
                entries.insert(format!("{ufo_file}/{path}"), contents);
            }
        }
        entries.insert(DOCUMENT.to_string(), text);
        entries
    }

    fn open(entries: &HashMap<String, String>) -> Font {
        load_entries(PathBuf::from(DOCUMENT), entries).unwrap()
    }

    fn save(font: &Font, entries: &HashMap<String, String>, glyphs: Option<&[String]>) -> UfoSave {
        let layouts = layouts_from_entries(&entries[DOCUMENT], entries).unwrap();
        save_entries(
            font,
            DesignspaceSaveRequest {
                document: DOCUMENT,
                layouts: &layouts,
                glyphs,
                font_level: false,
            },
        )
        .unwrap()
    }

    fn whole(font: &Font, entries: &HashMap<String, String>) -> UfoEntries {
        match save(font, entries, None) {
            UfoSave::Entries(entries) => entries,
            UfoSave::NeedsFullSave(reason) => panic!("whole save refused: {reason}"),
        }
    }

    fn as_map(entries: &UfoEntries) -> HashMap<String, String> {
        entries.files.clone().into_iter().collect()
    }

    #[test]
    fn entries_round_trip_keeps_every_file_and_master() {
        let original = project_entries();
        let font = open(&original);
        let saved = whole(&font, &original);

        for path in original.keys().filter(|p| p.ends_with(".glif")) {
            assert!(saved.files.contains_key(path), "{path} was lost");
        }
        // Each UFO keeps its own master's metrics.
        for ufo_file in source_files(&original[DOCUMENT]).unwrap() {
            let info = |entries: &HashMap<String, String>| {
                plist::from_bytes::<norad::Plist>(
                    entries[&format!("{ufo_file}/fontinfo.plist")].as_bytes(),
                )
                .unwrap()
            };
            let (before, after) = (info(&original), info(&as_map(&saved)));
            for key in [
                "ascender",
                "capHeight",
                "xHeight",
                "styleName",
                "italicAngle",
            ] {
                assert_eq!(after.get(key), before.get(key), "{ufo_file} {key}");
            }
        }
        assert_eq!(
            parse_document(&saved.files[DOCUMENT]).unwrap(),
            parse_document(&original[DOCUMENT]).unwrap()
        );

        let reloaded = open(&as_map(&saved));
        assert_eq!(reloaded.glyphs, font.glyphs);
        assert_eq!(
            reloaded.masters.iter().map(|m| &m.id).collect::<Vec<_>>(),
            font.masters.iter().map(|m| &m.id).collect::<Vec<_>>()
        );
        assert_eq!(whole(&reloaded, &as_map(&saved)).files, saved.files);
    }

    #[test]
    fn partial_save_writes_one_glyphs_files_in_every_ufo() {
        // A first save gives every file Counterpunch's formatting.
        let first = project_entries();
        let original = as_map(&whole(&open(&first), &first));
        let mut original_with_document = original.clone();
        original_with_document.insert(DOCUMENT.to_string(), first[DOCUMENT].clone());
        let original = original_with_document;
        let mut font = open(&original);
        let bold = font.masters[1].id.clone();
        let glyph = font.glyphs.get_mut("A").unwrap();
        let layer = glyph
            .layers
            .iter_mut()
            .find(|l| l.master == LayerType::DefaultForMaster(bold.clone()))
            .unwrap();
        layer.width += 10.0;

        let full = whole(&font, &original);
        let names = ["A".to_string()];
        let UfoSave::Entries(partial) = save(&font, &original, Some(&names)) else {
            panic!("partial save refused");
        };
        assert!(!partial.files.is_empty());
        for (path, text) in &partial.files {
            assert!(path.ends_with("/A_.glif"), "{path}");
            assert_eq!(text, &full.files[path], "{path}");
        }
        let changed: Vec<_> = partial
            .files
            .iter()
            .filter(|(path, text)| original.get(*path) != Some(*text))
            .map(|(path, _)| path.as_str())
            .collect();
        assert_eq!(changed, ["IbarraRealNova-Bold.ufo/glyphs/A_.glif"]);
    }

    #[test]
    fn glyphs_load_from_their_glifs_like_the_whole_designspace() {
        let original = project_entries();
        let font = open(&original);
        let layouts = layouts_from_entries(&original[DOCUMENT], &original).unwrap();
        let (glyphs, unknown) = glyph_files_for_paths(
            &original[DOCUMENT],
            &layouts,
            &[
                "IbarraRealNova-Bold.ufo/glyphs/A_.glif".to_string(),
                "IbarraRealNova-Bold.ufo/images/x.png".to_string(),
            ],
        )
        .unwrap();
        assert_eq!(unknown, ["IbarraRealNova-Bold.ufo/images/x.png"]);
        let glifs: HashMap<String, String> = glyphs["A"]
            .iter()
            .map(|path| (path.clone(), original[path].clone()))
            .collect();
        let loaded =
            load_glyphs_from_glifs(&original[DOCUMENT], &layouts, &glifs, &["A".to_string()])
                .unwrap();
        let full = font.glyphs.get("A").unwrap();
        assert_eq!(loaded[0].layers, full.layers);
        assert_eq!(loaded[0].codepoints, full.codepoints);
    }

    #[test]
    fn sparse_masters_live_in_their_ufo_layer() {
        let mut entries = project_entries();
        let ufo = "IbarraRealNova-Regular.ufo";
        // A sparse master: glyph A in a layer of the Regular UFO.
        entries.insert(
            format!("{ufo}/layercontents.plist"),
            entries[&format!("{ufo}/layercontents.plist")].replace(
                "  </array>\n</plist>",
                "    <array>\n      <string>semi</string>\n      <string>glyphs.semi</string>\n    </array>\n  </array>\n</plist>",
            ),
        );
        entries.insert(
            format!("{ufo}/glyphs.semi/contents.plist"),
            entries[&format!("{ufo}/glyphs.public.background/contents.plist")].clone(),
        );
        for (path, text) in entries.clone() {
            if let Some(file) = path.strip_prefix(&format!("{ufo}/glyphs.public.background/")) {
                if file.ends_with(".glif") {
                    entries.insert(format!("{ufo}/glyphs.semi/{file}"), text);
                }
            }
        }
        entries.insert(
            DOCUMENT.to_string(),
            entries[DOCUMENT].replace(
                "  </sources>",
                "    <source filename=\"IbarraRealNova-Regular.ufo\" name=\"Semi\" layer=\"semi\">\n      <location>\n        <dimension name=\"Weight\" xvalue=\"550\"/>\n        <dimension name=\"Italic\" xvalue=\"0\"/>\n      </location>\n    </source>\n  </sources>",
            ),
        );

        let font = open(&entries);
        assert_eq!(font.masters.len(), 5);
        let saved = whole(&font, &entries);
        let semi_glifs: Vec<_> = saved
            .files
            .keys()
            .filter(|p| p.starts_with(&format!("{ufo}/glyphs.semi/")) && p.ends_with(".glif"))
            .collect();
        assert!(!semi_glifs.is_empty());
        let glyph = |text: &str| norad::Glyph::parse_raw(text.as_bytes()).unwrap();
        for path in &semi_glifs {
            assert_eq!(glyph(&saved.files[*path]), glyph(&entries[*path]), "{path}");
        }
        let document = parse_document(&saved.files[DOCUMENT]).unwrap();
        assert_eq!(document.sources[4].layer.as_deref(), Some("semi"));
        assert_eq!(open(&as_map(&saved)).glyphs, font.glyphs);
    }

    #[test]
    fn rules_mappings_and_lib_survive() {
        let mut entries = project_entries();
        entries.insert(
            DOCUMENT.to_string(),
            entries[DOCUMENT].replace(
                "  <sources>",
                "  <rules>\n    <rule name=\"heavy\">\n      <conditionset>\n        <condition name=\"Weight\" minimum=\"600\" maximum=\"700\"/>\n      </conditionset>\n      <sub name=\"dollar\" with=\"dollar.rvrn\"/>\n    </rule>\n  </rules>\n  <sources>",
            )
            .replace(
                "</designspace>",
                "  <lib>\n    <dict>\n      <key>com.example.note</key>\n      <string>kept</string>\n    </dict>\n  </lib>\n</designspace>",
            ),
        );
        let font = open(&entries);
        let saved = whole(&font, &entries);
        let before = parse_document(&entries[DOCUMENT]).unwrap();
        let after = parse_document(&saved.files[DOCUMENT]).unwrap();
        assert_eq!(after.rules, before.rules);
        assert_eq!(after.lib, before.lib);
        assert!(!after.rules.rules.is_empty());
    }

    /// A Glyphs font saved as a designspace keeps what compiling it needs.
    #[test]
    fn a_glyphs_font_keeps_what_compiling_needs() {
        let font = crate::load("resources/FustatG4-subset.glyphs").unwrap();
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("Fustat.designspace");
        font.save(&path).unwrap();
        let back = crate::load(&path).unwrap();

        // Names Glyphs keeps in English only.
        assert!(font.names.copyright.get_default().is_none());
        assert_eq!(
            back.names.copyright.get_default(),
            font.names.copyright.get_default_or_fallback()
        );
        assert_eq!(
            back.names.designer.get_default(),
            font.names.designer.get_default_or_fallback()
        );
        // OS/2 metrics Glyphs keeps in custom parameters.
        for metric in [
            crate::MetricType::SubscriptXSize,
            crate::MetricType::SuperscriptYOffset,
        ] {
            let before: Vec<_> = font
                .masters
                .iter()
                .map(|m| m.metrics.get(&metric))
                .collect();
            let after: Vec<_> = back
                .masters
                .iter()
                .map(|m| m.metrics.get(&metric))
                .collect();
            assert!(before.iter().all(Option::is_some), "{metric:?}");
            assert_eq!(after, before, "{metric:?}");
        }
        // Number values, for feature code using $dotshift.
        let numbers = "com.schriftgestalt.Glyphs.numberNames";
        assert!(font.format_specific.get(numbers).is_some());
        assert_eq!(
            back.format_specific.get(numbers),
            font.format_specific.get(numbers)
        );
        let values = "com.schriftgestalt.Glyphs.numberValues";
        for (a, b) in font.masters.iter().zip(&back.masters) {
            assert_eq!(b.format_specific.get(values), a.format_specific.get(values));
        }
        // The variable font export setting.
        assert_eq!(
            back.instances.iter().filter(|i| i.variable).count(),
            font.instances.iter().filter(|i| i.variable).count()
        );
        // Only UFO categories, and the one compiling gives an unset glyph.
        for entry in std::fs::read_dir(dir.path()).unwrap() {
            let ufo = entry.unwrap().path();
            if ufo.extension().is_some_and(|e| e == "ufo") {
                let ufo = norad::Font::load(&ufo).unwrap();
                let categories = ufo.lib["public.openTypeCategories"]
                    .as_dictionary()
                    .unwrap();
                for (glyph, category) in categories {
                    assert!(
                        ["base", "ligature", "mark", "component", "unassigned"]
                            .contains(&category.as_string().unwrap()),
                        "{glyph}: {category:?}"
                    );
                }
                assert_eq!(categories["acutecomb"].as_string(), Some("mark"));
            }
        }
    }
}

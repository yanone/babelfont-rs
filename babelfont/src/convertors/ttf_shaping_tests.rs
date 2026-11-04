//! Shapes text with a TTF and with the font its import compiles to, and
//! compares the glyphs and where they land: the importer moves kerning and mark
//! attachment out of the feature code for a compiler to regenerate, and the
//! regenerated font must shape as the original did.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::indexing_slicing)]

use harfrust::{Buffer, Direction, Font, ShapeOptions, ShaperFont};
use skrifa::{MetadataProvider, Tag};

use crate::convertors::fontir::{BabelfontIrSource, CompilationOptions};

/// A text to shape, with the script and language a text layout engine would
/// give its run (None for what the text itself says)
pub(crate) struct Sample<'a> {
    pub text: &'a str,
    pub script: Option<&'a str>,
    pub language: Option<&'a str>,
}

pub(crate) const fn text(text: &str) -> Sample<'_> {
    Sample {
        text,
        script: None,
        language: None,
    }
}

pub(crate) const fn in_script<'a>(text: &'a str, script: &'a str) -> Sample<'a> {
    Sample {
        text,
        script: Some(script),
        language: None,
    }
}

pub(crate) const fn in_language<'a>(
    text: &'a str,
    script: &'a str,
    language: &'a str,
) -> Sample<'a> {
    Sample {
        text,
        script: Some(script),
        language: Some(language),
    }
}

/// A glyph id, its advance and its offset
type Shaped = (u32, i32, i32, i32, i32);

/// Glyph ids, advances and offsets
fn shape(binary: &[u8], location: &[(Tag, f32)], sample: &Sample) -> Vec<Shaped> {
    let font = Font::new(binary.to_vec(), 0).expect("a font");
    let font = font
        .instance_builder()
        .variations(location.iter().map(|(t, v)| (*t, *v)))
        .build();
    let shaper = ShaperFont::new(&font);
    let mut buffer = Buffer::new();
    buffer.push_str(sample.text);
    if let Some(script) = sample.script {
        buffer.set_script(Some(script.parse().expect("a script")));
        buffer.set_direction(if matches!(script, "Arab" | "Hebr") {
            Direction::RightToLeft
        } else {
            Direction::LeftToRight
        });
    }
    if let Some(language) = sample.language {
        buffer.set_language(Some(language.parse().expect("a language")));
    }
    buffer.guess_segment_properties();
    harfrust::shape(&shaper, &mut buffer, ShapeOptions::new()).expect("shaped");
    buffer
        .glyph_infos()
        .iter()
        .zip(buffer.glyph_positions())
        .map(|(info, pos)| {
            (
                info.glyph_id,
                pos.x_advance,
                pos.y_advance,
                pos.x_offset,
                pos.y_offset,
            )
        })
        .collect()
}

/// Locations to shape at: the default, each axis's extremes and a point
/// between masters
fn locations(binary: &[u8]) -> Vec<Vec<(Tag, f32)>> {
    let font = skrifa::FontRef::new(binary).unwrap();
    let axes: Vec<_> = font.axes().iter().collect();
    let mut out = vec![vec![]];
    for axis in &axes {
        for value in [
            axis.min_value(),
            axis.max_value(),
            (axis.default_value() + axis.max_value()) * 0.5 + 0.37,
            (axis.default_value() + axis.min_value()) * 0.5 - 0.21,
        ] {
            out.push(vec![(axis.tag(), value)]);
        }
    }
    if axes.len() > 1 {
        out.push(
            axes.iter()
                .map(|a| {
                    (
                        a.tag(),
                        a.min_value() + (a.max_value() - a.min_value()) * 0.71,
                    )
                })
                .collect(),
        );
    }
    out
}

/// Whether two shapings have the same glyphs, advances and offsets, a unit
/// apart at most (an attached mark's offset two, its base's advance and its
/// anchor each): between masters, deltas compiled again may round otherwise
fn same_shaping(a: &[Shaped], b: &[Shaped]) -> bool {
    a.len() == b.len()
        && a.iter().zip(b).all(|(a, b)| {
            a.0 == b.0
                && (a.1 - b.1).abs() <= 1
                && (a.2 - b.2).abs() <= 1
                && (a.3 - b.3).abs() <= 2
                && (a.4 - b.4).abs() <= 2
        })
}

/// The differences between shaping with a TTF and with its import compiled
pub(crate) fn shaping_differences(binary: &[u8], samples: &[Sample]) -> Vec<String> {
    let font = crate::convertors::ttf::load_bytes(binary).unwrap();
    let compiled = BabelfontIrSource::compile(font, CompilationOptions::default())
        .unwrap_or_else(|e| panic!("the import compiles: {e:?}"));
    let mut differences = vec![];
    for location in locations(binary) {
        for sample in samples {
            let original = shape(binary, &location, sample);
            let recompiled = shape(&compiled, &location, sample);
            if !same_shaping(&original, &recompiled) {
                differences.push(format!(
                    "{:?} script {:?} language {:?} at {:?}:\n  original   {:?}\n  recompiled {:?}",
                    sample.text, sample.script, sample.language, location, original, recompiled
                ));
            }
        }
    }
    differences
}

pub(crate) fn assert_shapes_as_original(path: &str, samples: &[Sample]) {
    let binary = std::fs::read(path).unwrap();
    let differences = shaping_differences(&binary, samples);
    assert!(
        differences.is_empty(),
        "{path} shapes differently once imported and compiled:\n{}",
        differences.join("\n")
    );
}

/// Texts for the shaping fixtures: pairs, pairs across a mark, marks on
/// bases and on marks, in each script the fixtures have
const SAMPLES: &[Sample] = &[
    text("AV"),
    text("VA"),
    text("AO"),
    text("OA"),
    text("A\u{301}V"),
    text("A\u{300}V"),
    text("A\u{301}"),
    text("A\u{301}\u{300}"),
    text("A\u{300}\u{301}"),
    text("V\u{301}"),
    text("O\u{301}"),
    text("AΑ"),
    text("Α\u{301}"),
    text("11"),
    in_script("11", "Latn"),
    in_script("11", "Arab"),
    in_script("11", "Grek"),
    in_script("11", "Deva"),
    in_script("11", "Hebr"),
    in_script("11", "Cyrl"),
    text("-A"),
    text("A-"),
    text("\u{628}\u{627}"),
    text("\u{628}\u{64E}\u{627}"),
    text("\u{628}\u{628}"),
    text("\u{628}\u{628}\u{628}"),
    text("\u{627}\u{628}"),
    text("\u{628}\u{301}"),
    text("\u{628}\u{64E}"),
    text("\u{5D1}\u{5D0}"),
    text("\u{5D0}\u{5D1}"),
    text("\u{915}\u{916}"),
    text("\u{916}\u{915}"),
    text("\u{915}\u{93C}"),
    text("\u{916}\u{93C}"),
    text("\u{915}\u{301}"),
    in_script("\u{301}", "Latn"),
    in_language("AV", "Latn", "tr"),
    in_language("VA", "Latn", "tr"),
    in_language("AV", "Latn", "ro"),
    in_language("VA", "Latn", "ro"),
];

macro_rules! shaping_fixture_tests {
    ($($name:ident: $file:literal,)*) => {
        $(
            #[test]
            fn $name() {
                assert_shapes_as_original(concat!("resources/ttf/", $file), SAMPLES);
            }
        )*
    };
}

shaping_fixture_tests! {
    test_shaping_kerning_without_ignore_marks: "ShapingIgnoreMarks.ttf",
    test_shaping_kerning_for_one_script: "ShapingScripts.ttf",
    test_shaping_kerning_written_against_direction: "ShapingRtl.ttf",
    test_shaping_kerning_as_compiled_for_direction: "ShapingRtlCompiled.ttf",
    test_shaping_indic_kerning_beside_dist: "ShapingDist.ttf",
    test_shaping_marks_per_script: "ShapingMarks.ttf",
    test_shaping_marks_beside_blwm: "ShapingMarksEverywhere.ttf",
    test_shaping_indic_marks: "ShapingMarksNoBlwm.ttf",
    test_shaping_anchors_of_kept_lookups: "ShapingSuperset.ttf",
    test_shaping_anchor_names_across_scripts: "ShapingShare.ttf",
    test_shaping_default_language_system_only: "ShapingDflt.ttf",
    test_shaping_contextual_kerning_first: "ShapingContext.ttf",
    test_shaping_zero_pair_before_class_pair: "ShapingZero.ttf",
    test_shaping_language_under_script: "ShapingLanguage.ttf",
    test_shaping_language_excluding_default: "ShapingExcludeDflt.ttf",
    test_shaping_cursive: "ShapingCursive.ttf",
    test_shaping_kerning_before_cursive: "ShapingKernThenCursive.ttf",
    test_shaping_mark_to_mark: "ShapingMkmk.ttf",
    test_shaping_mark_to_mark_without_filter: "ShapingMkmkNoFilter.ttf",
    test_shaping_mark_to_mark_in_mark: "ShapingMkmkInMark.ttf",
    test_shaping_unencoded_alternate: "ShapingAlternate.ttf",
    test_shaping_variable: "ShapingVariable.ttf",
}

#[test]
fn test_shaping_existing_fixtures() {
    for path in [
        "resources/ttf/Variable.ttf",
        "resources/ttf/RedditSans-Subset.ttf",
        "resources/ttf/Anchors.ttf",
        "resources/ttf/RtlKerning.ttf",
        "resources/ttf/GdefClasses.ttf",
        "resources/ttf/MarkNoGdef.ttf",
    ] {
        let binary = std::fs::read(path).unwrap();
        let font = skrifa::FontRef::new(&binary).unwrap();
        let mut texts: Vec<String> = vec![];
        let chars: Vec<char> = font
            .charmap()
            .mappings()
            .filter_map(|(c, _)| char::from_u32(c))
            .collect();
        let bases: Vec<char> = chars
            .iter()
            .copied()
            .filter(|c| !matches!(*c as u32, 0x300..=0x36F))
            .collect();
        let marks: Vec<char> = chars
            .iter()
            .copied()
            .filter(|c| matches!(*c as u32, 0x300..=0x36F))
            .collect();
        for a in &bases {
            for b in &bases {
                texts.push(format!("{a}{b}"));
            }
            for m in &marks {
                texts.push(format!("{a}{m}"));
                for n in &marks {
                    texts.push(format!("{a}{m}{n}"));
                }
                if let Some(b) = bases.first() {
                    texts.push(format!("{a}{m}{b}"));
                }
            }
        }
        let samples: Vec<Sample> = texts.iter().map(|t| text(t)).collect();
        let differences = shaping_differences(&binary, &samples);
        assert!(
            differences.is_empty(),
            "{path} shapes differently once imported and compiled ({} of {}):\n{}",
            differences.len(),
            samples.len() * locations(&binary).len(),
            differences
                .iter()
                .take(20)
                .cloned()
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

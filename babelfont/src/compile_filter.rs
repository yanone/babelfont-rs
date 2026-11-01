//! Editor and command-line CompileFilter orchestration.
//!
//! Anchor inheritance runs in Rust, and only when a linked plugin implements
//! `filter_layer_json_propagated_anchors`. The Python hooks then see Babelfont
//! JSON. `setup`, `takedown`, and `onGuideChange` are editor-only.

use std::path::Path;
use std::process::Command;

use serde_json::{json, Value};

use crate::computed_anchors::{
    fill_font_missing_inherited_anchors, linked_compile_filter_ids,
};
use crate::features::PossiblyAutomaticCode;
use crate::{Features, Font};

const RUNNER: &str = include_str!("compile_filter_runner.py");

/// Run linked CompileFilters from vendored wheels, then merge their JSON output.
pub fn apply_compile_filters(font: &mut Font, wheels_dir: Option<&Path>) -> Result<(), String> {
    let ids = linked_compile_filter_ids(font);
    if ids.is_empty() {
        return Ok(());
    }
    let Some(wheels_dir) = wheels_dir else {
        return Ok(());
    };
    if !wheels_dir.is_dir() {
        return Err(format!(
            "compile filter wheels directory does not exist: {}",
            wheels_dir.display()
        ));
    }

    let probe = run_python(
        wheels_dir,
        json!({
            "op": "probe",
            "ids": ids,
        }),
    )?;
    let wants_anchors = probe
        .get("propagated")
        .and_then(|value| value.as_bool())
        .unwrap_or(false);
    if wants_anchors {
        fill_font_missing_inherited_anchors(font);
    }

    let font_json = serde_json::to_value(&*font).map_err(|error| error.to_string())?;
    let result = run_python(
        wheels_dir,
        json!({
            "op": "compile",
            "ids": linked_compile_filter_ids(font),
            "font": font_json,
        }),
    )?;
    apply_python_result(font, &result)
}

fn apply_python_result(font: &mut Font, result: &Value) -> Result<(), String> {
    let Some(layers) = result.get("layers").and_then(|value| value.as_object()) else {
        return Ok(());
    };
    for (key, layer_json) in layers {
        let Some((glyph_name, layer_id)) = key.split_once('\u{1f}') else {
            continue;
        };
        let Ok(layer) = serde_json::from_value(layer_json.clone()) else {
            continue;
        };
        let Some(glyph) = font.glyphs.get_mut(glyph_name) else {
            continue;
        };
        if let Some(slot) = glyph
            .layers
            .iter_mut()
            .find(|candidate| candidate.id.as_deref() == Some(layer_id))
        {
            *slot = layer;
        }
    }
    if let Some(features) = result.get("features").and_then(|value| value.as_str()) {
        if !features.is_empty() {
            insert_mkmk_lookups(&mut font.features, features);
        }
    }
    Ok(())
}

fn insert_mkmk_lookups(features: &mut Features, extra: &str) {
    const MARKER: &str = "# Automatic Code";
    if let Some((_, code)) = features
        .features
        .iter_mut()
        .find(|(tag, _)| tag.as_str() == "mkmk")
    {
        if let Some(index) = code.code.find(MARKER) {
            let insert_at = index + MARKER.len();
            code.code.insert_str(insert_at, &format!("\n{extra}\n"));
        } else {
            code.code.push_str(&format!("\n{MARKER}\n{extra}\n"));
        }
        return;
    }
    features.features.push((
        "mkmk".into(),
        PossiblyAutomaticCode::new(format!("{MARKER}\n{extra}\n")),
    ));
}

fn run_python(wheels_dir: &Path, payload: Value) -> Result<Value, String> {
    let mut child = Command::new("python3")
        .arg("-c")
        .arg(RUNNER)
        .arg(wheels_dir)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|error| format!("failed to start python3: {error}"))?;
    {
        let stdin = child.stdin.as_mut().ok_or("python3 stdin was closed")?;
        serde_json::to_writer(stdin, &payload).map_err(|error| error.to_string())?;
    }
    let output = child
        .wait_with_output()
        .map_err(|error| format!("python3 failed: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "compile filter python failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    serde_json::from_slice(&output.stdout).map_err(|error| {
        format!(
            "compile filter python returned invalid JSON: {error}; {}",
            String::from_utf8_lossy(&output.stdout)
        )
    })
}

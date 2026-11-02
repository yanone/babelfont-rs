#![cfg(feature = "cli")]

/// A font the command line cannot load is an error message and a failed
/// exit, not a panic
#[test]
fn test_cli_reports_a_font_it_cannot_load() {
    let dir = std::env::temp_dir().join(format!("babelfont-cli-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let input = dir.join("broken.ttf");
    std::fs::write(&input, b"not a font").unwrap();
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_babelfont"))
        .arg(&input)
        .arg(dir.join("out.babelfont"))
        .output()
        .unwrap();
    std::fs::remove_dir_all(&dir).unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(1), "{stderr}");
    assert!(stderr.contains("Failed to load"), "{stderr}");
    assert!(!stderr.contains("panicked"), "{stderr}");
}

/// Reading a font's features does not flood the log
#[test]
fn test_cli_does_not_log_every_glyph_class_token() {
    let dir = std::env::temp_dir().join(format!("babelfont-cli-log-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_babelfont"))
        .arg("resources/ttf/RedditSans-Subset.ttf")
        .arg(dir.join("out.babelfont"))
        .env_remove("RUST_LOG")
        .output()
        .unwrap();
    std::fs::remove_dir_all(&dir).unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "{stderr}");
    assert!(!stderr.contains("Unrecognized child"), "{stderr}");
}

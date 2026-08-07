//! Drop a description that is really a copyright notice.
//!
//! FontLab Studio 5 had a long-standing bug that copied the copyright notice
//! into the description field, and sources built with it -- including
//! FontForge files derived from them -- still carry it. Emitted, it becomes a
//! name ID 10 holding a copyright notice.
//!
//! A faithful conversion keeps what the source states, so this is an opt-in
//! correction: it removes the description only when it looks like a notice,
//! and leaves real prose alone.

use crate::filters::FontFilter;

/// Does this string look like a copyright notice rather than prose?
fn is_copyright_notice(notice: &str) -> bool {
    let trimmed = notice.trim_start();
    let lowered = trimmed.to_lowercase();
    lowered.starts_with("copyright") || trimmed.starts_with('\u{a9}') || lowered.starts_with("(c)")
}

/// Drop a description that is really a copyright notice.
#[derive(Default)]
pub struct DropCopyrightDescription;

impl DropCopyrightDescription {
    /// Create a new DropCopyrightDescription filter
    pub fn new() -> Self {
        DropCopyrightDescription
    }
}

impl FontFilter for DropCopyrightDescription {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        if font
            .names
            .description
            .0
            .values()
            .any(|value| is_copyright_notice(value))
        {
            log::info!("Dropping a description field that is a copyright notice");
            font.names.description.0.clear();
        }
        Ok(())
    }

    fn from_str(_s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(DropCopyrightDescription::new())
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        clap::Arg::new("dropcopyrightdescription")
            .long("drop-copyright-description")
            .help(
                "Drop a description field that is really a copyright notice \
                 (a FontLab Studio 5 bug copied the notice there). A \
                 correction, not a faithful conversion.",
            )
            .action(clap::ArgAction::SetTrue)
    }
}

#[allow(clippy::unwrap_used, clippy::expect_used)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_copyright_notice() {
        // The forms seen in affected sources.
        assert!(is_copyright_notice(
            "Copyright (c) 2011 by Gesine Todt. All rights reserved."
        ));
        assert!(is_copyright_notice("copyright 2012 vernon adams"));
        assert!(is_copyright_notice("(c) 2011 Kimberly Geswein"));
        assert!(is_copyright_notice("\u{a9} 2011 Kimberly Geswein"));

        // Leading whitespace must not hide it.
        assert!(is_copyright_notice("   Copyright 2011"));

        // Real prose is left alone, including text that merely mentions
        // copyright rather than being a notice.
        assert!(!is_copyright_notice(
            "A display face for headlines and posters."
        ));
        assert!(!is_copyright_notice(
            "This font is in the public domain; no copyright is claimed."
        ));
        assert!(!is_copyright_notice(""));
    }

    #[test]
    fn drops_a_notice_but_keeps_prose() {
        let mut font = crate::Font::new();
        font.names
            .description
            .set_default("Copyright (c) 2011 by Gesine Todt.".to_string());
        DropCopyrightDescription::new().apply(&mut font).unwrap();
        assert!(font.names.description.0.is_empty());

        let mut font = crate::Font::new();
        font.names
            .description
            .set_default("A display face for headlines.".to_string());
        DropCopyrightDescription::new().apply(&mut font).unwrap();
        assert!(!font.names.description.0.is_empty());
    }
}

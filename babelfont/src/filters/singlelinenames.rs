//! Flatten every name-table string to a single line.
//!
//! A `name` record is single-line, but source formats are not restricted to
//! single-line values: an SFD encodes hard line breaks (and sometimes NULs)
//! into its `LangName` licence text, FontLab stores carriage returns, and a
//! Glyphs file may carry line breaks in its properties. A faithful conversion
//! keeps them, so this is an opt-in correction rather than convertor
//! behaviour: applying it produces a font that differs from the source's
//! stated text.
//!
//! Any run of line breaks, with the whitespace around it, becomes one space;
//! every other control character is removed.

use crate::{filters::FontFilter, I18NDictionary};

// Flatten a string so it can be stored in a `name` record.
fn single_line(value: &str) -> String {
    value
        .split(['\r', '\n'])
        // A name record cannot carry a control character at all. Line breaks
        // become the join below; anything else in C0 (NUL especially) is
        // simply removed. Real licence text does encode NUL: an SFD LangName
        // `+AA0ACgAA-` run is CR, LF, NUL.
        .map(|part| part.chars().filter(|c| !c.is_control()).collect::<String>())
        .map(|part| part.trim().to_string())
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

/// Flatten every name-table string to a single line.
#[derive(Default)]
pub struct SingleLineNames;

impl SingleLineNames {
    /// Create a new SingleLineNames filter
    pub fn new() -> Self {
        SingleLineNames
    }
}

fn flatten(dict: &mut I18NDictionary) {
    for value in dict.0.values_mut() {
        *value = single_line(value);
    }
}

impl FontFilter for SingleLineNames {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        for (_nameid, dict) in font.names.iter_mut() {
            flatten(dict);
        }
        Ok(())
    }

    fn from_str(_s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        Ok(SingleLineNames::new())
    }

    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg
    where
        Self: Sized,
    {
        clap::Arg::new("singlelinenames")
            .long("single-line-names")
            .help(
                "Flatten every name-table string to a single line and strip \
                 control characters. A correction, not a faithful conversion: \
                 a source may deliberately carry line breaks in its licence or \
                 description text.",
            )
            .action(clap::ArgAction::SetTrue)
    }
}

#[allow(clippy::unwrap_used, clippy::expect_used)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_line() {
        // The case this exists for: an OFL description with hard breaks. An
        // SFD encodes these as carriage returns in its LangName line.
        assert_eq!(
            single_line(
                "This Font Software is licensed under the SIL Open Font License,\rVersion 1.1."
            ),
            "This Font Software is licensed under the SIL Open Font License, Version 1.1."
        );

        // Every flavour of break, and runs of them, collapse to one space.
        assert_eq!(single_line("a\nb"), "a b");
        assert_eq!(single_line("a\r\nb"), "a b");
        assert_eq!(single_line("a\n\n\nb"), "a b");

        // Whitespace around a break is absorbed rather than doubled up.
        assert_eq!(single_line("a  \n  b"), "a b");

        // Leading and trailing breaks leave no stray space.
        assert_eq!(single_line("\na\n"), "a");

        // A string with no break is returned unchanged, including interior
        // spacing, which is not ours to normalise.
        assert_eq!(single_line("a  b"), "a  b");
        assert_eq!(single_line(""), "");

        // A control character can never appear in a name record. NUL is the
        // one that actually occurs in licence text.
        assert_eq!(single_line("a\u{0}b"), "ab");
        assert_eq!(
            single_line("---\r\n\u{0}SIL OPEN FONT"),
            "--- SIL OPEN FONT"
        );
        assert_eq!(single_line("\u{0}"), "");
        assert_eq!(single_line("a\u{7}\u{1b}b"), "ab");
        // A part that is only control characters must not leave a stray space.
        assert_eq!(single_line("a\n\u{0}\nb"), "a b");

        // Idempotent: flattening an already-flat string changes nothing.
        let once = single_line("a\nb\nc");
        assert_eq!(single_line(&once), once);
    }

    #[test]
    fn flattens_every_field_and_language() {
        let mut font = crate::Font::new();
        font.names.license.set_default(
            "This Font Software is licensed under the SIL Open Font License,\rVersion 1.1."
                .to_string(),
        );
        font.names
            .description
            .0
            .insert("fr".to_string(), "Une\ndescription".to_string());
        // A NUL encoded into licence text must go too.
        font.names
            .copyright
            .set_default("Copyright\u{0} 2011".to_string());

        SingleLineNames::new().apply(&mut font).unwrap();

        assert_eq!(
            font.names.license.get_default().unwrap(),
            "This Font Software is licensed under the SIL Open Font License, Version 1.1."
        );
        assert_eq!(
            font.names.description.0.get("fr").unwrap(),
            "Une description"
        );
        assert_eq!(
            font.names.copyright.get_default().unwrap(),
            "Copyright 2011"
        );
    }

    #[test]
    fn leaves_single_line_values_untouched() {
        let mut font = crate::Font::new();
        font.names.family_name.set_default("Two  Words".to_string());
        SingleLineNames::new().apply(&mut font).unwrap();
        // Interior spacing is not ours to normalise.
        assert_eq!(font.names.family_name.get_default().unwrap(), "Two  Words");
    }
}

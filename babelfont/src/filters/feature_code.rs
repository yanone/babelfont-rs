//! Helpers for filters that move rules out of feature code
use std::collections::HashSet;

use fea_rs_ast::{AsFea, Comment, FeatureBlock, FeatureFile, Statement, ToplevelItem};
use smol_str::SmolStr;

use crate::Features;

pub(crate) use super::layout_model::INSERTION_MARKER;

fn reference_pattern(names: &HashSet<SmolStr>) -> Option<regex::Regex> {
    if names.is_empty() {
        return None;
    }
    let alternatives = names
        .iter()
        .map(|n| regex::escape(n))
        .collect::<Vec<_>>()
        .join("|");
    regex::Regex::new(&format!(r"\blookup\s+(?:{alternatives})\s*;")).ok()
}

/// Takes `lookup <name>;` references out of feature code, however many share
/// a line; a line left with nothing goes
pub(crate) fn remove_lookup_references(code: &str, names: &HashSet<SmolStr>) -> String {
    let Some(pattern) = reference_pattern(names) else {
        return code.to_string();
    };
    code.lines()
        .filter_map(|line| {
            if !pattern.is_match(line) {
                return Some(line.to_string());
            }
            let indent = &line[..line.len() - line.trim_start().len()];
            let rest = pattern.replace_all(line, "");
            (!rest.trim().is_empty()).then(|| format!("{indent}{}", rest.trim()))
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Whether feature code refers to or defines any of the lookups
fn mentions_lookups(code: &str, names: &HashSet<SmolStr>) -> bool {
    if names.is_empty() {
        return false;
    }
    let alternatives = names
        .iter()
        .map(|n| regex::escape(n))
        .collect::<Vec<_>>()
        .join("|");
    regex::Regex::new(&format!(r"\blookup\s+(?:{alternatives})\b"))
        .map(|p| p.is_match(code))
        .unwrap_or(true)
}

/// Whether feature code states nothing but scripts, languages and comments
pub(crate) fn is_empty_feature(code: &str) -> bool {
    code.lines().all(|line| {
        let line = line.split('#').next().unwrap_or("").trim();
        line.is_empty()
            || line
                .split(';')
                .map(str::trim)
                .all(|s| s.is_empty() || s.starts_with("script ") || s.starts_with("language "))
    })
}

/// A lookup block's text in feature code: `lookup <name> { ... } <name>;`
pub(crate) fn lookup_block_pattern(name: &str) -> Option<regex::Regex> {
    let name = regex::escape(name);
    regex::Regex::new(&format!(
        r"(?s)[ \t]*\blookup\s+{name}\s*\{{.*?\}}\s*{name}\s*;[ \t]*\n?"
    ))
    .ok()
}

/// Feature blocks holding nothing but an insertion marker: a compiler puts
/// the lookups it generates for each where the block is
pub(crate) fn marker_blocks(tags: &[&str]) -> String {
    tags.iter()
        .map(|tag| format!("feature {tag} {{\n    {INSERTION_MARKER}\n}} {tag};\n"))
        .collect()
}

/// Takes moved lookups out of feature code where each is a block of its own
/// in some prefix, as a decompiled font's code is: the blocks and references
/// to them go, a feature left with nothing goes, and feature blocks with an
/// insertion marker for `marker_tags` take the place of the lookup `first`,
/// so that a compiler generates them there, in the order the lookup list had
/// (`markers` names each such lookup with its tags).
/// False, changing nothing, where the code is not structured so or a
/// reference would stay.
pub(crate) fn remove_lookup_blocks(
    features: &mut Features,
    names: &HashSet<SmolStr>,
    markers: &[(SmolStr, Vec<&str>)],
) -> bool {
    let patterns: Vec<(SmolStr, regex::Regex)> = names
        .iter()
        .filter_map(|name| lookup_block_pattern(name).map(|p| (name.clone(), p)))
        .collect();
    let structured = patterns.len() == names.len()
        && markers.iter().all(|(first, _)| names.contains(first))
        && patterns.iter().all(|(_, pattern)| {
            features
                .prefixes
                .values()
                .filter(|code| pattern.is_match(&code.code))
                .count()
                == 1
        });
    if !structured {
        return false;
    }
    let before = features.clone();
    for code in features.prefixes.values_mut() {
        for (name, pattern) in &patterns {
            let replacement = markers
                .iter()
                .find(|(first, _)| first == name)
                .map(|(_, tags)| marker_blocks(tags))
                .unwrap_or_default();
            let replacement = replacement.as_str();
            code.code = pattern
                .replacen(&code.code, 1, regex::NoExpand(replacement))
                .into_owned();
        }
    }
    features
        .prefixes
        .retain(|_, code| !code.code.trim().is_empty());
    for (_, code) in features.features.iter_mut() {
        code.code = remove_lookup_references(&code.code, names);
    }
    features
        .features
        .retain(|(_, code)| !is_empty_feature(&code.code));
    let remaining = features
        .prefixes
        .values()
        .chain(features.features.iter().map(|(_, c)| c))
        .any(|code| mentions_lookups(&code.code, names));
    if remaining {
        log::warn!("Lookups stay feature code; references to them would remain");
        *features = before;
        return false;
    }
    true
}

/// Takes moved lookups out of a parsed feature file, whatever its structure:
/// definitions and references go, a feature left with nothing goes, and
/// feature blocks with an insertion marker take the place of the lookup
/// `first`.
pub(crate) fn remove_lookups_from_file(
    file: &FeatureFile,
    names: &HashSet<SmolStr>,
    markers: &[(SmolStr, Vec<&str>)],
) -> Features {
    let mut file = file.clone();
    let marker_items = |tags: &[&str]| -> Vec<ToplevelItem> {
        tags.iter()
            .map(|tag| {
                ToplevelItem::Feature(FeatureBlock::new(
                    (*tag).into(),
                    vec![Statement::Comment(Comment::new(INSERTION_MARKER.into()))],
                    false,
                    0..0,
                ))
            })
            .collect()
    };
    let mut statements = vec![];
    for item in std::mem::take(&mut file.statements) {
        match item {
            ToplevelItem::Lookup(lookup) if names.contains(&lookup.name) => {
                if let Some((_, tags)) = markers.iter().find(|(first, _)| *first == lookup.name) {
                    statements.extend(marker_items(tags));
                }
            }
            ToplevelItem::Feature(mut feature) => {
                feature.statements.retain(|s| match s {
                    Statement::LookupReference(r) => !names.contains(r.lookup_name.as_str()),
                    _ => true,
                });
                let empty = feature.statements.iter().all(|s| {
                    matches!(
                        s,
                        Statement::Script(_) | Statement::Language(_) | Statement::Comment(_)
                    )
                });
                if !empty {
                    statements.push(ToplevelItem::Feature(feature));
                }
            }
            other => statements.push(other),
        }
    }
    file.statements = statements;
    Features::from_fea(&file.as_fea(""))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn names(names: &[&str]) -> HashSet<SmolStr> {
        names.iter().map(|n| SmolStr::from(*n)).collect()
    }

    #[test]
    fn test_references_sharing_a_line_go() {
        let code = "lookup k; lookup ctx; lookup k2; # kerning\nlookup k;";
        let out = remove_lookup_references(code, &names(&["k", "k2"]));
        assert_eq!(out, "lookup ctx;  # kerning");
        assert!(!mentions_lookups(&out, &names(&["k", "k2"])));
    }

    #[test]
    fn test_a_reference_in_a_comment_does_not_go_alone() {
        let code = "lookup ctx; # lookup k;";
        let out = remove_lookup_references(code, &names(&["k"]));
        assert_eq!(out, "lookup ctx; #");
    }

    #[test]
    fn test_empty_features() {
        assert!(is_empty_feature(
            "script latn; language TRK;\n# Automatic Code"
        ));
        assert!(!is_empty_feature("script latn; lookup x;"));
    }
}

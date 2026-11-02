//! Helpers for filters that move rules out of feature code
use std::collections::HashSet;

use fea_rs_ast::Statement;
use smol_str::SmolStr;

use crate::Features;

/// The lookups each script and language section of a feature block applies
pub(crate) fn lookups_by_section(statements: &[Statement]) -> Option<Vec<Vec<SmolStr>>> {
    // a block that starts with a script statement has no default section
    let starts_with_script = statements
        .iter()
        .find(|s| !matches!(s, Statement::Comment(_)))
        .is_some_and(|s| matches!(s, Statement::Script(_) | Statement::Language(_)));
    let mut sections: Vec<Vec<SmolStr>> = if starts_with_script {
        vec![]
    } else {
        vec![vec![]]
    };
    let mut default_lookups: Vec<SmolStr> = vec![];
    let mut in_default = true;
    for statement in statements {
        match statement {
            Statement::Script(_) | Statement::Language(_) => {
                in_default = false;
                // a new section starts with the default section's lookups
                sections.push(default_lookups.clone());
            }
            Statement::LookupReference(reference) => {
                let name: SmolStr = reference.lookup_name.as_str().into();
                if in_default {
                    default_lookups.push(name.clone());
                }
                sections.last_mut()?.push(name);
            }
            Statement::Comment(_) => {}
            // rules written in the feature block itself stay where they are
            _ => {}
        }
    }
    Some(sections)
}

pub(crate) fn lookup_reference_count(fea: &str, name: &str) -> usize {
    let Ok(pattern) = regex::Regex::new(&format!(r"\blookup\s+{}\b", regex::escape(name))) else {
        return usize::MAX;
    };
    pattern.find_iter(fea).count()
}

/// The comment that tells a compiler where to put the kerning it generates
/// (ufo2ft's; fontc skips generating a feature the code states without it)
pub(crate) const INSERTION_MARKER: &str = "# Automatic Code";

/// Replaces `lookup <name>;` lines in feature code with one insertion marker,
/// where the first of them was
pub(crate) fn remove_lookup_references(code: &str, names: &HashSet<SmolStr>) -> String {
    let mut marked = code.lines().any(|l| l.trim().starts_with(INSERTION_MARKER));
    code.lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            let moved = trimmed.ends_with(';')
                && names.iter().any(|name| {
                    trimmed
                        .strip_prefix("lookup")
                        .map(|rest| rest.trim().trim_end_matches(';').trim() == name.as_str())
                        .unwrap_or(false)
                });
            if !moved {
                return Some(line.to_string());
            }
            if marked {
                return None;
            }
            marked = true;
            Some(INSERTION_MARKER.to_string())
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Whether feature code states nothing but scripts, languages and comments
pub(crate) fn is_empty_feature(code: &str) -> bool {
    code.lines().all(|line| {
        let line = line.split('#').next().unwrap_or("").trim();
        line.is_empty() || line.starts_with("script ") || line.starts_with("language ")
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

/// Takes moved lookups out of feature code where each is a block of its own
/// in some prefix and features of these tags refer to it, as a decompiled
/// font's code is: the blocks go, references to them become one insertion
/// marker per feature (so that a compiler generates the feature there), and a
/// feature left with nothing goes. False, changing nothing, where the code is
/// not structured so.
pub(crate) fn remove_lookup_blocks(
    features: &mut Features,
    names: &HashSet<SmolStr>,
    feature_tags: &[&str],
) -> bool {
    let patterns: Vec<regex::Regex> = names
        .iter()
        .filter_map(|name| lookup_block_pattern(name))
        .collect();
    let structured = patterns.len() == names.len()
        && patterns.iter().all(|pattern| {
            features
                .prefixes
                .values()
                .filter(|code| pattern.is_match(&code.code))
                .count()
                == 1
        })
        && features
            .features
            .iter()
            .any(|(n, _)| feature_tags.contains(&n.as_str()));
    if !structured {
        return false;
    }
    for code in features.prefixes.values_mut() {
        for pattern in &patterns {
            code.code = pattern.replace(&code.code, "").into_owned();
        }
    }
    features
        .prefixes
        .retain(|_, code| !code.code.trim().is_empty());
    for (_, code) in features
        .features
        .iter_mut()
        .filter(|(n, _)| feature_tags.contains(&n.as_str()))
    {
        code.code = remove_lookup_references(&code.code, names);
    }
    features
        .features
        .retain(|(n, code)| !feature_tags.contains(&n.as_str()) || !is_empty_feature(&code.code));
    true
}

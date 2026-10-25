use smol_str::SmolStr;

/// Pair the source and target glyphs of a single substitution.
///
/// A single target glyph applies to every source glyph, as in
/// `sub [a b c] by x;`. Otherwise both sides are paired by position and must
/// have the same length. Returns `None` when the lengths don't match.
pub(crate) fn single_subst_pairs(
    from: Vec<SmolStr>,
    to: Vec<SmolStr>,
) -> Option<Vec<(SmolStr, SmolStr)>> {
    if to.len() == 1 {
        let target = &to[0];
        return Some(from.into_iter().map(|f| (f, target.clone())).collect());
    }
    if from.len() != to.len() {
        return None;
    }
    Some(from.into_iter().zip(to).collect())
}

#[cfg(test)]
mod tests {
    use super::single_subst_pairs;
    use smol_str::SmolStr;

    fn names(glyphs: &[&str]) -> Vec<SmolStr> {
        glyphs.iter().map(|g| SmolStr::new(g)).collect()
    }

    fn pairs(from: &[&str], to: &[&str]) -> Option<Vec<(String, String)>> {
        single_subst_pairs(names(from), names(to)).map(|pairs| {
            pairs
                .into_iter()
                .map(|(f, t)| (f.to_string(), t.to_string()))
                .collect()
        })
    }

    fn owned(pairs: &[(&str, &str)]) -> Option<Vec<(String, String)>> {
        Some(
            pairs
                .iter()
                .map(|(f, t)| (f.to_string(), t.to_string()))
                .collect(),
        )
    }

    #[test]
    fn single_target_applies_to_every_source() {
        assert_eq!(
            pairs(&["a", "b", "c"], &["x"]),
            owned(&[("a", "x"), ("b", "x"), ("c", "x")])
        );
    }

    #[test]
    fn equal_lengths_pair_by_position() {
        assert_eq!(
            pairs(&["a", "b"], &["x", "y"]),
            owned(&[("a", "x"), ("b", "y")])
        );
    }

    #[test]
    fn mismatched_lengths_are_rejected() {
        assert_eq!(pairs(&["a", "b", "c"], &["x", "y"]), None);
    }
}

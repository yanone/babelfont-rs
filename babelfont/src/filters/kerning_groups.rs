//! Kerning groups a compiler split by script, put back together.
//!
//! fontc writes a kerning group's members of each script as a class of their
//! own: Fustat's @A becomes a class of its Latin letters and one of
//! backslash and backslash.case, which are common to all scripts, each
//! kerning as the group did. Read back, the pieces are groups of their own,
//! and a piece of one glyph is no group at all, so kerning the group means
//! kerning each piece. Two pieces are taken to be one group where their
//! glyphs are of different scripts (a compiler never splits a group
//! otherwise) and they kern alike wherever both kern; they are merged where
//! that leaves every pair of glyphs kerning as it did, but for a pair of
//! glyphs of both directions, which a compiler leaves out as no text has it.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use indexmap::IndexMap;
use smol_str::SmolStr;

use crate::Font;

/// What a compiler knows of each glyph, by name
pub(crate) struct GlyphScripts {
    /// The scripts a compiler kerns the glyph in; common glyphs as Zyyy
    pub scripts: HashMap<SmolStr, BTreeSet<SmolStr>>,
    pub left_to_right: HashSet<SmolStr>,
    pub right_to_left: HashSet<SmolStr>,
}

impl GlyphScripts {
    fn of(&self, glyphs: &[SmolStr]) -> BTreeSet<SmolStr> {
        glyphs
            .iter()
            .flat_map(|g| {
                self.scripts
                    .get(g)
                    .cloned()
                    .unwrap_or_else(|| [SmolStr::from("Zyyy")].into())
            })
            .collect()
    }

    /// Whether no run of text has both glyphs: one is left to right, the
    /// other right to left
    fn apart(&self, a: &SmolStr, b: &SmolStr) -> bool {
        (self.left_to_right.contains(a) && self.right_to_left.contains(b))
            || (self.right_to_left.contains(a) && self.left_to_right.contains(b))
    }
}

type Kerning = IndexMap<(SmolStr, SmolStr), i16>;
type Groups = IndexMap<SmolStr, Vec<SmolStr>>;

/// One side of the kerning, seen from its first glyphs: the second side's
/// is the same with each pair turned round
struct View {
    masters: Vec<Kerning>,
    firsts: Groups,
    seconds: Groups,
    /// Turned round: a glyph-and-group pair comes after a group-and-glyph one
    turned: bool,
}

impl View {
    fn key(name: &SmolStr) -> SmolStr {
        format!("@{name}").into()
    }

    /// The kerning of two glyphs at a master, as a compiler resolves it: a
    /// pair of glyphs, then of a glyph and a group, of a group and a glyph,
    /// then of groups
    fn value(
        &self,
        kerning: &Kerning,
        first_of: &HashMap<SmolStr, SmolStr>,
        second_of: &HashMap<SmolStr, SmolStr>,
        a: &SmolStr,
        b: &SmolStr,
    ) -> i16 {
        let ga = first_of.get(a).map(Self::key);
        let gb = second_of.get(b).map(Self::key);
        let mut keys = vec![(Some(a.clone()), Some(b.clone()))];
        let glyph_group = (Some(a.clone()), gb.clone());
        let group_glyph = (ga.clone(), Some(b.clone()));
        if self.turned {
            keys.extend([group_glyph, glyph_group]);
        } else {
            keys.extend([glyph_group, group_glyph]);
        }
        keys.push((ga, gb));
        keys.into_iter()
            .find_map(|(x, y)| kerning.get(&(x?, y?)))
            .copied()
            .unwrap_or(0)
    }

    fn members(groups: &Groups, side: &SmolStr) -> Vec<SmolStr> {
        match side.strip_prefix('@') {
            Some(name) => groups.get(name).cloned().unwrap_or_default(),
            None => vec![side.clone()],
        }
    }

    fn group_of(groups: &Groups) -> HashMap<SmolStr, SmolStr> {
        groups
            .iter()
            .flat_map(|(name, members)| members.iter().map(move |m| (m.clone(), name.clone())))
            .collect()
    }

    /// Each first side's kerning: by second side, its value at each master
    fn rows(&self) -> BTreeMap<SmolStr, BTreeMap<SmolStr, Vec<i16>>> {
        let mut rows: BTreeMap<SmolStr, BTreeMap<SmolStr, Vec<i16>>> = BTreeMap::new();
        let count = self.masters.len();
        for (i, kerning) in self.masters.iter().enumerate() {
            for ((first, second), value) in kerning {
                let row = rows.entry(first.clone()).or_default();
                row.entry(second.clone()).or_insert_with(|| vec![0; count])[i] = *value;
            }
        }
        rows
    }

    /// Merges first sides `a` and `b` into one group, if that keeps every
    /// pair of glyphs kerning as it did; the merged group's side
    fn try_merge(&mut self, a: &SmolStr, b: &SmolStr, info: &GlyphScripts) -> Option<SmolStr> {
        let grouped = |side: &SmolStr| {
            !side.starts_with('@') && self.firsts.values().any(|members| members.contains(side))
        };
        if grouped(a) || grouped(b) {
            return None;
        }
        let members: Vec<SmolStr> = Self::members(&self.firsts, a)
            .into_iter()
            .chain(Self::members(&self.firsts, b))
            .collect();
        // the merged group takes the name of a group among them, the larger
        let name: SmolStr = [a, b]
            .into_iter()
            .filter_map(|s| s.strip_prefix('@'))
            .max_by_key(|n| self.firsts.get(*n).map(Vec::len).unwrap_or(0))
            .map(SmolStr::from)
            .unwrap_or_else(|| a.clone());
        let mut merged = View {
            masters: self.masters.clone(),
            firsts: self.firsts.clone(),
            seconds: self.seconds.clone(),
            turned: self.turned,
        };
        for side in [a, b] {
            if let Some(group) = side.strip_prefix('@') {
                merged.firsts.shift_remove(group);
            }
        }
        merged.firsts.insert(name.clone(), members.clone());
        let new_key = Self::key(&name);
        for kerning in merged.masters.iter_mut() {
            let moved: Vec<((SmolStr, SmolStr), i16)> = kerning
                .iter()
                .filter(|((first, _), _)| first == a || first == b)
                .map(|(k, v)| (k.clone(), *v))
                .collect();
            for ((first, second), value) in moved {
                kerning.shift_remove(&(first, second.clone()));
                kerning.entry((new_key.clone(), second)).or_insert(value);
            }
        }
        // every pair of a member and a glyph either side kerns
        let seconds: BTreeSet<SmolStr> = self
            .masters
            .iter()
            .chain(&merged.masters)
            .flat_map(|k| k.keys())
            .filter(|(first, _)| first == a || first == b || *first == new_key)
            .flat_map(|(_, second)| Self::members(&self.seconds, second))
            .collect();
        let (first_before, second_of) =
            (Self::group_of(&self.firsts), Self::group_of(&self.seconds));
        let first_after = Self::group_of(&merged.firsts);
        for (before, after) in self.masters.iter().zip(&merged.masters) {
            for first in &members {
                for second in &seconds {
                    if info.apart(first, second) {
                        continue;
                    }
                    let then = self.value(before, &first_before, &second_of, first, second);
                    let now = merged.value(after, &first_after, &second_of, first, second);
                    if then != now {
                        return None;
                    }
                }
            }
        }
        *self = merged;
        Some(new_key)
    }

    /// Puts the first sides a compiler split by script back together
    fn merge(&mut self, info: &GlyphScripts) -> usize {
        // the pieces: groups, and glyphs kerned outside any group (a group
        // member's own pairs are exceptions to its group's)
        let grouped: HashSet<SmolStr> = self.firsts.values().flatten().cloned().collect();
        let mut rows = self.rows();
        rows.retain(|side, _| side.starts_with('@') || !grouped.contains(side));
        let scripts: BTreeMap<SmolStr, BTreeSet<SmolStr>> = rows
            .keys()
            .map(|side| (side.clone(), info.of(&Self::members(&self.firsts, side))))
            .collect();
        // pairs of first sides of different scripts that kern alike wherever
        // both kern, by how many second sides they share
        let mut by_second: BTreeMap<&SmolStr, Vec<&SmolStr>> = BTreeMap::new();
        for (first, row) in &rows {
            for second in row.keys() {
                by_second.entry(second).or_default().push(first);
            }
        }
        let mut shared: BTreeMap<(&SmolStr, &SmolStr), usize> = BTreeMap::new();
        for firsts in by_second.values() {
            for (i, a) in firsts.iter().enumerate() {
                for b in &firsts[i + 1..] {
                    *shared.entry((*a, *b)).or_default() += 1;
                }
            }
        }
        let mut candidates: Vec<(usize, &SmolStr, &SmolStr)> = shared
            .into_iter()
            .filter(|((a, b), _)| {
                scripts
                    .get(*a)
                    .is_some_and(|s| scripts.get(*b).is_some_and(|t| s.is_disjoint(t)))
            })
            .filter(|((a, b), _)| {
                let (ra, rb) = (&rows[*a], &rows[*b]);
                ra.iter()
                    .all(|(second, values)| rb.get(second).is_none_or(|v| v == values))
            })
            .map(|((a, b), count)| (count, a, b))
            .collect();
        candidates.sort_by(|x, y| y.0.cmp(&x.0).then_with(|| (x.1, x.2).cmp(&(y.1, y.2))));
        // a side merged goes by its group's name from then on
        let mut now_in: HashMap<SmolStr, SmolStr> = HashMap::new();
        let mut merged_scripts = scripts.clone();
        let mut merges = 0;
        for (_, a, b) in candidates {
            let resolve = |side: &SmolStr| {
                let mut side = side.clone();
                while let Some(next) = now_in.get(&side) {
                    side = next.clone();
                }
                side
            };
            let (a, b) = (resolve(a), resolve(b));
            if a == b {
                continue;
            }
            let (Some(sa), Some(sb)) = (merged_scripts.get(&a), merged_scripts.get(&b)) else {
                continue;
            };
            // pieces of one group are of different scripts, every one
            if !sa.is_disjoint(sb) {
                continue;
            }
            let union: BTreeSet<SmolStr> = sa.union(sb).cloned().collect();
            if let Some(name) = self.try_merge(&a, &b, info) {
                for side in [&a, &b] {
                    if *side != name {
                        now_in.insert(side.clone(), name.clone());
                    }
                }
                merged_scripts.insert(name, union);
                merges += 1;
            }
        }
        merges
    }

    fn turn(&mut self) {
        for kerning in self.masters.iter_mut() {
            *kerning = kerning.drain(..).map(|((a, b), v)| ((b, a), v)).collect();
        }
        std::mem::swap(&mut self.firsts, &mut self.seconds);
        self.turned = !self.turned;
    }
}

/// Merges the kerning groups a compiler split by script, on both sides
pub(crate) fn merge_split_groups(font: &mut Font, info: &GlyphScripts) {
    let mut view = View {
        masters: font.masters.iter().map(|m| m.kerning.clone()).collect(),
        firsts: font.first_kern_groups.clone(),
        seconds: font.second_kern_groups.clone(),
        turned: false,
    };
    let firsts = view.merge(info);
    view.turn();
    let seconds = view.merge(info);
    view.turn();
    if firsts + seconds > 0 {
        log::info!(
            "Merged {firsts} first and {seconds} second kerning group pieces a compiler split by script"
        );
    }
    for (master, kerning) in font.masters.iter_mut().zip(view.masters) {
        master.kerning = kerning;
    }
    font.first_kern_groups = view.firsts;
    font.second_kern_groups = view.seconds;
}

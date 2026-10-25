use crate::Features;
use fea_rs_ast::{
    AsFea as _, Comment, FeatureFile, GlyphClass, GlyphContainer, GlyphName, LayoutVisitor,
    Statement, SubOrPos,
};
use smol_str::SmolStr;
use std::{
    collections::{HashMap, HashSet},
    ops::Range,
    sync::LazyLock,
};

use crate::filters::FontFilter;
use crate::layout::expand::single_subst_pairs;

/// A filter that subsets the layout features of a font to only include specified glyphs
pub struct SubsetLayout(Vec<SmolStr>);

impl SubsetLayout {
    /// Create a new SubsetLayout filter
    pub fn new<T: Into<SmolStr>>(glyphs: Vec<T>) -> Self {
        SubsetLayout(glyphs.into_iter().map(|g| g.into()).collect())
    }
}

impl FontFilter for SubsetLayout {
    fn apply(&self, font: &mut crate::Font) -> Result<(), crate::BabelfontError> {
        let old_glyphs = font
            .glyphs
            .iter()
            .map(|g| g.name.as_str())
            .collect::<Vec<_>>();
        let new_glyphs = self
            .0
            .iter()
            .filter(|glyph| old_glyphs.contains(&glyph.as_str()))
            .map(|s| s.as_str())
            .collect::<Vec<_>>();
        let features = font.features.to_fea();
        let mut feature_file: FeatureFile =
            FeatureFile::new_from_fea(&features, Some(&old_glyphs), font.source.clone())?;
        let glyph_set: HashSet<&str> = new_glyphs.iter().copied().collect();
        let mut visitor = SubsetVisitor::new(glyph_set);
        visitor.visit(&mut feature_file).map_err(|e| {
            crate::BabelfontError::FilterError(format!("Error during feature subsetting: {}", e))
        })?;
        font.features = Features::from_fea(&feature_file.as_fea(""));
        Ok(())
    }

    fn from_str(s: &str) -> Result<Self, crate::BabelfontError>
    where
        Self: Sized,
    {
        let glyphs = s.split(',').map(|s| SmolStr::new(s.trim())).collect();
        Ok(SubsetLayout(glyphs))
    }
    #[cfg(feature = "cli")]
    fn arg() -> clap::Arg {
        clap::Arg::new("subsetlayout")
            .long("subset-layout")
            .value_name("GLYPHS")
            .help("Subset layout features to only include specified glyphs (comma-separated list)")
    }
}

/// The glyph mapping of a single substitution after subsetting.
enum SingleMapping {
    /// The two sides could not be paired; leave the rule as it is.
    Unpaired,
    /// No pair survives; drop the rule.
    Empty,
    /// The surviving source and target sides.
    Kept(Vec<GlyphContainer>, Vec<GlyphContainer>),
}

static DELETION_COMMENT: LazyLock<Statement> = std::sync::LazyLock::new(|| {
    Statement::Comment(Comment::new(
        "# Removed statement due to no glyphs remaining".to_string(),
    ))
});
struct SubsetVisitor<'a> {
    glyphs: HashSet<&'a str>,
    dropped_lookups: HashSet<SmolStr>,
    dropped_features: HashSet<String>,
    empty_classes: HashSet<String>,
    original_class_definitions: HashMap<SmolStr, Vec<SmolStr>>,
}
impl<'a> SubsetVisitor<'a> {
    fn new(glyphs: HashSet<&'a str>) -> Self {
        Self {
            glyphs,
            dropped_lookups: HashSet::new(),
            dropped_features: HashSet::new(),
            empty_classes: HashSet::new(),
            original_class_definitions: HashMap::new(),
        }
    }

    fn expand_glyph_container(&self, gc: &GlyphContainer) -> Vec<SmolStr> {
        // Expand original glyphs recursively
        let mut todo = vec![gc.clone()];
        let mut original_glyphs = vec![];
        while let Some(container) = todo.pop() {
            match container {
                GlyphContainer::GlyphName(glyph_name) => {
                    original_glyphs.push(glyph_name.name.clone());
                }
                GlyphContainer::GlyphClassName(mut class_name) => {
                    if class_name.starts_with("@") {
                        class_name = class_name[1..].into();
                    }
                    if let Some(definition) = self.original_class_definitions.get(&class_name) {
                        for glyph in definition.iter().rev() {
                            todo.push(GlyphContainer::GlyphName(GlyphName::new(glyph)));
                        }
                    } else {
                        log::warn!(
                            "Warning: no definition found for glyph class {}",
                            class_name
                        );
                    }
                }
                GlyphContainer::GlyphClass(glyph_class) => {
                    for gc in glyph_class.glyphs.iter().rev() {
                        todo.push(gc.clone());
                    }
                }
                GlyphContainer::GlyphNameOrRange(name) => {
                    // I'm just going to treat it as a glyph name for now
                    original_glyphs.push(name.clone());
                }
                GlyphContainer::GlyphRange(range) => {
                    for glyph in range.glyphset() {
                        original_glyphs.push(glyph);
                    }
                }
            }
        }
        original_glyphs
    }

    /// Subset the glyph mapping shared by single and reverse-chain single
    /// substitutions, keeping only pairs whose source and target both survive.
    fn subset_single_mapping(
        &self,
        glyphs: &[GlyphContainer],
        replacements: &[GlyphContainer],
        location: &Range<usize>,
    ) -> SingleMapping {
        // Look into class definitions, then pair glyph -> replacement.
        let mapping_from: Vec<SmolStr> = glyphs
            .iter()
            .flat_map(|gc| self.expand_glyph_container(gc))
            .collect();
        let mapping_to: Vec<SmolStr> = replacements
            .iter()
            .flat_map(|gc| self.expand_glyph_container(gc))
            .collect();
        let Some(mapping) = single_subst_pairs(mapping_from, mapping_to) else {
            return SingleMapping::Unpaired;
        };
        let kept: Vec<(SmolStr, SmolStr)> = mapping
            .into_iter()
            .filter(|(glyph, replacement)| {
                self.glyphs.contains(glyph.as_str()) && self.glyphs.contains(replacement.as_str())
            })
            .collect();
        let name = |glyph: &SmolStr| GlyphContainer::GlyphName(GlyphName::new(glyph.as_str()));
        let class = |glyphs: Vec<GlyphContainer>| {
            GlyphContainer::GlyphClass(GlyphClass::new(glyphs, location.clone()))
        };
        match kept.as_slice() {
            [] => SingleMapping::Empty,
            [(glyph, replacement)] => SingleMapping::Kept(vec![name(glyph)], vec![name(replacement)]),
            [(_, first), rest @ ..] if rest.iter().all(|(_, r)| r == first) => {
                // Keep the class-to-glyph form: sub [a b] by x;
                let from = kept.iter().map(|(glyph, _)| name(glyph)).collect();
                SingleMapping::Kept(vec![class(from)], vec![name(first)])
            }
            _ => {
                let (from, to) = kept.iter().map(|(g, r)| (name(g), name(r))).unzip();
                SingleMapping::Kept(vec![class(from)], vec![class(to)])
            }
        }
    }

    fn subset_single_subst(
        &self,
        statement: &mut fea_rs_ast::SingleSubstStatement,
    ) -> Option<Statement> {
        for vec_container in [statement.prefix.iter_mut(), statement.suffix.iter_mut()] {
            for container in vec_container {
                if !self.filter_container(container) {
                    return Some(DELETION_COMMENT.clone());
                }
            }
        }
        let (glyphs, replacement) =
            match self.subset_single_mapping(
                &statement.glyphs,
                &statement.replacement,
                &statement.location,
            ) {
                SingleMapping::Unpaired => return None,
                SingleMapping::Empty => return Some(DELETION_COMMENT.clone()),
                SingleMapping::Kept(glyphs, replacement) => (glyphs, replacement),
            };
        Some(Statement::SingleSubst(fea_rs_ast::SingleSubstStatement {
            prefix: statement.prefix.clone(),
            suffix: statement.suffix.clone(),
            glyphs,
            replacement,
            location: statement.location.clone(),
            force_chain: statement.force_chain,
        }))
    }
    fn subset_multiple_subst(
        &self,
        statement: &mut fea_rs_ast::MultipleSubstStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.glyph) {
            return Some(DELETION_COMMENT.clone());
        }

        for vec_container in [
            statement.replacement.iter_mut(),
            statement.prefix.iter_mut(),
            statement.suffix.iter_mut(),
        ] {
            for container in vec_container {
                if !self.filter_container(container) {
                    return Some(DELETION_COMMENT.clone());
                }
            }
        }
        None
    }
    fn subset_alternate_subst(
        &self,
        statement: &mut fea_rs_ast::AlternateSubstStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.glyph) {
            return Some(DELETION_COMMENT.clone());
        }
        if !self.filter_container(&mut statement.replacement) {
            return Some(DELETION_COMMENT.clone());
        }

        for vec_container in [statement.prefix.iter_mut(), statement.suffix.iter_mut()] {
            for container in vec_container {
                if !self.filter_container(container) {
                    return Some(DELETION_COMMENT.clone());
                }
            }
        }
        None
    }
    fn subset_ligature_subst(
        &self,
        statement: &mut fea_rs_ast::LigatureSubstStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.replacement) {
            return Some(DELETION_COMMENT.clone());
        }
        for vec_container in [
            statement.glyphs.iter_mut(),
            statement.prefix.iter_mut(),
            statement.suffix.iter_mut(),
        ] {
            for container in vec_container {
                if !self.filter_container(container) {
                    return Some(DELETION_COMMENT.clone());
                }
            }
        }
        None
    }
    fn subset_reverse_chain_single_subst(
        &self,
        statement: &mut fea_rs_ast::ReverseChainSingleSubstStatement,
    ) -> Option<Statement> {
        for vec_container in [statement.prefix.iter_mut(), statement.suffix.iter_mut()] {
            for container in vec_container {
                if !self.filter_container(container) {
                    return Some(DELETION_COMMENT.clone());
                }
            }
        }
        let (glyphs, replacements) =
            match self.subset_single_mapping(
                &statement.glyphs,
                &statement.replacements,
                &statement.location,
            ) {
                SingleMapping::Unpaired => return None,
                SingleMapping::Empty => return Some(DELETION_COMMENT.clone()),
                SingleMapping::Kept(glyphs, replacements) => (glyphs, replacements),
            };
        Some(Statement::ReverseChainSubst(
            fea_rs_ast::ReverseChainSingleSubstStatement {
                prefix: statement.prefix.clone(),
                suffix: statement.suffix.clone(),
                glyphs,
                replacements,
                location: statement.location.clone(),
            },
        ))
    }
    fn subset_single_pos(
        &self,
        statement: &mut fea_rs_ast::SinglePosStatement,
    ) -> Option<Statement> {
        for vec_container in [statement.prefix.iter_mut(), statement.suffix.iter_mut()] {
            for container in vec_container {
                if !self.filter_container(container) {
                    return Some(DELETION_COMMENT.clone());
                }
            }
        }
        for (container, _vr) in statement.pos.iter_mut() {
            if !self.filter_container(container) {
                return Some(DELETION_COMMENT.clone());
            }
        }
        None
    }
    fn subset_pair_pos(&self, statement: &mut fea_rs_ast::PairPosStatement) -> Option<Statement> {
        if !self.filter_container(&mut statement.glyphs_1) {
            return Some(DELETION_COMMENT.clone());
        }
        if !self.filter_container(&mut statement.glyphs_2) {
            return Some(DELETION_COMMENT.clone());
        }
        None
    }
    fn subset_cursive_pos(
        &self,
        statement: &mut fea_rs_ast::CursivePosStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.glyphclass) {
            return Some(DELETION_COMMENT.clone());
        }
        None
    }
    fn subset_mark_base_pos(
        &self,
        statement: &mut fea_rs_ast::MarkBasePosStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.base) {
            return Some(DELETION_COMMENT.clone());
        }
        None
    }
    fn subset_mark_lig_pos(
        &self,
        statement: &mut fea_rs_ast::MarkLigPosStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.ligatures) {
            return Some(DELETION_COMMENT.clone());
        }
        None
    }
    fn subset_mark_mark_pos(
        &self,
        statement: &mut fea_rs_ast::MarkMarkPosStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.base_marks) {
            return Some(DELETION_COMMENT.clone());
        }
        None
    }
    fn subset_chained_context<T: SubOrPos>(
        &self,
        statement: &mut fea_rs_ast::ChainedContextStatement<T>,
    ) -> Option<Statement> {
        for vec_container in [
            statement.prefix.iter_mut(),
            statement.suffix.iter_mut(),
            statement.glyphs.iter_mut(),
        ] {
            for container in vec_container {
                if !self.filter_container(container) {
                    return Some(DELETION_COMMENT.clone());
                }
            }
        }
        // If any of the lookups have been dropped, drop this statement too
        for lookupset in statement.lookups.iter() {
            for lookup in lookupset {
                if self.dropped_lookups.contains(lookup) {
                    return Some(DELETION_COMMENT.clone());
                }
            }
        }
        None
    }
    fn subset_ignore<T: SubOrPos>(
        &self,
        statement: &mut fea_rs_ast::IgnoreStatement<T>,
    ) -> Option<Statement> {
        let mut new_context = vec![];
        for context in statement.chain_contexts.iter_mut() {
            let mut include = true;
            for vec_container in [
                context.0.iter_mut(),
                context.1.iter_mut(),
                context.2.iter_mut(),
            ] {
                for container in vec_container {
                    if !self.filter_container(container) {
                        include = false;
                    }
                }
            }
            if include {
                new_context.push(context.clone());
            }
        }
        if new_context.is_empty() {
            return Some(DELETION_COMMENT.clone());
        }
        statement.chain_contexts = new_context;
        None
    }

    fn subset_mark_class_definition(
        &self,
        statement: &mut fea_rs_ast::MarkClassDefinition,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.glyphs) {
            return Some(Statement::Comment(Comment::new(format!(
                "# Removed mark class definition {} due to no glyphs remaining",
                statement.mark_class.name
            ))));
        }
        None
    }
    fn subset_glyph_class_definition(
        &mut self,
        statement: &mut fea_rs_ast::GlyphClassDefinition,
    ) -> Option<Statement> {
        // Store the original class definition
        let name = statement.name.clone();

        let original_glyphs = statement
            .glyphs
            .glyphs
            .iter()
            .flat_map(|gc| self.expand_glyph_container(gc))
            .collect();
        self.original_class_definitions
            .insert(name.into(), original_glyphs);

        statement
            .glyphs
            .glyphs
            .retain_mut(|container| self.filter_container(container));

        if statement.glyphs.glyphs.is_empty() {
            self.empty_classes.insert("@".to_string() + &statement.name);
            return Some(Statement::Comment(Comment::new(format!(
                "# Removed glyph class {} due to no glyphs remaining",
                statement.name
            ))));
        }
        None
    }
    fn subset_gdef_class_definition(
        &mut self,
        statement: &mut fea_rs_ast::GlyphClassDefStatement,
    ) -> Option<Statement> {
        let _ = statement
            .base_glyphs
            .iter_mut()
            .map(|x| self.filter_container(x));
        let _ = statement
            .mark_glyphs
            .iter_mut()
            .map(|container| self.filter_container(container));
        let _ = statement
            .ligature_glyphs
            .iter_mut()
            .map(|container| self.filter_container(container));
        let _ = statement
            .component_glyphs
            .iter_mut()
            .map(|container| self.filter_container(container));

        None
    }
    fn subset_gdef_attach(
        &mut self,
        statement: &mut fea_rs_ast::AttachStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.glyphs) {
            return Some(Statement::Comment(Comment::new(
                "# Removed GDEF attach statement due to no glyphs remaining".to_string(),
            )));
        }
        None
    }
    fn subset_gdef_ligature_caret_by_index(
        &mut self,
        statement: &mut fea_rs_ast::LigatureCaretByIndexStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.glyphs) {
            return Some(Statement::Comment(Comment::new(
                "# Removed GDEF ligature caret by index statement due to no glyphs remaining"
                    .to_string(),
            )));
        }
        None
    }
    fn subset_gdef_ligature_caret_by_pos(
        &mut self,
        statement: &mut fea_rs_ast::LigatureCaretByPosStatement,
    ) -> Option<Statement> {
        if !self.filter_container(&mut statement.glyphs) {
            return Some(Statement::Comment(Comment::new(
                "# Removed GDEF ligature caret by pos statement due to no glyphs remaining"
                    .to_string(),
            )));
        }
        None
    }
    fn subset_lookupflag(
        &mut self,
        lookupflag: &mut fea_rs_ast::LookupFlagStatement,
    ) -> Option<Statement> {
        if let Some(GlyphContainer::GlyphClassName(ma)) = lookupflag.mark_attachment.as_ref() {
            // If the mark classes departed, replace with [] literal
            if self.empty_classes.contains(ma.as_str()) {
                lookupflag.mark_attachment = Some(GlyphContainer::GlyphClass(GlyphClass::new(
                    vec![],
                    lookupflag.location.clone(),
                )))
            }
        }
        // Same trick for mark filtering
        if let Some(GlyphContainer::GlyphClassName(ma)) = lookupflag.mark_filtering_set.as_ref() {
            if self.empty_classes.contains(ma.as_str()) {
                lookupflag.mark_filtering_set = Some(GlyphContainer::GlyphClass(GlyphClass::new(
                    vec![],
                    lookupflag.location.clone(),
                )))
            }
        }
        None
    }
    fn subset_feature_block(
        &mut self,
        feature_block: &mut fea_rs_ast::FeatureBlock,
    ) -> Option<Statement> {
        feature_block
            .statements
            .retain(|statement| statement != &*DELETION_COMMENT);
        if feature_block.statements.iter().any(non_trivial_statement) {
            return None;
        }
        self.dropped_features.insert(feature_block.name.to_string());
        Some(Statement::Comment(Comment::new(format!(
            "# Removed feature {} due to no statements remaining",
            feature_block.name
        ))))
    }
    fn subset_lookup_block(
        &mut self,
        lookup_block: &mut fea_rs_ast::LookupBlock,
    ) -> Option<Statement> {
        lookup_block
            .statements
            .retain(|statement| statement != &*DELETION_COMMENT);
        if lookup_block.statements.iter().any(non_trivial_statement) {
            return None;
        }
        self.dropped_lookups.insert(lookup_block.name.clone());
        Some(Statement::Comment(Comment::new(format!(
            "# Removed lookup {} due to no statements remaining",
            lookup_block.name
        ))))
    }
    fn subset_feature_reference(
        &mut self,
        feature_reference: &mut fea_rs_ast::FeatureReferenceStatement,
    ) -> Option<Statement> {
        if self
            .dropped_features
            .contains(&feature_reference.feature_name)
        {
            return Some(Statement::Comment(Comment::new(format!(
                "# Removed feature reference to {} due to feature being dropped",
                feature_reference.feature_name
            ))));
        }
        None
    }
    fn subset_lookup_reference(
        &mut self,
        lookup_reference: &mut fea_rs_ast::LookupReferenceStatement,
    ) -> Option<Statement> {
        if self
            .dropped_lookups
            .contains(&SmolStr::from(lookup_reference.lookup_name.clone()))
        {
            return Some(Statement::Comment(Comment::new(format!(
                "# Removed lookup reference to {} due to lookup being dropped",
                lookup_reference.lookup_name
            ))));
        }
        None
    }
    fn subset_nested_block(
        &mut self,
        nested_block: &mut fea_rs_ast::NestedBlock,
    ) -> Option<Statement> {
        nested_block
            .statements
            .retain(|statement| statement != &*DELETION_COMMENT);
        // A featureNames (or sizemenuname) block is nothing BUT name records;
        // they are its payload, not scaffolding, and subsetting glyphs is no
        // reason to lose the font's UI strings.
        if nested_block
            .statements
            .iter()
            .any(|st| non_trivial_statement(st) || matches!(st, Statement::FeatureNameStatement(_)))
        {
            return None;
        }
        Some(Statement::Comment(Comment::new(
            "# Removed nested block due to no statements remaining".to_string(),
        )))
    }
    fn filter_container(&self, container: &mut fea_rs_ast::GlyphContainer) -> bool {
        match container {
            fea_rs_ast::GlyphContainer::GlyphName(glyph_name) => {
                self.glyphs.contains(glyph_name.name.as_str())
            }
            fea_rs_ast::GlyphContainer::GlyphClass(glyph_class) => {
                glyph_class
                    .glyphs
                    .retain_mut(|gc| self.filter_container(gc));
                !glyph_class.glyphs.is_empty()
            }
            fea_rs_ast::GlyphContainer::GlyphClassName(smol_str) => {
                !self.empty_classes.contains(smol_str.as_str())
            }
            fea_rs_ast::GlyphContainer::GlyphRange(range) => {
                *container = fea_rs_ast::GlyphContainer::GlyphClass(fea_rs_ast::GlyphClass::new(
                    range
                        .glyphset()
                        .map(|x| fea_rs_ast::GlyphContainer::GlyphName(GlyphName::new(&x)))
                        .collect(),
                    0..0, // Oops, we don't know
                ));
                self.filter_container(container)
            }
            fea_rs_ast::GlyphContainer::GlyphNameOrRange(smol_str) => {
                if self.glyphs.contains(smol_str.as_str()) {
                    return true;
                }
                // try interpreting as range
                todo!();
            }
        }
    }
}

fn non_trivial_statement(statement: &Statement) -> bool {
    // A lookup reference is a block's payload, not scaffolding: a feature block
    // whose rules all live in referenced lookups does something. References to
    // lookups that were dropped have already been turned into comments by
    // subset_lookup_reference, so any reference still here points at a live
    // lookup and must keep its block alive.
    !matches!(
        statement,
        Statement::Comment(_)
            | Statement::FeatureNameStatement(_)
            | Statement::FontRevision(_)
            | Statement::FeatureReference(_)
            | Statement::Language(_)
            | Statement::LanguageSystem(_)
            | Statement::LookupFlag(_)
            | Statement::SizeParameters(_)
            | Statement::SizeMenuName(_)
            | Statement::Subtable(_)
            | Statement::Script(_)
            | Statement::Head(_)
    )
}
impl LayoutVisitor for SubsetVisitor<'_> {
    fn depth_first(&self) -> bool {
        true
    }
    fn visit_statement(&mut self, statement: &mut Statement) -> bool {
        if let Some(rewritten) = match statement {
            Statement::SingleSubst(single_subst_statement) => {
                self.subset_single_subst(single_subst_statement)
            }
            Statement::MultipleSubst(multiple_subst_statement) => {
                self.subset_multiple_subst(multiple_subst_statement)
            }
            Statement::AlternateSubst(alternate_subst_statement) => {
                self.subset_alternate_subst(alternate_subst_statement)
            }
            Statement::LigatureSubst(ligature_subst_statement) => {
                self.subset_ligature_subst(ligature_subst_statement)
            }
            Statement::ReverseChainSubst(reverse_chain_single_subst_statement) => {
                self.subset_reverse_chain_single_subst(reverse_chain_single_subst_statement)
            }
            Statement::ChainedContextSubst(chained_context_statement) => {
                self.subset_chained_context(chained_context_statement)
            }
            Statement::IgnoreSubst(ignore_statement) => self.subset_ignore(ignore_statement),
            Statement::SinglePos(single_pos_statement) => {
                self.subset_single_pos(single_pos_statement)
            }
            Statement::PairPos(pair_pos_statement) => self.subset_pair_pos(pair_pos_statement),
            Statement::CursivePos(cursive_pos_statement) => {
                self.subset_cursive_pos(cursive_pos_statement)
            }
            Statement::MarkBasePos(mark_base_pos_statement) => {
                self.subset_mark_base_pos(mark_base_pos_statement)
            }
            Statement::MarkLigPos(mark_lig_pos_statement) => {
                self.subset_mark_lig_pos(mark_lig_pos_statement)
            }
            Statement::MarkMarkPos(mark_mark_pos_statement) => {
                self.subset_mark_mark_pos(mark_mark_pos_statement)
            }
            Statement::ChainedContextPos(chained_context_statement) => {
                self.subset_chained_context(chained_context_statement)
            }
            Statement::IgnorePos(ignore_statement) => self.subset_ignore(ignore_statement),
            Statement::AnchorDefinition(_) => None,
            Statement::GdefAttach(attach_statement) => self.subset_gdef_attach(attach_statement),
            Statement::GdefClassDef(glyph_class_def_statement) => {
                self.subset_gdef_class_definition(glyph_class_def_statement)
            }
            Statement::GdefLigatureCaretByIndex(ligature_caret_by_index_statement) => {
                self.subset_gdef_ligature_caret_by_index(ligature_caret_by_index_statement)
            }
            Statement::GdefLigatureCaretByPos(ligature_caret_by_pos_statement) => {
                self.subset_gdef_ligature_caret_by_pos(ligature_caret_by_pos_statement)
            }
            Statement::MarkClassDefinition(mark_class_definition) => {
                self.subset_mark_class_definition(mark_class_definition)
            }
            Statement::Comment(_)
            | Statement::FeatureNameStatement(_)
            | Statement::FontRevision(_) => None,
            Statement::FeatureReference(feature_reference) => {
                self.subset_feature_reference(feature_reference)
            }
            Statement::GlyphClassDefinition(glyph_class_definition) => {
                self.subset_glyph_class_definition(glyph_class_definition)
            }
            Statement::Language(_) | Statement::LanguageSystem(_) => None,
            Statement::LookupFlag(lookupflag) => self.subset_lookupflag(lookupflag),
            Statement::LookupReference(lookup_reference) => {
                self.subset_lookup_reference(lookup_reference)
            }
            Statement::SizeParameters(_)
            | Statement::SizeMenuName(_)
            | Statement::Subtable(_)
            | Statement::Script(_) => None,
            Statement::Gdef(_) => {
                // Visitor will recurse
                None
            }
            Statement::Head(_)
            | Statement::Hhea(_)
            | Statement::Name(_)
            | Statement::Stat(_)
            | Statement::Vhea(_)
            | Statement::Os2(_)
            | Statement::Base(_) => None,
            Statement::FeatureBlock(feature_block) => self.subset_feature_block(feature_block),
            Statement::LookupBlock(lookup_block) => self.subset_lookup_block(lookup_block),
            Statement::NestedBlock(nested_block) => self.subset_nested_block(nested_block),
            Statement::ValueRecordDefinition(_) => None,
            Statement::ConditionSet(_) => None,
            Statement::VariationBlock(_) => None,
        } {
            *statement = rewritten;
            return true;
        }
        true
    }
}

#[allow(clippy::expect_used, clippy::unwrap_used)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Font, Glyph};
    use pretty_assertions::assert_eq;

    fn dummy_font_with_glyphs(glyph_names: Vec<&str>) -> Font {
        let mut font = Font::new();
        for name in glyph_names {
            font.glyphs.push(Glyph::new(name));
        }
        font
    }

    #[test]
    fn test_subset_single_subst() {
        let mut font = dummy_font_with_glyphs(vec!["a", "b", "c"]);
        font.features = Features::from_fea(
            "feature foo { sub a by c; sub b by c; } foo;\nfeature bar { sub b by a; } bar;\n",
        );
        // Now subset to a and c only
        SubsetLayout::new(vec!["a", "c"])
            .apply(&mut font)
            .expect("Feature subsetting failed");
        let fea = font.features.to_fea();
        assert_eq!(fea, "feature foo {\nsub a by c;\n} foo;\n# Removed feature bar due to no statements remaining\n\n");
    }

    #[test]
    fn test_feature_of_lookup_references_survives() {
        // The SFD convertor emits features as script/language/lookup-reference
        // triples with the rules living in lookup prefixes. None of that may be
        // dropped as trivial while the referenced lookup is alive -- that empties
        // the font's FeatureList. A reference to a dropped lookup is still cleaned
        // away, and a feature left with nothing else still goes.
        let mut font = dummy_font_with_glyphs(vec!["a", "b", "c"]);
        font.features = Features::from_fea(
            "lookup one { sub a by c; } one;\nlookup two { sub b by c; } two;\n\
             feature foo { script DFLT; language dflt; lookup one; } foo;\n\
             feature bar { script DFLT; language dflt; lookup two; } bar;\n",
        );
        // Subset away b: lookup two empties, so bar loses its only payload.
        SubsetLayout::new(vec!["a", "c"])
            .apply(&mut font)
            .expect("Feature subsetting failed");
        let fea = font.features.to_fea();
        assert!(
            fea.contains("lookup one;"),
            "foo must survive on the strength of its lookup reference:\n{fea}"
        );
        assert!(
            fea.contains("# Removed feature bar"),
            "bar's lookup was dropped, so bar must still go:\n{fea}"
        );
    }

    #[test]
    fn test_featurenames_block_survives_subsetting() {
        // featureNames blocks hold nothing but name records, which the triviality
        // list treats as scaffolding -- but they are the block's payload, and
        // subsetting glyphs is no reason to drop the font's UI strings.
        let mut font = dummy_font_with_glyphs(vec!["a", "b", "c"]);
        font.features = Features::from_fea(
            "feature ss01 { featureNames { name 3 1 1033 \"Fancy\"; }; sub a by c; } ss01;\n",
        );
        SubsetLayout::new(vec!["a", "c"])
            .apply(&mut font)
            .expect("Feature subsetting failed");
        let fea = font.features.to_fea();
        assert!(
            fea.contains("featureNames") && fea.contains("name \"Fancy\";"),
            "the name record must survive (3/1/1033 renders as the elided default):\n{fea}"
        );
    }

    #[test]
    fn test_filter_range() {
        let visitor = SubsetVisitor::new(vec!["a", "b", "g"].into_iter().collect());
        let mut container = fea_rs_ast::GlyphContainer::GlyphRange(fea_rs_ast::GlyphRange::new(
            "a".into(),
            "f".into(),
        ));

        let retained = visitor.filter_container(&mut container);
        assert!(retained);
        assert_eq!(container.as_fea(""), "[a b]");
    }

    #[test]
    fn test_multiple_subst_with_classes() {
        let mut font = dummy_font_with_glyphs(vec!["a", "b", "c", "d"]);
        font.features = Features::from_fea(
            "@before = [a b]; @after = [c d]; feature foo { sub @before by @after; } foo;\n",
        );
        // Now subset to a and c only
        SubsetLayout::new(vec!["a", "c"])
            .apply(&mut font)
            .expect("Feature subsetting failed");
        let fea = font.features.to_fea();
        assert_eq!(
            fea,
            "@before = [a];\n@after = [c];\nfeature foo {\nsub a by c;\n} foo;\n\n"
        );
    }

    #[test]
    fn test_multiple_subset_retains_classes() {
        let all_glyphs = vec![
            "heh-ar.isol",
            "heh-ar.fina",
            "hamzaabove-ar",
            "heh-ar.isol.1",
            "heh-ar.fina.1",
        ];
        let mut font = dummy_font_with_glyphs(all_glyphs.clone());
        let feature_code = "feature foo {
sub [heh-ar.isol heh-ar.fina]' hamzaabove-ar by [heh-ar.isol.1 heh-ar.fina.1];
} foo;\n";
        font.features = Features::from_fea(feature_code);
        // keep them all, just rewrite
        SubsetLayout::new(all_glyphs.clone())
            .apply(&mut font)
            .expect("Feature subsetting failed");
        // Should be same
        let fea = font.features.to_fea();
        assert_eq!(fea.trim_end(), feature_code.trim_end());
    }
    use crate::{
        close_layout,
        convertors::fontir::{BabelfontIrSource, CompilationOptions},
    };

    fn subset_fea(glyphs: Vec<&str>, fea: &str, keep: Vec<&str>) -> String {
        let mut font = dummy_font_with_glyphs(glyphs);
        font.features = Features::from_fea(fea);
        SubsetLayout::new(keep)
            .apply(&mut font)
            .expect("Feature subsetting failed");
        font.features.to_fea()
    }

    #[test]
    fn test_subset_class_to_single_glyph_keeps_every_member() {
        let fea = subset_fea(
            vec!["beh", "noon", "yeh", "beh.medi"],
            "feature medi { sub [beh noon yeh] by beh.medi; } medi;\n",
            vec!["beh", "noon", "yeh", "beh.medi"],
        );
        assert_eq!(fea, "feature medi {\nsub [beh noon yeh] by beh.medi;\n} medi;\n\n");
    }

    #[test]
    fn test_subset_class_to_single_glyph_drops_missing_members() {
        let fea = subset_fea(
            vec!["beh", "noon", "yeh", "beh.medi"],
            "feature medi { sub [beh noon yeh] by beh.medi; } medi;\n",
            vec!["noon", "yeh", "beh.medi"],
        );
        assert_eq!(fea, "feature medi {\nsub [noon yeh] by beh.medi;\n} medi;\n\n");
    }

    #[test]
    fn test_subset_class_to_single_glyph_without_target_is_removed() {
        let fea = subset_fea(
            vec!["beh", "noon", "beh.medi"],
            "feature medi { sub [beh noon] by beh.medi; } medi;\n",
            vec!["beh", "noon"],
        );
        assert!(!fea.contains("sub "), "rule should be removed: {fea}");
    }

    #[test]
    fn test_subset_class_to_class_still_pairs_by_position() {
        let fea = subset_fea(
            vec!["a", "b", "c", "x", "y", "z"],
            "feature foo { sub [a b c] by [x y z]; } foo;\n",
            vec!["a", "c", "x", "z"],
        );
        assert_eq!(fea, "feature foo {\nsub [a c] by [x z];\n} foo;\n\n");
    }

    #[test]
    fn test_subset_reverse_chain_class_to_single_glyph() {
        let fea = subset_fea(
            vec!["p", "a", "b", "c", "x"],
            "feature foo { rsub p [a b c]' by x; } foo;\n",
            vec!["p", "a", "c", "x"],
        );
        assert!(fea.contains("[a c]"), "class members dropped: {fea}");
        assert!(fea.contains("by x;"), "target dropped: {fea}");
    }

    #[test]
    fn test_fustat_medial_yeh_survives_subsetting() {
        // Yeh decomposes to alefMaksura-ar in ccmp; medi then maps it via
        // `sub [behDotless-ar noonghunna-ar alefMaksura-ar] by behDotless-ar.medi;`
        let mut font = crate::load("resources/Fustat.glyphs").unwrap();
        let seed = ["alefMaksura-ar", "behDotless-ar", "twodotshorizontalbelow-ar"];
        let new_glyphset =
            close_layout(&font, seed.iter().map(|s| (*s).into()).collect()).unwrap();
        assert!(new_glyphset.contains("behDotless-ar.medi"));
        SubsetLayout::new(new_glyphset.into_iter().collect())
            .apply(&mut font)
            .expect("Feature subsetting failed");
        let fea = font.features.to_fea();
        assert!(
            fea.lines().any(|line| line.trim_start().starts_with("sub ")
                && line.contains("alefMaksura-ar")
                && line.trim_end().ends_with("by behDotless-ar.medi;")),
            "alefMaksura-ar -> behDotless-ar.medi was dropped"
        );
        BabelfontIrSource::compile(font, CompilationOptions::default()).unwrap();
    }

    #[test]
    fn test_fustat_subset() {
        let mut font = crate::load("resources/Fustat.glyphs").unwrap();
        let subset = [
            "fathatan-ar",
            "alef-ar.short.fina",
            "dotbelow-ar",
            "behDotless-ar.medi",
            "fatha-ar",
            "hah-ar.init",
            "reh-ar.fina",
            "meem-ar.init",
        ];
        // Perform layout closure
        let new_glyphset =
            close_layout(&font, subset.iter().map(|s| (*s).into()).collect()).unwrap();
        // Now subset to that glyphset
        SubsetLayout::new(new_glyphset.into_iter().collect())
            .apply(&mut font)
            .expect("Feature subsetting failed");
        // Just check that the resulting fea compiles
        BabelfontIrSource::compile(font, CompilationOptions::default()).unwrap();
    }
}

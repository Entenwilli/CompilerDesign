use tracing::trace;

use crate::{
    parser::{ast::block_tree::BlockTree, symbols::Name, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState, DeclarationStatus},
};

impl SemanticAnalysis for BlockTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        trace!("Running semantic analyze on block");
        let mut old_namespace = state.namespace.clone();
        for statement in self.statements() {
            statement.analyze(state)?;
        }
        trace!("Namespace in block: {:?}", state.namespace);
        for initialized_variable in state
            .namespace
            .iter()
            .filter(|v| old_namespace.contains_key(v.0))
            .filter(|v| v.1.declaration().eq(&DeclarationStatus::Initialized))
            .map(|v| v.0)
            .collect::<Vec<&Name>>()
        {
            old_namespace
                .get_mut(initialized_variable)
                .unwrap()
                .set_initialized();
        }
        state.namespace = old_namespace;
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}

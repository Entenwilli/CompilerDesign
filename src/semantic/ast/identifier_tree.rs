use tracing::trace;

use crate::{
    parser::{ast::identifier_tree::IdentifierExpressionTree, types::Type},
    semantic::{
        ast::SemanticAnalysis, error::SemanticError, AnalysisState, DeclarationStatus, ReturnState,
    },
};

impl SemanticAnalysis for IdentifierExpressionTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        trace!("Analyzing identifier with state: {:?}", state);
        self.name().analyze(state)?;
        state
            .namespace
            .get(self.name().name())
            .ok_or(SemanticError::UndefinedVariable(self.name().clone()))?;

        if state
            .namespace
            .get(&self.name().name())
            .unwrap()
            .declaration()
            .eq(&DeclarationStatus::Declared)
            && state.return_state.ne(&ReturnState::Returning)
            && state.is_reachable()
        {
            return Err(SemanticError::UninitializedVariable(self.name().clone()));
        };
        Ok(())
    }

    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        state
            .namespace
            .get(self.name().name())
            .map(|v| v.type_status().clone())
            .ok_or(SemanticError::UndefinedVariable(self.name().clone()))
    }
}

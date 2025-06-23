use crate::{
    parser::{
        ast::{continue_tree::ContinueTree, Tree},
        types::Type,
    },
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for ContinueTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        if !state.loop_active() {
            return Err(SemanticError::ContinueOutsideLoop(self.span()));
        }
        state.set_unreachable();
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}

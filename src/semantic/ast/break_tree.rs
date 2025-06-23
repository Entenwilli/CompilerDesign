use crate::{
    parser::{
        ast::{break_tree::BreakTree, Tree},
        types::Type,
    },
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for BreakTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        if !state.loop_active() {
            return Err(SemanticError::BreakOutsideLoop(self.span()));
        }
        state.set_unreachable();
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}

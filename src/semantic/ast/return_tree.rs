use crate::{
    parser::{ast::return_tree::ReturnTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState, ReturnState},
};

impl SemanticAnalysis for ReturnTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        self.expression().analyze(state)?;
        if self.expression().r#type(state)?.ne(state.return_type()) {
            return Err(SemanticError::IncompatibleReturnType(
                self.expression().clone(),
            ));
        }
        state.return_state = ReturnState::Returning;
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}

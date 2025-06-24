use std::collections::HashMap;

use tracing::trace;

use crate::{
    parser::{ast::function_tree::FunctionTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState, ReturnState},
};

impl SemanticAnalysis for FunctionTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        trace!("Running semantic analysis on function");
        state.namespace = HashMap::new();
        self.return_type().analyze(state)?;
        state.set_return_type(self.return_type().type_tree().clone());
        self.name_tree().analyze(state)?;
        for function_parameter in self.parameters() {
            function_parameter.analyze(state)?;
        }
        self.body().analyze(state)?;
        if state.return_state.eq(&ReturnState::NotReturing) {
            return Err(SemanticError::FunctionNotReturning(self.body().clone()));
        }
        if self.name_tree().name().as_string() == "main" {
            if self.return_type().type_tree().ne(&Type::Int) {
                return Err(SemanticError::MainMustReturnInt);
            }
        }
        state.return_state = ReturnState::NotReturing;
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}

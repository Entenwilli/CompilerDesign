use crate::{
    parser::{ast::lvalue_tree::LValueTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for LValueTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        self.identifier().analyze(state)
    }

    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        state
            .namespace
            .get(self.identifier().name())
            .map(|v| v.type_status().clone())
            .ok_or(SemanticError::UndefinedVariable(self.identifier().clone()))
    }
}

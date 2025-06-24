use crate::{
    parser::{ast::call_parameter_tree::CallParameterTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for CallParameterTree {
    fn analyze(&self, _: &mut AnalysisState) -> Result<(), SemanticError> {
        Ok(())
    }

    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        self.expression().r#type(state)
    }
}

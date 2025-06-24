use crate::{
    parser::{ast::call_tree::CallTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for CallTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        let parameter_info = state
            .get_function_parameter_info(self.identifier().name().name())?
            .clone();
        if parameter_info.len() != self.parameter().len() {
            return Err(SemanticError::FunctionParameterMismatch);
        }
        for (index, defined_type) in parameter_info.iter().enumerate() {
            let actual_type = self
                .parameter()
                .get(index)
                .ok_or(SemanticError::FunctionParameterMismatch)?;
            if actual_type.r#type(state)?.ne(defined_type) {
                return Err(SemanticError::FunctionParameterMismatch);
            }
        }
        Ok(())
    }

    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        state
            .get_function_return_type(self.identifier().name().name())
            .cloned()
    }
}

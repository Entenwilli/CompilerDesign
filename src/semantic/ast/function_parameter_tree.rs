use crate::{
    parser::{ast::function_parameter_tree::FunctionParameterTree, types::Type},
    semantic::{
        ast::SemanticAnalysis, error::SemanticError, AnalysisState, DeclarationStatus,
        VariableStatus,
    },
};

impl SemanticAnalysis for FunctionParameterTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        state.namespace.insert(
            self.identifier().name().name().clone(),
            VariableStatus::new(
                self.type_tree().type_tree().clone(),
                DeclarationStatus::Initialized,
            ),
        );
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}

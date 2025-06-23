use crate::{
    parser::{ast::function_parameter_tree::FunctionParameterTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for FunctionParameterTree {
    fn analyze(&self, _state: &mut AnalysisState) -> Result<(), SemanticError> {
        unimplemented!()
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}

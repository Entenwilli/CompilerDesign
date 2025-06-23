use crate::{
    parser::{ast::call_tree::CallTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for CallTree {
    fn analyze(&self, _state: &mut AnalysisState) -> Result<(), SemanticError> {
        todo!("Function must be called with all parameters, they must have the correct type");
    }

    fn r#type(&self, _state: &mut AnalysisState) -> Result<Type, SemanticError> {
        todo!("How to get return type of calling function? Analysis State?")
    }
}

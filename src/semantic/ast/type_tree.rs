use crate::{
    parser::{ast::type_tree::TypeTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for TypeTree {
    fn analyze(&self, _: &mut AnalysisState) -> Result<(), SemanticError> {
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(self.type_tree().clone())
    }
}

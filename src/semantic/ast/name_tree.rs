use crate::{
    parser::{ast::name_tree::NameTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for NameTree {
    fn analyze(&self, _: &mut AnalysisState) -> Result<(), SemanticError> {
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}

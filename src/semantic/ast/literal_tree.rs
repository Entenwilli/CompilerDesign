use crate::{
    parser::{
        ast::literal_tree::{BooleanLiteralTree, IntegerLiteralTree},
        types::Type,
    },
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
    util::int_parsing::parse_int,
};

impl SemanticAnalysis for IntegerLiteralTree {
    fn analyze(
        &self,
        _: &mut crate::semantic::AnalysisState,
    ) -> Result<(), crate::semantic::error::SemanticError> {
        if self.base() != 16 && self.base() != 10 {
            return Err(SemanticError::LiteralInvalidBase);
        }
        parse_int(self.value().to_owned(), self.base() as u64)
            .ok_or(SemanticError::LiteralInvalid)?;
        Ok(())
    }

    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Int)
    }
}

impl SemanticAnalysis for BooleanLiteralTree {
    fn analyze(&self, _: &mut crate::semantic::AnalysisState) -> Result<(), SemanticError> {
        Ok(())
    }

    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Bool)
    }
}

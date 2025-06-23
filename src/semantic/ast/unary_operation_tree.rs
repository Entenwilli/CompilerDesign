use crate::{
    lexer::operator::UnaryOperator,
    parser::{ast::unary_operation_tree::UnaryOperationTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for UnaryOperationTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        match self.operator() {
            UnaryOperator::LogicalNot => {
                if self.expression().r#type(state)?.ne(&Type::Bool) {
                    return Err(SemanticError::IncompatibleTypeUnary(
                        self.operator().clone(),
                        self.expression().clone(),
                    ));
                }
            }
            UnaryOperator::BitwiseNot | UnaryOperator::Minus => {
                if self.expression().r#type(state)?.ne(&Type::Int) {
                    return Err(SemanticError::IncompatibleTypeUnary(
                        self.operator().clone(),
                        self.expression().clone(),
                    ));
                }
            }
        }
        self.expression().analyze(state)
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        match self.operator() {
            UnaryOperator::Minus => Ok(Type::Int),
            UnaryOperator::LogicalNot => Ok(Type::Bool),
            UnaryOperator::BitwiseNot => Ok(Type::Int),
        }
    }
}

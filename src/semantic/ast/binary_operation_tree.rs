use crate::{
    lexer::operator::BinaryOperator,
    parser::{ast::binary_operation_tree::BinaryOperationTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for BinaryOperationTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        match self.operator() {
            BinaryOperator::LogicalOr | BinaryOperator::LogicalAnd => {
                if self.lhs().r#type(state)?.ne(&Type::Bool) {
                    return Err(SemanticError::IncompatibleTypeOperation(
                        self.operator().clone(),
                        self.lhs().clone(),
                    ));
                } else if self.rhs().r#type(state)?.ne(&Type::Bool) {
                    return Err(SemanticError::IncompatibleTypeOperation(
                        self.operator().clone(),
                        self.rhs().clone(),
                    ));
                }
            }
            BinaryOperator::Minus
            | BinaryOperator::ShiftRight
            | BinaryOperator::ShiftLeft
            | BinaryOperator::BitwiseXor
            | BinaryOperator::BitwiseAnd
            | BinaryOperator::BitwiseOr
            | BinaryOperator::Plus
            | BinaryOperator::Mul
            | BinaryOperator::Mod
            | BinaryOperator::Div => {
                if self.lhs().r#type(state)?.ne(&Type::Int) {
                    return Err(SemanticError::IncompatibleTypeOperation(
                        self.operator().clone(),
                        self.lhs().clone(),
                    ));
                } else if self.rhs().r#type(state)?.ne(&Type::Int) {
                    return Err(SemanticError::IncompatibleTypeOperation(
                        self.operator().clone(),
                        self.rhs().clone(),
                    ));
                }
            }
            BinaryOperator::Lower
            | BinaryOperator::LowerEquals
            | BinaryOperator::Equals
            | BinaryOperator::NotEquals
            | BinaryOperator::Higher
            | BinaryOperator::HigherEquals => {
                let lhs_type = self.lhs().r#type(state)?;
                let rhs_type = self.rhs().r#type(state)?;
                if lhs_type.ne(&rhs_type) {
                    return Err(SemanticError::IncompatibleTypeComparison(
                        self.lhs().clone(),
                        self.rhs().clone(),
                    ));
                }
            }
        }
        self.lhs().analyze(state)?;
        self.rhs().analyze(state)
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        match self.operator() {
            BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Plus
            | BinaryOperator::Mod
            | BinaryOperator::ShiftLeft
            | BinaryOperator::ShiftRight
            | BinaryOperator::Minus => Ok(Type::Int),
            BinaryOperator::Lower
            | BinaryOperator::LowerEquals
            | BinaryOperator::Equals
            | BinaryOperator::NotEquals
            | BinaryOperator::HigherEquals
            | BinaryOperator::Higher => Ok(Type::Bool),
            BinaryOperator::BitwiseOr | BinaryOperator::BitwiseAnd | BinaryOperator::BitwiseXor => {
                Ok(Type::Int)
            }
            BinaryOperator::LogicalOr | BinaryOperator::LogicalAnd => Ok(Type::Bool),
        }
    }
}

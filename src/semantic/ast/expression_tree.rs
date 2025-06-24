use crate::{
    parser::{ast::expression_tree::ExpressionTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for ExpressionTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        match self {
            ExpressionTree::BooleanLiteralTree(tree) => tree.analyze(state),
            ExpressionTree::IntegerLiteralTree(tree) => tree.analyze(state),
            ExpressionTree::UnaryOperationTree(tree) => tree.analyze(state),
            ExpressionTree::BinaryOperationTree(tree) => tree.analyze(state),
            ExpressionTree::TernaryOperationTree(tree) => tree.analyze(state),
            ExpressionTree::IdentifierExpressionTree(tree) => tree.analyze(state),
            ExpressionTree::CallTree(tree) => tree.analyze(state),
        }
    }

    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        match self {
            ExpressionTree::BooleanLiteralTree(tree) => tree.r#type(state),
            ExpressionTree::IntegerLiteralTree(tree) => tree.r#type(state),
            ExpressionTree::UnaryOperationTree(tree) => tree.r#type(state),
            ExpressionTree::BinaryOperationTree(tree) => tree.r#type(state),
            ExpressionTree::TernaryOperationTree(tree) => tree.r#type(state),
            ExpressionTree::IdentifierExpressionTree(tree) => tree.r#type(state),
            ExpressionTree::CallTree(tree) => tree.r#type(state),
        }
    }
}

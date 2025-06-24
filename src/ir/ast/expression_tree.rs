use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::expression_tree::ExpressionTree,
};

use super::IRConstructor;

impl ToIR for ExpressionTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        match self {
            ExpressionTree::BooleanLiteralTree(tree) => tree.to_ir(constructor),
            ExpressionTree::IntegerLiteralTree(tree) => tree.to_ir(constructor),
            ExpressionTree::IdentifierExpressionTree(tree) => tree.to_ir(constructor),
            ExpressionTree::CallTree(tree) => tree.to_ir(constructor),
            ExpressionTree::UnaryOperationTree(tree) => tree.to_ir(constructor),
            ExpressionTree::BinaryOperationTree(tree) => tree.to_ir(constructor),
            ExpressionTree::TernaryOperationTree(tree) => tree.to_ir(constructor),
        }
    }
}

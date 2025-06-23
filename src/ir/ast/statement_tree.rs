use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::statement_tree::{ControlStatementTree, SimpleStatementTree, StatementTree},
};

use super::IRConstructor;

impl ToIR for StatementTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        match self {
            StatementTree::BlockStatement(tree) => tree.to_ir(constructor),
            StatementTree::SimpleStatement(tree) => tree.to_ir(constructor),
            StatementTree::ControlStatement(tree) => tree.to_ir(constructor),
        }
    }
}

impl ToIR for SimpleStatementTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        match self {
            SimpleStatementTree::CallTree(tree) => tree.to_ir(constructor),
            SimpleStatementTree::AssignmentTree(tree) => tree.to_ir(constructor),
            SimpleStatementTree::DeclerationTree(tree) => tree.to_ir(constructor),
        }
    }
}

impl ToIR for ControlStatementTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        match self {
            ControlStatementTree::IfTree(tree) => tree.to_ir(constructor),
            ControlStatementTree::WhileTree(tree) => tree.to_ir(constructor),
            ControlStatementTree::ForTree(tree) => tree.to_ir(constructor),
            ControlStatementTree::BreakTree(tree) => tree.to_ir(constructor),
            ControlStatementTree::ContinueTree(tree) => tree.to_ir(constructor),
            ControlStatementTree::ReturnTree(tree) => tree.to_ir(constructor),
        }
    }
}

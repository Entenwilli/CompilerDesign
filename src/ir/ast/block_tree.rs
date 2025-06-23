use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::{
        block_tree::BlockTree,
        statement_tree::{ControlStatementTree, StatementTree},
    },
};

use super::IRConstructor;

impl ToIR for BlockTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        for statement in self.statements() {
            if let StatementTree::ControlStatement(control_statement) = statement {
                if let ControlStatementTree::ReturnTree(return_tree) = control_statement {
                    return_tree.to_ir(constructor);
                    break;
                }
            }
            statement.to_ir(constructor);
        }
        None
    }
}

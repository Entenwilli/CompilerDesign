use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::identifier_tree::IdentifierExpressionTree,
};

use super::IRConstructor;

impl ToIR for IdentifierExpressionTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        let value =
            constructor.read_variable(self.name().name().clone(), constructor.current_block());
        Some(value)
    }
}

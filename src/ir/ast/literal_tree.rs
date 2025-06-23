use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::literal_tree::{BooleanLiteralTree, IntegerLiteralTree},
    util::int_parsing::parse_int,
};

use super::IRConstructor;

impl ToIR for IntegerLiteralTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        let value = parse_int(self.value().to_owned(), self.base() as u64)?;
        let node = constructor.create_constant_int(value);
        Some(node)
    }
}

impl ToIR for BooleanLiteralTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        let node = if self.value() {
            constructor.create_constant_int(1)
        } else {
            constructor.create_constant_int(0)
        };
        Some(node)
    }
}

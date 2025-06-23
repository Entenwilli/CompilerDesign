use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::return_tree::ReturnTree,
};

use super::IRConstructor;

impl ToIR for ReturnTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        let node = self
            .expression()
            .to_ir(constructor)
            .expect("Return must have a expression");
        constructor.create_return(node);
        None
    }
}

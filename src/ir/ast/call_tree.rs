use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::call_tree::CallTree,
};

use super::IRConstructor;

impl ToIR for CallTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        //TODO: Unimplemented
        let temp = constructor.create_constant_int(0);
        Some(temp)
    }
}

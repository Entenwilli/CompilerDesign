use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::call_tree::CallTree,
};

use super::IRConstructor;

impl ToIR for CallTree {
    fn to_ir(&self, _constructor: &mut IRConstructor) -> Option<NodeIndex> {
        unimplemented!()
    }
}

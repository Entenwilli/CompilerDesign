use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::program_tree::ProgramTree,
};

use super::IRConstructor;

impl ToIR for ProgramTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        for function in self.functions() {
            function.to_ir(constructor);
        }
        None
    }
}

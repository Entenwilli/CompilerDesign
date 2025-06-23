use tracing::debug;

use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::continue_tree::ContinueTree,
};

use super::IRConstructor;

impl ToIR for ContinueTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        debug!(
            "Generating IR for continue statement with active loops: {:?}",
            constructor.active_loop_entries
        );
        let jump = constructor.create_jump();
        constructor.register_entry_point(
            *constructor.active_loop_entries.last().unwrap(),
            constructor.current_block(),
            jump,
        );
        None
    }
}

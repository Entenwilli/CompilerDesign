use tracing::debug;

use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    parser::ast::break_tree::BreakTree,
};

use super::IRConstructor;

impl ToIR for BreakTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        debug!(
            "Generating IR for break statement with active loops: {:?}",
            constructor.active_loop_exits
        );
        let jump = constructor.create_jump();
        constructor.register_entry_point(
            *constructor.active_loop_exits.last().unwrap(),
            constructor.current_block(),
            jump,
        );
        debug!(
            "Modifying entry_points of {:?}",
            constructor
                .graph
                .get_block_mut(*constructor.active_loop_exits.last().unwrap())
        );
        None
    }
}

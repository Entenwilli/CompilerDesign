use tracing::debug;

use crate::{
    ir::{
        ast::ToIR,
        block::{Block, NodeIndex},
    },
    parser::ast::while_tree::WhileTree,
};

use super::IRConstructor;

impl ToIR for WhileTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        debug!("Generating IR for while");
        let inverted_entry_condition_node = constructor.create_inverted_condition(self.condition());
        let entry_conditional_jump =
            constructor.create_conditional_jump(inverted_entry_condition_node);

        let entry_true_projection = constructor.create_true_projection(entry_conditional_jump);
        let entry_false_projection = constructor.create_false_projection(entry_conditional_jump);

        let mut loop_body = Block::new("while-body".to_string());
        loop_body.register_entry_point(constructor.current_block(), entry_false_projection);
        let loop_body_index = constructor.register_block(loop_body);
        let loop_back_block = Block::new("while-condition".to_string());
        let loop_back_block_index = constructor.register_block(loop_back_block);
        let mut following_block = Block::new("while-following".to_string());
        following_block.register_entry_point(constructor.current_block(), entry_true_projection);
        let following_block_index = constructor.register_block(following_block);
        constructor.seal_block(constructor.current_block());

        constructor.set_current_block(loop_body_index);
        constructor.active_loop_entries.push(loop_back_block_index);
        constructor.active_loop_exits.push(following_block_index);
        self.statement().to_ir(constructor);
        let loop_body_jump = constructor.create_jump();
        constructor.register_entry_point(
            loop_back_block_index,
            constructor.current_block(),
            loop_body_jump,
        );
        constructor.active_loop_entries.pop();
        constructor.active_loop_exits.pop();

        constructor.set_current_block(loop_back_block_index);
        constructor.seal_block(loop_back_block_index);
        let condition_node = self
            .condition()
            .to_ir(constructor)
            .expect("Expected condition of while to be expression");
        let conditional_jump = constructor.create_conditional_jump(condition_node);

        let true_projection = constructor.create_true_projection(conditional_jump);
        let false_projection = constructor.create_false_projection(conditional_jump);

        constructor.register_entry_point(loop_body_index, loop_back_block_index, true_projection);
        constructor.seal_block(loop_body_index);
        constructor.register_entry_point(
            following_block_index,
            loop_back_block_index,
            false_projection,
        );
        constructor.seal_block(loop_body_index);
        constructor.set_current_block(following_block_index);
        constructor.seal_block(constructor.current_block());
        None
    }
}

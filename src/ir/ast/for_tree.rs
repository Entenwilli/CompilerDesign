use tracing::debug;

use crate::{
    ir::{
        ast::ToIR,
        block::{Block, NodeIndex},
    },
    parser::ast::for_tree::ForTree,
};

use super::IRConstructor;

impl ToIR for ForTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        debug!("Generating IR for for expression");
        if let Some(initializer) = self.initializer() {
            initializer.to_ir(constructor);
        }
        let inverted_entry_condition_node = constructor.create_inverted_condition(self.condition());
        let entry_conditional_jump =
            constructor.create_conditional_jump(inverted_entry_condition_node);

        let entry_true_projection = constructor.create_true_projection(entry_conditional_jump);
        let entry_false_projection = constructor.create_false_projection(entry_conditional_jump);

        let mut loop_body = Block::new("for-body".to_string());
        loop_body.register_entry_point(constructor.current_block(), entry_false_projection);
        let loop_body_index = constructor.register_block(loop_body);
        let mut following_block = Block::new("for-following".to_string());
        following_block.register_entry_point(constructor.current_block(), entry_true_projection);
        let following_block_index = constructor.register_block(following_block);
        let loop_post = Block::new("for-post".to_string());
        let loop_post_index = constructor.register_block(loop_post);

        constructor.seal_block(constructor.current_block());

        constructor.set_current_block(loop_body_index);
        constructor.active_loop_entries.push(loop_post_index);
        constructor.active_loop_exits.push(following_block_index);
        self.statement().to_ir(constructor);
        constructor.active_loop_entries.pop();
        constructor.active_loop_exits.pop();
        let loop_body_exit = constructor.create_jump();
        constructor.register_entry_point(
            loop_post_index,
            constructor.current_block(),
            loop_body_exit,
        );
        constructor.set_current_block(loop_post_index);
        constructor.seal_block(loop_post_index);

        if let Some(advancement) = self.advancement() {
            advancement.to_ir(constructor);
        }

        let condition_node = self
            .condition()
            .to_ir(constructor)
            .expect("Expected condition to be statement");
        let conditional_jump = constructor.create_conditional_jump(condition_node);

        let true_projection = constructor.create_true_projection(conditional_jump);
        let false_projection = constructor.create_false_projection(conditional_jump);

        constructor.register_entry_point(loop_body_index, loop_post_index, true_projection);
        constructor.seal_block(loop_body_index);
        constructor.register_entry_point(following_block_index, loop_post_index, false_projection);
        constructor.seal_block(loop_body_index);
        constructor.set_current_block(following_block_index);
        constructor.seal_block(constructor.current_block());
        debug!("Following block after for: {}", constructor.current_block());
        None
    }
}

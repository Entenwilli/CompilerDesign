use tracing::debug;

use crate::{
    ir::{
        ast::ToIR,
        block::{Block, NodeIndex},
    },
    parser::ast::if_tree::IfTree,
};

use super::IRConstructor;

impl ToIR for IfTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        debug!("Generating IR for If");
        let condition_node = self
            .condition()
            .to_ir(constructor)
            .expect("Condition of if statement not a condition!");
        let conditional_jump = constructor.create_conditional_jump(condition_node);

        let true_projection = constructor.create_true_projection(conditional_jump);
        let false_projection = constructor.create_false_projection(conditional_jump);

        let mut false_block = Block::new("if-false".to_string());
        false_block.register_entry_point(constructor.current_block(), false_projection);
        let mut true_block = Block::new("if-true".to_string());
        true_block.register_entry_point(constructor.current_block(), true_projection);
        constructor.seal_block(constructor.current_block());
        let mut following_block = Block::new("if-following".to_string());

        let false_block_index = constructor.register_block(false_block);
        constructor.seal_block(false_block_index);
        constructor.set_current_block(false_block_index);
        if let Some(else_statement) = self.else_statement() {
            else_statement.to_ir(constructor);
        }
        let false_jump = constructor.create_jump();
        following_block.register_entry_point(constructor.current_block(), false_jump);

        let true_block_index = constructor.register_block(true_block);
        constructor.seal_block(true_block_index);
        constructor.set_current_block(true_block_index);
        self.if_statement().to_ir(constructor);
        let true_jump = constructor.create_jump();
        following_block.register_entry_point(constructor.current_block(), true_jump);

        let following_block_index = constructor.register_block(following_block);
        constructor.set_current_block(following_block_index);
        constructor.seal_block(following_block_index);
        None
    }
}

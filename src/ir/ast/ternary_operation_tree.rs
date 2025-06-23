use tracing::debug;

use crate::{
    ir::{
        ast::ToIR,
        block::{Block, NodeIndex},
    },
    parser::ast::ternary_operation_tree::TernaryOperationTree,
};

use super::IRConstructor;

impl ToIR for TernaryOperationTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        debug!("Generating IR for TernaryOperation");
        let condition_node = self
            .condition()
            .to_ir(constructor)
            .expect("Expected condition of ternary operation to be an expression");
        let conditional_jump = constructor.create_conditional_jump(condition_node);

        let true_projection = constructor.create_true_projection(conditional_jump);
        let false_projection = constructor.create_false_projection(conditional_jump);

        let mut false_block = Block::new("ternary-false".to_string());
        false_block.register_entry_point(constructor.current_block(), false_projection);
        let mut true_block = Block::new("ternary-true".to_string());
        true_block.register_entry_point(constructor.current_block(), true_projection);
        constructor.seal_block(constructor.current_block());
        let mut following_block = Block::new("ternary-following".to_string());

        let false_block_index = constructor.register_block(false_block);
        constructor.set_current_block(false_block_index);
        let false_expression = self
            .false_expression()
            .to_ir(constructor)
            .expect("Expected false expression in ternary operation to be expression");
        let false_jump = constructor.create_jump();
        following_block.register_entry_point(constructor.current_block(), false_jump);
        constructor.seal_block(false_block_index);

        let true_block_index = constructor.register_block(true_block);
        constructor.set_current_block(true_block_index);
        let true_expression = self
            .true_expression()
            .to_ir(constructor)
            .expect("Expected true expression in ternary operation to be expression");
        let true_jump = constructor.create_jump();
        following_block.register_entry_point(constructor.current_block(), true_jump);
        constructor.seal_block(true_block_index);

        let following_block_index = constructor.register_block(following_block);
        constructor.set_current_block(following_block_index);
        constructor.seal_block(true_block_index);
        constructor.seal_block(false_block_index);
        let phi = constructor.create_phi_from_operands(vec![
            (false_block_index, false_expression),
            (true_block_index, true_expression),
        ]);
        constructor.seal_block(constructor.current_block());
        Some(phi)
    }
}

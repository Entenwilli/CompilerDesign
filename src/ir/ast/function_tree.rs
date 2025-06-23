use crate::{
    ir::{
        ast::ToIR,
        block::{Block, NodeIndex},
        graph::{END_BLOCK, START_BLOCK},
        node::{
            projection::{ProjectionData, ProjectionInformation},
            Node,
        },
    },
    parser::ast::function_tree::FunctionTree,
};

use super::IRConstructor;

impl ToIR for FunctionTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        let mut function_body_block = Block::new("fn-body".to_string());
        // Function body can be entered from start function block
        function_body_block.register_entry_point(START_BLOCK, 0);
        let side_effect_projection = function_body_block.register_node(Node::Projection(
            ProjectionData::new(0, ProjectionInformation::SideEffect),
        ));
        let function_body_block_index = constructor.register_block(function_body_block);
        constructor.set_current_block(function_body_block_index);

        constructor.write_current_side_effect(side_effect_projection);
        self.body().to_ir(constructor);
        constructor.seal_block(constructor.current_block());

        // The last statement after parsing the body can exit the function
        if !constructor.get_block(constructor.current_block()).empty() {
            let last_statement_index = constructor
                .get_block(constructor.current_block())
                .get_last_node_index();
            constructor.register_entry_point(
                END_BLOCK,
                constructor.current_block(),
                last_statement_index,
            );
        } else {
            constructor.register_entry_point(END_BLOCK, constructor.current_block(), 0);
        }
        None
    }
}

use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    lexer::operator::UnaryOperator,
    parser::ast::unary_operation_tree::UnaryOperationTree,
};

use super::IRConstructor;

impl ToIR for UnaryOperationTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        match self.operator() {
            UnaryOperator::Minus => {
                let node = self
                    .expression()
                    .to_ir(constructor)
                    .expect("Expected operand of unary operator to be expression");
                let zero = constructor.create_constant_int(0);
                let result = constructor.create_sub(zero, node);
                Some(result)
            }
            UnaryOperator::BitwiseNot => {
                let node = self
                    .expression()
                    .to_ir(constructor)
                    .expect("Expected operand of unary operator to be expression");
                let result = constructor.create_bitwise_not(node);
                Some(result)
            }
            UnaryOperator::LogicalNot => {
                let node = self
                    .expression()
                    .to_ir(constructor)
                    .expect("Expected operand of unary operator to be expression");
                let result = constructor.create_logical_not(node);
                Some(result)
            }
        }
    }
}

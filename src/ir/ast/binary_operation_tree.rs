use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    lexer::operator::BinaryOperator,
    parser::ast::binary_operation_tree::BinaryOperationTree,
};

use super::IRConstructor;

impl ToIR for BinaryOperationTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        let lhs_node = self
            .lhs()
            .to_ir(constructor)
            .expect("Expected LHS to be expression");
        let rhs_node = self
            .rhs()
            .to_ir(constructor)
            .expect("Expected RHS to be expression");
        let result = match self.operator() {
            BinaryOperator::Minus => constructor.create_sub(lhs_node, rhs_node),
            BinaryOperator::Plus => constructor.create_add(lhs_node, rhs_node),
            BinaryOperator::Mul => constructor.create_mul(lhs_node, rhs_node),
            BinaryOperator::Div => {
                let div_node = constructor.create_div(lhs_node, rhs_node);
                constructor.create_div_mod_projection(div_node)
            }
            BinaryOperator::Mod => {
                let mod_node = constructor.create_mod(lhs_node, rhs_node);
                constructor.create_div_mod_projection(mod_node)
            }
            BinaryOperator::ShiftLeft => constructor.create_shift_left(lhs_node, rhs_node),
            BinaryOperator::ShiftRight => constructor.create_shift_right(lhs_node, rhs_node),
            BinaryOperator::Lower => constructor.create_lower(lhs_node, rhs_node),
            BinaryOperator::LowerEquals => constructor.create_lower_equals(lhs_node, rhs_node),
            BinaryOperator::Equals => constructor.create_equals(lhs_node, rhs_node),
            BinaryOperator::NotEquals => constructor.create_not_equals(lhs_node, rhs_node),
            BinaryOperator::HigherEquals => constructor.create_higher_equals(lhs_node, rhs_node),
            BinaryOperator::Higher => constructor.create_higher(lhs_node, rhs_node),
            BinaryOperator::BitwiseOr => constructor.create_or(lhs_node, rhs_node),
            BinaryOperator::BitwiseAnd => constructor.create_and(lhs_node, rhs_node),
            BinaryOperator::BitwiseXor => constructor.create_xor(lhs_node, rhs_node),
            BinaryOperator::LogicalAnd => constructor.create_and(lhs_node, rhs_node),
            BinaryOperator::LogicalOr => constructor.create_or(lhs_node, rhs_node),
        };
        Some(result)
    }
}

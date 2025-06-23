use crate::{
    ir::{ast::ToIR, block::NodeIndex},
    lexer::operator::AssignmentOperator,
    parser::ast::assignment_tree::AssignmentTree,
};

use super::IRConstructor;

impl ToIR for AssignmentTree {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex> {
        let rhs = self
            .expression()
            .to_ir(constructor)
            .expect("Invalid expression!");
        match self.operator() {
            AssignmentOperator::AssignMinus => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let desugar = constructor.create_sub(lhs, rhs);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignPlus => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let desugar = constructor.create_add(lhs, rhs);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignMul => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let desugar = constructor.create_mul(lhs, rhs);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignDiv => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let div = constructor.create_div(lhs, rhs);
                let desugar = constructor.create_div_mod_projection(div);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignMod => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let mod_node = constructor.create_mod(lhs, rhs);
                let desugar = constructor.create_div_mod_projection(mod_node);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignShiftLeft => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let desugar = constructor.create_shift_left(lhs, rhs);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignShiftRight => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let desugar = constructor.create_shift_right(lhs, rhs);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignBitwiseOr => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let desugar = constructor.create_or(lhs, rhs);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignBitwiseAnd => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let desugar = constructor.create_and(lhs, rhs);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::AssignBitwiseXor => {
                let lhs = constructor.read_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                );
                let desugar = constructor.create_xor(lhs, rhs);
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    desugar,
                );
            }
            AssignmentOperator::Assign => {
                constructor.write_variable(
                    self.lvalue().identifier().name().clone(),
                    constructor.current_block(),
                    rhs,
                );
            }
            AssignmentOperator::AssignBitwiseNot => todo!(),
        };
        return None;
    }
}

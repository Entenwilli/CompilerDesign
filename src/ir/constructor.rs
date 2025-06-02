use core::panic;
use std::collections::HashMap;

use tracing::{debug, event, Level};

use crate::{
    ir::{graph, node::JumpData},
    lexer::token::{OperatorType, Token},
    parser::{ast::Tree, symbols::Name},
    util::int_parsing::parse_int,
};

use super::{
    graph::{IRGraph, START_BLOCK},
    node::{
        AddData, AndData, BitwiseNegateData, BlockData, ConditionalJumpData, ConstantBoolData,
        ConstantIntData, DivisionData, EqualsData, HigherData, HigherEqualsData, LowerData,
        LowerEqualsData, ModuloData, MultiplicationData, Node, NodeData, NotEqualsData, OrData,
        PhiData, ProjectionData, ProjectionInformation, ReturnData, ShiftLeftData, ShiftRightData,
        StartData, SubtractionData, XorData,
    },
    optimizer::Optimizer,
};

pub struct IRGraphConstructor {
    optimizer: Optimizer,
    graph: IRGraph,
    current_definitions: HashMap<Name, HashMap<usize, usize>>,
    incomplete_phis: HashMap<usize, HashMap<Name, usize>>,
    current_side_effect: HashMap<usize, usize>,
    incomplete_side_effect_phis: HashMap<usize, usize>,
    sealed_blocks: Vec<usize>,
    loop_starts: Vec<usize>,
    loop_ends: Vec<usize>,
    current_block: usize,
    next_block_number: usize,
}

impl IRGraphConstructor {
    pub fn new() -> IRGraphConstructor {
        IRGraphConstructor {
            optimizer: Optimizer::new(),
            graph: IRGraph::new(),
            current_definitions: HashMap::new(),
            incomplete_phis: HashMap::new(),
            current_side_effect: HashMap::new(),
            incomplete_side_effect_phis: HashMap::new(),
            // Start Block never gets more predecessors
            sealed_blocks: vec![0],
            loop_starts: Vec::new(),
            loop_ends: Vec::new(),
            current_block: 0,
            next_block_number: 2,
        }
    }

    pub fn convert(&mut self, tree: Tree) -> Option<usize> {
        debug!("Converting AST {} to IR!", tree);
        match tree {
            Tree::Program(functions) => {
                for function in functions {
                    self.convert(function);
                }
                None
            }
            Tree::Function(_, _, body) => {
                let start = self.create_start_block();
                let side_effect_projection = self.create_side_effect_projection(start);
                self.write_current_side_effect(side_effect_projection);
                self.convert_boxed(body);
                None
            }
            Tree::Assignment(lvalue, operator, expression) => match *lvalue {
                Tree::LValueIdentifier(identifier) => {
                    if let Tree::Name(name, _) = *identifier {
                        let rhs = self.convert_boxed(expression).expect("Invalid expression!");
                        if let Token::Operator(_, operator_type) = operator {
                            match operator_type {
                                OperatorType::AssignMinus => {
                                    let lhs = self.read_variable(name.clone(), self.current_block);
                                    let desugar = self.create_sub(lhs, rhs);
                                    self.write_variable(name, self.current_block, desugar);
                                }
                                OperatorType::AssignPlus => {
                                    let lhs = self.read_variable(name.clone(), self.current_block);
                                    let desugar = self.create_add(lhs, rhs);
                                    self.write_variable(name, self.current_block, desugar);
                                }
                                OperatorType::AssignMul => {
                                    let lhs = self.read_variable(name.clone(), self.current_block);
                                    let desugar = self.create_mul(lhs, rhs);
                                    self.write_variable(name, self.current_block, desugar);
                                }
                                OperatorType::AssignDiv => {
                                    let lhs = self.read_variable(name.clone(), self.current_block);
                                    let div = self.create_div(lhs, rhs);
                                    let desugar = self.create_div_mod_projection(div);
                                    self.write_variable(name, self.current_block, desugar);
                                }
                                OperatorType::AssignMod => {
                                    let lhs = self.read_variable(name.clone(), self.current_block);
                                    let mod_node = self.create_mod(lhs, rhs);
                                    let desugar = self.create_div_mod_projection(mod_node);
                                    self.write_variable(name, self.current_block, desugar);
                                }
                                OperatorType::Assign => {
                                    self.write_variable(name, self.current_block, rhs);
                                }
                                _ => panic!("Assignment has no assignment operator"),
                            };
                        }
                        return None;
                    }
                    None
                }
                _ => todo!(),
            },
            Tree::BinaryOperation(lhs, rhs, operator_type) => {
                let lhs_node = self.convert_boxed(lhs).unwrap();
                let rhs_node = self.convert_boxed(rhs).unwrap();
                let result = match operator_type {
                    OperatorType::Minus => self.create_sub(lhs_node, rhs_node),
                    OperatorType::Plus => self.create_add(lhs_node, rhs_node),
                    OperatorType::Mul => self.create_mul(lhs_node, rhs_node),
                    OperatorType::Div => {
                        let div_node = self.create_div(lhs_node, rhs_node);
                        self.create_div_mod_projection(div_node)
                    }
                    OperatorType::Mod => {
                        let mod_node = self.create_mod(lhs_node, rhs_node);
                        self.create_div_mod_projection(mod_node)
                    }
                    OperatorType::ShiftLeft => self.create_shift_left(lhs_node, rhs_node),
                    OperatorType::ShiftRight => self.create_shift_right(lhs_node, rhs_node),
                    OperatorType::Lower => self.create_lower(lhs_node, rhs_node),
                    OperatorType::LowerEquals => self.create_lower_equals(lhs_node, rhs_node),
                    OperatorType::Equals => self.create_equals(lhs_node, rhs_node),
                    OperatorType::NotEquals => self.create_not_equals(lhs_node, rhs_node),
                    OperatorType::HigherEquals => self.create_higher_equals(lhs_node, rhs_node),
                    OperatorType::Higher => self.create_higher(lhs_node, rhs_node),
                    OperatorType::BitwiseOr => self.create_or(lhs_node, rhs_node),
                    OperatorType::BitwiseAnd => self.create_and(lhs_node, rhs_node),
                    OperatorType::BitwiseXor => self.create_xor(lhs_node, rhs_node),
                    OperatorType::LogicalAnd => self.create_and(lhs_node, rhs_node),
                    OperatorType::LogicalOr => self.create_or(lhs_node, rhs_node),
                    OperatorType::Assign
                    | OperatorType::AssignMul
                    | OperatorType::AssignDiv
                    | OperatorType::AssignMod
                    | OperatorType::AssignPlus
                    | OperatorType::AssignMinus
                    | OperatorType::AssignShiftLeft
                    | OperatorType::AssignShiftRight
                    | OperatorType::AssignBitwiseOr
                    | OperatorType::AssignBitwiseNot
                    | OperatorType::AssignBitwiseAnd
                    | OperatorType::AssignBitwiseXor => {
                        panic!("Expected binary operator, got assignment operator!")
                    }
                    OperatorType::LogicalNot | OperatorType::BitwiseNot => {
                        panic!("Expected binary operator, got unary operator!")
                    }
                    OperatorType::TernaryQuestionMark | OperatorType::TernaryColon => {
                        panic!("Expected binary operator, got ternary operator!")
                    }
                };
                Some(result)
            }
            Tree::Block(statements, _) => {
                for statement in statements {
                    if let Tree::Return(_, _) = statement {
                        self.convert(statement);
                        break;
                    }
                    self.convert(statement);
                }
                None
            }
            Tree::Declaration(_, identifier, initializer) => {
                if let Tree::Name(name, _) = *identifier {
                    if initializer.is_some() {
                        let rhs = self.convert_boxed(initializer.unwrap()).unwrap();
                        self.write_variable(name, self.current_block, rhs);
                    }
                    None
                } else {
                    panic!("Identifier of declaration is not name!");
                }
            }
            Tree::IdentifierExpression(identifier) => {
                if let Tree::Name(name, _) = *identifier {
                    let value = self.read_variable(name, self.current_block);
                    Some(value)
                } else {
                    panic!("Identifier expression did not have name as identifier!")
                }
            }
            Tree::Literal(constant, base, _) => {
                let value = parse_int(constant, base)?;
                let node = self.create_constant_int(value);
                Some(node)
            }
            Tree::LValueIdentifier(_) => None,
            Tree::Name(_, _) => None,
            Tree::UnaryOperation(expression, operator_type, _) => match operator_type {
                OperatorType::Minus => {
                    let node = self.convert_boxed(expression)?;
                    let zero = self.create_constant_int(0);
                    let result = self.create_sub(zero, node);
                    Some(result)
                }
                OperatorType::BitwiseNot => {
                    let node = self.convert_boxed(expression)?;
                    let result = self.create_bitwise_not(node);
                    Some(result)
                }
                OperatorType::LogicalNot => {
                    let node = self.convert_boxed(expression)?;
                    let result = self.create_bitwise_not(node);
                    Some(result)
                }
                _ => panic!("Unregistered Unary Operation {:?}", operator_type),
            },
            Tree::Return(expression, _) => {
                let node = self.convert_boxed(expression).unwrap();
                let return_node = self.create_return(node);
                self.graph
                    .end_block_mut()
                    .predecessors_mut()
                    .push(return_node);
                None
            }
            Tree::Type(_, _) => None,
            Tree::BoolLiteral(boolean, _) => {
                let node = if boolean {
                    self.create_constant_bool(true)
                } else {
                    self.create_constant_bool(false)
                };
                Some(node)
            }
            Tree::Break(_) => {
                // Jump to after loop
                todo!("Create unconditional jump with correct target");
            }
            Tree::Continue(_) => {
                // Jump to post condition of loop
                todo!("Create unconditional jump with correct target");
            }
            Tree::TernaryOperation(expression, true_value, false_value) => {
                debug!("Generating IR for TernaryOperation");
                let condition_node = self.convert_boxed(expression)?;
                let conditional_jump = self.create_conditional_jump(condition_node);

                let true_projection = self.create_true_projection(conditional_jump);
                let false_projection = self.create_false_projection(conditional_jump);

                let false_block = self.create_block("ternary-false".to_string());
                self.graph
                    .get_node_mut(false_block)
                    .predecessors_mut()
                    .push(false_projection);

                let true_block = self.create_block("ternary-true".to_string());
                self.graph
                    .get_node_mut(true_block)
                    .predecessors_mut()
                    .push(true_projection);
                self.seal_block(self.current_block);

                self.current_block = self.graph.get_node(false_block).block();
                let false_expression = self.convert_boxed(false_value)?;
                self.graph
                    .get_node_mut(false_expression)
                    .predecessors_mut()
                    .push(false_block);
                let false_jump = self.create_jump();
                self.graph
                    .get_node_mut(false_jump)
                    .predecessors_mut()
                    .push(false_expression);

                self.current_block = self.graph.get_node(true_block).block();
                let true_expression = self.convert_boxed(true_value)?;
                self.graph
                    .get_node_mut(true_expression)
                    .predecessors_mut()
                    .push(true_block);
                let true_jump = self.create_jump();
                self.graph
                    .get_node_mut(true_jump)
                    .predecessors_mut()
                    .push(true_expression);

                let following_block = self.create_block("ternary-following".to_string());
                self.graph
                    .get_node_mut(following_block)
                    .predecessors_mut()
                    .push(true_jump);
                self.graph
                    .get_node_mut(following_block)
                    .predecessors_mut()
                    .push(false_jump);
                self.current_block = self.graph.get_node(following_block).block();
                self.seal_block(true_block);
                self.seal_block(false_block);
                let phi = self.create_phi();
                self.graph
                    .get_node_mut(phi)
                    .predecessors_mut()
                    .push(true_expression);
                self.graph
                    .get_node_mut(phi)
                    .predecessors_mut()
                    .push(false_expression);
                self.graph
                    .get_node_mut(phi)
                    .predecessors_mut()
                    .push(following_block);
                Some(phi)
            }
            Tree::While(condition, expression, _) => {
                self.seal_block(self.current_block);

                let while_block = self.create_block("while".to_string());
                self.graph
                    .get_node_mut(while_block)
                    .predecessors_mut()
                    .push(self.current_block);
                self.current_block = while_block;
                self.loop_starts.push(while_block);

                let condition_block = self.convert_boxed(condition)?;

                let conditional_jump = self.create_conditional_jump(condition_block);
                let true_block = self.create_true_projection(conditional_jump);
                let false_block = self.create_false_projection(conditional_jump);

                let follow_block = self.create_block("while-follow".to_string());
                self.graph
                    .get_node_mut(follow_block)
                    .predecessors_mut()
                    .push(false_block);
                self.loop_ends.push(follow_block);

                let body_block = self.create_block("while-body".to_string());
                self.graph
                    .get_node_mut(body_block)
                    .predecessors_mut()
                    .push(true_block);
                self.seal_block(body_block);
                self.current_block = body_block;
                self.convert_boxed(expression);
                let continue_jump = self.create_jump();
                self.seal_block(self.current_block);
                self.graph
                    .get_node_mut(while_block)
                    .predecessors_mut()
                    .push(continue_jump);

                self.loop_starts.pop();
                self.seal_block(while_block);
                self.loop_ends.pop();
                self.seal_block(follow_block);
                self.current_block = follow_block;
                None
            }
            Tree::If(condition, body, else_body, _) => {
                let condition_block = self.convert_boxed(condition)?;
                let conditional_jump = self.create_conditional_jump(condition_block);
                let true_projection = self.create_true_projection(conditional_jump);
                let false_projection = self.create_false_projection(conditional_jump);
                self.seal_block(self.current_block);

                let true_block = self.process_branch(body, true_projection, "true");
                let false_block = if let Some(else_body_ab) = else_body {
                    Some(self.process_branch(else_body_ab, false_projection, "false"))
                } else {
                    None
                };

                let following_block = self.create_block("following-if".to_string());
                self.current_block = following_block;
                let following_node = self.graph.get_node_mut(following_block);
                following_node.predecessors_mut().push(true_block);
                if let Some(false_block_index) = false_block {
                    following_node.predecessors_mut().push(false_block_index);
                }
                self.seal_block(following_block);
                None
            }
            node => todo!("Unimplemented {:?}", node),
        }
    }

    pub fn convert_boxed(&mut self, tree: Box<Tree>) -> Option<usize> {
        self.convert(*tree)
    }

    pub fn create_conditional_jump(&mut self, condition: usize) -> usize {
        self.graph
            .register_node(Node::ConditionalJump(ConditionalJumpData::new(
                self.current_block,
                condition,
            )))
    }

    pub fn process_branch(&mut self, a: Box<Tree>, b: usize, label: &str) -> usize {
        let block = self.create_block(format!("if-body-{}", label));
        self.current_block = block;
        self.graph.get_node_mut(block).predecessors_mut().push(b);
        self.seal_block(block);
        self.convert_boxed(a);
        self.seal_block(self.current_block);
        self.create_jump()
    }
    pub fn create_block(&mut self, name: String) -> usize {
        let result = self
            .graph
            .register_node(Node::Block(BlockData::new(self.next_block_number)));
        self.next_block_number += 1;
        result
    }

    pub fn create_jump(&mut self) -> usize {
        self.graph
            .register_node(Node::Jump(JumpData::new(self.current_block)))
    }

    fn create_start_block(&mut self) -> usize {
        self.graph
            .register_node(Node::Start(StartData::new(self.current_block)))
    }

    fn create_noop(&mut self) -> usize {
        self.graph
            .register_node(Node::NoOp(NodeData::new(self.current_block, vec![])))
    }

    fn create_add(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::Add(AddData::new(
                self.current_block,
                left,
                right,
            ))))
    }

    fn create_sub(&mut self, left: usize, right: usize) -> usize {
        self.graph.register_node(
            self.optimizer
                .transform(Node::Subtraction(SubtractionData::new(
                    self.current_block,
                    left,
                    right,
                ))),
        )
    }

    fn create_bitwise_not(&mut self, node: usize) -> usize {
        self.graph
            .register_node(
                self.optimizer
                    .transform(Node::BitwiseNegate(BitwiseNegateData::new(
                        self.current_block,
                        node,
                    ))),
            )
    }

    fn create_mul(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(
                self.optimizer
                    .transform(Node::Multiplication(MultiplicationData::new(
                        self.current_block,
                        left,
                        right,
                    ))),
            )
    }

    fn create_div(&mut self, left: usize, right: usize) -> usize {
        let current_side_effect = self.read_current_side_effect();
        self.graph
            .register_node(self.optimizer.transform(Node::Division(DivisionData::new(
                self.current_block,
                left,
                right,
                current_side_effect,
            ))))
    }

    fn create_mod(&mut self, left: usize, right: usize) -> usize {
        let current_side_effect = self.read_current_side_effect();
        self.graph
            .register_node(self.optimizer.transform(Node::Modulo(ModuloData::new(
                self.current_block,
                left,
                right,
                current_side_effect,
            ))))
    }

    fn create_shift_right(&mut self, left: usize, right: usize) -> usize {
        self.graph.register_node(
            self.optimizer
                .transform(Node::ShiftRight(ShiftRightData::new(
                    self.current_block,
                    left,
                    right,
                ))),
        )
    }

    fn create_shift_left(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::ShiftLeft(ShiftLeftData::new(
                self.current_block,
                left,
                right,
            ))))
    }
    fn create_or(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::Or(OrData::new(
                self.current_block,
                left,
                right,
            ))))
    }
    fn create_and(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::And(AndData::new(
                self.current_block,
                left,
                right,
            ))))
    }
    fn create_xor(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::Xor(XorData::new(
                self.current_block,
                left,
                right,
            ))))
    }
    fn create_lower(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::Lower(LowerData::new(
                self.current_block,
                left,
                right,
            ))))
    }
    fn create_lower_equals(&mut self, left: usize, right: usize) -> usize {
        self.graph.register_node(
            self.optimizer
                .transform(Node::LowerEquals(LowerEqualsData::new(
                    self.current_block,
                    left,
                    right,
                ))),
        )
    }
    fn create_equals(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::Equals(EqualsData::new(
                self.current_block,
                left,
                right,
            ))))
    }
    fn create_not_equals(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::NotEquals(NotEqualsData::new(
                self.current_block,
                left,
                right,
            ))))
    }
    fn create_higher_equals(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(
                self.optimizer
                    .transform(Node::HigherEquals(HigherEqualsData::new(
                        self.current_block,
                        left,
                        right,
                    ))),
            )
    }
    fn create_higher(&mut self, left: usize, right: usize) -> usize {
        self.graph
            .register_node(self.optimizer.transform(Node::Higher(HigherData::new(
                self.current_block,
                left,
                right,
            ))))
    }

    fn create_return(&mut self, result: usize) -> usize {
        let current_side_effect = self.read_current_side_effect();
        self.graph.register_node(Node::Return(ReturnData::new(
            self.current_block,
            result,
            current_side_effect,
        )))
    }

    fn create_constant_int(&mut self, value: i32) -> usize {
        self.graph.register_node(
            self.optimizer
                .transform(Node::ConstantInt(ConstantIntData::new(
                    self.current_block,
                    value,
                ))),
        )
    }

    fn create_side_effect_projection(&mut self, node: usize) -> usize {
        self.graph
            .register_node(Node::Projection(ProjectionData::new(
                self.current_block,
                node,
                ProjectionInformation::SideEffect,
            )))
    }

    fn create_result_projection(&mut self, node: usize) -> usize {
        self.graph
            .register_node(Node::Projection(ProjectionData::new(
                self.current_block,
                node,
                ProjectionInformation::Result,
            )))
    }

    fn create_phi(&mut self) -> usize {
        self.graph
            .register_node(Node::Phi(PhiData::new(self.current_block)))
    }

    fn create_phi_operands(&mut self, block: usize) -> usize {
        let mut operands = Vec::new();
        for operand in self.graph.get_predecessors(block).clone() {
            operands.push(self.read_side_effect(operand));
        }
        self.graph
            .register_node(Node::Phi(PhiData::new_with_operands(
                self.current_block,
                operands,
            )))
    }

    fn create_phi_variable_operands(&mut self, block: usize, variable: Name) -> usize {
        let mut operands = Vec::new();
        for operand in self.graph.get_predecessors(block).clone() {
            let block = self.graph.get_node(operand).block();
            operands.push(self.read_variable(variable.clone(), block));
        }
        self.graph
            .register_node(Node::Phi(PhiData::new_with_operands(
                self.current_block,
                operands,
            )))
    }

    fn create_div_mod_projection(&mut self, node: usize) -> usize {
        let projection_side_effect = self.create_side_effect_projection(node);
        self.write_current_side_effect(projection_side_effect);
        self.create_result_projection(node)
    }

    fn write_variable(&mut self, variable: Name, block: usize, node: usize) {
        match self.current_definitions.contains_key(&variable) {
            true => {
                self.current_definitions
                    .get_mut(&variable)
                    .unwrap()
                    .insert(block, node);
            }
            false => {
                self.current_definitions
                    .insert(variable, HashMap::from([(block, node)]));
            }
        }
    }

    fn read_variable(&mut self, variable: Name, block: usize) -> usize {
        if self.current_definitions.contains_key(&variable) {
            *self
                .current_definitions
                .get(&variable)
                .unwrap()
                .get(&block)
                .unwrap()
        } else {
            self.read_variable_recursive(variable, block)
        }
    }

    fn read_variable_recursive(&mut self, variable: Name, block: usize) -> usize {
        let node = if !self.sealed_blocks.contains(&block) {
            let phi = self.create_phi();
            if let std::collections::hash_map::Entry::Vacant(e) = self.incomplete_phis.entry(block)
            {
                e.insert(HashMap::from([(variable.clone(), phi)]));
            } else {
                self.incomplete_phis
                    .get_mut(&block)
                    .unwrap()
                    .insert(variable.clone(), phi);
            }
            phi
        } else if self.graph.get_predecessors(block).len() == 1 {
            self.read_variable(
                variable.clone(),
                *self.graph.get_predecessors(block).first().unwrap(),
            )
        } else {
            let phi = self.create_phi_variable_operands(block, variable.clone());
            self.write_variable(variable.clone(), block, phi);
            phi
        };
        self.write_variable(variable.clone(), block, node);
        node
    }

    fn seal_block(&mut self, block: usize) {
        if !self.incomplete_phis.contains_key(&block) {
            self.sealed_blocks.push(block);
            return;
        }
        for (variable, index) in self.incomplete_phis.get(&block).unwrap().clone() {
            let phi = self.graph.remove_node(index);
            if let Node::Phi(mut data) = phi {
                for predecessor in data.node_data().predecessors().clone() {
                    let block = self.graph.get_node(predecessor).block();
                    data.add_operand(self.read_variable(variable.clone(), block));
                }
            }
        }
        self.sealed_blocks.push(block);
    }

    fn write_current_side_effect(&mut self, node: usize) {
        self.write_side_effect(self.current_block, node);
    }

    fn write_side_effect(&mut self, block: usize, node: usize) {
        self.current_side_effect.insert(block, node);
    }

    fn read_current_side_effect(&mut self) -> usize {
        self.read_side_effect(self.current_block)
    }

    fn read_side_effect(&mut self, block: usize) -> usize {
        if self.current_side_effect.contains_key(&block) {
            *self.current_side_effect.get(&block).unwrap()
        } else {
            self.read_side_effect_recusive(block)
        }
    }

    fn read_side_effect_recusive(&mut self, block: usize) -> usize {
        let node = if !self.sealed_blocks.contains(&block) {
            let phi = self.create_phi();
            let old_phi = self.incomplete_side_effect_phis.insert(block, phi);
            if old_phi.is_some() {
                panic!("Double read side effect recursive!");
            }
            phi
        } else if self.graph.get_predecessors(block).len() == 1 {
            self.read_side_effect(*self.graph.get_predecessors(block).first().unwrap())
        } else {
            let phi = self.create_phi_operands(block);
            self.write_side_effect(block, phi);
            phi
        };
        self.write_side_effect(block, node);
        node
    }

    pub fn graph(self) -> IRGraph {
        self.graph
    }

    pub fn create_constant_bool(&mut self, value: bool) -> usize {
        self.graph
            .register_node(Node::ConstantBool(ConstantBoolData::new(
                self.current_block,
                value,
            )))
    }
    pub fn create_true_projection(&mut self, block: usize) -> usize {
        self.graph
            .register_node(Node::Projection(ProjectionData::new(
                self.current_block,
                block,
                ProjectionInformation::IfTrue,
            )))
    }

    pub fn create_false_projection(&mut self, block: usize) -> usize {
        self.graph
            .register_node(Node::Projection(ProjectionData::new(
                self.current_block,
                block,
                ProjectionInformation::IfFalse,
            )))
    }
}

impl Default for IRGraphConstructor {
    fn default() -> Self {
        Self::new()
    }
}

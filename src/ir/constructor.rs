use core::panic;
use std::collections::HashMap;

use tracing::{debug, info, trace};

use crate::{
    ir::{
        block::Block,
        graph::START_BLOCK,
        node::{
            binary_operation::BinaryOperationData,
            projection::{ProjectionData, ProjectionInformation},
        },
    },
    lexer::token::{OperatorType, Token},
    parser::{ast::Tree, symbols::Name},
    util::int_parsing::parse_int,
};

use super::{
    block::NodeIndex,
    graph::{BlockIndex, IRGraph},
    node::{
        unary_operation::UnaryOperationData, ConstantBoolData, ConstantIntData, Node, PhiData,
        ReturnData,
    },
};

pub struct IRGraphConstructor {
    graph: IRGraph,
    current_definitions: HashMap<Name, HashMap<BlockIndex, NodeIndex>>,
    incomplete_phis: HashMap<BlockIndex, HashMap<Name, NodeIndex>>,
    current_side_effect: HashMap<usize, usize>,
    _incomplete_side_effect_phis: HashMap<usize, usize>,
    sealed_blocks: Vec<usize>,
    current_block_index: BlockIndex,
    active_loop_entries: Vec<BlockIndex>,
    active_loop_exits: Vec<BlockIndex>,
}

impl IRGraphConstructor {
    pub fn new() -> IRGraphConstructor {
        IRGraphConstructor {
            graph: IRGraph::new(),
            current_definitions: HashMap::new(),
            incomplete_phis: HashMap::new(),
            current_side_effect: HashMap::new(),
            _incomplete_side_effect_phis: HashMap::new(),
            // Start Block never gets more predecessors
            sealed_blocks: vec![START_BLOCK],
            current_block_index: START_BLOCK,
            active_loop_entries: Vec::new(),
            active_loop_exits: Vec::new(),
        }
    }

    pub fn convert(&mut self, tree: Tree) -> Option<NodeIndex> {
        debug!("Converting AST {:?} to IR!", tree);
        match tree {
            Tree::Program(_) => {
                unimplemented!("Program Trees cannot be parsed to a single IR Representation!")
            }
            Tree::Function(_, _, body) => {
                let mut function_body_block = Block::new("fn-body".to_string());
                // Function body can be entered from start function block
                function_body_block.register_entry_point(START_BLOCK, 0);
                let side_effect_projection = function_body_block.register_node(Node::Projection(
                    ProjectionData::new(0, ProjectionInformation::SideEffect),
                ));
                self.current_block_index = self.graph.register_block(function_body_block);

                self.write_current_side_effect(side_effect_projection);
                self.convert_boxed(body);
                self.seal_block(self.current_block_index);

                // The last statement after parsing the body can exit the function
                if !self.graph.get_block(self.current_block_index).empty() {
                    let last_statement_index = self
                        .graph
                        .get_block(self.current_block_index)
                        .get_last_node_index();
                    self.graph
                        .end_block_mut()
                        .register_entry_point(self.current_block_index, last_statement_index);
                } else {
                    self.graph
                        .end_block_mut()
                        .register_entry_point(self.current_block_index, 0);
                }
                None
            }
            Tree::Assignment(lvalue, operator, expression) => match *lvalue {
                Tree::LValueIdentifier(identifier) => {
                    if let Tree::Name(name, _) = *identifier {
                        let rhs = self.convert_boxed(expression).expect("Invalid expression!");
                        if let Token::Operator(_, operator_type) = operator {
                            match operator_type {
                                OperatorType::AssignMinus => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let desugar = self.create_sub(lhs, rhs);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignPlus => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let desugar = self.create_add(lhs, rhs);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignMul => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let desugar = self.create_mul(lhs, rhs);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignDiv => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let div = self.create_div(lhs, rhs);
                                    let desugar = self.create_div_mod_projection(div);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignMod => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let mod_node = self.create_mod(lhs, rhs);
                                    let desugar = self.create_div_mod_projection(mod_node);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignShiftLeft => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let desugar = self.create_shift_left(lhs, rhs);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignShiftRight => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let desugar = self.create_shift_right(lhs, rhs);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignBitwiseOr => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let desugar = self.create_or(lhs, rhs);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignBitwiseAnd => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let desugar = self.create_and(lhs, rhs);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::AssignBitwiseXor => {
                                    let lhs =
                                        self.read_variable(name.clone(), self.current_block_index);
                                    let desugar = self.create_xor(lhs, rhs);
                                    self.write_variable(name, self.current_block_index, desugar);
                                }
                                OperatorType::Assign => {
                                    self.write_variable(name, self.current_block_index, rhs);
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
                        self.write_variable(name, self.current_block_index, rhs);
                    }
                    None
                } else {
                    panic!("Identifier of declaration is not name!");
                }
            }
            Tree::IdentifierExpression(identifier) => {
                if let Tree::Name(name, _) = *identifier {
                    let value = self.read_variable(name, self.current_block_index);
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
                    let result = self.create_logical_not(node);
                    Some(result)
                }
                _ => panic!("Unregistered Unary Operation {:?}", operator_type),
            },
            Tree::Return(expression, _) => {
                let node = self.convert_boxed(expression).unwrap();
                self.create_return(node);
                None
            }
            Tree::Type(_, _) => None,
            Tree::BoolLiteral(boolean, _) => {
                let node = if boolean {
                    self.create_constant_int(1)
                } else {
                    self.create_constant_int(0)
                };
                Some(node)
            }
            Tree::Break(_) => {
                debug!(
                    "Generating IR for break statement with active loops: {:?}",
                    self.active_loop_exits
                );
                let jump = self.create_jump();
                self.graph
                    .get_block_mut(*self.active_loop_exits.last().unwrap())
                    .register_entry_point(self.current_block_index, jump);
                debug!(
                    "Modifying entry_points of {:?}",
                    self.graph
                        .get_block_mut(*self.active_loop_exits.last().unwrap())
                );
                None
            }
            Tree::Continue(_) => {
                debug!(
                    "Generating IR for continue statement with active loops: {:?}",
                    self.active_loop_entries
                );
                let jump = self.create_jump();
                self.graph
                    .get_block_mut(*self.active_loop_entries.last().unwrap())
                    .register_entry_point(self.current_block_index, jump);
                None
            }
            Tree::TernaryOperation(expression, true_value, false_value) => {
                debug!("Generating IR for TernaryOperation");
                let condition_node = self.convert_boxed(expression)?;
                let conditional_jump = self.create_conditional_jump(condition_node);

                let true_projection = self.create_true_projection(conditional_jump);
                let false_projection = self.create_false_projection(conditional_jump);

                let mut false_block = Block::new("ternary-false".to_string());
                false_block.register_entry_point(self.current_block_index, false_projection);
                let mut true_block = Block::new("ternary-true".to_string());
                true_block.register_entry_point(self.current_block_index, true_projection);
                self.seal_block(self.current_block_index);
                let mut following_block = Block::new("ternary-following".to_string());

                let false_block_index = self.graph.register_block(false_block);
                self.current_block_index = false_block_index;
                let false_expression = self.convert_boxed(false_value)?;
                let false_jump = self.create_jump();
                following_block.register_entry_point(self.current_block_index, false_jump);
                self.seal_block(false_block_index);

                let true_block_index = self.graph.register_block(true_block);
                self.current_block_index = true_block_index;
                let true_expression = self.convert_boxed(true_value)?;
                let true_jump = self.create_jump();
                following_block.register_entry_point(self.current_block_index, true_jump);
                self.seal_block(true_block_index);

                self.current_block_index = self.graph.register_block(following_block);
                self.seal_block(true_block_index);
                self.seal_block(false_block_index);
                let phi = self.create_phi_from_operands(vec![
                    (false_block_index, false_expression),
                    (true_block_index, true_expression),
                ]);
                self.seal_block(self.current_block_index);
                Some(phi)
            }
            Tree::While(condition, expression, _) => {
                debug!("Generating IR for while");
                let inverted_entry_condition_node =
                    self.create_inverted_condition(condition.clone());
                let entry_conditional_jump =
                    self.create_conditional_jump(inverted_entry_condition_node);

                let entry_true_projection = self.create_true_projection(entry_conditional_jump);
                let entry_false_projection = self.create_false_projection(entry_conditional_jump);

                let mut loop_body = Block::new("while-body".to_string());
                loop_body.register_entry_point(self.current_block_index, entry_false_projection);
                let loop_body_index = self.graph.register_block(loop_body);
                let loop_back_block = Block::new("while-condition".to_string());
                let loop_back_block_index = self.graph.register_block(loop_back_block);
                let mut following_block = Block::new("while-following".to_string());
                following_block
                    .register_entry_point(self.current_block_index, entry_true_projection);
                let following_block_index = self.graph.register_block(following_block);
                self.seal_block(self.current_block_index);

                self.current_block_index = loop_body_index;
                self.active_loop_entries.push(loop_back_block_index);
                self.active_loop_exits.push(following_block_index);
                self.convert_boxed(expression);
                let loop_body_jump = self.create_jump();
                self.graph
                    .get_block_mut(loop_back_block_index)
                    .register_entry_point(self.current_block_index, loop_body_jump);
                self.active_loop_entries.pop();
                self.active_loop_exits.pop();

                self.current_block_index = loop_back_block_index;
                self.seal_block(loop_back_block_index);
                let condition_node = self.convert_boxed(condition)?;
                let conditional_jump = self.create_conditional_jump(condition_node);

                let true_projection = self.create_true_projection(conditional_jump);
                let false_projection = self.create_false_projection(conditional_jump);

                self.graph
                    .get_block_mut(loop_body_index)
                    .register_entry_point(loop_back_block_index, true_projection);
                self.seal_block(loop_body_index);
                self.graph
                    .get_block_mut(following_block_index)
                    .register_entry_point(loop_back_block_index, false_projection);
                self.seal_block(loop_body_index);
                self.current_block_index = following_block_index;
                self.seal_block(self.current_block_index);
                None
            }
            Tree::If(condition, body, else_body, _) => {
                debug!("Generating IR for If");
                let condition_node = self.convert_boxed(condition)?;
                let conditional_jump = self.create_conditional_jump(condition_node);

                let true_projection = self.create_true_projection(conditional_jump);
                let false_projection = self.create_false_projection(conditional_jump);

                let mut false_block = Block::new("if-false".to_string());
                false_block.register_entry_point(self.current_block_index, false_projection);
                let mut true_block = Block::new("if-true".to_string());
                true_block.register_entry_point(self.current_block_index, true_projection);
                self.seal_block(self.current_block_index);
                let mut following_block = Block::new("if-following".to_string());

                let false_block_index = self.graph.register_block(false_block);
                self.seal_block(false_block_index);
                self.current_block_index = false_block_index;
                if let Some(else_statements) = else_body {
                    self.convert_boxed(else_statements);
                }
                let false_jump = self.create_jump();
                following_block.register_entry_point(self.current_block_index, false_jump);

                let true_block_index = self.graph.register_block(true_block);
                self.seal_block(true_block_index);
                self.current_block_index = true_block_index;
                self.convert_boxed(body);
                let true_jump = self.create_jump();
                following_block.register_entry_point(self.current_block_index, true_jump);

                self.current_block_index = self.graph.register_block(following_block);
                self.seal_block(self.current_block_index);
                None
            }
            Tree::For(option_initializer, condition, option_postincrement, expression, _) => {
                debug!("Generating IR for for expression");
                if let Some(initializer) = option_initializer {
                    self.convert_boxed(initializer);
                }
                let inverted_entry_condition_node =
                    self.create_inverted_condition(condition.clone());
                let entry_conditional_jump =
                    self.create_conditional_jump(inverted_entry_condition_node);

                let entry_true_projection = self.create_true_projection(entry_conditional_jump);
                let entry_false_projection = self.create_false_projection(entry_conditional_jump);

                let mut loop_body = Block::new("for-body".to_string());
                loop_body.register_entry_point(self.current_block_index, entry_false_projection);
                let loop_body_index = self.graph.register_block(loop_body);
                let mut following_block = Block::new("for-following".to_string());
                following_block
                    .register_entry_point(self.current_block_index, entry_true_projection);
                let following_block_index = self.graph.register_block(following_block);
                let loop_post = Block::new("for-post".to_string());
                let loop_post_index = self.graph.register_block(loop_post);

                self.seal_block(self.current_block_index);

                self.current_block_index = loop_body_index;
                self.active_loop_entries.push(loop_post_index);
                self.active_loop_exits.push(following_block_index);
                self.convert_boxed(expression);
                self.active_loop_entries.pop();
                self.active_loop_exits.pop();
                let loop_body_exit = self.create_jump();

                self.graph
                    .get_block_mut(loop_post_index)
                    .register_entry_point(self.current_block_index, loop_body_exit);
                self.current_block_index = loop_post_index;
                self.seal_block(loop_post_index);

                if let Some(postincrement) = option_postincrement {
                    self.convert_boxed(postincrement);
                }

                let condition_node = self.convert_boxed(condition)?;
                let conditional_jump = self.create_conditional_jump(condition_node);

                let true_projection = self.create_true_projection(conditional_jump);
                let false_projection = self.create_false_projection(conditional_jump);

                self.graph
                    .get_block_mut(loop_body_index)
                    .register_entry_point(loop_post_index, true_projection);

                self.seal_block(loop_body_index);
                self.graph
                    .get_block_mut(following_block_index)
                    .register_entry_point(loop_post_index, false_projection);
                self.seal_block(loop_body_index);
                self.current_block_index = following_block_index;
                self.seal_block(self.current_block_index);
                debug!("Following block after for: {}", self.current_block_index);
                None
            }
            #[allow(unreachable_patterns)]
            node => todo!("Unimplemented {:?}", node),
        }
    }

    pub fn convert_boxed(&mut self, tree: Box<Tree>) -> Option<NodeIndex> {
        self.convert(*tree)
    }

    pub fn create_conditional_jump(&mut self, condition: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::ConditionalJump(UnaryOperationData::new(condition)))
    }

    // TODO: Refactor
    pub fn process_branch(&mut self, a: Box<Tree>, _b: usize, label: &str) -> usize {
        let block = Block::new(format!("if-body-{}", label));
        self.current_block_index = self.graph.register_block(block);
        //self.graph.get_node_mut(block).predecessors_mut().push(b);
        self.seal_block(self.current_block_index);
        self.convert_boxed(a);
        self.seal_block(self.current_block_index);
        self.create_jump()
    }

    fn create_jump(&mut self) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Jump)
    }

    fn create_add(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Add(BinaryOperationData::new(lhs, rhs)))
    }

    fn create_inverted_condition(&mut self, condition: Box<Tree>) -> NodeIndex {
        if let Tree::BinaryOperation(lhs, rhs, operator) = *condition {
            let left_node = self.convert_boxed(lhs).unwrap();
            let right_node = self.convert_boxed(rhs).unwrap();
            let current_block = self.graph.get_block_mut(self.current_block_index);
            match operator {
                OperatorType::TernaryColon
                | OperatorType::Mul
                | OperatorType::Div
                | OperatorType::Mod
                | OperatorType::Plus
                | OperatorType::Assign
                | OperatorType::AssignMul
                | OperatorType::AssignDiv
                | OperatorType::AssignMod
                | OperatorType::ShiftLeft
                | OperatorType::BitwiseOr
                | OperatorType::LogicalOr
                | OperatorType::AssignPlus
                | OperatorType::LogicalNot
                | OperatorType::BitwiseNot
                | OperatorType::ShiftRight
                | OperatorType::BitwiseAnd
                | OperatorType::BitwiseXor
                | OperatorType::Minus => panic!("Invalid operator for condition!"),
                OperatorType::Lower => current_block.register_node(Node::HigherEquals(
                    BinaryOperationData::new(left_node, right_node),
                )),
                OperatorType::Higher => current_block.register_node(Node::LowerEquals(
                    BinaryOperationData::new(left_node, right_node),
                )),
                OperatorType::Equals => current_block.register_node(Node::NotEquals(
                    BinaryOperationData::new(left_node, right_node),
                )),
                OperatorType::NotEquals => current_block.register_node(Node::Equals(
                    BinaryOperationData::new(left_node, right_node),
                )),
                OperatorType::LowerEquals => current_block.register_node(Node::Higher(
                    BinaryOperationData::new(left_node, right_node),
                )),
                OperatorType::HigherEquals => current_block
                    .register_node(Node::Lower(BinaryOperationData::new(left_node, right_node))),
                OperatorType::LogicalAnd => todo!(),
                _ => unimplemented!("Unimplemented inverted operator {:?}", operator),
            }
        } else if let Tree::IdentifierExpression(_) = *condition {
            let variable = self.convert_boxed(condition).unwrap();
            self.create_logical_not(variable)
        } else if let Tree::BoolLiteral(value, _) = *condition {
            if value {
                self.create_constant_int(0)
            } else {
                self.create_constant_int(1)
            }
        } else {
            panic!("Condition is not a binary operation, got {:?}", condition);
        }
    }

    fn create_sub(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Subtraction(BinaryOperationData::new(lhs, rhs)))
    }

    fn create_mul(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Multiplication(BinaryOperationData::new(lhs, rhs)))
    }

    fn create_div(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Division(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_mod(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Modulo(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_shift_left(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::ShiftLeft(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_shift_right(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::ShiftRight(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_lower(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Lower(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_lower_equals(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::LowerEquals(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_equals(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Equals(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_not_equals(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Lower(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_higher_equals(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::HigherEquals(
            BinaryOperationData::new_with_sideeffect(lhs, rhs, sideeffect),
        ))
    }

    fn create_higher(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Higher(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_or(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Or(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_and(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::And(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_xor(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Xor(BinaryOperationData::new_with_sideeffect(
            lhs, rhs, sideeffect,
        )))
    }

    fn create_bitwise_not(&mut self, node: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::BitwiseNegate(
            UnaryOperationData::new_with_sideeffect(node, sideeffect),
        ))
    }

    fn create_logical_not(&mut self, node: NodeIndex) -> NodeIndex {
        let sideeffect = self.read_current_side_effect();
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::LogicalNot(UnaryOperationData::new_with_sideeffect(
            node, sideeffect,
        )))
    }

    fn create_constant_int(&mut self, value: i32) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::ConstantInt(ConstantIntData::new(value)))
    }

    fn _create_constant_bool(&mut self, value: bool) -> NodeIndex {
        let start_block = self.graph.get_block_mut(START_BLOCK);
        start_block.register_node(Node::ConstantBool(ConstantBoolData::new(value)))
    }

    fn create_return(&mut self, input: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        let return_node_index = current_block.register_node(Node::Return(ReturnData::new(input)));
        self.graph
            .end_block_mut()
            .register_entry_point(self.current_block_index, return_node_index);
        return_node_index
    }

    fn _create_phi(&mut self) -> usize {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Phi(PhiData::empty()))
    }

    fn create_phi_from_operands(&mut self, operands: Vec<(BlockIndex, NodeIndex)>) -> usize {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Phi(PhiData::new(operands)))
    }

    fn _create_phi_operands(&mut self, block_index: BlockIndex) -> NodeIndex {
        let block = self.graph.get_block(block_index);
        let operands = block
            .entry_points()
            .iter()
            .flat_map(|(v1, v2)| v2.iter().map(|v| (v1.clone(), v)))
            .map(|(v1, v2)| (v1, *v2))
            .collect();
        trace!("Creating phi with operands {:?}", operands);
        let current_block = self.graph.get_block_mut(block_index);
        current_block.register_node(Node::Phi(PhiData::new(operands)))
    }

    fn create_phi_variable_operands(
        &mut self,
        phi: usize,
        block_index: BlockIndex,
        variable: Name,
    ) -> NodeIndex {
        // Creating the operands for a block, by iterating over its entry points and reading the
        // variable there
        let mut operands = Vec::new();
        for (block_index, _) in self.graph.get_block(block_index).entry_points().clone() {
            operands.push((
                block_index,
                self.read_variable(variable.clone(), block_index),
            ));
        }
        trace!(
            "Created phi operands for block {} whilst reading {:?}: {:?}",
            block_index,
            variable,
            operands
        );

        if let Node::Phi(data) = self.graph.get_block_mut(block_index).get_node_mut(phi) {
            for operand in operands {
                data.add_operand(operand);
            }
        }
        phi
    }

    fn create_div_mod_projection(&mut self, input: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        let projection_side_effect = current_block.register_node(Node::Projection(
            ProjectionData::new(input, ProjectionInformation::SideEffect),
        ));
        let result_projection = current_block.register_node(Node::Projection(ProjectionData::new(
            input,
            ProjectionInformation::Result,
        )));
        self.write_current_side_effect(projection_side_effect);
        result_projection
    }

    fn write_variable(&mut self, variable: Name, block: usize, node: usize) {
        trace!("Trying to write into variable {:?}", variable);
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

    fn read_variable(&mut self, variable: Name, block: BlockIndex) -> NodeIndex {
        trace!(
            "Trying to read from variable {:?} in block {}",
            variable,
            block
        );
        if self.current_definitions.contains_key(&variable) {
            if self
                .current_definitions
                .get(&variable)
                .unwrap()
                .contains_key(&block)
            {
                trace!("Variable defined in the same block! Returning value");
                *self
                    .current_definitions
                    .get(&variable)
                    .unwrap()
                    .get(&block)
                    .unwrap()
            } else {
                self.read_variable_recursive(variable, block)
            }
        } else {
            self.read_variable_recursive(variable, block)
        }
    }

    fn read_variable_recursive(&mut self, variable: Name, block_index: BlockIndex) -> NodeIndex {
        trace!(
            "Reading variable {:?} recursively in block {}",
            variable,
            block_index
        );
        trace!("Sealed blocks: {:?}", self.sealed_blocks);
        let node = if !self.sealed_blocks.contains(&block_index) {
            // Current block is not sealed yet, the list of operands is not final yet
            let phi = self
                .graph
                .get_block_mut(block_index)
                .register_node(Node::Phi(PhiData::empty()));
            trace!("Writing incomplete phi: ({:?}, {})", variable, phi);
            if self.incomplete_phis.contains_key(&block_index) {
                let mut entry = self.incomplete_phis.get_mut(&block_index).unwrap().clone();
                entry.insert(variable.clone(), phi);
                self.incomplete_phis.insert(block_index, entry);
            } else {
                self.incomplete_phis
                    .insert(block_index, HashMap::from([(variable.clone(), phi)]));
            };
            phi
        } else if self.graph.get_block(block_index).entry_points().len() == 1 {
            // The block we are reading the variable in is sealed and has one previous block.
            // We can read the variable from there
            let previous_block = self
                .graph
                .get_block(block_index)
                .entry_points()
                .iter()
                .last()
                .unwrap()
                .0
                .clone();
            let defining_node = self.read_variable(variable.clone(), previous_block);
            let phi = self
                .graph
                .get_block_mut(block_index)
                .register_node(Node::Phi(PhiData::new(vec![(
                    previous_block,
                    defining_node,
                )])));
            phi
        } else {
            // The block we are reading the variable in has multiple entry points and is sealed.
            // The value for the variable can come from multiple previous blocks.
            // The value of the variable is dependent on the values of the variable in the previous
            // blocks
            let phi = self
                .graph
                .get_block_mut(block_index)
                .register_node(Node::Phi(PhiData::empty()));
            self.write_variable(variable.clone(), block_index, phi);
            self.create_phi_variable_operands(phi, block_index, variable.clone());
            phi
        };

        // Denote that the newly created phi defines the variable in the current block
        self.write_variable(variable.clone(), block_index, node);
        node
    }

    fn seal_block(&mut self, block: BlockIndex) {
        debug!(
            "Current graph before sealing block {}: {}",
            block, self.graph
        );
        info!("Incomplete Phis: {:?}", self.incomplete_phis);
        if !self.incomplete_phis.contains_key(&block) {
            self.sealed_blocks.push(block);
            return;
        }
        for (block_index, definitions) in &self.incomplete_phis.clone() {
            if block.ne(block_index) {
                continue;
            }
            for (variable, phi) in definitions {
                let operands = {
                    let mut operands = Vec::new();
                    let phi_block = self.graph.get_block_mut(*block_index);
                    for (prev_block, _prev_nodes) in phi_block.entry_points().clone() {
                        operands
                            .push((prev_block, self.read_variable(variable.clone(), prev_block)));
                    }
                    operands
                };
                let block = self.graph.get_block_mut(*block_index);
                if let Node::Phi(data) = block.get_node_mut(*phi) {
                    for operand in operands {
                        data.add_operand(operand);
                    }
                }
            }
        }
        self.sealed_blocks.push(block);
    }

    fn write_current_side_effect(&mut self, node: usize) {
        self.write_side_effect(self.current_block_index, node);
    }

    fn write_side_effect(&mut self, block: usize, node: usize) {
        self.current_side_effect.insert(block, node);
    }

    fn read_current_side_effect(&mut self) -> usize {
        return 0;
        //self.read_side_effect(self.current_block_index)
    }

    fn _read_side_effect(&mut self, block: usize) -> usize {
        if self.current_side_effect.contains_key(&block) {
            *self.current_side_effect.get(&block).unwrap()
        } else {
            self._read_side_effect_recusive(block)
        }
    }

    fn _read_side_effect_recusive(&mut self, block: usize) -> usize {
        let node = if !self.sealed_blocks.contains(&block) {
            let phi = self._create_phi();
            let old_phi = self._incomplete_side_effect_phis.insert(block, phi);
            if old_phi.is_some() {
                panic!("Double read side effect recursive!");
            }
            phi
        } else if self.graph.get_block(block).entry_points().len() == 1 {
            let (previous_block, _) = self
                .graph
                .get_block(block)
                .entry_points()
                .iter()
                .last()
                .unwrap();
            self._read_side_effect(*previous_block)
        } else {
            let phi = self._create_phi_operands(block);
            self.write_side_effect(block, phi);
            phi
        };
        self.write_side_effect(block, node);
        node
    }

    pub fn graph(self) -> IRGraph {
        self.graph
    }

    pub fn create_true_projection(&mut self, conditional_jump: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Projection(ProjectionData::new(
            conditional_jump,
            ProjectionInformation::IfTrue,
        )))
    }

    pub fn create_false_projection(&mut self, conditional_jump: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Projection(ProjectionData::new(
            conditional_jump,
            ProjectionInformation::IfFalse,
        )))
    }
}

impl Default for IRGraphConstructor {
    fn default() -> Self {
        Self::new()
    }
}

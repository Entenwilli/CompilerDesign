use core::panic;
use std::collections::HashMap;

use tracing::{debug, trace};

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
    current_definitions: HashMap<Name, HashMap<usize, usize>>,
    incomplete_phis: HashMap<usize, HashMap<Name, usize>>,
    current_side_effect: HashMap<usize, usize>,
    incomplete_side_effect_phis: HashMap<usize, usize>,
    sealed_blocks: Vec<usize>,
    current_block_index: BlockIndex,
}

impl IRGraphConstructor {
    pub fn new() -> IRGraphConstructor {
        IRGraphConstructor {
            graph: IRGraph::new(),
            current_definitions: HashMap::new(),
            incomplete_phis: HashMap::new(),
            current_side_effect: HashMap::new(),
            incomplete_side_effect_phis: HashMap::new(),
            // Start Block never gets more predecessors
            sealed_blocks: vec![0],
            current_block_index: START_BLOCK,
        }
    }

    pub fn convert(&mut self, tree: Tree) -> Option<usize> {
        debug!("Converting AST {} to IR!", tree);
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

                // The last statement after parsing the body can exit the function
                let last_statement_index = self
                    .graph
                    .get_block(self.current_block_index)
                    .get_last_node_index();
                self.graph
                    .end_block_mut()
                    .register_entry_point(self.current_block_index, last_statement_index);
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
                    let result = self.create_bitwise_not(node);
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

                let mut false_block = Block::new("ternary-false".to_string());
                false_block.register_entry_point(self.current_block_index, false_projection);
                let mut true_block = Block::new("ternary-true".to_string());
                true_block.register_entry_point(self.current_block_index, true_projection);
                self.seal_block(self.current_block_index);

                let false_block_index = self.graph.register_block(false_block);
                self.current_block_index = false_block_index;
                let false_expression = self.convert_boxed(false_value)?;
                let false_jump = self.create_jump();

                let true_block_index = self.graph.register_block(true_block);
                self.current_block_index = true_block_index;
                let true_expression = self.convert_boxed(true_value)?;
                let true_jump = self.create_jump();

                let mut following_block = Block::new("ternary-following".to_string());
                following_block.register_entry_point(false_block_index, false_jump);
                following_block.register_entry_point(true_block_index, true_jump);
                self.current_block_index = self.graph.register_block(following_block);
                self.seal_block(true_block_index);
                self.seal_block(false_block_index);
                let phi = self.create_phi_from_operands(vec![
                    (false_block_index, false_expression),
                    (true_block_index, true_expression),
                ]);
                Some(phi)
            }
            Tree::While(_condition, _expression, _) => {
                todo!("Implement while")
            }
            Tree::If(_condition, _body, _else_body, _) => {
                todo!("Implement if")
            }
            Tree::For(_option_initializer, _comparison, _option_postincrement, _statement, _) => {
                todo!("Implement for")
            }
            #[allow(unreachable_patterns)]
            node => todo!("Unimplemented {:?}", node),
        }
    }

    pub fn convert_boxed(&mut self, tree: Box<Tree>) -> Option<usize> {
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

    fn create_constant_int(&mut self, value: i32) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::ConstantInt(ConstantIntData::new(value)))
    }

    fn create_constant_bool(&mut self, value: bool) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::ConstantBool(ConstantBoolData::new(value)))
    }

    fn create_return(&mut self, input: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        let return_node_index = current_block.register_node(Node::Return(ReturnData::new(input)));
        self.graph
            .end_block_mut()
            .register_entry_point(self.current_block_index, return_node_index);
        return_node_index
    }

    fn create_phi(&mut self) -> usize {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Phi(PhiData::empty()))
    }

    fn create_phi_from_operands(&mut self, operands: Vec<(BlockIndex, NodeIndex)>) -> usize {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Phi(PhiData::new(operands)))
    }

    fn create_phi_operands(&mut self, block_index: BlockIndex) -> NodeIndex {
        let block = self.graph.get_block(block_index);
        let operands = block
            .entry_points()
            .iter()
            .map(|(v1, v2)| (*v1, *v2))
            .collect();
        trace!("Creating phi with operands {:?}", operands);
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Phi(PhiData::new(operands)))
    }

    fn create_phi_variable_operands(
        &mut self,
        block_index: BlockIndex,
        variable: Name,
    ) -> NodeIndex {
        let mut operands = Vec::new();
        for (block_index, _) in self.graph.get_block(block_index).entry_points().clone() {
            operands.push((
                block_index,
                self.read_variable(variable.clone(), block_index),
            ));
        }
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Phi(PhiData::new(operands)))
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

    fn read_variable(&mut self, variable: Name, block: usize) -> usize {
        trace!("Trying to read from variable {:?}", variable);
        if self.current_definitions.contains_key(&variable) {
            if self
                .current_definitions
                .get(&variable)
                .unwrap()
                .contains_key(&block)
            {
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

    fn read_variable_recursive(&mut self, variable: Name, block_index: BlockIndex) -> usize {
        let node = if !self.sealed_blocks.contains(&block_index) {
            let phi = self.create_phi();
            trace!("Writing incomplete phi: ({:?}, {})", variable, phi);
            if let std::collections::hash_map::Entry::Vacant(e) =
                self.incomplete_phis.entry(block_index)
            {
                e.insert(HashMap::from([(variable.clone(), phi)]));
            } else {
                self.incomplete_phis
                    .get_mut(&block_index)
                    .unwrap()
                    .insert(variable.clone(), phi);
            }
            phi
        } else if self.graph.get_block(block_index).entry_points().len() == 1 {
            let (previous_block, _) = self
                .graph
                .get_block(block_index)
                .entry_points()
                .iter()
                .last()
                .unwrap();
            self.read_variable(variable.clone(), *previous_block)
        } else {
            let phi = self.create_phi_variable_operands(block_index, variable.clone());
            self.write_variable(variable.clone(), block_index, phi);
            phi
        };
        self.write_variable(variable.clone(), block_index, node);
        node
    }

    fn seal_block(&mut self, block: BlockIndex) {
        trace!("Current graph before sealing block: {}", self.graph);
        trace!("Incomplete Phis: {:?}", self.incomplete_phis);
        if !self.incomplete_phis.contains_key(&block) {
            self.sealed_blocks.push(block);
            return;
        }
        for (variable, index) in self.incomplete_phis.get(&block).unwrap().clone() {
            let operands = {
                let mut operands = Vec::new();
                let block = self.graph.get_block_mut(block);
                if let Node::Phi(data) = block.get_node_mut(index) {
                    for (block_index, _) in data.operands() {
                        operands.push((
                            block_index,
                            self.read_variable(variable.clone(), block_index),
                        ));
                    }
                }
                operands
            };
            let block = self.graph.get_block_mut(block);
            if let Node::Phi(data) = block.get_node_mut(index) {
                for operand in operands {
                    data.add_operand(operand);
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
        self.read_side_effect(self.current_block_index)
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
        } else if self.graph.get_block(block).entry_points().len() == 1 {
            let (previous_block, _) = self
                .graph
                .get_block(block)
                .entry_points()
                .iter()
                .last()
                .unwrap();
            self.read_side_effect(*previous_block)
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

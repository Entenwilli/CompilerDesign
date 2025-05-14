use core::panic;
use std::collections::HashMap;

use crate::{
    lexer::token::{OperatorType, Token},
    parser::{ast::Tree, symbols::Name},
    util::int_parsing::parse_int,
};

use super::{
    graph::{IRGraph, START_BLOCK},
    node::{
        AddData, ConstantIntData, DivisionData, ModuloData, MultiplicationData, Node, PhiData,
        ProjectionData, ProjectionInformation, ReturnData, StartData, SubtractionData,
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
    current_block: usize,
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
            current_block: 0,
        }
    }

    pub fn convert(&mut self, tree: Tree) -> Option<usize> {
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
                    _ => panic!("Not a binary operator"),
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
            Tree::Negate(expression, _) => {
                let node = self.convert_boxed(expression).unwrap();
                let zero = self.create_constant_int(0);
                let result = self.create_sub(zero, node);
                Some(result)
            }
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
        }
    }

    pub fn convert_boxed(&mut self, tree: Box<Tree>) -> Option<usize> {
        self.convert(*tree)
    }

    fn create_start_block(&mut self) -> usize {
        self.graph
            .register_node(Node::Start(StartData::new(self.current_block)))
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

    fn create_return(&mut self, result: usize) -> usize {
        let current_side_effect = self.read_current_side_effect();
        self.graph.register_node(Node::Return(ReturnData::new(
            self.current_block,
            result,
            current_side_effect,
        )))
    }

    fn create_constant_int(&mut self, value: i32) -> usize {
        // Always move const into start block, allows for better deduplication
        self.graph.register_node(
            self.optimizer
                .transform(Node::ConstantInt(ConstantIntData::new(START_BLOCK, value))),
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

    fn _seal_block(&mut self, block: usize) {
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
}

impl Default for IRGraphConstructor {
    fn default() -> Self {
        Self::new()
    }
}

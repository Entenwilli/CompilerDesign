use std::{collections::HashMap, usize};

use tracing::{debug, info, trace};

use crate::{
    ir::{
        block::{Block, NodeIndex},
        graph::{BlockIndex, IRGraph, START_BLOCK},
        node::{
            binary_operation::BinaryOperationData,
            projection::{ProjectionData, ProjectionInformation},
            unary_operation::UnaryOperationData,
            ConstantBoolData, ConstantIntData, Node, PhiData, ReturnData,
        },
    },
    lexer::operator::BinaryOperator,
    parser::{ast::expression_tree::ExpressionTree, symbols::Name},
};

pub mod assignment_tree;
pub mod binary_operation_tree;
pub mod block_tree;
pub mod break_tree;
pub mod call_tree;
pub mod continue_tree;
pub mod declaration_tree;
pub mod expression_tree;
pub mod for_tree;
pub mod function_tree;
pub mod identifier_tree;
pub mod if_tree;
pub mod literal_tree;
pub mod program_tree;
pub mod return_tree;
pub mod statement_tree;
pub mod ternary_operation_tree;
pub mod unary_operation_tree;
pub mod while_tree;

pub struct IRConstructor {
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

pub trait ToIR {
    fn to_ir(&self, constructor: &mut IRConstructor) -> Option<NodeIndex>;
}

impl IRConstructor {
    pub fn new() -> IRConstructor {
        IRConstructor {
            graph: IRGraph::new(),
            current_definitions: HashMap::new(),
            incomplete_phis: HashMap::new(),
            current_side_effect: HashMap::new(),
            _incomplete_side_effect_phis: HashMap::new(),
            sealed_blocks: vec![START_BLOCK],
            current_block_index: START_BLOCK,
            active_loop_entries: Vec::new(),
            active_loop_exits: Vec::new(),
        }
    }

    pub fn current_block(&self) -> BlockIndex {
        self.current_block_index
    }

    pub fn set_current_block(&mut self, block_index: BlockIndex) {
        self.current_block_index = block_index;
    }

    pub fn get_block(&self, block_index: BlockIndex) -> &Block {
        self.graph.get_block(block_index)
    }

    pub fn get_block_mut(&mut self, block_index: BlockIndex) -> &mut Block {
        self.graph.get_block_mut(block_index)
    }

    pub fn register_entry_point(
        &mut self,
        block_target: BlockIndex,
        block_origin: BlockIndex,
        node_origin: NodeIndex,
    ) {
        self.graph
            .get_block_mut(block_target)
            .register_entry_point(block_origin, node_origin);
    }

    pub fn register_block(&mut self, block: Block) -> BlockIndex {
        self.graph.register_block(block)
    }

    fn create_jump(&mut self) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Jump)
    }

    fn create_add(&mut self, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::Add(BinaryOperationData::new(lhs, rhs)))
    }

    fn create_inverted_condition(&mut self, condition: &ExpressionTree) -> NodeIndex {
        match condition {
            ExpressionTree::BinaryOperationTree(tree) => {
                let left_node = tree
                    .lhs()
                    .to_ir(self)
                    .expect("Expected LHS to be expression");
                let right_node = tree
                    .rhs()
                    .to_ir(self)
                    .expect("Expected RHS to be expression");
                let current_block = self.graph.get_block_mut(self.current_block_index);
                match tree.operator() {
                    BinaryOperator::Lower => current_block.register_node(Node::HigherEquals(
                        BinaryOperationData::new(left_node, right_node),
                    )),
                    BinaryOperator::Higher => current_block.register_node(Node::LowerEquals(
                        BinaryOperationData::new(left_node, right_node),
                    )),
                    BinaryOperator::Equals => current_block.register_node(Node::NotEquals(
                        BinaryOperationData::new(left_node, right_node),
                    )),
                    BinaryOperator::NotEquals => current_block.register_node(Node::Equals(
                        BinaryOperationData::new(left_node, right_node),
                    )),
                    BinaryOperator::LowerEquals => current_block.register_node(Node::Higher(
                        BinaryOperationData::new(left_node, right_node),
                    )),
                    BinaryOperator::HigherEquals => current_block.register_node(Node::Lower(
                        BinaryOperationData::new(left_node, right_node),
                    )),
                    BinaryOperator::LogicalAnd => todo!(),
                    BinaryOperator::Mul
                    | BinaryOperator::Div
                    | BinaryOperator::Mod
                    | BinaryOperator::Plus
                    | BinaryOperator::ShiftLeft
                    | BinaryOperator::BitwiseOr
                    | BinaryOperator::LogicalOr
                    | BinaryOperator::ShiftRight
                    | BinaryOperator::BitwiseAnd
                    | BinaryOperator::BitwiseXor
                    | BinaryOperator::Minus => {
                        unreachable!("Ensured by semantic analysis!")
                    }
                }
            }
            ExpressionTree::BooleanLiteralTree(tree) => {
                if tree.value() {
                    self.create_constant_int(0)
                } else {
                    self.create_constant_int(1)
                }
            }
            ExpressionTree::IdentifierExpressionTree(tree) => {
                let variable = tree
                    .to_ir(self)
                    .expect("Expected Identfier to be expression");
                self.create_logical_not(variable)
            }
            ExpressionTree::IntegerLiteralTree(_) => {
                unreachable!("Integer literal tree cannot occur in top level condition")
            }
            ExpressionTree::UnaryOperationTree(_tree) => todo!(),
            ExpressionTree::TernaryOperationTree(_tree) => todo!(),
            ExpressionTree::CallTree(_tree) => todo!(),
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

    pub fn create_conditional_jump(&mut self, condition: NodeIndex) -> NodeIndex {
        let current_block = self.graph.get_block_mut(self.current_block_index);
        current_block.register_node(Node::ConditionalJump(UnaryOperationData::new(condition)))
    }

    pub fn process_branch(&mut self, branch: &ExpressionTree, label: &str) -> usize {
        let block = Block::new(format!("if-body-{}", label));
        self.current_block_index = self.graph.register_block(block);
        self.seal_block(self.current_block_index);
        branch.to_ir(self);
        self.seal_block(self.current_block_index);
        self.create_jump()
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

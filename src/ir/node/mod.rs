pub mod binary_operation;
pub mod projection;
pub mod unary_operation;

use std::fmt::Display;

use binary_operation::BinaryOperationData;
use projection::ProjectionData;
use unary_operation::UnaryOperationData;

use super::{block::NodeIndex, graph::BlockIndex};

#[derive(Eq, Hash, PartialEq, Debug)]
pub enum Node {
    Add(BinaryOperationData),
    ConstantInt(ConstantIntData),
    ConstantBool(ConstantBoolData),
    Division(BinaryOperationData),
    Modulo(BinaryOperationData),
    Multiplication(BinaryOperationData),
    Phi(PhiData),
    Projection(ProjectionData),
    Return(ReturnData),
    Subtraction(BinaryOperationData),
    ShiftLeft(BinaryOperationData),
    ShiftRight(BinaryOperationData),
    Lower(BinaryOperationData),
    LowerEquals(BinaryOperationData),
    Equals(BinaryOperationData),
    NotEquals(BinaryOperationData),
    HigherEquals(BinaryOperationData),
    Higher(BinaryOperationData),
    BitwiseNegate(UnaryOperationData),
    Or(BinaryOperationData),
    And(BinaryOperationData),
    Xor(BinaryOperationData),
    ConditionalJump(UnaryOperationData),
    Jump,
}

impl Node {
    pub fn predecessors(&self) -> Vec<usize> {
        match self {
            Node::Jump | Node::ConstantBool(_) | Node::ConstantInt(_) => vec![],
            Node::Projection(data) => vec![data.input().1],
            Node::Phi(_data) => todo!("What to return?"),
            Node::Return(data) => vec![data.input().1],
            Node::ConditionalJump(data) | Node::BitwiseNegate(data) => vec![data.input().1],
            Node::Add(data)
            | Node::Division(data)
            | Node::ShiftRight(data)
            | Node::Lower(data)
            | Node::Higher(data)
            | Node::LowerEquals(data)
            | Node::HigherEquals(data)
            | Node::Equals(data)
            | Node::NotEquals(data)
            | Node::Multiplication(data)
            | Node::Modulo(data)
            | Node::And(data)
            | Node::Xor(data)
            | Node::Or(data)
            | Node::Subtraction(data)
            | Node::ShiftLeft(data) => vec![data.lhs().1, data.rhs().1],
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ConstantIntData {
    value: i32,
}

impl ConstantIntData {
    pub fn new(value: i32) -> ConstantIntData {
        ConstantIntData { value }
    }

    pub fn value(&self) -> i32 {
        self.value
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ConstantBoolData {
    value: bool,
}

impl ConstantBoolData {
    pub fn new(value: bool) -> ConstantBoolData {
        ConstantBoolData { value }
    }

    pub fn value(&self) -> bool {
        self.value
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PhiData {
    operands: Vec<(BlockIndex, NodeIndex)>,
}

impl PhiData {
    pub fn empty() -> PhiData {
        PhiData { operands: vec![] }
    }

    pub fn new(operands: Vec<(BlockIndex, NodeIndex)>) -> PhiData {
        PhiData { operands }
    }

    pub fn add_operand(&mut self, operand: (BlockIndex, NodeIndex)) {
        match self.operands.binary_search(&operand) {
            Ok(_) => {}
            Err(pos) => self.operands.insert(pos, operand),
        }
    }

    pub fn operands(&self) -> Vec<(BlockIndex, NodeIndex)> {
        self.operands.iter().copied().collect()
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ReturnData {
    input: (BlockIndex, NodeIndex),
}

impl ReturnData {
    pub fn new(input: (BlockIndex, NodeIndex)) -> ReturnData {
        ReturnData { input }
    }

    pub fn input(&self) -> (BlockIndex, NodeIndex) {
        self.input
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

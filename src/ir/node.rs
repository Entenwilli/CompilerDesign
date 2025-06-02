use std::fmt::Display;

use crate::lexer::token::OperatorType;

#[derive(Eq, Hash, PartialEq, Debug)]
pub enum Node {
    Add(AddData),
    Block(BlockData),
    ConstantInt(ConstantIntData),
    ConstantBool(ConstantBoolData),
    Division(DivisionData),
    Modulo(ModuloData),
    Multiplication(MultiplicationData),
    Phi(PhiData),
    Projection(ProjectionData),
    Return(ReturnData),
    Start(StartData),
    Subtraction(SubtractionData),
    ShiftLeft(ShiftLeftData),
    ShiftRight(ShiftRightData),
    Lower(LowerData),
    LowerEquals(LowerEqualsData),
    Equals(EqualsData),
    NotEquals(NotEqualsData),
    HigherEquals(HigherEqualsData),
    Higher(HigherData),
    BitwiseNegate(BitwiseNegateData),
    Or(OrData),
    And(AndData),
    Xor(XorData),
    NoOp(NodeData),
    ConditionalJump(ConditionalJumpData),
    Jump(JumpData),
}

impl Node {
    pub fn predecessors(&self) -> &Vec<usize> {
        match self {
            Node::Add(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Block(data) => data.node_data.predecessors(),
            Node::ConstantInt(data) => data.node_data.predecessors(),
            Node::ConstantBool(data) => data.node_data.predecessors(),
            Node::Division(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Modulo(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Multiplication(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Phi(data) => data.node_data.predecessors(),
            Node::Projection(data) => data.node_data.predecessors(),
            Node::Return(data) => data.node_data.predecessors(),
            Node::Start(data) => data.node_data.predecessors(),
            Node::Subtraction(data) => data.binary_operation_data.node_data.predecessors(),
            Node::ShiftLeft(data) => data.binary_operation_data.node_data.predecessors(),
            Node::ShiftRight(data) => data.binary_operation_data.node_data.predecessors(),
            Node::BitwiseNegate(data) => data.node_data.predecessors(),
            Node::Lower(data) => data.binary_operation_data.node_data.predecessors(),
            Node::LowerEquals(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Equals(data) => data.binary_operation_data.node_data.predecessors(),
            Node::NotEquals(data) => data.binary_operation_data.node_data.predecessors(),
            Node::HigherEquals(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Higher(data) => data.binary_operation_data.node_data.predecessors(),
            Node::And(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Or(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Xor(data) => data.binary_operation_data.node_data.predecessors(),
            Node::NoOp(data) => data.predecessors(),
            Node::ConditionalJump(data) => data.node_data.predecessors(),
            Node::Jump(data) => data.node_data.predecessors(),
        }
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<usize> {
        match self {
            Node::Add(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Block(data) => data.node_data.predecessors_mut(),
            Node::ConstantInt(data) => data.node_data.predecessors_mut(),
            Node::ConstantBool(data) => data.node_data.predecessors_mut(),
            Node::Division(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Modulo(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Multiplication(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Phi(data) => data.node_data.predecessors_mut(),
            Node::Projection(data) => data.node_data.predecessors_mut(),
            Node::Return(data) => data.node_data.predecessors_mut(),
            Node::Start(data) => data.node_data.predecessors_mut(),
            Node::Subtraction(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::ShiftLeft(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::ShiftRight(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::BitwiseNegate(data) => data.node_data.predecessors_mut(),
            Node::Lower(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::LowerEquals(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Equals(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::NotEquals(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::HigherEquals(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Higher(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::And(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Or(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Xor(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::NoOp(data) => data.predecessors_mut(),
            Node::ConditionalJump(data) => data.node_data.predecessors_mut(),
            Node::Jump(data) => data.node_data.predecessors_mut(),
        }
    }

    pub fn block(&self) -> usize {
        match self {
            Node::Add(data) => data.binary_operation_data.node_data.block(),
            Node::Block(data) => data.node_data.block(),
            Node::ConstantInt(data) => data.node_data.block(),
            Node::ConstantBool(data) => data.node_data.block(),
            Node::Division(data) => data.binary_operation_data.node_data.block(),
            Node::Modulo(data) => data.binary_operation_data.node_data.block(),
            Node::Multiplication(data) => data.binary_operation_data.node_data.block(),
            Node::Phi(data) => data.node_data.block(),
            Node::Projection(data) => data.node_data.block(),
            Node::Return(data) => data.node_data.block(),
            Node::Start(data) => data.node_data.block(),
            Node::Subtraction(data) => data.binary_operation_data.node_data.block(),
            Node::ShiftLeft(data) => data.binary_operation_data.node_data.block(),
            Node::ShiftRight(data) => data.binary_operation_data.node_data.block(),
            Node::BitwiseNegate(data) => data.node_data.block(),
            Node::Lower(data) => data.binary_operation_data.node_data.block(),
            Node::LowerEquals(data) => data.binary_operation_data.node_data.block(),
            Node::Equals(data) => data.binary_operation_data.node_data.block(),
            Node::NotEquals(data) => data.binary_operation_data.node_data.block(),
            Node::HigherEquals(data) => data.binary_operation_data.node_data.block(),
            Node::Higher(data) => data.binary_operation_data.node_data.block(),
            Node::And(data) => data.binary_operation_data.node_data.block(),
            Node::Or(data) => data.binary_operation_data.node_data.block(),
            Node::Xor(data) => data.binary_operation_data.node_data.block(),
            Node::NoOp(data) => data.block(),
            Node::ConditionalJump(data) => data.node_data.block(),
            Node::Jump(data) => data.node_data.block(),
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct NodeData {
    block_index: usize,
    predecessors: Vec<usize>,
}

impl NodeData {
    pub fn new(block_index: usize, predecessors: Vec<usize>) -> NodeData {
        NodeData {
            block_index,
            predecessors,
        }
    }

    pub fn block(&self) -> usize {
        self.block_index
    }

    pub fn predecessors(&self) -> &Vec<usize> {
        &self.predecessors
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<usize> {
        &mut self.predecessors
    }

    pub fn set_predecessor(&mut self, index: usize, node: usize) {
        self.predecessors[index] = node;
    }

    pub fn add_predecessor(&mut self, node: usize) {
        self.predecessors.push(node);
    }

    pub fn get_predecessor(&self, index: usize) -> Option<usize> {
        self.predecessors.get(index).copied()
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct AddData {
    binary_operation_data: BinaryOperationData,
}

impl AddData {
    pub fn new(block: usize, left: usize, right: usize) -> AddData {
        AddData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

pub const BINARY_OPERATION_LEFT: usize = 0;
pub const BINARY_OPERATION_RIGHT: usize = 1;
#[derive(Eq, Hash, PartialEq, Debug)]
pub struct BinaryOperationData {
    node_data: NodeData,
}

impl BinaryOperationData {
    pub fn new(block_index: usize, left: usize, right: usize) -> BinaryOperationData {
        BinaryOperationData {
            node_data: NodeData::new(block_index, vec![left, right]),
        }
    }

    pub fn new_with_side_effect(
        block: usize,
        left: usize,
        right: usize,
        side_effect: usize,
    ) -> BinaryOperationData {
        BinaryOperationData {
            node_data: NodeData::new(block, vec![left, right, side_effect]),
        }
    }

    pub fn left(&self) -> usize {
        self.node_data.predecessors[0]
    }

    pub fn right(&self) -> usize {
        self.node_data.predecessors[1]
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct BlockData {
    node_data: NodeData,
}

impl BlockData {
    pub fn new(block_index: usize) -> BlockData {
        BlockData {
            node_data: NodeData::new(block_index, vec![]),
        }
    }

    pub fn predecessors(&self) -> &Vec<usize> {
        &self.node_data.predecessors
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ConstantIntData {
    node_data: NodeData,
    value: i32,
}

impl ConstantIntData {
    pub fn new(block: usize, value: i32) -> ConstantIntData {
        ConstantIntData {
            node_data: NodeData::new(block, vec![]),
            value,
        }
    }

    pub fn value(&self) -> i32 {
        self.value
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct DivisionData {
    binary_operation_data: BinaryOperationData,
}

impl DivisionData {
    pub fn new(block: usize, left: usize, right: usize, side_effect: usize) -> DivisionData {
        DivisionData {
            binary_operation_data: BinaryOperationData::new_with_side_effect(
                block,
                left,
                right,
                side_effect,
            ),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ModuloData {
    binary_operation_data: BinaryOperationData,
}

impl ModuloData {
    pub fn new(block: usize, left: usize, right: usize, side_effect: usize) -> ModuloData {
        ModuloData {
            binary_operation_data: BinaryOperationData::new_with_side_effect(
                block,
                left,
                right,
                side_effect,
            ),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct MultiplicationData {
    binary_operation_data: BinaryOperationData,
}

impl MultiplicationData {
    pub fn new(block: usize, left: usize, right: usize) -> MultiplicationData {
        MultiplicationData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct PhiData {
    node_data: NodeData,
}

impl PhiData {
    pub fn new(block: usize) -> PhiData {
        PhiData {
            node_data: NodeData::new(block, vec![]),
        }
    }

    pub fn new_with_operands(block: usize, operands: Vec<usize>) -> PhiData {
        PhiData {
            node_data: NodeData {
                block_index: block,
                predecessors: operands,
            },
        }
    }

    pub fn node_data(&self) -> &NodeData {
        &self.node_data
    }

    pub fn add_operand(&mut self, operand: usize) {
        self.node_data.predecessors.push(operand);
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub enum ProjectionInformation {
    SideEffect,
    Result,
    IfTrue,
    IfFalse,
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ProjectionData {
    node_data: NodeData,
    projection_info: ProjectionInformation,
}

impl ProjectionData {
    pub fn new(
        block: usize,
        input: usize,
        projection_info: ProjectionInformation,
    ) -> ProjectionData {
        ProjectionData {
            node_data: NodeData::new(block, vec![input]),
            projection_info,
        }
    }

    pub fn projection_info(&self) -> &ProjectionInformation {
        &self.projection_info
    }

    pub fn node_data(&self) -> &NodeData {
        &self.node_data
    }

    pub fn input(&self) -> usize {
        self.node_data.predecessors()[0]
    }
}

pub const RETURN_RESULT_INDEX: usize = 0;
#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ReturnData {
    node_data: NodeData,
}

impl ReturnData {
    pub fn new(block: usize, result: usize, side_effect: usize) -> ReturnData {
        ReturnData {
            node_data: NodeData::new(block, vec![result, side_effect]),
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct StartData {
    node_data: NodeData,
}

impl StartData {
    pub fn new(block: usize) -> StartData {
        StartData {
            node_data: NodeData::new(block, vec![]),
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct SubtractionData {
    binary_operation_data: BinaryOperationData,
}

impl SubtractionData {
    pub fn new(block: usize, left: usize, right: usize) -> SubtractionData {
        SubtractionData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ShiftLeftData {
    binary_operation_data: BinaryOperationData,
}

impl ShiftLeftData {
    pub fn new(block: usize, left: usize, right: usize) -> ShiftLeftData {
        ShiftLeftData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ShiftRightData {
    binary_operation_data: BinaryOperationData,
}

impl ShiftRightData {
    pub fn new(block: usize, left: usize, right: usize) -> ShiftRightData {
        ShiftRightData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct BitwiseNegateData {
    node_data: NodeData,
}

impl BitwiseNegateData {
    pub fn new(block: usize, operand: usize) -> BitwiseNegateData {
        BitwiseNegateData {
            node_data: NodeData::new(block, vec![operand]),
        }
    }

    pub fn node_data(&self) -> &NodeData {
        &self.node_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct LowerData {
    binary_operation_data: BinaryOperationData,
}

impl LowerData {
    pub fn new(block: usize, left: usize, right: usize) -> LowerData {
        LowerData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct LowerEqualsData {
    binary_operation_data: BinaryOperationData,
}

impl LowerEqualsData {
    pub fn new(block: usize, left: usize, right: usize) -> LowerEqualsData {
        LowerEqualsData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct EqualsData {
    binary_operation_data: BinaryOperationData,
}

impl EqualsData {
    pub fn new(block: usize, left: usize, right: usize) -> EqualsData {
        EqualsData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct NotEqualsData {
    binary_operation_data: BinaryOperationData,
}

impl NotEqualsData {
    pub fn new(block: usize, left: usize, right: usize) -> NotEqualsData {
        NotEqualsData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}
#[derive(Eq, Hash, PartialEq, Debug)]
pub struct HigherEqualsData {
    binary_operation_data: BinaryOperationData,
}

impl HigherEqualsData {
    pub fn new(block: usize, left: usize, right: usize) -> HigherEqualsData {
        HigherEqualsData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct HigherData {
    binary_operation_data: BinaryOperationData,
}

impl HigherData {
    pub fn new(block: usize, left: usize, right: usize) -> HigherData {
        HigherData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}
#[derive(Eq, Hash, PartialEq, Debug)]
pub struct OrData {
    binary_operation_data: BinaryOperationData,
}

impl OrData {
    pub fn new(block: usize, left: usize, right: usize) -> OrData {
        OrData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}
#[derive(Eq, Hash, PartialEq, Debug)]
pub struct AndData {
    binary_operation_data: BinaryOperationData,
}

impl AndData {
    pub fn new(block: usize, left: usize, right: usize) -> AndData {
        AndData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}
#[derive(Eq, Hash, PartialEq, Debug)]
pub struct XorData {
    binary_operation_data: BinaryOperationData,
}

impl XorData {
    pub fn new(block: usize, left: usize, right: usize) -> XorData {
        XorData {
            binary_operation_data: BinaryOperationData::new(block, left, right),
        }
    }

    pub fn binary_operation_data(&self) -> &BinaryOperationData {
        &self.binary_operation_data
    }
}

pub const COMPARISON_INDEX: usize = 0;
#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ConditionalJumpData {
    node_data: NodeData,
}

impl ConditionalJumpData {
    pub fn new(block: usize, condition: usize) -> ConditionalJumpData {
        ConditionalJumpData {
            node_data: NodeData::new(block, vec![condition]),
        }
    }

    pub fn node_data(&self) -> &NodeData {
        &self.node_data
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct JumpData {
    node_data: NodeData,
}

impl JumpData {
    pub fn new(block: usize) -> JumpData {
        JumpData {
            node_data: NodeData::new(block, vec![]),
        }
    }

    pub fn node_data(&self) -> &NodeData {
        &self.node_data
    }
}
#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ConstantBoolData {
    node_data: NodeData,
    value: bool,
}

impl ConstantBoolData {
    pub fn new(block: usize, value: bool) -> ConstantBoolData {
        ConstantBoolData {
            node_data: NodeData::new(block, vec![]),
            value,
        }
    }

    pub fn value(&self) -> bool {
        self.value
    }
}
impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Add(data) => write!(
                f,
                "Add: {:?}",
                data.binary_operation_data.node_data.predecessors()
            ),
            Node::Phi(data) => write!(f, "!!PHI!! {:?}", data.node_data().predecessors()),
            Node::Block(data) => write!(f, "Block: {:?}", data.node_data.predecessors()),
            Node::Start(data) => write!(f, "Start: {:?}", data.node_data.predecessors()),
            Node::ConstantInt(data) => write!(
                f,
                "Constant: {} {:?}",
                data.value,
                data.node_data.predecessors()
            ),
            Node::Division(data) => write!(
                f,
                "Div: {:?}",
                data.binary_operation_data.node_data.predecessors()
            ),
            Node::Modulo(data) => write!(
                f,
                "Mod: {:?}",
                data.binary_operation_data.node_data.predecessors()
            ),
            Node::Projection(data) => write!(f, "Projection: {:?}", data.node_data.predecessors()),
            Node::Multiplication(data) => write!(
                f,
                "Multiplication: {:?}",
                data.binary_operation_data.node_data.predecessors()
            ),
            Node::Return(data) => write!(f, "Return: {:?}", data.node_data.predecessors()),
            Node::Subtraction(data) => {
                write!(
                    f,
                    "Subtraction: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::BitwiseNegate(data) => {
                write!(f, "BitwiseNegate: {:?}", data.node_data.predecessors())
            }
            Node::ShiftRight(data) => {
                write!(
                    f,
                    "ShiftRight: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::ShiftLeft(data) => {
                write!(
                    f,
                    "ShiftLeft: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::Lower(data) => {
                write!(
                    f,
                    "Lower: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::LowerEquals(data) => {
                write!(
                    f,
                    "LowerEquals: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::Equals(data) => {
                write!(
                    f,
                    "Equals: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::NotEquals(data) => {
                write!(
                    f,
                    "NotEquals: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::HigherEquals(data) => {
                write!(
                    f,
                    "HigherEquals: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::Higher(data) => {
                write!(
                    f,
                    "Higher: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::Or(data) => {
                write!(
                    f,
                    "Or: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::And(data) => {
                write!(
                    f,
                    "And: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::Xor(data) => {
                write!(
                    f,
                    "Xor: {:?}",
                    data.binary_operation_data.node_data.predecessors()
                )
            }
            Node::NoOp(_) => Ok(()),
            Node::ConditionalJump(data) => {
                write!(f, "ConditionalJump: {:?}", data.node_data.predecessors())
            }
            Node::Jump(data) => {
                write!(f, "Jump: {:?}", data.node_data.predecessors())
            }
            Node::ConstantBool(data) => {
                write!(f, "{}", data.value)
            }
        }
    }
}

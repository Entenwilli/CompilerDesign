use std::fmt::Display;

#[derive(Eq, Hash, PartialEq)]
pub enum Node {
    Add(AddData),
    Block(BlockData),
    ConstantInt(ConstantIntData),
    Division(DivisionData),
    Modulo(ModuloData),
    Multiplication(MultiplicationData),
    Phi(PhiData),
    Projection(ProjectionData),
    Return(ReturnData),
    Start(StartData),
    Subtraction(SubtractionData),
}

impl Node {
    pub fn predecessors(&self) -> &Vec<usize> {
        match self {
            Node::Add(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Block(data) => data.node_data.predecessors(),
            Node::ConstantInt(data) => data.node_data.predecessors(),
            Node::Division(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Modulo(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Multiplication(data) => data.binary_operation_data.node_data.predecessors(),
            Node::Phi(data) => data.node_data.predecessors(),
            Node::Projection(data) => data.node_data.predecessors(),
            Node::Return(data) => data.node_data.predecessors(),
            Node::Start(data) => data.node_data.predecessors(),
            Node::Subtraction(data) => data.binary_operation_data.node_data.predecessors(),
        }
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<usize> {
        match self {
            Node::Add(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Block(data) => data.node_data.predecessors_mut(),
            Node::ConstantInt(data) => data.node_data.predecessors_mut(),
            Node::Division(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Modulo(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Multiplication(data) => data.binary_operation_data.node_data.predecessors_mut(),
            Node::Phi(data) => data.node_data.predecessors_mut(),
            Node::Projection(data) => data.node_data.predecessors_mut(),
            Node::Return(data) => data.node_data.predecessors_mut(),
            Node::Start(data) => data.node_data.predecessors_mut(),
            Node::Subtraction(data) => data.binary_operation_data.node_data.predecessors_mut(),
        }
    }

    pub fn block(&self) -> usize {
        match self {
            Node::Add(data) => data.binary_operation_data.node_data.block(),
            Node::Block(data) => data.node_data.block(),
            Node::ConstantInt(data) => data.node_data.block(),
            Node::Division(data) => data.binary_operation_data.node_data.block(),
            Node::Modulo(data) => data.binary_operation_data.node_data.block(),
            Node::Multiplication(data) => data.binary_operation_data.node_data.block(),
            Node::Phi(data) => data.node_data.block(),
            Node::Projection(data) => data.node_data.block(),
            Node::Return(data) => data.node_data.block(),
            Node::Start(data) => data.node_data.block(),
            Node::Subtraction(data) => data.binary_operation_data.node_data.block(),
        }
    }
}

#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
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
}

#[derive(Eq, Hash, PartialEq)]
pub struct BlockData {
    node_data: NodeData,
}

impl BlockData {
    pub fn new(block_index: usize) -> BlockData {
        BlockData {
            node_data: NodeData::new(block_index, vec![]),
        }
    }
}

#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
pub enum ProjectionInformation {
    SideEffect,
    Result,
}

#[derive(Eq, Hash, PartialEq)]
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

    pub fn input(&self) -> usize {
        self.node_data.predecessors()[0]
    }
}

pub const RETURN_RESULT_INDEX: usize = 0;
#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
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

#[derive(Eq, Hash, PartialEq)]
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

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Add(data) => write!(
                f,
                "Add: {:?}",
                data.binary_operation_data.node_data.predecessors()
            ),
            Node::Phi(_) => write!(f, "!!PHI!!"),
            Node::Block(data) => write!(f, "Block: {:?}", data.node_data.predecessors()),
            Node::Start(data) => write!(f, "Start: {:?}", data.node_data.predecessors()),
            Node::ConstantInt(data) => write!(f, "Constant: {}", data.value),
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
        }
    }
}

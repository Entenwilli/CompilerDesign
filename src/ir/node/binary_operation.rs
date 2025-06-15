use crate::ir::{block::NodeIndex, graph::BlockIndex};

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub struct BinaryOperationData {
    left_node: NodeIndex,
    right_node: NodeIndex,
    sideeffect: Option<usize>,
}

impl BinaryOperationData {
    pub fn new(left_node: NodeIndex, right_node: NodeIndex) -> BinaryOperationData {
        BinaryOperationData {
            left_node,
            right_node,
            sideeffect: None,
        }
    }

    pub fn new_with_sideeffect(
        left_node: NodeIndex,
        right_node: NodeIndex,
        sideeffect: usize,
    ) -> BinaryOperationData {
        BinaryOperationData {
            left_node,
            right_node,
            sideeffect: Some(sideeffect),
        }
    }

    pub fn lhs(&self) -> NodeIndex {
        self.left_node
    }

    pub fn rhs(&self) -> NodeIndex {
        self.right_node
    }
}

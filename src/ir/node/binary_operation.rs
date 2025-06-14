use crate::ir::{block::NodeIndex, graph::BlockIndex};

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct BinaryOperationData {
    left_node: (BlockIndex, NodeIndex),
    right_node: (BlockIndex, NodeIndex),
    sideeffect: Option<usize>,
}

impl BinaryOperationData {
    pub fn new(
        left_node: (BlockIndex, NodeIndex),
        right_node: (BlockIndex, NodeIndex),
    ) -> BinaryOperationData {
        BinaryOperationData {
            left_node,
            right_node,
            sideeffect: None,
        }
    }

    pub fn new_with_sideeffect(
        left_node: (BlockIndex, NodeIndex),
        right_node: (BlockIndex, NodeIndex),
        sideeffect: usize,
    ) -> BinaryOperationData {
        BinaryOperationData {
            left_node,
            right_node,
            sideeffect: Some(sideeffect),
        }
    }

    pub fn lhs(&self) -> (BlockIndex, NodeIndex) {
        self.left_node
    }

    pub fn rhs(&self) -> (BlockIndex, NodeIndex) {
        self.right_node
    }
}

use crate::ir::block::NodeIndex;

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct BinaryOperationData {
    left_node: usize,
    right_node: usize,
    sideeffect: Option<usize>,
}

impl BinaryOperationData {
    pub fn new(left_node: usize, right_node: usize) -> BinaryOperationData {
        BinaryOperationData {
            left_node,
            right_node,
            sideeffect: None,
        }
    }

    pub fn new_with_sideeffect(
        left_node: usize,
        right_node: usize,
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

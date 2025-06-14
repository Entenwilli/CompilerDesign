use crate::ir::{block::NodeIndex, graph::BlockIndex};

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct UnaryOperationData {
    input: (BlockIndex, NodeIndex),
    sideffect: Option<usize>,
}

impl UnaryOperationData {
    pub fn new(input: (BlockIndex, NodeIndex)) -> UnaryOperationData {
        UnaryOperationData {
            input,
            sideffect: None,
        }
    }

    pub fn new_with_sideeffect(
        input: (BlockIndex, NodeIndex),
        sideeffect: usize,
    ) -> UnaryOperationData {
        UnaryOperationData {
            input,
            sideffect: Some(sideeffect),
        }
    }

    pub fn input(&self) -> (BlockIndex, NodeIndex) {
        self.input
    }
}

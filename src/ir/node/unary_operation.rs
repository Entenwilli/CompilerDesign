use crate::ir::{block::NodeIndex, graph::BlockIndex};

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub struct UnaryOperationData {
    input: NodeIndex,
    sideffect: Option<usize>,
}

impl UnaryOperationData {
    pub fn new(input: NodeIndex) -> UnaryOperationData {
        UnaryOperationData {
            input,
            sideffect: None,
        }
    }

    pub fn new_with_sideeffect(input: NodeIndex, sideeffect: usize) -> UnaryOperationData {
        UnaryOperationData {
            input,
            sideffect: Some(sideeffect),
        }
    }

    pub fn input(&self) -> NodeIndex {
        self.input
    }
}

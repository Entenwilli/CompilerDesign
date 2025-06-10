use crate::ir::block::NodeIndex;

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct UnaryOperationData {
    input: usize,
    sideffect: Option<usize>,
}

impl UnaryOperationData {
    pub fn new(input: usize) -> UnaryOperationData {
        UnaryOperationData {
            input,
            sideffect: None,
        }
    }

    pub fn new_with_sideeffect(input: usize, sideeffect: usize) -> UnaryOperationData {
        UnaryOperationData {
            input,
            sideffect: Some(sideeffect),
        }
    }

    pub fn input(&self) -> NodeIndex {
        self.input
    }
}

use crate::ir::block::NodeIndex;

#[derive(Eq, Hash, PartialEq, Debug)]
pub enum ProjectionInformation {
    SideEffect,
    Result,
    IfTrue,
    IfFalse,
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct ProjectionData {
    input: usize,
    projection_information: ProjectionInformation,
}

impl ProjectionData {
    pub fn new(input: NodeIndex, projection_information: ProjectionInformation) -> ProjectionData {
        ProjectionData {
            input,
            projection_information,
        }
    }

    pub fn input(&self) -> NodeIndex {
        self.input
    }

    pub fn projection_info(&self) -> &ProjectionInformation {
        &self.projection_information
    }
}

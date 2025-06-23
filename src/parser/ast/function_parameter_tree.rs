use std::fmt::Display;

use crate::parser::ast::{identifier_tree::IdentifierExpressionTree, type_tree::TypeTree};

#[derive(Clone, Debug)]
pub struct FunctionParameterTree {
    type_tree: TypeTree,
    identifier: IdentifierExpressionTree,
}

impl Display for FunctionParameterTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.type_tree)
    }
}

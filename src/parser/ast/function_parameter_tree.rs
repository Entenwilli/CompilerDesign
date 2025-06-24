use std::fmt::Display;

use crate::{
    lexer::collection::ParserTokens,
    parser::{
        ast::{identifier_tree::IdentifierExpressionTree, type_tree::TypeTree, Tree},
        error::ParseError,
    },
};

#[derive(Clone, Debug)]
pub struct FunctionParameterTree {
    type_tree: TypeTree,
    identifier: IdentifierExpressionTree,
}

impl Tree for FunctionParameterTree {
    fn span(&self) -> crate::util::span::Span {
        self.type_tree.span().merge(&self.identifier.span())
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let type_tree = TypeTree::from_tokens(tokens)?;
        let identifier = IdentifierExpressionTree::from_tokens(tokens)?;
        Ok(FunctionParameterTree {
            type_tree,
            identifier,
        })
    }
}

impl FunctionParameterTree {
    pub fn type_tree(&self) -> &TypeTree {
        &self.type_tree
    }

    pub fn identifier(&self) -> &IdentifierExpressionTree {
        &self.identifier
    }
}

impl Display for FunctionParameterTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.type_tree)
    }
}

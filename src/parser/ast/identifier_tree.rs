use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::collection::ParserTokens,
    parser::{
        ast::{name_tree::NameTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct IdentifierExpressionTree {
    name: NameTree,
}

impl Tree for IdentifierExpressionTree {
    fn span(&self) -> Span {
        self.name.span()
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into identifier");
        trace!("First token {:?}", tokens.peek());
        let name = NameTree::from_tokens(tokens)?;
        trace!("Successfully parsed tokens into name");
        Ok(IdentifierExpressionTree { name })
    }
}

impl IdentifierExpressionTree {
    pub fn new(name: NameTree) -> IdentifierExpressionTree {
        IdentifierExpressionTree { name }
    }

    pub fn name(&self) -> &NameTree {
        &self.name
    }
}

impl Display for IdentifierExpressionTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

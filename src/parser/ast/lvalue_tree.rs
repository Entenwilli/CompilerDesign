use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{collection::ParserTokens, token::SeperatorType},
    parser::{
        ast::{name_tree::NameTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct LValueTree {
    identifier: NameTree,
}

impl Tree for LValueTree {
    fn span(&self) -> Span {
        self.identifier.span().clone()
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing lvalue from tokens");
        trace!("First token: {:?}", tokens.peek());
        if tokens
            .peek()
            .ok_or(ParseError::ReachedEnd)?
            .is_separator(&SeperatorType::ParenOpen)
        {
            tokens.expect_seperator(SeperatorType::ParenOpen)?;
            let inner = LValueTree::from_tokens(tokens)?;
            tokens.expect_seperator(SeperatorType::ParenClose)?;
            return Ok(inner);
        }
        Ok(LValueTree {
            identifier: NameTree::from_tokens(tokens)?,
        })
    }
}

impl LValueTree {
    pub fn identifier(&self) -> &NameTree {
        &self.identifier
    }
}

impl Display for LValueTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

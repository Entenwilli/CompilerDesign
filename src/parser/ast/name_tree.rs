use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{collection::ParserTokens, token::Token},
    parser::{ast::Tree, error::ParseError, symbols::Name},
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct NameTree {
    name: Name,
    span: Span,
}

impl Tree for NameTree {
    fn span(&self) -> Span {
        self.span.clone()
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into name");
        if let Token::Identifier(span, value) = tokens.expect_identifier()? {
            trace!("Sucessfully parsed into name");
            return Ok(NameTree {
                name: Name::IdentifierName(value),
                span,
            });
        }
        Err(ParseError::ExpectedIdentifier)
    }
}

impl NameTree {
    pub fn name(&self) -> &Name {
        &self.name
    }
}

impl Display for NameTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.as_string())
    }
}

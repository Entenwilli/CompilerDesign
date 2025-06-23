use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{
        collection::ParserTokens,
        token::{KeywordType, Token},
    },
    parser::{ast::Tree, error::ParseError, types::Type},
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct TypeTree {
    type_tree: Type,
    span: Span,
}

impl Tree for TypeTree {
    fn span(&self) -> Span {
        self.span.clone()
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into type");
        let token = tokens.expect_type()?;
        if let Token::Keyword(span, keyword_type) = token {
            match keyword_type {
                KeywordType::Int => {
                    trace!("Successfully parsed into type");
                    return Ok(TypeTree {
                        type_tree: Type::Int,
                        span,
                    });
                }
                KeywordType::Bool => {
                    trace!("Successfully parsed into type");
                    return Ok(TypeTree {
                        type_tree: Type::Bool,
                        span,
                    });
                }
                _ => return Err(ParseError::ExpectedType),
            }
        } else {
            return Err(ParseError::ExpectedType);
        }
    }
}

impl TypeTree {
    pub fn type_tree(&self) -> &Type {
        &self.type_tree
    }
}

impl Display for TypeTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_tree.as_string())
    }
}

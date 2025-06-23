use std::fmt::Display;

use crate::{
    lexer::{collection::ParserTokens, token::KeywordType},
    parser::{ast::Tree, error::ParseError},
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct BreakTree {
    span: Span,
}

impl Tree for BreakTree {
    fn span(&self) -> Span {
        self.span.clone()
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let keyword = tokens.expect_keyword(KeywordType::Break)?;
        Ok(BreakTree {
            span: keyword.span(),
        })
    }
}

impl Display for BreakTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "break")
    }
}

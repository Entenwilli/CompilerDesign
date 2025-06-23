use std::fmt::Display;

use crate::{
    lexer::{collection::ParserTokens, token::KeywordType},
    parser::{ast::Tree, error::ParseError},
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct ContinueTree {
    span: Span,
}

impl Tree for ContinueTree {
    fn span(&self) -> Span {
        self.span.clone()
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let keyword = tokens.expect_keyword(KeywordType::Continue)?;
        Ok(ContinueTree {
            span: keyword.span(),
        })
    }
}

impl Display for ContinueTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "continue")
    }
}

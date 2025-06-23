use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{
        collection::ParserTokens,
        token::{KeywordType, Token},
    },
    parser::{ast::Tree, error::ParseError},
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct IntegerLiteralTree {
    value: String,
    base: usize,
    span: Span,
}

impl Tree for IntegerLiteralTree {
    fn span(&self) -> Span {
        self.span.clone()
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into integer literal");
        trace!("First token {:?}", tokens.peek());
        let token = tokens.consume()?;
        if let Token::NumberLiteral(span, value, base) = token {
            trace!("Sucessfully parsed into integer literal");
            Ok(IntegerLiteralTree {
                value: value.clone(),
                base: base.clone(),
                span: span.clone(),
            })
        } else {
            tokens.push(token);
            Err(ParseError::ExpectedLiteral)
        }
    }
}

impl IntegerLiteralTree {
    pub fn new(value: String, base: usize, span: Span) -> IntegerLiteralTree {
        IntegerLiteralTree { value, base, span }
    }

    pub fn value(&self) -> &String {
        &self.value
    }

    pub fn base(&self) -> usize {
        self.base
    }
}

#[derive(Clone, Debug)]
pub struct BooleanLiteralTree {
    value: bool,
    span: Span,
}

impl Tree for BooleanLiteralTree {
    fn span(&self) -> Span {
        self.span.clone()
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        if let Token::Keyword(_, keyword_type) = tokens.peek().ok_or(ParseError::ReachedEnd)? {
            if keyword_type.eq(&KeywordType::True) {
                let token = tokens.consume()?;
                Ok(BooleanLiteralTree {
                    value: true,
                    span: token.span(),
                })
            } else if keyword_type.eq(&KeywordType::False) {
                let token = tokens.consume()?;
                Ok(BooleanLiteralTree {
                    value: false,
                    span: token.span(),
                })
            } else {
                Err(ParseError::ExpectedLiteral)
            }
        } else {
            Err(ParseError::ExpectedLiteral)
        }
    }
}

impl BooleanLiteralTree {
    pub fn value(&self) -> bool {
        self.value
    }
}

impl Display for IntegerLiteralTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for BooleanLiteralTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

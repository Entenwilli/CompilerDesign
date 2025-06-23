use std::fmt::Display;

use crate::{
    lexer::collection::ParserTokens,
    parser::{
        ast::{expression_tree::ExpressionTree, identifier_tree::IdentifierExpressionTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct CallTree {
    identifier: IdentifierExpressionTree,
    parameter_expressions: Vec<ExpressionTree>,
    closing_span: Span,
}

impl Tree for CallTree {
    fn span(&self) -> Span {
        self.identifier.span().merge(&self.closing_span)
    }
    fn from_tokens(_tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        // TODO: Implement parsing from tokens
        Err(ParseError::InvalidCharacter)
    }
}

impl Display for CallTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)?;
        write!(f, "(")?;
        for parameter_expression in &self.parameter_expressions {
            write!(f, "{} ", parameter_expression)?;
        }
        write!(f, ")")
    }
}

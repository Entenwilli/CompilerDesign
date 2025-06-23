use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{collection::ParserTokens, token::KeywordType},
    parser::{
        ast::{expression_tree::ExpressionTree, Tree},
        error::ParseError,
    },
    util::{position::Position, span::Span},
};

#[derive(Clone, Debug)]
pub struct ReturnTree {
    expression: ExpressionTree,
    start: Position,
}

impl Tree for ReturnTree {
    fn span(&self) -> Span {
        Span::new(self.start.clone(), self.expression.span().end().clone())
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into return");
        trace!("First token {:?}", tokens.peek());
        let return_keyword = tokens.expect_keyword(KeywordType::Return)?;
        let expression = ExpressionTree::from_tokens(tokens)?;
        Ok(ReturnTree {
            expression,
            start: return_keyword.span().start().clone(),
        })
    }
}

impl ReturnTree {
    pub fn expression(&self) -> &ExpressionTree {
        &self.expression
    }
}

impl Display for ReturnTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {}", self.expression)
    }
}

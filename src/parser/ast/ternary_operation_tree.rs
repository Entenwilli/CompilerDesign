use std::fmt::Display;

use crate::{
    lexer::{collection::ParserTokens, token::MAX_PRECEDENCE},
    parser::{
        ast::{expression_tree::ExpressionTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct TernaryOperationTree {
    condition: Box<ExpressionTree>,
    true_expression: Box<ExpressionTree>,
    false_expression: Box<ExpressionTree>,
}

impl Tree for TernaryOperationTree {
    fn span(&self) -> Span {
        self.condition.span().merge(&self.false_expression.span())
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let result = ExpressionTree::from_tokens_precedence(tokens, MAX_PRECEDENCE)?;
        match result {
            ExpressionTree::TernaryOperationTree(tree) => Ok(tree),
            _ => Err(ParseError::NotAOperation),
        }
    }
}

impl TernaryOperationTree {
    pub fn new(
        condition: Box<ExpressionTree>,
        true_expression: Box<ExpressionTree>,
        false_expression: Box<ExpressionTree>,
    ) -> TernaryOperationTree {
        TernaryOperationTree {
            condition,
            true_expression,
            false_expression,
        }
    }
}

impl TernaryOperationTree {
    pub fn condition(&self) -> &ExpressionTree {
        &self.condition
    }

    pub fn true_expression(&self) -> &ExpressionTree {
        &self.true_expression
    }

    pub fn false_expression(&self) -> &ExpressionTree {
        &self.false_expression
    }
}

impl Display for TernaryOperationTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ? {} : {}",
            self.condition, self.true_expression, self.false_expression
        )
    }
}

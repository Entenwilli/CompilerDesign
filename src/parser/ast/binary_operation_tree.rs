use std::fmt::Display;

use crate::{
    lexer::{collection::ParserTokens, operator::BinaryOperator, token::MAX_PRECEDENCE},
    parser::{
        ast::{expression_tree::ExpressionTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct BinaryOperationTree {
    lhs: Box<ExpressionTree>,
    operator: BinaryOperator,
    rhs: Box<ExpressionTree>,
}

impl Tree for BinaryOperationTree {
    fn span(&self) -> Span {
        self.lhs.span().merge(&self.rhs.span())
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<BinaryOperationTree, ParseError> {
        let result = ExpressionTree::from_tokens_precedence(tokens, MAX_PRECEDENCE)?;
        match result {
            ExpressionTree::BinaryOperationTree(tree) => Ok(tree),
            _ => Err(ParseError::NotAOperation),
        }
    }
}

impl BinaryOperationTree {
    pub fn new(
        lhs: Box<ExpressionTree>,
        operator: BinaryOperator,
        rhs: Box<ExpressionTree>,
    ) -> BinaryOperationTree {
        BinaryOperationTree { lhs, operator, rhs }
    }

    pub fn operator(&self) -> &BinaryOperator {
        &self.operator
    }

    pub fn lhs(&self) -> &ExpressionTree {
        &self.lhs
    }

    pub fn rhs(&self) -> &ExpressionTree {
        &self.rhs
    }
}

impl Display for BinaryOperationTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", *self.lhs, self.operator, *self.rhs)
    }
}

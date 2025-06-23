use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{
        collection::ParserTokens,
        operator::UnaryOperator,
        token::{OperatorType, Token, MAX_PRECEDENCE},
    },
    parser::{
        ast::{expression_tree::ExpressionTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct UnaryOperationTree {
    operator: UnaryOperator,
    expression: Box<ExpressionTree>,
    span: Span,
}

impl Tree for UnaryOperationTree {
    fn span(&self) -> Span {
        self.span.clone()
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let result = UnaryOperationTree::from_tokens_precedence(tokens, MAX_PRECEDENCE)?;
        match result {
            ExpressionTree::UnaryOperationTree(tree) => Ok(tree),
            _ => Err(ParseError::NotAOperation),
        }
    }
}

impl UnaryOperationTree {
    pub fn new(
        operator: UnaryOperator,
        expression: Box<ExpressionTree>,
        span: Span,
    ) -> UnaryOperationTree {
        UnaryOperationTree {
            operator,
            expression,
            span,
        }
    }

    pub fn from_tokens_precedence(
        tokens: &mut ParserTokens,
        precedence: u8,
    ) -> Result<ExpressionTree, ParseError> {
        trace!("Parsing into unary operation (predecence {})", precedence);
        if precedence == 0 {
            return ExpressionTree::from_tokens_precedence(tokens, precedence);
        }
        if let Token::Operator(_, ref operator_type) =
            // TODO: Is this clone nececarry
            tokens.peek().ok_or(ParseError::ReachedEnd)?.clone()
        {
            if operator_type.get_precedence().contains(&precedence) {
                let expression =
                    Box::new(ExpressionTree::from_tokens_precedence(tokens, precedence)?);
                let span = tokens.consume()?.span().merge(&expression.span());
                let operator = match operator_type {
                    OperatorType::LogicalNot => UnaryOperator::LogicalNot,
                    OperatorType::BitwiseNot => UnaryOperator::BitwiseNot,
                    OperatorType::Minus => UnaryOperator::Minus,
                    _ => return Err(ParseError::ExpectedUnaryOperator),
                };
                return Ok(ExpressionTree::UnaryOperationTree(UnaryOperationTree {
                    expression,
                    operator,
                    span,
                }));
            }
        }
        ExpressionTree::from_tokens_precedence(tokens, precedence - 1)
    }

    pub fn operator(&self) -> &UnaryOperator {
        &self.operator
    }

    pub fn expression(&self) -> &ExpressionTree {
        &self.expression
    }
}

impl Display for UnaryOperationTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.operator, self.expression)
    }
}

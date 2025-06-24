use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{
        collection::ParserTokens,
        operator::{BinaryOperator, UnaryOperator},
        token::{KeywordType, OperatorType, SeperatorType, Token, MAX_PRECEDENCE},
    },
    parser::{
        ast::{
            binary_operation_tree::BinaryOperationTree,
            call_tree::CallTree,
            identifier_tree::IdentifierExpressionTree,
            literal_tree::{BooleanLiteralTree, IntegerLiteralTree},
            ternary_operation_tree::TernaryOperationTree,
            unary_operation_tree::UnaryOperationTree,
            Tree,
        },
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub enum ExpressionTree {
    BooleanLiteralTree(BooleanLiteralTree),
    IntegerLiteralTree(IntegerLiteralTree),
    IdentifierExpressionTree(IdentifierExpressionTree),
    CallTree(CallTree),
    UnaryOperationTree(UnaryOperationTree),
    BinaryOperationTree(BinaryOperationTree),
    TernaryOperationTree(TernaryOperationTree),
}

impl Tree for ExpressionTree {
    fn span(&self) -> Span {
        match self {
            ExpressionTree::BooleanLiteralTree(tree) => tree.span(),
            ExpressionTree::IntegerLiteralTree(tree) => tree.span(),
            ExpressionTree::IdentifierExpressionTree(tree) => tree.span(),
            ExpressionTree::CallTree(tree) => tree.span(),
            ExpressionTree::UnaryOperationTree(tree) => tree.span(),
            ExpressionTree::BinaryOperationTree(tree) => tree.span(),
            ExpressionTree::TernaryOperationTree(tree) => tree.span(),
        }
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into expression");
        trace!("First token {:?}", tokens.peek());
        let lhs = ExpressionTree::from_tokens_precedence(tokens, MAX_PRECEDENCE)?;
        trace!(
            "Parsed possible expression. First token after: {:?}",
            tokens.peek()
        );
        if !tokens.peek().ok_or(ParseError::ReachedEnd)?.is_operator() {
            trace!("Finished parsing expression");
            return Ok(lhs);
        }
        if let Token::Operator(_, operator) = tokens.peek().ok_or(ParseError::ReachedEnd)? {
            if !operator.eq(&OperatorType::TernaryQuestionMark) {
                return Ok(lhs);
            }
        }
        tokens.consume()?;
        let true_expression = ExpressionTree::from_tokens(tokens)?;
        tokens.expect_operator(OperatorType::TernaryColon)?;
        let false_expression = ExpressionTree::from_tokens(tokens)?;
        Ok(ExpressionTree::TernaryOperationTree(
            TernaryOperationTree::new(
                Box::new(lhs),
                Box::new(true_expression),
                Box::new(false_expression),
            ),
        ))
    }
}

impl ExpressionTree {
    pub fn from_tokens_precedence(
        tokens: &mut ParserTokens,
        precedence: u8,
    ) -> Result<Self, ParseError> {
        if precedence == 1 {
            return ExpressionTree::parse_unary_expression(tokens, precedence);
        } else if precedence == 0 {
            return ExpressionTree::parse_basic_expression(tokens);
        }

        let mut lhs = ExpressionTree::from_tokens_precedence(tokens, precedence - 1)?;
        loop {
            let next_operator = tokens.consume()?;
            if let Token::Operator(_, ref operator) = next_operator {
                if matches!(
                    operator,
                    OperatorType::TernaryColon | OperatorType::TernaryQuestionMark
                ) || operator.is_assignment_operator()
                {
                    tokens.push(next_operator);
                    return Ok(lhs);
                }
                if operator.get_precedence().contains(&precedence) {
                    let operator = match operator {
                        OperatorType::Plus => BinaryOperator::Plus,
                        OperatorType::Minus => BinaryOperator::Minus,
                        OperatorType::Mul => BinaryOperator::Mul,
                        OperatorType::Div => BinaryOperator::Div,
                        OperatorType::ShiftLeft => BinaryOperator::ShiftLeft,
                        OperatorType::ShiftRight => BinaryOperator::ShiftRight,
                        OperatorType::Mod => BinaryOperator::Mod,
                        OperatorType::Lower => BinaryOperator::Lower,
                        OperatorType::LowerEquals => BinaryOperator::LowerEquals,
                        OperatorType::Equals => BinaryOperator::Equals,
                        OperatorType::NotEquals => BinaryOperator::NotEquals,
                        OperatorType::Higher => BinaryOperator::Higher,
                        OperatorType::HigherEquals => BinaryOperator::HigherEquals,
                        OperatorType::BitwiseOr => BinaryOperator::BitwiseOr,
                        OperatorType::BitwiseAnd => BinaryOperator::BitwiseAnd,
                        OperatorType::BitwiseXor => BinaryOperator::BitwiseXor,
                        OperatorType::LogicalAnd => BinaryOperator::LogicalAnd,
                        OperatorType::LogicalOr => BinaryOperator::LogicalOr,
                        _ => return Err(ParseError::ExpectedBinaryOperator),
                    };
                    let rhs = ExpressionTree::from_tokens_precedence(tokens, precedence - 1)?;
                    lhs = ExpressionTree::BinaryOperationTree(BinaryOperationTree::new(
                        Box::new(lhs),
                        operator.clone(),
                        Box::new(rhs),
                    ));
                    continue;
                }
            }
            tokens.push(next_operator);
            return Ok(lhs);
        }
    }

    pub fn parse_unary_expression(
        tokens: &mut ParserTokens,
        precedence: u8,
    ) -> Result<Self, ParseError> {
        let token = tokens.consume()?;
        if let Token::Operator(_, ref operator_type) = token {
            if operator_type.get_precedence().contains(&precedence) {
                let value = ExpressionTree::from_tokens_precedence(tokens, precedence)?;
                let span = token.clone().span().merge(&value.span());
                let operator = match operator_type {
                    OperatorType::BitwiseNot => UnaryOperator::BitwiseNot,
                    OperatorType::Minus => UnaryOperator::Minus,
                    OperatorType::LogicalNot => UnaryOperator::LogicalNot,
                    _ => return Err(ParseError::ExpectedUnaryOperator),
                };
                return Ok(ExpressionTree::UnaryOperationTree(UnaryOperationTree::new(
                    operator,
                    Box::new(value),
                    span,
                )));
            }
        }
        tokens.push(token);
        ExpressionTree::from_tokens_precedence(tokens, precedence - 1)
    }

    pub fn parse_basic_expression(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        match tokens.peek().ok_or(ParseError::ReachedEnd)?.clone() {
            Token::Separator(_, seperator) if seperator.eq(&SeperatorType::ParenOpen) => {
                tokens.expect_seperator(SeperatorType::ParenOpen)?;
                let expression = ExpressionTree::from_tokens(tokens)?;
                tokens.expect_seperator(SeperatorType::ParenClose)?;
                Ok(expression)
            }
            // FIXME: Janky negative hack, because I wanted nothing to do with int parsing
            Token::Operator(span, operator) if operator.eq(&OperatorType::Minus) => {
                tokens.consume()?;
                match tokens.consume()? {
                    Token::NumberLiteral(span, value, base) => {
                        Ok(ExpressionTree::IntegerLiteralTree(IntegerLiteralTree::new(
                            "-".to_owned() + value.as_str(),
                            base,
                            span.clone(),
                        )))
                    }
                    token => {
                        tokens.push(token);
                        Ok(ExpressionTree::UnaryOperationTree(UnaryOperationTree::new(
                            UnaryOperator::Minus,
                            Box::new(ExpressionTree::from_tokens(tokens)?),
                            span.clone(),
                        )))
                    }
                }
            }
            Token::Identifier(_, _) => {
                if tokens
                    .peek_index(1)?
                    .is_separator(&SeperatorType::ParenOpen)
                {
                    Ok(ExpressionTree::CallTree(CallTree::from_tokens(tokens)?))
                } else {
                    Ok(ExpressionTree::IdentifierExpressionTree(
                        IdentifierExpressionTree::from_tokens(tokens)?,
                    ))
                }
            }
            Token::Keyword(_, keyword_type) if keyword_type.is_function() => {
                Ok(ExpressionTree::CallTree(CallTree::from_tokens(tokens)?))
            }
            Token::NumberLiteral(_, _, _) => Ok(ExpressionTree::IntegerLiteralTree(
                IntegerLiteralTree::from_tokens(tokens)?,
            )),
            Token::Keyword(_, keyword)
                if keyword.eq(&KeywordType::True) || keyword.eq(&KeywordType::False) =>
            {
                Ok(ExpressionTree::BooleanLiteralTree(
                    BooleanLiteralTree::from_tokens(tokens)?,
                ))
            }
            _ => Err(ParseError::ExpectedLiteral),
        }
    }
}

impl Display for ExpressionTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionTree::BooleanLiteralTree(tree) => write!(f, "{}", tree),
            ExpressionTree::IntegerLiteralTree(tree) => write!(f, "{}", tree),
            ExpressionTree::IdentifierExpressionTree(tree) => write!(f, "{}", tree),
            ExpressionTree::CallTree(tree) => write!(f, "{}", tree),
            ExpressionTree::UnaryOperationTree(tree) => write!(f, "{}", tree),
            ExpressionTree::BinaryOperationTree(tree) => write!(f, "{}", tree),
            ExpressionTree::TernaryOperationTree(tree) => write!(f, "{}", tree),
        }
    }
}

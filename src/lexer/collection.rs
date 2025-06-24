use std::collections::VecDeque;

use crate::{
    lexer::{
        operator::AssignmentOperator,
        token::{KeywordType, OperatorType, SeperatorType, Token},
    },
    parser::error::ParseError,
};

pub struct ParserTokens {
    tokens: VecDeque<Token>,
}

impl ParserTokens {
    pub fn new(tokens: VecDeque<Token>) -> ParserTokens {
        ParserTokens { tokens }
    }

    #[must_use]
    pub fn expect_keyword(&mut self, keyword: KeywordType) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            match token {
                Token::Keyword(_, ref keyword_type) if keyword.eq(keyword_type) => {
                    return Ok(token);
                }
                _ => {
                    self.tokens.push_front(token);
                    return Err(ParseError::ExpectedKeyword(keyword));
                }
            }
        }
        Err(ParseError::ReachedEnd)
    }

    #[must_use]
    pub fn expect_seperator(&mut self, seperator: SeperatorType) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            match token {
                Token::Separator(_, ref seperator_type) if seperator.eq(seperator_type) => {
                    return Ok(token)
                }
                _ => {
                    self.tokens.push_front(token);
                    return Err(ParseError::ExpectedSeparator(seperator));
                }
            }
        }
        Err(ParseError::ReachedEnd)
    }

    #[must_use]
    pub fn expect_operator(&mut self, operator: OperatorType) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            match token {
                Token::Operator(_, ref operator_type) if operator.eq(operator_type) => {
                    return Ok(token);
                }
                _ => {
                    self.tokens.push_front(token);
                    return Err(ParseError::ExpectedOperator(operator));
                }
            }
        }
        Err(ParseError::ReachedEnd)
    }

    #[must_use]
    pub fn expect_identifier(&mut self) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            match token {
                Token::Identifier(_, _) => {
                    return Ok(token);
                }
                _ => {
                    self.tokens.push_front(token);
                    return Err(ParseError::ExpectedIdentifier);
                }
            }
        }
        Err(ParseError::ReachedEnd)
    }

    #[must_use]
    pub fn expect_type(&mut self) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            match token {
                Token::Keyword(_, ref keyword_type) if keyword_type.is_type() => {
                    return Ok(token);
                }
                _ => {
                    self.tokens.push_front(token);
                    return Err(ParseError::ExpectedType);
                }
            }
        }
        Err(ParseError::ReachedEnd)
    }

    #[must_use]
    pub fn expect_assignment_operator(&mut self) -> Result<AssignmentOperator, ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            match token {
                Token::Operator(_, operator) if operator.is_assignment_operator() => match operator
                {
                    OperatorType::Assign => return Ok(AssignmentOperator::Assign),
                    OperatorType::AssignPlus => return Ok(AssignmentOperator::AssignPlus),
                    OperatorType::AssignMinus => return Ok(AssignmentOperator::AssignMinus),
                    OperatorType::AssignMul => return Ok(AssignmentOperator::AssignMul),
                    OperatorType::AssignDiv => return Ok(AssignmentOperator::AssignDiv),
                    OperatorType::AssignMod => return Ok(AssignmentOperator::AssignMod),
                    OperatorType::AssignShiftLeft => {
                        return Ok(AssignmentOperator::AssignShiftLeft)
                    }
                    OperatorType::AssignShiftRight => {
                        return Ok(AssignmentOperator::AssignShiftRight)
                    }
                    OperatorType::AssignBitwiseNot => {
                        return Ok(AssignmentOperator::AssignBitwiseNot)
                    }
                    OperatorType::AssignBitwiseAnd => {
                        return Ok(AssignmentOperator::AssignBitwiseAnd)
                    }
                    OperatorType::AssignBitwiseOr => {
                        return Ok(AssignmentOperator::AssignBitwiseOr)
                    }
                    OperatorType::AssignBitwiseXor => {
                        return Ok(AssignmentOperator::AssignBitwiseXor)
                    }
                    _ => return Err(ParseError::ExpectedAssignmentOperator),
                },
                _ => {
                    self.tokens.push_front(token);
                    return Err(ParseError::ExpectedAssignmentOperator);
                }
            }
        }
        Err(ParseError::ReachedEnd)
    }

    #[must_use]
    pub fn consume(&mut self) -> Result<Token, ParseError> {
        self.tokens.pop_front().ok_or(ParseError::ReachedEnd)
    }

    pub fn has_next(&self) -> bool {
        !self.tokens.is_empty()
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    pub fn peek_index(&self, index: usize) -> Result<&Token, ParseError> {
        self.tokens.get(index).ok_or(ParseError::ReachedEnd)
    }

    pub fn push(&mut self, value: Token) {
        self.tokens.push_front(value);
    }
}

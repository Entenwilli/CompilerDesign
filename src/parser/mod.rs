use std::collections::VecDeque;

use ast::Tree;
use error::ParseError;
use symbols::Name;
use types::Type;

use crate::lexer::token::{KeywordType, OperatorType, SeperatorType, Token};

pub mod ast;
pub mod error;
pub mod symbols;
pub mod types;

pub struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Parser {
        Parser { tokens }
    }

    pub fn parse_program(self) -> Result<Tree, ParseError> {
        Ok(Tree::Program(vec![parse_function(self.tokens)?]))
    }
}

fn parse_function(mut tokens: VecDeque<Token>) -> Result<Tree, ParseError> {
    let return_type = expect_keyword(&mut tokens, KeywordType::Int)
        .ok_or(ParseError::Error("Expected int".to_string()))?;
    let identifier = expect_identifier(&mut tokens).unwrap();
    expect_seperator(&mut tokens, SeperatorType::ParenOpen)
        .ok_or(ParseError::Error("Expected ParenOpen".to_string()))?;
    expect_seperator(&mut tokens, SeperatorType::ParenClose)
        .ok_or(ParseError::Error("Expected ParenClose".to_string()))?;
    let body = parse_block(&mut tokens)?;
    Ok(Tree::Function(
        Box::new(Tree::Type(Type::Int, return_type.span())),
        name(identifier)?,
        body,
    ))
}

fn parse_block(tokens: &mut VecDeque<Token>) -> Result<Box<Tree>, ParseError> {
    let body_open = expect_seperator(tokens, SeperatorType::BraceOpen).unwrap();
    let mut statements = vec![];
    while !tokens.is_empty() {
        match tokens.front().unwrap() {
            Token::Separator(_, seperator) if seperator.eq(&SeperatorType::BraceClose) => {
                break;
            }
            _ => statements.push(parse_statement(tokens)?),
        }
    }
    let body_close = expect_seperator(tokens, SeperatorType::BraceClose)
        .ok_or(ParseError::Error("Expected BraceClose".to_string()))?;
    Ok(Box::new(Tree::Block(
        statements,
        body_open.span().merge(body_close.span()),
    )))
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let statement = if tokens.front().unwrap().is_keyword(&KeywordType::Int) {
        parse_decleration(tokens)?
    } else if tokens.front().unwrap().is_keyword(&KeywordType::Return) {
        parse_return(tokens)?
    } else {
        parse_simple(tokens)?
    };
    expect_seperator(tokens, SeperatorType::Semicolon)
        .ok_or(ParseError::Error("Expecting Semicolon!".to_string()))?;
    Ok(statement)
}

fn parse_decleration(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let type_token = expect_keyword(tokens, KeywordType::Int)
        .ok_or(ParseError::Error("Expected keyword".to_string()))?;
    let identifier =
        expect_identifier(tokens).ok_or(ParseError::Error("Expected identifier".to_string()))?;
    let mut expression = None;
    if tokens.front().unwrap().is_operator(&OperatorType::Assign) {
        expect_operator(tokens, OperatorType::Assign);
        expression = Some(parse_expression(tokens)?);
    }
    Ok(Tree::Declaration(
        Box::new(Tree::Type(Type::Int, type_token.span())),
        name(identifier)?,
        expression,
    ))
}

fn parse_return(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let return_keyword = expect_keyword(tokens, KeywordType::Return)
        .ok_or(ParseError::Error("Expected keyword".to_string()))?;
    let expression = parse_expression(tokens)?;
    Ok(Tree::Return(
        expression,
        return_keyword.span().start_owned(),
    ))
}

fn parse_simple(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let lvalue = parse_lvalue(tokens)?;
    let assignment_operator = parse_assignment_operator(tokens)?;
    let expression = parse_expression(tokens)?;
    Ok(Tree::Assignment(lvalue, assignment_operator, expression))
}

fn parse_assignment_operator(tokens: &mut VecDeque<Token>) -> Result<Token, ParseError> {
    if tokens.front().unwrap().is_assignment_operator() {
        consume(tokens);
        return tokens.pop_front().ok_or(ParseError::Finished);
    }
    Err(ParseError::Error(
        "Expected assignment operator".to_string(),
    ))
}

fn parse_lvalue(tokens: &mut VecDeque<Token>) -> Result<Box<Tree>, ParseError> {
    if tokens
        .front()
        .unwrap()
        .is_separator(&SeperatorType::ParenOpen)
    {
        expect_seperator(tokens, SeperatorType::ParenOpen)
            .ok_or(ParseError::Error("Expected ParenOpen".to_string()))?;
        let inner = parse_lvalue(tokens)?;
        expect_seperator(tokens, SeperatorType::ParenClose)
            .ok_or(ParseError::Error("Expected ParenClose".to_string()))?;
        return Ok(inner);
    }
    let identifier =
        expect_identifier(tokens).ok_or(ParseError::Error("Expected identifier!".to_string()))?;
    Ok(Box::new(Tree::LValueIdentifier(name(identifier)?)))
}

fn parse_expression(tokens: &mut VecDeque<Token>) -> Result<Box<Tree>, ParseError> {
    let mut lhs = parse_term(tokens)?;
    loop {
        match tokens.pop_front().unwrap() {
            Token::Operator(_, operator)
                if operator.eq(&OperatorType::Plus) || operator.eq(&OperatorType::Minus) =>
            {
                lhs = Box::new(Tree::BinaryOperation(lhs, parse_term(tokens)?, operator));
            }
            token => {
                tokens.push_front(token);
                return Ok(lhs);
            }
        }
    }
}

fn parse_term(tokens: &mut VecDeque<Token>) -> Result<Box<Tree>, ParseError> {
    let mut lhs = parse_factor(tokens)?;
    loop {
        match tokens.pop_front().unwrap() {
            Token::Operator(_, operator_type)
                if matches!(
                    operator_type,
                    OperatorType::Mul | OperatorType::Div | OperatorType::Mod
                ) =>
            {
                lhs = Box::new(Tree::BinaryOperation(
                    lhs,
                    parse_factor(tokens)?,
                    operator_type,
                ));
            }
            token => {
                tokens.push_front(token);
                return Ok(lhs);
            }
        }
    }
}

fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Box<Tree>, ParseError> {
    match tokens.pop_front().unwrap() {
        Token::Separator(_, seperator) if seperator.eq(&SeperatorType::ParenOpen) => {
            let expression = parse_expression(tokens);
            expect_seperator(tokens, SeperatorType::ParenClose)
                .ok_or(ParseError::Error("Expecting ParenClose".to_string()))?;
            expression
        }
        Token::Operator(span, operator) if operator.eq(&OperatorType::Minus) => {
            Ok(Box::new(Tree::Negate(parse_factor(tokens)?, span)))
        }
        identifier @ Token::Identifier(_, _) => {
            Ok(Box::new(Tree::IdentifierExpression(name(identifier)?)))
        }
        Token::NumberLiteral(span, value, base) => Ok(Box::new(Tree::Literal(value, base, span))),
        _ => Err(ParseError::Error(
            "Expected ParenOpen, Minus, Identifier or Number Literal".to_string(),
        )),
    }
}

fn name(identifier: Token) -> Result<Box<Tree>, ParseError> {
    if let Token::Identifier(span, value) = identifier {
        return Ok(Box::new(Tree::Name(Name::IdentifierName(value), span)));
    }
    Err(ParseError::Error("Expected identifier as name".to_string()))
}

fn expect_keyword(tokens: &mut VecDeque<Token>, keyword: KeywordType) -> Option<Token> {
    if let Some(token) = tokens.front() {
        match token {
            Token::Keyword(_, keyword_type) if keyword.eq(keyword_type) => {
                return tokens.pop_front();
            }
            _ => return None,
        }
    }
    None
}

fn expect_seperator(tokens: &mut VecDeque<Token>, seperator: SeperatorType) -> Option<Token> {
    if let Some(token) = tokens.front() {
        match token {
            Token::Separator(_, seperator_type) if seperator.eq(seperator_type) => {
                return tokens.pop_front()
            }
            _ => return None,
        }
    }
    None
}

fn expect_operator(tokens: &mut VecDeque<Token>, operator: OperatorType) -> Option<Token> {
    if let Some(token) = tokens.front() {
        match token {
            Token::Operator(_, operator_type) if operator.eq(operator_type) => {
                return tokens.pop_front()
            }
            _ => return None,
        }
    }
    None
}

fn expect_identifier(tokens: &mut VecDeque<Token>) -> Option<Token> {
    if let Some(token) = tokens.front() {
        match token {
            Token::Identifier(_, _) => {
                return tokens.pop_front();
            }
            _ => return None,
        }
    }
    None
}

fn consume(tokens: &mut VecDeque<Token>) -> Option<Token> {
    tokens.pop_front()
}

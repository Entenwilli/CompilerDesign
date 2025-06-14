use std::collections::VecDeque;

use ast::Tree;
use error::ParseError;
use symbols::Name;
use tracing::trace;
use types::Type;

use crate::lexer::token::{KeywordType, OperatorType, SeperatorType, Token, MAX_PRECEDENCE};

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

    pub fn parse_program(mut self) -> Result<Tree, ParseError> {
        let functions = vec![parse_function(&mut self.tokens)?];
        if !self.tokens.is_empty() {
            return Err(ParseError::Error(
                "Tokens leftover after parsing!".to_string(),
            ));
        }
        Ok(Tree::Program(functions))
    }
}

fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let return_type = expect_keyword(tokens, KeywordType::Int)
        .ok_or(ParseError::Error("Expected int".to_string()))?;
    let identifier =
        expect_identifier(tokens).ok_or(ParseError::Error("Expected identifier".to_string()))?;
    if let Token::Identifier(_, name) = identifier.clone() {
        if name != "main" {
            return Err(ParseError::Error(format!(
                "Expected main function, but got {}",
                &name
            )));
        }
    } else {
        return Err(ParseError::Error(format!(
            "Expected identifier, but got {:?}",
            &identifier
        )));
    }
    expect_seperator(tokens, SeperatorType::ParenOpen)
        .ok_or(ParseError::Error("Expected ParenOpen".to_string()))?;
    expect_seperator(tokens, SeperatorType::ParenClose)
        .ok_or(ParseError::Error("Expected ParenClose".to_string()))?;
    let body = parse_block(tokens)?;
    Ok(Tree::Function(
        Box::new(Tree::Type(Type::Int, return_type.span())),
        name(identifier)?,
        Box::new(body),
    ))
}

fn parse_block(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
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
    Ok(Tree::Block(
        statements,
        body_open.span().merge(body_close.span()),
    ))
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let statement = if tokens
        .front()
        .unwrap()
        .is_separator(&SeperatorType::BraceOpen)
    {
        parse_block(tokens)?
    } else if tokens.front().unwrap().is_control_keyword() {
        parse_control(tokens)?
    } else {
        let simple = parse_simple(tokens)?;
        expect_seperator(tokens, SeperatorType::Semicolon)
            .ok_or(ParseError::Error("Expecting Semicolon!".to_string()))?;
        simple
    };
    Ok(statement)
}

fn parse_control(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let keyword = tokens.front().unwrap();
    let expression = if keyword.is_keyword(&KeywordType::Return) {
        let result = parse_return(tokens);
        expect_seperator(tokens, SeperatorType::Semicolon)
            .ok_or(ParseError::Error("Expected Semicolon".to_string()))?;
        result
    } else if keyword.is_keyword(&KeywordType::Break) {
        let result = parse_break(tokens);
        expect_seperator(tokens, SeperatorType::Semicolon)
            .ok_or(ParseError::Error("Expected Semicolon".to_string()))?;
        result
    } else if keyword.is_keyword(&KeywordType::Continue) {
        let result = parse_continue(tokens);
        expect_seperator(tokens, SeperatorType::Semicolon)
            .ok_or(ParseError::Error("Expected Semicolon".to_string()))?;
        result
    } else if keyword.is_keyword(&KeywordType::For) {
        parse_for(tokens)
    } else if keyword.is_keyword(&KeywordType::While) {
        parse_while(tokens)
    } else if keyword.is_keyword(&KeywordType::If) {
        parse_if(tokens)
    } else {
        Err(ParseError::Error("Expected control keyword".to_string()))
    };
    trace!("Finished parsing control structure: {:?}", tokens);

    expression
}

fn parse_decleration(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let type_token = expect_keyword(tokens, KeywordType::Int)
        .ok_or(ParseError::Error("Expected keyword".to_string()))?;
    let identifier =
        expect_identifier(tokens).ok_or(ParseError::Error("Expected identifier".to_string()))?;
    let mut expression = None;
    if tokens
        .front()
        .unwrap()
        .is_operator_type(&OperatorType::Assign)
    {
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
    trace!("Parsing return: {:?}", tokens);
    let return_keyword = expect_keyword(tokens, KeywordType::Return)
        .ok_or(ParseError::Error("Expected keyword".to_string()))?;
    let expression = parse_expression(tokens)?;
    Ok(Tree::Return(
        expression,
        return_keyword.span().start_owned(),
    ))
}

fn parse_break(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let break_keyword = expect_keyword(tokens, KeywordType::Break)
        .ok_or(ParseError::Error("Expected keyword".to_string()))?;
    Ok(Tree::Break(break_keyword.span()))
}

fn parse_continue(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let continue_keyword = expect_keyword(tokens, KeywordType::Continue)
        .ok_or(ParseError::Error("Expected keyword".to_string()))?;
    Ok(Tree::Continue(continue_keyword.span()))
}

fn parse_for(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let keyword = expect_keyword(tokens, KeywordType::For)
        .ok_or(ParseError::Error("Expected keyword IF".to_string()))?;
    expect_seperator(tokens, SeperatorType::ParenOpen);
    let initializer = parse_simple(tokens).ok();
    expect_seperator(tokens, SeperatorType::Semicolon);
    let expression = parse_expression(tokens)?;
    expect_seperator(tokens, SeperatorType::Semicolon);
    let updating_expression = parse_simple(tokens).ok();
    expect_seperator(tokens, SeperatorType::ParenClose);
    let statement = parse_statement(tokens)?;
    let span = keyword.span().merge(statement.span());
    Ok(Tree::For(
        initializer.map(|v| Box::new(v)),
        expression,
        updating_expression.map(|v| Box::new(v)),
        Box::new(statement),
        span,
    ))
}

fn parse_while(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let keyword = expect_keyword(tokens, KeywordType::While)
        .ok_or(ParseError::Error("Expected keyword WHILE".to_string()))?;
    expect_seperator(tokens, SeperatorType::ParenOpen);
    let expression = parse_expression(tokens)?;
    expect_seperator(tokens, SeperatorType::ParenClose);
    let statement = parse_statement(tokens)?;
    let span = keyword.span().merge(statement.span());
    Ok(Tree::While(expression, Box::new(statement), span))
}

fn parse_if(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    let keyword = expect_keyword(tokens, KeywordType::If)
        .ok_or(ParseError::Error("Expected keyword IF".to_string()))?;
    expect_seperator(tokens, SeperatorType::ParenOpen);
    let expression = parse_expression(tokens)?;
    expect_seperator(tokens, SeperatorType::ParenClose);
    let statement = parse_statement(tokens)?;
    if tokens.front().unwrap().is_keyword(&KeywordType::Else) {
        consume(tokens);
        let else_statement = parse_statement(tokens)?;
        let span = keyword.span().merge(else_statement.span());
        Ok(Tree::If(
            expression,
            Box::new(statement),
            Some(Box::new(else_statement)),
            span,
        ))
    } else {
        let span = keyword.span().merge(statement.span());
        Ok(Tree::If(expression, Box::new(statement), None, span))
    }
}

fn parse_simple(tokens: &mut VecDeque<Token>) -> Result<Tree, ParseError> {
    if tokens.front().unwrap().is_type_keyword() {
        parse_decleration(tokens)
    } else {
        let lvalue = parse_lvalue(tokens)?;
        let assignment_operator = parse_assignment_operator(tokens)?;
        let expression = parse_expression(tokens)?;
        Ok(Tree::Assignment(lvalue, assignment_operator, expression))
    }
}

fn parse_assignment_operator(tokens: &mut VecDeque<Token>) -> Result<Token, ParseError> {
    if tokens.front().unwrap().is_assignment_operator() {
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
    let identifier = expect_identifier(tokens).ok_or(ParseError::Error(format!(
        "Expected identifier, but got {:?}!",
        tokens.front().unwrap()
    )))?;
    Ok(Box::new(Tree::LValueIdentifier(name(identifier)?)))
}

fn parse_expression(tokens: &mut VecDeque<Token>) -> Result<Box<Tree>, ParseError> {
    let lhs = parse_precedence_expression(tokens, MAX_PRECEDENCE)?;
    if !tokens.front().unwrap().is_operator() {
        return Ok(lhs);
    }
    if let Token::Operator(_, operator) = tokens.front().unwrap() {
        if !operator.eq(&OperatorType::TernaryQuestionMark) {
            return Ok(lhs);
        }
    }
    consume(tokens);
    let true_expression = parse_expression(tokens)?;
    expect_operator(tokens, OperatorType::TernaryColon);
    let false_expression = parse_expression(tokens)?;
    Ok(Box::new(Tree::TernaryOperation(
        lhs,
        true_expression,
        false_expression,
    )))
}

fn parse_precedence_expression(
    tokens: &mut VecDeque<Token>,
    predecence: u8,
) -> Result<Box<Tree>, ParseError> {
    if predecence == 1 {
        return parse_unary_expression(tokens, predecence);
    } else if predecence == 0 {
        return parse_basic_expression(tokens);
    }

    let mut lhs = parse_precedence_expression(tokens, predecence - 1)?;
    loop {
        let next_operator = tokens.pop_front().unwrap();
        if let Token::Operator(_, ref operator) = next_operator {
            if matches!(
                operator,
                OperatorType::TernaryColon | OperatorType::TernaryQuestionMark
            ) || operator.is_assignment_operator()
            {
                tokens.push_front(next_operator);
                return Ok(lhs);
            }
            if operator.get_precedence().contains(&predecence) {
                let rhs = parse_precedence_expression(tokens, predecence - 1)?;
                lhs = Box::new(Tree::BinaryOperation(lhs, rhs, operator.clone()));
                continue;
            }
        }
        tokens.push_front(next_operator);
        return Ok(lhs);
    }
}

fn parse_unary_expression(
    tokens: &mut VecDeque<Token>,
    predecence: u8,
) -> Result<Box<Tree>, ParseError> {
    let token = tokens.pop_front().unwrap();
    if let Token::Operator(_, ref operator_type) = token {
        if operator_type.get_precedence().contains(&predecence) {
            let value = parse_precedence_expression(tokens, predecence)?;
            let span = token.clone().span().merge(value.span());
            return Ok(Box::new(Tree::UnaryOperation(
                value,
                operator_type.clone(),
                span,
            )));
        }
    }
    tokens.push_front(token);
    parse_precedence_expression(tokens, predecence - 1)
}

fn parse_basic_expression(tokens: &mut VecDeque<Token>) -> Result<Box<Tree>, ParseError> {
    match tokens.pop_front().unwrap() {
        Token::Separator(_, seperator) if seperator.eq(&SeperatorType::ParenOpen) => {
            let expression = parse_expression(tokens);
            expect_seperator(tokens, SeperatorType::ParenClose)
                .ok_or(ParseError::Error("Expecting ParenClose".to_string()))?;
            expression
        }
        // FIXME: Janky negative hack, because I wanted nothing to do with int parsing
        Token::Operator(span, operator) if operator.eq(&OperatorType::Minus) => {
            match tokens.pop_front().unwrap() {
                Token::NumberLiteral(span, value, base) => Ok(Box::new(Tree::Literal(
                    "-".to_owned() + value.as_str(),
                    base,
                    span.clone(),
                ))),
                token => {
                    tokens.push_front(token);
                    Ok(Box::new(Tree::UnaryOperation(
                        parse_expression(tokens)?,
                        OperatorType::Minus,
                        span,
                    )))
                }
            }
        }
        identifier @ Token::Identifier(_, _) => {
            Ok(Box::new(Tree::IdentifierExpression(name(identifier)?)))
        }
        Token::NumberLiteral(span, value, base) => Ok(Box::new(Tree::Literal(value, base, span))),
        Token::Keyword(span, keyword) if keyword.eq(&KeywordType::True) => {
            Ok(Box::new(Tree::BoolLiteral(true, span)))
        }
        Token::Keyword(span, keyword) if keyword.eq(&KeywordType::False) => {
            Ok(Box::new(Tree::BoolLiteral(false, span)))
        }
        token => Err(ParseError::Error(format!(
            "Expected ParenOpen, Minus, Identifier or Number Literal, but got {:?}",
            token
        ))),
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

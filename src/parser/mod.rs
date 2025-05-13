use core::panic;
use std::collections::VecDeque;

use ast::Tree;
use symbols::Name;
use types::Type;

use crate::lexer::token::{KeywordType, OperatorType, SeperatorType, Token};

pub mod ast;
pub mod symbols;
pub mod types;

pub struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Parser {
        println!();
        println!("Parsing with tokens: {:?}", tokens);
        println!();
        Parser { tokens }
    }

    pub fn parse_program(self) -> Tree {
        Tree::Program(vec![parse_function(self.tokens)])
    }
}

fn parse_function(mut tokens: VecDeque<Token>) -> Tree {
    let return_type = expect_keyword(&mut tokens, KeywordType::Int).expect("Missing int keyword!");
    let identifier = expect_identifier(&mut tokens).unwrap();
    expect_seperator(&mut tokens, SeperatorType::ParenOpen);
    expect_seperator(&mut tokens, SeperatorType::ParenClose);
    let body = parse_block(&mut tokens);
    Tree::Function(
        Box::new(Tree::Type(Type::Int, return_type.span())),
        name(identifier),
        body,
    )
}

fn parse_block(tokens: &mut VecDeque<Token>) -> Box<Tree> {
    let body_open = expect_seperator(tokens, SeperatorType::BraceOpen).unwrap();
    let mut statements = vec![];
    while !tokens.is_empty() {
        match tokens.front().unwrap() {
            Token::Separator(_, seperator) if seperator.eq(&SeperatorType::BraceClose) => {
                break;
            }
            _ => statements.push(parse_statement(tokens)),
        }
    }
    let body_close =
        expect_seperator(tokens, SeperatorType::BraceClose).expect("Expected closing brace!");
    Box::new(Tree::Block(
        statements,
        body_open.span().merge(body_close.span()),
    ))
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Tree {
    let statement = if tokens.front().unwrap().is_keyword(&KeywordType::Int) {
        parse_decleration(tokens)
    } else if tokens.front().unwrap().is_keyword(&KeywordType::Return) {
        parse_return(tokens)
    } else {
        parse_simple(tokens)
    };
    expect_seperator(tokens, SeperatorType::Semicolon);
    statement
}

fn parse_decleration(tokens: &mut VecDeque<Token>) -> Tree {
    let type_token = expect_keyword(tokens, KeywordType::Int).unwrap();
    let identifier = expect_identifier(tokens).unwrap();
    let mut expression = None;
    if tokens.front().unwrap().is_operator(&OperatorType::Assign) {
        expect_operator(tokens, OperatorType::Assign);
        expression = Some(parse_expression(tokens));
    }
    Tree::Declaration(
        Box::new(Tree::Type(Type::Int, type_token.span())),
        name(identifier),
        expression,
    )
}

fn parse_return(tokens: &mut VecDeque<Token>) -> Tree {
    let return_keyword = expect_keyword(tokens, KeywordType::Return).unwrap();
    let expression = parse_expression(tokens);
    Tree::Return(expression, return_keyword.span().start_owned())
}

fn parse_simple(tokens: &mut VecDeque<Token>) -> Tree {
    let lvalue = parse_lvalue(tokens);
    let assignment_operator = parse_assignment_operator(tokens);
    let expression = parse_expression(tokens);
    Tree::Assignment(lvalue, assignment_operator, expression)
}

fn parse_assignment_operator(tokens: &mut VecDeque<Token>) -> Token {
    if tokens.front().unwrap().is_assignment_operator() {
        consume(tokens);
        return tokens.pop_front().unwrap();
    }
    panic!();
}

fn parse_lvalue(tokens: &mut VecDeque<Token>) -> Box<Tree> {
    if tokens
        .front()
        .unwrap()
        .is_separator(&SeperatorType::ParenOpen)
    {
        expect_seperator(tokens, SeperatorType::ParenOpen);
        let inner = parse_lvalue(tokens);
        expect_seperator(tokens, SeperatorType::ParenClose);
        return inner;
    }
    let identifier = expect_identifier(tokens).unwrap();
    Box::new(Tree::LValueIdentifier(name(identifier)))
}

fn parse_expression(tokens: &mut VecDeque<Token>) -> Box<Tree> {
    let mut lhs = parse_factor(tokens);
    loop {
        match tokens.pop_front().unwrap() {
            Token::Operator(_, operator)
                if operator.eq(&OperatorType::Mul)
                    || operator.eq(&OperatorType::Div)
                    || operator.eq(&OperatorType::Mod) =>
            {
                lhs = Box::new(Tree::BinaryOperation(lhs, parse_factor(tokens), operator));
            }
            _ => return lhs,
        }
    }
}

fn parse_factor(tokens: &mut VecDeque<Token>) -> Box<Tree> {
    match tokens.pop_front().unwrap() {
        Token::Separator(_, seperator) if seperator.eq(&SeperatorType::ParenOpen) => {
            let expression = parse_expression(tokens);
            expect_seperator(tokens, SeperatorType::ParenClose);
            expression
        }
        Token::Operator(span, operator) if operator.eq(&OperatorType::Minus) => {
            Box::new(Tree::Negate(parse_factor(tokens), span))
        }
        identifier @ Token::Identifier(_, _) => {
            Box::new(Tree::IdentifierExpression(name(identifier)))
        }
        Token::NumberLiteral(span, value, base) => Box::new(Tree::Literal(value, base, span)),
        _ => panic!(),
    }
}

fn name(identifier: Token) -> Box<Tree> {
    if let Token::Identifier(span, value) = identifier {
        return Box::new(Tree::Name(Name::IdentifierName(value), span));
    }
    panic!();
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

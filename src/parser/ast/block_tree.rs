use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{
        collection::ParserTokens,
        token::{SeperatorType, Token},
    },
    parser::{
        ast::{statement_tree::StatementTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct BlockTree {
    statements: Vec<StatementTree>,
    block_span: Span,
}

impl Tree for BlockTree {
    fn span(&self) -> Span {
        self.block_span.clone()
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into body");
        let body_open = tokens.expect_seperator(SeperatorType::BraceOpen)?;
        let mut statements = vec![];
        while tokens.has_next() {
            trace!("Parsing body statement");
            trace!("First token {:?}", tokens.peek());
            match tokens.peek().unwrap() {
                Token::Separator(_, seperator) if seperator.eq(&SeperatorType::BraceClose) => {
                    break;
                }
                _ => statements.push(StatementTree::from_tokens(tokens)?),
            }
        }
        let body_close = tokens.expect_seperator(SeperatorType::BraceClose)?;
        trace!("Successfully parsed block");
        Ok(BlockTree {
            statements,
            block_span: body_open.span().merge(&body_close.span()),
        })
    }
}

impl BlockTree {
    pub fn statements(&self) -> &Vec<StatementTree> {
        &self.statements
    }
}

impl Display for BlockTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        write!(f, "}}")
    }
}

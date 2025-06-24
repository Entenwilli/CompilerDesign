use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{
        collection::ParserTokens,
        token::{KeywordType, SeperatorType},
    },
    parser::{
        ast::{
            assignment_tree::AssignmentTree, block_tree::BlockTree, break_tree::BreakTree,
            call_tree::CallTree, continue_tree::ContinueTree, declaration_tree::DeclarationTree,
            for_tree::ForTree, if_tree::IfTree, return_tree::ReturnTree, while_tree::WhileTree,
            Tree,
        },
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub enum StatementTree {
    SimpleStatement(SimpleStatementTree),
    ControlStatement(ControlStatementTree),
    BlockStatement(BlockTree),
}

impl Tree for StatementTree {
    fn span(&self) -> Span {
        match self {
            StatementTree::SimpleStatement(tree) => tree.span(),
            StatementTree::ControlStatement(tree) => tree.span(),
            StatementTree::BlockStatement(tree) => tree.span(),
        }
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into statement");
        trace!("First token {:?}", tokens.peek());
        match tokens.peek().ok_or(ParseError::ReachedEnd)? {
            token if token.is_control_keyword() => Ok(StatementTree::ControlStatement(
                ControlStatementTree::from_tokens(tokens)?,
            )),
            token if token.is_separator(&SeperatorType::BraceOpen) => Ok(
                StatementTree::BlockStatement(BlockTree::from_tokens(tokens)?),
            ),
            _ => {
                let statement = SimpleStatementTree::from_tokens(tokens)?;
                tokens.expect_seperator(SeperatorType::Semicolon)?;
                Ok(StatementTree::SimpleStatement(statement))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum SimpleStatementTree {
    AssignmentTree(AssignmentTree),
    DeclerationTree(DeclarationTree),
    CallTree(CallTree),
}

impl Tree for SimpleStatementTree {
    fn span(&self) -> Span {
        match self {
            SimpleStatementTree::AssignmentTree(tree) => tree.span(),
            SimpleStatementTree::DeclerationTree(tree) => tree.span(),
            SimpleStatementTree::CallTree(tree) => tree.span(),
        }
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into simple statement");
        trace!("First token {:?}", tokens.peek());
        match tokens.peek().ok_or(ParseError::ReachedEnd)? {
            token if token.is_type_keyword() => {
                let declaration_tree = DeclarationTree::from_tokens(tokens)?;
                Ok(SimpleStatementTree::DeclerationTree(declaration_tree))
            }
            _ => {
                if tokens
                    .peek_index(1)?
                    .is_separator(&SeperatorType::ParenOpen)
                {
                    let call_tree = CallTree::from_tokens(tokens)?;
                    Ok(SimpleStatementTree::CallTree(call_tree))
                } else {
                    let assignment_tree = AssignmentTree::from_tokens(tokens)?;
                    Ok(SimpleStatementTree::AssignmentTree(assignment_tree))
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum ControlStatementTree {
    IfTree(IfTree),
    WhileTree(WhileTree),
    ForTree(ForTree),
    ContinueTree(ContinueTree),
    BreakTree(BreakTree),
    ReturnTree(ReturnTree),
}

impl Tree for ControlStatementTree {
    fn span(&self) -> Span {
        match self {
            ControlStatementTree::IfTree(tree) => tree.span(),
            ControlStatementTree::WhileTree(tree) => tree.span(),
            ControlStatementTree::ForTree(tree) => tree.span(),
            ControlStatementTree::ContinueTree(tree) => tree.span(),
            ControlStatementTree::BreakTree(tree) => tree.span(),
            ControlStatementTree::ReturnTree(tree) => tree.span(),
        }
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into control statement");
        trace!("First token {:?}", tokens.peek());
        match tokens.peek().ok_or(ParseError::ReachedEnd)? {
            token if token.is_keyword(&KeywordType::If) => {
                Ok(ControlStatementTree::IfTree(IfTree::from_tokens(tokens)?))
            }
            token if token.is_keyword(&KeywordType::While) => Ok(ControlStatementTree::WhileTree(
                WhileTree::from_tokens(tokens)?,
            )),
            token if token.is_keyword(&KeywordType::Continue) => {
                let statement = ContinueTree::from_tokens(tokens)?;
                tokens.expect_seperator(SeperatorType::Semicolon)?;
                Ok(ControlStatementTree::ContinueTree(statement))
            }
            token if token.is_keyword(&KeywordType::For) => {
                return Ok(ControlStatementTree::ForTree(ForTree::from_tokens(tokens)?))
            }
            token if token.is_keyword(&KeywordType::Break) => {
                let statement = BreakTree::from_tokens(tokens)?;
                tokens.expect_seperator(SeperatorType::Semicolon)?;
                Ok(ControlStatementTree::BreakTree(statement))
            }
            token if token.is_keyword(&KeywordType::Return) => {
                let statement = ReturnTree::from_tokens(tokens)?;
                tokens.expect_seperator(SeperatorType::Semicolon)?;
                Ok(ControlStatementTree::ReturnTree(statement))
            }
            _ => Err(ParseError::NotAStatement),
        }
    }
}

impl Display for StatementTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementTree::SimpleStatement(tree) => writeln!(f, "{}", tree),
            StatementTree::ControlStatement(tree) => writeln!(f, "{}", tree),
            StatementTree::BlockStatement(tree) => writeln!(f, "{}", tree),
        }
    }
}

impl Display for SimpleStatementTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleStatementTree::AssignmentTree(tree) => write!(f, "{}", tree),
            SimpleStatementTree::DeclerationTree(tree) => write!(f, "{}", tree),
            SimpleStatementTree::CallTree(tree) => write!(f, "{}", tree),
        }
    }
}

impl Display for ControlStatementTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ControlStatementTree::IfTree(tree) => write!(f, "{}", tree),
            ControlStatementTree::WhileTree(tree) => write!(f, "{}", tree),
            ControlStatementTree::ForTree(tree) => write!(f, "{}", tree),
            ControlStatementTree::ContinueTree(tree) => write!(f, "{}", tree),
            ControlStatementTree::BreakTree(tree) => write!(f, "{}", tree),
            ControlStatementTree::ReturnTree(tree) => write!(f, "{}", tree),
        }
    }
}

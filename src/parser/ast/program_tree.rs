use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::collection::ParserTokens,
    parser::{
        ast::{function_tree::FunctionTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct ProgramTree {
    functions: Vec<FunctionTree>,
}

impl ProgramTree {
    pub fn functions(&self) -> &Vec<FunctionTree> {
        &self.functions
    }
}

impl Tree for ProgramTree {
    fn span(&self) -> Span {
        self.functions
            .first()
            .expect("Expected at least one function")
            .span()
            .merge(
                &self
                    .functions
                    .last()
                    .expect("Expected at least one function")
                    .span(),
            )
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into program");
        let mut functions = Vec::new();
        while tokens.has_next() {
            functions.push(FunctionTree::from_tokens(tokens)?);
        }
        trace!("Parsed functions: {:?}", functions);
        if functions.is_empty() {
            return Err(ParseError::NoFunctions);
        }
        if functions
            .iter()
            .any(|v| v.name_tree().name().as_string() == "main")
        {
            Ok(ProgramTree { functions })
        } else {
            Err(ParseError::NoMainFunction)
        }
    }
}

impl Display for ProgramTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for function in &self.functions {
            writeln!(f, "{}", function)?;
        }
        Ok(())
    }
}

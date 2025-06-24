use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{collection::ParserTokens, token::SeperatorType},
    parser::{
        ast::{
            block_tree::BlockTree, function_parameter_tree::FunctionParameterTree,
            name_tree::NameTree, type_tree::TypeTree, Tree,
        },
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct FunctionTree {
    return_type: TypeTree,
    name: NameTree,
    body: BlockTree,
    parameters: Vec<FunctionParameterTree>,
}

impl Tree for FunctionTree {
    fn span(&self) -> Span {
        self.return_type.span().merge(&self.body.span())
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing tokens into function");
        let return_type = TypeTree::from_tokens(tokens)?;
        let name = NameTree::from_tokens(tokens)?;
        tokens.expect_seperator(SeperatorType::ParenOpen)?;
        let mut parameters = vec![];
        if tokens
            .peek()
            .ok_or(ParseError::ReachedEnd)?
            .is_type_keyword()
        {
            parameters.push(FunctionParameterTree::from_tokens(tokens)?);
            while tokens
                .peek()
                .ok_or(ParseError::ReachedEnd)?
                .is_separator(&SeperatorType::Comma)
            {
                tokens.consume()?;
                parameters.push(FunctionParameterTree::from_tokens(tokens)?);
            }
        }
        tokens.expect_seperator(SeperatorType::ParenClose)?;
        let body = BlockTree::from_tokens(tokens)?;
        trace!("Successfully parsed function");
        Ok(FunctionTree {
            return_type,
            name,
            body,
            parameters,
        })
    }
}

impl FunctionTree {
    pub fn return_type(&self) -> &TypeTree {
        &self.return_type
    }

    pub fn name_tree(&self) -> &NameTree {
        &self.name
    }

    pub fn body(&self) -> &BlockTree {
        &self.body
    }

    pub fn parameters(&self) -> &Vec<FunctionParameterTree> {
        &self.parameters
    }
}

impl Display for FunctionTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}(", self.return_type, self.name)?;
        for parameter in &self.parameters {
            write!(f, "{}", parameter)?;
        }
        writeln!(f, "){}", self.body)
    }
}

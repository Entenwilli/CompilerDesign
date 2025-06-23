use std::fmt::{Debug, Display};

use tracing::trace;

use crate::{
    lexer::{collection::ParserTokens, token::OperatorType},
    parser::{
        ast::{expression_tree::ExpressionTree, name_tree::NameTree, type_tree::TypeTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct DeclarationTree {
    type_tree: TypeTree,
    name_tree: NameTree,
    initializer_tree: Option<ExpressionTree>,
}

impl Tree for DeclarationTree {
    fn span(&self) -> Span {
        if let Some(initializer_tree) = &self.initializer_tree {
            self.type_tree.span().merge(&initializer_tree.span())
        } else {
            self.type_tree.span().merge(&self.name_tree.span())
        }
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        trace!("Parsing declaration from tokens");
        trace!("First token {:?}", tokens.peek());
        let type_tree = TypeTree::from_tokens(tokens)?;
        let name_tree = NameTree::from_tokens(tokens)?;
        let mut initializer_tree = None;
        if tokens
            .peek()
            .ok_or(ParseError::ReachedEnd)?
            .is_operator_type(&OperatorType::Assign)
        {
            trace!("Declaration may initialize variable");
            trace!("First token {:?}", tokens.peek());
            tokens.expect_operator(OperatorType::Assign)?;
            initializer_tree = Some(ExpressionTree::from_tokens(tokens)?);
            trace!("Parsed initializer: {:?}", initializer_tree);
        }
        Ok(DeclarationTree {
            type_tree,
            name_tree,
            initializer_tree,
        })
    }
}

impl DeclarationTree {
    pub fn type_tree(&self) -> &TypeTree {
        &self.type_tree
    }
    pub fn name_tree(&self) -> &NameTree {
        &self.name_tree
    }
    pub fn initializer_tree(&self) -> &Option<ExpressionTree> {
        &self.initializer_tree
    }
}

impl Display for DeclarationTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.type_tree, self.name_tree)?;
        if let Some(initializer_tree) = &self.initializer_tree {
            write!(f, " = {}", initializer_tree)?;
        }
        Ok(())
    }
}

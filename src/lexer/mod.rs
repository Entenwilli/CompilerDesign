use std::ops::{Range, RangeFrom};

use token::{KeywordType, OperatorType, SeperatorType, Token};
use tracing::trace;

use crate::{
    parser::error::ParseError,
    util::{position::Position, span::Span},
};

pub mod token;

pub struct Lexer {
    source: String,
    position: usize,
    line_start: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        Lexer {
            source,
            position: 0,
            line_start: 0,
            line: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, ParseError> {
        trace!(
            "Reading next token: {}",
            self.get_substring_from(self.position..)
        );
        let whitespace = self.skip_whitespace();
        if whitespace.is_some() {
            return whitespace
                .clone()
                .ok_or(ParseError::Error(format!("{:?}", whitespace.unwrap())));
        }
        if self.position >= self.source.chars().count() {
            return Err(ParseError::Finished);
        }

        let token = match self
            .peek()
            .ok_or(ParseError::Error("Not a character".to_string()))?
        {
            '(' => self.seperator(SeperatorType::ParenOpen),
            ')' => self.seperator(SeperatorType::ParenClose),
            '{' => self.seperator(SeperatorType::BraceOpen),
            '}' => self.seperator(SeperatorType::BraceClose),
            ';' => self.seperator(SeperatorType::Semicolon),
            '?' => Token::Operator(self.build_span(1), OperatorType::TernaryQuestionMark),
            ':' => Token::Operator(self.build_span(1), OperatorType::TernaryColon),
            '-' => self
                .single_assign(OperatorType::Minus, OperatorType::AssignMinus, 1)
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '+' => self
                .single_assign(OperatorType::Plus, OperatorType::AssignPlus, 1)
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '*' => self
                .single_assign(OperatorType::Mul, OperatorType::AssignMul, 1)
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '/' => self
                .single_assign(OperatorType::Div, OperatorType::AssignDiv, 1)
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '%' => self
                .single_assign(OperatorType::Mod, OperatorType::AssignMod, 1)
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '&' => self
                .single_assign_logical(
                    '&',
                    OperatorType::BitwiseAnd,
                    OperatorType::AssignBitwiseAnd,
                    OperatorType::LogicalAnd,
                )
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '~' => self
                .single_assign(OperatorType::BitwiseNot, OperatorType::AssignBitwiseNot, 1)
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '^' => self
                .single_assign(OperatorType::BitwiseXor, OperatorType::AssignBitwiseXor, 1)
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '|' => self
                .single_assign_logical(
                    '|',
                    OperatorType::BitwiseOr,
                    OperatorType::AssignBitwiseOr,
                    OperatorType::LogicalOr,
                )
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '<' => self
                .shift_or_comparison(
                    '<',
                    OperatorType::ShiftLeft,
                    OperatorType::AssignShiftLeft,
                    OperatorType::Lower,
                    OperatorType::LowerEquals,
                )
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '>' => self
                .shift_or_comparison(
                    '>',
                    OperatorType::ShiftRight,
                    OperatorType::AssignShiftRight,
                    OperatorType::Higher,
                    OperatorType::HigherEquals,
                )
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '!' => self
                .not_or_comparison()
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            '=' => self
                .assign_or_comparison()
                .ok_or(ParseError::Error("Not a character".to_string()))?,
            char => {
                if self.is_identifier_char(char) {
                    if self.is_numeric(char) {
                        self.lex_number()?
                    } else {
                        self.lex_identifier_keyword().map_err(|_| {
                            ParseError::Error("Error lexing identifier keyword".to_string())
                        })?
                    }
                } else {
                    Token::ErrorToken(self.build_span(1), char.to_string())
                }
            }
        };
        Ok(token)
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        let mut state: CommentState = CommentState::None;
        let mut comment_start: usize = 0;
        let mut comment_depth = 0;
        while self.has_more(0) {
            match self.peek()? {
                '\x0b' => return Some(Token::ErrorToken(self.build_span(1), "\x0b".to_string())),
                ' ' | '\t' => {
                    self.position += 1;
                }
                '\n' | '\r' => {
                    self.position += 1;
                    self.line_start = self.position;
                    self.line += 1;
                    if state.eq(&CommentState::SingleLine) {
                        state = CommentState::None
                    }
                }
                '/' => {
                    if state.eq(&CommentState::SingleLine) {
                        self.position += 1;
                        continue;
                    }
                    if self.has_more(1) {
                        match self.peek_pos(1) {
                            Some('/') if state.eq(&CommentState::None) => {
                                state = CommentState::SingleLine
                            }
                            Some('*') => {
                                state = CommentState::MultiLine;
                                comment_depth += 1;
                            }
                            Some(_) => {
                                if state.eq(&CommentState::MultiLine) {
                                    self.position += 1;
                                    continue;
                                } else {
                                    return None;
                                }
                            }
                            None => self.position += 1,
                        }
                        comment_start = self.position;
                        self.position += 2;
                        continue;
                    }
                    if comment_depth > 0 {
                        self.position += 1;
                        continue;
                    }
                    return None;
                }
                char => match state {
                    CommentState::MultiLine => {
                        if char.eq(&'*')
                            && self.has_more(1)
                            && self.peek_pos(1).is_some()
                            && self.peek_pos(1).unwrap().eq(&'/')
                        {
                            self.position += 2;
                            comment_depth -= 1;
                            state = match comment_depth {
                                0 => CommentState::None,
                                _ => CommentState::MultiLine,
                            }
                        } else {
                            self.position += 1;
                        }
                    }
                    CommentState::SingleLine => {
                        self.position += 1;
                    }
                    _ => return None,
                },
            }
        }
        if !self.has_more(0) && state.eq(&CommentState::MultiLine) {
            return Some(Token::ErrorToken(
                self.build_span(0),
                self.get_substring_from(comment_start..),
            ));
        }
        None
    }

    fn get_substring(&self, range: Range<usize>) -> String {
        self.source
            .chars()
            .skip(range.start)
            .take(range.end - range.start)
            .collect::<String>()
    }

    fn get_substring_from(&self, range: RangeFrom<usize>) -> String {
        self.source.chars().skip(range.start).collect::<String>()
    }

    fn has_more(&self, offset: usize) -> bool {
        self.position + offset < self.source.len()
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().skip(self.position).nth(0)
    }

    fn peek_pos(&self, position: usize) -> Option<char> {
        self.source.chars().skip(self.position).nth(position)
    }

    fn build_span(&mut self, length: usize) -> Span {
        let start = self.position;
        self.position += length;
        let start_position = Position::new(self.line, start - self.line_start);
        let end_position = Position::new(self.line, start - self.line_start + length);
        Span::new(start_position, end_position)
    }

    fn lex_number(&mut self) -> Result<Token, ParseError> {
        if self
            .is_hex_prefix()
            .ok_or(ParseError::Error("Not a character".to_string()))?
        {
            let mut offset = 2;
            while self.has_more(offset)
                && is_hex(
                    self.peek_pos(offset)
                        .ok_or(ParseError::Error("Not a character".to_string()))?,
                )
            {
                offset += 1;
            }
            let value = self
                .get_substring(self.position..self.position + offset)
                .to_lowercase();
            if offset == 2 {
                return Ok(Token::ErrorToken(self.build_span(2), value));
            }
            return Ok(Token::NumberLiteral(self.build_span(offset), value, 16));
        }

        let mut offset = 1;
        while self.has_more(offset)
            && self.is_numeric(
                self.peek_pos(offset)
                    .ok_or(ParseError::Error("Not a character".to_string()))?,
            )
        {
            offset += 1;
        }

        if self
            .peek()
            .ok_or(ParseError::Error("Not a character".to_string()))?
            .eq(&'0')
            && offset > 1
        {
            return Ok(Token::ErrorToken(
                self.build_span(offset),
                self.get_substring(self.position..self.position + offset),
            ));
        }
        let number_literal = self.get_substring(self.position..self.position + offset);
        Ok(Token::NumberLiteral(
            self.build_span(offset),
            number_literal,
            10,
        ))
    }

    fn is_hex_prefix(&self) -> Option<bool> {
        Some(
            self.peek()?.eq(&'0')
                && self.has_more(1)
                && self.peek_pos(1)?.to_ascii_lowercase().eq(&'x'),
        )
    }

    fn lex_identifier_keyword(&mut self) -> Result<Token, ()> {
        let mut offset = 1;
        while self.has_more(offset) && self.is_identifier_char(self.peek_pos(offset).ok_or(())?) {
            offset += 1;
        }

        let identifier = self
            .source
            .chars()
            .skip(self.position)
            .take(offset)
            .collect::<String>();
        if let Some(keyword) = KeywordType::from_string(&identifier) {
            return Ok(Token::Keyword(self.build_span(offset), keyword));
        }
        // Do not allow variable names with keywords in them!
        if KeywordType::from_string(&identifier.to_lowercase()).is_some() {
            return Err(());
        }

        let identifier_string = identifier.to_string();
        Ok(Token::Identifier(
            self.build_span(offset),
            identifier_string,
        ))
    }

    fn seperator(&mut self, seperator: SeperatorType) -> Token {
        Token::Separator(self.build_span(1), seperator)
    }

    fn single_assign(
        &mut self,
        single_type: OperatorType,
        assign_type: OperatorType,
        equals_offset: usize,
    ) -> Option<Token> {
        if self.has_more(equals_offset) && self.peek_pos(equals_offset)?.eq(&'=') {
            return Some(Token::Operator(
                self.build_span(equals_offset + 1),
                assign_type,
            ));
        }
        Some(Token::Operator(self.build_span(1), single_type))
    }

    fn single_assign_logical(
        &mut self,
        operator: char,
        single_type: OperatorType,
        assign_type: OperatorType,
        logical_type: OperatorType,
    ) -> Option<Token> {
        if self.has_more(1) && self.peek_pos(1)?.eq(&'=') {
            return Some(Token::Operator(self.build_span(2), assign_type));
        }
        if self.has_more(1) && self.peek_pos(1)?.eq(&operator) {
            return Some(Token::Operator(self.build_span(2), logical_type));
        }
        Some(Token::Operator(self.build_span(1), single_type))
    }

    fn assign_or_comparison(&mut self) -> Option<Token> {
        if self.has_more(1) && self.peek_pos(1)?.eq(&'=') {
            return Some(Token::Operator(self.build_span(2), OperatorType::Equals));
        }
        Some(Token::Operator(self.build_span(1), OperatorType::Assign))
    }

    fn not_or_comparison(&mut self) -> Option<Token> {
        if self.has_more(1) && self.peek_pos(1)?.eq(&'=') {
            return Some(Token::Operator(self.build_span(2), OperatorType::NotEquals));
        }
        Some(Token::Operator(
            self.build_span(1),
            OperatorType::LogicalNot,
        ))
    }

    fn shift_or_comparison(
        &mut self,
        shift: char,
        single_type: OperatorType,
        assign_type: OperatorType,
        comparison_type: OperatorType,
        comparison_equal_type: OperatorType,
    ) -> Option<Token> {
        if !self.has_more(1) {
            return Some(Token::Operator(self.build_span(1), comparison_type));
        }

        if self.peek_pos(1)?.ne(&shift) {
            if self.peek_pos(1)?.eq(&'=') {
                return Some(Token::Operator(self.build_span(2), comparison_equal_type));
            } else {
                return Some(Token::Operator(self.build_span(1), comparison_type));
            }
        }
        if self.has_more(2) && self.peek_pos(2)?.eq(&'=') {
            return Some(Token::Operator(self.build_span(3), assign_type));
        }
        Some(Token::Operator(self.build_span(2), single_type))
    }

    fn is_identifier_char(&self, char: char) -> bool {
        char.is_ascii_alphanumeric() || char.eq(&'_')
    }

    fn is_numeric(&self, char: char) -> bool {
        char.is_numeric()
    }
}

#[derive(PartialEq, Debug)]
enum CommentState {
    None,
    SingleLine,
    MultiLine,
}

fn is_hex(char: char) -> bool {
    char.is_ascii_hexdigit()
}

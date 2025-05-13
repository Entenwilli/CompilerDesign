use token::{KeywordType, OperatorType, SeperatorType, Token};

use crate::util::{position::Position, span::Span};

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

    pub fn next_token(&mut self) -> Option<Token> {
        let whitespace = self.skip_whitespace();
        if whitespace.is_some() {
            return whitespace;
        }
        if self.position >= self.source.len() {
            return None;
        }

        let token = match self.peek() {
            '(' => self.seperator(SeperatorType::ParenOpen),
            ')' => self.seperator(SeperatorType::ParenClose),
            '{' => self.seperator(SeperatorType::BraceOpen),
            '}' => self.seperator(SeperatorType::BraceClose),
            ';' => self.seperator(SeperatorType::Semicolon),
            '-' => self.single_assign(OperatorType::Minus, OperatorType::AssignMinus),
            '+' => self.single_assign(OperatorType::Plus, OperatorType::AssignPlus),
            '*' => self.single_assign(OperatorType::Mul, OperatorType::AssignMul),
            '/' => self.single_assign(OperatorType::Div, OperatorType::AssignDiv),
            '%' => self.single_assign(OperatorType::Mod, OperatorType::AssignMod),
            '=' => Token::Operator(self.build_span(1), OperatorType::Assign),
            char => {
                if self.is_identifier_char(char) {
                    if self.is_numeric(char) {
                        self.lex_number()
                    } else {
                        self.lex_identifier_keyword()
                    }
                } else {
                    Token::ErrorToken(self.build_span(1), char.to_string())
                }
            }
        };
        Some(token)
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        let mut state: CommentState = CommentState::None;
        let mut comment_start: usize = 0;
        let mut comment_depth = 0;
        while self.has_more(0) {
            match self.peek() {
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
                            '/' if state.eq(&CommentState::None) => {
                                state = CommentState::SingleLine
                            }
                            '*' => {
                                state = CommentState::MultiLine;
                                comment_depth += 1;
                            }
                            _ => return None,
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
                        if char.eq(&'*') && self.has_more(1) && self.peek_pos(1).eq(&'/') {
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
                self.source.as_str()[comment_start..].to_string(),
            ));
        }
        None
    }

    fn has_more(&self, offset: usize) -> bool {
        self.position + offset < self.source.len()
    }

    fn peek(&self) -> char {
        self.source.chars().skip(self.position).nth(0).unwrap()
    }

    fn peek_pos(&self, position: usize) -> char {
        self.source
            .chars()
            .skip(self.position)
            .nth(position)
            .unwrap()
    }

    fn build_span(&mut self, length: usize) -> Span {
        let start = self.position;
        self.position += length;
        let start_position = Position::new(self.line, start - self.line_start);
        let end_position = Position::new(self.line, start - self.line_start + length);
        Span::new(start_position, end_position)
    }

    fn lex_number(&mut self) -> Token {
        if self.is_hex_prefix() {
            let mut offset = 2;
            while self.has_more(offset) && is_hex(self.peek_pos(offset)) {
                offset += 1;
            }
            if offset == 2 {
                return Token::ErrorToken(
                    self.build_span(2),
                    self.source.as_str()[self.position..self.position + offset].to_string(),
                );
            }
            return Token::NumberLiteral(
                self.build_span(offset),
                self.source.as_str()[self.position..self.position + offset].to_string(),
                16,
            );
        }

        let mut offset = 1;
        while self.has_more(offset) && self.is_numeric(self.peek_pos(offset)) {
            offset += 1;
        }

        if self.peek().eq(&'0') && offset > 1 {
            return Token::ErrorToken(
                self.build_span(offset),
                self.source.as_str()[self.position..self.position + offset].to_string(),
            );
        }
        let number_literal =
            self.source.as_str()[self.position..self.position + offset].to_string();
        Token::NumberLiteral(self.build_span(offset), number_literal, 10)
    }

    fn is_hex_prefix(&self) -> bool {
        self.peek().eq(&'0') && self.has_more(1) && self.peek_pos(1).to_ascii_lowercase().eq(&'x')
    }

    fn lex_identifier_keyword(&mut self) -> Token {
        let mut offset = 1;
        while self.has_more(offset) && self.is_identifier_char(self.peek_pos(offset)) {
            offset += 1;
        }

        let identifier = &self.source[self.position..self.position + offset];
        if let Some(keyword) = KeywordType::from_string(identifier) {
            return Token::Keyword(self.build_span(offset), keyword);
        }
        let identifier_string = identifier.to_string();
        Token::Identifier(self.build_span(offset), identifier_string)
    }

    fn seperator(&mut self, seperator: SeperatorType) -> Token {
        Token::Separator(self.build_span(1), seperator)
    }

    fn single_assign(&mut self, single_type: OperatorType, assign_type: OperatorType) -> Token {
        if self.has_more(1) && self.peek_pos(1).eq(&'=') {
            return Token::Operator(self.build_span(2), assign_type);
        }
        Token::Operator(self.build_span(1), single_type)
    }

    fn is_identifier_char(&self, char: char) -> bool {
        char.is_ascii_alphanumeric() || char.eq(&'_')
    }

    fn is_numeric(&self, char: char) -> bool {
        char.is_numeric()
    }
}

#[derive(PartialEq)]
enum CommentState {
    None,
    SingleLine,
    MultiLine,
}

fn is_hex(char: char) -> bool {
    char.is_ascii_hexdigit()
}

use std::str::Chars;
use std::{iter::Peekable, process};
use unicode_ident::{is_xid_continue, is_xid_start};
use unicode_properties::UnicodeEmoji;

pub mod keyword;
pub mod punct;

use keyword::Keyword;
use punct::Punct;

use crate::{Span, Spanned};

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ident({})", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct StringLit {
    pub value: String,
    pub span: Span,
}

impl std::fmt::Display for StringLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "String({})", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub value: String,
    pub span: Span,
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Number({})", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comment {
    pub value: String,
    pub span: Span,
    pub multiline: bool,
}

#[derive(Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    Ident(Ident),
    Number(Number),
    StringLiteral(StringLit),
    Punct(Punct),
    Eof(Span),
    Comment(Comment),
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::Keyword(keyword) => keyword.span().clone(),
            Token::Ident(ident) => ident.span.clone(),
            Token::Number(number) => number.span.clone(),
            Token::StringLiteral(string) => string.span.clone(),
            Token::Punct(punct) => punct.span().clone(),
            Token::Eof(span) => span.clone(),
            Token::Comment(comment) => comment.span.clone(),
        }
    }

    pub fn is_keyword(&self) -> bool {
        matches!(self, Token::Keyword(_))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Token::Ident(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Token::Number(_))
    }

    pub fn is_string_literal(&self) -> bool {
        matches!(self, Token::StringLiteral(_))
    }

    pub fn is_punct(&self) -> bool {
        matches!(self, Token::Punct(_))
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, Token::Comment(_))
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, Token::Eof(_))
    }
}

// It only checks if the variants are the same, not the contents. E.g. two keywords are equal even
// if they have different spans.
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Keyword(a), Token::Keyword(b)) => a == b,
            (Token::Punct(a), Token::Punct(b)) => a == b,
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::Number(_), Token::Number(_)) => true,
            (Token::StringLiteral(_), Token::StringLiteral(_)) => true,
            (Token::Eof(_), Token::Eof(_)) => true,
            (Token::Comment(_), Token::Comment(_)) => true,
            _ => false,
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token::Eof(Span::default())
    }
}

impl Token {
    pub fn keyword(keyword: Keyword) -> Self {
        Token::Keyword(keyword)
    }

    pub fn ident(ident: String, span: Span) -> Self {
        Token::Ident(Ident { name: ident, span })
    }

    pub fn number(number: String, span: Span) -> Self {
        Token::Number(Number {
            value: number,
            span,
        })
    }

    pub fn string_literal(string: String, span: Span) -> Self {
        Token::StringLiteral(StringLit {
            value: string,
            span,
        })
    }

    pub fn punct(punct: &str, span: Span) -> Self {
        Token::Punct(Punct::new(punct, span))
    }

    pub fn eof(span: Span) -> Self {
        Token::Eof(span)
    }

    pub fn comment(comment: String, span: Span, multiline: bool) -> Self {
        Token::Comment(Comment {
            value: comment,
            span,
            multiline,
        })
    }
}

// impl Default for Token {
//     fn default() -> Self {
//         Self {
//             kind: TokenKind::Eof,
//             span: Span::default(),
//         }
//     }
// }

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    current_char: Option<char>,
    line: usize,
    column: usize,
    saved_line: usize,
    saved_column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars().peekable();
        let current_char = chars.next();
        Lexer {
            input,
            chars,
            current_char,
            line: 1,
            column: 1,
            saved_line: 1,
            saved_column: 1,
        }
    }

    fn current_span(&self) -> Span {
        Span {
            line_start: self.saved_line,
            line_end: self.line,
            column_start: self.saved_column,
            column_end: self.column - 1,
        }
    }

    fn advance(&mut self) {
        if let Some(c) = self.current_char {
            if c == '\r' {
                self.column = 1;
                if let Some('\n') = self.peek() {
                    self.chars.next(); // consume the \n
                    self.line += 1;
                }
            } else if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        self.current_char = self.chars.next();
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().cloned()
    }

    fn error(&self, err: &str) -> ! {
        crate::hard_error(
            err,
            Span {
                line_start: self.saved_line,
                line_end: self.line,
                column_start: self.saved_column,
                column_end: std::cmp::max(self.column - 1, 1),
            },
        )
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
        self.saved_line = self.line;
        self.saved_column = self.column;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let line = self.line;
        let column = self.column;

        let c = match self.current_char {
            Some(c) => c,
            None => {
                return Token::eof(Span {
                    line_start: line,
                    line_end: line,
                    column_start: column,
                    column_end: column,
                });
            }
        };

        if c == '/' {
            if let Some('/') = self.peek() {
                self.advance();
                let comment = self.lex_comment();
                return Token::comment(comment, self.current_span(), false);
            }
        }

        // Check for operators and punctuation first
        if let Some(token) = self.match_punct(c) {
            return token;
        }

        // Check for string literals
        if c == '"' {
            return self.lex_string();
        }

        // Check for numbers
        if c.is_ascii_digit() {
            return self.lex_number();
        }

        // All other non-whitespace, non-operator, non-punctuation characters are part of an identifier
        let ident = self.lex_identifier();
        let ident = match ident {
            Ok(ident) => ident,
            Err(err) => self.error(&err),
        };

        let span = self.current_span();
        let keyword = Keyword::new(ident.as_str(), span.clone());
        if let Some(keyword) = keyword {
            return Token::keyword(keyword);
        }

        Token::ident(ident, span)
    }

    fn match_punct(&mut self, c: char) -> Option<Token> {
        let next = self.peek();
        match c {
            '+' => {
                self.advance();
                Some(Token::punct("+", self.current_span()))
            }
            '-' => {
                self.advance();
                Some(Token::punct("-", self.current_span()))
            }
            '*' => {
                self.advance();
                Some(Token::punct("*", self.current_span()))
            }
            '/' => {
                self.advance();
                Some(Token::punct("/", self.current_span()))
            }
            '%' => {
                self.advance();
                Some(Token::punct("%", self.current_span()))
            }
            '=' => {
                self.advance();
                if let Some('=') = self.current_char {
                    self.advance();
                    Some(Token::punct("==", self.current_span()))
                } else {
                    Some(Token::punct("=", self.current_span()))
                }
            }
            '!' => {
                self.advance();
                if let Some('=') = self.current_char {
                    self.advance();
                    Some(Token::punct("!=", self.current_span()))
                } else {
                    Some(Token::punct("!", self.current_span()))
                }
            }
            '<' => {
                self.advance();
                if let Some('=') = self.current_char {
                    self.advance();
                    Some(Token::punct("<=", self.current_span()))
                } else {
                    Some(Token::punct("<", self.current_span()))
                }
            }
            '>' => {
                self.advance();
                if let Some('=') = self.current_char {
                    self.advance();
                    Some(Token::punct(">=", self.current_span()))
                } else {
                    Some(Token::punct(">", self.current_span()))
                }
            }
            '&' if next == Some('&') => {
                self.advance();
                self.advance();
                Some(Token::punct("&&", self.current_span()))
            }
            '|' if next == Some('|') => {
                self.advance();
                self.advance();
                Some(Token::punct("||", self.current_span()))
            }
            ';' => {
                self.advance();
                Some(Token::punct(";", self.current_span()))
            }
            ':' => {
                self.advance();
                Some(Token::punct(":", self.current_span()))
            }
            ',' => {
                self.advance();
                Some(Token::punct(",", self.current_span()))
            }
            '(' => {
                self.advance();
                Some(Token::punct("(", self.current_span()))
            }
            ')' => {
                self.advance();
                Some(Token::punct(")", self.current_span()))
            }
            '{' => {
                self.advance();
                Some(Token::punct("{", self.current_span()))
            }
            '}' => {
                self.advance();
                Some(Token::punct("}", self.current_span()))
            }
            '[' => {
                self.advance();
                Some(Token::punct("[", self.current_span()))
            }
            ']' => {
                self.advance();
                Some(Token::punct("]", self.current_span()))
            }
            '^' => {
                self.advance();
                Some(Token::punct("^", self.current_span()))
            }
            '#' => {
                self.advance();
                Some(Token::punct("#", self.current_span()))
            }
            '.' => {
                self.advance();
                if let Some('.') = self.current_char {
                    self.advance();
                    if let Some('.') = self.current_char {
                        self.advance();
                        Some(Token::punct("...", self.current_span()))
                    } else {
                        Some(Token::punct("..", self.current_span()))
                    }
                } else {
                    Some(Token::punct(".", self.current_span()))
                }
            }
            _ => None,
        }
    }

    fn lex_string(&mut self) -> Token {
        // Consume the starting double quote
        self.advance();

        let mut string_content = String::new();

        while let Some(c) = self.current_char {
            if c == '"' {
                // End of string
                self.advance();
                return Token::string_literal(string_content, self.current_span());
            } else if c == '\\' {
                // Escape sequence
                self.advance();
                if let Some(escaped_char) = self.current_char {
                    match escaped_char {
                        'n' => {
                            string_content.push('\n');
                            self.advance();
                        }
                        't' => {
                            string_content.push('\t');
                            self.advance();
                        }
                        'r' => {
                            string_content.push('\r');
                            self.advance();
                        }
                        '\\' => {
                            string_content.push('\\');
                            self.advance();
                        }
                        '"' => {
                            string_content.push('"');
                            self.advance();
                        }
                        '0' => {
                            string_content.push('\0');
                            self.advance();
                        }
                        _ => {
                            // Unknown escape sequence; include as is or handle as an error
                            string_content.push('\\');
                            string_content.push(escaped_char);
                            self.advance();
                        }
                    }
                } else {
                    // String ends with a backslash; return an error
                    crate::hard_error(
                        "Unfinished escape sequence in string literal",
                        self.current_span(),
                    );
                }
            } else if c == '\n' {
                // Unescaped newline in string literal; return an error
                self.error("Unterminated string literal");
            } else {
                string_content.push(c);
                self.advance();
            }
        }
        // If we reach here, EOF was reached before closing quote
        crate::hard_error("Unterminated string literal", self.current_span());
    }

    fn lex_number(&mut self) -> Token {
        let mut number = String::new();

        // hexadecimal
        if let Some('0') = self.current_char {
            number.push('0');
            self.advance();
            if let Some(c @ ('x' | 'X')) = self.current_char {
                number.push(c);
                self.advance();
                while let Some(c) = self.current_char {
                    if c.is_ascii_hexdigit() {
                        number.push(c);
                        self.advance();
                    } else {
                        break;
                    }
                }
                return Token::number(number, self.current_span());
            }
        }

        while let Some(c) = self.current_char {
            if c.is_ascii_digit() {
                number.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // fractional part
        if let Some('.') = self.current_char {
            self.advance();
            number.push('.');
            if let Some(c) = self.current_char {
                if c.is_ascii_digit() {
                    while let Some(c) = self.current_char {
                        if c.is_ascii_digit() {
                            number.push(c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                } else {
                    crate::hard_error(
                        &format!("Unexpected character '{}'", c),
                        self.current_span(),
                    );
                }
            }
        }

        // exponent part
        if let Some('e') | Some('E') = self.current_char {
            number.push(self.current_char.unwrap());
            self.advance();

            // handle optional sign
            if let Some('+') | Some('-') = self.current_char {
                number.push(self.current_char.unwrap());
                self.advance();
            }

            // current character must be a digit
            if let Some(c) = self.current_char {
                if c.is_ascii_digit() {
                    while let Some(c) = self.current_char {
                        if c.is_ascii_digit() {
                            number.push(c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                } else {
                    crate::hard_error(
                        &format!("Unexpected character '{}'", c),
                        self.current_span(),
                    );
                }
            }
        }

        Token::number(number, self.current_span())
    }

    fn lex_identifier(&mut self) -> Result<String, String> {
        let mut ident = String::new();

        if let Some(c) = self.current_char {
            if is_xid_start(c) || c == '_' || c.is_emoji_char() {
                ident.push(c);
                self.advance();
            } else {
                return Err(format!("Unexpected character {:?}", c));
            }
        }

        while let Some(c) = self.current_char {
            if is_xid_continue(c) || c.is_emoji_char() {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        Ok(ident)
    }

    fn lex_comment(&mut self) -> String {
        self.advance();

        let mut comment = String::new();

        // if next two characters are [[ or next three [=[ then it's a multi-line comment
        // we need to preserve the = count to know how many = to use to close the comment
        if let Some('[') = self.current_char {
            self.advance();
            if self.current_char == Some('[') {
                self.advance();
                todo!("multi-line comments");
            }
            comment.push('[');
        }

        while let Some(c) = self.current_char {
            if c == '\n' || c == '\r' && self.peek() == Some('\n') {
                break;
            }
            comment.push(c);
            self.advance();
        }

        comment
    }

    fn lex_signed_multi_line_comment(&mut self, count: usize) -> String {
        String::new()
    }

    // fn lex_c_multi_line_comment(&mut self) -> String {}
}

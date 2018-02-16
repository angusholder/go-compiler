use std::str::FromStr;

use utils::chars::PeekableCharIndices;
use utils::result::{ CompileResult, Span };
use utils::intern::Atom;
use token::*;

fn is_ident_head(c: char) -> bool { c.is_alphabetic() || c == '_' }

fn is_ident_tail(c: char) -> bool { c.is_alphanumeric() || c == '_' }

fn is_number_head(c: char) -> bool { c.is_numeric() }

pub struct Lexer<'src> {
    iter: PeekableCharIndices<'src>,
    src: &'src str,
    insert_semicolon: bool,
    reached_eof: bool,
}

impl<'src> Lexer<'src> {
    pub fn new<'a>(src: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            iter: PeekableCharIndices::new(src),
            src,
            insert_semicolon: false,
            reached_eof: false,
        };
        lexer.eat_whitespace();
        lexer
    }

    pub fn next(&mut self) -> CompileResult<Token> {
        if self.insert_semicolon {
            self.insert_semicolon = false;
            return Ok(Token {
                kind: TokenKind::Semicolon,
                span: Span::new(self.offset(), self.offset()),
            });
        }

        let (start_index, ch) = match self.iter.next() {
            Some(next) => next,
            None => {
                if self.reached_eof {
                    panic!("Lexer::next() was called after EOF reached");
                }
                self.reached_eof = true;
                return Ok(Token {
                    kind: TokenKind::Eof,
                    span: Span::new(self.offset(), self.offset()),
                })
            }
        };

        use self::TokenKind::*;
        let kind = match ch {
            '-' => {
                if self.iter.match_char('-') {
                    Decrement
                } else if self.iter.match_char('=') {
                    AssignmentOp(AssignOp::Sub)
                } else {
                    Minus
                }
            }
            ',' => Comma,
            ';' => Semicolon,
            ':' => {
                if self.iter.match_char('=') {
                    ColonEq
                } else {
                    Colon
                }
            }
            '!' => {
                if self.iter.match_char('=') {
                    NotEqual
                } else {
                    LogNot
                }
            }
            '.' => {
                if self.iter.match_char('.') {
                    if self.iter.match_char('.') {
                        Ellipsis
                    } else {
                        let span = Span::new(start_index, self.iter.offset());
                        return err!(span, "Unexpected character sequence `..`");
                    }
                } else {
                    Dot
                }
            }
            '(' => LParen,
            ')' => RParen,
            '[' => LBracket,
            ']' => RBracket,
            '{' => LBrace,
            '}' => RBrace,
            '*' => {
                if self.iter.match_char('=') {
                    AssignmentOp(AssignOp::Mul)
                } else {
                    Star
                }
            }
            '/' => {
                if self.iter.match_char('=') {
                    AssignmentOp(AssignOp::Div)
                } else {
                    Slash
                }
            }
            '&' => {
                if self.iter.match_char('&') {
                    LogAnd
                } else if self.iter.match_char('^') {
                    if self.iter.match_char('=') {
                        AssignmentOp(AssignOp::AndNot)
                    } else {
                        AndNot
                    }
                } else if self.iter.match_char('=') {
                    AssignmentOp(AssignOp::And)
                } else {
                    And
                }
            }
            '%' => {
                if self.iter.match_char('=') {
                    AssignmentOp(AssignOp::Remainder)
                } else {
                    Percent
                }
            }
            '^' => {
                if self.iter.match_char('=') {
                    AssignmentOp(AssignOp::Xor)
                } else {
                    Caret
                }
            }
            '+' => {
                if self.iter.match_char('+') {
                    Increment
                } else if self.iter.match_char('=') {
                    AssignmentOp(AssignOp::Add)
                } else {
                    Plus
                }
            }
            '<' => {
                if self.iter.match_char('-') {
                    SendReceive
                } else if self.iter.match_char('<') {
                    if self.iter.match_char('=') {
                        AssignmentOp(AssignOp::LShift)
                    } else {
                        LShift
                    }
                } else if self.iter.match_char('=') {
                    LessOrEqual
                } else {
                    Less
                }
            }
            '=' => {
                if self.iter.match_char('=') {
                    Equals
                } else {
                    Assignment
                }
            }
            '>' => {
                if self.iter.match_char('>') {
                    if self.iter.match_char('=') {
                        AssignmentOp(AssignOp::RShift)
                    } else {
                        RShift
                    }
                } else if self.iter.match_char('=') {
                    GreaterOrEqual
                } else {
                    Greater
                }
            }
            '|' => {
                if self.iter.match_char('=') {
                    AssignmentOp(AssignOp::Or)
                } else if self.iter.match_char('|') {
                    LogOr
                } else {
                    Or
                }
            }
            '"' => {
                let processed_literal = self.process_string_literal(start_index)?;

                StrLit(Atom::from(processed_literal.as_str()))
            }
            _ if is_ident_head(ch) => {
                while self.iter.match_char_with(is_ident_tail) {
                }
                let span = Span::new(start_index, self.iter.offset());
                let ident = span.as_str(self.src);
                if let Ok(keyword) = self::Keyword::from_str(ident) {
                    Keyword(keyword)
                } else {
                    Ident(Atom::from(ident))
                }
            }
            _ if is_number_head(ch) => {
                while self.iter.match_char_with(char::is_numeric) {
                }
                let span = Span::new(start_index, self.iter.offset());
                let string = span.as_str(self.src);
                Integer(string.parse().unwrap())
            }
            _ => {
                let span = Span::new(start_index, self.offset());
                return err!(span, "unexpected character `{:#?}`", ch);
            }
        };

        let span = Span::new(start_index, self.iter.offset());

        let semicolon_should_follow = kind.semicolon_should_follow();
        let passed_line = self.eat_whitespace();
        let semicolon_follows = self.iter.peek().map(|(_, c)| c) == Some(';');

        self.insert_semicolon = passed_line && !semicolon_follows && semicolon_should_follow;

        Ok(Token { kind, span })
    }

    fn eat_whitespace(&mut self) -> bool {
        let mut passed_line = false;
        while let Some((idx, c)) = self.iter.peek() {
            if c == '\n' { passed_line = true }
            if c.is_whitespace() {
                self.iter.next();
            } else if idx + 1 < self.src.len() && &self.src[idx..idx+2] == "//" {
                self.iter.next();
                self.iter.next();

                while let Some((_, c)) = self.iter.next() {
                    if c == '\n' {
                        passed_line = true;
                        break;
                    }
                }
            } else {
                break;
            }
        }
        passed_line
    }

    fn process_string_literal(&mut self, start_index: usize) -> CompileResult<String> {
        let mut contents = String::new();

        let get_next = |l: &mut Lexer| -> CompileResult<(usize, char)> {
            if let Some((i, ch)) = l.iter.next() {
                Ok((i, ch))
            } else {
                let span = Span::new(start_index, l.iter.offset());
                return err!(span, "string literal terminated prematurely");
            }
        };

        loop {
            let (i, ch) = get_next(self)?;
            if ch == '\\' {
                let mapped_ch = match get_next(self)?.1 {
                    // \a   U+0007 alert or bell
                    'a' => 0x7 as char,

                    // \b   U+0008 backspace
                    'b' => 0x8 as char,

                    // \f   U+000C form feed
                    'f' => 0xC as char,

                    // \n   U+000A line feed or newline
                    'n' => '\n',

                    // \r   U+000D carriage return
                    'r' => '\r',

                    // \t   U+0009 horizontal tab
                    't' => '\t',

                    // \v   U+000b vertical tab
                    'v' => 0xB as char,

                    // \\   U+005c backslash
                    '\\' => '\\',

                    // \"   U+0022 double quote
                    '"' => '"',

                    ch2 => {
                        let span = Span::new(i, self.iter.offset());
                        return err!(span, "unknown escape sequence '\\{}'", ch2);
                    }
                };
                contents.push(mapped_ch);
            } else if ch == '"' {
                break;
            } else {
                contents.push(ch);
            }
        }

        Ok(contents)
    }

    pub fn offset(&self) -> usize {
        self.iter.offset()
    }
}
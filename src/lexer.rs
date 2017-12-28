use std::str::FromStr;
use std::fmt::{ self, Display };

use ast::Ident;
use utils::chars::PeekableCharIndices;
use utils::result::{ CompileResult, Span };

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Keyword {
    Break,
    Case,
    Chan,
    Const,
    Continue,
    Default,
    Defer,
    Else,
    Fallthrough,
    For,
    Func,
    Go,
    Goto,
    If,
    Import,
    Interface,
    Map,
    Package,
    Range,
    Return,
    Select,
    Struct,
    Switch,
    Type,
    Var,
}

impl Keyword {
    pub fn as_str(&self) -> &'static str {
        use self::Keyword::*;
        match *self {
            Break => "break",
            Case => "case",
            Chan => "chan",
            Const => "const",
            Continue => "continue",
            Default => "default",
            Defer => "defer",
            Else => "else",
            Fallthrough => "fallthrough",
            For => "for",
            Func => "func",
            Go => "go",
            Goto => "goto",
            If => "if",
            Import => "import",
            Interface => "interface",
            Map => "map",
            Package => "package",
            Range => "range",
            Return => "return",
            Select => "select",
            Struct => "struct",
            Switch => "switch",
            Type => "type",
            Var => "var",
        }
    }
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::Keyword::*;
        Ok(match s {
            "break" => Break,
            "case" => Case,
            "chan" => Chan,
            "const" => Const,
            "continue" => Continue,
            "default" => Default,
            "defer" => Defer,
            "else" => Else,
            "fallthrough" => Fallthrough,
            "for" => For,
            "func" => Func,
            "go" => Go,
            "goto" => Goto,
            "if" => If,
            "import" => Import,
            "interface" => Interface,
            "map" => Map,
            "package" => Package,
            "range" => Range,
            "return" => Return,
            "select" => Select,
            "struct" => Struct,
            "switch" => Switch,
            "type" => Type,
            "var" => Var,
            _ => return Err(())
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AssignOp {
    // Plain assign
    None,

    // Add op
    Add,
    Sub,
    Or,
    Xor,

    // Mul op
    Mul,
    Div,
    Modulo,
    LShift,
    RShift,
    And,
    AndNot,
}

impl AssignOp {
    pub fn as_str(&self) -> &'static str {
        use self::AssignOp::*;
        match *self {
            None => "=",

            Add => "+=",
            Sub => "-=",
            Or => "|=",
            Xor => "^=",

            Mul => "*=",
            Div => "/=",
            Modulo => "%=",
            LShift => "<<=",
            RShift => ">>=",
            And => "&=",
            AndNot => "&^=",
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Keyword(Keyword),
    Assign(AssignOp),

    Minus,          // -
    Decrement,      // --
    Comma,          // ,
    Semicolon,      // ;
    Colon,          // :
    ColonEq,        // :=
    LogNot,         // !
    NotEqual,       // !=
    Dot,            // .
    Ellipsis,       // ...
    LParen,         // (
    RParen,         // )
    LBracket,       // [
    RBracket,       // ]
    LBrace,         // {
    RBrace,         // }
    Star,           // *
    Slash,          // /
    And,            // &
    LogAnd,         // &&
    AndNot,         // &^
    Percent,        // %
    Caret,          // ^
    Plus,           // +
    Increment,      // ++
    Less,           // <
    SendReceive,    // <-
    LShift,         // <<
    LessOrEqual,    // <=
    Equals,         // ==
    Greater,        // >
    GreaterOrEqual, // >=
    RShift,         // >>
    Or,             // |
    LogOr,          // ||

    Ident(Ident),
    Integer(i64),
    StrLit(String),

    Eof,
}

impl TokenKind {
    pub fn fmt<'tok, 'src>(&'tok self, src: &'src str) -> TokenKindFormatter<'tok, 'src> {
        TokenKindFormatter { token_kind: self, src }
    }

    /// Should a semicolon be inserted following this token before a newline.
    /// https://golang.org/ref/spec#Semicolons
    fn semicolon_should_follow(&self) -> bool {
        use self::TokenKind::*;
        use self::Keyword::*;
        match *self {
            Increment | Decrement | RParen | RBrace | RBracket |
            Keyword(Break) | Keyword(Continue) | Keyword(Fallthrough) | Keyword(Return) |
            Integer(_) | Ident(_) | StrLit(_) => true,
            _ => false
        }
    }
}

pub struct TokenKindFormatter<'tok, 'src> {
    token_kind: &'tok TokenKind,
    src: &'src str,
}

impl<'tok, 'src> Display for TokenKindFormatter<'tok, 'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenKind::*;
        let s: &'static str = match *self.token_kind {
            Keyword(keyword) => keyword.as_str(),
            Assign(op) => op.as_str(),

            Minus => "-",
            Decrement => "--",
            Comma => ",",
            Semicolon => ";",
            Colon => ":",
            ColonEq => ":=",
            LogNot => "!",
            NotEqual => "!=",
            Dot => ".",
            Ellipsis => "...",
            LParen => "(",
            RParen => ")",
            LBracket => "[",
            RBracket => "]",
            LBrace => "{",
            RBrace => "}",
            Star => "*",
            Slash => "/",
            And => "&",
            LogAnd => "&&",
            AndNot => "&^",
            Percent => "%",
            Caret => "^",
            Plus => "+",
            Increment => "++",
            Less => "<",
            SendReceive => "<-",
            LShift => "<<",
            LessOrEqual => "<=",
            Equals => "==",
            Greater => ">",
            GreaterOrEqual=> ">=",
            RShift => ">>",
            Or => "|",
            LogOr => "||",

            Ident(ref ident) => return f.write_str(ident),
            Integer(i) => return write!(f, "{}", i),
            StrLit(ref s) => return f.write_str(s),
        };
        f.write_str(s)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

fn is_ident_head(c: char) -> bool { c.is_alphabetic() || c == '_' }

fn is_ident_tail(c: char) -> bool { c.is_alphanumeric() || c == '_' }

fn is_number_head(c: char) -> bool { c.is_numeric() }

pub struct Lexer<'src> {
    iter: PeekableCharIndices<'src>,
    src: &'src str,
    insert_semicolon: bool,
}

impl<'src> Lexer<'src> {
    pub fn new<'a>(src: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            iter: PeekableCharIndices::new(src),
            src,
            insert_semicolon: false,
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
            None => return Ok(None),
        };

        use self::TokenKind::*;
        let kind = match ch {
            '-' => {
                if self.iter.match_char('-') {
                    Decrement
                } else if self.iter.match_char('=') {
                    Assign(AssignOp::Sub)
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
                    Assign(AssignOp::Mul)
                } else {
                    Star
                }
            }
            '/' => {
                if self.iter.match_char('=') {
                    Assign(AssignOp::Div)
                } else {
                    Slash
                }
            }
            '&' => {
                if self.iter.match_char('&') {
                    LogAnd
                } else if self.iter.match_char('^') {
                    if self.iter.match_char('=') {
                        Assign(AssignOp::AndNot)
                    } else {
                        AndNot
                    }
                } else if self.iter.match_char('=') {
                    Assign(AssignOp::And)
                } else {
                    And
                }
            }
            '%' => {
                if self.iter.match_char('=') {
                    Assign(AssignOp::Modulo)
                } else {
                    Percent
                }
            }
            '^' => {
                if self.iter.match_char('=') {
                    Assign(AssignOp::Xor)
                } else {
                    Caret
                }
            }
            '+' => {
                if self.iter.match_char('+') {
                    Increment
                } else if self.iter.match_char('=') {
                    Assign(AssignOp::Add)
                } else {
                    Plus
                }
            }
            '<' => {
                if self.iter.match_char('-') {
                    SendReceive
                } else if self.iter.match_char('<') {
                    if self.iter.match_char('=') {
                        Assign(AssignOp::LShift)
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
                    Assign(AssignOp::None)
                }
            }
            '>' => {
                if self.iter.match_char('>') {
                    if self.iter.match_char('=') {
                        Assign(AssignOp::RShift)
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
                    Assign(AssignOp::Or)
                } else if self.iter.match_char('|') {
                    LogOr
                } else {
                    Or
                }
            }
            '"' => {
                let mut contents = String::new();
                let mut escape = false;

                loop {
                    let ch = if let Some((_, ch)) = self.iter.next() {
                        ch
                    } else {
                        let span = Span::new(start_index, self.iter.offset());
                        return err!(span, "string literal terminated prematurely");
                    };

                    let mapped_ch = match ch {
                        'a' if escape => char::from(0x7), // \a   U+0007 alert or bell
                        'b' if escape => char::from(0x8), // \b   U+0008 backspace
                        'f' if escape => char::from(0xC), // \f   U+000C form feed
                        'n' if escape => '\n', // \n   U+000A line feed or newline
                        'r' if escape => '\r', // \r   U+000D carriage return
                        't' if escape => '\t', // \t   U+0009 horizontal tab
                        'v' if escape => char::from(0xB), // \v   U+000b vertical tab

                        // \\   U+005c backslash
                        '\\' if escape => '\\',
                        '\\' if !escape => {
                            escape = true;
                            continue;
                        }

                        // \"   U+0022 double quote
                        '"' if escape => '"',
                        '"' if !escape => break,

                        _ => {
                            contents.push(ch);
                            continue
                        }
                    };

                    contents.push(mapped_ch);

                    escape = false;
                }

                StrLit(contents)
            }
            _ if is_ident_head(ch) => {
                while self.iter.match_char_with(is_ident_tail) {
                }
                let span = Span::new(start_index, self.iter.offset());
                let ident = span.as_str(self.src);
                if let Ok(keyword) = self::Keyword::from_str(ident) {
                    Keyword(keyword)
                } else {
                    Ident(ident.to_string().into())
                }
            }
            _ if is_number_head(ch) => {
                while self.iter.match_char_with(char::is_numeric) {
                }
                let span = Span::new(start_index, self.iter.offset());
                let string = span.as_str(self.src);
                Integer(string.parse::<i64>().unwrap())
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

    pub fn offset(&self) -> usize {
        if let Some(Some(Token { span, .. })) = self.peeked {
            span.start as usize
        } else {
            // if self.peeked == Some(None) then we are at the end of the source so still just
            // use the current offset
            self.iter.offset()
        }
    }
}
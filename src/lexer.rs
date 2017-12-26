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

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Keyword(Keyword),

    Minus,          // -
    Decrement,      // --
    MinusEq,        // -=
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
    StarEq,         // *=
    Slash,          // /
    SlashEq,        // /=
    And,            // &
    LogAnd,         // &&
    Nand,           // &^
    NandEq,         // &^=
    AndEq,          // &=
    Percent,        // %
    PercentEq,      // %=
    Caret,          // ^
    CaretEq,        // ^=
    Plus,           // +
    Increment,      // ++
    PlusEq,         // +=
    Less,           // <
    SendReceive,    // <-
    LShift,         // <<
    LShiftEq,       // <<=
    LessOrEqual,    // <=
    Assign,         // =
    Equals,         // ==
    Greater,        // >
    GreaterOrEqual, // >=
    RShift,         // >>
    RShiftEq,       // >>=
    Or,             // |
    OrEq,           // |=
    LogOr,          // ||

    Ident(Ident),
    Integer(i64),
    StrLit(String),
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

            Minus => "-",
            Decrement => "--",
            MinusEq => "-=",
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
            StarEq => "*=",
            Slash => "/",
            SlashEq => "/=",
            And => "&",
            LogAnd => "&&",
            Nand => "&^",
            NandEq => "&^=",
            AndEq => "&=",
            Percent => "%",
            PercentEq => "%=",
            Caret => "^",
            CaretEq => "^=",
            Plus => "+",
            Increment => "++",
            PlusEq => "+=",
            Less => "<",
            SendReceive => "<-",
            LShift => "<<",
            LShiftEq => "<<=",
            LessOrEqual => "<=",
            Assign => "=",
            Equals => "==",
            Greater => ">",
            GreaterOrEqual=> ">=",
            RShift => ">>",
            RShiftEq => ">>=",
            Or => "|",
            OrEq => "|=",
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
}

fn is_ident_head(c: char) -> bool { c.is_alphabetic() || c == '_' }

fn is_ident_tail(c: char) -> bool { c.is_alphanumeric() || c == '_' }

fn is_number_head(c: char) -> bool { c.is_numeric() }

pub struct Lexer<'src> {
    iter: PeekableCharIndices<'src>,
    peeked: Option<Option<Token>>,
    src: &'src str,
    insert_semicolon: bool,
}

impl<'src> Lexer<'src> {
    pub fn new<'a>(src: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            iter: PeekableCharIndices::new(src),
            peeked: None,
            src,
            insert_semicolon: false,
        };
        lexer.eat_whitespace();
        lexer
    }

    pub fn match_keyword(&mut self, keyword: Keyword) -> CompileResult<bool> {
        Ok(if let Some(&Token { kind: TokenKind::Keyword(got_keyword), .. }) = self.peek()? {
            if got_keyword == keyword {
                self.peeked = None;
                true
            } else {
                false
            }
        } else {
            false
        })
    }

    pub fn match_token(&mut self, token_kind: TokenKind) -> CompileResult<bool> {
        Ok(if self.peek()?.map_or(false, |t| t.kind == token_kind) {
            self.peeked = None;
            true
        } else {
            false
        })
    }

    pub fn match_ident(&mut self) -> CompileResult<Option<Ident>> {
        self.peek()?;
        match self.peeked.take().unwrap() {
            Some(Token { kind: TokenKind::Ident(ident) }) => Ok(Some(ident)),
            other => {
                self.peeked = Some(other);
                Ok(None)
            }
        }
    }

    pub fn expect_token(&mut self, token_kind: TokenKind) -> CompileResult<()> {
        if self.match_token(token_kind.clone())? {
            Ok(())
        } else {
            let peeked = self.peeked.as_ref().unwrap();
            err!(peeked, "expected token {:#?}, got {:#?}", token_kind, peeked)
        }
    }

    pub fn expect_keyword(&mut self, keyword: Keyword) -> CompileResult<()> {
        if self.match_keyword(keyword)? {
            Ok(())
        } else {
            let peeked = self.peeked.as_ref().unwrap();
            err!(peeked, "expected keyword {:#?}, got {:#?}", keyword, peeked)
        }
    }

    pub fn expect_ident(&mut self) -> CompileResult<Ident> {
        let next = self.next()?;
        if let Some(Token { kind: TokenKind::Ident(ident), ..}) = next {
            Ok(ident)
        } else {
            err!(next, "expected identifier, got {:#?}", next.map(|t| t.kind))
        }
    }

    pub fn expect_string_lit(&mut self) -> CompileResult<String> {
        let next = self.next()?;
        if let Some(Token { kind: TokenKind::StrLit(string), ..}) = next {
            Ok(string)
        } else {
            err!(next, "expected string literal, got {:#?}", next.map(|t| t.kind))
        }
    }

    pub fn next(&mut self) -> CompileResult<Option<Token>> {
        if let Some(token) = self.peeked.take() {
            return Ok(token)
        }

        if self.insert_semicolon {
            self.insert_semicolon = false;
            return Ok(Some(Token {
                kind: TokenKind::Semicolon,
            }));
        }

        let (start_index, ch) = match self.iter.next() {
            Some(next) => next,
            None => return Ok(None),
        };

        let span = Span::new(start_index, self.iter.offset());

        use self::TokenKind::*;
        let kind = match ch {
            '-' => {
                if self.iter.match_char('-') {
                    Decrement
                } else if self.iter.match_char('=') {
                    MinusEq
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
                    StarEq
                } else {
                    Star
                }
            }
            '/' => {
                if self.iter.match_char('=') {
                    SlashEq
                } else {
                    Slash
                }
            }
            '&' => {
                if self.iter.match_char('&') {
                    LogAnd
                } else if self.iter.match_char('^') {
                    if self.iter.match_char('=') {
                        NandEq
                    } else {
                        Nand
                    }
                } else if self.iter.match_char('=') {
                    AndEq
                } else {
                    And
                }
            }
            '%' => {
                if self.iter.match_char('=') {
                    PercentEq
                } else {
                    Percent
                }
            }
            '^' => {
                if self.iter.match_char('=') {
                    CaretEq
                } else {
                    Caret
                }
            }
            '+' => {
                if self.iter.match_char('+') {
                    Increment
                } else if self.iter.match_char('=') {
                    PlusEq
                } else {
                    Plus
                }
            }
            '<' => {
                if self.iter.match_char('-') {
                    SendReceive
                } else if self.iter.match_char('<') {
                    if self.iter.match_char('=') {
                        LShiftEq
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
                    Assign
                }
            }
            '>' => {
                if self.iter.match_char('>') {
                    if self.iter.match_char('=') {
                        RShiftEq
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
                    OrEq
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
                return err!(span, "unexpected character `{:#?}`", ch);
            }
        };

        let semicolon_should_follow = kind.semicolon_should_follow();
        let passed_line = self.eat_whitespace();
        let semicolon_follows = self.iter.peek().map(|(_, c)| c) == Some(';');

        self.insert_semicolon = passed_line && !semicolon_follows && semicolon_should_follow;

        Ok(Some(Token { kind }))
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

    pub fn peek(&mut self) -> CompileResult<Option<&Token>> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next()?);
        }
        Ok(self.peeked.as_ref().unwrap().as_ref())
    }

    pub fn peek_match_token(&mut self, token_kind: TokenKind) -> CompileResult<bool> {
        Ok(self.peek()?.map(|t| &t.kind) == Some(&token_kind))
    }

    pub fn unget(&mut self, token: Option<Token>) {
        assert!(self.peeked == None);
        self.peeked = Some(token);
    }
}
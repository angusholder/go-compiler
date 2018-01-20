use std::str::FromStr;
use std::fmt::{ self, Display };

use utils::intern::Atom;
use utils::result::{ Span, HasSpan };

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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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

    Ident(Atom),
    Integer(u64),
    StrLit(Atom),

    Eof,
}

impl TokenKind {
    pub fn fmt<'tok, 'src>(&'tok self, src: &'src str) -> TokenKindFormatter<'tok, 'src> {
        TokenKindFormatter { token_kind: self, src }
    }

    /// Should a semicolon be inserted following this token before a newline.
    /// https://golang.org/ref/spec#Semicolons
    pub fn semicolon_should_follow(&self) -> bool {
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
            StrLit(s) => return write!(f, "{}", s),

            Eof => "<end of file>"
        };
        f.write_str(s)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl HasSpan for Token {
    fn span(&self) -> Span {
        self.span
    }
}
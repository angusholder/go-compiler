use std::fmt;
use std::u32;

pub type CompileError = String;
pub type CompileResult<T> = Result<T, CompileError>;

macro_rules! err {
    ($token:expr, $fmt:expr, $($arg:expr),+) => {{
        let _ = &$token;
        Err(format!($fmt, $($arg),+))
    }};

    ($token:expr, $fmt:expr) => {{
        let _ = &$token;
        Err(format!($fmt))
    }};
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start: start as u32,
            end: end as u32,
        }
    }

    pub fn between(start: Span, end: Span) -> Span {
        Span {
            start: start.start,
            end: end.end,
        }
    }

    pub const INVALID: Span = Span {
        start: u32::MAX,
        end: u32::MAX,
    };

    pub fn as_str<'a>(&self, string: &'a str) -> &'a str {
        &string[self.start as usize..self.end as usize]
    }

    pub fn fmt<'a>(&self, src: &'a str) -> SpanFormatter<'a> {
        SpanFormatter {
            span: *self,
            src,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::INVALID
    }
}

pub struct SpanFormatter<'a> {
    span: Span,
    src: &'a str
}

impl<'a> fmt::Display for SpanFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut line = 1;
        let mut column = 1;
        for ch in self.src[..self.span.start as usize].chars() {
            if ch == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        write!(f, "[{}:{}]", line, column)
    }
}
use std::fmt;
use std::fmt::Write;
use std::u32;

pub struct CompileError {
    pub msg: String,
    pub span: Span,
}

impl CompileError {
    pub fn fmt<'err, 'src>(&'err self, src: &'src str) -> CompileErrorFormatter<'err, 'src> {
        CompileErrorFormatter { error: self, src }
    }
}

pub struct CompileErrorFormatter<'err, 'src> {
    error: &'err CompileError,
    src: &'src str,
}

impl<'err, 'src> fmt::Display for CompileErrorFormatter<'err, 'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let range = SourceRange::new(self.src, self.error.span);
        writeln!(f, "[{},{}] error: {}", range.start.line, range.start.col, self.error.msg)?;
        writeln!(f, "{}", range.fmt(self.src))
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

macro_rules! err {
    ($has_span:expr, $fmt:expr, $($arg:expr),+) => {{
        let has_span: &HasSpan = &$has_span;
        let span = has_span.span();
        let msg = format!($fmt, $($arg),+);
        Err($crate::utils::result::CompileError { msg, span })
    }};

    ($has_span:expr, $fmt:expr) => {{
        let has_span: &HasSpan = &$has_span;
        let span = has_span.span();
        let msg = $fmt.to_string();
        Err($crate::utils::result::CompileError { msg, span })
    }};
}

/// 0-based line and column numbers. Add 1 for formatting.
pub struct SourcePosition {
    pub line: u32,
    pub col: u32,
}

impl SourcePosition {
    pub fn new(src: &str, offset: u32) -> SourcePosition {
        let offset = offset as usize;
        let slice = &src[..offset];
        let line_index = slice.chars().filter(|&c| c == '\n').count();
        let line_start_offset = slice.rfind('\n').unwrap_or(0);

        SourcePosition {
            line: line_index as u32,
            col: (offset - line_start_offset) as u32,
        }
    }
}

pub struct SourceRange {
    span: Span,
    start: SourcePosition,
    end: SourcePosition,
}

impl SourceRange {
    pub fn new(src: &str, span: Span) -> SourceRange {
        SourceRange {
            span,
            start: SourcePosition::new(src, span.start),
            end: SourcePosition::new(src, span.end),
        }
    }

    pub fn fmt<'rng, 'src>(&'rng self, src: &'src str) -> SourceRangeFormatter<'rng, 'src> {
        SourceRangeFormatter { range: self, src }
    }
}

pub struct SourceRangeFormatter<'rng, 'src> {
    range: &'rng SourceRange,
    src: &'src str,
}

impl<'rng, 'src> SourceRangeFormatter<'rng, 'src> {
    fn get_source_range_context(&self) -> &str {
        let span = self.range.span;
        let start = self.src[..(span.start as usize)].rfind('\n').map(|n| n + 1).unwrap_or(0);
        let end = (span.end as usize) + self.src[(span.end as usize)..].find('\n').unwrap_or(self.src.len());
        &self.src[start..end]
    }
}

impl<'rng, 'src> fmt::Display for SourceRangeFormatter<'rng, 'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let context = self.get_source_range_context();
        let lines = context.split('\n').collect::<Vec<&str>>();

        let (start_line, end_line) = (self.range.start.line, self.range.end.line);
        let line_count = end_line - start_line + 1;
        assert!(lines.len() == line_count as usize);

        let width = (end_line as f32).log10().ceil() as usize;

        for (line, line_num) in lines.iter().zip(start_line..end_line + 1) {
            writeln!(f, "{:width$} | {}", line_num, line, width = width)?;
        }

        // width of line number plus " | "
        let padding = width + 3;

        if line_count == 1 {
            let x_offset = padding + self.range.start.col as usize;
            for i in 0..x_offset {
                f.write_char(' ')?;
            }

            for i in 0..self.range.span.len() {
                if i == 0 {
                    f.write_char('^')?;
                } else {
                    f.write_char('~')?;
                }
            }
        }

        Ok(())
    }
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

    pub fn len(&self) -> usize {
        (self.end as usize) - (self.start as usize)
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

pub trait HasSpan {
    fn span(&self) -> Span;
}

impl HasSpan for Span {
    fn span(&self) -> Span {
        *self
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
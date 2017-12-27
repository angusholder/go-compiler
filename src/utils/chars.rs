use std::str::Chars;

#[derive(Debug)]
pub struct PeekableCharIndices<'src> {
    src: &'src str,
    iter: Chars<'src>,
    peeked: Option<(usize, char)>,
}

impl<'src> PeekableCharIndices<'src> {
    pub fn new(src: &'src str) -> PeekableCharIndices<'src> {
        PeekableCharIndices {
            src,
            iter: src.chars(),
            peeked: None,
        }
    }

    pub fn next(&mut self) -> Option<(usize, char)> {
        let index = self.raw_offset();

        if let Some((index, ch)) = self.peeked.take() {
            Some((index, ch))
        } else if let Some(ch) = self.iter.next() {
            Some((index, ch))
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<(usize, char)> {
        if let Some((index, ch)) = self.peeked {
            Some((index, ch))
        } else {
            let result = self.next();
            self.peeked = result;
            result
        }
    }

    fn raw_offset(&self) -> usize {
        (self.iter.as_str().as_ptr() as usize) - (self.src.as_ptr() as usize)
    }

    pub fn offset(&self) -> usize {
        if let Some((index, _)) = self.peeked {
            index
        } else {
            self.raw_offset()
        }
    }

    pub fn set_offset(&mut self, offset: usize) {
        self.iter = self.src[offset..].chars();
    }

    pub fn match_char(&mut self, expected: char) -> bool {
        self.match_char_with(|c| c == expected)
    }

    pub fn match_char_with<F: Fn(char) -> bool>(&mut self, cond: F) -> bool {
        if let Some((_, found)) = self.peek() {
            if cond(found) {
                self.peeked = None;
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}
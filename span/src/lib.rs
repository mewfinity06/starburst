#[derive(Clone, Debug, Eq, PartialEq, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn from_range(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    fn line_col(text: &str, index: usize) -> (usize, usize) {
        let bytes = text.chars();

        let mut line = 1;
        let mut column = 1;
        let mut position = 1;

        for byte in bytes {
            if position >= index {
                break;
            }
            match byte {
                '\n' => {
                    line += 1;
                    column = 1
                }
                _ => column += 1,
            }
            position += 1;
        }

        (line, column)
    }

    pub fn line_col_start(&self, text: &str) -> (usize, usize) {
        Span::line_col(text, self.start)
    }

    pub fn line_col_end(&self, text: &str) -> (usize, usize) {
        Span::line_col(text, self.end)
    }

    pub fn slice<'src>(&self, source: &'src str) -> &'src str {
        // SAFETY: `start` and `end` are UTF-8 char indices, not byte indexes
        // for ascii, this should work fine though
        &source[self.start..self.end]
    }
}

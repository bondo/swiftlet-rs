use std::fmt::Display;

use tree_sitter::{LanguageError, Range};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum InternalParserError {
    #[error("Tree-sitter error: {0}")]
    Setup(#[from] LanguageError),

    #[error("Tree-sitter parse error")]
    ParseExecution,

    #[error("UTF-8 error: {0}")]
    Utf8(#[from] std::str::Utf8Error),
}

pub(super) struct UserParseError {
    pub(super) range: Range,
    pub(super) message: String,
    pub(super) context: String,
}

impl UserParseError {
    fn push_repeat_char(str: &mut String, c: char, n: usize) {
        str.extend(std::iter::repeat(c).take(n))
    }

    pub(super) fn format(
        &self,
        source: &[u8],
    ) -> Result<FormattedUserParseError, InternalParserError> {
        let start = self.range.start_byte;
        let end = self.range.end_byte;

        let start_line = self.range.start_point.row + 1;
        let start_column = self.range.start_point.column + 1;
        let end_line = self.range.end_point.row + 1;
        let end_column = self.range.end_point.column + 1;

        let line_number_width = format!("{}", end_line).len();

        let start_of_line = source[..start]
            .iter()
            .rposition(|&b| b == b'\n')
            .map(|i| i + 1)
            .unwrap_or(0);
        let end_of_line = source[end..]
            .iter()
            .position(|&b| b == b'\n')
            .map(|i| end + i)
            .unwrap_or(source.len());

        let mut formatted = format!("Error: {}\n", self.message);

        formatted.push_str(&format!(
            " --> {file}:{start_line}:{start_column}\n",
            file = "?"
        ));

        if start_line == end_line {
            let line = std::str::from_utf8(&source[start_of_line..end_of_line])?;
            formatted.push_str(&format!("{start_line} | {line}\n"));

            let mut caret = String::new();
            Self::push_repeat_char(&mut caret, ' ', line_number_width + start_column + 2);
            Self::push_repeat_char(&mut caret, '^', end_column - start_column);

            formatted.push_str(&caret);
            if self.context.len() > 0 {
                formatted.push_str(&format!(" {}", self.context));
            }
            formatted.push('\n');
        } else {
            for (off, line) in std::str::from_utf8(&source[start_of_line..end_of_line])?
                .split('\n')
                .enumerate()
            {
                formatted.push_str(&format!(
                    "{line_number:>line_number_width$} | {c} {line}\n",
                    line_number = start_line + off,
                    c = if off == 0 { ' ' } else { '|' },
                ));
                if off == 0 {
                    formatted.push_str(&format!(
                        "{line_number:>line_number_width$} | +",
                        line_number = start_line + off,
                    ));

                    let mut caret = String::new();
                    Self::push_repeat_char(&mut caret, '-', line_number_width + start_column - 1);
                    caret.push('^');
                    formatted.push_str(&caret);

                    if self.context.len() > 0 {
                        formatted.push_str(&format!(" {}", self.context));
                    }
                    formatted.push('\n');
                }
            }

            formatted.push_str(&format!("{end_line:>line_number_width$} | +"));

            let mut caret = String::new();
            Self::push_repeat_char(&mut caret, '-', end_column - 1);
            caret.push('^');

            formatted.push_str(&caret);
            formatted.push('\n');

            formatted.push_str(&format!(
                " --> {file}:{end_line}:{end_column}\n",
                file = "?"
            ));
        }

        Ok(FormattedUserParseError { message: formatted })
    }
}

#[derive(Debug, PartialEq)]
pub struct FormattedUserParseError {
    message: String,
}

impl Display for FormattedUserParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParserError {
    #[error("Internal parser error: {0}")]
    Internal(#[from] InternalParserError),

    #[error("Parse errors:\n\n{}", .0.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n\n"))]
    Source(Vec<FormattedUserParseError>),
}

use std::{fmt::Display, iter::repeat, str};

use colored::Colorize;
use tree_sitter::{LanguageError, Range};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum InternalParserError {
    #[error("Tree-sitter error: {0}")]
    Setup(#[from] LanguageError),

    #[error("Tree-sitter parse error")]
    ParseExecution,

    #[error("UTF-8 error: {0}")]
    Utf8(#[from] str::Utf8Error),
}

pub(super) struct UserParseError {
    pub(super) range: Range,
    pub(super) message: String,
    pub(super) context: String,
}

impl UserParseError {
    pub(super) fn format(
        &self,
        source: &[u8],
    ) -> Result<FormattedUserParseError, InternalParserError> {
        fn leading_whitespace(str: &str) -> usize {
            str.chars().take_while(|c| c.is_whitespace()).count()
        }
        fn common_leading_whitespace(strs: &[&str]) -> usize {
            if strs.is_empty() {
                return 0;
            }

            fn lcp(a: &str, b: &str) -> usize {
                a.chars()
                    .zip(b.chars())
                    .take_while(|(a, b)| a == b && a.is_whitespace())
                    .count()
            }

            let first = strs[0];
            let len = leading_whitespace(first);
            len.min(strs[1..].iter().map(|s| lcp(first, s)).min().unwrap_or(len))
        }

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

        let mut formatted = format!(
            "{error}: {message}\n",
            error = "error".red(),
            message = self.message
        );

        formatted.push_str(&format!(
            " {arrow} {file}:{start_line}:{start_column}\n",
            arrow = "-->".blue(),
            file = "?"
        ));

        if start_line == end_line {
            let line = str::from_utf8(&source[start_of_line..end_of_line])?;
            let trim = leading_whitespace(line);
            let line = &line[trim..];

            formatted.push_str(&format!(
                "{pos} {line}\n",
                pos = format!("{start_line} |").blue()
            ));

            let mut caret = String::new();
            caret.extend(repeat(' ').take(line_number_width + start_column - trim + 2));
            caret.extend(repeat('^').take(end_column - start_column));

            formatted.push_str(&format!("{caret}", caret = caret.red()));
            if !self.context.is_empty() {
                formatted.push_str(&format!(" {}", self.context.red()));
            }
            formatted.push('\n');
        } else {
            let lines: Vec<&str> = str::from_utf8(&source[start_of_line..end_of_line])?
                .split('\n')
                .collect();
            let trim = common_leading_whitespace(&lines);

            for (off, line) in lines.iter().map(|l| &l[trim..]).enumerate() {
                formatted.push_str(&format!(
                    "{pos} {c} {line}\n",
                    pos = format!(
                        "{line_number:>line_number_width$} |",
                        line_number = start_line + off
                    )
                    .blue(),
                    c = (if off == 0 { " " } else { "│" }).red(),
                ));
                if off == 0 {
                    formatted.push_str(&format!(
                        "{pos} ",
                        pos = format!("{c:line_number_width$} |", c = ' ').blue(),
                    ));

                    let mut caret = "╭".to_string();
                    caret.extend(repeat('─').take(line_number_width + start_column - trim - 1));
                    caret.push('^');
                    formatted.push_str(&format!("{caret}", caret = caret.red()));

                    if !self.context.is_empty() {
                        formatted.push_str(&format!(" {}", self.context.red()));
                    }
                    formatted.push('\n');
                }
            }

            formatted.push_str(&format!(
                "{pos} ",
                pos = format!("{c:line_number_width$} |", c = ' ').blue(),
            ));

            let mut caret = "╰".to_string();
            caret.extend(repeat('─').take(end_column - trim - 1));
            caret.push('^');

            formatted.push_str(&format!("{caret}\n", caret = caret.red()));
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

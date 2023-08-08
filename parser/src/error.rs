use std::str;

use parser_common::ParseError;
use tree_sitter::LanguageError;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum InternalParserError {
    #[error("Tree-sitter error: {0}")]
    Setup(#[from] LanguageError),

    #[error("Tree-sitter parse error")]
    ParseExecution,

    #[error("UTF-8 error: {0}")]
    Utf8(#[from] str::Utf8Error),
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    Internal(InternalParserError),

    Source(Vec<ParseError>),
}

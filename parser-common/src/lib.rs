#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub context: String,
    pub start_byte: usize,
    pub end_byte: usize,
}

impl ParseError {
    pub fn print(&self, source: &str) {
        use ariadne::{Label, Report, ReportKind, Source};

        // TODO: That label should be character offsets, not byte offsets
        Report::build(ReportKind::Error, (), 34)
            .with_message(&self.message)
            .with_label(Label::new(self.start_byte..self.end_byte).with_message(&self.context))
            .finish()
            .print(Source::from(source))
            .unwrap();
    }
}

pub trait Extract: Sized {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<ParseError>>;
    fn can_extract_kind(kind: &str) -> bool;
}

impl Extract for i32 {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<ParseError>> {
        node.utf8_text(source)
            .expect("valid utf8")
            .parse()
            .map_err(|e| {
                vec![node_error(
                    node,
                    "Failed to parse i32".to_string(),
                    format!("{e}"),
                )]
            })
    }

    fn can_extract_kind(_kind: &str) -> bool {
        false
    }
}

impl Extract for String {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<ParseError>> {
        Ok(node.utf8_text(source).expect("valid utf8").to_string())
    }

    fn can_extract_kind(_kind: &str) -> bool {
        false
    }
}

impl<T: Extract> Extract for Option<T> {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<ParseError>> {
        if T::can_extract_kind(node.kind()) {
            T::extract(node, source).map(Some)
        } else {
            Ok(None)
        }
    }

    fn can_extract_kind(_kind: &str) -> bool {
        true
    }
}

impl<T: Extract> Extract for Box<T> {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<ParseError>> {
        T::extract(node, source).map(Box::new)
    }

    fn can_extract_kind(kind: &str) -> bool {
        T::can_extract_kind(kind)
    }
}

impl<T: Extract> Extract for Vec<T> {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, Vec<ParseError>> {
        let mut cursor = node.walk();

        if !cursor.goto_first_child() {
            return Ok(vec![]);
        }

        let mut errors = Vec::new();
        let mut result = Vec::new();

        loop {
            if !cursor.node().is_extra() {
                match T::extract(cursor.node(), source) {
                    Ok(value) => result.push(value),
                    Err(mut child_errors) => errors.append(&mut child_errors),
                }
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    fn can_extract_kind(kind: &str) -> bool {
        T::can_extract_kind(kind)
    }
}

pub fn not_implemented_error(node: tree_sitter::Node) -> ParseError {
    ParseError {
        message: "Not implemented".to_string(),
        context: node.kind().to_string(),
        start_byte: node.range().start_byte,
        end_byte: node.range().end_byte,
    }
}

pub fn node_error(node: tree_sitter::Node, message: String, context: String) -> ParseError {
    ParseError {
        message,
        context,
        start_byte: node.range().start_byte,
        end_byte: node.range().end_byte,
    }
}

pub fn parse_error(node: tree_sitter::Node) -> ParseError {
    ParseError {
        message: "Invalid Swift".to_string(),
        context: node.kind().to_string(),
        start_byte: node.range().start_byte,
        end_byte: node.range().end_byte,
    }
}

pub fn missing_error(node: tree_sitter::Node) -> ParseError {
    ParseError {
        message: "Missing node".to_string(),
        context: node.kind().to_string(),
        start_byte: node.range().start_byte,
        end_byte: node.range().end_byte,
    }
}

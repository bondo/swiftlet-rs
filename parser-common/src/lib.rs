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
        use yansi::Color;

        // TODO: That label should be character offsets, not byte offsets
        Report::build(ReportKind::Error, (), 34)
            .with_message(&self.message)
            .with_label(
                Label::new(self.start_byte..self.end_byte)
                    .with_color(Color::Red)
                    .with_message(&self.context),
            )
            .finish()
            .print(Source::from(source))
            .unwrap();
    }
}

#[derive(Debug, PartialEq)]
pub enum ExtractError<V> {
    Advance(Vec<ParseError>),
    Skip(V),
}

pub trait Extract: Sized {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ExtractError<Self>>;
}

impl Extract for i32 {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ExtractError<Self>> {
        node.utf8_text(source)
            .expect("valid utf8")
            .parse()
            .map_err(|e| {
                ExtractError::Advance(vec![node_error(
                    node,
                    "Failed to parse i32".to_string(),
                    format!("{e}"),
                )])
            })
    }
}

impl Extract for String {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ExtractError<Self>> {
        Ok(node.utf8_text(source).expect("valid utf8").to_string())
    }
}

impl<T: Extract> Extract for Option<T> {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ExtractError<Self>> {
        T::extract(node, source)
            .map(Some)
            .map_err(|_| ExtractError::Skip(None))
    }
}

impl<T: Extract> Extract for Box<T> {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ExtractError<Self>> {
        T::extract(node, source).map(Box::new).map_err(|e| match e {
            ExtractError::Advance(errors) => ExtractError::Advance(errors),
            ExtractError::Skip(v) => ExtractError::Skip(Box::new(v)),
        })
    }
}

impl<T: Extract> Extract for Vec<T> {
    fn extract(node: tree_sitter::Node<'_>, source: &[u8]) -> Result<Self, ExtractError<Self>> {
        let mut cursor = node.walk();
        let mut errors = Vec::new();
        let mut result = Vec::new();

        loop {
            if !cursor.node().is_extra() {
                match T::extract(cursor.node(), source) {
                    Ok(value) => result.push(value),
                    Err(ExtractError::Advance(mut child_errors)) => {
                        errors.append(&mut child_errors)
                    }
                    Err(ExtractError::Skip(_)) => (),
                }
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(ExtractError::Advance(errors))
        }
    }
}

pub fn not_implemented_error(
    node: tree_sitter::Node,
    message: &str,
    constructor: &str,
) -> ParseError {
    let kind = if let Some(parent) = node.parent() {
        format!("{} in {}", node.kind(), parent.kind())
    } else {
        node.kind().to_string()
    };
    ParseError {
        message: format!("Not implemented. Expected {message}."),
        context: format!("Creating {constructor} from {kind}"),
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

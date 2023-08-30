use std::fmt::{self, Display, Formatter};

use ariadne::{Label, Report, ReportKind, Source};
use pest::Span;
use yansi::Color;

#[derive(Debug)]
enum SwiftletErrorKind {
    Syntax,
    Runtime,
}

impl Display for SwiftletErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SwiftletErrorKind::Syntax => write!(f, "Syntax Error"),
            SwiftletErrorKind::Runtime => write!(f, "Runtime Error"),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub struct SwiftletError<'i> {
    kind: SwiftletErrorKind,
    span: Span<'i>,
    message: String,
}

impl<'i> SwiftletError<'i> {
    fn new(kind: SwiftletErrorKind, span: Span<'i>, message: String) -> Self {
        Self {
            kind,
            span,
            message,
        }
    }

    pub fn syntax<M: ToString>(span: Span<'i>, message: M) -> Self {
        Self::new(SwiftletErrorKind::Syntax, span, message.to_string())
    }

    pub fn runtime<M: ToString>(span: Span<'i>, message: M) -> Self {
        Self::new(SwiftletErrorKind::Runtime, span, message.to_string())
    }

    fn build_report(&self) -> Report<'i> {
        Report::build(ReportKind::Error, (), 0)
            .with_message(self.kind.to_string())
            .with_label(
                Label::new(self.span.start()..self.span.end())
                    .with_message(&self.message)
                    .with_color(Color::Red),
            )
            .finish()
    }
}

pub struct SwiftletErrorPrinter {
    source: Source,
}

impl SwiftletErrorPrinter {
    pub fn new(input: &str) -> Self {
        Self {
            source: Source::from(input),
        }
    }

    pub fn eprint(&mut self, error: &SwiftletError) {
        error.build_report().eprint(&mut self.source).unwrap();
    }

    pub fn format(&mut self, error: &SwiftletError) -> String {
        let mut buffer = Vec::new();
        error
            .build_report()
            .write(&mut self.source, &mut buffer)
            .unwrap();
        String::from_utf8(buffer).unwrap()
    }
}

impl<'i> Display for SwiftletError<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut printer = SwiftletErrorPrinter::new(self.span.get_input());
        write!(f, "{}", printer.format(self))
    }
}

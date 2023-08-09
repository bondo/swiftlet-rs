use ast::Program;
use parser_common::{Extract, ExtractError};
use tree_sitter::{Parser, TreeCursor};

mod ast;
mod error;

pub use ast::*;
pub use error::*;

pub fn parse(source: &str) -> Result<Program, ParserError> {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_swift::language())
        .map_err(|e| ParserError::Internal(InternalParserError::Setup(e)))?;
    let tree = parser
        .parse(source, None)
        .ok_or(ParserError::Internal(InternalParserError::ParseExecution))?;

    let source = source.as_bytes();

    match Program::extract(tree.root_node(), source) {
        Ok(ast) => Ok(ast),
        Err(ExtractError::Advance(errors)) => Err(ParserError::Source(errors)),
        Err(ExtractError::Skip(_)) => {
            Err(ParserError::Internal(InternalParserError::ParseExecution))
        }
    }
}

pub fn print_syntax_tree(source: &str) {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_swift::language())
        .expect("Failed to load Swift language");
    let tree = parser
        .parse(source, None)
        .expect("Failed to parse source code");

    fn print_node(cursor: &mut TreeCursor, depth: usize) {
        let node = cursor.node();
        println!("{ind}{kind}", ind = "  ".repeat(depth), kind = node.kind());
        if cursor.goto_first_child() {
            loop {
                print_node(cursor, depth + 1);
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
            cursor.goto_parent();
        }
    }
    print_node(&mut tree.walk(), 0);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_1() {
        let result = parse(
            r#"
                var foo: Int = 4;
                let bar = foo;
                print(bar) // Prints 4
            "#,
        );
        assert_eq!(
            result,
            Ok(Program {
                exprs: vec![
                    Expr::PropertyDeclaration(PropertyDeclaration {
                        qualifier: Qualifier::Var,
                        lhs: Pattern {
                            identifier: (SimpleIdentifier("foo".to_string()))
                        },
                        r#type: Some(TypeAnnotation {
                            colon: ColonToken,
                            r#type: TypeIdentifier::UserType(UserType {
                                name: "Int".to_string()
                            })
                        }),
                        eq: EqToken,
                        rhs: Box::new(Expr::IntegerLiteral(4)),
                    }),
                    Expr::PropertyDeclaration(PropertyDeclaration {
                        qualifier: Qualifier::Let,
                        lhs: Pattern {
                            identifier: (SimpleIdentifier("bar".to_string()))
                        },
                        r#type: None,
                        eq: EqToken,
                        rhs: Box::new(Expr::SimpleIdentifier(SimpleIdentifier("foo".to_string()))),
                    }),
                    Expr::CallExpression(CallExpression {
                        callee: SimpleIdentifier("print".to_string()),
                        arguments: CallSuffix::ValueArguments(ValueArguments(vec![
                            ValueArgument {
                                label: None,
                                value: Box::new(Expr::SimpleIdentifier(SimpleIdentifier(
                                    "bar".to_string()
                                ))),
                            }
                        ])),
                    }),
                ],
            })
        );
    }

    #[test]
    fn example_2() {
        let result = parse(
            r#"
                struct Vec2 { var x: Int, y: Int };
                var v = Vec2 (x: 4, y: 2);
                print(v.y) // Prints 2
            "#,
        );
        assert_eq!(format!("{result:?}"), "Ok(SwiftletProc {})");
    }
}

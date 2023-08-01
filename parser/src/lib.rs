use ast::Program;
use tree_sitter::Parser;

mod ast;
mod error;

pub use ast::*;
pub use error::*;

pub fn parse(source: &str) -> Result<Program, ParserError> {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_swift::language())
        .map_err(InternalParserError::from)?;
    let tree = parser
        .parse(source, None)
        .ok_or(InternalParserError::ParseExecution)?;

    let mut cursor = tree.walk();
    let source = source.as_bytes();
    let (ast, errors) = Program::parse(&mut cursor, source)?;
    if !errors.is_empty() {
        let errors = errors
            .iter()
            .map(|e| e.format(source))
            .collect::<Result<Vec<_>, _>>()?;
        Err(ParserError::Source(errors))
    } else {
        Ok(ast)
    }
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
                        lhs: Pattern::SimpleIdentifier(SimpleIdentifier {
                            name: "foo".to_string()
                        },),
                        r#type: Some(TypeIdentifier::UserType(UserType {
                            name: "Int".to_string()
                        },),),
                        rhs: Box::new(Expr::IntegerLiteral(4,)),
                    },),
                    Expr::PropertyDeclaration(PropertyDeclaration {
                        qualifier: Qualifier::Let,
                        lhs: Pattern::SimpleIdentifier(SimpleIdentifier {
                            name: "bar".to_string()
                        },),
                        r#type: None,
                        rhs: Box::new(Expr::SimpleIdentifier(SimpleIdentifier {
                            name: "foo".to_string()
                        },)),
                    },),
                    Expr::CallExpression(CallExpression {
                        callee: SimpleIdentifier {
                            name: "print".to_string()
                        },
                        arguments: CallSuffix::ValueArguments(ValueArguments(vec![
                            ValueArgument::SimpleIdentifier(SimpleIdentifier {
                                name: "bar".to_string()
                            },),
                        ],),),
                    },),
                ],
            })
        );
    }

    // #[test]
    // fn example_2() {
    //     let result = parse(
    //         r#"
    //             struct Vec2 { ... };
    //             var v = Vec2 (x: 4, y: 2);
    //             print(v.y) // Prints 2
    //         "#,
    //     );
    //     assert_eq!(format!("{result:?}"), "Ok(SwiftletProc {})");
    // }
}

use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};

mod ast;

pub use ast::*;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct SwiftletParser;

#[derive(thiserror::Error, Debug)]
#[error("{0}")]
pub struct Error(#[from] pest::error::Error<Rule>);

pub fn parse(input: &str) -> Result<Program, Error> {
    let mut statements = vec![];
    let pairs = SwiftletParser::parse(Rule::Program, input)?;
    for pair in pairs {
        match pair.as_rule() {
            Rule::Stmt => statements.append(&mut build_statements_from_pair(pair)),
            Rule::EOI => {}
            r => panic!("Unexpected rule in program: {r:?}"),
        }
    }
    Ok(Program { statements })
}

fn build_statements_from_pair(pair: Pair<Rule>) -> Vec<Statement> {
    debug_assert_eq!(pair.as_rule(), Rule::Stmt);

    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();

    let stmts = match pair.as_rule() {
        Rule::Assign => vec![Statement::Assignment(build_assignment_from_pair(pair))],
        Rule::PropertyDecl => build_property_decelerations_from_pair(pair)
            .into_iter()
            .map(|p| Statement::PropertyDeceleration(p))
            .collect(),
        Rule::StructDecl => {
            let mut pairs = pair.into_inner();
            let name = build_identifier_from_pair(pairs.next().unwrap());
            let properties = pairs
                .flat_map(|p| build_property_decelerations_from_pair(p))
                .collect();

            vec![Statement::StructDeceleration(StructDeceleration {
                name,
                properties,
            })]
        }
        Rule::If => {
            let mut pairs = pair.into_inner();
            let condition = build_expression_from_pair(pairs.next().unwrap());
            let if_branch = build_statements_from_pair(pairs.next().unwrap());
            let else_branch = if let Some(pair) = pairs.next() {
                build_statements_from_pair(pair)
            } else {
                vec![]
            };

            debug_assert_eq!(pairs.next(), None);

            vec![Statement::IfStatement(IfStatement {
                condition,
                if_branch,
                else_branch,
            })]
        }
        Rule::While => {
            let mut pairs = pair.into_inner();
            let condition = build_expression_from_pair(pairs.next().unwrap());
            let body = build_statements_from_pair(pairs.next().unwrap());

            debug_assert_eq!(pairs.next(), None);

            vec![Statement::WhileStatement(WhileStatement {
                condition,
                body,
            })]
        }
        Rule::Print => {
            let mut pairs = pair.into_inner();
            let expression = build_expression_from_pair(pairs.next().unwrap());

            debug_assert_eq!(pairs.next(), None);

            vec![Statement::PrintStatement(PrintStatement { expression })]
        }
        Rule::Block => {
            let statements = pair
                .into_inner()
                .flat_map(|p| build_statements_from_pair(p))
                .collect();
            vec![Statement::BlockStatement(BlockStatement { statements })]
        }
        r => panic!("Unexpected rule in statement: {r:?}"),
    };

    debug_assert_eq!(pairs.next(), None);

    stmts
}

fn build_assignment_from_pair(pair: Pair<Rule>) -> Assignment {
    debug_assert_eq!(pair.as_rule(), Rule::Assign);

    let mut pairs = pair.into_inner();
    let lhs = build_identifier_from_pair(pairs.next().unwrap());
    let rhs = build_expression_from_pair(pairs.next().unwrap());

    debug_assert_eq!(pairs.next(), None);

    Assignment { lhs, rhs }
}

fn build_property_decelerations_from_pair(pair: Pair<Rule>) -> Vec<PropertyDeceleration> {
    debug_assert_eq!(pair.as_rule(), Rule::PropertyDecl);

    let mut pairs = pair.into_inner();
    let qualifier = match pairs.next().unwrap().as_rule() {
        Rule::Var => Qualifier::Var,
        Rule::Let => Qualifier::Let,
        v => panic!("Unexpected qualifier value: '{v:?}'"),
    };

    let mut decls = vec![];

    while let Some(pair) = pairs.next() {
        let name = build_identifier_from_pair(pair);

        let ty = {
            if let Some(Rule::Ident) = pairs.peek().map(|p| p.as_rule()) {
                Some(build_identifier_from_pair(pairs.next().unwrap()))
            } else {
                None
            }
        };

        let initializer = {
            if let Some(Rule::Expr) = pairs.peek().map(|p| p.as_rule()) {
                Some(build_expression_from_pair(pairs.next().unwrap()))
            } else {
                None
            }
        };

        decls.push(PropertyDeceleration {
            qualifier: qualifier.clone(),
            name,
            ty,
            initializer,
        });
    }

    decls
}

fn build_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::Expr);

    let mut pairs = pair.into_inner().rev();

    let mut expression = build_or_expression_from_pair(pairs.next().unwrap());

    while let Some(pair) = pairs.next() {
        let if_branch = build_or_expression_from_pair(pair);
        let condition = build_or_expression_from_pair(pairs.next().unwrap());

        expression = Expression::Conditional(
            Box::new(condition),
            Box::new(if_branch),
            Box::new(expression),
        )
    }

    expression
}

fn build_or_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::OrExpr);

    let mut pairs = pair.into_inner();

    let mut expr = build_and_expression_from_pair(pairs.next().unwrap());

    while let Some(rhs) = pairs.next() {
        expr = Expression::Or(
            Box::new(expr),
            Box::new(build_and_expression_from_pair(rhs)),
        );
    }

    expr
}

fn build_and_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::AndExpr);

    let mut pairs = pair.into_inner();

    let mut expr = build_eq_expression_from_pair(pairs.next().unwrap());

    while let Some(rhs) = pairs.next() {
        expr = Expression::And(Box::new(expr), Box::new(build_eq_expression_from_pair(rhs)))
    }

    expr
}

fn build_eq_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::EqExpr);

    let mut pairs = pair.into_inner();

    let mut expr = build_cmp_expression_from_pair(pairs.next().unwrap());

    if let Some(op) = pairs.next() {
        let op = match op.as_rule() {
            Rule::Eq => EqualityOperator::Equal,
            Rule::Ne => EqualityOperator::NotEqual,
            r => panic!("Unexpected equality operator: {r:?}"),
        };

        let rhs = build_cmp_expression_from_pair(pairs.next().unwrap());

        expr = Expression::Equality(Box::new(expr), op, Box::new(rhs));
    }

    expr
}

fn build_cmp_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::CmpExpr);

    let mut pairs = pair.into_inner();

    let mut expr = build_pratt_expression_from_pair(pairs.next().unwrap());

    if let Some(op) = pairs.next() {
        let op = match op.as_rule() {
            Rule::Lt => ComparisonOperator::LessThan,
            Rule::Le => ComparisonOperator::LessThanOrEqual,
            Rule::Gt => ComparisonOperator::GreaterThan,
            Rule::Ge => ComparisonOperator::GreaterThanOrEqual,
            r => panic!("Unexpected comparison operator: {r:?}"),
        };

        let rhs = build_pratt_expression_from_pair(pairs.next().unwrap());

        expr = Expression::Comparison(Box::new(expr), op, Box::new(rhs));
    }

    expr
}

fn build_pratt_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::PrattExpr);

    let pratt = PrattParser::new()
        .op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Sub, Assoc::Left))
        .op(Op::infix(Rule::Mul, Assoc::Left)
            | Op::infix(Rule::Div, Assoc::Left)
            | Op::infix(Rule::Rem, Assoc::Left))
        .op(Op::prefix(Rule::Neg) | Op::prefix(Rule::Not));

    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::Int => Expression::IntegerLiteral(primary.as_str().parse().unwrap()),
            Rule::Str => Expression::StringLiteral({
                let s = primary.as_str();
                s[1..s.len() - 1].to_string()
            }),
            Rule::True => Expression::BooleanLiteral(true),
            Rule::False => Expression::BooleanLiteral(false),
            Rule::Call => {
                let mut pairs = primary.into_inner();

                let mut expr =
                    Expression::Identifier(build_identifier_from_pair(pairs.next().unwrap()));

                while let Some(pair) = pairs.next() {
                    match pair.as_rule() {
                        Rule::Args => {
                            let mut pairs = pair.into_inner();
                            let mut arguments = vec![];

                            while let Some(p) = pairs.peek() {
                                let label = if p.as_rule() == Rule::Ident {
                                    Some(build_identifier_from_pair(pairs.next().unwrap()))
                                } else {
                                    None
                                };

                                let value = build_expression_from_pair(pairs.next().unwrap());

                                arguments.push(Argument { label, value });
                            }

                            expr = Expression::Invocation(Box::new(expr), arguments)
                        }
                        Rule::Ident => {
                            let property = build_identifier_from_pair(pair);
                            expr = Expression::PropertyAccess(Box::new(expr), property)
                        }
                        r => panic!("Unexpected rule in call expression: {:?}", r),
                    };
                }

                expr
            }
            Rule::Expr => build_expression_from_pair(primary),
            r => panic!("Unexpected rule in primary expression: {r:?}"),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::Neg => Expression::Unary(UnaryOperator::Negate, Box::new(rhs)),
            Rule::Not => Expression::Unary(UnaryOperator::Not, Box::new(rhs)),
            r => panic!("Unexpected rule in prefix expression: {r:?}"),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::Add => Expression::Term(Box::new(lhs), TermOperator::Plus, Box::new(rhs)),
            Rule::Sub => Expression::Term(Box::new(lhs), TermOperator::Minus, Box::new(rhs)),
            Rule::Mul => Expression::Factor(Box::new(lhs), FactorOperator::Multiply, Box::new(rhs)),
            Rule::Div => Expression::Factor(Box::new(lhs), FactorOperator::Divide, Box::new(rhs)),
            Rule::Rem => Expression::Factor(Box::new(lhs), FactorOperator::Modulo, Box::new(rhs)),
            r => panic!("Unexpected rule in infix expression: {r:?}"),
        })
        .parse(pair.into_inner())
}

fn build_identifier_from_pair(pair: Pair<Rule>) -> String {
    debug_assert_eq!(pair.as_rule(), Rule::Ident);
    pair.as_str().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_1() {
        let expression =
            parse(r#"let foo = 5 || !"20" < true && (john - 4) * -2 ? 42 : 19 < 30 ? 14 : 9;"#)
                .unwrap_or_else(|e| panic!("\n{e}\n"));

        assert_eq!(
            expression,
            Program {
                statements: vec![Statement::PropertyDeceleration(PropertyDeceleration {
                    qualifier: Qualifier::Let,
                    name: "foo".to_string(),
                    ty: None,
                    initializer: Some(Expression::Conditional(
                        Box::new(Expression::Or(
                            Box::new(Expression::IntegerLiteral(5)),
                            Box::new(Expression::And(
                                Box::new(Expression::Comparison(
                                    Box::new(Expression::Unary(
                                        UnaryOperator::Not,
                                        Box::new(Expression::StringLiteral("20".to_string()))
                                    )),
                                    ComparisonOperator::LessThan,
                                    Box::new(Expression::BooleanLiteral(true))
                                )),
                                Box::new(Expression::Factor(
                                    Box::new(Expression::Term(
                                        Box::new(Expression::Identifier("john".to_string())),
                                        TermOperator::Minus,
                                        Box::new(Expression::IntegerLiteral(4))
                                    )),
                                    FactorOperator::Multiply,
                                    Box::new(Expression::Unary(
                                        UnaryOperator::Negate,
                                        Box::new(Expression::IntegerLiteral(2))
                                    ))
                                ))
                            ))
                        )),
                        Box::new(Expression::IntegerLiteral(42)),
                        Box::new(Expression::Conditional(
                            Box::new(Expression::Comparison(
                                Box::new(Expression::IntegerLiteral(19)),
                                ComparisonOperator::LessThan,
                                Box::new(Expression::IntegerLiteral(30))
                            )),
                            Box::new(Expression::IntegerLiteral(14)),
                            Box::new(Expression::IntegerLiteral(9))
                        ))
                    ))
                })]
            }
        );
    }

    #[test]
    fn expr_2() {
        let expression = parse(r#"print(a > b && b < c);"#).unwrap_or_else(|e| panic!("\n{e}\n"));

        assert_eq!(
            expression,
            Program {
                statements: vec![Statement::PrintStatement(PrintStatement {
                    expression: Expression::And(
                        Box::new(Expression::Comparison(
                            Box::new(Expression::Identifier("a".to_string())),
                            ComparisonOperator::GreaterThan,
                            Box::new(Expression::Identifier("b".to_string()))
                        )),
                        Box::new(Expression::Comparison(
                            Box::new(Expression::Identifier("b".to_string())),
                            ComparisonOperator::LessThan,
                            Box::new(Expression::Identifier("c".to_string()))
                        ))
                    )
                })]
            }
        );
    }

    #[test]
    fn example_1() {
        let program = parse(
            r#"
                var foo: Int = 4;
                let bar = foo;
                print(bar); // Prints 4
            "#,
        )
        .unwrap_or_else(|e| panic!("\n{e}\n"));

        assert_eq!(
            program,
            Program {
                statements: vec![
                    Statement::PropertyDeceleration(PropertyDeceleration {
                        qualifier: Qualifier::Var,
                        name: "foo".to_string(),
                        ty: Some("Int".to_string()),
                        initializer: Some(Expression::IntegerLiteral(4))
                    }),
                    Statement::PropertyDeceleration(PropertyDeceleration {
                        qualifier: Qualifier::Let,
                        name: "bar".to_string(),
                        ty: None,
                        initializer: Some(Expression::Identifier("foo".to_string()))
                    }),
                    Statement::PrintStatement(PrintStatement {
                        expression: Expression::Identifier("bar".to_string())
                    })
                ]
            }
        );
    }

    #[test]
    fn example_2() {
        let program = parse(
            r#"
                struct Vec2 { var x: Int, y: Int };
                var v = Vec2 (x: 4, y: 2);
                print(v.y); // Prints 2
            "#,
        )
        .unwrap_or_else(|e| panic!("\n{e}\n"));

        assert_eq!(
            program,
            Program {
                statements: vec![
                    Statement::StructDeceleration(StructDeceleration {
                        name: "Vec2".to_string(),
                        properties: vec![
                            PropertyDeceleration {
                                qualifier: Qualifier::Var,
                                name: "x".to_string(),
                                ty: Some("Int".to_string()),
                                initializer: None
                            },
                            PropertyDeceleration {
                                qualifier: Qualifier::Var,
                                name: "y".to_string(),
                                ty: Some("Int".to_string()),
                                initializer: None
                            }
                        ]
                    }),
                    Statement::PropertyDeceleration(PropertyDeceleration {
                        qualifier: Qualifier::Var,
                        name: "v".to_string(),
                        ty: None,
                        initializer: Some(Expression::Invocation(
                            Box::new(Expression::Identifier("Vec2".to_string())),
                            vec![
                                Argument {
                                    label: Some("x".to_string()),
                                    value: Expression::IntegerLiteral(4)
                                },
                                Argument {
                                    label: Some("y".to_string()),
                                    value: Expression::IntegerLiteral(2)
                                }
                            ]
                        ))
                    }),
                    Statement::PrintStatement(PrintStatement {
                        expression: Expression::PropertyAccess(
                            Box::new(Expression::Identifier("v".to_string())),
                            "y".to_string()
                        )
                    })
                ]
            }
        );
    }
}

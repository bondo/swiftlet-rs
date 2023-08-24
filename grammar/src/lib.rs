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
            Rule::Stmt => statements.push(build_statement_from_pair(pair)),
            Rule::EOI => {}
            r => panic!("Unexpected rule in program: {r:?}"),
        }
    }
    Ok(Program { statements })
}

fn build_statement_from_pair(pair: Pair<Rule>) -> Statement {
    debug_assert_eq!(pair.as_rule(), Rule::Stmt);

    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();

    let stmt = match pair.as_rule() {
        Rule::Assign => Statement::Assignment(build_assignment_from_pair(pair)),
        Rule::PropertyDecl => {
            Statement::PropertyDeceleration(build_property_deceleration_from_pair(pair))
        }
        Rule::If => todo!(),
        Rule::While => todo!(),
        Rule::Print => {
            let mut pairs = pair.into_inner();
            let expression = build_expression_from_pair(pairs.next().unwrap());

            debug_assert_eq!(pairs.next(), None);

            Statement::PrintStatement(PrintStatement { expression })
        }
        r => panic!("Unexpected rule in statement: {r:?}"),
    };

    debug_assert_eq!(pairs.next(), None);

    stmt
}

fn build_assignment_from_pair(pair: Pair<Rule>) -> Assignment {
    debug_assert_eq!(pair.as_rule(), Rule::Assign);

    let mut pairs = pair.into_inner();
    let identifier = build_identifier_from_pair(pairs.next().unwrap());
    let expression = build_expression_from_pair(pairs.next().unwrap());

    debug_assert_eq!(pairs.next(), None);

    Assignment {
        lhs: identifier,
        rhs: expression,
    }
}

fn build_property_deceleration_from_pair(pair: Pair<Rule>) -> PropertyDeceleration {
    debug_assert_eq!(pair.as_rule(), Rule::PropertyDecl);

    let mut pairs = pair.into_inner();
    let qualifier = match pairs.next().unwrap().as_rule() {
        Rule::Var => Qualifier::Var,
        Rule::Let => Qualifier::Let,
        v => panic!("Unexpected qualifier value: '{v:?}'"),
    };
    let identifier = build_identifier_from_pair(pairs.next().unwrap());

    if pairs.peek().unwrap().as_rule() == Rule::Ident {
        let ty = build_identifier_from_pair(pairs.next().unwrap());
        let expression = build_expression_from_pair(pairs.next().unwrap());

        debug_assert_eq!(pairs.next(), None);

        PropertyDeceleration {
            qualifier,
            identifier,
            ty: Some(ty),
            expression,
        }
    } else {
        let expression = build_expression_from_pair(pairs.next().unwrap());

        debug_assert_eq!(pairs.next(), None);

        PropertyDeceleration {
            qualifier,
            identifier,
            ty: None,
            expression,
        }
    }
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
            Rule::Ident => Expression::Identifier(build_identifier_from_pair(primary)),
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
                    identifier: "foo".to_string(),
                    ty: None,
                    expression: Expression::Conditional(
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
                    )
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
                        identifier: "foo".to_string(),
                        ty: Some("Int".to_string()),
                        expression: Expression::IntegerLiteral(4)
                    }),
                    Statement::PropertyDeceleration(PropertyDeceleration {
                        qualifier: Qualifier::Let,
                        identifier: "bar".to_string(),
                        ty: None,
                        expression: Expression::Identifier("foo".to_string())
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
                    Statement::PropertyDeceleration(PropertyDeceleration {
                        qualifier: Qualifier::Var,
                        identifier: "foo".to_string(),
                        ty: Some("Int".to_string()),
                        expression: Expression::IntegerLiteral(4)
                    }),
                    Statement::PropertyDeceleration(PropertyDeceleration {
                        qualifier: Qualifier::Let,
                        identifier: "bar".to_string(),
                        ty: None,
                        expression: Expression::Identifier("foo".to_string())
                    }),
                    Statement::PrintStatement(PrintStatement {
                        expression: Expression::Identifier("bar".to_string())
                    })
                ]
            }
        );
    }
}

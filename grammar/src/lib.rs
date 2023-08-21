use pest::{iterators::Pair, Parser};

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
            Rule::Statement => statements.push(build_statement_from_pair(pair)),
            Rule::EOI => {}
            r => panic!("Unexpected rule in program: {r:?}"),
        }
    }
    Ok(Program { statements })
}

fn build_statement_from_pair(pair: Pair<Rule>) -> Statement {
    debug_assert_eq!(pair.as_rule(), Rule::Statement);

    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();

    let stmt = match pair.as_rule() {
        Rule::Assignment => Statement::Assignment(build_assignment_from_pair(pair)),
        Rule::PropertyDeceleration => {
            Statement::PropertyDeceleration(build_property_deceleration_from_pair(pair))
        }
        Rule::IfStatement => todo!(),
        Rule::WhileStatement => todo!(),
        Rule::PrintStatement => {
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
    debug_assert_eq!(pair.as_rule(), Rule::Assignment);

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
    debug_assert_eq!(pair.as_rule(), Rule::PropertyDeceleration);

    let mut pairs = pair.into_inner();
    let qualifier = match pairs.next().unwrap().as_str() {
        "var" => Qualifier::Var,
        "let" => Qualifier::Let,
        v => panic!("Unexpected qualifier value: '{v}'"),
    };
    let identifier = build_identifier_from_pair(pairs.next().unwrap());

    if pairs.peek().unwrap().as_rule() == Rule::Identifier {
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
    match pair.as_rule() {
        Rule::Expression => {
            let mut pairs = pair.into_inner();
            let pair = pairs.next().unwrap();
            let expr = build_expression_from_pair(pair);
            debug_assert_eq!(pairs.next(), None);
            expr
        }
        Rule::OrExpression => {
            let exprs: Vec<Expression> =
                pair.into_inner().map(build_expression_from_pair).collect();
            if exprs.len() == 1 {
                exprs[0].clone()
            } else {
                Expression::Or(exprs)
            }
        }
        Rule::AndExpression => {
            let exprs: Vec<Expression> =
                pair.into_inner().map(build_expression_from_pair).collect();
            if exprs.len() == 1 {
                exprs[0].clone()
            } else {
                Expression::And(exprs)
            }
        }
        Rule::EqualityExpression => {
            let mut pairs = pair.into_inner();
            let left = build_expression_from_pair(pairs.next().unwrap());

            if let Some(operator) = pairs.next() {
                debug_assert_eq!(operator.as_rule(), Rule::EqualityOperator);

                let operator = match operator.as_str() {
                    "==" => EqualityOperator::Equal,
                    "!=" => EqualityOperator::NotEqual,
                    v => panic!("Unexpected equality operator value: '{v}'"),
                };

                let right = build_expression_from_pair(pairs.next().unwrap());

                debug_assert_eq!(pairs.next(), None);

                Expression::Equality(Box::new(left), operator, Box::new(right))
            } else {
                return left;
            }
        }
        Rule::ComparisonExpression => {
            let mut pairs = pair.into_inner();
            let left = build_expression_from_pair(pairs.next().unwrap());

            if let Some(operator) = pairs.next() {
                debug_assert_eq!(operator.as_rule(), Rule::ComparisonOperator);

                let operator = match operator.as_str() {
                    "<" => ComparisonOperator::LessThan,
                    "<=" => ComparisonOperator::LessThanOrEqual,
                    ">" => ComparisonOperator::GreaterThan,
                    ">=" => ComparisonOperator::GreaterThanOrEqual,
                    v => panic!("Unexpected comparison operator value: '{v}'"),
                };

                let right = build_expression_from_pair(pairs.next().unwrap());

                debug_assert_eq!(pairs.next(), None);

                Expression::Comparison(Box::new(left), operator, Box::new(right))
            } else {
                return left;
            }
        }
        Rule::TermExpression => {
            let mut pairs = pair.into_inner().rev();

            let mut right = build_expression_from_pair(pairs.next().unwrap());

            while let Some(operator) = pairs.next() {
                debug_assert_eq!(operator.as_rule(), Rule::TermOperator);

                let operator = match operator.as_str() {
                    "+" => TermOperator::Plus,
                    "-" => TermOperator::Minus,
                    v => panic!("Unexpected term operator value: '{v}'"),
                };

                let left = build_expression_from_pair(pairs.next().unwrap());

                right = Expression::Term(Box::new(left), operator, Box::new(right));
            }

            right
        }
        Rule::FactorExpression => {
            let mut pairs = pair.into_inner().rev();

            let mut right = build_expression_from_pair(pairs.next().unwrap());

            while let Some(operator) = pairs.next() {
                debug_assert_eq!(operator.as_rule(), Rule::FactorOperator);

                let operator = match operator.as_str() {
                    "*" => FactorOperator::Multiply,
                    "/" => FactorOperator::Divide,
                    v => panic!("Unexpected factor operator value: '{v}'"),
                };

                let left = build_expression_from_pair(pairs.next().unwrap());

                right = Expression::Factor(Box::new(left), operator, Box::new(right));
            }

            right
        }
        Rule::UnaryExpression => todo!(),
        Rule::Identifier => {
            let identifier = pair.as_str().to_string();
            Expression::Identifier(identifier)
        }
        Rule::IntegerLiteral => {
            let value = pair.as_str().parse::<u64>().unwrap();
            Expression::IntegerLiteral(value)
        }
        Rule::BooleanLiteral => todo!(),
        Rule::StringLiteral => todo!(),
        r => panic!("Unexpected rule in expression: {r:?}"),
    }
}

fn build_identifier_from_pair(pair: Pair<Rule>) -> String {
    debug_assert_eq!(pair.as_rule(), Rule::Identifier);
    pair.as_str().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

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

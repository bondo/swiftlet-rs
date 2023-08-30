use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};

use ast::*;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct SwiftletParser;

pub fn parse(input: &str) -> Result<Program, SwiftletError> {
    let pairs = SwiftletParser::parse(Rule::Program, input).map_err(|e| {
        let span = match e.location {
            pest::error::InputLocation::Pos(pos) => pest::Span::new(input, pos, pos),
            pest::error::InputLocation::Span((start, end)) => pest::Span::new(input, start, end),
        };
        SwiftletError::syntax(span.unwrap(), e.variant.message())
    })?;

    let mut statements = vec![];

    for pair in pairs {
        match pair.as_rule() {
            Rule::Stmt => statements.extend(build_statements_from_pair(pair)),
            Rule::EOI => {}
            r => panic!("Unexpected rule in program: {r:?}"),
        }
    }

    Ok(Program { statements })
}

fn build_statements_from_pair(pair: Pair<Rule>) -> Vec<Statement> {
    debug_assert_eq!(pair.as_rule(), Rule::Stmt);
    pair.as_span().as_str().trim_end().to_string();

    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();

    let stmts = match pair.as_rule() {
        Rule::Assign => vec![Statement::Assignment(build_assignment_from_pair(pair))],
        Rule::PropertyDecl => build_property_decelerations_from_pair(pair)
            .into_iter()
            .map(Statement::PropertyDeceleration)
            .collect(),
        Rule::StructDecl => {
            let mut pairs = pair.into_inner();
            let name = build_identifier_from_pair(pairs.next().unwrap());
            let properties = pairs
                .flat_map(build_property_decelerations_from_pair)
                .collect();

            vec![Statement::StructDeceleration(StructDeceleration {
                name,
                properties,
            })]
        }
        Rule::If => {
            let mut pairs = pair.into_inner();
            let condition = build_spanned_expression_from_pair(pairs.next().unwrap());
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
            let condition = build_spanned_expression_from_pair(pairs.next().unwrap());
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
                .flat_map(build_statements_from_pair)
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

fn build_spanned_expression_from_pair(pair: Pair<Rule>) -> Spanned<Expression> {
    (pair.as_span(), build_expression_from_pair(pair))
}

fn build_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::Expr);

    let mut pairs = pair.into_inner().rev();

    let mut expression = build_or_expression_from_pair(pairs.next().unwrap());

    while let Some(pair) = pairs.next() {
        let if_branch = build_or_expression_from_pair(pair);

        let condition = build_spanned_or_expression_from_pair(pairs.next().unwrap());

        expression = Expression::Conditional(
            Box::new(condition),
            Box::new(if_branch),
            Box::new(expression),
        )
    }

    expression
}

fn build_spanned_or_expression_from_pair(pair: Pair<Rule>) -> Spanned<Expression> {
    (pair.as_span(), build_or_expression_from_pair(pair))
}

fn build_or_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::OrExpr);

    let mut pairs = pair.into_inner();

    let mut expr = build_and_expression_from_pair(pairs.next().unwrap());

    while let Some(op) = pairs.next() {
        debug_assert_eq!(op.as_rule(), Rule::OrOp);

        let rhs = build_and_expression_from_pair(pairs.next().unwrap());

        expr = Expression::Or(Box::new(expr), op.as_span(), Box::new(rhs));
    }

    expr
}

fn build_and_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::AndExpr);

    let mut pairs = pair.into_inner();

    let mut expr = build_eq_expression_from_pair(pairs.next().unwrap());

    while let Some(op) = pairs.next() {
        debug_assert_eq!(op.as_rule(), Rule::AndOp);

        let rhs = build_eq_expression_from_pair(pairs.next().unwrap());

        expr = Expression::And(Box::new(expr), op.as_span(), Box::new(rhs));
    }

    expr
}

fn build_eq_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::EqExpr);

    let mut pairs = pair.into_inner();

    let mut expr = build_cmp_expression_from_pair(pairs.next().unwrap());

    if let Some(pair) = pairs.next() {
        let op = match pair.as_rule() {
            Rule::Eq => EqualityOperator::Equal,
            Rule::Ne => EqualityOperator::NotEqual,
            r => panic!("Unexpected equality operator: {r:?}"),
        };

        let rhs = build_cmp_expression_from_pair(pairs.next().unwrap());

        expr = Expression::Equality(Box::new(expr), (pair.as_span(), op), Box::new(rhs));
    }

    expr
}

fn build_cmp_expression_from_pair(pair: Pair<Rule>) -> Expression {
    debug_assert_eq!(pair.as_rule(), Rule::CmpExpr);

    let mut pairs = pair.into_inner();

    let mut expr = build_pratt_expression_from_pair(pairs.next().unwrap());

    if let Some(pair) = pairs.next() {
        let op = match pair.as_rule() {
            Rule::Lt => ComparisonOperator::LessThan,
            Rule::Le => ComparisonOperator::LessThanOrEqual,
            Rule::Gt => ComparisonOperator::GreaterThan,
            Rule::Ge => ComparisonOperator::GreaterThanOrEqual,
            r => panic!("Unexpected comparison operator: {r:?}"),
        };

        let rhs = build_pratt_expression_from_pair(pairs.next().unwrap());

        expr = Expression::Comparison(Box::new(expr), (pair.as_span(), op), Box::new(rhs));
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

                let expr_pair = pairs.next().unwrap();
                let mut expr_span = expr_pair.as_span();
                let mut expr = Expression::Identifier(build_identifier_from_pair(expr_pair));

                for pair in pairs {
                    let pair_span = pair.as_span();
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

                                let value =
                                    build_spanned_expression_from_pair(pairs.next().unwrap());

                                arguments.push(Argument { label, value });
                            }

                            expr = Expression::Call(Box::new((expr_span, expr)), arguments);
                        }
                        Rule::Ident => {
                            let property = build_identifier_from_pair(pair);
                            expr = Expression::Navigation(Box::new((expr_span, expr)), property)
                        }
                        r => panic!("Unexpected rule in call expression: {:?}", r),
                    };

                    expr_span =
                        Span::new(expr_span.get_input(), expr_span.start(), pair_span.end())
                            .unwrap();
                }

                expr
            }
            Rule::Expr => build_expression_from_pair(primary),
            r => panic!("Unexpected rule in primary expression: {r:?}"),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::Neg => Expression::Unary((op.as_span(), UnaryOperator::Negate), Box::new(rhs)),
            Rule::Not => Expression::Unary((op.as_span(), UnaryOperator::Not), Box::new(rhs)),
            r => panic!("Unexpected rule in prefix expression: {r:?}"),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::Add => Expression::Term(
                Box::new(lhs),
                (op.as_span(), TermOperator::Plus),
                Box::new(rhs),
            ),
            Rule::Sub => Expression::Term(
                Box::new(lhs),
                (op.as_span(), TermOperator::Minus),
                Box::new(rhs),
            ),
            Rule::Mul => Expression::Factor(
                Box::new(lhs),
                (op.as_span(), FactorOperator::Multiply),
                Box::new(rhs),
            ),
            Rule::Div => Expression::Factor(
                Box::new(lhs),
                (op.as_span(), FactorOperator::Divide),
                Box::new(rhs),
            ),
            Rule::Rem => Expression::Factor(
                Box::new(lhs),
                (op.as_span(), FactorOperator::Modulo),
                Box::new(rhs),
            ),
            r => panic!("Unexpected rule in infix expression: {r:?}"),
        })
        .parse(pair.into_inner())
}

fn build_identifier_from_pair(pair: Pair<Rule>) -> Spanned<String> {
    debug_assert_eq!(pair.as_rule(), Rule::Ident);
    (pair.as_span(), pair.as_str().to_string())
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use super::*;

    #[test]
    fn expr_1() {
        let program =
            parse(r#"let foo = 5 || !"20" < true && (john - 4) * -2 ? 42 : 19 < 30 ? 14 : 9;"#)
                .unwrap_or_else(|e| panic!("\n{e}\n"));
        assert_debug_snapshot!(program);
    }

    #[test]
    fn expr_2() {
        let program = parse(r#"print(a > b && b < c);"#).unwrap_or_else(|e| panic!("\n{e}\n"));
        assert_debug_snapshot!(program);
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
        assert_debug_snapshot!(program);
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
        assert_debug_snapshot!(program);
    }
}

use tree_sitter::TreeCursor;

use crate::{ast::*, InternalParserError, UserParseError};

impl Program {
    pub(crate) fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Self, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "source_file");

        if !cursor.goto_first_child() {
            return Ok((
                Self { exprs: vec![] },
                vec![not_implemented_error(
                    "Program::convert::goto_first_child",
                    node,
                )],
            ));
        }

        let mut exprs: Vec<Expr> = vec![];
        let mut errors: Vec<UserParseError> = vec![];

        loop {
            let node = cursor.node();
            match node.kind() {
                "property_declaration" | "call_expression" => {
                    let (expr, mut errs) = Expr::convert(cursor, source)?;
                    if let Some(expr) = expr {
                        exprs.push(expr);
                    }
                    errors.append(&mut errs);
                }
                "comment" => (),
                "ERROR" => errors.push(parse_error(node)),
                _ => errors.push(not_implemented_error("Program", node)),
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }

        cursor.goto_parent();

        Ok((Self { exprs }, errors))
    }
}

impl Expr {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        match node.kind() {
            "call_expression" => {
                let (expr, errs) = CallExpression::convert(cursor, source)?;
                Ok((expr.map(Self::CallExpression), errs))
            }
            "navigation_expression" => {
                let (expr, errs) = NavigationExpression::convert(cursor, source)?;
                Ok((expr.map(Self::NavigationExpression), errs))
            }
            "property_declaration" => {
                let (expr, errs) = PropertyDeclaration::convert(cursor, source)?;
                Ok((expr.map(Self::PropertyDeclaration), errs))
            }
            "simple_identifier" => Ok((
                Some(Self::SimpleIdentifier(SimpleIdentifier::convert(
                    cursor, source,
                )?)),
                vec![],
            )),
            "integer_literal" => node.utf8_text(source)?.parse().ok().map_or(
                Ok((
                    None,
                    vec![not_implemented_error("Expr::integer_literal", node)],
                )),
                |value| Ok((Some(Self::IntegerLiteral(value)), vec![])),
            ),
            "ERROR" => Ok((None, vec![parse_error(node)])),
            _ => Ok((None, vec![not_implemented_error("Expr", node)])),
        }
    }
}

impl CallExpression {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "call_expression");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "CallExpression::convert::goto_first_child",
                    node,
                )],
            ));
        }

        let mut errors: Vec<UserParseError> = vec![];

        let node = cursor.node();
        let (callee, mut errs) = match node.kind() {
            "simple_identifier" => (Some(SimpleIdentifier::convert(cursor, source)?), vec![]),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("CallExpression", node)]),
        };
        errors.append(&mut errs);

        if !cursor.goto_next_sibling() {
            errors.push(not_implemented_error(
                "CallExpression::convert::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let node = cursor.node();
        let (arguments, mut errs) = match node.kind() {
            "call_suffix" => CallSuffix::convert(cursor, source)?,
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("CallExpression", node)]),
        };
        errors.append(&mut errs);

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "CallExpression::convert::goto_next_sibling",
                node,
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        cursor.goto_parent();

        Ok(match (callee, arguments) {
            (Some(callee), Some(arguments)) => (Some(Self { callee, arguments }), errors),
            _ => (None, errors),
        })
    }
}

impl CallSuffix {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "call_suffix");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "CallSuffix::convert::goto_first_child",
                    node,
                )],
            ));
        }

        let node = cursor.node();
        let (call_suffix, mut errors) = match node.kind() {
            "value_arguments" => {
                let (val, errs) = ValueArguments::convert(cursor, source)?;
                (Some(Self::ValueArguments(val)), errs)
            }
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("CallSuffix", node)]),
        };

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "CallSuffix::convert::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((call_suffix, errors))
    }
}

impl NavigationExpression {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "navigation_expression");

        if node.child_count() != 2 {
            return Ok((
                None,
                vec![not_implemented_error(
                    "NavigationExpression::convert::child_count",
                    node,
                )],
            ));
        }

        cursor.goto_first_child();
        let node = cursor.node();

        let mut errors: Vec<UserParseError> = vec![];

        let (target, mut errs) = match node.kind() {
            "simple_identifier" => (Some(SimpleIdentifier::convert(cursor, source)?), vec![]),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("ValueArgument", node)]),
        };
        errors.append(&mut errs);

        cursor.goto_next_sibling();

        let (suffix, mut errs) = match node.kind() {
            "navigation_suffix" => NavigationSuffix::convert(cursor, source)?,
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("ValueArgument", node)]),
        };
        errors.append(&mut errs);

        cursor.goto_parent();

        match (target, suffix) {
            (Some(target), Some(suffix)) => {
                Ok((Some(NavigationExpression { target, suffix }), errs))
            }
            _ => Ok((None, errs)),
        }
    }
}

impl NavigationSuffix {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "navigation_suffix");

        if node.child_count() != 2 {
            return Ok((
                None,
                vec![not_implemented_error(
                    "NavigationSuffix::convert::child_count",
                    node,
                )],
            ));
        }

        cursor.goto_first_child();
        let node = cursor.node();

        let mut errors: Vec<UserParseError> = vec![];

        match node.kind() {
            "." => (),
            "ERROR" => errors.push(parse_error(node)),
            _ => errors.push(not_implemented_error("NavigationSuffix", node)),
        };

        cursor.goto_next_sibling();

        let (suffix, mut errs) = match node.kind() {
            "simple_identifier" => (
                Some(NavigationSuffix(SimpleIdentifier::convert(cursor, source)?)),
                vec![],
            ),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("ValueArgument", node)]),
        };
        errors.append(&mut errs);

        cursor.goto_parent();

        Ok((suffix, errors))
    }
}

impl ValueArguments {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Self, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "value_arguments");

        if !cursor.goto_first_child() {
            return Ok((
                Self(vec![]),
                vec![not_implemented_error(
                    "Program::convert::goto_first_child",
                    node,
                )],
            ));
        }

        let mut args: Vec<ValueArgument> = vec![];
        let mut errors: Vec<UserParseError> = vec![];

        let node = cursor.node();
        match node.kind() {
            "(" => (),
            "ERROR" => errors.push(parse_error(node)),
            _ => errors.push(not_implemented_error("ValueArguments", node)),
        };
        if !cursor.goto_next_sibling() {
            errors.push(not_implemented_error(
                "ValueArguments::convert::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((Self(args), errors));
        }

        loop {
            let node = cursor.node();
            match node.kind() {
                "value_argument" => {
                    let (arg, mut errs) = ValueArgument::convert(cursor, source)?;
                    if let Some(arg) = arg {
                        args.push(arg);
                    }
                    errors.append(&mut errs);
                }
                "," => {
                    if !cursor.goto_next_sibling() {
                        errors.push(not_implemented_error(
                            "ValueArguments::convert::goto_next_sibling",
                            node,
                        ));
                        cursor.goto_parent();
                        return Ok((Self(args), errors));
                    }
                }
                ")" => {
                    if cursor.goto_next_sibling() {
                        let node = cursor.node();
                        errors.push(not_implemented_error(
                            "ValueArguments::convert::goto_next_sibling",
                            node,
                        ));
                    }
                    break;
                }
                "ERROR" => errors.push(parse_error(node)),
                _ => errors.push(not_implemented_error("ValueArguments", node)),
            }
            if !cursor.goto_next_sibling() {
                errors.push(not_implemented_error(
                    "ValueArguments::convert::goto_next_sibling",
                    node,
                ));
                break;
            }
        }

        cursor.goto_parent();

        Ok((Self(args), errors))
    }
}

impl ValueArgument {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "value_argument");

        match node.child_count() {
            1 => {
                cursor.goto_first_child();
                let (expr, errors) = Expr::convert(cursor, source)?;
                cursor.goto_parent();
                Ok((expr.map(|e| ValueArgument::Value(Box::new(e))), errors))
            }
            3 => {
                cursor.goto_first_child();
                let node = cursor.node();

                let mut errors: Vec<UserParseError> = vec![];

                let (identifier, mut errs) = match node.kind() {
                    "simple_identifier" => {
                        (Some(SimpleIdentifier::convert(cursor, source)?), vec![])
                    }
                    "ERROR" => (None, vec![parse_error(node)]),
                    _ => (None, vec![not_implemented_error("ValueArgument", node)]),
                };
                errors.append(&mut errs);

                cursor.goto_next_sibling();
                let node = cursor.node();
                match node.kind() {
                    ":" => (),
                    "ERROR" => errors.push(parse_error(node)),
                    _ => errors.push(not_implemented_error("PropertyDeclaration", node)),
                };

                cursor.goto_next_sibling();
                let (expr, mut errs) = Expr::convert(cursor, source)?;
                errors.append(&mut errs);

                cursor.goto_parent();

                match (identifier, expr) {
                    (Some(identifier), Some(expr)) => Ok((
                        Some(ValueArgument::NamedValue(identifier, Box::new(expr))),
                        errs,
                    )),
                    _ => Ok((None, errs)),
                }
            }
            _ => Ok((
                None,
                vec![not_implemented_error(
                    "ValueArgument::convert::child_count",
                    node,
                )],
            )),
        }
    }
}

impl SimpleIdentifier {
    fn convert(cursor: &mut TreeCursor, source: &[u8]) -> Result<Self, InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "simple_identifier");

        Ok(Self {
            name: node.utf8_text(source)?.to_string(),
        })
    }
}

impl Pattern {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "pattern");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "Pattern::convert::goto_first_child",
                    node,
                )],
            ));
        }

        let node = cursor.node();
        let (pattern, mut errors) = match node.kind() {
            "simple_identifier" => (
                Some(Self::SimpleIdentifier(SimpleIdentifier::convert(
                    cursor, source,
                )?)),
                vec![],
            ),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("Pattern", node)]),
        };

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "Pattern::convert::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((pattern, errors))
    }
}

impl UserType {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "user_type");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "UserType::convert::goto_first_child",
                    node,
                )],
            ));
        }

        let node = cursor.node();
        let (user_type, mut errors) = match node.kind() {
            "type_identifier" => (
                Some(Self {
                    name: node.utf8_text(source)?.to_string(),
                }),
                vec![],
            ),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("UserType", node)]),
        };

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "UserType::convert::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((user_type, errors))
    }
}

impl TypeIdentifier {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "type_annotation");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "TypeIdentifier::convert::goto_first_child",
                    node,
                )],
            ));
        }

        let node = cursor.node();
        let mut errors = match node.kind() {
            ":" => vec![],
            "ERROR" => vec![parse_error(node)],
            _ => vec![not_implemented_error("TypeIdentifier", node)],
        };

        if !cursor.goto_next_sibling() {
            errors.push(not_implemented_error(
                "TypeIdentifier::convert::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let node = cursor.node();
        let (identifier, mut errs) = match node.kind() {
            "user_type" => {
                let (user_type, errs) = UserType::convert(cursor, source)?;
                (user_type.map(Self::UserType), errs)
            }
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("TypeIdentifier", node)]),
        };
        errors.append(&mut errs);

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "TypeIdentifier::convert::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((identifier, errors))
    }
}

impl PropertyDeclaration {
    fn convert(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "property_declaration");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "PropertyDeclaration::convert::goto_first_child",
                    node,
                )],
            ));
        }

        let mut errors: Vec<UserParseError> = vec![];

        let node = cursor.node();
        let (qualifier, mut errs) = match node.kind() {
            "var" => (Some(Qualifier::Var), vec![]),
            "let" => (Some(Qualifier::Let), vec![]),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (
                None,
                vec![not_implemented_error("PropertyDeclaration", node)],
            ),
        };
        errors.append(&mut errs);

        if !cursor.goto_next_sibling() {
            errors.push(not_implemented_error(
                "PropertyDeclaration::convert::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let node = cursor.node();
        let (lhs, mut errs) = match node.kind() {
            "pattern" => Pattern::convert(cursor, source)?,
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (
                None,
                vec![not_implemented_error("PropertyDeclaration", node)],
            ),
        };
        errors.append(&mut errs);

        if !cursor.goto_next_sibling() {
            errors.push(not_implemented_error(
                "PropertyDeclaration::convert::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let node = cursor.node();
        let (r#type, mut errs) = match node.kind() {
            "type_annotation" => TypeIdentifier::convert(cursor, source)?,
            "=" => (None, vec![]),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (
                None,
                vec![not_implemented_error("PropertyDeclaration", node)],
            ),
        };
        errors.append(&mut errs);

        if r#type.is_some() {
            if !cursor.goto_next_sibling() {
                errors.push(not_implemented_error(
                    "PropertyDeclaration::convert::goto_next_sibling",
                    node.parent().unwrap(),
                ));
                cursor.goto_parent();
                return Ok((None, errors));
            }

            let node = cursor.node();
            match node.kind() {
                "=" => (),
                "ERROR" => errors.push(parse_error(node)),
                _ => errors.push(not_implemented_error("PropertyDeclaration", node)),
            };
        }

        if !cursor.goto_next_sibling() {
            errors.push(not_implemented_error(
                "PropertyDeclaration::convert::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let (rhs, mut errs) = Expr::convert(cursor, source)?;
        errors.append(&mut errs);

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "PropertyDeclaration::convert::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        match (qualifier, lhs, rhs) {
            (Some(qualifier), Some(lhs), Some(rhs)) => Ok((
                Some(Self {
                    qualifier,
                    lhs,
                    r#type,
                    rhs: Box::new(rhs),
                }),
                errors,
            )),
            _ => Ok((None, errors)),
        }
    }
}

fn parse_error(node: tree_sitter::Node) -> UserParseError {
    UserParseError {
        range: node.range(),
        message: "Invalid Swift".to_string(),
        context: node.kind().to_string(),
    }
}

fn not_implemented_error(context: &str, node: tree_sitter::Node) -> UserParseError {
    UserParseError {
        range: node.range(),
        message: "Not implemented".to_string(),
        context: format!("{context}::{kind}", kind = node.kind()),
    }
}

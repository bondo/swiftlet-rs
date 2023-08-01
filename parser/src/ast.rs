use tree_sitter::TreeCursor;

use crate::{InternalParserError, UserParseError};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub exprs: Vec<Expr>,
}

impl Program {
    pub(crate) fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Self, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "source_file");

        if !cursor.goto_first_child() {
            return Ok((
                Self { exprs: vec![] },
                vec![not_implemented_error(
                    "Program::parse::goto_first_child",
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
                    let (expr, mut errs) = Expr::parse(cursor, source)?;
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

#[derive(Debug, PartialEq)]
pub enum Expr {
    CallExpression(CallExpression),
    PropertyDeclaration(PropertyDeclaration),
    SimpleIdentifier(SimpleIdentifier),
    IntegerLiteral(i32),
}

impl Expr {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        match node.kind() {
            "call_expression" => {
                let (expr, errs) = CallExpression::parse(cursor, source)?;
                Ok((expr.map(Self::CallExpression), errs))
            }
            "property_declaration" => {
                let (expr, errs) = PropertyDeclaration::parse(cursor, source)?;
                Ok((expr.map(Self::PropertyDeclaration), errs))
            }
            "simple_identifier" => Ok((
                Some(Self::SimpleIdentifier(SimpleIdentifier::parse(
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

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub callee: SimpleIdentifier,
    pub arguments: CallSuffix,
}

impl CallExpression {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "call_expression");

        // todo!(
        //     "CallExpression::parse: {:#?}",
        //     node.children(cursor)
        //         .map(|n| format!("{kind}: {sexp}", kind = n.kind(), sexp = n.to_sexp()))
        //         .collect::<Vec<_>>()
        // );

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "CallExpression::parse::goto_first_child",
                    node,
                )],
            ));
        }

        let mut errors: Vec<UserParseError> = vec![];

        let node = cursor.node();
        let (callee, mut errs) = match node.kind() {
            "simple_identifier" => (Some(SimpleIdentifier::parse(cursor, source)?), vec![]),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("CallExpression", node)]),
        };
        errors.append(&mut errs);

        if !cursor.goto_next_sibling() {
            errors.push(not_implemented_error(
                "CallExpression::parse::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let node = cursor.node();
        let (arguments, mut errs) = match node.kind() {
            "call_suffix" => CallSuffix::parse(cursor, source)?,
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("CallExpression", node)]),
        };
        errors.append(&mut errs);

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "CallExpression::parse::goto_next_sibling",
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

#[derive(Debug, PartialEq)]
pub enum CallSuffix {
    ValueArguments(ValueArguments),
}

impl CallSuffix {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "call_suffix");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "CallSuffix::parse::goto_first_child",
                    node,
                )],
            ));
        }

        let node = cursor.node();
        let (call_suffix, mut errors) = match node.kind() {
            "value_arguments" => {
                let (val, errs) = ValueArguments::parse(cursor, source)?;
                (Some(Self::ValueArguments(val)), errs)
            }
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("CallSuffix", node)]),
        };

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "CallSuffix::parse::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((call_suffix, errors))
    }
}

#[derive(Debug, PartialEq)]
pub struct ValueArguments(pub(super) Vec<ValueArgument>);

impl ValueArguments {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Self, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "value_arguments");

        if !cursor.goto_first_child() {
            return Ok((
                Self(vec![]),
                vec![not_implemented_error(
                    "Program::parse::goto_first_child",
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
                "ValueArguments::parse::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((Self(args), errors));
        }

        loop {
            let node = cursor.node();
            match node.kind() {
                "value_argument" => {
                    let (arg, mut errs) = ValueArgument::parse(cursor, source)?;
                    if let Some(arg) = arg {
                        args.push(arg);
                    }
                    errors.append(&mut errs);
                }
                ")" => {
                    if cursor.goto_next_sibling() {
                        let node = cursor.node();
                        errors.push(not_implemented_error(
                            "ValueArguments::parse::goto_next_sibling",
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
                    "ValueArguments::parse::goto_next_sibling",
                    node,
                ));
                break;
            }
        }

        cursor.goto_parent();

        Ok((Self(args), errors))
    }
}

#[derive(Debug, PartialEq)]
pub enum ValueArgument {
    SimpleIdentifier(SimpleIdentifier),
}

impl ValueArgument {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "value_argument");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "ValueArgument::parse::goto_first_child",
                    node,
                )],
            ));
        }

        let node = cursor.node();
        let (value_argument, mut errors) = match node.kind() {
            "simple_identifier" => (
                Some(Self::SimpleIdentifier(SimpleIdentifier::parse(
                    cursor, source,
                )?)),
                vec![],
            ),
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("ValueArgument", node)]),
        };

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "ValueArgument::parse::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((value_argument, errors))
    }
}

#[derive(Debug, PartialEq)]
pub enum Qualifier {
    Var,
    Let,
}

#[derive(Debug, PartialEq)]
pub struct SimpleIdentifier {
    pub name: String,
}

impl SimpleIdentifier {
    fn parse(cursor: &mut TreeCursor, source: &[u8]) -> Result<Self, InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "simple_identifier");

        Ok(Self {
            name: node.utf8_text(source)?.to_string(),
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    SimpleIdentifier(SimpleIdentifier),
}

impl Pattern {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "pattern");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "Pattern::parse::goto_first_child",
                    node,
                )],
            ));
        }

        let node = cursor.node();
        let (pattern, mut errors) = match node.kind() {
            "simple_identifier" => (
                Some(Self::SimpleIdentifier(SimpleIdentifier::parse(
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
                "Pattern::parse::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((pattern, errors))
    }
}

#[derive(Debug, PartialEq)]
pub struct UserType {
    pub name: String,
}

impl UserType {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "user_type");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "UserType::parse::goto_first_child",
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
                "UserType::parse::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((user_type, errors))
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeIdentifier {
    UserType(UserType),
}

impl TypeIdentifier {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "type_annotation");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "TypeIdentifier::parse::goto_first_child",
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
                "TypeIdentifier::parse::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let node = cursor.node();
        let (identifier, mut errs) = match node.kind() {
            "user_type" => {
                let (user_type, errs) = UserType::parse(cursor, source)?;
                (user_type.map(Self::UserType), errs)
            }
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (None, vec![not_implemented_error("TypeIdentifier", node)]),
        };
        errors.append(&mut errs);

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "TypeIdentifier::parse::goto_next_sibling",
                node,
            ));
        }

        cursor.goto_parent();

        Ok((identifier, errors))
    }
}

#[derive(Debug, PartialEq)]
pub struct PropertyDeclaration {
    pub qualifier: Qualifier,
    pub lhs: Pattern,
    pub r#type: Option<TypeIdentifier>,
    pub rhs: Box<Expr>,
}

impl PropertyDeclaration {
    fn parse(
        cursor: &mut TreeCursor,
        source: &[u8],
    ) -> Result<(Option<Self>, Vec<UserParseError>), InternalParserError> {
        let node = cursor.node();
        debug_assert_eq!(node.kind(), "property_declaration");

        if !cursor.goto_first_child() {
            return Ok((
                None,
                vec![not_implemented_error(
                    "PropertyDeclaration::parse::goto_first_child",
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
                "PropertyDeclaration::parse::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let node = cursor.node();
        let (lhs, mut errs) = match node.kind() {
            "pattern" => Pattern::parse(cursor, source)?,
            "ERROR" => (None, vec![parse_error(node)]),
            _ => (
                None,
                vec![not_implemented_error("PropertyDeclaration", node)],
            ),
        };
        errors.append(&mut errs);

        if !cursor.goto_next_sibling() {
            errors.push(not_implemented_error(
                "PropertyDeclaration::parse::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let node = cursor.node();
        let (r#type, mut errs) = match node.kind() {
            "type_annotation" => TypeIdentifier::parse(cursor, source)?,
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
                    "PropertyDeclaration::parse::goto_next_sibling",
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
                "PropertyDeclaration::parse::goto_next_sibling",
                node.parent().unwrap(),
            ));
            cursor.goto_parent();
            return Ok((None, errors));
        }

        let (rhs, mut errs) = Expr::parse(cursor, source)?;
        errors.append(&mut errs);

        if cursor.goto_next_sibling() {
            let node = cursor.node();
            errors.push(not_implemented_error(
                "PropertyDeclaration::parse::goto_next_sibling",
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

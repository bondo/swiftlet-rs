use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::RwLock,
};

use ast::*;

#[derive(Debug, PartialEq, Clone)]
struct StructDef {
    name: String,
    fields: HashSet<String>,
}

#[derive(Debug, PartialEq, Clone)]
struct StructValue {
    name: String,
    values: HashMap<String, i64>,
}

#[derive(Debug, PartialEq, Clone)]
enum ExpressionValue {
    Number(i64),
    String(String),
    Boolean(bool),
    Constructor(StructDef),
    Struct(StructValue),
}

impl ExpressionValue {
    fn is_true<'i>(&self, span: Span<'i>) -> Result<bool, SwiftletError<'i>> {
        match self {
            ExpressionValue::Boolean(b) => Ok(*b),
            _ => Err(SwiftletError::runtime(
                span,
                format!("Expected boolean, got {self}"),
            )),
        }
    }

    fn is_false<'i>(&self, span: Span<'i>) -> Result<bool, SwiftletError<'i>> {
        Ok(!self.is_true(span)?)
    }

    fn as_struct<'i>(&self, span: Span<'i>) -> Result<StructValue, SwiftletError<'i>> {
        match self {
            ExpressionValue::Struct(v) => Ok(v.clone()),
            _ => Err(SwiftletError::runtime(
                span,
                format!("Expected struct, got {self}"),
            )),
        }
    }

    fn as_constructor<'i>(&self, span: Span<'i>) -> Result<StructDef, SwiftletError<'i>> {
        match self {
            ExpressionValue::Constructor(v) => Ok(v.clone()),
            _ => Err(SwiftletError::runtime(
                span,
                format!("Expected constructor, got {self}"),
            )),
        }
    }
}

#[derive(Debug)]
enum EnvDecl {
    Constant(Option<ExpressionValue>),
    Mutable(Option<ExpressionValue>),
    Struct(StructDef),
}

#[derive(Clone, Debug)]
struct Environment {
    parent: Option<Rc<Environment>>,
    decls: Rc<RwLock<HashMap<String, EnvDecl>>>,
}

impl Environment {
    fn new(parent: Option<Environment>) -> Self {
        Self {
            parent: parent.map(Rc::new),
            decls: Rc::new(RwLock::new(HashMap::new())),
        }
    }

    fn get<'i>(&self, name: &Spanned<'i, String>) -> Result<ExpressionValue, SwiftletError<'i>> {
        if let Some(decl) = self.decls.read().unwrap().get(&name.1) {
            match decl {
                EnvDecl::Constant(Some(val)) => return Ok(val.clone()),
                EnvDecl::Mutable(Some(val)) => return Ok(val.clone()),
                EnvDecl::Constant(None) | EnvDecl::Mutable(None) => {
                    return Err(SwiftletError::runtime(
                        name.0,
                        format!("Identifier '{}' not initialized", name.1),
                    ))
                }
                EnvDecl::Struct(def) => return Ok(ExpressionValue::Constructor(def.clone())),
            }
        }
        if let Some(parent) = &self.parent {
            return parent.get(name);
        }
        Err(SwiftletError::runtime(
            name.0,
            format!("Identifier '{}' not found", name.1),
        ))
    }

    fn declare_property<'i>(
        &mut self,
        qual: &Qualifier,
        name: &Spanned<'i, String>,
        value: Option<ExpressionValue>,
    ) -> Result<(), SwiftletError<'i>> {
        let mut decls = self.decls.write().unwrap();
        if decls.contains_key(&name.1) {
            return Err(SwiftletError::runtime(
                name.0,
                format!(
                    "Identifier '{}' is already declared in the current scope",
                    name.1
                ),
            ));
        }
        match qual {
            Qualifier::Var => decls.insert(name.1.to_string(), EnvDecl::Mutable(value)),
            Qualifier::Let => decls.insert(name.1.to_string(), EnvDecl::Constant(value)),
        };
        Ok(())
    }

    fn declare_struct<'i>(
        &mut self,
        name: &Spanned<'i, String>,
        fields: Vec<String>,
    ) -> Result<(), SwiftletError<'i>> {
        let mut decls = self.decls.write().unwrap();
        if decls.contains_key(&name.1) {
            return Err(SwiftletError::runtime(
                name.0,
                format!(
                    "Identifier '{}' is already declared in the current scope",
                    name.1
                ),
            ));
        }
        let set: HashSet<String> = fields.iter().cloned().collect();
        if set.len() != fields.len() {
            return Err(SwiftletError::runtime(
                name.0,
                format!("Duplicate field names in struct '{}'", name.1),
            ));
        }
        decls.insert(
            name.1.to_string(),
            EnvDecl::Struct(StructDef {
                name: name.1.to_string(),
                fields: set,
            }),
        );
        Ok(())
    }

    fn update<'i>(
        &self,
        name: &Spanned<'i, String>,
        value: ExpressionValue,
    ) -> Result<(), SwiftletError<'i>> {
        let mut decls = self.decls.write().unwrap();
        if let Some(decl) = decls.get(&name.1) {
            match decl {
                EnvDecl::Constant(_) => Err(SwiftletError::runtime(
                    name.0,
                    format!("Cannot redeclare constant '{}'", name.1),
                )),
                EnvDecl::Struct(_) => Err(SwiftletError::runtime(
                    name.0,
                    format!("Cannot redeclare struct '{}'", name.1),
                )),
                EnvDecl::Mutable(_) => {
                    decls.insert(name.1.to_string(), EnvDecl::Mutable(Some(value)));
                    Ok(())
                }
            }
        } else if let Some(parent) = &self.parent {
            parent.update(name, value)
        } else {
            Err(SwiftletError::runtime(
                name.0,
                format!("Identifier '{}' is not defined", name.1),
            ))
        }
    }
}

pub fn run(program: Program) -> Result<(), SwiftletError> {
    let mut env = Environment::new(None);
    program.run(&mut env)
}

trait Interpretable<'i> {
    type Value;
    fn run(&self, env: &mut Environment) -> Result<Self::Value, SwiftletError<'i>>;
}

impl<'i> Interpretable<'i> for Program<'i> {
    type Value = ();
    fn run(&self, env: &mut Environment) -> Result<Self::Value, SwiftletError<'i>> {
        for statement in &self.statements {
            statement.run(env)?;
        }
        Ok(())
    }
}

impl<'i> Interpretable<'i> for Statement<'i> {
    type Value = ();
    fn run(&self, env: &mut Environment) -> Result<Self::Value, SwiftletError<'i>> {
        match self {
            Statement::Assignment(Assignment { lhs, rhs }) => {
                let value = rhs.run(env)?;
                env.update(lhs, value)?;
            }
            Statement::PropertyDeceleration(PropertyDeceleration {
                qualifier,
                name,
                ty,
                initializer,
            }) => {
                if ty.is_some() {
                    todo!()
                }
                let value = initializer.as_ref().map(|i| i.run(env)).transpose()?;
                env.declare_property(qualifier, name, value)?;
            }
            Statement::StructDeceleration(StructDeceleration { name, properties }) => {
                let properties = properties
                    .iter()
                    .map(|prop| {
                        if prop.qualifier != Qualifier::Var {
                            todo!()
                        }
                        if !prop.ty.as_ref().is_some_and(|(_, t)| t == "Int") {
                            todo!()
                        }
                        if prop.initializer.is_some() {
                            todo!()
                        }

                        prop.name.1.clone()
                    })
                    .collect();

                env.declare_struct(name, properties)?;
            }
            Statement::IfStatement(IfStatement {
                condition,
                if_branch,
                else_branch,
            }) => {
                let cond = condition.1.run(env)?;
                if cond.is_true(condition.0)? {
                    if_branch.iter().try_for_each(|s| s.run(env))?;
                } else {
                    else_branch.iter().try_for_each(|s| s.run(env))?;
                }
            }
            Statement::WhileStatement(WhileStatement { condition, body }) => {
                while condition.1.run(env)?.is_true(condition.0)? {
                    body.iter().try_for_each(|s| s.run(env))?;
                }
            }
            Statement::PrintStatement(PrintStatement { expression }) => {
                let value = expression.run(env)?;
                println!("{value}");
            }
            Statement::BlockStatement(BlockStatement { statements }) => {
                let mut env = Environment::new(Some(env.clone()));
                for stmt in statements {
                    stmt.run(&mut env)?;
                }
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for ExpressionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionValue::Number(n) => write!(f, "{n}"),
            ExpressionValue::String(s) => write!(f, "{s}"),
            ExpressionValue::Boolean(b) => write!(f, "{b}"),
            ExpressionValue::Constructor(StructDef { name, fields: _ }) => write!(f, "{name}"),
            ExpressionValue::Struct(StructValue {
                name,
                values: fields,
            }) => {
                let fields = fields
                    .iter()
                    .map(|(name, value)| format!("{name}: {value}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{name} {{ {fields} }}")
            }
        }
    }
}

impl PartialOrd for ExpressionValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (ExpressionValue::Number(l), ExpressionValue::Number(r)) => l.partial_cmp(r),
            (ExpressionValue::String(l), ExpressionValue::String(r)) => l.partial_cmp(r),
            (ExpressionValue::Boolean(l), ExpressionValue::Boolean(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl<'i> Interpretable<'i> for Expression<'i> {
    type Value = ExpressionValue;

    fn run(&self, env: &mut Environment) -> Result<Self::Value, SwiftletError<'i>> {
        match self {
            Expression::Conditional(cond, if_branch, else_branch) => {
                let value = cond.1.run(env)?;
                if value.is_true(cond.0)? {
                    if_branch.run(env)
                } else {
                    else_branch.run(env)
                }
            }
            Expression::Or(left, op, right) => {
                let val = left.run(env)?;
                if val.is_true(op.to_owned())? {
                    Ok(val)
                } else {
                    right.run(env)
                }
            }
            Expression::And(left, op, right) => {
                let val = left.run(env)?;
                if val.is_false(op.to_owned())? {
                    Ok(val)
                } else {
                    right.run(env)
                }
            }
            Expression::Equality(left, op, right) => {
                let left = left.run(env)?;
                let right = right.run(env)?;
                match op.1 {
                    EqualityOperator::Equal => Ok(ExpressionValue::Boolean(left == right)),
                    EqualityOperator::NotEqual => Ok(ExpressionValue::Boolean(left != right)),
                }
            }
            Expression::Comparison(left, op, right) => {
                let left = left.run(env)?;
                let right = right.run(env)?;
                let ord = left.partial_cmp(&right).ok_or(SwiftletError::runtime(
                    op.0,
                    format!("Cannot compare {left} and {right}"),
                ))?;
                match op.1 {
                    ComparisonOperator::LessThan => Ok(ExpressionValue::Boolean(ord.is_lt())),
                    ComparisonOperator::LessThanOrEqual => {
                        Ok(ExpressionValue::Boolean(ord.is_le()))
                    }
                    ComparisonOperator::GreaterThan => Ok(ExpressionValue::Boolean(ord.is_gt())),
                    ComparisonOperator::GreaterThanOrEqual => {
                        Ok(ExpressionValue::Boolean(ord.is_eq()))
                    }
                }
            }
            Expression::Term(left, op, right) => {
                let left = left.run(env)?;
                let right = right.run(env)?;
                match op.1 {
                    TermOperator::Plus => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l + r))
                        }
                        (ExpressionValue::String(l), ExpressionValue::String(r)) => {
                            Ok(ExpressionValue::String(format!("{l}{r}")))
                        }
                        _ => Err(SwiftletError::runtime(
                            op.0,
                            "Can only add two numbers or two strings",
                        )),
                    },
                    TermOperator::Minus => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l - r))
                        }
                        _ => Err(SwiftletError::runtime(op.0, "Can only subtract numbers")),
                    },
                }
            }
            Expression::Factor(left, op, right) => {
                let left = left.run(env)?;
                let right = right.run(env)?;
                match op.1 {
                    FactorOperator::Multiply => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l * r))
                        }
                        _ => Err(SwiftletError::runtime(op.0, "Can only multiply numbers")),
                    },
                    FactorOperator::Divide => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l / r))
                        }
                        _ => Err(SwiftletError::runtime(op.0, "Can only divide numbers")),
                    },
                    FactorOperator::Modulo => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l % r))
                        }
                        _ => Err(SwiftletError::runtime(op.0, "Can only modulo numbers")),
                    },
                }
            }
            Expression::Unary(op, val) => {
                let val = val.run(env)?;
                match op.1 {
                    UnaryOperator::Not => Ok(ExpressionValue::Boolean(val.is_false(op.0)?)),
                    UnaryOperator::Negate => match val {
                        ExpressionValue::Number(n) => Ok(ExpressionValue::Number(-n)),
                        _ => Err(SwiftletError::runtime(op.0, "Can only negate numbers")),
                    },
                }
            }
            Expression::Identifier(id) => env.get(id),
            Expression::IntegerLiteral(v) => Ok(ExpressionValue::Number(*v)),
            Expression::BooleanLiteral(v) => Ok(ExpressionValue::Boolean(*v)),
            Expression::StringLiteral(v) => Ok(ExpressionValue::String(v.clone())),
            Expression::Call(callee_node, arguments) => {
                let callee = callee_node.1.run(env)?;
                let StructDef { name, fields } = callee.as_constructor(callee_node.0)?;

                let mut remaining = fields.clone();

                let values = arguments
                    .iter()
                    .map(|arg| {
                        let value = arg.value.1.run(env)?;
                        match (arg.label.clone(), value) {
                            (Some(label), ExpressionValue::Number(n))
                                if fields.contains(&label.1) =>
                            {
                                if remaining.contains(&label.1) {
                                    remaining.remove(&label.1);
                                    Ok((label.1, n))
                                } else {
                                    Err(SwiftletError::runtime(
                                        label.0,
                                        format!("Duplicate label '{}'", label.1),
                                    ))
                                }
                            }
                            (Some(label), _) => Err(SwiftletError::runtime(
                                arg.value.0,
                                format!("Invalid value for label '{}'", label.1),
                            )),
                            _ => Err(SwiftletError::runtime(
                                callee_node.0,
                                "Expected label and number as argument",
                            )),
                        }
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?;

                if !remaining.is_empty() {
                    return Err(SwiftletError::runtime(
                        callee_node.0,
                        format!(
                            "Missing arguments: {remaining}",
                            remaining = remaining
                                .iter()
                                .map(|s| s.to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    ));
                }

                Ok(ExpressionValue::Struct(StructValue { name, values }))
            }
            Expression::Navigation(target_node, selection) => {
                let target = target_node.1.run(env)?;
                let StructValue { name, values } = target.as_struct(target_node.0)?;

                let value = values.iter().find(|(field, _)| field == &&selection.1);

                if let Some((_, value)) = value {
                    Ok(ExpressionValue::Number(*value))
                } else {
                    Err(SwiftletError::runtime(
                        selection.0,
                        format!("Property '{}' not found in struct '{}'", selection.1, name),
                    ))
                }
            }
        }
    }
}

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
    fn is_true(&self) -> Result<bool, String> {
        match self {
            ExpressionValue::Boolean(b) => Ok(*b),
            _ => Err(format!("Expected boolean, got {self}")),
        }
    }

    fn is_false(&self) -> Result<bool, String> {
        Ok(!self.is_true()?)
    }

    fn as_struct(&self) -> Result<StructValue, String> {
        match self {
            ExpressionValue::Struct(v) => Ok(v.clone()),
            _ => Err(format!("Expected struct, got {self}")),
        }
    }

    fn as_constructor(&self) -> Result<StructDef, String> {
        match self {
            ExpressionValue::Constructor(v) => Ok(v.clone()),
            _ => Err(format!("Expected constructor, got {self}")),
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

    fn get(&self, name: &str) -> Result<ExpressionValue, String> {
        if let Some(decl) = self.decls.read().unwrap().get(name) {
            match decl {
                EnvDecl::Constant(Some(val)) => return Ok(val.clone()),
                EnvDecl::Mutable(Some(val)) => return Ok(val.clone()),
                EnvDecl::Constant(None) | EnvDecl::Mutable(None) => {
                    return Err(format!("Identifier {name} not initialized"))
                }
                EnvDecl::Struct(def) => return Ok(ExpressionValue::Constructor(def.clone())),
            }
        }
        if let Some(parent) = &self.parent {
            return parent.get(name);
        }
        Err(format!("Identifier {name} not found"))
    }

    fn declare_property(
        &mut self,
        qual: &Qualifier,
        name: &str,
        value: Option<ExpressionValue>,
    ) -> Result<(), String> {
        let mut decls = self.decls.write().unwrap();
        if decls.contains_key(name) {
            return Err(format!(
                "Identifier {name} is already declared in the current scope"
            ));
        }
        match qual {
            Qualifier::Var => decls.insert(name.to_string(), EnvDecl::Mutable(value)),
            Qualifier::Let => decls.insert(name.to_string(), EnvDecl::Constant(value)),
        };
        Ok(())
    }

    fn declare_struct(&mut self, name: &str, fields: Vec<String>) -> Result<(), String> {
        let mut decls = self.decls.write().unwrap();
        if decls.contains_key(name) {
            return Err(format!(
                "Identifier {name} is already declared in the current scope"
            ));
        }
        let set: HashSet<String> = fields.iter().cloned().collect();
        if set.len() != fields.len() {
            return Err(format!("Duplicate field names in struct {name}"));
        }
        decls.insert(
            name.to_string(),
            EnvDecl::Struct(StructDef {
                name: name.to_string(),
                fields: set,
            }),
        );
        Ok(())
    }

    fn update(&self, name: &str, value: ExpressionValue) -> Result<(), String> {
        let mut decls = self.decls.write().unwrap();
        if let Some(decl) = decls.get(name) {
            match decl {
                EnvDecl::Constant(_) => Err(format!("Cannot redeclare constant {name}")),
                EnvDecl::Struct(_) => Err(format!("Cannot redeclare struct {name}")),
                EnvDecl::Mutable(_) => {
                    decls.insert(name.to_string(), EnvDecl::Mutable(Some(value)));
                    Ok(())
                }
            }
        } else if let Some(parent) = &self.parent {
            parent.update(name, value)
        } else {
            Err(format!("Identifier {name} is not defined"))
        }
    }
}

pub fn run(program: Program) {
    let mut env = Environment::new(None);
    program.run(&mut env).unwrap();
}

trait Interpretable {
    type Value;
    fn run(&self, env: &mut Environment) -> Result<Self::Value, String>;
}

impl Interpretable for Program {
    type Value = ();
    fn run(&self, env: &mut Environment) -> Result<Self::Value, String> {
        for statement in &self.statements {
            statement.run(env)?;
        }
        Ok(())
    }
}

impl Interpretable for Statement {
    type Value = ();
    fn run(&self, env: &mut Environment) -> Result<Self::Value, String> {
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
                        if prop.ty != Some("Int".to_string()) {
                            todo!()
                        }
                        if prop.initializer.is_some() {
                            todo!()
                        }

                        prop.name.clone()
                    })
                    .collect();

                env.declare_struct(name, properties)?;
            }
            Statement::IfStatement(IfStatement {
                condition,
                if_branch,
                else_branch,
            }) => {
                let cond = condition.run(env)?;
                if cond.is_true()? {
                    if_branch.iter().try_for_each(|s| s.run(env))?;
                } else {
                    else_branch.iter().try_for_each(|s| s.run(env))?;
                }
            }
            Statement::WhileStatement(WhileStatement { condition, body }) => {
                while condition.run(env)?.is_true()? {
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

impl Interpretable for Expression {
    type Value = ExpressionValue;

    fn run(&self, env: &mut Environment) -> Result<Self::Value, String> {
        match self {
            Expression::Conditional(cond, if_branch, else_branch) => {
                let cond = cond.run(env)?;
                if cond.is_true()? {
                    if_branch.run(env)
                } else {
                    else_branch.run(env)
                }
            }
            Expression::Or(left, right) => {
                let val = left.run(env)?;
                if val.is_true()? {
                    Ok(val)
                } else {
                    right.run(env)
                }
            }
            Expression::And(left, right) => {
                let val = left.run(env)?;
                if val.is_false()? {
                    Ok(val)
                } else {
                    right.run(env)
                }
            }
            Expression::Equality(left, op, right) => {
                let left = left.run(env)?;
                let right = right.run(env)?;
                match op {
                    EqualityOperator::Equal => Ok(ExpressionValue::Boolean(left == right)),
                    EqualityOperator::NotEqual => Ok(ExpressionValue::Boolean(left != right)),
                }
            }
            Expression::Comparison(left, op, right) => {
                let left = left.run(env)?;
                let right = right.run(env)?;
                match op {
                    ComparisonOperator::LessThan => Ok(ExpressionValue::Boolean(left < right)),
                    ComparisonOperator::LessThanOrEqual => {
                        Ok(ExpressionValue::Boolean(left <= right))
                    }
                    ComparisonOperator::GreaterThan => Ok(ExpressionValue::Boolean(left > right)),
                    ComparisonOperator::GreaterThanOrEqual => {
                        Ok(ExpressionValue::Boolean(left >= right))
                    }
                }
            }
            Expression::Term(left, op, right) => {
                let left = left.run(env)?;
                let right = right.run(env)?;
                match op {
                    TermOperator::Plus => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l + r))
                        }
                        (ExpressionValue::String(l), ExpressionValue::String(r)) => {
                            Ok(ExpressionValue::String(format!("{l}{r}")))
                        }
                        _ => Err("Can only add two numbers or two strings".to_string()),
                    },
                    TermOperator::Minus => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l - r))
                        }
                        _ => Err("Can only subtract numbers".to_string()),
                    },
                }
            }
            Expression::Factor(left, op, right) => {
                let left = left.run(env)?;
                let right = right.run(env)?;
                match op {
                    FactorOperator::Multiply => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l * r))
                        }
                        _ => Err("Can only multiply numbers".to_string()),
                    },
                    FactorOperator::Divide => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l / r))
                        }
                        _ => Err("Can only divide numbers".to_string()),
                    },
                    FactorOperator::Modulo => match (left, right) {
                        (ExpressionValue::Number(l), ExpressionValue::Number(r)) => {
                            Ok(ExpressionValue::Number(l % r))
                        }
                        _ => Err("Can only modulo numbers".to_string()),
                    },
                }
            }
            Expression::Unary(op, val) => {
                let val = val.run(env)?;
                match op {
                    UnaryOperator::Not => Ok(ExpressionValue::Boolean(val.is_false()?)),
                    UnaryOperator::Negate => match val {
                        ExpressionValue::Number(n) => Ok(ExpressionValue::Number(-n)),
                        _ => Err("Can only negate numbers".to_string()),
                    },
                }
            }
            Expression::Identifier(id) => env.get(id),
            Expression::IntegerLiteral(v) => Ok(ExpressionValue::Number(*v)),
            Expression::BooleanLiteral(v) => Ok(ExpressionValue::Boolean(*v)),
            Expression::StringLiteral(v) => Ok(ExpressionValue::String(v.clone())),
            Expression::Call(callee, arguments) => {
                let callee = callee.run(env)?;
                let StructDef { name, fields } = callee.as_constructor()?;

                let mut remaining = fields.clone();

                let values = arguments
                    .iter()
                    .map(|arg| {
                        let value = arg.value.run(env)?;
                        match (arg.label.clone(), value) {
                            (Some(label), ExpressionValue::Number(n))
                                if fields.contains(&label) =>
                            {
                                if remaining.contains(&label) {
                                    remaining.remove(&label);
                                    Ok((label, n))
                                } else {
                                    Err(format!("Duplicate label {label}"))
                                }
                            }
                            (Some(label), _) => Err(format!("Invalid label {label}")),
                            _ => Err("Expected label and number as argument".to_string()),
                        }
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?;

                if !remaining.is_empty() {
                    return Err(format!(
                        "Missing arguments: {remaining}",
                        remaining = remaining
                            .iter()
                            .map(|s| s.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                }

                Ok(ExpressionValue::Struct(StructValue { name, values }))
            }
            Expression::Navigation(target, selection) => {
                let target = target.run(env)?;
                let StructValue { name, values } = target.as_struct()?;

                let value = values.iter().find(|(field, _)| field == &selection);

                if let Some((_, value)) = value {
                    Ok(ExpressionValue::Number(*value))
                } else {
                    Err(format!("Property {selection} not found in struct {name}"))
                }
            }
        }
    }
}

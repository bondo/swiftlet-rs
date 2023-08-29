use std::{collections::HashMap, rc::Rc, sync::RwLock};

use grammar::*;

#[derive(Debug, PartialEq, Clone)]
enum ExpressionValue {
    Number(i64),
    String(String),
    Boolean(bool),
}

enum EnvDecl {
    Constant(Option<ExpressionValue>),
    Mutable(Option<ExpressionValue>),
}

impl EnvDecl {
    fn val(&self) -> Option<ExpressionValue> {
        match self {
            EnvDecl::Constant(v) => v.clone(),
            EnvDecl::Mutable(v) => v.clone(),
        }
    }
}

#[derive(Clone)]
struct Environment {
    parent: Option<Rc<Environment>>,
    decls: Rc<RwLock<HashMap<String, EnvDecl>>>,
}

impl Environment {
    fn new(parent: Option<Environment>) -> Self {
        Self {
            parent: parent.map(|p| Rc::new(p)),
            decls: Rc::new(RwLock::new(HashMap::new())),
        }
    }

    fn get(&self, name: &str) -> Result<ExpressionValue, String> {
        if let Some(res) = self.decls.read().unwrap().get(name) {
            if let Some(val) = res.val() {
                return Ok(val);
            }
            return Err(format!("Identifier {name} not initialized"));
        }
        if let Some(parent) = &self.parent {
            return parent.get(name);
        }
        return Err(format!("Identifier {name} not found"));
    }

    fn declare(
        &mut self,
        qual: &Qualifier,
        name: &str,
        value: Option<ExpressionValue>,
    ) -> Result<(), String> {
        let mut decl = self.decls.write().unwrap();
        if decl.contains_key(name) {
            return Err(format!(
                "Identifier {name} is already declared in the current scope"
            ));
        }
        match qual {
            Qualifier::Var => decl.insert(name.to_string(), EnvDecl::Mutable(value)),
            Qualifier::Let => decl.insert(name.to_string(), EnvDecl::Constant(value)),
        };
        Ok(())
    }

    fn update(&self, name: &str, value: ExpressionValue) -> Result<(), String> {
        let mut decls = self.decls.write().unwrap();
        if let Some(decl) = decls.get(name) {
            match decl {
                EnvDecl::Constant(_) => Err(format!("Cannot redeclare constant {name}")),
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
                env.declare(qualifier, name, value)?;
            }
            Statement::IfStatement(IfStatement {
                condition,
                if_branch,
                else_branch,
            }) => {
                let cond = condition.run(env)?;
                if cond.is_truthy() {
                    if_branch.into_iter().try_for_each(|s| s.run(env))?;
                } else {
                    else_branch.into_iter().try_for_each(|s| s.run(env))?;
                }
            }
            Statement::WhileStatement(WhileStatement { condition, body }) => {
                while condition.run(env)?.is_truthy() {
                    body.into_iter().try_for_each(|s| s.run(env))?;
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
            Statement::StructDeceleration(_) => todo!(),
        }
        Ok(())
    }
}

impl ExpressionValue {
    fn is_truthy(&self) -> bool {
        match self {
            ExpressionValue::Number(n) => *n != 0,
            ExpressionValue::String(s) => !s.is_empty(),
            ExpressionValue::Boolean(b) => *b,
        }
    }
}

impl std::fmt::Display for ExpressionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionValue::Number(n) => write!(f, "{}", n),
            ExpressionValue::String(s) => write!(f, "{}", s),
            ExpressionValue::Boolean(b) => write!(f, "{}", b),
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
                if cond.is_truthy() {
                    if_branch.run(env)
                } else {
                    else_branch.run(env)
                }
            }
            Expression::Or(left, right) => {
                let val = left.run(env)?;
                if val.is_truthy() {
                    Ok(val)
                } else {
                    right.run(env)
                }
            }
            Expression::And(left, right) => {
                let val = left.run(env)?;
                if !val.is_truthy() {
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
                    UnaryOperator::Not => Ok(ExpressionValue::Boolean(!val.is_truthy())),
                    UnaryOperator::Negate => match val {
                        ExpressionValue::Number(n) => Ok(ExpressionValue::Number(-n)),
                        _ => Err("Can only negate numbers".to_string()),
                    },
                }
            }
            Expression::Identifier(id) => env.get(id),
            Expression::IntegerLiteral(v) => Ok(ExpressionValue::Number(*v as i64)),
            Expression::BooleanLiteral(v) => Ok(ExpressionValue::Boolean(*v)),
            Expression::StringLiteral(v) => Ok(ExpressionValue::String(v.clone())),
            Expression::Invocation(_, _) => todo!(),
            Expression::PropertyAccess(_, _) => todo!(),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn it_works() {
//         let result = add(2, 2);
//         assert_eq!(result, 4);
//     }
// }

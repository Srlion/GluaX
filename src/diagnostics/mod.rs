use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    add_diagnostic,
    lexer::{self, Ident, Token},
    parser::{self, Block, Expression, Func, Variable},
    Diagnostic, DiagnosticSeverity, Span,
};

pub struct Diagnostics {
    ast: Block,
    main_scope: ScopeRef,
}

pub struct Scope {
    pub variables: HashMap<String, Variable>,
    pub functions: Vec<Func>,
    pub parent: Option<ScopeRef>,
    pub types: Vec<Ident>,
}

type ScopeRef = Rc<RefCell<Scope>>;

fn new_type(name: &str) -> Ident {
    Ident {
        name: name.to_string(),
        span: Span::default(),
    }
}

fn is_primitive_type(name: &str) -> bool {
    matches!(name, "nil" | "bool" | "number" | "string")
}

impl Diagnostics {
    pub fn new(ast: Block) -> Self {
        let main_scope = Scope {
            variables: HashMap::new(),
            functions: Vec::new(),
            parent: None,
            types: Vec::new(),
        };

        Self {
            ast,
            main_scope: Rc::new(RefCell::new(main_scope)),
        }
    }

    pub fn diagnose(&mut self) {
        self.diagnose_block(self.main_scope.clone(), &self.ast.clone());
        println!("{:#?}", self.main_scope.borrow().variables);
    }

    fn diagnose_block(&mut self, scope: ScopeRef, block: &parser::Block) {
        for statement in &block.statements {
            match statement {
                parser::Statement::Let(var) => {
                    self.diagnose_variable_declaration(scope.clone(), var)
                }
                _ => todo!(),
            }
        }
    }

    fn diagnose_variable_declaration(&mut self, scope: ScopeRef, var: &parser::Variable) {
        let mut var_type = var.type_.clone();

        let initializer = &var.initializer;
        let initializer_type = self.get_expression_type(scope.clone(), initializer);

        if let Some(ref declared_type) = var_type {
            if !self.type_exists(scope.clone(), &declared_type.name) {
                add_diagnostic(Diagnostic {
                    span: declared_type.span.clone(),
                    severity: DiagnosticSeverity::Error,
                    message: format!("Type '{}' not found", declared_type.name),
                });
            }
            if !self.type_matches(scope.clone(), declared_type, &initializer_type) {
                add_diagnostic(Diagnostic {
                    span: initializer.span().clone(),
                    severity: DiagnosticSeverity::Error,
                    message: format!(
                        "Type mismatch for variable '{}': expected '{}', got '{}'",
                        var.name, declared_type.name, initializer_type.name
                    ),
                });
            }
        } else {
            var_type = Some(initializer_type);
        }

        let var_to_add = if let Some(var_type) = var_type {
            Variable {
                name: var.name.clone(),
                span: var.span.clone(),
                type_: Some(var_type),
                initializer: var.initializer.clone(),
            }
        } else {
            Variable {
                name: var.name.clone(),
                span: var.span.clone(),
                type_: None,
                initializer: var.initializer.clone(),
            }
        };

        scope
            .borrow_mut()
            .variables
            .insert(var.name.name.clone(), var_to_add);
    }

    fn get_expression_type(&self, scope: ScopeRef, expr: &parser::Expression) -> Ident {
        match expr {
            Expression::Nil(_) => new_type("nil"),
            Expression::Bool(_, _) => new_type("bool"),
            Expression::Number(_) => new_type("number"),
            Expression::StringLit(_) => new_type("string"),
            Expression::Var(name_token) => {
                let var_name = name_token.name.clone();
                self.lookup_variable_type(scope, &var_name)
                    .unwrap_or_else(|| {
                        add_diagnostic(Diagnostic {
                            span: name_token.span.clone(),
                            severity: DiagnosticSeverity::Error,
                            message: format!("Variable '{}' not found in scope", var_name),
                        });
                        new_type("unknown")
                    })
            }
            _ => todo!("Type inference not implemented for this expression"),
        }
    }

    fn lookup_variable_type(&self, scope: ScopeRef, name: &str) -> Option<Ident> {
        fn lookup(scope: ScopeRef, name: &str) -> Option<Ident> {
            let scope_ref = scope.borrow();
            if let Some(var) = scope_ref.variables.get(name) {
                var.type_.clone()
            } else if let Some(parent) = &scope_ref.parent {
                lookup(parent.clone(), name)
            } else {
                None
            }
        }
        lookup(scope, name)
    }

    fn type_exists(&self, scope: ScopeRef, name: &str) -> bool {
        if is_primitive_type(name) {
            return true;
        }
        let scope_ref = scope.borrow();
        for ident in &scope_ref.types {
            if ident.name == name {
                return true;
            }
        }
        false
    }

    fn type_matches(&self, scope: ScopeRef, left: &Ident, right: &Ident) -> bool {
        if is_primitive_type(&left.name) || is_primitive_type(&right.name) {
            return left.name == right.name;
        }
        if left.name == "any" {
            return true;
        }

        // we need to find out how to make sure types with same name from different sources match or not
        todo!("");
        if left.name == right.name {
            return true;
        }
        false
    }
}

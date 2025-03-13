use serde::{Deserialize, Serialize};
use std::{cell::RefCell, sync::Mutex};

use lexer::*;
use parser::Parser;

// mod diagnostics;
mod lexer;
mod parser;

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
    Hint,
    HardError,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub span: Span,
    pub severity: DiagnosticSeverity,
    pub message: String,
}

pub static CURRENT_FILE_DIAGNOSTICS: Mutex<Vec<Diagnostic>> = Mutex::new(Vec::new());

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct Span {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
}

impl Span {
    pub const fn default() -> Self {
        Span {
            line_start: 0,
            line_end: 0,
            column_start: 0,
            column_end: 0,
        }
    }

    pub fn from(start_span: Span, end_span: Span) -> Self {
        Span {
            line_start: start_span.line_start,
            line_end: end_span.line_end,
            column_start: start_span.column_start,
            column_end: end_span.column_end,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Spanned { inner, span }
    }
}

pub fn add_diagnostic(diagnostic: Diagnostic) {
    let mut diagnostics = CURRENT_FILE_DIAGNOSTICS.lock().unwrap();
    diagnostics.push(diagnostic);
}

fn main() {
    let code = include_str!("test.gluax");
    let start = std::time::Instant::now();

    let mut lexer = Lexer::new(code);

    // let start = std::time::Instant::now();

    let mut parser = Parser::new(code);
    let statements = parser.parse();
    println!("{:#?}", statements);
    // let mut compiler = diagnostics::Diagnostics::new(statements);
    // compiler.diagnose();

    // println!("Took {:?} to diagnose", start.elapsed());

    let mut diagnostics = CURRENT_FILE_DIAGNOSTICS.lock().unwrap();
    println!(
        "{}",
        serde_json::to_string_pretty(&diagnostics.clone()).unwrap()
    );
}

pub fn hard_error(msg: &str, span: Span) -> ! {
    let diagnostic = Diagnostic {
        span,
        severity: DiagnosticSeverity::HardError,
        message: msg.to_string(),
    };
    let output_error = serde_json::to_string(&diagnostic).unwrap();
    println!("{}", output_error);
    std::process::exit(0);
}

fn escape_string<I>(input: I) -> String
where
    I: std::fmt::Debug,
{
    let input = format!("{:?}", input);
    println!("{input}");
    let mut escaped = String::new();

    for ch in input.chars() {
        match ch {
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            '\r' => escaped.push_str("\\r"),
            '\x08' => escaped.push_str("\\b"), // Backspace
            '\x0C' => escaped.push_str("\\f"), // Form feed
            _ => escaped.push(ch),
        }
    }

    escaped
}

#[macro_export]
macro_rules! format_escaped {
    ($fmt:expr, $($args:expr),*) => {
        format!($fmt, $($crate::escape_string(&$args)),*)
    };
}

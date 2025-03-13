use crate::{
    format_escaped,
    lexer::{
        keyword::{self, Keyword},
        punct::{self, Punct},
        Ident, Lexer, Number, StringLit, Token,
    },
    Span,
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    peeked: Option<Token>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Ident,
    pub type_: Ident,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Ident,
    pub parameters: Vec<Parameter>,
    pub block: Block,
    pub vararg: Option<Span>, // if Some, it's a vararg
    pub returns: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub block: Block,
    pub else_if: Vec<(Expression, Block)>,
    pub else_: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expression,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub exps: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub name: Ident,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Func(Func),
    Let(Variable),
    Assignment(Assignment),
    If(If),
    Loop(Loop),
    While(While),
    Block(Block),
    Return(Return),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Nil(Span),
    Bool(bool, Span),
    Number(Number),
    StringLit(StringLit),
    Var(Ident),
    Vararg(Span),
    Binary(Box<Expression>, Span, Box<Expression>),
    FuncCall(FuncCall),
    Not(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Ident,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Ident,
    pub initializer: Expression,
    pub span: Span,
    pub type_: Option<Ident>,
}

#[derive(Debug)]
enum Associativity {
    Left,
    Right,
}

// fn get_operator_precedence(token: &Token) -> Option<(u32, Associativity)> {
//     match &token {
//         Token::Punct(Punct::Expo(_)) => Some((8, Associativity::Right)),
//         Token::Punct(Punct::Asterisk)
//         | Token::Punct(Punct::Slash)
//         | Token::Punct(Punct::Percent) => Some((7, Associativity::Left)),
//         Token::Punct(Punct::Plus) | Token::Punct(Punct::Minus) => Some((6, Associativity::Left)),
//         Token::Punct(Punct::Concat) => Some((5, Associativity::Right)),
//         Token::Punct(Punct::Less)
//         | Token::Punct(Punct::LessEqual)
//         | Token::Punct(Punct::Greater)
//         | Token::Punct(Punct::GreaterEqual)
//         | Token::Punct(Punct::EqualEqual)
//         | Token::Punct(Punct::NotEqual) => Some((4, Associativity::Left)),
//         Token::Punct(Punct::Equal) | Token::Punct(Punct::Expo) => Some((3, Associativity::Left)),
//         Token::Punct(Punct::And) | Token::Punct(Punct::Or) => Some((2, Associativity::Left)),
//         _ => None,
//     }
// }

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            lexer: Lexer::new(code),
            current: Token::default(),
            peeked: None,
        }
    }

    fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.lexer.next_token());
        }
        self.peeked.as_ref().unwrap()
    }

    fn advance(&mut self) -> &Token {
        if let Some(token) = self.peeked.take() {
            self.current = token;
        } else {
            self.current = self.lexer.next_token();
        }
        &self.current
    }

    pub fn parse(&mut self) -> Block {
        let start_span = self.current.span();
        self.advance();
        let mut statements = Vec::new();

        while !self.peek().is_eof() {
            statements.push(self.parse_statement());
        }

        Block {
            statements,
            span: Span::from(start_span, self.current.span()),
        }
    }

    fn consume_keyword(&mut self, keyword: Keyword, err_msg: &str) {
        let token = self.current.clone();
        match token {
            Token::Keyword(ref ref_keyword) => {
                if keyword != *ref_keyword {
                    crate::hard_error(err_msg, token.span());
                }
                self.advance(); // consume keyword
            }
            _ => crate::hard_error(err_msg, token.span()),
        }
    }

    fn consume_punct(&mut self, punct: Punct, err_msg: &str) -> Punct {
        let token = self.current.clone();
        match token {
            Token::Punct(ref ref_punct) => {
                if punct != *ref_punct {
                    crate::hard_error(err_msg, token.span());
                }
                self.advance(); // consume punct
                ref_punct.clone()
            }
            _ => crate::hard_error(err_msg, token.span()),
        }
    }

    fn consume_ident_msg(&mut self, msg: &str) -> Ident {
        let token = self.current.clone();
        match token {
            Token::Ident(ref ref_ident) => {
                self.advance(); // consume ident
                ref_ident.clone()
            }
            other => crate::hard_error(
                &format_escaped!("{msg}, found {:?}", other),
                self.current.span(),
            ),
        }
    }

    fn consume_ident(&mut self) -> Ident {
        self.consume_ident_msg("Expected identifier")
    }

    fn match_punct(&mut self, punct: Punct) -> bool {
        match &self.current {
            Token::Punct(ref ref_punct) => {
                if punct != *ref_punct {
                    return false;
                }
                self.advance(); // consume punct
                true
            }
            _ => false,
        }
    }

    fn is_punct(&mut self, punct: Punct) -> bool {
        match &self.current {
            Token::Punct(ref ref_punct) => punct == *ref_punct,
            _ => false,
        }
    }

    fn is_next_punct(&mut self, punct: Punct) -> bool {
        match &self.peek() {
            Token::Punct(ref ref_punct) => punct == *ref_punct,
            _ => false,
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> bool {
        match &self.current {
            Token::Keyword(ref ref_keyword) => {
                if keyword != *ref_keyword {
                    crate::hard_error("Unexpected keyword", self.current.span());
                }
                self.advance(); // consume keyword
                true
            }
            _ => false,
        }
    }

    fn parse_statement(&mut self) -> Statement {
        match &self.current {
            Token::Keyword(Keyword::Let(_)) => Statement::Let(self.parse_let_stmt()),
            Token::Keyword(Keyword::Func(_)) => Statement::Func(self.parse_function()),
            Token::Keyword(Keyword::If(_)) => Statement::If(self.parse_if_stmt()),
            Token::Keyword(Keyword::While(_)) => Statement::While(self.parse_while()),
            Token::Keyword(Keyword::Loop(_)) => Statement::Loop(self.parse_loop()),
            Token::Keyword(Keyword::Return(_)) => Statement::Return(self.parse_return()),
            Token::Punct(Punct::OpenBrace(_)) => Statement::Block(self.parse_block()),
            Token::Ident(_) => Statement::Assignment(self.parse_assignment()),
            Token::Eof(_) => {
                crate::hard_error("Unexpected end of file", self.current.span());
            }
            _ => {
                todo!("other statements")
            }
        }
    }

    fn parse_parameters(&mut self) -> (Vec<Parameter>, Option<Span>) {
        let mut params = Vec::new();
        if self.is_punct(punct::CLOSEPAREN) {
            return (params, None);
        }
        loop {
            match &self.current {
                Token::Punct(Punct::Vararg(_)) => {
                    let vararg = self.advance();
                    return (params, Some(vararg.span().clone()));
                }
                _ => {
                    let name = self.consume_ident();
                    self.consume_punct(punct::COLON, "Expected ':' after parameter name");
                    let type_ = self.consume_ident_msg("Expected parameter type");
                    params.push(Parameter { name, type_ });
                }
            }
            if self.match_punct(punct::COMMA) {
                continue;
            }
            break;
        }
        (params, None)
    }

    fn parse_function(&mut self) -> Func {
        let start_span = self.current.span();
        self.advance(); // consume 'func'

        let name = self.consume_ident();
        self.consume_punct(punct::OPENPAREN, "Expected '(' after function name");

        let (parameters, vararg) = self.parse_parameters();

        self.consume_punct(punct::CLOSEPAREN, "Expected ')' after parameters");

        let mut returns = Vec::new();

        if self.match_punct(punct::OPENPAREN) {
            if self.current.is_ident() {
                returns.push(self.consume_ident());
            }

            while self.match_punct(punct::COMMA) {
                returns.push(self.consume_ident_msg("Expected return type"));
            }

            println!("{:?}", self.current);
            self.consume_punct(punct::CLOSEPAREN, "Expected ')' after return types");
        } else if self.current.is_ident() {
            returns.push(self.consume_ident());
        }

        let block = self.parse_block();
        let end_span = block.span.clone();
        Func {
            name,
            parameters,
            block,
            vararg,
            returns,
            span: Span::from(start_span, end_span),
        }
    }

    fn parse_let_stmt(&mut self) -> Variable {
        let start_span = self.current.span();
        self.advance(); // consume 'let'

        let name = self.consume_ident();

        let type_ = if self.match_punct(punct::COLON) {
            Some(self.consume_ident_msg("Expected type"))
        } else {
            None
        };

        self.consume_punct(punct::EQUAL, "Expected '=' after variable name");

        let initializer = self.parse_expression();

        let end_span = self
            .consume_punct(punct::SEMICOLON, "Expected ';' after variable declaration")
            .span();

        Variable {
            name,
            span: Span {
                line_start: start_span.line_start,
                line_end: end_span.line_end,
                column_start: start_span.column_start,
                column_end: end_span.column_end,
            },
            type_,
            initializer,
        }
    }

    fn parse_assignment(&mut self) -> Assignment {
        let start_span = self.current.span();
        let name = self.consume_ident();
        self.consume_punct(punct::EQUAL, "Expected '=' after variable name");
        let value = self.parse_expression();
        self.consume_punct(punct::SEMICOLON, "Expected ';' after assignment");
        let end_span = self.current.span();
        Assignment {
            name,
            value,
            span: Span {
                line_start: start_span.line_start,
                line_end: end_span.line_end,
                column_start: start_span.column_start,
                column_end: end_span.column_end,
            },
        }
    }

    fn parse_if_stmt(&mut self) -> If {
        let start_span = self.current.span();
        self.advance(); // consume 'if'
        let condition = self.parse_expression();
        let body = self.parse_block();

        let mut else_if = Vec::new();
        let mut else_ = None;

        let mut end_span;
        loop {
            end_span = self.current.span();
            if !self.match_keyword(keyword::ELSE) {
                break;
            }

            if self.match_keyword(keyword::IF) {
                let condition = self.parse_expression();
                let body = self.parse_block();
                else_if.push((condition, body));
            } else {
                let body = self.parse_block();
                else_ = Some(body);
            }
        }

        If {
            condition,
            block: body,
            else_if,
            else_,
            span: Span::from(start_span, end_span),
        }
    }

    fn parse_while(&mut self) -> While {
        let start_span = self.current.span();
        self.advance(); // consume 'while'
        let condition = self.parse_expression();
        let block = self.parse_block();
        let end_span = block.span.clone();
        While {
            condition,
            block,
            span: Span::from(start_span, end_span),
        }
    }

    fn parse_loop(&mut self) -> Loop {
        let start_span = self.current.span();
        self.advance(); // consume 'loop'
        let block = self.parse_block();
        let end_span = block.span.clone();
        Loop {
            block,
            span: Span::from(start_span, end_span),
        }
    }

    fn parse_return(&mut self) -> Return {
        let start_span = self.current.span();
        self.advance(); // consume 'return'
        let exps = self.parse_expression_list();
        let end_span = self
            .consume_punct(punct::SEMICOLON, "Expected ';' after return")
            .span();
        Return {
            exps,
            span: Span::from(start_span, end_span),
        }
    }

    fn parse_expression_list(&mut self) -> Vec<Expression> {
        let mut expressions = Vec::new();
        expressions.push(self.parse_expression());
        while self.match_punct(punct::COMMA) {
            expressions.push(self.parse_expression());
        }
        expressions
    }

    fn parse_function_call(&mut self) -> Expression {
        let name = self.consume_ident();
        if self.match_punct(punct::CLOSEPAREN) {
            return Expression::FuncCall(FuncCall {
                name,
                args: Vec::new(),
            });
        }
        let mut args = Vec::new();
        args.push(self.parse_expression());
        while self.match_punct(punct::COMMA) {
            args.push(self.parse_expression());
        }
        self.consume_punct(punct::CLOSEPAREN, "Expected ')' after function arguments");
        Expression::FuncCall(FuncCall { name, args })
    }

    fn parse_identifier_or_function_call(&mut self) -> Expression {
        if self.is_next_punct(punct::OPENPAREN) {
            return self.parse_function_call();
        }
        let ident = self.consume_ident();
        Expression::Var(ident)
    }

    // fn parse_primary_expression(&mut self) -> Expression {
    //     match self.peek().kind {
    //         TokenKind::Keyword(Keyword::Nil) => Expression::Nil(self.advance().clone()),
    //         TokenKind::Keyword(Keyword::True) => Expression::Bool(self.advance().clone()),
    //         TokenKind::Keyword(Keyword::False) => Expression::Bool(self.advance().clone()),
    //         TokenKind::Number(_) => Expression::Number(self.advance().clone()),
    //         TokenKind::StringLiteral(_) => Expression::String(self.advance().clone()),
    //         TokenKind::Identifier(_) => self.parse_identifier_or_function_call(),
    //         _ => crate::hard_error("Expected expression", self.peek().span.clone()),
    //     }
    // }

    fn parse_expression(&mut self) -> Expression {
        match self.current.clone() {
            Token::Keyword(Keyword::Nil(span)) => {
                self.advance(); // consume 'nil'
                Expression::Nil(span)
            }
            Token::Keyword(Keyword::True(span)) => {
                self.advance(); // consume 'true'
                Expression::Bool(true, span)
            }
            Token::Keyword(Keyword::False(span)) => {
                self.advance(); // consume 'false'
                Expression::Bool(false, span)
            }
            Token::Number(number) => {
                self.advance(); // consume number
                Expression::Number(number.clone())
            }
            Token::StringLiteral(string) => {
                self.advance(); // consume string
                Expression::StringLit(string.clone())
            }
            Token::Ident(_) => self.parse_identifier_or_function_call(),
            Token::Punct(Punct::Exclamation(_)) => {
                self.advance(); // consume '!'
                let exp = self.parse_expression();
                Expression::Not(Box::new(exp))
            }
            _ => crate::hard_error("Expected expression", self.current.span().clone()),
        }
    }

    fn parse_block(&mut self) -> Block {
        let start_span = self.current.span();
        self.advance(); // consume '{'
        let mut statements = Vec::new();
        let mut end_span;
        loop {
            end_span = self.current.span();
            if self.match_punct(punct::CLOSEBRACE) {
                break;
            }
            statements.push(self.parse_statement());
        }
        Block {
            statements,
            span: Span::from(start_span, end_span),
        }
    }
}

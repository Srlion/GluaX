use crate::Span;

const fn const_str_eq(a: &str, b: &str) -> bool {
    let a_bytes = a.as_bytes();
    let b_bytes = b.as_bytes();
    if a_bytes.len() != b_bytes.len() {
        return false;
    }
    let mut i = 0;
    while i < a_bytes.len() {
        if a_bytes[i] != b_bytes[i] {
            return false;
        }
        i += 1;
    }
    true
}

macro_rules! punct {
    ( $( $name:ident => $disp:expr ),* $(,)? ) => {
        #[derive(Debug, Clone)]
        pub enum Punct {
            $( $name(Span), )*
        }

        impl std::fmt::Display for Punct {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                // basically prints "Punct(supplied_disp)" for each variant
                match self { $( Punct::$name(_) => write!(f, "{}", $disp), )* }
            }
        }

        impl Punct {
            pub fn new(s: &str, span: Span) -> Self {
                $(
                    if s == $disp {
                        return Punct::$name(span);
                    }
                )*
                panic!("Unexpected punctuation '{}'", s)
            }

            pub const fn const_new(s: &'static str, span: Span) -> Self {
                $(
                    if const_str_eq(s, $disp) {
                        return Punct::$name(span);
                    }
                )*
                panic!("Unexpected punctuation")
            }

            pub fn span(&self) -> Span {
                match self {
                    $( Punct::$name(span) => span.clone(), )*
                }
            }
        }

        impl PartialEq for Punct {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $( (Punct::$name(_), Punct::$name(_)) => true, )*
                    _ => false,
                }
            }
        }

        $(
            paste::item! {
                #[allow(unused)]
                pub const [<$name:upper>]: Punct = Punct::$name(Span::default());
            }
        )*
    }
}

punct! {
    Plus => "+",
    Minus => "-",
    Asterisk => "*",
    Slash => "/",
    Percent => "%",
    EqualEqual => "==",
    NotEqual => "!=",
    Less => "<",
    LessEqual => "<=",
    Greater => ">",
    GreaterEqual => ">=",
    Equal => "=",
    Expo => "^",
    Concat => "..",
    Length => "#",
    Exclamation => "!",
    And => "&&",
    Or => "||",
    Vararg => "...",
    Semicolon => ";",
    Colon => ":",
    Comma => ",",
    OpenParen => "(",
    CloseParen => ")",
    OpenBrace => "{",
    CloseBrace => "}",
    OpenBracket => "[",
    CloseBracket => "]",
    Dot => ".",
}

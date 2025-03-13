use crate::Span;

macro_rules! keywords {
    ( $( $name:ident ),* $(,)? ) => {
        #[derive(Debug, Clone)]
        pub enum Keyword {
            $( $name(Span), )*
        }

        impl Keyword {
            pub fn new(s: &str, span: Span) -> Option<Self> {
                $(
                if s == stringify!($name).to_ascii_lowercase() {
                        return Some(Keyword::$name(span));
                    }
                )*
                None
            }

            pub fn span(&self) -> Span {
                match self {
                    $( Keyword::$name(span) => span.clone(), )*
                }
            }
        }

        impl std::fmt::Display for Keyword {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( Keyword::$name(_) => write!(f, "Keyword({})", stringify!($name)), )*
                }
            }
        }

        impl PartialEq for Keyword {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $(
                        (Keyword::$name(_), Keyword::$name(_)) => true,
                    )*
                    _ => false,
                }
            }
        }

        $(
            paste::item! {
                // Creates an associated constant with an uppercase name (e.g., AND, BREAK, etc.)
                pub const [<$name:upper>]: Keyword = Keyword::$name(Span::default());
            }
        )*
    }
}

keywords! {
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Func,
    If,
    In,
    Let,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
    Continue,
    Loop,
}

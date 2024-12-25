use macros_core::ebnf;
use std::io::{Cursor, Read};
use nom::character::complete::char;
use nom::IResult as NomResult;
use nom::combinator::{map, opt};
use nom::branch::alt;
use nom::multi::{many1, many0};
use nom::sequence::tuple;
pub struct Literal {
    value: String,
}
#[automatically_derived]
impl ::core::fmt::Debug for Literal {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Literal",
            "value",
            &&self.value,
        )
    }
}
impl From<char> for Literal {
    fn from(value: char) -> Self {
        Literal {
            value: value.to_string(),
        }
    }
}
impl Literal {
    fn parse_lit<'a>(lit: char) -> impl FnMut(&'a str) -> NomResult<&'a str, Literal> {
        map(char(lit), |t| Literal { value: t.to_string() })
    }
}
pub struct Optional {
    value: Box<Option<Token>>,
}
#[automatically_derived]
impl ::core::fmt::Debug for Optional {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Optional",
            "value",
            &&self.value,
        )
    }
}
impl From<Option<Token>> for Optional {
    fn from(value: Option<Token>) -> Self {
        Optional { value: Box::new(value) }
    }
}
pub struct Repetition {
    value: Vec<Token>,
}
#[automatically_derived]
impl ::core::fmt::Debug for Repetition {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Repetition",
            "value",
            &&self.value,
        )
    }
}
impl From<Vec<Token>> for Repetition {
    fn from(value: Vec<Token>) -> Self {
        Repetition { value }
    }
}
pub struct Alternatives {
    value: Box<Token>,
}
#[automatically_derived]
impl ::core::fmt::Debug for Alternatives {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Alternatives",
            "value",
            &&self.value,
        )
    }
}
impl Alternatives {
    fn parse<'a, F: 'a>(
        alts: F,
    ) -> impl FnMut(&'a str) -> NomResult<&'a str, Alternatives>
    where
        F: FnMut(&'a str) -> NomResult<&'a str, Token>,
    {
        map(
            alts,
            |ts| Alternatives {
                value: Box::new(ts),
            },
        )
    }
}
pub struct Sequence {
    value: Vec<Token>,
}
#[automatically_derived]
impl ::core::fmt::Debug for Sequence {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Sequence",
            "value",
            &&self.value,
        )
    }
}
pub enum Token {
    Digit(Digit),
    Digits(Digits),
    Plusminus(Plusminus),
    Integer(Integer),
    Literal(Literal),
    Optional(Optional),
    Repetition(Repetition),
    Alternatives(Alternatives),
    Sequence(Sequence),
}
#[automatically_derived]
impl ::core::fmt::Debug for Token {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match self {
            Token::Digit(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Digit", &__self_0)
            }
            Token::Digits(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Digits", &__self_0)
            }
            Token::Plusminus(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Plusminus",
                    &__self_0,
                )
            }
            Token::Integer(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Integer",
                    &__self_0,
                )
            }
            Token::Literal(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Literal",
                    &__self_0,
                )
            }
            Token::Optional(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Optional",
                    &__self_0,
                )
            }
            Token::Repetition(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Repetition",
                    &__self_0,
                )
            }
            Token::Alternatives(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Alternatives",
                    &__self_0,
                )
            }
            Token::Sequence(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Sequence",
                    &__self_0,
                )
            }
        }
    }
}
impl Token {
    pub fn value<T>(&self) -> T {
        match self {
            _ => ::core::panicking::panic("not implemented"),
        }
    }
}
pub struct Digit {
    value: Literal,
}
#[automatically_derived]
impl ::core::fmt::Debug for Digit {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Digit",
            "value",
            &&self.value,
        )
    }
}
impl Digit {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Digit>::parse, Token::Digit)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Digit> {
        map(Literal::parse_lit('0'), |l| Digit { value: l })(input)
    }
}
pub struct Digits {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Digits {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Digits",
            "value",
            &&self.value,
        )
    }
}
impl Digits {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Digits>::parse, Token::Digits)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Digits> {
        map(
            Alternatives::parse(
                alt((
                    map(Literal::parse_lit('0'), Token::Literal),
                    map(Literal::parse_lit('1'), Token::Literal),
                )),
            ),
            |v| Digits { value: v },
        )(input)
    }
}
pub struct Plusminus {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Plusminus {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Plusminus",
            "value",
            &&self.value,
        )
    }
}
impl Plusminus {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Plusminus>::parse, Token::Plusminus)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Plusminus> {
        map(
            Alternatives::parse(
                alt((
                    map(Literal::parse_lit('+'), Token::Literal),
                    map(Literal::parse_lit('-'), Token::Literal),
                )),
            ),
            |v| Plusminus { value: v },
        )(input)
    }
}
pub struct Integer {
    value: Sequence,
}
#[automatically_derived]
impl ::core::fmt::Debug for Integer {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Integer",
            "value",
            &&self.value,
        )
    }
}
impl Integer {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Integer>::parse, Token::Integer)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Integer> {
        map(
            map(
                tuple((
                    map(
                        map(
                            opt(<Plusminus>::parse_token),
                            |t| Optional { value: Box::new(t) },
                        ),
                        Token::Optional,
                    ),
                    <Digits>::parse_token,
                    map(
                        map(many0(<Digits>::parse_token), |ts| Repetition { value: ts }),
                        Token::Repetition,
                    ),
                )),
                |ts| {
                    let mut v = Vec::with_capacity(3usize);
                    v.push(ts.0);
                    v.push(ts.1);
                    v.push(ts.2);
                    Sequence { value: v }
                },
            ),
            |v| Integer { value: v },
        )(input)
    }
}
pub fn main() {}

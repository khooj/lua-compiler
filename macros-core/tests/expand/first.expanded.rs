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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Literal {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Literal {
    #[inline]
    fn eq(&self, other: &Literal) -> bool {
        self.value == other.value
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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Optional {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Optional {
    #[inline]
    fn eq(&self, other: &Optional) -> bool {
        self.value == other.value
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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Repetition {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Repetition {
    #[inline]
    fn eq(&self, other: &Repetition) -> bool {
        self.value == other.value
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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Alternatives {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Alternatives {
    #[inline]
    fn eq(&self, other: &Alternatives) -> bool {
        self.value == other.value
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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Sequence {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Sequence {
    #[inline]
    fn eq(&self, other: &Sequence) -> bool {
        self.value == other.value
    }
}
pub struct Disjunction {
    value: Box<Token>,
    dis: Box<Token>,
}
#[automatically_derived]
impl ::core::fmt::Debug for Disjunction {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field2_finish(
            f,
            "Disjunction",
            "value",
            &self.value,
            "dis",
            &&self.dis,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Disjunction {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Disjunction {
    #[inline]
    fn eq(&self, other: &Disjunction) -> bool {
        self.value == other.value && self.dis == other.dis
    }
}
pub enum Token {
    Digit(Digit),
    Digits(Digits),
    Plusminus(Plusminus),
    Integer(Integer),
    Dis1(Dis1),
    Dis2(Dis2),
    Literal(Literal),
    Optional(Optional),
    Repetition(Repetition),
    Alternatives(Alternatives),
    Sequence(Sequence),
    Disjunction(Disjunction),
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
            Token::Dis1(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Dis1", &__self_0)
            }
            Token::Dis2(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Dis2", &__self_0)
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
            Token::Disjunction(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Disjunction",
                    &__self_0,
                )
            }
        }
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Token {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Token {
    #[inline]
    fn eq(&self, other: &Token) -> bool {
        let __self_tag = ::core::intrinsics::discriminant_value(self);
        let __arg1_tag = ::core::intrinsics::discriminant_value(other);
        __self_tag == __arg1_tag
            && match (self, other) {
                (Token::Digit(__self_0), Token::Digit(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Digits(__self_0), Token::Digits(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Plusminus(__self_0), Token::Plusminus(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Integer(__self_0), Token::Integer(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Dis1(__self_0), Token::Dis1(__arg1_0)) => *__self_0 == *__arg1_0,
                (Token::Dis2(__self_0), Token::Dis2(__arg1_0)) => *__self_0 == *__arg1_0,
                (Token::Literal(__self_0), Token::Literal(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Optional(__self_0), Token::Optional(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Repetition(__self_0), Token::Repetition(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Alternatives(__self_0), Token::Alternatives(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Sequence(__self_0), Token::Sequence(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Disjunction(__self_0), Token::Disjunction(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                _ => unsafe { ::core::intrinsics::unreachable() }
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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Digit {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Digit {
    #[inline]
    fn eq(&self, other: &Digit) -> bool {
        self.value == other.value
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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Digits {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Digits {
    #[inline]
    fn eq(&self, other: &Digits) -> bool {
        self.value == other.value
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
                    alt((
                        map(Literal::parse_lit('0'), Token::Literal),
                        map(Literal::parse_lit('1'), Token::Literal),
                    )),
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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Plusminus {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Plusminus {
    #[inline]
    fn eq(&self, other: &Plusminus) -> bool {
        self.value == other.value
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
                    alt((
                        map(Literal::parse_lit('+'), Token::Literal),
                        map(Literal::parse_lit('-'), Token::Literal),
                    )),
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
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Integer {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Integer {
    #[inline]
    fn eq(&self, other: &Integer) -> bool {
        self.value == other.value
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
                        map(many1(<Digits>::parse_token), |ts| Repetition { value: ts }),
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
pub struct Dis1 {
    value: Sequence,
}
#[automatically_derived]
impl ::core::fmt::Debug for Dis1 {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Dis1",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Dis1 {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Dis1 {
    #[inline]
    fn eq(&self, other: &Dis1) -> bool {
        self.value == other.value
    }
}
impl Dis1 {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Dis1>::parse, Token::Dis1)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Dis1> {
        map(
            map(
                tuple((
                    map(Literal::parse_lit('\''), Token::Literal),
                    map(
                        map(
                            many1(
                                map(
                                    map(
                                        <Digits>::parse_token,
                                        |v| Disjunction {
                                            value: Box::new(v),
                                            dis: Box::new(
                                                Token::Literal(Literal {
                                                    value: "test".to_string(),
                                                }),
                                            ),
                                        },
                                    ),
                                    Token::Disjunction,
                                ),
                            ),
                            |ts| Repetition { value: ts },
                        ),
                        Token::Repetition,
                    ),
                    map(Literal::parse_lit('\''), Token::Literal),
                )),
                |ts| {
                    let mut v = Vec::with_capacity(3usize);
                    v.push(ts.0);
                    v.push(ts.1);
                    v.push(ts.2);
                    Sequence { value: v }
                },
            ),
            |v| Dis1 { value: v },
        )(input)
    }
}
pub struct Dis2 {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Dis2 {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Dis2",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Dis2 {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Dis2 {
    #[inline]
    fn eq(&self, other: &Dis2) -> bool {
        self.value == other.value
    }
}
impl Dis2 {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Dis2>::parse, Token::Dis2)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Dis2> {
        map(
            Alternatives::parse(
                alt((
                    alt((
                        map(
                            map(
                                tuple((
                                    map(Literal::parse_lit('A'), Token::Literal),
                                    map(
                                        map(
                                            <Digits>::parse_token,
                                            |v| Disjunction {
                                                value: Box::new(v),
                                                dis: Box::new(
                                                    Token::Literal(Literal {
                                                        value: "test".to_string(),
                                                    }),
                                                ),
                                            },
                                        ),
                                        Token::Disjunction,
                                    ),
                                    map(
                                        map(
                                            many1(
                                                map(
                                                    map(
                                                        <Digits>::parse_token,
                                                        |v| Disjunction {
                                                            value: Box::new(v),
                                                            dis: Box::new(
                                                                Token::Literal(Literal {
                                                                    value: "test".to_string(),
                                                                }),
                                                            ),
                                                        },
                                                    ),
                                                    Token::Disjunction,
                                                ),
                                            ),
                                            |ts| Repetition { value: ts },
                                        ),
                                        Token::Repetition,
                                    ),
                                    map(Literal::parse_lit('B'), Token::Literal),
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(4usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    v.push(ts.3);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                        map(
                            map(
                                tuple((
                                    map(Literal::parse_lit('C'), Token::Literal),
                                    map(
                                        map(
                                            <Digits>::parse_token,
                                            |v| Disjunction {
                                                value: Box::new(v),
                                                dis: Box::new(
                                                    Token::Literal(Literal {
                                                        value: "test".to_string(),
                                                    }),
                                                ),
                                            },
                                        ),
                                        Token::Disjunction,
                                    ),
                                    map(
                                        map(
                                            many1(
                                                map(
                                                    map(
                                                        <Digits>::parse_token,
                                                        |v| Disjunction {
                                                            value: Box::new(v),
                                                            dis: Box::new(
                                                                Token::Literal(Literal {
                                                                    value: "test".to_string(),
                                                                }),
                                                            ),
                                                        },
                                                    ),
                                                    Token::Disjunction,
                                                ),
                                            ),
                                            |ts| Repetition { value: ts },
                                        ),
                                        Token::Repetition,
                                    ),
                                    map(Literal::parse_lit('D'), Token::Literal),
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(4usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    v.push(ts.3);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                    )),
                )),
            ),
            |v| Dis2 { value: v },
        )(input)
    }
}
pub fn main() {}

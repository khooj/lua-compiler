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
pub enum Token {
    Letter(Letter),
    Digit(Digit),
    Symbol(Symbol),
    Character(Character),
    Identifier(Identifier),
    S(S),
    Terminal(Terminal),
    Terminator(Terminator),
    Term(Term),
    Factor(Factor),
    Concatenation(Concatenation),
    Alternation(Alternation),
    Rhs(Rhs),
    Lhs(Lhs),
    Rule(Rule),
    Grammar(Grammar),
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
            Token::Letter(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Letter", &__self_0)
            }
            Token::Digit(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Digit", &__self_0)
            }
            Token::Symbol(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Symbol", &__self_0)
            }
            Token::Character(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Character",
                    &__self_0,
                )
            }
            Token::Identifier(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Identifier",
                    &__self_0,
                )
            }
            Token::S(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "S", &__self_0)
            }
            Token::Terminal(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Terminal",
                    &__self_0,
                )
            }
            Token::Terminator(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Terminator",
                    &__self_0,
                )
            }
            Token::Term(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Term", &__self_0)
            }
            Token::Factor(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Factor", &__self_0)
            }
            Token::Concatenation(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Concatenation",
                    &__self_0,
                )
            }
            Token::Alternation(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Alternation",
                    &__self_0,
                )
            }
            Token::Rhs(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Rhs", &__self_0)
            }
            Token::Lhs(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Lhs", &__self_0)
            }
            Token::Rule(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Rule", &__self_0)
            }
            Token::Grammar(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Grammar",
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
                (Token::Letter(__self_0), Token::Letter(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Digit(__self_0), Token::Digit(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Symbol(__self_0), Token::Symbol(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Character(__self_0), Token::Character(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Identifier(__self_0), Token::Identifier(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::S(__self_0), Token::S(__arg1_0)) => *__self_0 == *__arg1_0,
                (Token::Terminal(__self_0), Token::Terminal(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Terminator(__self_0), Token::Terminator(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Term(__self_0), Token::Term(__arg1_0)) => *__self_0 == *__arg1_0,
                (Token::Factor(__self_0), Token::Factor(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Concatenation(__self_0), Token::Concatenation(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Alternation(__self_0), Token::Alternation(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (Token::Rhs(__self_0), Token::Rhs(__arg1_0)) => *__self_0 == *__arg1_0,
                (Token::Lhs(__self_0), Token::Lhs(__arg1_0)) => *__self_0 == *__arg1_0,
                (Token::Rule(__self_0), Token::Rule(__arg1_0)) => *__self_0 == *__arg1_0,
                (Token::Grammar(__self_0), Token::Grammar(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
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
pub struct Letter {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Letter {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Letter",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Letter {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Letter {
    #[inline]
    fn eq(&self, other: &Letter) -> bool {
        self.value == other.value
    }
}
impl Letter {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Letter>::parse, Token::Letter)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Letter> {
        map(
            Alternatives::parse(
                alt((
                    alt((
                        map(Literal::parse_lit('A'), Token::Literal),
                        map(Literal::parse_lit('B'), Token::Literal),
                        map(Literal::parse_lit('C'), Token::Literal),
                        map(Literal::parse_lit('D'), Token::Literal),
                        map(Literal::parse_lit('E'), Token::Literal),
                        map(Literal::parse_lit('F'), Token::Literal),
                        map(Literal::parse_lit('G'), Token::Literal),
                        map(Literal::parse_lit('H'), Token::Literal),
                        map(Literal::parse_lit('I'), Token::Literal),
                        map(Literal::parse_lit('J'), Token::Literal),
                        map(Literal::parse_lit('K'), Token::Literal),
                        map(Literal::parse_lit('L'), Token::Literal),
                        map(Literal::parse_lit('M'), Token::Literal),
                        map(Literal::parse_lit('N'), Token::Literal),
                        map(Literal::parse_lit('O'), Token::Literal),
                        map(Literal::parse_lit('P'), Token::Literal),
                        map(Literal::parse_lit('Q'), Token::Literal),
                        map(Literal::parse_lit('R'), Token::Literal),
                        map(Literal::parse_lit('S'), Token::Literal),
                        map(Literal::parse_lit('T'), Token::Literal),
                        map(Literal::parse_lit('U'), Token::Literal),
                    )),
                    alt((
                        map(Literal::parse_lit('V'), Token::Literal),
                        map(Literal::parse_lit('W'), Token::Literal),
                        map(Literal::parse_lit('X'), Token::Literal),
                        map(Literal::parse_lit('Y'), Token::Literal),
                        map(Literal::parse_lit('Z'), Token::Literal),
                        map(Literal::parse_lit('a'), Token::Literal),
                        map(Literal::parse_lit('b'), Token::Literal),
                        map(Literal::parse_lit('c'), Token::Literal),
                        map(Literal::parse_lit('d'), Token::Literal),
                        map(Literal::parse_lit('e'), Token::Literal),
                        map(Literal::parse_lit('f'), Token::Literal),
                        map(Literal::parse_lit('g'), Token::Literal),
                        map(Literal::parse_lit('h'), Token::Literal),
                        map(Literal::parse_lit('i'), Token::Literal),
                        map(Literal::parse_lit('j'), Token::Literal),
                        map(Literal::parse_lit('k'), Token::Literal),
                        map(Literal::parse_lit('l'), Token::Literal),
                        map(Literal::parse_lit('m'), Token::Literal),
                        map(Literal::parse_lit('n'), Token::Literal),
                        map(Literal::parse_lit('o'), Token::Literal),
                        map(Literal::parse_lit('p'), Token::Literal),
                    )),
                    alt((
                        map(Literal::parse_lit('q'), Token::Literal),
                        map(Literal::parse_lit('r'), Token::Literal),
                        map(Literal::parse_lit('s'), Token::Literal),
                        map(Literal::parse_lit('t'), Token::Literal),
                        map(Literal::parse_lit('u'), Token::Literal),
                        map(Literal::parse_lit('v'), Token::Literal),
                        map(Literal::parse_lit('w'), Token::Literal),
                        map(Literal::parse_lit('x'), Token::Literal),
                        map(Literal::parse_lit('y'), Token::Literal),
                        map(Literal::parse_lit('z'), Token::Literal),
                    )),
                )),
            ),
            |v| Letter { value: v },
        )(input)
    }
}
pub struct Digit {
    value: Alternatives,
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
        map(
            Alternatives::parse(
                alt((
                    alt((
                        map(Literal::parse_lit('0'), Token::Literal),
                        map(Literal::parse_lit('1'), Token::Literal),
                        map(Literal::parse_lit('2'), Token::Literal),
                        map(Literal::parse_lit('3'), Token::Literal),
                        map(Literal::parse_lit('4'), Token::Literal),
                        map(Literal::parse_lit('5'), Token::Literal),
                        map(Literal::parse_lit('6'), Token::Literal),
                        map(Literal::parse_lit('7'), Token::Literal),
                        map(Literal::parse_lit('8'), Token::Literal),
                        map(Literal::parse_lit('9'), Token::Literal),
                    )),
                )),
            ),
            |v| Digit { value: v },
        )(input)
    }
}
pub struct Symbol {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Symbol {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Symbol",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Symbol {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Symbol {
    #[inline]
    fn eq(&self, other: &Symbol) -> bool {
        self.value == other.value
    }
}
impl Symbol {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Symbol>::parse, Token::Symbol)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Symbol> {
        map(
            Alternatives::parse(
                alt((
                    alt((
                        map(Literal::parse_lit('['), Token::Literal),
                        map(Literal::parse_lit(']'), Token::Literal),
                        map(Literal::parse_lit('{'), Token::Literal),
                        map(Literal::parse_lit('}'), Token::Literal),
                        map(Literal::parse_lit('('), Token::Literal),
                        map(Literal::parse_lit(')'), Token::Literal),
                        map(Literal::parse_lit('<'), Token::Literal),
                        map(Literal::parse_lit('>'), Token::Literal),
                        map(Literal::parse_lit('"'), Token::Literal),
                        map(Literal::parse_lit('\''), Token::Literal),
                        map(Literal::parse_lit('='), Token::Literal),
                        map(Literal::parse_lit('|'), Token::Literal),
                        map(Literal::parse_lit('.'), Token::Literal),
                        map(Literal::parse_lit(','), Token::Literal),
                        map(Literal::parse_lit(';'), Token::Literal),
                        map(Literal::parse_lit('-'), Token::Literal),
                        map(Literal::parse_lit('+'), Token::Literal),
                        map(Literal::parse_lit('*'), Token::Literal),
                        map(Literal::parse_lit('?'), Token::Literal),
                        map(Literal::parse_lit('\n'), Token::Literal),
                        map(Literal::parse_lit('\t'), Token::Literal),
                    )),
                    alt((map(Literal::parse_lit('\r'), Token::Literal),)),
                )),
            ),
            |v| Symbol { value: v },
        )(input)
    }
}
pub struct Character {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Character {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Character",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Character {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Character {
    #[inline]
    fn eq(&self, other: &Character) -> bool {
        self.value == other.value
    }
}
impl Character {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Character>::parse, Token::Character)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Character> {
        map(
            Alternatives::parse(
                alt((
                    alt((
                        <Letter>::parse_token,
                        <Digit>::parse_token,
                        <Symbol>::parse_token,
                        map(Literal::parse_lit('_'), Token::Literal),
                        map(Literal::parse_lit(' '), Token::Literal),
                    )),
                )),
            ),
            |v| Character { value: v },
        )(input)
    }
}
pub struct Identifier {
    value: Sequence,
}
#[automatically_derived]
impl ::core::fmt::Debug for Identifier {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Identifier",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Identifier {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Identifier {
    #[inline]
    fn eq(&self, other: &Identifier) -> bool {
        self.value == other.value
    }
}
impl Identifier {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Identifier>::parse, Token::Identifier)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Identifier> {
        map(
            map(
                tuple((
                    <Letter>::parse_token,
                    map(
                        map(
                            many0(
                                map(
                                    Alternatives::parse(
                                        alt((
                                            alt((
                                                <Letter>::parse_token,
                                                <Digit>::parse_token,
                                                map(Literal::parse_lit('_'), Token::Literal),
                                            )),
                                        )),
                                    ),
                                    Token::Alternatives,
                                ),
                            ),
                            |ts| Repetition { value: ts },
                        ),
                        Token::Repetition,
                    ),
                )),
                |ts| {
                    let mut v = Vec::with_capacity(2usize);
                    v.push(ts.0);
                    v.push(ts.1);
                    Sequence { value: v }
                },
            ),
            |v| Identifier { value: v },
        )(input)
    }
}
pub struct S {
    value: Repetition,
}
#[automatically_derived]
impl ::core::fmt::Debug for S {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(f, "S", "value", &&self.value)
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for S {}
#[automatically_derived]
impl ::core::cmp::PartialEq for S {
    #[inline]
    fn eq(&self, other: &S) -> bool {
        self.value == other.value
    }
}
impl S {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<S>::parse, Token::S)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, S> {
        map(
            map(
                many0(
                    map(
                        Alternatives::parse(
                            alt((
                                alt((
                                    map(Literal::parse_lit(' '), Token::Literal),
                                    map(Literal::parse_lit('\n'), Token::Literal),
                                    map(Literal::parse_lit('\t'), Token::Literal),
                                    map(Literal::parse_lit('\r'), Token::Literal),
                                )),
                            )),
                        ),
                        Token::Alternatives,
                    ),
                ),
                |ts| Repetition { value: ts },
            ),
            |v| S { value: v },
        )(input)
    }
}
pub struct Terminal {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Terminal {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Terminal",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Terminal {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Terminal {
    #[inline]
    fn eq(&self, other: &Terminal) -> bool {
        self.value == other.value
    }
}
impl Terminal {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Terminal>::parse, Token::Terminal)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Terminal> {
        map(
            Alternatives::parse(
                alt((
                    alt((
                        map(
                            map(
                                tuple((
                                    map(Literal::parse_lit('\''), Token::Literal),
                                    <Character>::parse_token,
                                    map(
                                        map(
                                            many0(<Character>::parse_token),
                                            |ts| Repetition { value: ts },
                                        ),
                                        Token::Repetition,
                                    ),
                                    map(Literal::parse_lit('\''), Token::Literal),
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
                                    map(Literal::parse_lit('"'), Token::Literal),
                                    <Character>::parse_token,
                                    map(
                                        map(
                                            many0(<Character>::parse_token),
                                            |ts| Repetition { value: ts },
                                        ),
                                        Token::Repetition,
                                    ),
                                    map(Literal::parse_lit('"'), Token::Literal),
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
            |v| Terminal { value: v },
        )(input)
    }
}
pub struct Terminator {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Terminator {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Terminator",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Terminator {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Terminator {
    #[inline]
    fn eq(&self, other: &Terminator) -> bool {
        self.value == other.value
    }
}
impl Terminator {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Terminator>::parse, Token::Terminator)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Terminator> {
        map(
            Alternatives::parse(
                alt((
                    alt((
                        map(Literal::parse_lit(';'), Token::Literal),
                        map(Literal::parse_lit('.'), Token::Literal),
                    )),
                )),
            ),
            |v| Terminator { value: v },
        )(input)
    }
}
pub struct Term {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Term {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Term",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Term {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Term {
    #[inline]
    fn eq(&self, other: &Term) -> bool {
        self.value == other.value
    }
}
impl Term {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Term>::parse, Token::Term)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Term> {
        map(
            Alternatives::parse(
                alt((
                    alt((
                        map(
                            map(
                                tuple((
                                    map(Literal::parse_lit('('), Token::Literal),
                                    <S>::parse_token,
                                    <Rhs>::parse_token,
                                    <S>::parse_token,
                                    map(Literal::parse_lit(')'), Token::Literal),
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(5usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    v.push(ts.3);
                                    v.push(ts.4);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                        map(
                            map(
                                tuple((
                                    map(Literal::parse_lit('['), Token::Literal),
                                    <S>::parse_token,
                                    <Rhs>::parse_token,
                                    <S>::parse_token,
                                    map(Literal::parse_lit(']'), Token::Literal),
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(5usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    v.push(ts.3);
                                    v.push(ts.4);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                        map(
                            map(
                                tuple((
                                    map(Literal::parse_lit('{'), Token::Literal),
                                    <S>::parse_token,
                                    <Rhs>::parse_token,
                                    <S>::parse_token,
                                    map(Literal::parse_lit('}'), Token::Literal),
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(5usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    v.push(ts.3);
                                    v.push(ts.4);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                        <Terminal>::parse_token,
                        <Identifier>::parse_token,
                    )),
                )),
            ),
            |v| Term { value: v },
        )(input)
    }
}
pub struct Factor {
    value: Alternatives,
}
#[automatically_derived]
impl ::core::fmt::Debug for Factor {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Factor",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Factor {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Factor {
    #[inline]
    fn eq(&self, other: &Factor) -> bool {
        self.value == other.value
    }
}
impl Factor {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Factor>::parse, Token::Factor)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Factor> {
        map(
            Alternatives::parse(
                alt((
                    alt((
                        map(
                            map(
                                tuple((
                                    <Term>::parse_token,
                                    <S>::parse_token,
                                    map(Literal::parse_lit('?'), Token::Literal),
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(3usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                        map(
                            map(
                                tuple((
                                    <Term>::parse_token,
                                    <S>::parse_token,
                                    map(Literal::parse_lit('*'), Token::Literal),
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(3usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                        map(
                            map(
                                tuple((
                                    <Term>::parse_token,
                                    <S>::parse_token,
                                    map(Literal::parse_lit('+'), Token::Literal),
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(3usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                        map(
                            map(
                                tuple((
                                    <Term>::parse_token,
                                    <S>::parse_token,
                                    map(Literal::parse_lit('-'), Token::Literal),
                                    <S>::parse_token,
                                    <Term>::parse_token,
                                )),
                                |ts| {
                                    let mut v = Vec::with_capacity(5usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    v.push(ts.2);
                                    v.push(ts.3);
                                    v.push(ts.4);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                        map(
                            map(
                                tuple((<Term>::parse_token, <S>::parse_token)),
                                |ts| {
                                    let mut v = Vec::with_capacity(2usize);
                                    v.push(ts.0);
                                    v.push(ts.1);
                                    Sequence { value: v }
                                },
                            ),
                            Token::Sequence,
                        ),
                    )),
                )),
            ),
            |v| Factor { value: v },
        )(input)
    }
}
pub struct Concatenation {
    value: Repetition,
}
#[automatically_derived]
impl ::core::fmt::Debug for Concatenation {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Concatenation",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Concatenation {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Concatenation {
    #[inline]
    fn eq(&self, other: &Concatenation) -> bool {
        self.value == other.value
    }
}
impl Concatenation {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Concatenation>::parse, Token::Concatenation)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Concatenation> {
        map(
            map(
                many1(
                    map(
                        map(
                            tuple((
                                <S>::parse_token,
                                <Factor>::parse_token,
                                <S>::parse_token,
                                map(
                                    map(
                                        opt(map(Literal::parse_lit(','), Token::Literal)),
                                        |t| Optional { value: Box::new(t) },
                                    ),
                                    Token::Optional,
                                ),
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
                ),
                |ts| Repetition { value: ts },
            ),
            |v| Concatenation { value: v },
        )(input)
    }
}
pub struct Alternation {
    value: Repetition,
}
#[automatically_derived]
impl ::core::fmt::Debug for Alternation {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Alternation",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Alternation {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Alternation {
    #[inline]
    fn eq(&self, other: &Alternation) -> bool {
        self.value == other.value
    }
}
impl Alternation {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Alternation>::parse, Token::Alternation)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Alternation> {
        map(
            map(
                many1(
                    map(
                        map(
                            tuple((
                                <S>::parse_token,
                                <Concatenation>::parse_token,
                                <S>::parse_token,
                                map(
                                    map(
                                        opt(map(Literal::parse_lit('|'), Token::Literal)),
                                        |t| Optional { value: Box::new(t) },
                                    ),
                                    Token::Optional,
                                ),
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
                ),
                |ts| Repetition { value: ts },
            ),
            |v| Alternation { value: v },
        )(input)
    }
}
pub struct Rhs {
    value: Alternation,
}
#[automatically_derived]
impl ::core::fmt::Debug for Rhs {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Rhs",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Rhs {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Rhs {
    #[inline]
    fn eq(&self, other: &Rhs) -> bool {
        self.value == other.value
    }
}
impl Rhs {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Rhs>::parse, Token::Rhs)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Rhs> {
        map(<Alternation>::parse, |rn| Rhs { value: rn })(input)
    }
}
pub struct Lhs {
    value: Identifier,
}
#[automatically_derived]
impl ::core::fmt::Debug for Lhs {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Lhs",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Lhs {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Lhs {
    #[inline]
    fn eq(&self, other: &Lhs) -> bool {
        self.value == other.value
    }
}
impl Lhs {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Lhs>::parse, Token::Lhs)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Lhs> {
        map(<Identifier>::parse, |rn| Lhs { value: rn })(input)
    }
}
pub struct Rule {
    value: Sequence,
}
#[automatically_derived]
impl ::core::fmt::Debug for Rule {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Rule",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Rule {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Rule {
    #[inline]
    fn eq(&self, other: &Rule) -> bool {
        self.value == other.value
    }
}
impl Rule {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Rule>::parse, Token::Rule)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Rule> {
        map(
            map(
                tuple((
                    <Lhs>::parse_token,
                    <S>::parse_token,
                    map(Literal::parse_lit('='), Token::Literal),
                    <S>::parse_token,
                    <Rhs>::parse_token,
                    <S>::parse_token,
                    <Terminator>::parse_token,
                )),
                |ts| {
                    let mut v = Vec::with_capacity(7usize);
                    v.push(ts.0);
                    v.push(ts.1);
                    v.push(ts.2);
                    v.push(ts.3);
                    v.push(ts.4);
                    v.push(ts.5);
                    v.push(ts.6);
                    Sequence { value: v }
                },
            ),
            |v| Rule { value: v },
        )(input)
    }
}
pub struct Grammar {
    value: Repetition,
}
#[automatically_derived]
impl ::core::fmt::Debug for Grammar {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Grammar",
            "value",
            &&self.value,
        )
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Grammar {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Grammar {
    #[inline]
    fn eq(&self, other: &Grammar) -> bool {
        self.value == other.value
    }
}
impl Grammar {
    pub fn parse_token(input: &str) -> NomResult<&str, Token> {
        map(<Grammar>::parse, Token::Grammar)(input)
    }
    pub fn parse(input: &str) -> NomResult<&str, Grammar> {
        map(
            map(
                many0(
                    map(
                        map(
                            tuple((
                                <S>::parse_token,
                                <Rule>::parse_token,
                                <S>::parse_token,
                            )),
                            |ts| {
                                let mut v = Vec::with_capacity(3usize);
                                v.push(ts.0);
                                v.push(ts.1);
                                v.push(ts.2);
                                Sequence { value: v }
                            },
                        ),
                        Token::Sequence,
                    ),
                ),
                |ts| Repetition { value: ts },
            ),
            |v| Grammar { value: v },
        )(input)
    }
}
pub fn main() {}

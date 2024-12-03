use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while_m_n},
    character::{
        complete::{
            alpha1, alphanumeric0, alphanumeric1, char, digit1, hex_digit1, line_ending,
            multispace0, newline, not_line_ending, one_of,
        },
        is_alphabetic,
    },
    combinator::{cut, map, map_res, not, opt},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::{count, length_count, many0, many0_count, many1, many_m_n, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use super::literal::{self, Identifier};
use super::expressions::{self, PrefixExp, prefix_exp, Expression, expression};

#[derive(Debug)]
pub(crate) enum Variable {
    Identifier(Identifier),
    TableAccess(TableAccess),
}

fn table_access<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Variable, E> {
    let (i, pe) = context("table_access_prefixexp", prefix_exp)(i)?;
    let (i, s) = context("table_access", alt((
        tuple((char('['), expression, char(']'))),
        preceded(char('.'), literal::identifier),
    )))(i)?;
}

pub(crate) fn identifier<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Variable, E> {
    let (i, s) = context("variable_ident", literal::identifier)(i)?;
    Ok((i, Variable::Identifier(s)))
}

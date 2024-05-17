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
    sequence::{delimited, preceded, tuple, terminated},
    IResult,
};

#[derive(Debug)]
pub(crate) struct Expression;

pub(crate) fn expression<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Expression, E> {
    Ok((i, Expression))
}

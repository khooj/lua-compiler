use std::error::Error;

use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::quote;
use syn::parse::{discouraged::Speculative, Parse, ParseStream, Peek};
use syn::punctuated::Punctuated;
use syn::{
    braced, bracketed, parse_macro_input, token, Error as SynError, Ident, LitChar, LitStr, Result,
    Token,
};

mod kw {
    use syn::custom_keyword;

    custom_keyword!(ebnf_sep);
}

mod punct {
    use syn::custom_punctuation;

    custom_punctuation!(OrSep, |);
    custom_punctuation!(RuleDef, ::=);
}

pub struct Terminal {
    pub value: char,
}

impl Parse for Terminal {
    fn parse(input: ParseStream) -> Result<Self> {
        let l: LitStr = input.parse().map_err(fast_error(input.span()))?;
        if l.value().len() != 1 {
            return Err(SynError::new_spanned(
                l,
                format!("terminal should be 1 character"),
            ));
        }
        Ok(Terminal {
            value: l.value().chars().nth(0).unwrap(),
        })
    }
}

pub struct RuleName {
    pub name: String,
}
impl Parse for RuleName {
    fn parse(input: ParseStream) -> Result<Self> {
        let rn: Ident = input.parse().map_err(fast_error(input.span()))?;
        Ok(RuleName {
            name: rn.to_string(),
        })
    }
}

impl RuleName {
    pub fn ident(&self, span: Span) -> Ident {
        Ident::new(&self.name.to_case(Case::UpperCamel), span)
    }
}

pub struct Alternatives {
    pub choices: Punctuated<Rhs, punct::OrSep>,
}

impl Parse for Alternatives {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut segments = Punctuated::new();
        let first = parse_until_strict_end(input, punct::OrSep).map_err(|e| {
            SynError::new(
                input.span(),
                format!("unexpected token while parsing alternation: {}", e),
            )
        })?;
        segments.push_value(syn::parse2(first).map_err(fast_error(input.span()))?);

        while input.peek(punct::OrSep) {
            segments.push_punct(input.parse().map_err(fast_error(input.span()))?);
            let second =
                parse_until_optional_end(input, punct::OrSep).map_err(fast_error(input.span()))?;
            segments.push_value(syn::parse2(second).map_err(fast_error(input.span()))?);
        }

        Ok(Alternatives { choices: segments })
    }
}

pub struct Sequence {
    pub seq: Punctuated<Rhs, Token![,]>,
}

impl Parse for Sequence {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut segments = Punctuated::new();
        let first = parse_until_strict_end(input, Token![,]).map_err(|e| {
            SynError::new(
                input.span(),
                format!("unexpected token while parsing sequence: {}", e),
            )
        })?;
        let value = syn::parse2(first).map_err(|e| {
            SynError::new(
                input.span(),
                format!("unexpected value while parsing sequence: {}", e),
            )
        })?;
        segments.push_value(value);

        while input.peek(Token![,]) {
            segments.push_punct(input.parse().map_err(fast_error(input.span()))?);
            let second =
                parse_until_optional_end(input, Token![,]).map_err(fast_error(input.span()))?;
            segments.push_value(syn::parse2(second).map_err(fast_error(input.span()))?);
        }

        Ok(Sequence { seq: segments })
    }
}

pub struct Optional {
    pub optional: Box<Rhs>,
}

impl Parse for Optional {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let p: Rhs = content.parse().map_err(fast_error(input.span()))?;
        Ok(Optional {
            optional: Box::new(p),
        })
    }
}

pub enum RepeatType {
    ZeroOrMore,
    OneOrMore,
}

pub struct Repetition {
    pub rep: Box<Rhs>,
    pub repeat: RepeatType,
}

impl Parse for Repetition {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        braced!(content in input);
        let p: Rhs = content.parse().map_err(fast_error(input.span()))?;

        let repeat = if input.peek(Token![*]) {
            let _: Token![*] = input.parse()?;
            RepeatType::ZeroOrMore
        } else if input.peek(Token![+]) {
            let _: Token![+] = input.parse()?;
            RepeatType::OneOrMore
        } else {
            panic!("unknown seq char");
        };

        Ok(Repetition {
            rep: Box::new(p),
            repeat,
        })
    }
}

pub struct Disjunction {
    pub value: Box<Rhs>,
    pub dis: Box<Rhs>,
}

fn fast_error<E: Error>(span: Span) -> impl Fn(E) -> SynError {
    move |_| SynError::new(span, "fast error")
}

impl Parse for Disjunction {
    fn parse(input: ParseStream) -> Result<Self> {
        let tokens = parse_until_strict_end(input, Token![-]).map_err(|e| {
            SynError::new(
                input.span(),
                format!("unexpected token while parsing disjunction: {}", e),
            )
        })?;
        let value = syn::parse2(tokens).map_err(|e| {
            SynError::new(
                input.span(),
                format!(
                    "unexpected error while parsing tokens in disjunction: {}",
                    e
                ),
            )
        })?;
        let _: Token![-] = input.parse().map_err(fast_error(input.span()))?;
        //let tokens = parse_until_optional_end(input, Token![-])?;
        let dis = input.parse().map_err(|e| {
            SynError::new(
                input.span(),
                format!("unexpected token while parsing dis in disjunction: {}", e),
            )
        })?;

        Ok(Disjunction {
            value: Box::new(value),
            dis: Box::new(dis),
        })
    }
}

pub enum Rhs {
    Terminal(Terminal),
    Rule(RuleName),
    Alternatives(Alternatives),
    Sequence(Sequence),
    Optional(Optional),
    Repetition(Repetition),
    Disjunction(Disjunction),
}

impl Parse for Rhs {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        let fork = input.fork();
        if let Ok(alt) = fork.parse() {
            input.advance_to(&fork);
            return Ok(Rhs::Alternatives(alt));
        }

        let fork = input.fork();
        if let Ok(seq) = fork.parse() {
            input.advance_to(&fork);
            return Ok(Rhs::Sequence(seq));
        }

        if input.peek2(Token![-]) {
            return Ok(Rhs::Disjunction(
                input.parse().map_err(fast_error(input.span()))?,
            ));
        }

        if lookahead.peek(token::Brace) {
            let rep: Repetition = input.parse().map_err(fast_error(input.span()))?;
            return Ok(Rhs::Repetition(rep));
        }

        if lookahead.peek(token::Bracket) {
            let opt: Optional = input.parse().map_err(fast_error(input.span()))?;
            return Ok(Rhs::Optional(opt));
        }

        if lookahead.peek(Ident) {
            let rn: RuleName = input.parse().map_err(fast_error(input.span()))?;
            return Ok(Rhs::Rule(rn));
        }

        Ok(Rhs::Terminal(
            input.parse().map_err(fast_error(input.span()))?,
        ))
    }
}

pub struct Rule {
    pub name: Ident,
    pub rhs: Rhs,
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self> {
        let first = parse_until_strict_end(input, Token![=])?;
        let name: Ident = syn::parse2(first)?;
        let _: Token![=] = input.parse()?;
        let ts = parse_until_strict_end(input, Token![;])?;
        let rhs: Rhs = syn::parse2(ts)?;

        Ok(Rule { name, rhs })
    }
}

impl Rule {
    pub fn ident(&self) -> Ident {
        Ident::new(
            &self.name.to_string().to_case(Case::UpperCamel),
            self.name.span(),
        )
    }
}

fn parse_until_optional_end<E: Peek>(input: ParseStream, end: E) -> Result<TokenStream> {
    let mut tokens = TokenStream::new();
    while !input.is_empty() && !input.peek(end) {
        let next: TokenTree = input.parse()?;
        tokens.extend(Some(next));
    }
    Ok(tokens)
}

fn parse_until_strict_end<E: Peek>(input: ParseStream, end: E) -> Result<TokenStream> {
    let fork = input.fork();
    while !fork.is_empty() {
        if fork.peek(end) {
            return parse_until_optional_end(input, end);
        }
        let _: TokenTree = fork.parse().map_err(fast_error(input.span()))?;
    }

    let tt: TokenTree = input.parse().map_err(fast_error(input.span()))?;

    Err(syn::Error::new_spanned(
        tt,
        "expected tokens to contain sequence or alternation",
    ))
}

pub struct EbnfInput {
    pub rules: Vec<Rule>,
}

impl Parse for EbnfInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut rules = vec![];
        while !input.is_empty() {
            rules.push(input.parse::<Rule>()?);
            let end: TokenTree = input.parse()?;
            if end.to_string() != ";" {
                return Err(syn::Error::new_spanned(
                    end.clone(),
                    format!("wrong rules separator {}", end),
                ));
            }
        }

        Ok(EbnfInput { rules })
    }
}

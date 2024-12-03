use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use syn::parse::{Parse, ParseStream, Peek};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Expr, Ident, Result, Token};

mod kw {
    use syn::custom_keyword;

    custom_keyword!(ebnf_sep);
}

struct EbnfSep {
    kw_token: kw::ebnf_sep,
    eq_token: Token![=],
    sep_token: TokenTree,
}

impl Parse for EbnfSep {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::ebnf_sep) {
            Ok(EbnfSep {
                kw_token: input.parse::<kw::ebnf_sep>()?,
                eq_token: input.parse()?,
                sep_token: input.parse()?,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

mod punct {
    use syn::custom_punctuation;

    custom_punctuation!(OrSep, |);
    custom_punctuation!(RuleDef, ::=);
}

struct Rule {
    name: Ident,
    rhs: Expr,
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self> {
        let first = Rule::parse_until(input, punct::RuleDef)?;
        let name: Ident = syn::parse2(first)?;
        let _: punct::RuleDef = input.parse()?;
        let rhs: Expr = input.parse()?;

        Ok(Rule { name, rhs })
    }
}

impl Rule {
    fn parse_until<E: Peek>(input: ParseStream, end: E) -> Result<TokenStream> {
        let mut tokens = TokenStream::new();
        while !input.is_empty() && !input.peek(end) {
            let next: TokenTree = input.parse()?;
            tokens.extend(Some(next));
        }
        Ok(tokens)
    }
}

struct EbnfInput {}

impl Parse for EbnfInput {
    fn parse(input: ParseStream) -> Result<Self> {
        println!("full input: {}", input.to_string());

        let ebnf_sep: EbnfSep = input.parse()?;

        println!("sep for ebnf: {}", ebnf_sep.sep_token.to_string());

        let mut rules = vec![];
        while !input.is_empty() {
            rules.push(input.parse::<Rule>()?);
            let end: TokenTree = input.parse()?;
            if end.to_string() != ebnf_sep.sep_token.to_string() {
                return Err(syn::Error::new_spanned(
                    end.clone(),
                    format!("wrong rules separator {}", end),
                ));
            }
        }

        Ok(EbnfInput {})
    }
}

#[proc_macro]
pub fn ebnf(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    parse_macro_input!(input as EbnfInput);
    proc_macro::TokenStream::new()
}

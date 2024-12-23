use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Peek};
use syn::punctuated::Punctuated;
use syn::{
    braced, bracketed, custom_punctuation, parse_macro_input, token, Ident, LitStr, Result, Token,
};

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

struct Terminal {
    value: String,
}
impl Parse for Terminal {
    fn parse(input: ParseStream) -> Result<Self> {
        let l: LitStr = input.parse()?;
        Ok(Terminal { value: l.value() })
        //}
        //
        //Err(syn::Error::new_spanned(l, "string literal expected"))
    }
}

impl Terminal {
    fn generate(&self, name: &Ident) -> TokenStream {
        let lit = LitStr::new(&self.value, name.span());
        quote! {
            pub struct #name {
                lit: String,
            }

            impl #name {
                pub fn parse(input: &str) -> #name {
                    if !input.starts_with(#lit) {
                        panic!("unexpected terminal: {}", #lit);
                    }
                    #name { lit: #lit.to_string() }
                }
            }
        }
    }
}

struct RuleName {
    name: String,
}
impl Parse for RuleName {
    fn parse(input: ParseStream) -> Result<Self> {
        let rn: Ident = input.parse()?;
        Ok(RuleName {
            name: rn.to_string(),
        })
    }
}

struct Alternatives {
    choices: Punctuated<Rhs, punct::OrSep>,
}

impl Parse for Alternatives {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut segments = Punctuated::new();
        let first = parse_until(input, punct::OrSep)?;
        segments.push_value(syn::parse2(first)?);

        while input.peek(punct::OrSep) {
            segments.push_punct(input.parse()?);
            let second = parse_until(input, punct::OrSep)?;
            segments.push_value(syn::parse2(second)?);
        }

        Ok(Alternatives { choices: segments })
    }
}

impl Alternatives {
    fn generate(&self, name: &Ident) -> TokenStream {
        quote! {
            pub struct #name {
                //values: Vec<String>,
            }

            impl #name {
                pub fn parse(input: &str) -> #name {
                    #name {}
                }
            }
        }
    }
}

struct Sequence {
    seq: Punctuated<Rhs, Token![,]>,
}

impl Parse for Sequence {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut segments = Punctuated::new();
        let first = parse_until(input, Token![,])?;
        segments.push_value(syn::parse2(first)?);

        while input.peek(Token![,]) {
            segments.push_punct(input.parse()?);
            let second = parse_until(input, Token![,])?;
            segments.push_value(syn::parse2(second)?);
        }

        Ok(Sequence { seq: segments })
    }
}

struct Optional {
    optional: Box<Rhs>,
}

impl Parse for Optional {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let p: Rhs = content.parse()?;
        Ok(Optional {
            optional: Box::new(p),
        })
    }
}

struct Repetition {
    rep: Box<Rhs>,
}

impl Parse for Repetition {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        braced!(content in input);
        let p: Rhs = content.parse()?;
        Ok(Repetition { rep: Box::new(p) })
    }
}

enum Rhs {
    Terminal(Terminal),
    Rule(RuleName),
    Alternatives(Alternatives),
    Sequence(Sequence),
    Optional(Optional),
    Repetition(Repetition),
    End,
}

impl Parse for Rhs {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if input.peek2(punct::OrSep) {
            let alt: Alternatives = input.parse()?;
            return Ok(Rhs::Alternatives(alt));
        }

        if input.peek2(Token![,]) {
            let seq: Sequence = input.parse()?;
            return Ok(Rhs::Sequence(seq));
        }

        if lookahead.peek(token::Brace) {
            let rep: Repetition = input.parse()?;
            return Ok(Rhs::Repetition(rep));
        }

        if lookahead.peek(token::Bracket) {
            let opt: Optional = input.parse()?;
            return Ok(Rhs::Optional(opt));
        }

        if lookahead.peek(Ident) {
            let rn: RuleName = input.parse()?;
            return Ok(Rhs::Rule(rn));
        }

        if lookahead.peek(LitStr) {
            let l: Terminal = input.parse()?;
            return Ok(Rhs::Terminal(l));
        }

        Ok(Rhs::End)
    }
}

impl Rhs {
    fn generate(&self, name: &Ident) -> TokenStream {
        match self {
            Rhs::Terminal(t) => t.generate(name),
            Rhs::Alternatives(alt) => alt.generate(name),
            _ => quote! {},
        }
    }
}

struct Rule {
    name: Ident,
    rhs: Rhs,
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self> {
        let first = parse_until(input, Token![=])?;
        let name: Ident = syn::parse2(first)?;
        let _: Token![=] = input.parse()?;
        let ts = parse_until(input, Token![;])?;
        let rhs: Rhs = syn::parse2(ts)?;

        Ok(Rule { name, rhs })
    }
}

impl Rule {
    fn generate_rule_parser(&self) -> TokenStream {
        //let parser_name = &self.name;
        let name = self.name.to_string().to_case(Case::UpperCamel);
        let name = Ident::new(&name, self.name.span());
        self.rhs.generate(&name)

        //quote! {
        //    pub struct #parser_name {
        //
        //    }
        //
        //    impl #parser_name {
        //        pub fn parse(input: &str) -> Self {
        //            #parser_body
        //        }
        //    }
        //}
    }
}

fn parse_until<E: Peek>(input: ParseStream, end: E) -> Result<TokenStream> {
    let mut tokens = TokenStream::new();
    while !input.is_empty() && !input.peek(end) {
        let next: TokenTree = input.parse()?;
        tokens.extend(Some(next));
    }
    Ok(tokens)
}

struct EbnfInput {
    rules: Vec<Rule>,
}

impl Parse for EbnfInput {
    fn parse(input: ParseStream) -> Result<Self> {
        //let ebnf_sep: EbnfSep = input.parse()?;
        //
        //println!("sep for ebnf: {}", ebnf_sep.sep_token.to_string());

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

#[proc_macro]
pub fn ebnf(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as EbnfInput);
    input
        .rules
        .into_iter()
        .map(|e| e.generate_rule_parser())
        .collect::<TokenStream>()
        .into()
}

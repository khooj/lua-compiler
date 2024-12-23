use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Peek};
use syn::punctuated::Punctuated;
use syn::{
    braced, bracketed, custom_punctuation, parse_macro_input, token, Ident, LitChar, LitStr,
    Result, Token,
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
    pub value: char,
}

impl Parse for Terminal {
    fn parse(input: ParseStream) -> Result<Self> {
        let l: LitStr = input.parse()?;
        if l.value().len() != 1 {
            return Err(syn::Error::new_spanned(
                l,
                format!("terminal should be 1 character"),
            ));
        }
        Ok(Terminal {
            value: l.value().chars().nth(0).unwrap(),
        })
    }
}

impl Terminal {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.body(name);
        quote! {
            #[derive(Debug)]
            pub struct #name {
                value: String,
            }

            impl #name {
                pub fn parse(input: &str) -> NomResult<&str, Token> {
                    let (input, ch) = #body(input)?;
                    Ok((input, ch))
                }

                pub fn value(&self) -> &String {
                    &self.value
                }
            }
        }
    }

    fn body(&self, name: &Ident) -> TokenStream {
        let lit = LitChar::new(self.value, name.span());
        quote! {
            map(char(#lit), |t| Token::#name(#name{ value: t.to_string() }))
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

impl RuleName {
    fn ident(&self, name: &Ident) -> Ident {
        Ident::new(&self.name.to_case(Case::UpperCamel), name.span())
    }

    fn generate(&self, name: &Ident) -> TokenStream {
        let ident = self.ident(name);
        let body = self.body(name);
        quote! {
            pub struct #name {}

            impl #name {
                pub fn parse(input: &str) -> NomResult<&str, Token> {
                    let (input, parsed) = #body(input)?;
                    Ok((input, parsed))
                }

                pub fn value(&self) -> String {
                    unimplemented!()
                }
            }
        }
    }

    fn body(&self, name: &Ident) -> TokenStream {
        let ident = self.ident(name);
        quote! {
            <#ident>::parse
        }
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
        let body = self.body(name);
        quote! {
            pub struct #name {
                value: Box<Token>,
            }

            impl #name {
                pub fn parse(input: &str) -> NomResult<&str, Token> {
                    let (input, parsed_alt) = #body(input)?;
                    Ok((input, parsed_alt))
                }

                pub fn value(&self) -> &Token {
                    self.value.as_ref()
                }
            }
        }
    }

    fn body(&self, name: &Ident) -> TokenStream {
        let mut tokens = TokenStream::new();
        for (idx, choice) in self.choices.iter().enumerate() {
            let b = match choice {
                Rhs::Terminal(t) => {
                    let lit = LitChar::new(t.value, name.span());
                    quote! {
                        map(char(#lit), |t| Token::Literal(Literal::from(t)))
                    }
                }
                //Rhs::Optional(opt) => {
                //    let body = opt.optional.body()
                //    quote! {
                //        map(opt())
                //    }
                //}
                _ => choice.body(name),
            };
            let q = quote! {
                #b,
            };
            tokens.extend(Some(q));
        }

        quote! {
            map(alt((#tokens)), |ts| Token::#name(#name { value: Box::new(ts) }))
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

impl Sequence {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.body(name);
        quote! {
            pub struct #name {
                value: Vec<Token>,
            }

            impl #name {
                pub fn parse(input: &str) -> NomResult<&str, Token> {
                    let (input, seq) = #body(input)?;
                    Ok((input, seq))
                }

                pub fn value(&self) -> Vec<Token> {
                    //self.value.clone()
                    unimplemented!()
                }
            }
        }
    }

    fn body(&self, name: &Ident) -> TokenStream {
        let mut tokens = TokenStream::new();
        for seq in self.seq.iter() {
            let b = seq.body(name);
            let q = quote! {
                #b,
            };
            tokens.extend(Some(q));
        }
        let i = (0..self.seq.len()).map(syn::Index::from);
        let sz = self.seq.len();
        quote! {
            map(tuple((#tokens)), |ts| {
                let mut v = Vec::with_capacity(#sz);
                #( v.push(ts.#i); )*
                Token::#name(#name { value: v })
            })
        }
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

impl Optional {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.body(name);
        quote! {
            pub struct #name {
                value: Box<Token>,
            }

            impl #name {
                pub fn parse(input: &str) -> NomResult<&str, Token> {
                    let (input, parsed_opt) = #body(input)?;
                    Ok((input, Token::#name(#name { value: Box::new(parsed_opt) })))
                }
            }
        }
    }

    fn body(&self, name: &Ident) -> TokenStream {
        let b = self.optional.body(name);
        quote! {
            map(opt(#b), |t| Token::Optional(Optional { value: Box::new(t) }))
        }
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

impl Repetition {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.body(name);
        quote! {
            pub struct #name {
                value: Box<Token>,
            }

            impl #name {
                pub fn parse(input: &str) -> NomResult<&str, Token> {
                    let (input, rep) = #body(input)?;
                    Ok((input, Token::#name(#name { value: Box::new(rep) })))
                }
            }
        }
    }

    fn body(&self, name: &Ident) -> TokenStream {
        let body = self.rep.body(name);
        quote! {
            map(many1(#body), |ts| Token::Repetition(Repetition { value: ts }))
        }
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
            Rhs::Optional(opt) => opt.generate(name),
            Rhs::Repetition(rep) => rep.generate(name),
            Rhs::Sequence(seq) => seq.generate(name),
            Rhs::Rule(rn) => rn.generate(name),
            _ => quote! {},
        }
    }

    fn body(&self, name: &Ident) -> TokenStream {
        match self {
            Rhs::Terminal(t) => t.body(name),
            Rhs::Alternatives(alt) => alt.body(name),
            Rhs::Optional(opt) => opt.body(name),
            Rhs::Repetition(rep) => rep.body(name),
            Rhs::Sequence(seq) => seq.body(name),
            Rhs::Rule(rn) => rn.body(name),
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
    fn ident(&self) -> Ident {
        Ident::new(
            &self.name.to_string().to_case(Case::UpperCamel),
            self.name.span(),
        )
    }

    fn generate_rule_parser(&self) -> TokenStream {
        let name = self.ident();
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
    let prelude = quote! {
        use std::io::{Cursor, Read};
        use nom::character::streaming::char;
        use nom::{IResult as NomResult};
        use nom::combinator::{map, opt};
        use nom::branch::alt;
        use nom::multi::many1;
        use nom::sequence::tuple;
    };
    let enum_types = input
        .rules
        .iter()
        .map(|r| {
            let name = r.ident();
            quote! {
                #name(#name),
            }
        })
        .collect::<TokenStream>();

    //let token_parse = input
    //    .rules
    //    .iter()
    //    .map(|r| {
    //        let name = r.ident();
    //        quote! {
    //            Token::#name(t) => t.parse(input),
    //        }
    //    })
    //    .collect::<TokenStream>();

    let token_value = input
        .rules
        .iter()
        .map(|r| {
            let name = r.ident();
            quote! {
                Token::#name(t) => t.value(),
            }
        })
        .collect::<TokenStream>();

    let body = input
        .rules
        .into_iter()
        .map(|e| e.generate_rule_parser())
        .collect::<TokenStream>();

    // TODO: support conjuction, unnamed inline rules (like `["+"|"-"]`)
    let res = quote! {
        #prelude

        pub struct Literal {
            value: String,
        }

        impl From<char> for Literal {
            fn from(value: char) -> Self {
                Literal { value: value.to_string() }
            }
        }

        pub struct Optional {
            value: Box<Option<Token>>,
        }

        impl From<Option<Token>> for Optional {
            fn from(value: Option<Token>) -> Self {
                Optional { value: Box::new(value) }
            }
        }

        pub struct Repetition {
            value: Vec<Token>,
        }

        impl From<Vec<Token>> for Repetition {
            fn from(value: Vec<Token>) -> Self {
                Repetition { value }
            }
        }

        pub enum Token {
            #enum_types
            Literal(Literal),
            Optional(Optional),
            Repetition(Repetition),
        }

        impl Token {
            //pub fn parse(&self, input: &str) -> NomResult<&str, Token> {
            //    match self {
            //        #token_parse
            //        _ => panic!("parse should be used for named rules"),
            //    }
            //}

            pub fn value<T>(&self) -> T {
                match self {
                    //#token_value
                    _ => unimplemented!()
                }
            }
        }

        #body
    };

    res.into()
}

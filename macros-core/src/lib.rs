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
        let b = self.parse_func(name.span());
        quote! {
            #[derive(Debug)]
            pub struct #name {
                value: Literal,
            }

            impl #name {
                pub fn parse_token(input: &str) -> NomResult<&str, Token> {
                    map(<#name>::parse, Token::#name)(input)
                }

                pub fn parse(input: &str) -> NomResult<&str, #name> {
                    map(#b, |l| #name { value: l })(input)
                }
            }
        }
    }

    fn parse_func(&self, span: Span) -> TokenStream {
        let lit = LitChar::new(self.value, span);
        quote! {
            Literal::parse_lit(#lit)
        }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        let ts = self.parse_func(span);
        quote! {
            map(#ts, Token::Literal)
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
    fn ident(&self, span: Span) -> Ident {
        Ident::new(&self.name.to_case(Case::UpperCamel), span)
    }

    fn generate(&self, name: &Ident) -> TokenStream {
        let ident = self.ident(name.span());
        quote! {
            #[derive(Debug)]
            pub struct #name {
                value: #ident,
            }

            impl #name {
                pub fn parse_token(input: &str) -> NomResult<&str, Token> {
                    map(<#name>::parse, Token::#name)(input)
                }

                pub fn parse(input: &str) -> NomResult<&str, #name> {
                    map(<#ident>::parse, |rn| #name { value: rn })(input)
                }
            }
        }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        let ident = self.ident(span);
        quote! {
            <#ident>::parse_token
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
        let body = self.body(name.span());
        quote! {
            #[derive(Debug)]
            pub struct #name {
                value: Alternatives,
            }

            impl #name {
                pub fn parse_token(input: &str) -> NomResult<&str, Token> {
                    map(<#name>::parse, Token::#name)(input)
                }

                pub fn parse(input: &str) -> NomResult<&str, #name> {
                    map(#body, |v| #name { value: v })(input)
                }
            }
        }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        let body = self.body(span);
        quote! {
            map(#body, Token::Alternatives)
        }
    }

    fn body(&self, span: Span) -> TokenStream {
        let mut tokens = TokenStream::new();
        for choice in self.choices.iter() {
            let b = choice.parse_func_token(span.clone());
            let q = quote! { #b, };
            tokens.extend(Some(q));
        }

        quote! {
            Alternatives::parse(alt((#tokens)))
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
        let body = self.body(name.span());
        quote! {
            #[derive(Debug)]
            pub struct #name {
                value: Sequence,
            }

            impl #name {
                pub fn parse_token(input: &str) -> NomResult<&str, Token> {
                    map(<#name>::parse, Token::#name)(input)
                }

                pub fn parse(input: &str) -> NomResult<&str, #name> {
                    map(#body, |v| #name { value: v })(input)
                }
            }
        }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        let body = self.body(span);
        quote! {
            map(#body, Token::Sequence)
        }
    }

    fn body(&self, span: Span) -> TokenStream {
        let mut tokens = TokenStream::new();
        for it in self.seq.iter() {
            let b = it.parse_func_token(span.clone());
            let q = quote! { #b, };
            tokens.extend(Some(q));
        }
        let i = (0..self.seq.len()).map(syn::Index::from);
        let sz = self.seq.len();
        quote! {
            map(tuple((#tokens)), |ts| {
                let mut v = Vec::with_capacity(#sz);
                #( v.push(ts.#i); )*
                Sequence { value: v }
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
        let body = self.body(name.span());
        quote! {
            #[derive(Debug)]
            pub struct #name {
                value: Optional,
            }

            impl #name {
                pub fn parse_token(input: &str) -> NomResult<&str, Token> {
                    map(<#name>::parse, Token::#name)(input)
                }

                pub fn parse(input: &str) -> NomResult<&str, #name> {
                    map(#body, |v| #name { value: v })(input)
                }
            }
        }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        let body = self.body(span);
        quote! {
            map(#body, Token::Optional)
        }
    }

    fn body(&self, span: Span) -> TokenStream {
        let b = self.optional.parse_func_token(span);
        quote! {
            map(opt(#b), |t| Optional { value: Box::new(t) })
        }
    }
}

enum RepeatType {
    ZeroOrMore,
    OneOrMore,
}

struct Repetition {
    rep: Box<Rhs>,
    repeat: RepeatType,
}

impl Parse for Repetition {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        braced!(content in input);
        let p: Rhs = content.parse()?;

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

impl Repetition {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.parse_func(name.span());
        quote! {
            #[derive(Debug)]
            pub struct #name {
                value: Repetition,
            }

            impl #name {
                pub fn parse_token(input: &str) -> NomResult<&str, Token> {
                    map(<#name>::parse, Token::#name)(input)
                }

                pub fn parse(input: &str) -> NomResult<&str, #name> {
                    map(#body, |v| #name { value: v })(input)
                }
            }
        }
    }

    fn parse_func(&self, span: Span) -> TokenStream {
        let body = self.rep.parse_func_token(span);
        let rep = match self.repeat {
            RepeatType::OneOrMore => quote! { many1(#body) },
            RepeatType::ZeroOrMore => quote! { many0(#body) },
        };
        quote! { map(#rep, |ts| Repetition { value: ts }) }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        let body = self.parse_func(span);
        quote! {
            map(#body, Token::Repetition)
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

        Ok(Rhs::Terminal(input.parse()?))
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
        }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        match self {
            Rhs::Terminal(t) => t.parse_func_token(span),
            Rhs::Rule(rn) => rn.parse_func_token(span),
            Rhs::Alternatives(alt) => alt.parse_func_token(span),
            Rhs::Sequence(seq) => seq.parse_func_token(span),
            Rhs::Optional(opt) => opt.parse_func_token(span),
            Rhs::Repetition(rep) => rep.parse_func_token(span),
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
        use nom::character::complete::char;
        use nom::{IResult as NomResult};
        use nom::combinator::{map, opt};
        use nom::branch::alt;
        use nom::multi::{many1, many0};
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

        #[derive(Debug)]
        pub struct Literal {
            value: String,
        }

        impl From<char> for Literal {
            fn from(value: char) -> Self {
                Literal { value: value.to_string() }
            }
        }

        impl Literal {
            //fn parse(input: &str) -> NomResult<&str, Literal> {
            //
            //}

            fn parse_lit<'a>(lit: char) -> impl FnMut(&'a str) -> NomResult<&'a str, Literal> {
                map(char(lit), |t| Literal { value: t.to_string() })
            }
        }

        #[derive(Debug)]
        pub struct Optional {
            value: Box<Option<Token>>,
        }

        impl From<Option<Token>> for Optional {
            fn from(value: Option<Token>) -> Self {
                Optional { value: Box::new(value) }
            }
        }

        #[derive(Debug)]
        pub struct Repetition {
            value: Vec<Token>,
        }

        impl From<Vec<Token>> for Repetition {
            fn from(value: Vec<Token>) -> Self {
                Repetition { value }
            }
        }

        #[derive(Debug)]
        pub struct Alternatives {
            value: Box<Token>,
        }

        impl Alternatives {
            fn parse<'a, F: 'a>(alts: F) -> impl FnMut(&'a str) -> NomResult<&'a str, Alternatives>
                where F: FnMut(&'a str) -> NomResult<&'a str, Token>
            {
                map(alts, |ts| Alternatives { value: Box::new(ts) })
            }
        }

        #[derive(Debug)]
        pub struct Sequence {
            value: Vec<Token>,
        }

        #[derive(Debug)]
        pub enum Token {
            #enum_types
            Literal(Literal),
            Optional(Optional),
            Repetition(Repetition),
            Alternatives(Alternatives),
            Sequence(Sequence),
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

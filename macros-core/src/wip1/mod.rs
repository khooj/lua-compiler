use std::error::Error;

use crate::ebnf::{
    Alternatives as EbnfAlternatives, Disjunction as EbnfDisjunction, EbnfInput,
    Optional as EbnfOptional, Repetition as EbnfRepetition, Rhs as EbnfRhs, Rule as EbnfRule,
    RuleName as EbnfRuleName, Sequence as EbnfSequence, Terminal as EbnfTerminal,
};

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Error as SynError, Ident, LitChar, LitStr};

trait Generate {
    fn generate(&self, name: &Ident) -> TokenStream;
    fn parse_func_token(&self, span: Span) -> TokenStream;

    fn parse_func(&self, span: Span) -> TokenStream {
        quote! {}
    }

    fn body(&self, span: Span) -> TokenStream {
        quote! {}
    }
}

impl Generate for EbnfTerminal {
    fn generate(&self, name: &Ident) -> TokenStream {
        let b = self.parse_func(name.span());
        let ln = LitStr::new(&name.to_string(), name.span());
        quote! {
            #[derive(Debug, PartialEq)]
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

                fn print_tree(&self, level: u32) {
                    print_shifted(level, #ln);
                    self.value.print_tree(level+1);
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

impl Generate for EbnfRuleName {
    fn generate(&self, name: &Ident) -> TokenStream {
        let ident = self.ident(name.span());
        let ln = LitStr::new(&name.to_string(), name.span());
        quote! {
            #[derive(Debug, PartialEq)]
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

                fn print_tree(&self, level: u32) {
                    print_shifted(level, #ln);
                    self.value.print_tree(level+1);
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

impl Generate for EbnfAlternatives {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.body(name.span());
        let ln = LitStr::new(&name.to_string(), name.span());
        quote! {
            #[derive(Debug, PartialEq)]
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

                fn print_tree(&self, level: u32) {
                    print_shifted(level, #ln);
                    self.value.print_tree(level+1);
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
        let mut alts = TokenStream::new();
        for (idx, choice) in self.choices.iter().enumerate() {
            let b = choice.parse_func_token(span.clone());
            let q = quote! { #b, };
            tokens.extend(Some(q));

            if (idx + 1) % 21 == 0 {
                let qq = quote! {
                    alt((#tokens)),
                };
                alts.extend(Some(qq));
                tokens = TokenStream::new();
            }
        }

        let qq = quote! {
            alt((#tokens)),
        };
        alts.extend(Some(qq));

        quote! {
            Alternatives::parse(alt((#alts)))
        }
    }
}

impl Generate for EbnfSequence {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.body(name.span());
        let ln = LitStr::new(&name.to_string(), name.span());
        quote! {
            #[derive(Debug, PartialEq)]
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

                fn print_tree(&self, level: u32) {
                    print_shifted(level, #ln);
                    self.value.print_tree(level+1);
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
        if self.seq.len() > 21 {
            panic!("can't parse sequences large than 21 elements, please split")
        }

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

impl Generate for EbnfOptional {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.body(name.span());
        let ln = LitStr::new(&name.to_string(), name.span());
        quote! {
            #[derive(Debug, PartialEq)]
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

                fn print_tree(&self, level: u32) {
                    print_shifted(level, #ln);
                    self.value.print_tree(level+1);
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

impl Generate for EbnfRepetition {
    fn generate(&self, name: &Ident) -> TokenStream {
        let body = self.parse_func(name.span());
        let ln = LitStr::new(&name.to_string(), name.span());
        quote! {
            #[derive(Debug, PartialEq)]
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

                fn print_tree(&self, level: u32) {
                    print_shifted(level, #ln);
                    self.value.print_tree(level+1);
                }
            }
        }
    }

    fn parse_func(&self, span: Span) -> TokenStream {
        let body = self.rep.parse_func_token(span);
        let rep = match self.repeat {
            crate::ebnf::RepeatType::OneOrMore => quote! { many1(#body) },
            crate::ebnf::RepeatType::ZeroOrMore => quote! { many0(#body) },
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

fn fast_error<E: Error>(span: Span) -> impl Fn(E) -> SynError {
    move |_| SynError::new(span, "fast error")
}

impl Generate for EbnfDisjunction {
    fn generate(&self, name: &Ident) -> TokenStream {
        let ps = self.parse_func(name.span());
        let ln = LitStr::new(&name.to_string(), name.span());
        quote! {
            #[derive(Debug, PartialEq)]
            pub struct #name {
                value: Disjunction,
            }

            impl #name {
                pub fn parse_token(input: &str) -> NomResult<&str, Token> {
                    map(<#name>::parse, Token::#name)(input)
                }

                pub fn parse(input: &str) -> NomResult<&str, #name> {
                    map(#ps, |value| #name { value })(input)
                }

                fn print_tree(&self, level: u32) {
                    print_shifted(level, #ln);
                    self.value.print_tree(level+1);
                }
            }
        }
    }

    fn parse_func(&self, span: Span) -> TokenStream {
        let value_parse = self.value.parse_func_token(span.clone());
        let dis_parse = self.dis.parse_func_token(span);
        quote! {
            Disjunction::parse(#value_parse, #dis_parse)
        }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        let ps = self.parse_func(span);
        quote! { map(#ps, Token::Disjunction) }
    }
}

impl Generate for EbnfRhs {
    fn generate(&self, name: &Ident) -> TokenStream {
        match self {
            EbnfRhs::Terminal(t) => t.generate(name),
            EbnfRhs::Alternatives(alt) => alt.generate(name),
            EbnfRhs::Optional(opt) => opt.generate(name),
            EbnfRhs::Repetition(rep) => rep.generate(name),
            EbnfRhs::Sequence(seq) => seq.generate(name),
            EbnfRhs::Rule(rn) => rn.generate(name),
            EbnfRhs::Disjunction(dis) => dis.generate(name),
        }
    }

    fn parse_func_token(&self, span: Span) -> TokenStream {
        match self {
            EbnfRhs::Terminal(t) => t.parse_func_token(span),
            EbnfRhs::Rule(rn) => rn.parse_func_token(span),
            EbnfRhs::Alternatives(alt) => alt.parse_func_token(span),
            EbnfRhs::Sequence(seq) => seq.parse_func_token(span),
            EbnfRhs::Optional(opt) => opt.parse_func_token(span),
            EbnfRhs::Repetition(rep) => rep.parse_func_token(span),
            EbnfRhs::Disjunction(dis) => dis.parse_func_token(span),
        }
    }
}

struct Rule<'a>(&'a EbnfRule);

impl<'a> Rule<'a> {
    fn generate(&self) -> TokenStream {
        let name = self.0.ident();
        self.0.rhs.generate(&name)
    }
}

pub struct Generator(EbnfInput);

impl Generator {
    pub fn generate(input: EbnfInput) -> TokenStream {
        Generator(input).generate_impl()
    }

    fn generate_impl(&self) -> TokenStream {
        let prelude = quote! {
            use std::io::{Cursor, Read};
            use nom::character::complete::char;
            use nom::{IResult as NomResult};
            use nom::combinator::{map, opt};
            use nom::branch::alt;
            use nom::multi::{many1, many0};
            use nom::sequence::tuple;
            use nom::bytes::complete::take_till;
        };
        let enum_types = self
            .0
            .rules
            .iter()
            .map(|r| {
                let name = r.ident();
                quote! {
                    #name(#name),
                }
            })
            .collect::<TokenStream>();

        let print_tree_match = self
            .0
            .rules
            .iter()
            .map(|r| {
                let name = r.ident();
                quote! {
                    Token::#name(t) => t.print_tree(level+1),
                }
            })
            .collect::<TokenStream>();

        let body = self
            .0
            .rules
            .iter()
            .map(|e| Rule(e).generate())
            .collect::<TokenStream>();

        quote! {
            #prelude

            #[derive(Debug, PartialEq)]
            pub struct Literal {
                value: String,
            }

            impl From<char> for Literal {
                fn from(value: char) -> Self {
                    Literal { value: value.to_string() }
                }
            }

            impl Literal {
                fn parse_lit<'a>(lit: char) -> impl FnMut(&'a str) -> NomResult<&'a str, Literal> {
                    map(char(lit), |t| Literal { value: t.to_string() })
                }

                fn print_tree(&self, level: u32) {
                    print_shifted(level, "Literal:");
                    print_shifted(level+1, &format!("{}", self.value));
                }
            }

            #[derive(Debug, PartialEq)]
            pub struct Optional {
                value: Box<Option<Token>>,
            }

            impl From<Option<Token>> for Optional {
                fn from(value: Option<Token>) -> Self {
                    Optional { value: Box::new(value) }
                }
            }

            impl Optional {
                fn print_tree(&self, level: u32) {
                    print_shifted(level, "Optional:");
                    if let Some(ref t) = *self.value {
                        Token::print_tree_impl(t, level+1);
                    } else {
                        print_shifted(level+1, "None");
                    }
                }
            }

            #[derive(Debug, PartialEq)]
            pub struct Repetition {
                value: Vec<Token>,
            }

            impl From<Vec<Token>> for Repetition {
                fn from(value: Vec<Token>) -> Self {
                    Repetition { value }
                }
            }

            impl Repetition {
                fn print_tree(&self, level: u32) {
                    print_shifted(level, "Repetition:");
                    for el in self.value.iter() {
                        Token::print_tree_impl(el, level+1);
                    }
                }
            }

            #[derive(Debug, PartialEq)]
            pub struct Alternatives {
                value: Box<Token>,
            }

            impl Alternatives {
                fn parse<'a, F: 'a>(alts: F) -> impl FnMut(&'a str) -> NomResult<&'a str, Alternatives>
                    where F: FnMut(&'a str) -> NomResult<&'a str, Token>
                {
                    map(alts, |ts| Alternatives { value: Box::new(ts) })
                }

                fn print_tree(&self, level: u32) {
                    print_shifted(level, "Alternatives:");
                    Token::print_tree_impl(&*self.value, level+1);
                }
            }

            #[derive(Debug, PartialEq)]
            pub struct Sequence {
                value: Vec<Token>,
            }

            impl Sequence {
                fn print_tree(&self, level: u32) {
                    print_shifted(level, "Sequence:");
                    for el in self.value.iter() {
                        Token::print_tree_impl(el, level+1);
                    }
                }
            }

            #[derive(Debug, PartialEq)]
            pub struct Disjunction {
                value: Box<Token>,
            }

            impl Disjunction {
                fn parse<'a, F, F2>(mut value: F, mut dis: F2) -> impl FnMut(&'a str) -> NomResult<&'a str, Disjunction>
                    where
                        F: FnMut(&'a str) -> NomResult<&'a str, Token>,
                        F2: FnMut(&'a str) -> NomResult<&'a str, Token>,
                {
                    map(move |i: &'a str| {
                        let v = value(i);
                        let d = dis(i);
                        if d.is_err() {
                            v
                        } else {
                            Err(nom::Err::Error(nom::error::make_error(i, nom::error::ErrorKind::Fail)))
                        }
                    },
                    |v| Disjunction { value: Box::new(v) })
                }

                fn print_tree(&self, level: u32) {
                    print_shifted(level, "Disjunction:");
                    Token::print_tree_impl(&*self.value, level+1);
                }
            }

            fn print_shifted(level: u32, text: &str) {
                println!("{}", " ".repeat(level.try_into().unwrap()) + text);
            }

            #[derive(Debug, PartialEq)]
            pub enum Token {
                #enum_types
                Literal(Literal),
                Optional(Optional),
                Repetition(Repetition),
                Alternatives(Alternatives),
                Sequence(Sequence),
                Disjunction(Disjunction),
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

                pub fn print_tree(&self) {
                    Token::print_tree_impl(self, 0)
                }

                fn print_tree_impl(token: &Token, level: u32) {
                    match token {
                        #print_tree_match
                        Token::Literal(t) => t.print_tree(level),
                        Token::Optional(t) => t.print_tree(level),
                        Token::Repetition(t) => t.print_tree(level),
                        Token::Alternatives(t) => t.print_tree(level),
                        Token::Sequence(t) => t.print_tree(level),
                        Token::Disjunction(t) => t.print_tree(level),
                    }
                }
            }

            #body
        }
    }
}

use syn::parse_macro_input;

mod ebnf;
mod wip1;
//mod wip2;

#[proc_macro]
pub fn ebnf(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ebnf::EbnfInput);
    wip1::Generator::generate(input).into()
}

#[proc_macro]
pub fn ebnf_wip2(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::new()
}

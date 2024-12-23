use macros_core::ebnf;
pub struct Digit {
    lit: String,
}
impl Digit {
    pub fn parse(input: &str) -> Digit {
        if !input.starts_with("0") {
            {
                ::core::panicking::panic_fmt(
                    format_args!("unexpected terminal: {0}", "0"),
                );
            };
        }
        Digit { lit: "0".to_string() }
    }
}
pub fn main() {}

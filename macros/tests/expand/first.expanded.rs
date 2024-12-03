use macros::ebnf;
pub struct digit {
    pub inner: u8,
}
impl digit {
    pub fn parse(s: &str) -> digit {
        let d = s.parse::<u8>().unwrap();
        if ![0, 1, 2, 3, 4, 5, 6, 7, 8, 9].contains(d) {
            {
                ::core::panicking::panic_fmt(format_args!("parse"));
            };
        }
        digit { inner: d }
    }
}
pub fn main() {}

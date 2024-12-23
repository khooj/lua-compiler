use macros_core::ebnf;

ebnf! {
    digit = "0";
    digits = "0" | "1";
    plusminus = "+" | "-";
    integer = [plusminus], digits, {digits};
};

pub fn main() {}

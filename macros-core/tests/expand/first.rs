use macros_core::ebnf;

ebnf! {
    digit = "0";
    digits = "0" | "1";
    plusminus = "+" | "-";
    integer = [plusminus], digits, {digits}+;
    dis1 = "'", {digits - "0"}+, "'";
    dis2 = "A", digits - "0", {digits-"0"}+, "B" | "C", digits-"1", {digits-"1"}+, "D";
};

pub fn main() {}

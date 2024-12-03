use macros_core::ebnf;

#[test]
pub fn pass() {
    macrotest::expand("tests/expand/*.rs");
}

#[test]
pub fn terminals() {
    ebnf! {
        ebnf_sep = ;
        digit ::= 0 | 1 | 2 | 3;
    }
}

#[test]
pub fn err_rule() {
    ebnf! {
        ebnf_sep = ;
        err_rule <- 0 | 1 | 2;
    }
}

#[test]
pub fn or_rule() {
    ebnf! {
        ebnf_sep = ;
        plusminus ::= [+|-];
    }
}

#[test]
pub fn repetition() {
    ebnf! {
        ebnf_sep = ;
        integer ::= digit{digit};
    }
}

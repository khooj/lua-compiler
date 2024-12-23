use macros_core::ebnf;

#[test]
pub fn pass() {
    macrotest::expand("tests/expand/*.rs");
}

#[test]
pub fn simple_term() {
    ebnf! {
        simple_term = "0";
    };

    let d = SimpleTerm::parse("0");
    assert_eq!(d.lit, "0");
}

#[test]
pub fn terminals() {
    ebnf! {
        digit = "0" | "1" | "2" | "3";
    }
}

//#[ignore]
//#[test]
//pub fn err_rule() {
//    ebnf! {
//        err_rule <- "0" | "1" | "2";
//    }
//}

#[test]
pub fn or_rule() {
    ebnf! {
        signs = "+" | "-";
        plusminus = [signs];
    }
}

#[test]
pub fn repetition() {
    ebnf! {
        integer = digit, {digit};
        many_rep = digit, {digit | "a" | "b"};
    }
}

#[test]
pub fn char_literals() {
    ebnf! {
        char = "c" | "a";
        string = "" | char, {char};
    }
}

#[test]
pub fn simple_seq() {
    ebnf! {
        s = "a", "b", "c";
    }
}

#[test]
pub fn char_seq() {
    ebnf! {
        char = "c" | "a";
        seq_s = char, char, char;
    }
}

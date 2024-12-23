use macros_core::ebnf;

//#[test]
//pub fn simple_term() {
//    ebnf! {
//        simple_term = "0";
//    };
//
//    let (_, d) = SimpleTerm::parse("0").unwrap();
//    assert_eq!(d.lit, '0');
//}
//
//#[test]
//pub fn terminals() {
//    ebnf! {
//        digit = "0" | "1" | "2" | "3";
//    }
//}

#[test]
fn simple_lang() {
    //impl From<i32> for str {
    //    fn from(value: T) -> Self {
    //
    //    }
    //}
    ebnf! {
        digit = "0" | "1" | "2" | "3";
        plusminus = "+" | "-";
        integer = [plusminus], digit, {digit};
    };

    let (_, d) = Digit::parse("1").unwrap();
    assert!(matches!(d, Token::Digit(_)));
    //
    //let (_, d) = Digit::parse("3").unwrap();
    //assert_eq!(d.value, "3");
    //
    //let (_, i) = Integer::parse("1230").unwrap();
    //assert_eq!(i.value, "1230");
    //
    //let (_, i) = Integer::parse("+1230").unwrap();
    //assert_eq!(i.value, "+1230");
}

#[test]
fn cursor_check() {
    use std::io::Cursor;
    use std::io::Read;

    let asd = String::from("asd");

    let mut c = Cursor::new(asd.bytes().collect::<Vec<u8>>());
    let mut s = String::new();
    c.read_to_string(&mut s).unwrap();

    use nom::branch::alt;
    use nom::bytes::streaming::{tag, take};
    use nom::character::streaming::char;
}

//#[ignore]
//#[test]
//pub fn err_rule() {
//    ebnf! {
//        err_rule <- "0" | "1" | "2";
//    }
//}

//#[test]
//pub fn or_rule() {
//    ebnf! {
//        signs = "+" | "-";
//        plusminus = [signs];
//    }
//}
//
//#[test]
//pub fn repetition() {
//    ebnf! {
//        integer = digit, {digit};
//        many_rep = digit, {digit | "a" | "b"};
//    }
//}
//
//#[test]
//pub fn char_literals() {
//    ebnf! {
//        char = "c" | "a";
//        string = char, {char};
//    }
//}
//
//#[test]
//pub fn simple_seq() {
//    ebnf! {
//        s = "a", "b", "c";
//    }
//}
//
//#[test]
//pub fn char_seq() {
//    ebnf! {
//        char = "c" | "a";
//        seq_s = char, char, char;
//    }
//}

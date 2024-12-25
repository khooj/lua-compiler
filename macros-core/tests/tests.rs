use macros_core::ebnf;

#[test]
pub fn simple_term() {
    ebnf! {
        simple_term = "0";
    };

    let (_, st) = SimpleTerm::parse("0").unwrap();
    assert_eq!(st.value.value, "0");
}

#[test]
pub fn terminals() {
    ebnf! {
        digit = "0" | "1" | "2" | "3";
    };

    let (_, _) = Digit::parse("1").unwrap();
    let (_, _) = Digit::parse("2").unwrap();
    let (_, _) = Digit::parse("3").unwrap();
    let (_, _) = Digit::parse("0").unwrap();
    let (output, _) = Digit::parse("0000").unwrap();
    assert_eq!(output, "000");
    assert!(Digit::parse("5").is_err());
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
    use nom::bytes::complete::{tag, take};
    use nom::character::complete::char;
    use nom::{combinator::map, multi::many0};

    let (_, dd) = many0(tag::<&str, &str, ()>("asd"))("asdasdasd").unwrap();
    assert_eq!(dd.len(), 3);
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
    };
}

#[test]
pub fn simple_repetition() {
    ebnf! {
        digit = "0" | "1";
        integer = digit, {digit} *;
    };

    let (_, _) = Integer::parse("10").unwrap();
}

#[test]
pub fn simple_seq() {
    ebnf! {
        s = "a", "b", "c";
    };

    let (_, _) = S::parse("abc").unwrap();
}

#[test]
pub fn char_seq() {
    ebnf! {
        char = "c" | "a";
        seq_s = char, char, char;
    };

    let (_, _) = SeqS::parse("cac").unwrap();
}

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
        pm_opt  = [plusminus];
        integer = [plusminus], digit, {digit}*;
        float = [plusminus], digit, {digit}*, ".", digit, {digit}*;
    };

    let (_, d) = Digit::parse_token("1").unwrap();
    assert!(matches!(d, Token::Digit(_)));
    println!("{:?}", d);

    let (_, _) = Plusminus::parse("+").unwrap();
    let (_, _) = Plusminus::parse("-").unwrap();
    let (_, _) = PmOpt::parse("+").unwrap();
    let (_, _) = PmOpt::parse("-").unwrap();
    let (_, _) = PmOpt::parse("").unwrap();
    //
    //let (_, d) = Digit::parse("3").unwrap();
    //assert_eq!(d.value, "3");
    //
    //let (_, i) = Integer::parse("1230").unwrap();
    //assert_eq!(i.value, "1230");
    //
    let (_, i) = Integer::parse("11").unwrap();
    println!("{:?}", i);
}

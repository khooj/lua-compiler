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
        integer = [plusminus], digit, {digit} *;
        float = [plusminus], digit, {digit} *, ".", digit, {digit} *;
        alt_seq = "a", "b", "c" | "d", "e", "f";
        punct = "|";
        ws = " ";
        S = {ws}*;
        int_braced = "\"", integer, "\"";
        rhs = int_braced, S, punct, S, int_braced;
        term1 = S, rhs, S, ";";
        punct2 = "'";
        dis1 = digit - "0";
        dis2 = digit - digit;
        dis3 = digit - S;
        seq2 = "A", digit - "0", "B";
        alt2 = digit - "0" | digit - "1";
        alt_seq2 = "B", digit - "1", "C" | "D", plusminus - "+", "C";
        dis4 = "A", digit, { digit - "0" } +;
        dis5 = "'", {digit - "0"}+, "'";
    };

    let (_, d) = Digit::parse_token("1").unwrap();
    assert!(matches!(d, Token::Digit(_)));
    //println!("{:?}", d);

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
    let (_, i) = Integer::parse("+11").unwrap();
    //println!("{:?}", i);

    let (_, f) = Float::parse("+1.123").unwrap();

    let (_, _) = AltSeq::parse("abc").unwrap();
    let (_, _) = AltSeq::parse("def").unwrap();
    assert!(AltSeq::parse("ade").is_err());

    let (_, _) = Punct::parse(r#"|"#).unwrap();

    let (_, _) = Term1::parse(r#"    "123" |   "321"   ;"#).unwrap();

    let (_, _) = Punct2::parse("\'").unwrap();

    let (_, _) = Dis5::parse("'123'").unwrap();
}

#[test]
fn ebnf_check() {
    ebnf! {
        letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z" ;

        digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;

        symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"
            | "\"" | "'" | "=" | "|" | "." | "," | ";" | "-"
            | "+" | "*" | "?" | "\n" | "\t" | "\r" ;

        character = letter | digit | symbol | "_" | " " ;
        identifier = letter , { letter | digit | "_" } *;

        S = { " " | "\n" | "\t" | "\r" } *;

        terminal = "'" , character-"'" , { character-"'" } * , "'"
                | "\"" , character-"\"" , { character-"\"" } *, "\"" ;

        terminal2 = "'" , character , { character } * , "'"
                | "\"" , character , { character } *, "\"" ;

        terminator = ";" | "." ;

        term = "(" , S , rhs , S , ")"
            | "[" , S , rhs , S , "]"
            | "{" , S , rhs , S , "}"
            | terminal
            | identifier ;

        factor = term , S , "?"
            | term , S , "*"
            | term , S , "+"
            | term , S , "-" , S , term
            | term , S ;

        concatenation = { S , factor , S , [","] } + ;
        alternation = { S , concatenation , S , ["|"] } + ;

        rhs = alternation ;
        lhs = identifier ;

        rule = lhs , S , "=" , S , rhs , S , terminator ;

        grammar = { S , rule , S } * ;
    };

    let (_, l) = Letter::parse("A").unwrap();
    assert_eq!(
        l.value,
        Alternatives {
            value: Box::new(Token::Literal(Literal {
                value: "A".to_string()
            }))
        }
    );

    let (_, s) = Symbol::parse("\n").unwrap();
    println!("ebnf sym: {:?}", s);

    let (_, upper_s) = S::parse("   \r\n   \t").unwrap();
    println!("ebnf S: {:?}", upper_s);

    let (_, _) = Terminal::parse(r#""asd""#).unwrap();
    let (_, _) = Terminal::parse(r#"'asd'"#).unwrap();

    let (_, term) = Term::parse(r#"'A'"#).unwrap();

    let (_, term) = Term::parse(r#"( a | b | c ) +"#).unwrap();
    // error here
    println!("ebnf term: {:?}", term);

    let code = r#"digit = "0" | "1" | "2" ;
    integer = digit , { digit } ;
    "#;

    let (output, parsed_ebnf) = Grammar::parse(code).unwrap();
    println!("ebnf_check parsed: {:?}", parsed_ebnf);
    println!("ebnf_check out: {}", output);
}

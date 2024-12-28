use macros_core::ebnf;

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

        // we dont support disjunction
        terminal = "'" , character - "'" , { character - "'" } * , "'"
                | '"' , character - '"' , { character - '"' } *, '"' ;

        //terminal = "'" , character , { character } * , "'"
        //        | "\"" , character , { character } *, "\"" ;

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

pub fn main() {}
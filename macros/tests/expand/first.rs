use macros::ebnf;

ebnf! {
    digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
}

ebnf! {
    int123 ::= digit{digit}
}

ebnf! {
    var ::= Name
}

ebnf! {
    int123 ::= [+|-]digit{digit}
}

ebnf! {
    int123 ::= [+|-]
}

//integer ::= [+|-]digit{digit}

pub fn main() {}

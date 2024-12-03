use macros_core::ebnf;

ebnf! {
    ebnf_sep = ;
    digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;
    int123 ::= digit{digit};
    var ::= Name;
    int123 ::= [+|-]digit{digit};
    int123 ::= [+|-];
}

pub fn main() {}

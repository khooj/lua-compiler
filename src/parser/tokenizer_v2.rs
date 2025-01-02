mod lexer {
    use macros_core::ebnf;

    ebnf! {
        chunk = block;
        block = {stat}*, [retstat];
        stat = ";" |
            varlist, "=", explist |
            functioncall |
            label |
            "break" |
            "goto", Name |
            "do", block, "end" |
            "while", exp, "do", block, "end" |
            "repeat", block, "until", exp |
            "if", exp, "then", block, { "elseif", exp, "then", block }*, ["else", block], "end" |
            "for", Name, "=", exp, ",", exp, [",", exp], "do", block, "end" |
            "for", namelist, "in", explist, "do", block, "end" |
            "function", funcname, funcbody |
            "local function", Name, funcbody |
            "local", attnamelist, ["=", explist];
        attnamelist = Name, attrib, {",", Name, attrib}*;
        attrib = ["<", Name, ">"];
        retstat = "return", [explist], [";"];
        label = "::", Name, "::";
        funcname = Name, {".", Name}*, [":", Name];
        varlist = var, {",", var}*;
        var = Name | prefixexp, "[", exp, "]" | prefixexp, ".", Name;
        namelist = Name, {",", Name}*;
        explist = exp, {",", exp}*;
        exp = "nil" | "false" | "true" | Numeral | LiteralString | "..." | functiondef |
            prefixexp | tableconstructor | exp, binop, exp | unop, exp;
        prefixexp = var | functioncall | "(", exp, ")";
        functioncall = prefixexp, args | prefixexp, ":", Name, args;
        args = "(", [explist], ")" | tableconstructor | LiteralString;
        functiondef = "function", funcbody;
        funcbody = "(", [parlist], ")", block, "end";
        parlist = namelist, [",", "..."] | "...";
        tableconstructor = "{", [fieldlist], "}";
        fieldlist = field, {fieldsep, field}*, [fieldsep];
        field = "[", exp, "]", "=", exp | Name, "=", exp | exp;
        fieldsep = "," | ";";
        binop = "+" | "-" | "*" | "/" | "//" | "^" | "%" |
            "&" | "~" | "|" | ">>" | "<<" | ".." |
            "<" | "<=" | ">" | ">=" | "==" | "~=" |
            "and" | "or";
        unop = "-" | "not" | "#" | "~";
        spaces = "\r" | "\n" | " ";
        S = {spaces}*;
        keywords = "and" | "break" | "do" | "else" | "elseif" | "end" |
            "false" | "for" | "function" | "goto" | "if" | "in" |
            "local" | "nil" | "not" | "or" | "repeat" | "return" |
            "then" | "true" | "until" | "while";
        digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
        hex_digit = "a" | "b" | "c" | "d" | "e" | "f" |
            "A" | "B" | "C" | "D" | "E" | "F";
        hex_digits = digit | hex_digit;
        opt_hexfrac = ["p" | "P"];
        opt_hexfrac_e = ["e" | "E"];
        opt_frac_pm = ["+" | "-"];
        opt_frac_hex = [".", {hex_digits}*, opt_hexfrac, opt_frac_pm, {digit}*] |
            [".", {hex_digits}*, opt_hexfrac_e, opt_frac_pm, {digit}*];
        hex_numeral_base = hex_digits, {hex_digits}*, opt_frac_hex;
        hex_numeral = "0x", hex_numeral_base | "0X", hex_numeral_base;
        opt_frac_dec = [".", {digit}*, opt_hexfrac, opt_frac_pm, {digit}*] |
            [".", {digit}*, opt_hexfrac_e, opt_frac_pm, {digit}*];
        dec_numeral = digit, {digit}*, opt_frac_dec;
        Numeral = dec_numeral | hex_numeral;
        letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
            | "H" | "I" | "J" | "K" | "L" | "M" | "N"
            | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
            | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
            | "c" | "d" | "e" | "f" | "g" | "h" | "i"
            | "j" | "k" | "l" | "m" | "n" | "o" | "p"
            | "q" | "r" | "s" | "t" | "u" | "v" | "w"
            | "x" | "y" | "z" ;
        symb = letter | digit | "_";
        name_base = symb - digit, {symb}*;
        Name = name_base - keywords;
        esc_symbol = "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" |
            "\\v" | "\\\\" | "\\\'" | "\\\"" | "\\z";
        any_byte = "\\x", hex_digits, hex_digits | "\\", digit, digit, digit;
        unicode_codepoint = "\\u{", hex_digits, hex_digits, hex_digits, "}";
        short_literal_string_symb = esc_symbol | any_byte | unicode_codepoint;
        short_literal = "\"", {short_literal_string_symb - "\""}*, "\"" |
            "\'", {short_literal_string_symb - "\'"}*, "\'";
        long_literal_string = "[", [{"="}*], "[", {short_literal_string_symb}*, "]", [{"="}*], "]";
        LiteralString = short_literal | long_literal_string;
    }
}

#[cfg(test)]
mod test {
    use super::lexer::*;

    static SIMPLE_LUA: &str = include_str!("simple_lua.lua");

    #[test]
    fn check_parse() -> Result<(), Box<dyn std::error::Error>> {
        let (lost, chunk) = Chunk::parse(SIMPLE_LUA)?;
        Ok(())
    }
}

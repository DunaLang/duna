for symbol, name in [
    ("==", "equality"),
    ("=", "assign"),
    ("<=", "less_than_equals"),
    (">=", "more_than_equals"),
    ("<", "less_than"),
    (">", "more_than")
]:
    print(f'"{symbol.lower()}"   {{ printf("TOKEN(%s)\\n", yytext); return({name.upper()});}}')

for symbol, name in [
    ("(", "left_paren"),
    (")", "right_paren"),
    ("[", "left_bracket"),
    ("]", "right_bracket"),
    ("{", "block_begin"),
    ("}", "block_end"),
    (".", "dot"),
    (",", "comma"),
    (";", "semicolon"),
    ("+", "plus"),
    ("-", "minus"),
    ("*", "asterisk"),
    ("/", "slash"),
]:
    print(f'"{symbol.lower()}"   {{ printf("TOKEN(%s)\\n", yytext); return(yytext[0]);}}')

print()

for identifier in [
    "if",
    "else",
    "while",
    "for",
    "foreach",
    "func",
    "proc",
    "return",
    "break",
    "continue",
    "match",
    "enum",
    "union",
    "struct",
    "tuple",
    "const",
    "static",
    "usize",
    "u8",
    "u16",
    "u32",
    "u64",
    "i8",
    "i16",
    "i32",
    "i64",
    "f32",
    "f64",
    "bool",
    "string",
    "char",
    "true",
    "false",
    "not",
    "and",
    "or"
]:
    print(f'"{identifier.lower()}"          {{ printf("TOKEN(%s)\\n", yytext); return({identifier.upper()}); }}')

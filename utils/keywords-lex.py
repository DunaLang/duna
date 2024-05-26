for symbol, name in [
    ("==", "equality"),
    ("!=", "inequality"),
    ("=", "assign"),
    ("<=", "less_than_equals"),
    (">=", "more_than_equals"),
    ("<", "less_than"),
    (">", "more_than"),
    ("+", "plus"),
    ("-", "minus"),
    ("*", "asterisk"),
    ("/", "slash"),
    ("::", "double_colon"),
    ("=>", "equals_arrow"),
    ("&", "ampersand"),
    ("#", "hashtag"),
    ('%', "percentage")
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
    ("_", "underline"),
    (":", "colon"),
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
    "not",
    "and",
    "or",
    "new",
    "delete",
    "print",
    "cast",
    "typedef"
]:
    print(f'"{identifier.lower()}"          {{ printf("TOKEN(%s)\\n", yytext); return({identifier.upper()}); }}')

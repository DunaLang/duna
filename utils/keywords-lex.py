DEBUG = True

for symbol, name in [
    ("==", "equality"),
    ("=", "equals"),
    ("<=", "less_than_equals"),
    (">=", "more_than_equals"),
    ("<", "less_than"),
    (">", "more_than"),
    ("(", "left_paren"),
    (")", "right_paren"),
    ("[", "left_brace"),
    ("]", "right_brace"),
    ("{", "left_bracket"),
    ("}", "right_bracket"),
    (".", "dot"),
    (",", "comma"),
    (";", "semicolon"),
    ("+", "plus"),
    ("-", "minus"),
    ("*", "asterisk"),
    ("/", "slash"),
]:
    print(f'"{symbol.lower()}" {{')
    if DEBUG:
        print(f'    ADJ; printf("{symbol.lower()}\\n"); return {name.upper()}; }}')
    else:
        print(f"    ADJ; return {name.upper()}; }}")

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
]:
    print(f'"{identifier.lower()}" {{')
    if DEBUG:
        print(
            f'    ADJ; printf("{identifier.lower()}\\n"); return {identifier.upper()}; }}'
        )
    else:
        print(f"    ADJ; return {identifier.upper()}; }}")

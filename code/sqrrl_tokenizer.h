
// The different tokens that the tokenizer can recognize
#define DEF_TOKEN_TYPES                                 \
TOKEN(Invalid,                 "invalid")           \
TOKEN(Semi,                    ";")                 \
TOKEN(Colon,                   ":")                 \
TOKEN(Dot,                     ".")                 \
TOKEN(Comma,                   ",")                 \
TOKEN(Open_Paren,              "(")                 \
TOKEN(Close_Paren,             ")")                 \
TOKEN(Open_Brace,              "{")                 \
TOKEN(Close_Brace,             "}")                 \
TOKEN(Open_Bracket,            "[")                 \
TOKEN(Close_Bracket,           "]")                 \
TOKEN(Annotation,              "@")                 \
TOKEN(Directive,               "#")                 \
TOKEN(Question,                "?")                 \
TOKEN(Dollar,                  "$")                 \
TOKEN(Assign,                  "=")                 \
TOKEN(Lt,                      "<")                 \
TOKEN(Gt,                      ">")                 \
TOKEN(Add,                     "+")                 \
TOKEN(Sub,                     "-")                 \
TOKEN(Mul,                     "*")                 \
TOKEN(Div,                     "/")                 \
TOKEN(Bit_Not,                 "~")                 \
TOKEN(Bit_And,                 "&")                 \
TOKEN(Bit_Or,                  "|")                 \
TOKEN(Bit_Xor,                 "^")                 \
TOKEN(Mod,                     "%")                 \
TOKEN(Line_Comment,            "line comment")      \
TOKEN(Block_Comment,           "block comment")     \
TOKEN(Whitespace,              "whitespace")        \
TOKEN(Ident,                   "identifier")        \
TOKEN(Raw_Ident,               "identifier")        \
TOKEN(Int,                     "number")            \
TOKEN(Float,                   "number")            \
TOKEN(Char,                    "character")         \
TOKEN(String,                  "string")            \
TOKEN(Add_Assign,              "+=")                \
TOKEN(Sub_Assign,              "-=")                \
TOKEN(Mul_Assign,              "*=")                \
TOKEN(Div_Assign,              "/=")                \
TOKEN(Mod_Assign,              "%=")                \
TOKEN(Bit_And_Assign,          "&=")                \
TOKEN(Bit_Or_Assign,           "|=")                \
TOKEN(Bit_Xor_Assign,          "^=")                \
TOKEN(Shl,                     "<<")                \
TOKEN(Shr,                     ">>")                \
TOKEN(Shl_Assign,              "<<=")               \
TOKEN(Shr_Assign,              ">>=")               \
TOKEN(Pow,                     "**")                \
TOKEN(Equals,                  "==")                \
TOKEN(Not_Equals,              "!=")                \
TOKEN(Logical_Not,             "!")                 \
TOKEN(Logical_And,             "&&")                \
TOKEN(Logical_Or,              "||")                \
TOKEN(Lt_Equals,               "<=")                \
TOKEN(Gt_Equals,               ">=")                \
TOKEN(Left_Arrow,              "<-")                \
TOKEN(Right_Arrow,             "->")                \
TOKEN(Double_Right_Arrow,      "=>")                \
TOKEN(Scope,                   "::")                \
TOKEN(Range,                   "..")                \
TOKEN(Ellipsis,                "...")               \
TOKEN(EOF,                     "end of file")       \
TOKEN(Error,                   "error")

global cstring token_type_repr[] = {
#define TOKEN(name, str) str,
    DEF_TOKEN_TYPES
#undef TOKEN
};

global cstring token_type_strings[] = {
#define TOKEN(name, str) "Token_" #name,
    DEF_TOKEN_TYPES
#undef TOKEN
};

enum Token_Type {
#define TOKEN(name, str) Token_##name,
    DEF_TOKEN_TYPES
#undef TOKEN
};

enum Int_Base {
    IntBase_Decimal,
    IntBase_Binary,
    IntBase_Octal,
    IntBase_Hexadecimal,
};

struct Token {
    Token_Type type;
    string source;
    
    string file;
    umm line;
    umm column;
    umm offset;
    
    Int_Base int_base;
    umm suffix_start; // optional defined for literal type suffix e.g.  10i8
    umm num_hashes; // optionally defined for raw string literals.
};

#define f_token(x) FormatType_cstring, token_type_repr[x]
#define f_token_name(x) FormatType_cstring, token_type_strings[x]

struct Tokenizer {
    u8* start;
    u8* end;
    u8* next; // points to the beginning of the next character (peeked)
    u8* curr; // the current character
    u8* curr_line; // points to the first character of a new line.
    
    string source;
    string file;
    smm line_number; // starts at zero
    smm column_number; // starts at zero
    u32 curr_utf32_character; // the current character as unicode codepoint
    smm* lines;
};

// NOTE(alexander): forward declare function
void utf8_advance_character(Tokenizer* tokenizer);

inline void
tokenizer_set_source(Tokenizer* tokenizer, string source, string file) {
    tokenizer->start = source.data;
    tokenizer->end = tokenizer->start + source.count;
    tokenizer->next = tokenizer->start;
    tokenizer->source = source;
    tokenizer->curr = tokenizer->start;
    tokenizer->curr_line = tokenizer->start;
    tokenizer->file = file;
    utf8_advance_character(tokenizer);
    
    if (tokenizer->lines) {
        array_free(tokenizer->lines); // SPEED(alexander): maybe clear instead
        tokenizer->lines = 0;
    }
    
    array_set_capacity(tokenizer->lines, 32);
    array_push(tokenizer->lines, 0);
}

struct Tokenizer_State {
    Tokenizer* tokenizer;
    u8* next;
    u8* curr;
    u8* curr_line;
    smm line_number;
    smm column_number;
    u32 curr_utf32_character;
};

inline Tokenizer_State
save_tokenizer(Tokenizer* t) {
    Tokenizer_State result;
    result.tokenizer = t;
    result.next = t->next;
    result.curr = t->curr;
    result.curr_line = t->curr_line;
    result.line_number = t->line_number;
    result.column_number = t->column_number;
    result.curr_utf32_character = t->curr_utf32_character;
    return result;
}

inline void
restore_tokenizer(Tokenizer_State* state) {
    Tokenizer* t = state->tokenizer;
    smm delta_lines =  state->line_number - t->line_number;
    t->next = state->next;
    t->curr = state->curr;
    t->curr_line = state->curr_line;
    t->line_number = state->line_number;
    t->column_number = state->column_number;
    t->curr_utf32_character = state->curr_utf32_character;
    for (int i = 0; i < delta_lines && array_count(t->lines) > 0; i++) {
        array_pop(t->lines);
    }
}

inline bool
is_hex_digit(u8 c) {
    return ('0' <= c && c <= '9')
        || ('a' <= c && c <= 'f')
        || ('A' <= c && c <= 'F');
}

inline s32
hex_digit_to_s32(u8 c) {
    if      ('0' <= c && c <= '9') return c - (u8) '0';
    else if ('a' <= c && c <= 'f') return c - (u8) 'a' + 10;
    else if ('A' <= c && c <= 'F') return c - (u8) 'A' + 10;
    else                           return -1;
}

inline bool
is_whitespace(u32 c) { // NOTE(alexander): takes utf-32 character as input
    // NOTE(alexander): newlines are not included, they are used to count number of lines etc.
    return (c == 0x0009)  // \t
        || (c == 0x000A)  // \n
        || (c == 0x000B)  // vertical tab
        || (c == 0x000C)  // form feed
        || (c == 0x000D)  // \r
        || (c == 0x0020)  // space
        || (c == 0x0085)  // NEXT LINE from latin1
        || (c == 0x200E)  // LEFT-TO-RIGHT MARK
        || (c == 0x200F)  // RIGHT-TO-LEFT MARK
        || (c == 0x2028)  // LINE SEPARATOR
        || (c == 0x2029); // PARAGRAPH SEPARATOR
}

inline bool
is_ident_start(u32 c) {
    return (('a' <= c && c <= 'z') ||
            ('A' <= c && c <= 'Z') ||
            '_' == c);
}

inline bool
is_ident_continue(u32 c) {
    return (('a' <= c && c <= 'z') ||
            ('A' <= c && c <= 'Z') ||
            ('0' <= c && c <= '9') ||
            '_' == c);
}

inline s32
scan_while(Tokenizer* tokenizer, bool predicate(u32)) {
    int num_scanned = 0;
    while (predicate(tokenizer->curr_utf32_character) && tokenizer->next < tokenizer->end) {
        utf8_advance_character(tokenizer);
        num_scanned++;
    }
    return num_scanned;
}

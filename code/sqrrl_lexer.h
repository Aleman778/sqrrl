
// The different tokens that the tokenizer can recognize
#define DEF_TOKEN_KINDS                                 \
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
TOKEN(Polymorphic,             "$")                 \
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
TOKEN(Byte,                    "number")            \
TOKEN(Char,                    "character")         \
TOKEN(String,                  "string")            \
TOKEN(Raw_String,              "string")            \
TOKEN(Byte_String,             "byte string")       \
TOKEN(Byte_Raw_String,         "byte raw string")   \
TOKEN(Logical_Eq,              "+=")                \
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

global const char* token_kind_repr[] = {
#define TOKEN(name, str) str,
    DEF_TOKEN_KINDS
#undef TOKEN
};

global const char* token_kind_names[] = {
#define TOKEN(name, str) "Token_" #name,
    DEF_TOKEN_KINDS
#undef TOKEN
};

typedef s32 Token_Type;
enum {
#define TOKEN(name, str) Token_##name,
    DEF_TOKEN_KINDS
#undef TOKEN
};

struct Token {
    Token_Type type;
    str source;
    
    str filename;
    smm line;
    smm column;
    smm offset;
}

struct Lexer {
    u8* start;
    u8* end;
    u8* next; // points to the beginning of the next character (peeked)
    u8* curr; // the current character
    u8* curr_line; // points to the first character of a new line.
    
    str source;
    str filepath;
    smm line_number; // starts at zero
    smm column_number; // starts at zero
    u32 curr_utf32_character; // the current character as unicode codepoint
    smm* lines;
};
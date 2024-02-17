// TODO(alexander): this will in the future be replaced with our own implementation
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h" // TODO(alexander): implement this on our own!

#include "sqrrl_basic.h"
#include "sqrrl_identifier.h"


typedef string String; // TODO: We should just rename string -> String makes more sense!

enum Token_Kind {
    Token_EOF = 0,
    // Single character tokens are defined as ASCII code 1 - 127
    
    Token_Semi          = ';',
    Token_Colon         = ':',
    Token_Dot           = '.',
    Token_Comma         = ',',
    Token_Open_Paren    = '(',
    Token_Close_Paren   = ')',
    Token_Open_Brace    = '{',
    Token_Close_Brace   = '}',
    Token_Open_Bracket  = '[',
    Token_Close_Bracket = ']',
    Token_Attribute     = '@',
    Token_Directive     = '#',
    Token_Question      = '?',
    Token_Dollar        = '$',
    Token_Assign        = '=',     
    Token_Lt            = '<',
    Token_Gt            = '>',
    Token_Add           = '+',
    Token_Sub           = '-',
    Token_Mul           = '*',
    Token_Div           = '/',
    Token_Bit_Not       = '~',
    Token_Bit_And       = '&',
    Token_Bit_Or        = '|',
    Token_Bit_Xor       = '^',
    Token_Mod           = '%',
    
    // Keywords (same order as DEF_KEYWORDS)
    Token_Asm = 128,
    Token_Break,
    Token_Case,
    Token_Continue,
    Token_Const,
    Token_Volatile,
    Token_Defer,
    Token_Do,
    Token_Else,
    Token_Enum,
    Token_Export,
    Token_False,
    Token_For,
    Token_Global,
    Token_Extern,
    Token_Internal,
    Token_If,
    Token_In,
    Token_Inline,
    Token_Local_Persist,
    Token_Operator,
    Token_Return,
    Token_Struct,
    Token_Switch,
    Token_True,
    Token_Typedef,
    Token_Union,
    Token_While,
    
    // Type keywords (same order as DEF_TYPE_KEYWORDS)
    Token_Void,
    Token_Bool,
    Token_S8,
    Token_S16,
    Token_S32,
    Token_S64,
    Token_Smm,
    Token_Int,
    Token_U8,
    Token_U16,
    Token_U32,
    Token_U64,
    Token_Umm,
    Token_Uint,
    Token_F32,
    Token_F64,
    Token_String,
    Token_Cstring,
    Token_Typeid,
    
    Token_Ident,
    Token_Int_Literal,
    Token_Float_Literal,
    Token_String_Literal,
    Token_Char_Literal,
    
    // Two character tokens
    Token_Equals,
    Token_Concatenator,
    
    // TODO: Three character tokens
    
    Token_Count,
};

#define token_lit(ch) ((Token_Kind) ch)

struct Location {
    u32 file_index;
    u32 line_number;
    u32 column_number;
};

struct Token {
    Token_Kind kind;
    Location loc;
    
    union {
        string_id ident;
        u64 u64_value;
        f64 f64_value;
    };
};

string
token_to_string(Token token) {
    if (token.kind == Token_EOF) {
        return string_lit("end of file");
        
    } else if (token.kind == Token_Ident) {
        return vars_load_string(token.ident);
        
    } else if (token.kind == Token_Int_Literal) {
        return string_print("%", f_u64(token.u64_value));
        
    } else if (token.kind == Token_Float_Literal) {
        return string_print("%", f_float(token.f64_value));
        
    } else {
        // TODO(Alexander): might be smarter way to do this
        String s = string_alloc(1);
        s.data[0] = (u8) token.kind;
        return s;
    }
}

struct Lexer {
    u8* curr;
    u8* begin;
    u8* end;
    
    Memory_Arena* ast_arena;
    
    Location loc;
    Token curr_token;
    Token unlex_token;
};

Token_Kind lex(Lexer* parser);
void unlex(Lexer* parser);
bool lex_if_matched(Lexer* lexer, Token_Kind kind);

#include "parser.h"

#if 0

{
    int(int, int)* adder; Ast_Pointer(Ast_Call_Expr) Ast_Ident; (Call_Expr -> Type_Procedure);
    Array(int)* ints; Ast_Pointer(Ast_Call_Expr) Ast_Ident; (Call_Expr -> Type_Struct);
    String str;
    fun(); Ast_Call_Expr
}

#endif

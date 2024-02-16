#include "sqrrl_basic.h"
#include "sqrrl_basic.cpp"

typedef string String; // TODO: We should just rename string -> String makes more sense!

enum Token_Kind {
    Token_None = 0,
    Token_EOF = 1,
    
    // Single character tokens are defined as ASCII code 2 - 127
    
    Token_Ident = 128,
    Token_Integer = 129,
    Token_Float = 130,
    
    Token_Equals = 131,
    
    Token_Count,
};

inline Token_Kind
token_lit(u8 ch) {
    return (Token_Kind) ch;
}

struct Location {
    u32 file_index;
    u32 line_number;
    u32 column_number;
};

struct Token {
    Token_Kind kind;
    Location loc;
    
    union {
        String ident;
        u64 s64_value;
        s64 u64_value;
        f64 f64_value;
    };
};

string
token_to_string(Token token) {
    if (token.kind == Token_EOF) {
        return string_lit("end of file");
        
    } else if (token.kind == Token_Ident) {
        return token.ident;
        
    } else if (token.kind == Token_Integer) {
        return string_print("%", f_u64(token.u64_value));
        
    } else if (token.kind == Token_Float) {
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
    
    Location loc;
    Token curr_token;
    Token unlex_token;
};


#if 0

{
    int(int, int)* adder; Ast_Pointer(Ast_Call_Expr) Ast_Ident; (Call_Expr -> Type_Procedure);
    Array(int)* ints; Ast_Pointer(Ast_Call_Expr) Ast_Ident; (Call_Expr -> Type_Struct);
    String str;
    fun(); Ast_Call_Expr
}

#endif
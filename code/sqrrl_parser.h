
struct Parser {
    Tokenizer* tokenizer;
    Token current_token;
    Token peeked_tokens[2];
    s32 num_peeked_tokens;
    
    Arena ast_arena;
};

#define push_ast_node(parser) arena_push_struct(&parser->ast_arena, Ast, 0)

// NOTE(alexander): consumes the following token
Token next_token(Parser* parser);

// NOTE(alexander): produces a token without consuming the source code
Token peek_token(Parser* parser);

// NOTE(alexander): produces a token without consuming the source code
Token peek_second_token(Parser* parser);

// TODO(alexander): better diagnostic, this will do for now!
inline void
parse_error(Parser* parser, Token token, str message) {
    pln("Parse error (%): %\n", f_token(token.type), f_str(message));
}

inline void
parse_error(Parser* parser, Token token, cstr message) {
    parse_error(parser, token, str_lit(message));
}

inline void
parse_error_unexpected_token(Parser* parser, Token_Type expected, Token_Type found) {
    pln("Parse error: expected token `%s` found `%s`", f_token(expected), f_token(found));
}

bool next_token_if_matched(Parser* parser, Token_Type expected, bool report_error=true);
Keyword parse_keyword(Parser* parser);

Ast* parse_identifier(Parser* parser, bool report_error=true);
Ast* parse_expression(Parser* parser, bool report_error=true);
Ast* parse_statement(Parser* parser, bool report_error=true);

Ast* parse_struct_or_union_argument(Parser* parser);
Ast* parse_enum_argument(Parser* parser);
Ast* parse_formal_function_argument(Parser* parser);
Ast* parse_actual_function_argument(Parser* parser);
Ast* parse_compound(Parser* parser, 
                    Token_Type begin, Token_Type end, Token_Type separator,
                    Ast* (*element_parser)(Parser* parser));

Ast* parse_type(Parser* parser);
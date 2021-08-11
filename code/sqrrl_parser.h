
struct Parser {
    Tokenizer* tokenizer;
    Token current_token;
    Token peeked_tokens[2];
    s32 num_peeked_tokens;
    
    Arena ast_arena;
};

inline Ast*
push_ast_node(Parser* parser) {
    return arena_push_struct(&parser->ast_arena, Ast, 0);
}

inline Ast*
push_ast_value(Parser* parser, Value value) {
    Ast* ast = push_ast_node(parser);
    ast->type = Ast_Value;
    ast->Value = value;
    return ast;
}

// TODO(alexander): better diagnostic, this will do for now!
inline void
parse_error(Parser* parser, Token token, str message) {
    pln("%:%:%: %", f_str(token.file), f_smm(token.line + 1), f_smm(token.column + 1), f_str(message));
}

inline void
parse_error_unexpected_token(Parser* parser, Token_Type expected, Token found) {
    parse_error(parser, found, str_format("expected token `%` found `%`", f_token(expected), f_token(found.type)));
}

bool next_token_if_matched(Parser* parser, Token_Type expected, bool report_error=true);
Keyword parse_keyword(Parser* parser, bool report_error=true);

Ast* parse_identifier(Parser* parser, bool report_error=true);
Ast* parse_expression(Parser* parser, bool report_error=true);
Ast* parse_statement(Parser* parser);

Ast* parse_struct_or_union_argument(Parser* parser);
Ast* parse_enum_argument(Parser* parser);
Ast* parse_formal_function_argument(Parser* parser);
Ast* parse_actual_function_argument(Parser* parser);
Ast* parse_compound(Parser* parser, 
                    Token_Type begin, Token_Type end, Token_Type separator,
                    Ast* (*element_parser)(Parser* parser));

Ast* parse_type(Parser* parser);

Token next_token(Parser* parser);
Token peek_token(Parser* parser);
Token peek_second_token(Parser* parser);

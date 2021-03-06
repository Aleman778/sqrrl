
struct Parser {
    Tokenizer* tokenizer;
    Token current_token;
    Token peeked_tokens[2];
    s32 num_peeked_tokens;
    
    Arena ast_arena;
};

inline Ast*
push_ast_node(Parser* parser, Token* token=0) {
    Ast* result = arena_push_struct(&parser->ast_arena, Ast, 0);
    token = token ? token : &parser->current_token;
    if (token) {
        result->span = token_to_span(*token);
    }
    return result;
}

inline Ast*
push_ast_value(Parser* parser, Value value) {
    Ast* result = push_ast_node(parser);
    result->type = Ast_Value;
    result->Value = value;
    return result;
}

inline void
update_span(Parser* parser, Ast* node, Token* token=0) {
    token = token ? token : &parser->current_token;
    if (token) {
        node->span = span_combine(node->span, token_to_span(*token));
    }
}

Token next_token(Parser* parser);
Token peek_token(Parser* parser);
Token peek_second_token(Parser* parser);


// TODO(alexander): better diagnostic, this will do for now!
inline void
parse_error(Parser* parser, Token token, str message) {
    pln("%:%:%: %\n(Tokens - current: `%`, peek: `%`, peek second: `%`)", f_str(token.file), f_smm(token.line + 1), f_smm(token.column + 1), f_str(message), f_token(parser->current_token.type), 
        f_token(peek_token(parser).type), f_token(peek_second_token(parser).type));
    DEBUG_log_backtrace();
}

inline void
parse_error_unexpected_token(Parser* parser, Token_Type expected, Token found) {
    parse_error(parser, found, str_format("expected token `%` found `%`", f_token(expected), f_token(found.type)));
}

bool next_token_if_matched(Parser* parser, Token_Type expected, bool report_error=true);
Keyword parse_keyword(Parser* parser, bool report_error=true);

Ast* parse_identifier(Parser* parser, bool report_error=true);
Ast* parse_expression(Parser* parser, bool report_error=true, u8 min_prec=1);
Ast* parse_atom_expression(Parser* parser, bool report_error=true);
Ast* parse_statement(Parser* parser);

Unary_Op parse_unary_op(Parser* parser);
Binary_Op parse_binary_op(Parser* parser);

Ast* parse_formal_struct_or_union_argument(Parser* parser);
Ast* parse_actual_struct_or_union_argument(Parser* parser);
Ast* parse_formal_enum_argument(Parser* parser);
Ast* parse_formal_function_argument(Parser* parser);
Ast* parse_actual_function_argument(Parser* parser);
Ast* parse_actual_argument(Parser* parser);
Ast* parse_compound(Parser* parser, 
                    Token_Type begin, Token_Type end, Token_Type separator,
                    Ast* (*element_parser)(Parser* parser));

Ast* parse_type(Parser* parser);

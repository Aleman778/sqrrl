
struct Parser {
    array(Source_Group)* source_groups;
    s32 curr_source_group_index;
    
    Tokenizer* tokenizer;
    Token current_token;
    Token peeked_tokens[2];
    s32 num_peeked_tokens;
    
    s32 error_count;
    
    b32 c_compatibility_mode;
    b32 abort_curr_file;
    
    Memory_Arena ast_arena;
};

inline Ast*
push_ast_node(Parser* parser, Token* token=0) {
    Ast* result = arena_push_struct(&parser->ast_arena, Ast);
    token = token ? token : &parser->current_token;
    if (token) {
        result->token = *token;
        result->span = token_to_span(*token);
    }
    return result;
}

inline Ast*
push_ast_value(Parser* parser, Value value, Type* type) {
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Value;
    result->Value = value;
    result->type = type;
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
parse_error(Parser* parser, Token token, string message) {
    pln("%:%:%: error: %", f_string(token.file), f_smm(token.line + 1), f_smm(token.column + 1), f_string(message));
    
#if BUILD_DEBUG
    u8* lookback = token.source.data - 30;
    if (lookback < parser->tokenizer->start) {
        lookback = parser->tokenizer->start;
    }
    u8* lookahead = token.source.data + 30;
    if (lookahead > parser->tokenizer->end) {
        lookahead = parser->tokenizer->end;
    }
    pln("  Source: `...%...`", f_string(string_view(lookback, lookahead)));
    pln("  Tokens: { current = `%`, peek = `%`, peek second = `%` }", f_token(parser->current_token.type), 
        f_token(peek_token(parser).type), f_token(peek_second_token(parser).type));
    
#endif
    
    DEBUG_log_backtrace();
    parser->error_count++;
}

inline void
parse_error_expected_type(Parser* parser, Token found) {
    parse_error(parser, found,
                string_print("expected type found `%`", f_string(found.source)));
}

inline void
parse_error_unexpected_token(Parser* parser, Token_Type expected, Token found) {
    parse_error(parser, found, string_print("expected token `%` found `%`", f_token(expected), f_string(found.source)));
}

inline void
parse_error_unexpected_token(Parser* parser, Token found) {
    parse_error(parser, found, string_print("unexpected token `%`", f_token(found.type)));
}

struct Parse_U64_Value_Result {
    u64 value;
    b32 is_too_large;
};
Parse_U64_Value_Result parse_u64_value(Token token);

bool next_token_if_matched(Parser* parser, Token_Type expected, bool report_error=true);
bool parse_keyword(Parser* parser, Var keyword, bool report_error=true);

Ast* parse_identifier(Parser* parser, bool report_error=true);
Ast* parse_atom(Parser* parser, bool report_error=true, u8 min_prec=1);
Ast* parse_expression(Parser* parser, bool report_error=true, u8 min_prec=1, Ast* atom_expr=0);
Ast* parse_statement(Parser* parser, bool report_error=true);
Ast* parse_block_or_single_statement(Parser* parser, bool report_error=true);
Ast* parse_aggregate_or_expression(Parser* parser, bool report_error=true, Ast* atom=0);

inline Ast* parse_array_type(Parser* parser, Ast* elem_type, Ast_Decl_Modifier mods=0);
Ast* parse_type(Parser* parser, bool report_error=true, Ast_Decl_Modifier mods=0);
Ast* parse_complex_type(Parser* parser, Ast* base_type, bool report_error=true, Ast_Decl_Modifier mods=0);
Ast* parse_pointer_type(Parser* parser, Ast* base_type, bool report_error=true, Ast_Decl_Modifier mods=0);

Operator parse_unary_op(Parser* parser);
Operator parse_binary_op(Parser* parser);

Ast* parse_formal_struct_or_union_argument(Parser* parser);
Ast* parse_actual_struct_or_union_argument(Parser* parser);
Ast* parse_formal_enum_argument(Parser* parser);
Ast* parse_formal_function_argument(Parser* parser);
Ast* parse_actual_function_argument(Parser* parser);
Ast* parse_actual_argument(Parser* parser);
Ast* parse_actual_statement(Parser* parser);
Ast* parse_actual_type(Parser* parser);
Ast* parse_actual_identifier(Parser* parser);
Ast* parse_switch_case(Parser* parser);
Ast* parse_declaration_attribute(Parser* parser);

Ast* parse_compound(Parser* parser, 
                    Token_Type begin, Token_Type end, Token_Type separator,
                    Ast* (*element_parser)(Parser* parser));
Ast* parse_prefixed_compound(Parser* parser, Token_Type prefix,
                             Ast* (*element_parser)(Parser* parser));

Ast_File parse_file(Parser* parser);
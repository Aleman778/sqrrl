
 internal inline void
parse_error(Parser* parser, Token token, str message) {
    pln("Parse error (%): %\n", f_token(token.type), f_str(message));
}

internal inline void
parse_error(Parser* parser, Token token, cstr message) {
    parse_error(parser, token, str_lit(message));
}

internal inline Token
next_semantical_token(Parser* parser) {
    //NOTE(alexander): filtering of comments and whitespace, maybe parameterize this later...
    Token token = advance_token(parser->tokenizer);
    while (!is_semantical_token(token)) {
        token = advance_token(parser->tokenizer);
    }
    return token;
}

Token
next_token(Parser* parser) {
    if (parser->num_peeked_tokens > 0) {
        parser->current_token = parser->peeked_tokens[0];
        parser->peeked_tokens[0] = parser->peeked_tokens[1];
        parser->num_peeked_tokens--;
    } else {
        parser->current_token = next_semantical_token(parser);
    }
    
    if (parser->current_token.type == Token_EOF) {
        // TODO(alexander): add help to remove e.g. mismatched brace.
        parse_error(parser, parser->current_token, "reached end of file while parsing");
    }
    
    return parser->current_token;
}

Token 
peek_token(Parser* parser) {
    if (parser->num_peeked_tokens == 0) {
        parser->peeked_tokens[0] = next_semantical_token(parser);
        parser->num_peeked_tokens++;
    }
    return parser->peeked_tokens[0];
}

Token 
peek_second_token(Parser* parser) {
    if (parser->num_peeked_tokens > 1) {
        return parser->peeked_tokens[1];
    }
    
    if (parser->num_peeked_tokens == 0) {
        parser->peeked_tokens[0] = next_semantical_token(parser);
        parser->num_peeked_tokens++;
    }
    
    parser->peeked_tokens[1] = next_semantical_token(parser);
    parser->num_peeked_tokens++;
    return parser->peeked_tokens[1];
}


struct Parser {
    Tokenizer* tokenizer;
    Token current_token;
    Token peeked_tokens[2];
    s32 num_peeked_tokens;
};




// NOTE(alexander): consumes the following token
Token next_token(Parser* parser);

// NOTE(alexander): produces a token without consuming the source code
Token peek_token(Parser* parser);

// NOTE(alexander): produces a token without consuming the source code
Token peek_second_token(Parser* parser);
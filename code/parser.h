
Ast_Type* parse_type(Lexer* lexer);

Ast_Argument_List* parse_call_argument_list(Lexer* lexer);

Ast_Expression* parse_expression(Lexer* lexer, int min_prec=0);

Ast_Expression* parse_statement(Lexer* lexer);

Ast_Declaration* parse_declaration(Lexer* lexer);

Ast_Type* parse_aggregate_type(Lexer* lexer, Ast_Type* base_type);


Ast_Type* parse_type(Lexer* lexer);

Ast_Argument_List* parse_call_argument_list(Lexer* lexer);

Ast_Expression* parse_expression(Lexer* lexer, int min_prec=0);

Ast_Expression* parse_statement(Lexer* lexer);

void parse_struct_declaration(Lexer* lexer, Ast_Struct_Type* struct_type);

void parse_struct_initializer_list(Lexer* lexer, Ast_Struct_Literal* literal);

Ast_Declaration* parse_declaration(Lexer* lexer);

Ast_Type* parse_aggregate_type(Lexer* lexer, Ast_Type* base_type);

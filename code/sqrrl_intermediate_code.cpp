
void
string_builder_dump_bytecode(String_Builder* sb, Compilation_Unit* cu) {
    if (cu->ast->kind != Ast_Decl_Stmt) {
        return;
    }
    
    if (cu->ast->type->kind != TypeKind_Function) {
        return;
    }
    
    string_builder_push_format(sb, "\n% {", f_type(cu->ast->type));
    
    int bb_index = 0;
    Intermediate_Code* curr = cu->ic_first;
    while (curr) {
        if (curr->opcode == IC_LABEL) {
            string_builder_push_format(sb, "\n");
        }
        
        string_builder_push(sb, curr, bb_index);
        if (curr->opcode == IC_LABEL) {
            bb_index++;
        }
        
        if (curr->next) {
            string_builder_push(sb, "\n");
        }
        curr = curr->next;
    }
    
    string_builder_push(sb, "\n}");
}

void
dump_bytecode(Compilation_Unit* cu) {
    String_Builder sb = {};
    string_builder_dump_bytecode(&sb, cu);
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
}
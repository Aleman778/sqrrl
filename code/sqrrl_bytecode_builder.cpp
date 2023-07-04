

Bytecode_Function*
convert_function_to_bytecode(Bytecode_Builder* bc, Compilation_Unit* cu, bool insert_debug_break) {
    assert(cu->ast->type->kind == TypeKind_Function);
    assert(cu->ast->kind == Ast_Decl_Stmt);
    
    // TODO: handle extern functions here
    if (!cu->ast->Decl_Stmt.stmt) {
        return 0;
    }
    
    Type* type = cu->ast->type;
    
    
    Bytecode_Function* func = begin_bytecode_function(bc, type);
    
    if (insert_debug_break) {
        add_insn(bc, BC_DEBUG_BREAK);
    }
    
    end_bytecode_function(bc);
    
    return func;
}

void
string_builder_dump_bytecode_insn(String_Builder* sb, Bytecode_Instruction* insn) {
    switch (insn->kind) {
        case BytecodeInstructionKind_None: break;
        
        case BytecodeInstructionKind_Base: {
            string_builder_push(sb, bc_opcode_names[insn->opcode]);
            
        } break;
        
        case BytecodeInstructionKind_Binary: {
            string_builder_push(sb, bc_opcode_names[insn->opcode]);
            string_builder_push(sb, "binary");
        } break;
    }
}

void
string_builder_dump_bytecode(String_Builder* sb, Bytecode_Function* func, Type* type) {
    if (!func) return;
    
    if (type) {
        string_builder_push_format(sb, "\n% {", f_type(type));
    } else {
        string_builder_push_format(sb, "func (t%) {", f_u32(func->type_index));
    }
    // TODO: add function params!
    
    int bb_index = 0;
    
    Bytecode_Instruction* curr = iter_bytecode_instructions(func, 0);
    while (curr->kind) {
        
        if (curr->opcode == BC_LABEL) {
            string_builder_push_format(sb, "\n");
        }
        
        //string_builder_push(sb, curr, bb_index);
        if (curr->opcode == BC_LABEL) {
            bb_index++;
        }
        
        if (curr->next_insn) {
            string_builder_push(sb, "\n");
        }
        curr = iter_bytecode_instructions(func, curr);
    }
    
    string_builder_push(sb, "\n}");
}

void
dump_bytecode(Compilation_Unit* cu) {
    if (!cu->bc_func) {
        return;
    }
    
    String_Builder sb = {};
    string_builder_dump_bytecode(&sb, cu->bc_func, cu->ast->type);
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
}
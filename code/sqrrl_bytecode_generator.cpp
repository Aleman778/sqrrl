

struct Bc_Generator {
    Arena* storage;
    
};

Bc_Instruction
bc_generate_add_instruction(Bc_Generator* bc) {
    Bc_Instruction result = {};
    
    Bc_Register var_x = {};
    var_x.index = 0;
    
    result.opcode = Op_Add;
    
    result.dest.kind = BcOperand_Register;
    result.dest.Register = var_x;
    
    result.src0.kind = BcOperand_Register;
    result.src0.Register = var_x;
    result.src0.type.kind = BcTypeKind_s32;
    
    result.src1.kind = BcOperand_Const;
    result.src1.Const.type = Value_signed_int;
    result.src1.Const.signed_int = 20;
    result.src1.type.kind = BcTypeKind_s32;
    
    return result;
}

Bc_Type
bc_generate_type(Bc_Generator* bc, Ast* type) {
    Bc_Type result = {};
    
    switch (type->kind) {
        case Ast_Named_Type: {
            Ast* ast_ident = type->Named_Type;
            assert(ast_ident->kind == Ast_Ident);
            string_id ident = ast_ident->Ident;
            
            if (ident == Kw_string) {
                // TODO(Alexander): string type
            } else if (is_builtin_type_keyword(ident)) {
                u32 primitive = (u32) ident - (u32) builtin_types_begin;
                result.kind = (Bc_Type_Kind) ((u32) BcTypeKind_int + primitive);
            }
        } break;
        
    }
    
    return result;
}

Bc_Instruction
bc_generate_statement(Bc_Generator* bc, Ast* stmt) { 
    Bc_Instruction result = {};
    
    switch (stmt->kind) {
        case Ast_Assign_Stmt: {
            
        } break;
        
        case Ast_Expr_Stmt: {
        } break;
        
        case Ast_Block_Stmt: {
        } break;
        
        case Ast_Break_Stmt: {
        } break;
        
        case Ast_Continue_Stmt: {
        } break;
        
        case Ast_Decl_Stmt: {
        } break;
        
        case Ast_If_Stmt: {
        } break;
        
        case Ast_For_Stmt: {
        } break;
        
        case Ast_While_Stmt: {
        } break;
        
        case Ast_Return_Stmt: {
        } break;
    }
    
    return result;
}

Bc_Basic_Block
bc_generate_basic_block(Bc_Generator* bc, Ast* stmts) {
    Bc_Basic_Block result = {};
    
    Ast* curr = stmts;
    while (curr) {
        Ast* stmt = curr->Compound.node;
        curr = curr->Compound.next;
        Bc_Instruction insn = bc_generate_statement(bc, stmt);
        
        String_Builder sb = {};
        string_builder_push(&sb, &insn);
        string str = string_builder_to_string_nocopy(&sb);
        pln("%", f_string(str));
        string_builder_free(&sb);
        
    }
    
    return result;
}

void
bc_register_declaration(Bc_Generator* bc, string_id ident, Ast* type, Ast* decl) {
    switch (type->kind) {
        case Ast_Function_Type: {
            assert(decl->kind == Ast_Block_Stmt);
            
            Ast* stmts = decl->Block_Stmt.stmts;
            bc_generate_basic_block(bc, stmts);
            
        } break;
    }
}

void
bc_generate_from_top_level_declaration(Bc_Generator* bc, Ast* ast) {
    assert(ast->kind == Ast_Decl);
    
    Bc_Instruction result = {};
    Ast* stmt = ast->Decl.stmt;
    
    switch (stmt->kind) {
        case Ast_Decl_Stmt: {
            Ast* ast_ident = stmt->Decl_Stmt.ident;
            assert(ast_ident->kind == Ast_Ident);
            
            string_id ident = ast_ident->Ident;
            Ast* type = stmt->Decl_Stmt.type;
            Ast* decl = stmt->Decl_Stmt.decl;
            bc_register_declaration(bc, ident, type, decl);
        } break;
        
        case Ast_Assign_Stmt: {
            
            
        } break;
    }
}

void
bc_generate_from_ast(Ast_File* ast_file) {
    Bc_Generator bc = {};
    
    String_Builder sb = {};
    
    for_map(ast_file->decls, decl) {
        bc_generate_from_top_level_declaration(&bc, decl->value);
    }
    
    string result = string_builder_to_string_nocopy(&sb);
    
    pln("%", f_string(result));
}

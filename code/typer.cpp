
void
type_error(Type_Context* tcx, string message, Span span) {
    if (tcx->error_count == 0) {
        Source_File* file = 0;
        if (tcx->file) {
            file = tcx->file->source_file;
        }
        
        if (file) {
            pln("%:%:%: error: %", f_string(file->abspath), 
                f_int(span.l0 + 1), f_int(span.c0 + 1), f_string(message));
        } else {
            pln("error: %", f_string(message));
        }
        
        DEBUG_log_backtrace();
    }
    
    tcx->error_count++;
}

Ast_Declaration*
resolve_declaration_by_identifier(Type_Context* tcx, Ast_Block* block, Identifier ident) {
    Ast_Declaration* result = map_get(block->members, ident);
    if (!result && block->parent) {
        result = resolve_declaration_by_identifier(tcx, block->parent, ident);
    }
    
    
    return result;
}

inline Ast_Type*
resolve_identifier(Type_Context* tcx, Ast_Block* block, Identifier ident) {
    if (is_builtin_type_keyword(ident)) {
        return &ast_basic_types[ident - builtin_types_begin];
        
    } else {
        Ast_Type* result = 0;
        Ast_Declaration* decl = resolve_declaration_by_identifier(tcx, block, ident);
        if (decl) {
            result = decl->inferred_type;
        }
        
        return result;
    }
}

inline Ast_Type*
resolve_identifier(Type_Context* tcx, Identifier ident) {
    return resolve_identifier(tcx, tcx->block, ident);
}

Ast_Type*
infer_expression(Type_Context* tcx, Ast_Expression* expr) {
    Ast_Type* result = 0;
    
    switch (expr->kind) {
        case AST_TYPE: {
            result = (Ast_Type*) expr;
            pln("infer AST_TYPE: %", f_type(result));
        } break;
        
        case AST_LITERAL: {
            auto literal = (Ast_Literal*) expr;
            result = infer_expression(tcx, &ast_basic_types[literal->type]);
        } break;
        
        case AST_IDENTIFIER: {
            auto ident = (Ast_Identifier*) expr;
            result = resolve_identifier(tcx, ident->identifier);
        } break;
        
        case AST_UNARY: {
            unimplemented;
        } break;
        
        case AST_BINARY: {
            result = infer_binary_expression(tcx, (Ast_Binary*) expr);
        } break;
        
        case AST_BLOCK: {
            auto block = (Ast_Block*) expr;
            
            begin_block(tcx, block);
            if (infer_block(tcx, block)) {
                result = t_void;
            }
            end_block(tcx);
        } break;
        
        case AST_RETURN: {
            auto ret = (Ast_Return*) expr;
            Ast_Type* found = infer_expression(tcx, ret->expression);
            result = found;
        } break;
        
        case AST_DECLARATION: {
            result = infer_declaration(tcx, (Ast_Declaration*) expr);
        } break;
        
        case AST_PROCEDURE: {
            if (infer_function(tcx, (Ast_Procedure*) expr)) {
                result = t_void;
            }
        } break;
        
        case AST_STRUCT: {
            unimplemented;
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    expr->inferred_type = result;
    return result;
}

Ast_Type*
infer_binary_expression(Type_Context* tcx, Ast_Binary* binary) {
    Ast_Type* left = infer_expression(tcx, binary->left);
    Ast_Type* right = infer_expression(tcx, binary->right);
    Ast_Type* result = 0;
    
    if (!left || !right) {
        return 0;
    }
    
    // TODO(Alexander): operator overloading
    
    // TODO(Alexander): this only applies to simple operations such as +, -, / and *, add more later
    if (left->kind == TYPE_POINTER) {
        result = left;
        unimplemented; // TODO(Alexander): check legal operators and types on right
        
    } else if ((left->flags & TYPE_FLAG_INTEGER) && (right->flags & TYPE_FLAG_INTEGER)) {
        result = (left->size >= right->size) ? left : right;
        
    } else if ((left->flags & TYPE_FLAG_FLOAT) && (right->flags & TYPE_FLAG_FLOAT)) {
        result = (left->size >= right->size) ? left : right;
        
    } else if ((left->flags & TYPE_FLAG_INTEGER) && (right->flags & TYPE_FLAG_FLOAT)) {
        result = right;
        
        // Add implicit cast 
        //Ast_Cast* cast = push_ast_expression(tcx->lexer, Cast);
        //cast->expr = left;
        //cast->inferred_type = right;
        //binary->left = cast;
        
    } else if ((left->flags & TYPE_FLAG_FLOAT) && (right->flags & TYPE_FLAG_INTEGER)) {
        result = left;
        unimplemented; // TODO(Alexander): add implicit conversion on right to f32/f64
    }
    
    return result;
}

bool
infer_block(Type_Context* tcx, Ast_Block* block) {
    bool success = true;
    for_array_v(block->statements, stmt, _) {
        if (!infer_expression(tcx, (Ast_Expression*) stmt)) {
            success = false;
            break;
        }
    }
    
    return success;
}

bool
infer_function(Type_Context* tcx, Ast_Procedure* proc) {
    begin_block(tcx, proc->args);
    bool result = infer_block(tcx, proc->args);
    
    if (proc->body) {
        begin_block(tcx, proc->body);
        result = result && infer_block(tcx, proc->body);
        end_block(tcx);
    }
    
    end_block(tcx);
    return result;
}

Ast_Type*
infer_declaration(Type_Context* tcx, Ast_Declaration* decl) {
    Ast_Type* result = infer_expression(tcx, decl->type);
    if (result && decl->initializer) {
        if (!infer_expression(tcx, decl->initializer)) {
            result = 0;
        }
    }
    
    return result;
}

bool
check_assignment(Type_Context* tcx, Ast_Type* dest, Ast_Expression* src_expr) {
    bool result = true;
    Ast_Type* src = src_expr->inferred_type;
    if (!dest || !src) {
        // TODO(Alexander): I think if we hit this there should be an error at infer stage.
        return false;
    }
    
    // Integer and float types accept only equal or conversions that are non-lossy 
    // (no data is lost in the auto conversion) lossy if one of these statements are true
    // - not same kind e.g. one is int and other is float
    // - size of dest type is smaller than src type
    u32 numeric_flags = TYPE_FLAG_INTEGER | TYPE_FLAG_FLOAT;
    u32 src_numeric_flags = (src->flags & numeric_flags);
    u32 dest_numeric_flags = (dest->flags & numeric_flags);
    
    if (src_numeric_flags && dest_numeric_flags) {
        
        bool lossy = false;
        if (src_expr->kind == AST_LITERAL) {
            // For literals we can allow auto casts to arbitrary size as long as the value fits
            Ast_Literal* lit = (Ast_Literal*) src_expr;
            
            if (dest_numeric_flags != src_numeric_flags) {
                // TODO: Convert literal value
                unimplemented;
            }
            
            if (dest->flags & TYPE_FLAG_INTEGER) {
                // Check integer overflow
                bool overflow = lit->u64_overflow;
                
                u64 mask = lit->u64_value;
                if (!(src->flags & TYPE_FLAG_UNSIGNED) && (mask & U64_LAST_BIT)) {
                    mask = ~mask;
                }
                int num_bits = intrin_index_of_last_set_bit(mask);
                pln("%: %", f_u64(mask), f_int(num_bits));
                
                
                switch (dest->storage) {
                    case TYPE_S8:  overflow |= num_bits >= 7;  break;
                    case TYPE_U8:  overflow |= num_bits >= 8;  break;
                    case TYPE_S16: overflow |= num_bits >= 15; break;
                    case TYPE_U16: overflow |= num_bits >= 16; break;
                    case TYPE_S32: overflow |= num_bits >= 31; break;
                    case TYPE_U32: overflow |= num_bits >= 32; break;
                    
                    case TYPE_VOID:
                    case TYPE_BOOL:
                    case TYPE_INT:
                    case TYPE_SMM:
                    case TYPE_UMM:
                    case TYPE_UINT: {
                        result = false;
                        type_error(tcx, string_lit("invalid type"), dest->span);
                    } break;
                }
                
                if (overflow) {
                    result = false;
                    type_error(tcx, string_print("constant cannot fit in type `%`", f_type(dest)),
                               src_expr->span);
                }
            }
            
        } else {
            lossy = src_numeric_flags != dest_numeric_flags && dest->size < src->size;
        }
        
        if (lossy) {
            result = false;
            type_error_lossy_conversion(tcx, dest, src, src_expr->span);
        }
    }
    
    return result;
}

bool
check_expression(Type_Context* tcx, Ast_Expression* expr) {
    bool result = true;
    
    switch (expr->kind) {
        case AST_IDENTIFIER: {
            if (!expr->inferred_type) {
                Identifier ident = try_unwrap_identifier(expr);
                if (ident) {
                    type_error(tcx, string_print("undeclared identifier `%`", f_ident(ident)), expr->span);
                } else {
                    type_error(tcx, string_lit("invalid identifier"), expr->span);
                }
                result = false;
            }
        } break;
        
        case AST_RETURN: {
            result = check_expression(tcx, ((Ast_Return*) expr)->expression);
            result = result && check_assignment(tcx, tcx->return_type, expr);
        } break;
        
        case AST_BLOCK: {
            result = check_block(tcx, (Ast_Block*) expr);
        } break;
        
        case AST_DECLARATION: {
            Ast_Declaration* decl = (Ast_Declaration*) expr;
            result = check_expression(tcx, decl->type);
            
            // Check if this declaration shadows a previous one
            Ast_Declaration* shadow = resolve_declaration_by_identifier(tcx, tcx->block, decl->identifier);
            if (shadow != decl) {
                result = false;
                type_error(tcx, string_print("cannot redeclare previous declaration `%`", 
                                             f_ident(decl->identifier)), decl->span);
            }
            
            Ast_Expression* initializer = decl->initializer;
            if (initializer) {
                Ast_Type* type = 0;
                if (decl->type) {
                    type = decl->type->inferred_type;
                }
                
                if (type && type->storage != TYPE_VOID) {
                    result = result && check_expression(tcx, initializer);
                    result = result && check_assignment(tcx, type, initializer);
                    
                } else {
                    result = false;
                    type_error(tcx, string_lit("cannot declare with void type"), {});
                }
            }
        } break;
        
        case AST_PROCEDURE: {
            Ast_Procedure* proc = (Ast_Procedure*) expr;
            Ast_Type* prev_return_type = tcx->return_type;
            tcx->return_type = proc->return_type;
            result = check_expression(tcx, proc->body);
            tcx->return_type = prev_return_type;
        } break;
    }
    
    return result;
}

bool
check_block(Type_Context* tcx, Ast_Block* block) {
    begin_block(tcx, block);
    for_array_v(block->statements, stmt, _) {
        if (!check_expression(tcx, (Ast_Expression*) stmt)) {
            return false;
        }
    }
    end_block(tcx);
    
    return true;
}
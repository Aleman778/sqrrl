

#if 0
void
test_type_rules() {
    // Type rules
    
    {
        // 1. Int + int
        int x;
        x = (s32) 10 + (s64) 20;
        x = (u32) 10 + (s64) 20;
        x = (s32) 10 + (u64) 20;
        x = (s64) 10 + (u64) 20;
        x = 10u + 20ll;
        x = 10 + 20llu;
        x = 1000000000000; // error: doesn't fit in int
    }
    
    {
        u32 a = 10;
        s64 b = -40;
        int x = a + b;
    }
    
    
    {
        s32 a = 10;
        s32 b = -40;
        uint x = a + b;
    }
    
    {
        // Warning for int conversion to lower bitwidth
        s16 x = 10;
        s8 y = 20;
        y += x; // error: conversion lower type
    }
    
    {
        // Unary minus doens't work on unsigned positive number
        int x = -10u; // error: unary minus on unsigned type
    }
    
    {
        // float -> int (vice-versa) reinterprets the value
        f32 x = 10.9f;   // x = 66 66 2E 41 = 10.9
        s32 y = (int) x; // y = 0A 00 00 00 = 10
    }
    
    {
        // ...however int -> int doesn't reinterpret the value
        u32 x = 10u - 20u; // x = FF FF FF F6 = 4294967286
        s32 y = (s32) x;   // y = FF FF FF F6 = -10
    }
    
    {
        int x = (u32) 400;
        bool y = ((uint) -10) > -10;
    }
}
#endif


struct Type_Context {
    Memory_Arena* arena;
    map(string_id, Type*)* locals;
    Ast_Decl_Table* decls;
    s32 error_count;
    s32 warning_count;
};


void
type_error(Type_Context* tcx, string message) {
    Span_Data span = {};
    string filename = string_lit("examples/demo.sq"); // TODO: store names of source files somewhere!
    pln("%:%:%: error: %\n", f_string(filename), f_smm(span.begin_line), f_smm(span.begin_column), f_string(message));
    DEBUG_log_backtrace();
    tcx->error_count++;
}

void
type_warning(Type_Context* tcx, string message) {
    Span_Data span = {};
    string filename = string_lit("examples/demo.sq"); // TODO: store names of source files somewhere!
    pln("%:%:%: warning: %\n", f_string(filename), f_smm(span.begin_line), f_smm(span.begin_column), f_string(message));
    DEBUG_log_backtrace();
    tcx->warning_count++;
}


Type*
type_infer_by_value(Value value) {
    switch (value.type) {
        case Value_void: return &global_void_type;
        case Value_boolean: return &global_primitive_types[PrimitiveType_bool];
        case Value_signed_int: return &global_primitive_types[PrimitiveType_int];
        case Value_unsigned_int: return &global_primitive_types[PrimitiveType_bool];
        case Value_floating: return &global_primitive_types[PrimitiveType_f64];
        
        // TODO(Alexander): complex types require potentially building new types
        //case Value_pointer: 
        //case Value_array:
        //case Value_string:
        
        default: {
            assert(0 && "invalid type");
        } break;
    }
    
    return 0;
}


Type*
type_infer_by_expression(Type_Context* tcx, Ast* expr) {
    Type* result = 0;
    
    switch (expr->kind) {
        case Ast_Value: {
            result = expr->Value.type;
            if (!result) {
                result = type_infer_by_value(expr->Value.value);
            }
            expr->Value.type = result;
        } break;
        
        case Ast_Ident: {
            
            // TODO(Alexander): we need to remember local variables
            Type* type = map_get(tcx->locals, expr->Ident);
            if (!type) {
                type_error(tcx, string_format("`%` is an undeclared identifier", 
                                              f_string(vars_load_string(expr->Ident))));
            }
            expr->Ident_String.type = type;
        } break;
        
        case Ast_Unary_Expr: {
            Type* type = type_infer_by_expression(tcx, expr->Unary_Expr.first);
            if (!type) return 0;
            
            switch (expr->Unary_Expr.op) {
                case UnaryOp_Dereference: {
                    if (type->kind == Type_Pointer) {
                        result = type->Pointer;
                    } else {
                        type_error(tcx, string_format("cannot dereference type `%`", f_type(type)));
                    }
                } break;
                
                case UnaryOp_Address_Of: {
                    result = arena_push_struct(tcx->arena, Type);
                    result->kind = Type_Pointer;
                    result->Pointer = type;
                } break;
                
                default: {
                    result = type;
                } break;
            }
            
            expr->Unary_Expr.type = result;
        } break;
        
        case Ast_Binary_Expr: {
            Type* first_type = type_infer_by_expression(tcx, expr->Binary_Expr.first);
            Type* second_type = type_infer_by_expression(tcx, expr->Binary_Expr.second);
            if (!first_type || !second_type) return 0;
            
            if (binary_is_comparator_table[expr->Binary_Expr.op]) {
                result = &global_primitive_types[PrimitiveType_bool];
            } else {
                result = first_type;
            }
            
            expr->Binary_Expr.type = result;
        } break;
        
        case Ast_Call_Expr: {
            
        } break;
        
        case Ast_Field_Expr: {
            
        } break;
        
        case Ast_Array_Expr: {
            
        } break;
        
        case Ast_Struct_Expr: {
            
        } break;
        
        case Ast_Tuple_Expr: {
            
        } break;
    }
    
    return result;;
}

// NOTE(Alexander): forward declare
Type* create_type_from_ast(Type_Context* tcx, Ast* ast);

internal Type*
create_struct_or_union_type_from_ast(Type_Context* tcx, Ast* arguments, Type_Kind typekind) {
    Type* result = arena_push_struct(tcx->arena, Type);
    result->kind = typekind;
    Type_Table* fields = &result->Struct_Or_Union;
    
    smm offset = 0;
    smm size = 0;
    smm align = 0;
    
    for_compound(arguments, argument) {
        assert(argument->kind == Ast_Argument);
        
        if (!argument->Argument.type) {
            break;
        }
        
        // NOTE(Alexander): for unions we reset the size and alignment for each entry
        if (typekind == Type_Union) {
            size = 0;
            align = 0;
        }
        
        Ast* ast_type = argument->Argument.type;
        Type* type = create_type_from_ast(tcx, ast_type);
        
        switch (argument->Argument.ident->kind) {
            case Ast_Compound: {
                unimplemented;
                for_compound(argument->Argument.ident, ast_ident) {
                    assert(ast_ident->kind == Ast_Ident);
                    string_id ident = ast_ident->Ident;
                    
                    // TODO(Alexander): NEED TO HANDLE THE TYPE TABLE HASH MAP MEMORY
                    map_put(fields->ident_to_type, ident, type);
                    map_put(fields->ident_to_offset, ident, offset);
                    offset += type->cached_size;
                    array_push(fields->idents, ident);
                    fields->count++;
                }
            } break;
            
            case Ast_None: {
                if (type->kind == Type_Struct) {
                    // NOTE(Alexander): anonymous type, pull out the identifiers from struct
                    if(ast_type->Struct_Type.ident->kind == Ast_None) {
                        if (type->Struct_Or_Union.count > 0) {
                            for_map(type->Struct_Or_Union.ident_to_type, other) {
                                // TODO(Alexander): check collisions!! we don't want two vars with same identifier
                                map_put(fields->ident_to_type, other->key, other->value);
                                array_push(fields->idents, other->key);
                                fields->count++;
                            }
                            
                            for_map(type->Struct_Or_Union.ident_to_offset, other) {
                                map_put(fields->ident_to_offset, other->key, other->value);
                            }
                        }
                        
                        if (type->cached_size > size) {
                            size = type->cached_size;
                            result->cached_size = (s32) size;
                        }
                        
                        if (type->cached_align > align) {
                            align = type->cached_align;
                            result->cached_align = (s32) align;
                        }
                    } else {
                        type_error(tcx, string_lit("only anonymous structs can be declared inside a type"));
                    }
                } else if (type->kind == Type_Union) {
                    assert(0 && "unimplemented :(");
                } else {
                    type_error(tcx, string_lit("expected identifier or struct"));
                }
            } break;
            
            case Ast_Ident: {
                assert(argument->Argument.ident->kind == Ast_Ident);
                string_id ident = argument->Argument.ident->Ident;
                
                // TODO(Alexander): NEED TO HANDLE THE TYPE TABLE HASH MAP MEMORY
                map_put(fields->ident_to_type, ident, type);
                map_put(fields->ident_to_offset, ident, offset);
                
                offset = align_forward(size, type->cached_align);
                offset += type->cached_size;
                array_push(fields->idents, ident);
                fields->count++;
                
                size = offset;
                result->cached_size = (s32) size;
                
                if (type->cached_align > align) {
                    align = type->cached_align;
                    result->cached_align = (s32) align;
                }
            } break;
            
            default: {
                assert(0 && "expected identifier, this is likely a parsing bug");
            }
        }
    }
    
    return result;
}

Type*
create_type_from_ast(Type_Context* tcx, Ast* ast) {
    assert(is_ast_type(ast));
    
    Type* result = 0;
    switch (ast->kind) {
        case Ast_Named_Type: {
            string_id ident = ast->Named_Type->Ident;
            if (is_builtin_type_keyword(ident)) {
                if (ident == Kw_string) {
                    result = &global_string_type;
                } else if (ident == Kw_infer) {
                    unimplemented;
                } else {
                    int index = Kw_int + ident;
                    result = &global_primitive_types[index];
                }
            } else {
                // TODO(Alexander): need to store types in global type table
                unimplemented;
            }
        } break;
        
        case Ast_Array_Type: {
            Type* elem_type = create_type_from_ast(tcx, ast->Array_Type.elem_type);
            // TODO(Alexander): do we wan't to store types on stack?
            result = arena_push_struct(tcx->arena, Type);
            result->kind = Type_Array;
            result->Array.type = elem_type;
            // TODO(Alexander): what is the shape, expression, I assume right now it's an integer?
            
            
            // TODO(Alexander): constant evaluation
            Value capacity = {};
            if (ast->Array_Type.shape && ast->Array_Type.shape->kind == Ast_Value) {
                capacity = ast->Array_Type.shape->Value.value;
            }
            
            result->Array.capacity = 0;
            
            // TODO(Alexander): proper type checking
            if (is_integer(capacity)) {
                result->Array.capacity = value_to_smm(capacity);
            } else if (!is_void(capacity)) {
                type_error(tcx, string_lit("expected integer value"));
            }
            result->cached_size = sizeof(smm)*2;
            result->cached_align = alignof(smm);
        } break;
        
        case Ast_Pointer_Type: {
            result = arena_push_struct(tcx->arena, Type);
            result->kind = Type_Pointer;
            result->Pointer = create_type_from_ast(tcx, ast->Pointer_Type);
            result->cached_size = sizeof(smm);
            result->cached_align = alignof(smm);
        } break;
        
        case Ast_Tuple_Type: {
            // TODO(Alexander): implement this
            assert(0 && "unimplemented");
        } break;
        
        case Ast_Infer_Type: {
            // TODO(Alexander): implement this
            assert(0 && "unimplemented");
        } break;
        
        case Ast_Function_Type: {
            result = arena_push_struct(tcx->arena, Type);
            result->kind = Type_Function;
            result->Function.is_variadic = false;
            
            // NOTE(Alexander): Loads in the function arguments
            Ast* ast_arguments = ast->Function_Type.arguments;
            Type_Table* type_arguments = &result->Function.arguments;
            smm offset = 0;
            for_compound(ast_arguments, ast_argument) {
                assert(ast_argument->kind == Ast_Argument);
                if (!ast_argument->Argument.type) {
                    break;
                }
                
                Type* type = create_type_from_ast(tcx, ast_argument->Argument.type);
                string_id ident = ast_argument->Argument.ident->Ident;
                array_push(type_arguments->idents, ident);
                map_put(type_arguments->ident_to_type, ident, type);
                map_put(type_arguments->ident_to_offset, ident, offset);
                type_arguments->count++;
                
                offset = align_forward(offset, type->cached_align);
                offset += type->cached_size;
                
                map_put(type_arguments->ident_to_offset, ident, offset);
            }
            
            result->Function.return_value = create_type_from_ast(tcx, ast->Function_Type.return_type);
            assert(ast->Function_Type.ident && ast->Function_Type.ident->kind == Ast_Ident);
            result->Function.ident = ast->Function_Type.ident->Ident;
            result->cached_size = sizeof(smm);
            result->cached_align = alignof(smm);
        } break;
        
        case Ast_Struct_Type: {
            result = create_struct_or_union_type_from_ast(tcx, ast->Struct_Type.fields, Type_Struct);
        } break;
        
        case Ast_Union_Type: {
            result = create_struct_or_union_type_from_ast(tcx, ast->Union_Type.fields, Type_Union);
        } break;
        
        case Ast_Enum_Type: {
            result = arena_push_struct(tcx->arena, Type);
            result->kind = Type_Enum;
            
            Type* type;
            if (ast->Enum_Type.elem_type && ast->Enum_Type.elem_type->kind != Ast_None) {
                type = create_type_from_ast(tcx, ast->Enum_Type.elem_type);
                if (type->kind != Type_Primitive) {
                    type_error(tcx, string_lit("enums can only be defined as primitive types"));
                    break;
                }
            } else {
                type = &global_primitive_types[PrimitiveType_s64];
            }
            
            result->Enum.type = type;
            
            Value value;
            if (type->Primitive.signedness) {
                value.type = Value_signed_int;
                value.data.signed_int = 0;
            } else {
                value.type = Value_unsigned_int;
                value.data.unsigned_int = 0;
            }
            
            Ast* arguments = ast->Enum_Type.fields;
            while (arguments && arguments->kind == Ast_Compound) {
                Ast* argument = arguments->Compound.node;
                arguments = arguments->Compound.next;
                if (argument->kind == Ast_None) {
                    continue;
                }
                
                assert(!argument->Argument.type && "enums fields don't have different types, parsing bug");
                
                if (argument->Argument.assign && argument->Argument.assign->kind != Ast_None) {
                    
                    // TODO(Alexander): add constant expression evaluation
                    //value = interp_expression(interp, argument->Argument.assign).value;
                    //if (!is_integer(value)) {
                    //TODO(Alexander): show the value in the message
                    //type_error(tcx, string_lit("enums only support integer values"));
                    //break;
                    //}
                    
                    if (type->Primitive.signedness) {
                        value.type = Value_signed_int;
                    } else {
                        value.type = Value_unsigned_int;
                    }
                }
                
                string_id ident = argument->Argument.ident->Ident;
                
                // TODO(Alexander): NEED TO HANDLE THE TYPE TABLE HASH MAP MEMORY
                map_put(result->Enum.values, ident, value);
                
                value.data.signed_int++;
            }
        } break;
        
        case Ast_Typedef: {
            result = create_type_from_ast(tcx, ast->Typedef.type);
        } break;
    }
    
    return result;
}

bool
type_check_assignment(Type_Context* tcx, Type* a, Type* b) {
    if (a->kind != b->kind) {
        return false;
    }
    
    switch (a->kind) {
        case Type_Primitive: {
            
            if (a->Primitive.archetype != b->Primitive.archetype ||
                a->Primitive.size < b->Primitive.size) {
                
                type_error(tcx, string_format("conversion from `%` to `%`, possible loss of data",
                                              f_type(a), f_type(b)));
                return false;
            }
        } break;
    }
    
    return true;
}

void
type_check_statement(Type_Context* tcx, Ast* stmt) {
    
    switch (stmt->kind) {
        
        case Ast_Assign_Stmt: {
            
            Type* expected_type = create_type_from_ast(tcx, stmt->Assign_Stmt.type);
            Type* found_type = create_type_from_ast(tcx, stmt->Assign_Stmt.expr);
            type_check_assignment(tcx, expected_type, found_type);
            
            string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
            map_put(tcx->locals, ident, expected_type);
            
        } break;
        
        case Ast_Expr_Stmt: {
            
        } break;
    }
}

void
type_check_declaration(Type_Context* tcx, Ast* decl) {
    string_id ident = ast_unwrap_ident(decl->Decl.ident);
    Ast* decl_stmt = decl->Decl.stmt;
    if (decl_stmt->kind == Ast_Decl_Stmt) {
        Type* type = create_type_from_ast(tcx, decl_stmt->Decl_Stmt.type);
        type_check_declaration(tcx, decl_stmt->Decl_Stmt.decl);
    } else {
        unimplemented;
    }
}

#if 0
if (first->kind == Type_Primitive && second->kind == Type_Primitive) {
    assert(first->Primitive.archetype != Primitive_None);
    assert(second->Primitive.archetype != Primitive_None);
    
    if (first->Primitive.archetype != second->Primitive.archetype) {
        
        if (expr->Binary_Expr.op == BinaryOp_Assign) {
            assert(0 && "error: cannot implicit cast float to int");
        }
        
        if (first->Primitive.archetype == Primitive_Int) {
            expr->Binary_Expr.first->type = second;
        }
        
        
        if (second->Primitive.archetype == Primitive_Int) {
            expr->Binary_Expr.first->type = second;
            // reinterpret second to float
            unimplemented;
        }
    }
    
    if (first->Primitive.archetype == Primitive_Int) {
        
        
        
        
    } else {
        // Check float, float binary expr
        
    }
    
    
    
}

} break;

}

bool
typer_check_binary_expression(Ast* first, Ast* second) {
    Type* first_type = typer_infer_by_expression(first);
    Type* second_type = typer_infer_by_expression(second);
    
    if (first_type->kind != second_type->kind) {
        return false;
    }
    
    
    switch (first_type->kind) {
        case Type_Primitive: {
            //if (first_type->Primitive.signedness == second_type->Primitive.signedness) {
            
            //}
            
        } break;
        
        case Type_Array: {
            if (!type_equals(first_type->Array.type, second_type->Array.type)) {
                return false;
            }
            if (first_type->Array.capacity != second_type->Array.capacity) {
                return false;
            }
        } break;
        
        case Type_Union:
        case Type_Struct: {
            if (first_type->kind != second_type->kind) {
                return false;
            }
            
            Type_Table* first_table = &first_type->Struct_Or_Union;
            Type_Table* second_table = &first_type->Struct_Or_Union;
            
            if (first_table->count != second_table->count) {
                return false;
            }
            
            // TODO(Alexander): check that entries in the struct/unions match
            unimplemented;
        } break;
        
        default: {
            pln("%", f_string(string_format("% == %", f_type(first_type), f_type(second_type))));
            assert(0 && "not implemented");
        } break;
    }
    
    return false;
}
#endif

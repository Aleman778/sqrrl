
void
interp_save_value(Interp* interp, Type* type, void* storage, Value value) {
    // TODO(Alexander): handle type errors here
    switch (type->kind) {
        case TypeKind_Void:
        case TypeKind_Unresolved:
        case TypeKind_Function: {
            assert(0 && "not a value");
        } break;
        
        case TypeKind_Primitive: {
#define PCASE(T, V) case PrimitiveTypeKind_##T: { \
*((T*) storage) = (T) (V); \
} break;
            switch (type->Primitive.kind) {
                PCASE(int, value.signed_int);
                PCASE(s8, value.signed_int);
                PCASE(s16, value.signed_int);
                PCASE(s32, value.signed_int);
                PCASE(s64, value.signed_int);
                PCASE(smm, value.signed_int);
                PCASE(uint, value.unsigned_int);
                PCASE(u8, value.unsigned_int);
                PCASE(u16, value.unsigned_int);
                PCASE(u32, value.unsigned_int);
                PCASE(u64, value.unsigned_int);
                PCASE(umm, value.unsigned_int);
                PCASE(f32, value.floating);
                PCASE(f64, value.floating);
                case PrimitiveTypeKind_char: {
                    *((u8*) storage) = (u8) value.unsigned_int;
                } break;
                PCASE(bool, value.boolean);
                PCASE(b32, value.signed_int);
                default: {
                    assert(0 && "invalid primitive type");
                } break;
            }
#undef PCASE
        } break;
        
        case TypeKind_Array: {
            // NOTE(Alexander): ugh little bit ugly hack to get this to work
            smm* data = (smm*) storage;
            *data++ = value.array.count;
            void** elements = (void**) data;
            *elements = value.array.elements;
        } break;
        
        case TypeKind_String: {
            *((string*) storage) = value.str;
        } break;
        
        case TypeKind_Pointer:
        case TypeKind_Struct: {
            *((smm*) storage) = value.pointer;
        } break;
    }
}

Interp_Value
interp_resolve_value(Interp* interp, Type* type, void* data) {
    Value value = {};
    
    switch (type->kind) {
        case TypeKind_Void:
        case TypeKind_Unresolved:
        case TypeKind_Function: {
            assert(0 && "not a value");
        } break;
        
        case TypeKind_Primitive: {
            if (type->cached_size <= 0) {
                assert(0 && "not a valid size");
            }
            
#define PCASE(T, V) case PrimitiveTypeKind_##T: { \
value.type = Value_##V; \
value.##V = *((T*) data); \
} break;
            
            switch (type->Primitive.kind) {
                PCASE(int, signed_int);
                PCASE(s8, signed_int);
                PCASE(s16, signed_int);
                PCASE(s32, signed_int);
                PCASE(s64, signed_int);
                PCASE(smm, signed_int);
                PCASE(uint, unsigned_int);
                PCASE(u8, unsigned_int);
                PCASE(u16, unsigned_int);
                PCASE(u32, unsigned_int);
                PCASE(u64, unsigned_int);
                PCASE(umm, unsigned_int);
                PCASE(f32, floating);
                PCASE(f64, floating);
                case PrimitiveTypeKind_char: {
                    value.type = Value_unsigned_int;
                    value.unsigned_int = *((u8*) data);
                } break;
                PCASE(bool, boolean);
                PCASE(b32, signed_int);
                default: {
                    assert(0 && "invalid primitive type");
                } break;
            }
#undef PCASE
        } break;
        
        case TypeKind_Array: {
            smm* mdata = (smm*) data;
            value.array.count = *mdata++;
            value.array.elements = *((void**) mdata);
            value.type = Value_array;
        } break;
        
        case TypeKind_String: {
            value.str = *((string*) data);
            value.type = Value_string;
        } break;
        
        case TypeKind_Pointer:
        case TypeKind_Struct: {
            value.pointer = *((smm*) data);
            value.type = Value_pointer;
        } break;
    }
    
    Interp_Value result = create_interp_value(interp);
    result.value = value;
    result.type = type;
    result.data = data;
    return result;
}

Interp_Value 
interp_expression(Interp* interp, Ast* ast) {
    assert(is_ast_expr(ast) || ast->type == Ast_Value || ast->type == Ast_Ident || ast->type == Ast_None);
    
    Interp_Value result = create_interp_value(interp);
    
    switch (ast->type) {
        // TODO(alexander): do we want values to be expressions?
        case Ast_Value: {
            result.value = ast->Value;
        } break;
        
        // TODO(alexander): do we want identifiers to be expressions?
        case Ast_Ident: {
            result = interp_resolve_value(interp, ast->Ident);
        } break;
        
        case Ast_Unary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Unary_Expr.first);
            switch (ast->Unary_Expr.op) {
                case UnaryOp_Negate: {
                    if (is_integer(first_op.value)) {
                        result.value.signed_int = -first_op.value.signed_int;
                    } else if(is_floating(first_op.value)) {
                        result.value.floating = -first_op.value.floating;
                    } else {
                        interp_error(interp, string_lit("expected numeric type"));
                    }
                } break;
                
                case UnaryOp_Not: {
                    if (is_integer(first_op.value)) {
                        result.value.boolean = !value_to_u64(first_op.value);
                    } else {
                        interp_error(interp, string_lit("expected integer type"));
                    }
                } break;
                
                case UnaryOp_Dereference: {
                    assert(0 && "unimplemented :(");
                } break;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Binary_Expr.first);
            Interp_Value second_op = interp_expression(interp, ast->Binary_Expr.second);
            
            // TODO(Alexander): check illegal types such as arrays and struct literals!
            Value first = first_op.value;
            Value second = second_op.value;
            
            // NOTE(Alexander): Type rules
            // int + int;
            // float + int -> float + float;
            // int + float -> float + float;
            // float -x-> int (is a no no, it has to be an explicit cast)
            
            if (is_floating(first) || is_floating(second)) {
                // NOTE(Alexander): Make sure both types are floating
                if (is_integer(first)) {
                    first.floating  = value_to_f64(first);
                    first.type = Value_floating;
                    result.type = second_op.type;
                } else if (is_integer(second)) {
                    second.floating = value_to_f64(second);
                    second.type = Value_floating;
                    result.type = first_op.type;
                } else if (is_floating(first) && is_floating(second)) {
                    result.type = first_op.type;
                } else {
                    interp_error(interp, string_lit("type error: mismatched types"));
                }
                
                first = value_floating_binary_operation(first, second, ast->Binary_Expr.op);
                
                result.value = first;
            } else if (is_integer(first) || is_integer(second)) {
                // NOTE(Alexander): integer math
                first.signed_int = value_integer_binary_operation(first, second, ast->Binary_Expr.op);
                
                result.value = first;
                result.type = first_op.type;
            } else {
                interp_error(interp, string_lit("type error: mismatched types"));
            }
            
            // NOTE(Alexander): handle assign binary expression
            if (result.value.type != Value_void && is_binary_assign(ast->Binary_Expr.op)) {
                
                if (ast->Binary_Expr.first->type == Ast_Ident) {
                    string_id ident = ast->Binary_Expr.first->Ident;
                    Entity entity = map_get(interp->symbol_table, ident);
                    if (interp_entity_is_declared(interp, &entity, ident)) {
                        if (entity.data) {
                            interp_save_value(interp, entity.type, entity.data, result.value);
                        } else {
                            entity.data = interp_push_value(interp, entity.type, result.value);
                        }
                    }
                } else if (first_op.data && first_op.type) {
                    interp_save_value(interp, first_op.type, first_op.data, first);
                    
                } else {
                    interp_error(interp, string_lit("unexpected assignment"));
                }
            }
        } break;
        
        case Ast_Ternary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Ternary_Expr.first);
            Interp_Value second_op = interp_expression(interp, ast->Ternary_Expr.second);
            Interp_Value third_op = interp_expression(interp, ast->Ternary_Expr.third);
            
            if (is_integer(first_op.value)) {
                if (first_op.value.boolean) {
                    result = second_op;
                } else {
                    result = third_op;
                }
            } else {
                interp_error(interp, string_lit("type error: expected left operand to a boolean"));
            }
            
            
        } break;
        
        case Ast_Call_Expr: {
            string_id ident = ast->Call_Expr.ident->Ident;
            result = interp_function_call(interp, ident, ast->Call_Expr.args);
        } break;
        
        case Ast_Field_Expr: {
            Interp_Value var = interp_expression(interp, ast->Field_Expr.var);
            assert(var.value.type == Value_pointer);
            assert(var.type && var.type->kind == TypeKind_Struct);
            
            Type_Table* type_table = &var.type->Struct.fields;
            assert(ast->Field_Expr.field && ast->Field_Expr.field->type == Ast_Ident);
            string_id ident = ast->Field_Expr.field->Ident;
            
            Type* field_type = map_get(type_table->ident_to_type, ident);
            
            if (field_type) {
                assert(var.type->kind == TypeKind_Struct && "mismatched types");
                
                void* data = var.value.data;
                smm offset = map_get(var.type->Struct.ident_to_offset, ident);
                data = (u8*) data + offset;
                
                result = interp_resolve_value(interp, field_type, data);
            } else {
                interp_unresolved_identifier_error(interp, ident);
            }
            
        } break;
        
        case Ast_Cast_Expr: {
            Type* type = interp_type(interp, ast->Cast_Expr.type);
            Interp_Value expr = interp_expression(interp, ast->Cast_Expr.expr);
            Value value = expr.value;
            
            switch (type->kind) {
                case TypeKind_Primitive: {
                    // NOTE(Alexander): meta programming yeah, less typing more confusing!
#define PCASE(T, V) case PrimitiveTypeKind_##T: { \
result.value.type = Value_##V; \
result.value.##V = (T) (value.##PVALUE); \
} break;
#define PTYPECONV switch (type->Primitive.kind) { \
PCASE(int, signed_int); \
PCASE(s8, signed_int); \
PCASE(s16, signed_int); \
PCASE(s32, signed_int); \
PCASE(s64, signed_int); \
PCASE(smm, signed_int); \
PCASE(uint, unsigned_int); \
PCASE(u8, unsigned_int); \
PCASE(u16, unsigned_int); \
PCASE(u32, unsigned_int); \
PCASE(u64, unsigned_int); \
PCASE(umm, unsigned_int); \
PCASE(f32, floating); \
PCASE(f64, floating); \
case PrimitiveTypeKind_char: { \
result.value.type = Value_unsigned_int; \
result.value.unsigned_int = (u8) result.value.unsigned_int; \
} break; \
PCASE(bool, boolean); \
PCASE(b32, signed_int); \
default: { \
assert(0 && "invalid primitive type"); \
} break; \
}
                    
                    switch (expr.value.type) {
                        case Value_boolean: {
#define PVALUE boolean
                            PTYPECONV
#undef PVALUE
                        } break;
                        
                        case Value_signed_int: {
#define PVALUE signed_int
                            PTYPECONV
#undef PVALUE
                        } break;
                        
                        case Value_unsigned_int: {
#define PVALUE unsigned_int
                            PTYPECONV
#undef PVALUE
                        } break;
                        
                        case Value_floating: {
#define PVALUE floating
                            PTYPECONV
#undef PVALUE
                        } break;
                        
                        default: {
                            assert(0 && "invalid type conversion");
                        } break;
                    }
                    
#undef PVALUE
#undef PTYPECONV
#undef PCASE
                    
                } break;
                
                case TypeKind_Pointer: {
                    if (value.type != Value_pointer) {
                        assert(0 && "error cannot convert value to pointer");
                    }
                    
                } break;
                
                default: {
                    assert(0 && "unimplemented :(");
                } break;
                
            }
            
            result.type = type;
        } break;
        
        case Ast_Paren_Expr: {
            result = interp_expression(interp, ast->Paren_Expr.expr);
        } break;
        
        case Ast_Index_Expr: {
            Interp_Value array = interp_expression(interp, ast->Index_Expr.array);
            
            // TODO(Alexander): also allow pointer numerics
            if (array.value.type == Value_array) {
                
                Interp_Value index = interp_expression(interp, ast->Index_Expr.index);
                if (is_integer(index.value)) {
                    Array_Value array_value = array.value.array;
                    
                    smm array_index = value_to_smm(index.value);
                    if (array_index < array_value.count) {
                        // TODO(Alexander): do we need null checking here?
                        Type* elem_type = array.type->Array.type;
                        smm elem_size = elem_type->cached_size;
                        assert(elem_size > 0 && "not valid array element size");
                        
                        void* data = (void*) array_value.elements;
                        data = (u8*) data + array_index * elem_size;
                        
                        result = interp_resolve_value(interp, elem_type, data);
                    } else {
                        interp_error(interp, string_lit("array index out of bounds"));
                    }
                } else {
                    interp_error(interp, string_lit("type error: expected numeric array index"));
                }
            } else {
                interp_error(interp, string_lit("type error: expected array value"));
            }
        } break;
        
        case Ast_Array_Expr:
        case Ast_Struct_Expr:
        case Ast_Tuple_Expr: {
            result.value.type = Value_ast_node;
            result.value.ast = ast;
        } break;
    }
    
    return result;
}

Interp_Value
interp_function_call(Interp* interp, string_id ident, Ast* args) {
    Interp_Value result = create_interp_value(interp);
    
    Entity decl = map_get(interp->symbol_table, ident);
    if (decl.type) {
        
        if (decl.type->kind == TypeKind_Function) {
            Ast* block = decl.type->Function.block;
            Type_Table* formal_arguments = &decl.type->Function.arguments;
            
            if (block) {
                
                // Push a new context by updating the base pointer
                smm new_base = interp->stack.curr_used;
                smm* old_base = (smm*) arena_push_size(&interp->stack, sizeof(smm));
                *old_base = interp->base_pointer;
                interp->base_pointer = new_base;
                
                // NOTE(Alexander): push arguments on stack
                if (args) {
                    int arg_index = 0;
                    while (args && args->type == Ast_Compound) {
                        Ast* expr = args->Compound.node;
                        args = args->Compound.next;
                        
                        // NOTE(Alexander): only interpret as many args as formally declared
                        if (arg_index < formal_arguments->count) {
                            // TODO(Alexander): assign binary expressions needs to be handled here
                            Interp_Value arg = interp_expression(interp, expr);
                            string_id arg_ident = formal_arguments->idents[arg_index];
                            
                            void* data = interp_push_value(interp, arg.type, arg.value);
                            Entity entity;
                            entity.data = data;
                            entity.type = arg.type;
                            map_put(interp->symbol_table, arg_ident, entity);
                        }
                        
                        arg_index++;
                    }
                    
                    if (arg_index > formal_arguments->count) {
                        interp_error(interp, string_format("function `%` did not take % arguments, expected % arguments", 
                                                           f_string(vars_load_string(ident)), 
                                                           f_int(arg_index), 
                                                           f_int(formal_arguments->count)));
                        return result;
                    }
                } else if (formal_arguments->count != 0) {
                    interp_error(interp, string_format("function `%` expected % arguments",
                                                       f_string(vars_load_string(ident)),
                                                       f_int(formal_arguments->count)));
                }
                
                result = interp_statement(interp, block);
                // TODO(alexander): only write to result if it is an actual return value!
                
                // Pop the context by fetching the old base pointer and updating the stack
                interp->stack.curr_used = interp->base_pointer;
                interp->base_pointer = *(smm*) ((u8*) interp->stack.base + interp->base_pointer);
            } else {
                // TODO(Alexander): this is not supposed to ever be the case, maybe assert instead!
                interp_error(interp, string_format("`%` function has no definition", f_string(vars_load_string(ident))));
            }
        } else {
            interp_error(interp, string_format("`%` is not a function", f_string(vars_load_string(ident))));
        }
    } else {
        interp_unresolved_identifier_error(interp, ident);
    }
    
    return result;
}

Interp_Value
interp_statement(Interp* interp, Ast* ast) {
    assert(is_ast_stmt(ast) || ast->type == Ast_None);
    
    Interp_Value result = {};
    
    switch (ast->type) {
        case Ast_Assign_Stmt: {
            Interp_Value expr = interp_expression(interp, ast->Assign_Stmt.expr);
            
            // TODO(Alexander): check expr.type and type
            Type* type = interp_type(interp, ast->Assign_Stmt.type);
            string_id ident = ast->Assign_Stmt.ident->Ident;
            
            
            switch (type->kind) {
                case TypeKind_Array: {
                    Array_Value array = {};
                    
                    if (is_ast_node(expr.value)) {
                        ast = expr.value.ast;
                        if (ast && ast->type == Ast_Array_Expr) {
                            Ast* elements = ast->Array_Expr.elements;
                            Type* elem_type = type->Array.type;
                            
                            while (elements && elements->type == Ast_Compound) {
                                Ast* element = elements->Compound.node;
                                elements = elements->Compound.next;
                                
                                Value elem_value = interp_expression(interp, element).value;
                                if (!is_void(elem_value)) {
                                    void* elem = interp_push_value(interp, elem_type, elem_value);
                                    array.count++;
                                    if (!array.elements) {
                                        array.elements = elem;
                                    }
                                } else {
                                    interp_error(interp, string_lit("type error: expected literal value"));
                                }
                            }
                        }
                    } else {
                        // TODO(Alexander): pre allocate array, MUST have a capacity
                        assert(0 && "unimplemented :(");
                    }
                    
                    Value value;
                    value.type = Value_array;
                    value.array = array;
                    void* data = interp_push_value(interp, type, value);
                    
                    Entity entity;
                    entity.data = data;
                    entity.type = type;
                    map_put(interp->symbol_table, ident, entity);
                    
                } break;
                
                case TypeKind_Struct: {
                    
                    Type_Table* type_table = &type->Struct.fields;
                    void* base_address = 0;
                    struct { string_id key; Value value; }* field_values = 0;
                    
                    if (is_ast_node(expr.value)) {
                        assert(expr.value.type == Value_ast_node &&
                               expr.value.ast->type == Ast_Struct_Expr);
                        Ast* fields = expr.value.ast->Struct_Expr.fields;
                        
                        // NOTE(Alexander): push elements onto the stack in the order defined by the type
                        // so first push the compound actual values into a auxillary hash map.
                        while (fields && fields->type == Ast_Compound) {
                            Ast* field = fields->Compound.node;
                            fields = fields->Compound.next;
                            assert(field->type == Ast_Argument);
                            
                            Interp_Value field_expr = interp_expression(interp, field->Argument.assign);
                            
                            assert(field->Argument.ident);
                            string_id field_ident = field->Argument.ident->Ident;
                            
                            // NOTE(Alexander): check that the actual type maches its definition
                            Type* def_type = map_get(type_table->ident_to_type, field_ident);
                            if (field_expr.type) {
                                assert(type_equals(field_expr.type, def_type) && "type mismatch");
                            } else {
                                // TODO(Alexander): check that the value conforms to def_type
                            }
                            
                            map_put(field_values, field_ident, field_expr.value);
                        }
                    }
                    
                    for (int i = 0; i < type_table->count; i++) {
                        string_id field_ident = type_table->idents[i];
                        Type* field_type = map_get(type_table->ident_to_type, field_ident);
                        Value field_value = map_get(field_values, field_ident);
                        
                        void* data = interp_push_value(interp, field_type, field_value);
                        if (!base_address) {
                            base_address = data;
                        }
                    }
                    
                    Value value;
                    value.type = Value_pointer;
                    value.data = base_address;
                    void* data = interp_push_value(interp, type, value);
                    
                    Entity entity;
                    entity.data = data;
                    entity.type = type;
                    map_put(interp->symbol_table, ident, entity);
                } break;
                
                case TypeKind_Union: {
                    
                } break;
                
                default: {
                    void* data = interp_push_value(interp, type, expr.value);
                    
                    Entity entity;
                    entity.data = data;
                    entity.type = type;
                    map_put(interp->symbol_table, ident, entity);
                } break;
            }
            
        } break;
        
        case Ast_Expr_Stmt: {
            result = interp_expression(interp, ast->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            result = interp_block(interp, ast->Block_Stmt.stmts);
        } break;
        
        case Ast_Break_Stmt: {
            result.modifier = InterpValueMod_Break;
            result.label = ast->Break_Stmt.ident->Ident;
        } break;
        
        case Ast_Continue_Stmt: {
            result.modifier = InterpValueMod_Continue;
            result.label = ast->Continue_Stmt.ident->Ident;
        } break;
        
        case Ast_Decl_Stmt: {
        } break;
        
        case Ast_If_Stmt: {
            Interp_Value condition = interp_expression(interp, ast->If_Stmt.cond);
            if (is_integer(condition.value)) {
                if (value_to_bool(condition.value)) {
                    result = interp_statement(interp, ast->If_Stmt.then_block);
                } else {
                    result = interp_statement(interp, ast->If_Stmt.else_block);
                }
            } else {
                interp_error(interp, string_lit("type error: expected boolean condition"));
            }
        } break;
        
        case Ast_For_Stmt: {
            interp_statement(interp, ast->For_Stmt.init);
            Interp_Value condition = interp_statement(interp, ast->For_Stmt.cond);
            while (condition.value.type == Value_void || (is_integer(condition.value) && value_to_bool(condition.value))) {
                Interp_Value block = interp_statement(interp, ast->For_Stmt.block);
                if (block.modifier == InterpValueMod_Return) {
                    result = block;
                    break;
                } else if (block.modifier == InterpValueMod_Break) {
                    break;
                }
                
                interp_expression(interp, ast->For_Stmt.update);
                condition = interp_statement(interp, ast->For_Stmt.cond);
            }
            
        } break;
        
        case Ast_While_Stmt: {
        } break;
        
        case Ast_Loop_Stmt: {
        } break;
        
        case Ast_Return_Stmt: {
            result = interp_expression(interp, ast->Return_Stmt.expr);
            result.modifier = InterpValueMod_Return;
        } break;
    }
    
    return result;
}

Interp_Value
interp_block(Interp* interp, Ast* ast) {
    interp->block_depth++;
    
    Interp_Value result = {};
    while (ast->type == Ast_Compound) {
        result = interp_statement(interp, ast->Compound.node);
        ast = ast->Compound.next;
        
        if (result.modifier == InterpValueMod_Return ||
            result.modifier == InterpValueMod_Continue ||
            result.modifier == InterpValueMod_Break) {
            break;
        }
    }
    
    interp->block_depth--;
    return result;
}


internal Type_Table
interp_formal_arguments(Interp* interp, Ast* arguments) {
    Type_Table result = {};
    while (arguments && arguments->type == Ast_Compound) {
        Ast* argument = arguments->Compound.node;
        arguments = arguments->Compound.next;
        if (argument->type == Ast_None) {
            continue;
        }
        assert(argument->type == Ast_Argument);
        
        
        if (!argument->Argument.type) {
            break;
        }
        
        Type* type = interp_type(interp, argument->Argument.type);
        string_id ident = argument->Argument.ident->Ident;
        
        // TODO(Alexander): NEED TO HANDLE THE TYPE TABLE HASH MAP MEMORY
        map_put(result.ident_to_type, ident, type);
        array_push(result.idents, ident);
        result.count++;
    }
    
    return result;
}

Type*
interp_type(Interp* interp, Ast* ast) {
    assert(is_ast_type(ast));
    // TODO(Alexander): right now we store types on the stack, maybe use separate storage?
    // since this will likely get used elsewhere in the system.
    
    Type* result = 0;
    switch (ast->type) {
        case Ast_Named_Type: {
            string_id ident = ast->Named_Type->Ident;
            Type* type = map_get(interp->symbol_table, ident).type;
            if (type) {
                result = type;
            } else {
                interp_unresolved_identifier_error(interp, ident);
            }
        } break;
        
        case Ast_Array_Type: {
            Type* elem_type = interp_type(interp, ast->Array_Type.elem_type);
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Array;
            result->Array.type = elem_type;
            // TODO(Alexander): what is the shape, expression, I assume right now it's an integer?
            Interp_Value capacity = interp_expression(interp, ast->Array_Type.shape); 
            result->Array.capacity = 0;
            if (is_integer(capacity.value)) {
                result->Array.capacity= value_to_smm(capacity.value);
            } else if (!is_void(capacity.value)) {
                interp_error(interp, string_lit("expected integer value"));
            }
            result->cached_size = sizeof(smm)*2;
            result->cached_align = alignof(smm);
        } break;
        
        
        case Ast_Pointer_Type: {
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Pointer;
            result->Pointer = interp_type(interp, ast->Pointer_Type);
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
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Function;
            
            // NOTE(Alexander): Loads in the function arguments
            result->Function.arguments = interp_formal_arguments(interp, ast->Function_Type.arg_types);
            
            result->Function.return_value = interp_type(interp, ast->Function_Type.return_type);
            assert(ast->Function_Type.ident && ast->Function_Type.ident->type == Ast_Ident);
            result->Function.ident = ast->Function_Type.ident->Ident;
            result->cached_size = sizeof(smm);
            result->cached_align = alignof(smm);
        } break;
        
        case Ast_Struct_Type: {
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Struct;
            result->Struct.fields = interp_formal_arguments(interp, ast->Struct_Type.fields);
            
            umm size = 0;
            umm align = 0;
            Type_Table* fields = &result->Struct.fields;
            for (int i = 0; i < fields->count; i++) {
                string_id ident = fields->idents[i];
                Type* type = map_get(fields->ident_to_type, ident);
                assert(type);
                assert(type->cached_size > 0 && "bad size");
                assert(type->cached_align > 0 && "bad align");
                if (type->cached_align > align) {
                    align = type->cached_align;
                }
                
                size = align_forward(size, type->cached_align);
                map_put(result->Struct.ident_to_offset, ident, size);
                size += type->cached_size;
            }
            
            result->cached_size = (s32) size;
            result->cached_align = (s32) align;
        } break;
        
        case Ast_Union_Type: {
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Union;
            result->Union.fields = interp_formal_arguments(interp, ast->Union_Type.fields);
        } break;
        
        case Ast_Enum_Type: {
            // TODO(Alexander): implement this
            assert(0 && "unimplemented");
        } break;
        
        case Ast_Typedef: {
            result = interp_type(interp, ast->Typedef.type);
        } break;
    }
    
    return result;
}

void
interp_register_primitive_types(Interp* interp) {
    for (int i = 0; i < fixed_array_count(global_primitive_types); i++) {
        Type* type = &global_primitive_types[i];
        string_id ident = (string_id) (Kw_int + type->Primitive.kind);
        Entity entity;
        entity.data = 0;
        entity.type = type;
        map_put(interp->symbol_table, ident, entity);
    }
}

void
interp_ast_declarations(Interp* interp, Ast_Decl_Entry* decls) {
    Ast** interp_statements = 0;
    
    for (int i = 0; i < map_count(decls); i++) {
        Ast_Decl_Entry decl = decls[i];
        assert(decl.value->type == Ast_Decl);
        Ast* stmt = decl.value->Decl.stmt;
        
        Entity entity = {};
        if (stmt->type == Ast_Decl_Stmt) {
            Type* type = interp_type(interp, stmt->Decl_Stmt.type);
            if (type->kind == TypeKind_Function) {
                type->Function.block = stmt->Decl_Stmt.decl;;
            }
            entity.data = 0;
            entity.type = type;
            map_put(interp->symbol_table, decl.key, entity);
        }
    }
    
    // HACK(alexander): this should be merged with the loop above,
    // but for the time being we don't want to run any code before injecting the types.
    for (int i = 0; i < map_count(decls); i++) {
        Ast_Decl_Entry decl = decls[i];
        assert(decl.value->type == Ast_Decl);
        Ast* stmt = decl.value->Decl.stmt;
        
        if (stmt->type != Ast_Decl_Stmt) {
            Interp_Value interp_result = interp_statement(interp, stmt);
            if (!is_void(interp_result.value)) {
                void* data= interp_push_value(interp, interp_result.type, interp_result.value);
                if (!data) {
                    Entity entity;
                    entity.data = data;
                    entity.type = interp_result.type;
                    map_put(interp->symbol_table, decl.key, entity);
                }
            }
        }
    }
}

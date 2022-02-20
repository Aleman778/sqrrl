
void
interp_save_value(Interp* interp, Type* type, void* storage, Value value) {
    // TODO(Alexander): handle type errors here
    switch (type->kind) {
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
        case TypeKind_Struct: 
        case TypeKind_Union: {
            *((smm*) storage) = value.pointer;
        } break;
        
        default: {
            assert(0 && "not a value");
        } break;
    }
}

Interp_Value
interp_load_value(Interp* interp, Type* type, void* data) {
    Value value = {};
    
    switch (type->kind) {
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
            if (type->Array.capacity <= 0) {
                smm* mdata = (smm*) data;
                value.array.count = *mdata++;
                value.array.elements = *((void**) mdata);
                value.type = Value_array;
            } else {
                value.array.count = type->Array.capacity;
                value.array.elements = data;
                value.type = Value_array;
            }
        } break;
        
        case TypeKind_String: {
            value.str = *((string*) data);
            value.type = Value_string;
        } break;
        
        case TypeKind_Pointer:
        case TypeKind_Struct:
        case TypeKind_Union: {
            value.pointer = *((smm*) data);
            value.type = Value_pointer;
        } break;
        
        default: {
            assert(0 && "not a value");
        } break;
    }
    
    Interp_Value result = create_interp_value(interp);
    result.value = value;
    result.type = *type;
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
            result = interp_load_value(interp, ast->Ident);
            if (!result.data && result.type.kind == TypeKind_None) {
                interp_error(interp, string_format("`%` is an undeclared identifier", 
                                                   f_string(vars_load_string(ast->Ident))));
            }
        } break;
        
        case Ast_Unary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Unary_Expr.first);
            switch (ast->Unary_Expr.op) {
                case UnaryOp_Negate: {
                    if (is_integer(first_op.value)) {
                        result.value.signed_int = -first_op.value.signed_int;
                        result.value.type = Value_signed_int;
                    } else if(is_floating(first_op.value)) {
                        result.value.floating = -first_op.value.floating;
                        result.value.type = Value_floating;
                    } else {
                        interp_error(interp, string_lit("unary negate expects numeric type"));
                    }
                } break;
                
                case UnaryOp_Not: {
                    if (is_integer(first_op.value)) {
                        result.value.boolean = !value_to_bool(first_op.value);
                        result.value.type = Value_boolean;
                    } else {
                        interp_error(interp, string_lit("unary not expects integer type"));
                    }
                } break;
                
                case UnaryOp_Address_Of: {
                    Ast* expr = ast->Unary_Expr.first;
                    
                    if (expr->type == Ast_Ident) {
                        Interp_Entity entity = interp_load_entity_from_current_scope(interp, expr->Ident);
                        if (entity.is_valid) {
                            
                            if (entity.data && entity.type) {
                                result.value.type = Value_pointer;
                                result.value.data = entity.data;
                                
                                Type type = {};
                                type.kind = TypeKind_Pointer;
                                type.Pointer = entity.type;
                                result.type = type;
                            } else {
                                interp_error(interp, string_format("cannot take address of uninitialized variable `%`",
                                                                   f_string(vars_load_string(expr->Ident))));
                            }
                        } else {
                            interp_error(interp, string_format("`%` is an undeclared identifier", 
                                                               f_string(vars_load_string(expr->Ident))));
                        }
                    } else {
                        interp_error(interp, string_lit("address of operator expects an identifier"));
                    }
                } break;
                
                case UnaryOp_Dereference: {
                    Interp_Value op = interp_expression(interp, ast->Unary_Expr.first);
                    
                    if (op.value.type == Value_pointer && op.type.kind == TypeKind_Pointer) {
                        Type* deref_type = op.type.Pointer;
                        result = interp_load_value(interp, deref_type, op.value.data);
                    } else {
                        interp_error(interp, string_lit("dereference operator expects identifier"));
                    }
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
                interp_mismatched_types(interp, &first_op.type, &second_op.type);
            }
            
            // NOTE(Alexander): handle assign binary expression
            if (result.value.type != Value_void && is_binary_assign(ast->Binary_Expr.op)) {
                
                if (ast->Binary_Expr.first->type == Ast_Ident) {
                    string_id ident = ast->Binary_Expr.first->Ident;
                    Interp_Entity entity = interp_load_entity_from_current_scope(interp, ident);
                    if (interp_entity_is_declared(interp, &entity, ident)) {
                        if (entity.data) {
                            interp_save_value(interp, entity.type, entity.data, result.value);
                        } else {
                            entity.data = interp_push_value(interp, entity.type, result.value);
                        }
                    }
                } else if (first_op.data && first_op.type.kind) {
                    interp_save_value(interp, &first_op.type, first_op.data, first);
                    
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
            result.modifier = InterpValueMod_None; // NOTE(Alexander): avoids returing multiple times
        } break;
        
        case Ast_Field_Expr: {
            Interp_Value var = interp_expression(interp, ast->Field_Expr.var);
            
            assert(ast->Field_Expr.field->type == Ast_Ident); // TODO(Alexander): turn into an error, where?
            string_id ident = ast->Field_Expr.field->Ident;
            
            result = interp_field_expr(interp, var, ident);
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
                        
                        case Value_pointer: {
#define PVALUE pointer
                            PTYPECONV
#undef PVALUE
                        } break;
                        
                        default: {
                            interp_error(interp, string_lit("cannot type cast void type"));
                        } break;
                    }
                    
#undef PVALUE
#undef PTYPECONV
#undef PCASE
                    
                } break;
                
                case TypeKind_Pointer: {
                    
                    if (value.type == Value_array) {
                        result.value.type = Value_pointer;
                        result.value.data = value.array.elements;
                    } else if (value.type != Value_pointer) {
                        interp_error(interp, string_lit("cannot type cast non-pointer value to pointer"));
                    }
                } break;
                
                default: {
                    assert(0 && "unimplemented :(");
                } break;
                
            }
            
            result.type = *type;
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
                        Type* elem_type = array.type.Array.type;
                        smm elem_size = elem_type->cached_size;
                        assert(elem_size > 0 && "not valid array element size");
                        
                        void* data = (void*) array_value.elements;
                        data = (u8*) data + array_index * elem_size;
                        
                        result = interp_load_value(interp, elem_type, data);
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
    
    Interp_Entity decl = interp_load_entity_from_current_scope(interp, ident);
    if (decl.type) {
        
        if (decl.type->kind == TypeKind_Function) {
            Type_Table* formal_arguments = &decl.type->Function.arguments;
            
            // First evaluate and push the arguments on the new scope
            // NOTE: arguments are pushed on the callers stack space
            
            // TODO(Alexander): this is temporary since we don't have variadic argument support yet.
            array(Interp_Value)* variadic_arguments = 0;
            
            Interp_Scope new_scope = {};
            int arg_index = 0;
            if (args) { 
                for_compound(args, expr) {
                    Interp_Value arg = interp_expression(interp, expr);
                    
                    // Only interpret as many args as formally declared
                    if (arg_index < formal_arguments->count) {
                        // TODO(Alexander): assign binary expressions needs to be handled here
                        string_id arg_ident = formal_arguments->idents[arg_index];
                        Type* formal_type = map_get(decl.type->Function.arguments.ident_to_type, arg_ident);
                        
                        // NOTE(Alexander): Check that the type provided as argument matches the formal
                        // type from the function declaration
                        if (!interp_check_type_match_of_value(interp, formal_type, arg)) {
                            return result;
                        }
                        
                        // NOTE(Alexander): we need to allocate it in order to pass it to the function
                        // Currently we only pass variables to functions through references
                        arg.data = interp_push_value(interp, formal_type, arg.value);
                        interp_push_entity_to_scope(&new_scope, arg_ident, arg.data, formal_type);
                        
                    } else if (decl.type->Function.is_variadic) {
                        array_push(variadic_arguments, arg);
                    }
                    
                    arg_index++;
                }
            }
            
            // Store the old base pointer on the stack and set the base pointer
            // to point at the top of the stack, then push the new scope
            smm new_base = interp->stack.curr_used;
            smm* old_base = (smm*) arena_push_size(&interp->stack, sizeof(smm));
            *old_base = interp->base_pointer;
            interp->base_pointer = new_base;
            array_push(interp->scopes, new_scope);
            
            // NOTE(Alexander): secondly push the evaluated arguments on stack
            if (args) {
                if (arg_index != formal_arguments->count && !decl.type->Function.is_variadic) {
                    
                    // NOTE(Alexander): it is allowed to have more arguments only if the function is variadic
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
            
            Ast* block = decl.type->Function.block;
            if (block) {
                result = interp_statement(interp, block);
                // TODO(alexander): only write to result if it is an actual return value!
                
            } else {
                // TODO(Alexander): this is not supposed to ever be the case, maybe assert instead!
                // NOTE(Alexander): what about FFI?
                
                if (decl.type->Function.intrinsic) {
                    result.value = decl.type->Function.intrinsic(interp, variadic_arguments);
                    
                } else {
                    interp_error(interp, string_format("`%` function has no definition and is no intrinsic", f_string(vars_load_string(ident))));
                }
            }
            
            // Pop the scope and free the data stored in the scope
            Interp_Scope old_scope = array_pop(interp->scopes);
            map_free(old_scope.locals);
            array_free(old_scope.local_stack);
            
            // Pop the stack by restoring the old base pointer
            interp->stack.curr_used = interp->base_pointer;
            interp->base_pointer = *(smm*) ((u8*) interp->stack.base + interp->base_pointer);
            
        } else {
            interp_error(interp, string_format("`%` is not a function", f_string(vars_load_string(ident))));
        }
    } else {
        interp_unresolved_identifier_error(interp, ident);
    }
    
    return result;
}

Interp_Value
interp_field_expr(Interp* interp, Interp_Value var, string_id ident) {
    Interp_Value result = {};
    
    switch (var.type.kind) {
        case TypeKind_Union:
        case TypeKind_Struct: {
            assert(var.value.type == Value_pointer);
            Type_Table* type_table = &var.type.Struct_Or_Union;
            
            Type* field_type = map_get(type_table->ident_to_type, ident);
            if (field_type) {
                void* data = var.value.data;
                smm offset = map_get(var.type.Struct_Or_Union.ident_to_offset, ident);
                data = (u8*) data + offset;
                result = interp_load_value(interp, field_type, data);
            } else {
                interp_error(interp, string_format("`%` is not a field of type `%`",
                                                   f_string(vars_load_string(ident)), f_string(var.type.name)));
            }
        } break;
        
        case TypeKind_Enum: {
            result.value = map_get(var.type.Enum.values, ident);
            result.type = *var.type.Enum.type;
        } break;
        
        case TypeKind_Pointer: {
            if (var.value.type == Value_pointer && var.type.kind == TypeKind_Pointer) {
                Type* deref_type = var.type.Pointer;
                var = interp_load_value(interp, deref_type, var.value.data);
                result = interp_field_expr(interp, var, ident);
                
            } else {
                interp_error(interp, string_lit("dereference operator expects identifier"));
            }
        } break;
        
        
        default: {
            interp_error(interp, string_format("left of `.%` must be a pointer, struct, union or enum",
                                               f_string(vars_load_string(ident))));
        } break;
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
            
            {
                // NOTE(Alexander): check that ident isn't already occupied
                Interp_Scope* scope = interp_get_current_scope(interp);
                if (scope && map_get(scope->locals, ident).is_valid) {
                    interp_error(interp, string_format("cannot redeclare previous local variable `%`",
                                                       f_string(vars_load_string(ident))));
                    break;
                }
            }
            
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
                    interp_push_entity_to_current_scope(interp, ident, data, type);
                } break;
                
                case TypeKind_Union:
                case TypeKind_Struct: {
                    
                    Type_Table* type_table = &type->Struct_Or_Union;
                    assert(type_table->count && "empty struct shouln't be possible");
                    
                    void* base_address = arena_push_size(&interp->stack, (umm) type->cached_size, (umm) type->cached_align);
                    
                    
                    if (is_ast_node(expr.value)) {
                        assert(expr.value.type == Value_ast_node &&
                               expr.value.ast->type == Ast_Struct_Expr);
                        
                        // Struct initialization
                        Ast* fields = expr.value.ast->Struct_Expr.fields;
                        struct { string_id key; Value value; }* field_values = 0;
                        
                        // NOTE(Alexander): push elements onto the stack in the order defined by the type
                        // so first push the compound actual values into a auxillary hash map.
                        for_compound(fields, field) {
                            assert(field->type == Ast_Argument);
                            
                            Interp_Value field_expr = interp_expression(interp, field->Argument.assign);
                            
                            assert(field->Argument.ident);
                            string_id field_ident = field->Argument.ident->Ident;
                            
                            // NOTE(Alexander): check that the actual type matches its definition
                            Type* def_type = map_get(type_table->ident_to_type, field_ident);
                            if (type_equals(&field_expr.type, def_type)) {
                                interp_mismatched_types(interp, def_type, &field_expr.type);
                            } else {
                                // TODO(Alexander): check that the value conforms to def_type
                            }
                            
                            map_put(field_values, field_ident, field_expr.value);
                        }
                        
                        // Store the result
                        if (field_values) {
                            for_map(field_values, field, i) {
                                string_id field_ident = field.key;
                                Type* field_type = map_get(type_table->ident_to_type, field_ident);
                                Value field_value = value_cast(field.value, field_type);
                                smm offset = map_get(type_table->ident_to_offset, field_ident);
                                void* storage = (u8*) base_address + offset;
                                interp_save_value(interp, field_type, storage, field_value);
                            }
                        } else {
                            // TODO(Alexander): no fields specified, should we clear the memory maybe?
                            memset((u8*) base_address, 0, (umm) type->cached_size);
                        }
                        
                    } else if (expr.value.type == Value_pointer) {
                        copy_memory(base_address, expr.value.data, (umm) type->cached_size);
                    } else {
                        // TODO(Alexander): for now we will clear entire struct/union memory
                        // we will want the user to be able to disable this behaviour
                        memset((u8*) base_address, 0, (umm) type->cached_size);
                    }
                    
                    Value value;
                    value.type = Value_pointer;
                    value.data = base_address;
                    void* data = interp_push_value(interp, type, value);
                    interp_push_entity_to_current_scope(interp, ident, data, type);
                } break;
                
                default: {
                    void* data = interp_push_value(interp, type, expr.value);
                    interp_push_entity_to_current_scope(interp, ident, data, type);
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
            interp_declaration_statement(interp, ast);
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
            if (condition.value.type == Value_void || is_integer(condition.value)) {
                // NOTE(Alexander): type check on condition
                while (condition.value.type == Value_void || value_to_bool(condition.value)) {
                    Interp_Value block = interp_statement(interp, ast->For_Stmt.block);
                    if (block.modifier == InterpValueMod_Return) {
                        result = block;
                        break;
                    } else if (block.modifier == InterpValueMod_Break) {
                        break;
                    }
                    
                    interp_expression(interp, ast->For_Stmt.update);
                    condition = interp_statement(interp, ast->For_Stmt.cond);
                    if (condition.value.type != Value_void && !is_integer(condition.value)) {
                        interp_error(interp, string_lit("type error: expected boolean condition"));
                        break;
                    }
                }
            } else {
                interp_error(interp, string_lit("type error: expected boolean condition"));
            }
            
        } break;
        
        case Ast_While_Stmt: {
            Interp_Value condition = interp_expression(interp, ast->While_Stmt.cond);
            if (is_integer(condition.value)) {
                while (value_to_bool(condition.value)) {
                    Interp_Value block = interp_statement(interp, ast->While_Stmt.block);
                    if (block.modifier == InterpValueMod_Return) {
                        result = block;
                        break;
                    } else if (block.modifier == InterpValueMod_Break) {
                        break;
                    }
                    
                    condition = interp_expression(interp, ast->While_Stmt.cond);
                    if (!is_integer(condition.value)) {
                        interp_error(interp, string_lit("type error: expected boolean condition"));
                        break;
                    }
                }
            } else {
                interp_error(interp, string_lit("type error: expected boolean condition"));
            }
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
    
    smm local_base_pointer = 0;
    Interp_Scope* current_scope = interp_get_current_scope(interp);
    if (current_scope) {
        local_base_pointer = (smm) array_count(current_scope->local_stack);
    }
    
    Interp_Value result = {};
    while (ast->type == Ast_Compound) {
        result = interp_statement(interp, ast->Compound.node);
        if (interp->error_count) {
            break;
        }
        
        if (result.modifier == InterpValueMod_Return ||
            result.modifier == InterpValueMod_Continue ||
            result.modifier == InterpValueMod_Break) {
            break;
        }
        
        ast = ast->Compound.next;
    }
    
    if (current_scope) {
        // Remove locals declared inside the block scope
        smm take = (smm) array_count(current_scope->local_stack) - local_base_pointer;
        while (take-- > 0) {
            string_id ident = array_pop(current_scope->local_stack);
            map_remove(current_scope->locals, ident);
        }
    }
    
    interp->block_depth--;
    return result;
}

internal Type*
interp_struct_or_union_type(Interp* interp, Ast* arguments, Type_Kind typekind) {
    Type* result = arena_push_struct(&interp->stack, Type);
    result->kind = typekind;
    Type_Table* fields = &result->Struct_Or_Union;
    
    smm offset = 0;
    smm size = 0;
    smm align = 0;
    
    for_compound(arguments, argument) {
        assert(argument->type == Ast_Argument);
        
        if (!argument->Argument.type) {
            break;
        }
        
        // NOTE(Alexander): for unions we reset the size and alignment for each entry
        if (typekind == TypeKind_Union) {
            size = 0;
            align = 0;
        }
        
        Ast* ast_type = argument->Argument.type;
        Type* type = interp_type(interp, ast_type);
        
        switch (argument->Argument.ident->type) {
            case Ast_Compound: {
                unimplemented;
                for_compound(argument->Argument.ident, ast_ident) {
                    assert(ast_ident->type == Ast_Ident);
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
                if (type->kind == TypeKind_Struct) {
                    // NOTE(Alexander): anonymous type, pull out the identifiers from struct
                    if(ast_type->Struct_Type.ident->type == Ast_None) {
                        if (type->Struct_Or_Union.count > 0) {
                            for_map(type->Struct_Or_Union.ident_to_type, other, i) {
                                // TODO(Alexander): check collisions!! we don't want two vars with same identifier
                                map_put(fields->ident_to_type, other.key, other.value);
                                array_push(fields->idents, other.key);
                                fields->count++;
                            }
                            
                            for_map(type->Struct_Or_Union.ident_to_offset, other, j) {
                                map_put(fields->ident_to_offset, other.key, other.value);
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
                        interp_error(interp, string_lit("only anonymous structs can be declared inside a type"));
                    }
                } else if (type->kind == TypeKind_Union) {
                    assert(0 && "unimplemented :(");
                } else {
                    interp_error(interp, string_lit("expected identifier or struct"));
                }
            } break;
            
            case Ast_Ident: {
                assert(argument->Argument.ident->type == Ast_Ident);
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
interp_type(Interp* interp, Ast* ast) {
    assert(is_ast_type(ast));
    // TODO(Alexander): right now we store types on the stack, maybe use separate storage?
    // since this will likely get used elsewhere in the system.
    
    Type* result = 0;
    switch (ast->type) {
        case Ast_Named_Type: {
            string_id ident = ast->Named_Type->Ident;
            if (ident == Kw_string) {
                return &global_string_type;
            } else {
                Interp_Entity entity = interp_load_entity_from_current_scope(interp, ident);
                if (entity.is_valid && entity.type) {
                    result = entity.type;
                } else {
                    interp_unresolved_identifier_error(interp, ident);
                }
            }
        } break;
        
        case Ast_Array_Type: {
            Type* elem_type = interp_type(interp, ast->Array_Type.elem_type);
            // TODO(Alexander): do we wan't to store types on stack?
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
            result->Function.is_variadic = false;
            
            // NOTE(Alexander): Loads in the function arguments
            Ast* ast_arguments = ast->Function_Type.arg_types;
            Type_Table* type_arguments = &result->Function.arguments;
            smm offset = 0;
            for_compound(ast_arguments, ast_argument) {
                assert(ast_argument->type == Ast_Argument);
                if (!ast_argument->Argument.type) {
                    break;
                }
                
                Type* type = interp_type(interp, ast_argument->Argument.type);
                string_id ident = ast_argument->Argument.ident->Ident;
                array_push(type_arguments->idents, ident);
                map_put(type_arguments->ident_to_type, ident, type);
                map_put(type_arguments->ident_to_offset, ident, offset);
                type_arguments->count++;
                
                offset = align_forward(offset, type->cached_align);
                offset += type->cached_size;
                
                map_put(type_arguments->ident_to_offset, ident, offset);
            }
            
            result->Function.return_value = interp_type(interp, ast->Function_Type.return_type);
            assert(ast->Function_Type.ident && ast->Function_Type.ident->type == Ast_Ident);
            result->Function.ident = ast->Function_Type.ident->Ident;
            result->cached_size = sizeof(smm);
            result->cached_align = alignof(smm);
        } break;
        
        case Ast_Struct_Type: {
            result = interp_struct_or_union_type(interp, ast->Struct_Type.fields, TypeKind_Struct);
        } break;
        
        case Ast_Union_Type: {
            result = interp_struct_or_union_type(interp, ast->Union_Type.fields, TypeKind_Union);
        } break;
        
        case Ast_Enum_Type: {
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Enum;
            
            Type* type;
            if (ast->Enum_Type.elem_type && ast->Enum_Type.elem_type->type != Ast_None) {
                type = interp_type(interp, ast->Enum_Type.elem_type);
                if (type->kind != TypeKind_Primitive) {
                    interp_error(interp, string_lit("enums can only be defined as primitive types"));
                    break;
                }
            } else {
                type = &global_primitive_types[PrimitiveTypeKind_s64];
            }
            
            result->Enum.type = type;
            
            Value value;
            if (type->Primitive.signedness) {
                value.type = Value_signed_int;
                value.signed_int = 0;
            } else {
                value.type = Value_unsigned_int;
                value.unsigned_int = 0;
            }
            
            Ast* arguments = ast->Enum_Type.fields;
            while (arguments && arguments->type == Ast_Compound) {
                Ast* argument = arguments->Compound.node;
                arguments = arguments->Compound.next;
                if (argument->type == Ast_None) {
                    continue;
                }
                
                assert(!argument->Argument.type && "enums fields don't have different types, parsing bug");
                
                if (argument->Argument.assign && argument->Argument.assign->type != Ast_None) {
                    value = interp_expression(interp, argument->Argument.assign).value;
                    if (!is_integer(value)) {
                        // TODO(Alexander): show the value in the message
                        interp_error(interp, string_lit("enums only support integer values"));
                        break;
                    }
                    
                    if (type->Primitive.signedness) {
                        value.type = Value_signed_int;
                    } else {
                        value.type = Value_unsigned_int;
                    }
                }
                
                string_id ident = argument->Argument.ident->Ident;
                
                // TODO(Alexander): NEED TO HANDLE THE TYPE TABLE HASH MAP MEMORY
                map_put(result->Enum.values, ident, value);
                
                value.signed_int++;
            }
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
        interp_push_entity_to_current_scope(interp, ident, 0, type);
    }
}

internal Format_Type
convert_type_to_format_type(Type* type) {
    switch (type->kind) {
        case TypeKind_Primitive: {
            switch (type->Primitive.kind) {
#define PRIMITIVE(symbol, ...) case PrimitiveTypeKind_##symbol: return FormatType_##symbol; 
                DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
            }
        } break;
        
        case TypeKind_String: return FormatType_string;
    }
    
    return FormatType_None;
}


internal Format_Type 
convert_value_type_to_format_type(Value_Type type) {
    switch (type) {
        case Value_boolean: return FormatType_bool;
        case Value_signed_int: return FormatType_s64;
        case Value_unsigned_int: return FormatType_u64;
        case Value_floating: return FormatType_f64;
        case Value_pointer: return FormatType_smm;
        //Value_array,
        case Value_string: return FormatType_string;
    }
    
    return FormatType_None;
}

Value
interp_intrinsic_pln(Interp* interp, array(Interp_Value)* var_args) {
    Interp_Value format = interp_load_value(interp, vars_save_cstring("format"));
    
    if (format.value.type == Value_string) {
        
        if (var_args) {
            String_Builder sb = {};
            
            string format_string = format.value.str;
            int format_count = (int) format_string.count;
            u8* scan = (u8*) format_string.data;
            u8* scan_at_prev_percent = scan;
            
            smm index = 0;
            int var_arg_index = 0;
            int count_until_percent = 0;
            while (index < format_count) {
                
                if (*scan == '%') {
                    if (count_until_percent > 0) {
                        string substring = create_string(count_until_percent, scan_at_prev_percent);
                        string_builder_push(&sb, substring);
                        count_until_percent = 0;
                    }
                    scan_at_prev_percent = scan + 1;
                    
                    if (index + 1 < format_count && *(scan + 1) == '%') {
                        string_builder_push(&sb, "%");
                        index += 2;
                        scan += 2;
                        continue;
                    }
                    
                    if (var_arg_index >= array_count(var_args)) {
                        interp_error(interp, string_format(""));
                        string_builder_free(&sb);
                        return {};
                    }
                    
                    Interp_Value* var_arg = var_args + var_arg_index++;
                    Format_Type format_type = convert_type_to_format_type(&var_arg->type);
                    if (format_type == FormatType_None) {
                        // NOTE(Alexander): if type didn't help then we guess based on value
                        format_type = convert_value_type_to_format_type(var_arg->value.type);
                    }
                    
                    void* value_data;
                    if (var_arg->value.type == Value_string) {
                        value_data = &var_arg->value.data;
                    } else {
                        value_data = var_arg->value.data;
                    }
                    string_builder_push_format(&sb, "%", format_type, value_data);
                } else {
                    count_until_percent++;
                }
                
                scan++;
                index++;
            }
            printf("%.*s\n", (int) sb.curr_used, (char*) sb.data);
            string_builder_free(&sb);
        } else {
            pln("%", f_string(format.value.str));
        }
    } else {
        interp_error(interp, string_format("expected `string` as first argument, found `%`", 
                                           f_type(&format.type)));
    }
    
    return {};
}

internal inline void
type_table_push_type(Type_Table* table, string_id ident, Type* type, smm offset) {
    map_put(table->ident_to_type, ident, type);
    map_put(table->ident_to_offset, ident, 0);
    array_push(table->idents, ident);
    table->count++;
}


void
DEBUG_interp_register_intrinsics(Interp* interp) {
    // TODO(Alexander): these are kind of temporary, since we don't really have
    // the ability to create these functions yet, need FFI!
    // We will still have intrinsics but these intrinsics are just for debugging
    
    {
        // pln(string format...)
        // TODO(Alexander): do we wan't to store types on stack?
        Type* type = arena_push_struct(&interp->stack, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = true;
        type->Function.arguments = {};
        
        string_id arg0_ident = vars_save_cstring("format");
        type_table_push_type(&type->Function.arguments, arg0_ident, &global_string_type, 0);
        
        string_id ident = vars_save_cstring("pln");
        type->Function.block = 0;
        type->Function.intrinsic = &interp_intrinsic_pln;
        type->Function.ident = ident;
        interp_push_entity_to_current_scope(interp, ident, 0, type);
    }
}

void
interp_declaration_statement(Interp* interp, Ast* ast) {
    assert(ast->type == Ast_Decl_Stmt);
    
    Type* type = interp_type(interp, ast->Decl_Stmt.type);
    if (type->kind == TypeKind_Function) {
        type->Function.block = ast->Decl_Stmt.decl;
    }
    
    assert(ast->Decl_Stmt.ident->type == Ast_Ident);
    string_id ident = ast->Decl_Stmt.ident->Ident;
    interp_push_entity_to_current_scope(interp, ident, 0, type);
}

void
interp_ast_declarations(Interp* interp, Ast_Decl_Entry* decls) {
    Ast** interp_statements = 0;
    
    for (int i = 0; i < map_count(decls); i++) {
        Ast_Decl_Entry decl = decls[i];
        assert(decl.value->type == Ast_Decl);
        Ast* stmt = decl.value->Decl.stmt;
        
        if (stmt->type == Ast_Decl_Stmt) {
            interp_declaration_statement(interp, stmt);
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
                void* data = interp_push_value(interp, &interp_result.type, interp_result.value);
                if (!data) {
                    interp_push_entity_to_current_scope(interp, decl.key, data, &interp_result.type);
                }
            }
        }
    }
}

bool
interp_check_type_match_of_value(Interp* interp, Type* type, Interp_Value interp_value) {
    Value value = interp_value.value;
    
    bool result = true;
    
    switch (value.type) {
        case Value_void: {
            result = type->kind == TypeKind_Void;
        } break;
        
        case Value_boolean: {
            result = (type->kind == TypeKind_Primitive &&
                      (type->Primitive.kind == PrimitiveTypeKind_bool || 
                       type->Primitive.kind == PrimitiveTypeKind_b32));
        } break;
        
        case Value_signed_int: {
            result = type->kind == TypeKind_Primitive;
            if (!result) break;
            
            switch (type->Primitive.kind) {
                case PrimitiveTypeKind_int:
                case PrimitiveTypeKind_s8:
                case PrimitiveTypeKind_s16:
                case PrimitiveTypeKind_s32:
                case PrimitiveTypeKind_s64:
                case PrimitiveTypeKind_smm:
                case PrimitiveTypeKind_b32: {
                    if (type->Primitive.min_value.signed_int < value.signed_int && 
                        type->Primitive.max_value.signed_int > value.signed_int) {
                        // TODO(Alexander): this is technically a warning!
                        interp_error(interp, string_format("expected type `%` cannot fit in value `%`", 
                                                           f_type(type), f_value(&value)));
                        
                        result = false;
                    }
                    
                } break;
                
                
                case PrimitiveTypeKind_uint:
                case PrimitiveTypeKind_u8:
                case PrimitiveTypeKind_u16:
                case PrimitiveTypeKind_u32:
                case PrimitiveTypeKind_u64:
                case PrimitiveTypeKind_umm: {
                    // TODO(Alexander): this is technically a warning!
                    interp_error(interp, string_format("expected type `%` signed/ unsigned mismatch with `%`", 
                                                       f_type(type), f_value(&value)));
                } break;
            }
            
        } break;
        
        case Value_unsigned_int: {
            result = type->kind == TypeKind_Primitive;
            if (!result) break;
            
            switch (type->Primitive.kind) {
                case PrimitiveTypeKind_uint:
                case PrimitiveTypeKind_u8:
                case PrimitiveTypeKind_u16:
                case PrimitiveTypeKind_u32:
                case PrimitiveTypeKind_u64:
                case PrimitiveTypeKind_umm: {
                    if (type->Primitive.min_value.unsigned_int < value.unsigned_int && 
                        type->Primitive.max_value.unsigned_int > value.unsigned_int) {
                        // TODO(Alexander): this is technically a warning!
                        interp_error(interp, string_format("expected type `%` cannot fit in value `%`", 
                                                           f_type(type), f_value(&value)));
                        
                        result = false;
                    }
                    
                } break;
                
                case PrimitiveTypeKind_int:
                case PrimitiveTypeKind_s8:
                case PrimitiveTypeKind_s16:
                case PrimitiveTypeKind_s32:
                case PrimitiveTypeKind_s64:
                case PrimitiveTypeKind_smm:
                case PrimitiveTypeKind_b32: {
                    // TODO(Alexander): this is technically a warning!
                    interp_error(interp, string_format("expected type `%` signed/ unsigned mismatch with `%`", 
                                                       f_type(type), f_value(&value)));
                } break;
            }
            
        } break;
        
        case Value_floating: {
            // TODO(Alexander): can we also track the precision and report error on mismatch?
            result = (type->kind == TypeKind_Primitive &&
                      (type->Primitive.kind == PrimitiveTypeKind_f32 || 
                       type->Primitive.kind == PrimitiveTypeKind_f64));
        } break;
        
        case Value_pointer:
        case Value_array: {
            // NOTE(Alexander): pointer and array values always assumes that it provides a valid type
            result = type_equals(type, &interp_value.type);
        } break;
        
        case Value_string: {
            result = type->kind == TypeKind_String;
        } break;
        
        case Value_ast_node: {
            compiler_bug("It shouldn't be possible to use ast node as a type");
        } break;
    }
    
    if (!result) {
        interp_error(interp, string_format("expected type `%` is not compatible with `%`", 
                                           f_type(type), f_value(&value)));
    }
    
    return result;
}

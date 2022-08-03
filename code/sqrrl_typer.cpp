
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

// NOTE(Alexander): forward declare
Type* create_type_from_ast(Type_Context* tcx, Ast* ast, bool report_error);

Type*
type_infer_value(Value value) {
    switch (value.type) {
        case Value_void: return &global_void_type;
        case Value_boolean: return &global_primitive_types[PrimitiveType_bool];
        case Value_signed_int: return &global_primitive_types[PrimitiveType_int];
        case Value_unsigned_int: return &global_primitive_types[PrimitiveType_bool];
        case Value_floating: return &global_primitive_types[PrimitiveType_f64];
        
        // TODO(Alexander): complex types require potentially building new types
        //case Value_pointer: 
        //case Value_array:
        
        case Value_string: return &global_string_type;
        
        default: {
            assert(0 && "invalid type");
        } break;
    }
    
    return 0;
}

bool
type_check_value(Type_Context* tcx, Type* type, Value value) {
    bool result = true;
    
    switch (value.type) {
        case Value_void: {
            result = type->kind == Type_Void;
        } break;
        
        case Value_boolean: {
            result = (type->kind == Type_Primitive &&
                      type->Primitive.archetype == Primitive_Int);
            
            // TODO(Alexander): this is maybe too restrictive,
            // we want to be able to implicitly cast bool to int but not vice versa
            //result = (type->kind == Type_Primitive &&
            //(type->Primitive.kind == PrimitiveType_bool || 
            //type->Primitive.kind == PrimitiveType_b32));
        } break;
        
        case Value_signed_int: {
            result = type->kind == Type_Primitive;
            if (!result) break;
            
            switch (type->Primitive.kind) {
                case PrimitiveType_int:
                case PrimitiveType_s8:
                case PrimitiveType_s16:
                case PrimitiveType_s32:
                case PrimitiveType_s64:
                case PrimitiveType_smm:
                case PrimitiveType_b32: {
                    if (value.data.signed_int < type->Primitive.min_value.signed_int ||
                        value.data.signed_int > type->Primitive.max_value.signed_int) {
                        
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
                                                        f_type(type), f_value(&value)));
                    }
                    
                } break;
                
                
                case PrimitiveType_uint:
                case PrimitiveType_u8:
                case PrimitiveType_u16:
                case PrimitiveType_u32:
                case PrimitiveType_u64:
                case PrimitiveType_umm: {
                    if (value.data.signed_int < 0) {
                        type_warning(tcx, string_format("expected type `%` signed/ unsigned mismatch with `%`", 
                                                        f_type(type), f_value(&value)));
                        
                    } else if (value.data.unsigned_int > type->Primitive.max_value.unsigned_int) {
                        
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
                                                        f_type(type), f_value(&value)));
                    }
                    
                } break;
            }
            
        } break;
        
        case Value_unsigned_int: {
            result = type->kind == Type_Primitive;
            if (!result) break;
            
            switch (type->Primitive.kind) {
                case PrimitiveType_uint:
                case PrimitiveType_u8:
                case PrimitiveType_u16:
                case PrimitiveType_u32:
                case PrimitiveType_u64:
                case PrimitiveType_umm: {
                    if (value.data.unsigned_int < type->Primitive.min_value.unsigned_int ||
                        value.data.unsigned_int > type->Primitive.max_value.unsigned_int) {
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
                                                        f_type(type), f_value(&value)));
                    }
                    
                } break;
                
                case PrimitiveType_int:
                case PrimitiveType_s8:
                case PrimitiveType_s16:
                case PrimitiveType_s32:
                case PrimitiveType_s64:
                case PrimitiveType_smm:
                case PrimitiveType_b32: {
                    if (value.data.unsigned_int > (u64) type->Primitive.max_value.signed_int) {
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
                                                        f_type(type), f_value(&value)));
                    }
                } break;
            }
            
        } break;
        
        case Value_floating: {
            result = (type->kind == Type_Primitive &&
                      (type->Primitive.kind == PrimitiveType_f32 || 
                       type->Primitive.kind == PrimitiveType_f64));
        } break;
        
        case Value_pointer:
        case Value_array: {
            // TODO(Alexander): do we need more stricter type checking of each value?
            result = type->kind == Type_Array;
        } break;
        
        case Value_string: {
            result = type->kind == Type_String;
        } break;
        
        case Value_ast_node: {
            compiler_bug("It shouldn't be possible to use ast node as a type");
        } break;
    }
    
    if (!result) {
        type_error(tcx, string_format("expected type `%` is not compatible with `%`", 
                                      f_type(type), f_value(&value)));
    }
    
    return result;
}

// TODO(Alexander): this is a compile opt technique, this belongs in different file
Value
constant_folding_of_expressions(Ast* ast) {
    assert(is_ast_expr(ast) || ast->kind == Ast_Value || ast->kind == Ast_Ident || ast->kind == Ast_None);
    Value result = {};
    
    switch (ast->kind) {
        // TODO(alexander): do we want values to be expressions?
        case Ast_Value: {
            result = ast->Value.value;
        } break;
        
        case Ast_Unary_Expr: {
            Value first = constant_folding_of_expressions(ast->Unary_Expr.first);
            if (!is_void(first)) {
                switch (ast->Unary_Expr.op) {
                    case UnaryOp_Negate: {
                        if (is_integer(first)) {
                            result.data.signed_int = -first.data.signed_int;
                            result.type = Value_signed_int;
                        } else if(is_floating(first)) {
                            result.data.floating = -first.data.floating;
                            result.type = Value_floating;
                        }
                    } break;
                    
                    case UnaryOp_Logical_Not: {
                        if (is_integer(first)) {
                            result.data.boolean = !value_to_bool(first);
                            result.type = Value_boolean;
                        }
                    } break;
                }
            }
        } break;
        
        case Ast_Binary_Expr: {
            
            if (is_binary_assign(ast->Binary_Expr.op)) {
                // NOTE(Alexander): we cannot constant fold lhs of an assignemnt operator
                constant_folding_of_expressions(ast->Binary_Expr.second);
            } else {
                Value first = constant_folding_of_expressions(ast->Binary_Expr.first);
                Value second = constant_folding_of_expressions(ast->Binary_Expr.second);
                
                if (!is_void(first) && !is_void(second)) {
                    
                    // NOTE(Alexander): Type rules
                    // int + int;
                    // float + int -> float + float;
                    // int + float -> float + float;
                    // float -x-> int (is a no no, it has to be an explicit cast)
                    // X == X -> bool (where == is any valid comparison operator)
                    
                    if (is_floating(first) || is_floating(second)) {
                        // NOTE(Alexander): Make sure both types are floating
                        if (is_integer(first)) {
                            first.data.floating  = value_to_f64(first);
                            first.type = Value_floating;
                        } else if (is_integer(second)) {
                            second.data.floating = value_to_f64(second);
                            second.type = Value_floating;
                        }
                        
                        first = value_floating_binary_operation(first, second, ast->Binary_Expr.op);
                        result = first;
                        
                    } else if (is_integer(first) || is_integer(second)) {
                        // NOTE(Alexander): integer math
                        first.data.signed_int = value_integer_binary_operation(first, second, ast->Binary_Expr.op);
                        
                        result = first;
                        
                        if (binary_is_comparator(ast->Binary_Expr.op)) {
                            // Comparison operands always results in boolean
                            result.type = Value_boolean;
                        }
                    }
                }
            }
        } break;
        
        case Ast_Ternary_Expr: {
            Value first = constant_folding_of_expressions(ast->Ternary_Expr.first);
            Value second = constant_folding_of_expressions(ast->Ternary_Expr.second);
            Value third = constant_folding_of_expressions(ast->Ternary_Expr.third);
            
            if (is_integer(first)) {
                if (first.data.boolean) {
                    result = second;
                } else {
                    result = third;
                }
            }
        } break;
    }
    
    if (result.type != Value_void) {
        ast->kind = Ast_Value;
        ast->Value.value = result;
        ast->type = 0;
    }
    
    return result;
}

// NOTE(Alexander): parent_type is used to coerce a non-typed literal to a specific type
Type*
type_infer_expression(Type_Context* tcx, Ast* expr, Type* parent_type, bool report_error) {
    Type* result = 0;
    
    switch (expr->kind) {
        case Ast_None: {
            result = &global_void_type;
        } break;
        
        
        case Ast_Value: {
            
            if (parent_type) {
                result = parent_type;
            } else {
                
                result = expr->type;
                if (!result) {
                    result = type_infer_value(expr->Value.value);
                }
            }
            
            type_check_value(tcx, result, expr->Value.value);
            expr->type = result;
        } break;
        
        case Ast_Ident: {
            
            string_id ident = ast_unwrap_ident(expr);
            Type* type = map_get(tcx->locals, ident);
            //pln("%: %", f_string(vars_load_string(ident)), f_type(type));
            if (type) {
                result = type;
            } else {
                type = map_get(tcx->globals, ident);
                
                if (type) {
                    result = type;
                    
                } else if (report_error) {
                    // NOTE(Alexander): copypasta
                    type_error(tcx, string_format("`%` is an undeclared identifier", 
                                                  f_string(vars_load_string(expr->Ident))));
                }
            }
            expr->type = result;
        } break;
        
        case Ast_Unary_Expr: {
            Type* type = type_infer_expression(tcx, expr->Unary_Expr.first, parent_type, report_error);
            
            if (type) {
                switch (expr->Unary_Expr.op) {
                    case UnaryOp_Dereference: {
                        if (type->kind == Type_Pointer) {
                            result = type->Pointer;
                        } else {
                            if (report_error) {
                                type_error(tcx, string_format("cannot dereference type `%`", f_type(type)));
                            }
                        }
                    } break;
                    
                    case UnaryOp_Address_Of: {
                        result = arena_push_struct(tcx->type_arena, Type);
                        result->kind = Type_Pointer;
                        result->Pointer = type;
                    } break;
                    
                    default: {
                        result = type;
                    } break;
                }
                
                expr->type = result;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Type* first_type = type_infer_expression(tcx, 
                                                     expr->Binary_Expr.first, 
                                                     parent_type,
                                                     report_error);
            Type* second_type = type_infer_expression(tcx, 
                                                      expr->Binary_Expr.second, 
                                                      parent_type,
                                                      report_error);
            
            if (first_type && second_type) {
                if (binary_is_comparator_table[expr->Binary_Expr.op]) {
                    result = &global_primitive_types[PrimitiveType_bool];
                } else {
                    result = first_type;
                }
                
                expr->type = result;
            }
        } break;
        
        case Ast_Call_Expr: {
            Type* function_type = type_infer_expression(tcx, 
                                                        expr->Call_Expr.ident,
                                                        parent_type,
                                                        report_error);
            
            if (function_type && function_type->kind == Type_Function) {
                expr->Call_Expr.function_type = function_type;
                result = function_type->Function.return_type;
                expr->type = result;
                
                // Arguments
                // TODO(Alexander): nice to have: support for keyworded args, default args
                Ast* actual_args = expr->Call_Expr.args;
                Type_Table* formal_args = &function_type->Function.arguments;
                s32 arg_index = 0;
                for_compound(actual_args, actual_arg) {
                    Type* formal_type = 0;
                    
                    if (arg_index < array_count(formal_args->idents)) {
                        string_id ident = formal_args->idents[arg_index];
                        formal_type = map_get(formal_args->ident_to_type, ident);
                    }
                    
                    constant_folding_of_expressions(actual_arg->Argument.assign);
                    Type* actual_type = type_infer_expression(tcx, 
                                                              actual_arg->Argument.assign, 
                                                              formal_type, 
                                                              report_error);
                    if (actual_type) {
                        actual_arg->type = actual_type;
                    } else {
                        result = 0;
                        break;
                    }
                    
                    arg_index++;
                }
            }
        } break;
        
        case Ast_Cast_Expr: {
            type_infer_expression(tcx, expr->Cast_Expr.expr, parent_type, report_error);
            result = create_type_from_ast(tcx, expr->Cast_Expr.type, report_error);
            expr->type = result;
        } break;
        
        case Ast_Paren_Expr: {
            result = type_infer_expression(tcx, expr->Paren_Expr.expr, parent_type, report_error);
        } break;
        
        case Ast_Index_Expr: {
            type_infer_expression(tcx, expr->Index_Expr.index, parent_type, report_error);
            Type* type = type_infer_expression(tcx, expr->Index_Expr.array, parent_type, report_error);
            if (type) {
                if (type->kind == Type_Array) {
                    result = type->Array.type;
                } else {
                    if (report_error) {
                        type_error(tcx, string_lit("index operator expects an array"));
                    }
                }
                expr->type = result;
            }
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
    
    return result;
}

internal Type*
create_struct_or_union_type_from_ast(Type_Context* tcx, 
                                     Ast* arguments, 
                                     Type_Kind typekind, 
                                     bool report_error) {
    Type* result = arena_push_struct(tcx->type_arena, Type);
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
        Type* type = create_type_from_ast(tcx, ast_type, report_error);
        
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
create_type_from_ast(Type_Context* tcx, Ast* ast, bool report_error) {
    assert(is_ast_type(ast));
    
    Type* result = 0;
    switch (ast->kind) {
        case Ast_Named_Type: {
            string_id ident = ast->Named_Type->Ident;
            if (is_builtin_type_keyword(ident)) {
                if (ident == Kw_void) {
                    result = &global_void_type;
                } else if (ident == Kw_string) {
                    result = &global_string_type;
                } else if (ident == Kw_infer) {
                    unimplemented;
                } else {
                    int index = ident - Kw_int;
                    assert(index >= 0 && index < fixed_array_count(global_primitive_types));
                    result = &global_primitive_types[index];
                }
            } else {
                result = map_get(tcx->local_type_table, ident);
                if (!result) {
                    result = map_get(tcx->global_type_table, ident);
                    if (!result) {
                        // NOTE(Alexander): copypasta
                        type_error(tcx, string_format("`%` is an undeclared identifier", 
                                                      f_string(vars_load_string(ident))));
                    }
                } 
            }
        } break;
        
        case Ast_Array_Type: {
            Type* elem_type = create_type_from_ast(tcx, ast->Array_Type.elem_type, report_error);
            result = arena_push_struct(tcx->type_arena, Type);
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
            // TODO(Alexander): do we really want to store a new type declaration
            // for each time we use a pointer?!???
            result = arena_push_struct(tcx->type_arena, Type);
            result->kind = Type_Pointer;
            result->Pointer = create_type_from_ast(tcx, ast->Pointer_Type, report_error);
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
            result = arena_push_struct(tcx->type_arena, Type);
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
                
                Type* type = create_type_from_ast(tcx, ast_argument->Argument.type, report_error);
                string_id ident = ast_argument->Argument.ident->Ident;
                array_push(type_arguments->idents, ident);
                map_put(type_arguments->ident_to_type, ident, type);
                map_put(type_arguments->ident_to_offset, ident, offset);
                type_arguments->count++;
                
                offset = align_forward(offset, type->cached_align);
                offset += type->cached_size;
                
                map_put(type_arguments->ident_to_offset, ident, offset);
            }
            
            result->Function.return_type = create_type_from_ast(tcx, 
                                                                ast->Function_Type.return_type,
                                                                report_error);
            assert(ast->Function_Type.ident && ast->Function_Type.ident->kind == Ast_Ident);
            result->Function.ident = ast_unwrap_ident(ast->Function_Type.ident);
            result->cached_size = sizeof(smm);
            result->cached_align = alignof(smm);
        } break;
        
        case Ast_Struct_Type: {
            result = create_struct_or_union_type_from_ast(tcx, 
                                                          ast->Struct_Type.fields, 
                                                          Type_Struct, 
                                                          report_error);
        } break;
        
        case Ast_Union_Type: {
            result = create_struct_or_union_type_from_ast(tcx, 
                                                          ast->Union_Type.fields,
                                                          Type_Union, 
                                                          report_error);
        } break;
        
        case Ast_Enum_Type: {
            result = arena_push_struct(tcx->type_arena, Type);
            result->kind = Type_Enum;
            
            Type* type;
            if (ast->Enum_Type.elem_type && ast->Enum_Type.elem_type->kind != Ast_None) {
                type = create_type_from_ast(tcx, ast->Enum_Type.elem_type, report_error);
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
            if (ast->Typedef.ident->kind == Ast_Compound) {
                // NOTE(Alexander): c crazyiess
                unimplemented;
            } else {
                string_id ident = ast_unwrap_ident(ast->Typedef.ident);
                
                // TODO(Alexander): we don't care about locality of typedefs at the moment (always global)
                result = map_get(tcx->global_type_table, ident);
                
                if (!result) {
                    result = create_type_from_ast(tcx, ast->Typedef.type, report_error);
                    map_put(tcx->global_type_table, ident, result);
                } else {
                    type_error(tcx, string_format("`%` is already defined"));
                }
            }
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

Type*
type_infer_statement(Type_Context* tcx, Ast* stmt, bool report_error) {
    Type* result = 0;
    
    switch (stmt->kind) {
        
        case Ast_Assign_Stmt: {
            // Get rid of unnecessary stuff
            constant_folding_of_expressions(stmt->Assign_Stmt.expr);
            
            Type* expected_type = create_type_from_ast(tcx, stmt->Assign_Stmt.type, report_error);
            if (!expected_type) {
                return result;
            }
            
            Type* found_type = type_infer_expression(tcx, 
                                                     stmt->Assign_Stmt.expr, 
                                                     expected_type,
                                                     report_error);
            
            if (found_type) {
                if (expected_type) {
                    stmt->type = expected_type;
                } else {
                    stmt->type = found_type;
                }
                result = found_type;
                type_check_assignment(tcx, expected_type, found_type);
                
                string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
                
                if (tcx->block_depth > 0) {
                    if (map_get(tcx->locals, ident)) {
                        result = 0;
                        if (report_error) {
                            type_error(tcx,
                                       string_format("cannot redeclare previous local variable `%`",
                                                     f_string(vars_load_string(ident))));
                        }
                    } else {
                        map_put(tcx->locals, ident, expected_type);
                    }
                    
                } else {
                    
                    if (map_get(tcx->globals, ident) && report_error) {
                        result = 0;
                        if (report_error) {
                            type_error(tcx,
                                       string_format("cannot redeclare previous declaration `%`",
                                                     f_string(vars_load_string(ident))));
                        }
                    } else {
                        map_put(tcx->globals, ident, expected_type);
                    }
                }
            }
            
        } break;
        
        case Ast_Expr_Stmt: {
            result = type_infer_expression(tcx, stmt->Expr_Stmt, 0, report_error);
        } break;
        
        case Ast_Block_Stmt: {
            // TODO(Alexander): create a new scope
            Ast* stmts = stmt->Block_Stmt.stmts;
            tcx->block_depth++;
            for_compound(stmts, it) {
                result = type_infer_statement(tcx, it, report_error);
                if (!result) {
                    break;
                }
            }
            tcx->block_depth--;
        } break;
        
        case Ast_Decl_Stmt: {
            string_id ident = ast_unwrap_ident(stmt->Decl_Stmt.ident);
            Type* expected_type = create_type_from_ast(tcx, stmt->Decl_Stmt.type, report_error);
            Type* found_type = type_infer_statement(tcx, stmt->Decl_Stmt.stmt, report_error);
            
            if (expected_type && found_type) {
                if (expected_type->kind == Type_Function) {
                    expected_type->Function.block = stmt->Decl_Stmt.stmt;
                }
                
                
                result = expected_type;
                stmt->type = result;
                map_put(tcx->locals, ident, result);
            }
        } break;
        
        case Ast_If_Stmt: {
            Type* cond = type_infer_expression(tcx, 
                                               stmt->If_Stmt.cond,
                                               &global_primitive_types[PrimitiveType_bool],
                                               report_error);
            Type* then_block = type_infer_statement(tcx, stmt->If_Stmt.then_block, report_error);
            if (is_ast_stmt(stmt->If_Stmt.else_block)) {
                Type* else_block = type_infer_statement(tcx, stmt->If_Stmt.else_block, report_error);
                if (cond && then_block && else_block) {
                    result = cond;
                }
            } else {
                if (cond && then_block) {
                    result = cond;
                }
            }
        } break;
        
        case Ast_For_Stmt: {
            Type* init = type_infer_statement(tcx, stmt->For_Stmt.init, report_error);
            Type* cond = type_infer_expression(tcx, 
                                               stmt->For_Stmt.cond,
                                               &global_primitive_types[PrimitiveType_bool],
                                               report_error);
            Type* update = type_infer_expression(tcx, stmt->For_Stmt.update, 0, report_error);
            Type* block = type_infer_statement(tcx, stmt->For_Stmt.block, report_error);
            
            if (init && cond && update && block) {
                result = init;
            }
        } break;
        
        case Ast_While_Stmt: {
            Type* cond = type_infer_expression(tcx, 
                                               stmt->While_Stmt.cond,
                                               &global_primitive_types[PrimitiveType_bool],
                                               report_error);
            Type* block = type_infer_statement(tcx, stmt->While_Stmt.block, report_error);
            
            if (cond && block) {
                result = cond;
            }
        } break;
        
        case Ast_Return_Stmt: {
            result = type_infer_expression(tcx, stmt->Return_Stmt.expr, 0, report_error);
        } break;
    }
    
    return result;
}

bool
type_check_ast(Type_Context* tcx, Compilation_Unit* comp_unit, bool report_error) {
    bool result = true;
    Ast* ast = comp_unit->ast;
    
    
    if (ast->kind == Ast_Decl_Stmt) {
        Type* type = ast->Decl_Stmt.type->type;
        ast->type = type;
        
        if (type && type->kind == Type_Function) {
            type->Function.block = ast->Decl_Stmt.stmt;
            
            // Store the arguments in local context
            Type_Table* args = &type->Function.arguments;
            if (args->idents) {
                for_array_v(args->idents, ident, _) {
                    Type* arg_type = map_get(args->ident_to_type, ident);
                    if (arg_type) {
                        map_put(tcx->locals, ident, arg_type);
                    }
                }
            }
            
            tcx->return_type = type->Function.return_type;
        }
    }
    
    if (is_ast_type(ast)) {
        string_id ident = comp_unit->ident;
        
        Type* type = create_type_from_ast(tcx, ast, report_error);
        if (type) {
            ast->type = type;
            map_put(tcx->globals, comp_unit->ident, type);
        } else {
            result = false;
        }
        
    } else if (is_ast_stmt(ast)) {
        Type* type = type_infer_statement(tcx, ast, report_error);
        if (type) {
            // Check the types
        } else {
            // NOTE(Alexander): we are missing type info somewhere fail and retry later
            result = false;
        }
    } else {
        assert(0 && "illegal type: expected X_Stmt or any X_Type node");
    }
    
    map_free(tcx->locals);
    tcx->block_depth = 0;
    
    return result;
}


void
DEBUG_setup_intrinsic_types(Type_Context* tcx) {
    // TODO(Alexander): these are kind of temporary, since we don't really have
    // the ability to create these functions yet, need FFI!
    // We will still have intrinsics but these intrinsics are just for debugging
    
    {
        // Intrinsic syntax: pln(string format...)
        // e.g. pln("hello %, lucky number is %", "world", 7);
        Type* type = arena_push_struct(tcx->type_arena, Type);
        type->kind = Type_Function;
        type->Function.is_variadic = true;
        type->Function.arguments = {};
        
        string_id arg0_ident = vars_save_cstring("format");
        type_table_push_type(&type->Function.arguments, arg0_ident, &global_string_type, 0);
        
        string_id ident = vars_save_cstring("pln");
        type->Function.block = 0;
        type->Function.interp_intrinsic = &interp_intrinsic_pln;
        type->Function.intrinsic = &print_format;
        type->Function.ident = ident;
        type->Function.return_type = &global_void_type;
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax: debug_break()
        // Inserts a breakpoint (e.g. int3 on x64) to enable debugger
        Type* type = arena_push_struct(tcx->type_arena, Type);
        type->kind = Type_Function;
        type->Function.is_variadic = false;
        type->Function.arguments = {};
        
        string_id ident = vars_save_cstring("debug_break");
        type->Function.block = 0;
        type->Function.interp_intrinsic = &interp_intrinsic_debug_break;
        type->Function.intrinsic = &interp_intrinsic_debug_break;
        type->Function.ident = ident;
        type->Function.return_type = &global_void_type;
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax: assert(int expr)
        // Assets that expr is true, used as test case for 
        Type* type = arena_push_struct(tcx->type_arena, Type);
        type->kind = Type_Function;
        type->Function.is_variadic = false;
        type->Function.arguments = {};
        
        string_id arg0_ident = vars_save_cstring("expr");
        type_table_push_type(&type->Function.arguments, arg0_ident, &global_primitive_types[PrimitiveType_int], 0);
        
        string_id ident = vars_save_cstring("assert");
        type->Function.block = 0;
        type->Function.interp_intrinsic = &interp_intrinsic_assert;
        type->Function.intrinsic = &intrinsic_assert;
        type->Function.ident = ident;
        type->Function.return_type = &global_void_type;
        
        map_put(tcx->globals, ident, type);
    }
}

s32
type_check_ast_file(Ast_File* ast_file) {
    Type_Context tcx = {};
    array(Compilation_Unit)* queue = 0;
    
    
    // NOTE(Alexander): store the actual type declarations globally
    Memory_Arena global_type_arena = {};
    tcx.type_arena = &global_type_arena;
    
    DEBUG_setup_intrinsic_types(&tcx);
    
    // First push decl statements and type declarations
    for_map(ast_file->decls, it) {
        string_id ident = it->key;
        Ast* decl = it->value;
        Compilation_Unit comp_unit = {};
        comp_unit.ident = ident;
        
        //pln("Push decl `%`", f_string(vars_load_string(ident)));
        
        if (decl->kind == Ast_Decl_Stmt) {
            comp_unit.ast = decl->Decl_Stmt.type;
        } else {
            comp_unit.ast = decl;
        }
        
        array_push(queue, comp_unit);
    }
    
    // NOTE(Alexander): push actual declarations, makes sure we first try processing the types
    //                  before the actual declaration
    for_map(ast_file->decls, it) {
        string_id ident = it->key;
        Ast* decl = it->value;
        
        if (decl->kind == Ast_Decl_Stmt) {
            Compilation_Unit comp_unit = {};
            comp_unit.ast = decl;
            array_push(queue, comp_unit);
        }
    }
    
    // NOTE(Alexander): exit condition: assumes that when all items in queue failed then we can
    //                  nolonger process any further and have to report errors and exit.
    smm last_queue_count = 0;
    while (last_queue_count != array_count(queue)) {
        smm queue_count = array_count(queue);
        for (smm queue_index = 0; queue_index < queue_count; queue_index++) {
            Compilation_Unit comp_unit = queue[queue_index];
            
            if (!type_check_ast(&tcx, &comp_unit, false)) {
                // Failed, retry later
                array_push(queue, comp_unit);
            }
        }
        
        // Remove processed nodes in queue
        array_remove_n(queue, 0, queue_count);
        last_queue_count = queue_count;
    }
    
    // NOTE(Alexander): anything left in the queue we report errors for
    for_array (queue, comp_unit, _) {
        type_check_ast(&tcx, comp_unit, true);
    }
    
    return tcx.error_count;
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
    Type* first_type = typer_infer_expression(first);
    Type* second_type = typer_infer_expression(second);
    
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

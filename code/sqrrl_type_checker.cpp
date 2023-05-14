
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

Type*
normalize_basic_types(Type* type) {
    if (!type) {
        return type;
    }
    
    if (type->kind != TypeKind_Basic) {
        if (type->kind == TypeKind_Pointer) {
            type->Pointer = normalize_basic_types(type->Pointer);
        }
        
        return type;
    }
    
    switch (type->Basic.kind) {
        case Basic_int: return t_s32; break; // TODO(Alexander): this should maybe be something else or customizable
        case Basic_uint: return t_u32; break; // TODO(Alexander): this should maybe be something else or customizable
        case Basic_smm: return t_s64; break; // TODO(Alexander): architecture dependant
        case Basic_umm: return t_u64; break; // TODO(Alexander): architecture dependant
    }
    
    return type;
} 

Type*
type_infer_value(Type_Context* tcx, Value value) {
    switch (value.type) {
        case Value_void: return t_void;
        case Value_boolean: return t_bool;
        
        case Value_signed_int: {
            s64 val = value_to_s64(value);
            return (val < S32_MIN || val > S32_MAX) ? t_s64 : t_int;
        } break;
        
        case Value_unsigned_int: {
            u64 val = value_to_u64(value);
            return (val > U32_MAX) ? t_u64 : t_uint;
        } break;
        
        case Value_floating: return t_f64;
        
        // TODO(Alexander): complex types require potentially building new types
        //case Value_pointer: 
        //case Value_array:
        
        case Value_string: {
#if 0 // TODO(Alexander): maybe string literals should be u8 arrays, for now it's easier to use t_string
            umm char_count = value.data.str.count;
            Type* result = arena_push_struct(&tcx->type_arena, Type);
            result->kind = TypeKind_Array;
            result->Array.type = t_u8;
            result->Array.capacity = (smm) char_count;
            result->size = (s32) char_count;
            result->align = 1;
            return result;
#endif
            return t_string;
        } break;
        
        default: {
            assert(0 && "invalid type");
        } break;
    }
    
    return 0;
}

bool
type_check_value(Type_Context* tcx, Type* type, Value value, Span span, bool report_error) {
    bool result = true;
    
    if (type->kind == TypeKind_Enum) {
        type = type->Enum.type;
        assert(type);
        //if (!type) {
        //type = t_s64; // TODO(Alexander): what is the default type for Enums?
        //} 
    }
    
    switch (value.type) {
        case Value_void: {
            result = type->kind == TypeKind_Void;
        } break;
        
        case Value_boolean: {
            result = (type->kind == TypeKind_Basic &&
                      is_bitflag_set(type->Basic.flags, BasicFlag_Integer));
            
            // TODO(Alexander): this is maybe too restrictive,
            // we want to be able to implicitly cast bool to int but not vice versa
            //result = (type->kind == Type_Basic &&
            //(type->Basic.kind == Basic_bool || 
            //type->Basic.kind == Basic_b32));
        } break;
        
        case Value_signed_int: {
            if (type->kind == TypeKind_Pointer) {
                result = true;
                break;
            }
            
            result = type->kind == TypeKind_Basic;
            if (!result) break;
            
            switch (type->Basic.kind) {
                case Basic_int:
                case Basic_s8:
                case Basic_s16:
                case Basic_s32:
                case Basic_s64:
                case Basic_smm: {
                    if (value.data.signed_int < type->Basic.limits.min_value ||
                        value.data.signed_int > type->Basic.limits.max_value) {
                        
                        type_warning(tcx, string_print("expected type `%` cannot fit in value `%`", 
                                                       f_type(type), f_value(&value)), span);
                    }
                } break;
                
                
                case Basic_uint:
                case Basic_u8:
                case Basic_u16:
                case Basic_u32:
                case Basic_u64:
                case Basic_umm: {
                    if (value.data.signed_int < 0) {
                        type_warning(tcx, string_print("expected type `%` signed/ unsigned mismatch with `%`", 
                                                       f_type(type), f_value(&value)), span);
                        
                    } else if (value.data.unsigned_int > type->Basic.limits.max_unsigned_value) {
                        
                        type_warning(tcx, string_print("expected type `%` cannot fit in value `%`", 
                                                       f_type(type), f_value(&value)), span);
                    }
                    
                } break;
            }
            
        } break;
        
        case Value_unsigned_int: {
            if (type->kind == TypeKind_Pointer) {
                result = true;
                break;
            }
            
            result = type->kind == TypeKind_Basic;
            if (!result) break;
            
            switch (type->Basic.kind) {
                case Basic_uint:
                case Basic_u8:
                case Basic_u16:
                case Basic_u32:
                case Basic_u64:
                case Basic_umm: {
                    if (value.data.unsigned_int > type->Basic.limits.max_unsigned_value) {
                        type_warning(tcx, string_print("expected type `%` cannot fit in value `%`", 
                                                       f_type(type), f_value(&value)), span);
                    }
                    
                } break;
                
                case Basic_int:
                case Basic_s8:
                case Basic_s16:
                case Basic_s32:
                case Basic_s64:
                case Basic_smm: {
                    if (value.data.unsigned_int > (u64) type->Basic.limits.max_value) {
                        type_warning(tcx, string_print("expected type `%` cannot fit in value `%`", 
                                                       f_type(type), f_value(&value)), span);
                    }
                } break;
            }
            
        } break;
        
        case Value_floating: {
            result = (type->kind == TypeKind_Basic &&
                      (type->Basic.kind == Basic_f32 || 
                       type->Basic.kind == Basic_f64));
        } break;
        
        case Value_pointer:
        case Value_array: {
            // TODO(Alexander): do we need more stricter type checking of each value?
            result = type->kind == TypeKind_Array;
        } break;
        
        case Value_string: {
            result = type->kind == TypeKind_Basic && 
                is_bitflag_set(type->Basic.flags, BasicFlag_String);
        } break;
        
        case Value_cstring: {
            result = type->kind == TypeKind_Basic && type->Basic.kind == Basic_cstring;
        } break;
    }
    
    if (report_error && !result) {
        type_error(tcx, string_print("expected type `%` is not compatible with `%`", 
                                     f_type(type), f_value(&value)), span);
    }
    
    return result;
}

// TODO(Alexander): this is a compile opt technique, this belongs in different file
Value
constant_folding_of_expressions(Type_Context* tcx, Ast* ast) {
    assert(is_ast_expr(ast) || 
           ast->kind == Ast_Value || 
           ast->kind == Ast_Ident || 
           ast->kind == Ast_Exported_Type ||
           ast->kind == Ast_None);
    Value result = {};
    
    switch (ast->kind) {
        // TODO(alexander): do we want values to be expressions?
        case Ast_Value: {
            result = ast->Value;
        } break;
        
        case Ast_Unary_Expr: {
            Value first = constant_folding_of_expressions(tcx, ast->Unary_Expr.first);
            if (!is_void(first)) {
                switch (ast->Unary_Expr.op) {
                    case Op_Negate: {
                        if (is_integer(first)) {
                            result.data.signed_int = -first.data.signed_int;
                            result.type = Value_signed_int;
                        } else if(is_floating(first)) {
                            result.data.floating = -first.data.floating;
                            result.type = Value_floating;
                        }
                    } break;
                    
                    case Op_Logical_Not: {
                        if (is_integer(first)) {
                            result.data.boolean = !value_to_bool(first);
                            result.type = Value_boolean;
                        }
                    } break;
                }
                
                if (result.type != Value_void) {
                    ast->type = ast->Unary_Expr.first->type;
                }
            }
        } break;
        
        case Ast_Binary_Expr: {
            if (operator_is_assign(ast->Binary_Expr.op)) {
                // NOTE(Alexander): we cannot constant fold lhs of an assignemnt operator
                constant_folding_of_expressions(tcx, ast->Binary_Expr.second);
            } else {
                Value first = constant_folding_of_expressions(tcx, ast->Binary_Expr.first);
                Value second = constant_folding_of_expressions(tcx, ast->Binary_Expr.second);
                
                if (!is_void(first) && !is_void(second)) {
                    
                    // NOTE(Alexander): Type rules
                    // int + int;
                    // float + int -> float + float;
                    // int + float -> float + float;
                    // float -X-> int (is a no no, it has to be an explicit cast)
                    // X == X -> bool (where == is any valid comparison operator)
                    
                    if (is_floating(first) || is_floating(second)) {
                        // NOTE(Alexander): Make sure both types are floating
                        if (is_integer(first)) {
                            first.data.floating  = value_to_f64(first);
                            first.type = Value_floating;
                            ast->type = ast->Binary_Expr.second->type;
                        } else if (is_integer(second)) {
                            second.data.floating = value_to_f64(second);
                            second.type = Value_floating;
                            ast->type = ast->Binary_Expr.first->type;
                        } else {
                            //pln("%: %", f_ast(ast), f_type(ast->Binary_Expr.first->type));
                            ast->type = ast->Binary_Expr.first->type;
                        }
                        
                        first = value_floating_binary_operation(first, second, ast->Binary_Expr.op);
                        result = first;
                        
                    } else if (is_integer(first) && is_integer(second)) {
                        // NOTE(Alexander): integer math
                        first.data.signed_int = value_integer_binary_operation(first, second, ast->Binary_Expr.op);
                        
                        result = first;
                        ast->type = ast->Binary_Expr.first->type;
                        
                        if (operator_is_comparator(ast->Binary_Expr.op)) {
                            // Comparison operands always results in boolean
                            result.type = Value_boolean;
                        }
                    }
                }
            }
        } break;
        
        case Ast_Ternary_Expr: {
            Value first = constant_folding_of_expressions(tcx, ast->Ternary_Expr.first);
            Value second = constant_folding_of_expressions(tcx, ast->Ternary_Expr.second);
            Value third = constant_folding_of_expressions(tcx, ast->Ternary_Expr.third);
            
            if (is_integer(first)) {
                if (first.data.boolean) {
                    result = second;
                } else {
                    result = third;
                }
            }
        } break;
        
        case Ast_Cast_Expr: {
            Value value = constant_folding_of_expressions(tcx, ast->Cast_Expr.expr);
            
            if (!is_void(value)) {
                Type* type = ast->type;
                if (!type && is_valid_ast(ast->Cast_Expr.type)) {
                    type = create_type_from_ast(tcx, ast->Cast_Expr.type, false).type;
                }
                if (type && type->kind == TypeKind_Basic) {
                    value = value_cast(value, type->Basic.kind);
                    result = result;
                    ast->type = type;
                }
            }
        } break;
        
        case Ast_Field_Expr: {
            if (ast->Field_Expr.var) {
                string_id var_ident = try_unwrap_ident(ast->Field_Expr.var);
                if (var_ident > 0) {
                    Type* type = load_type_declaration(tcx, var_ident, 
                                                       ast->Field_Expr.var->span, false);
                    
                    if (type && type->kind == TypeKind_Enum) {
                        string_id ident = try_unwrap_ident(ast->Field_Expr.field);
                        result = map_get(type->Enum.values, ident);
                        ast->type = type->Enum.type;
                    }
                }
            }
            
        } break;
        
        case Ast_Call_Expr: {
            for_compound(ast->Call_Expr.args, arg) {
                if (arg->Argument.assign) {
                    constant_folding_of_expressions(tcx, arg->Argument.assign);
                }
            }
        }
        
        case Ast_Paren_Expr: {
            result = constant_folding_of_expressions(tcx, ast->Paren_Expr.expr);
            if (!is_void(result)) {
                ast->type = ast->Paren_Expr.expr->type;
            }
        } break;
    }
    
    if (result.type != Value_void) {
        ast->kind = Ast_Value;
        ast->Value = result;
    }
    
    return result;
}

struct Function_Match_Result {
    int score;
    bool accepted;
};


internal Function_Match_Result
match_function_args(Type_Context* tcx,
                    Type* function_type,
                    array(Type*)* actual_args,
                    array(bool)* actual_args_is_value,
                    bool report_error,
                    bool strict_match=false) {
    
    Function_Match_Result result = {};
    result.accepted = true;
    
    Type_Function* t_func = &function_type->Function;
    array(Type*)* formal_args = t_func->arg_types;
    smm actual_arg_count = array_count(actual_args);
    smm formal_arg_count = array_count(formal_args);
    
    if (actual_arg_count < t_func->first_default_arg_index ||
        (actual_arg_count > formal_arg_count && !t_func->is_variadic)) {
        result.accepted = false;
    }
    
    // TODO(Alexander): nice to have: support for keyworded args, default args
    for_array_v(actual_args, actual_type, arg_index) {
        Type* formal_type = 0;
        if (arg_index < array_count(formal_args)) {
            formal_type = formal_args[arg_index];
        }
        
        bool arg_is_value = false;
        if (actual_args_is_value) {
            arg_is_value = actual_args_is_value[arg_index];
        }
        
        
        if (actual_type) {
            bool exact_match = formal_type && type_equals(formal_type, actual_type);
            if (exact_match) {
                result.score++;
            }
            
            if (strict_match) {
                if (!exact_match) {
                    if (report_error) {
                        type_error_mismatch(tcx, formal_type, actual_type, empty_span);
                    }
                    result.accepted = false;
                    break;
                }
            } else {
                if (formal_type && !type_check_assignment(tcx, formal_type, actual_type, arg_is_value,
                                                          empty_span, Op_Assign, report_error)) {
                    result.accepted = false;
                    break;
                }
            }
        } else {
            result.accepted = false;
            break;
        }
        
        result.score++;
        arg_index++;
    }
    
    return result;
}

internal Ast*
auto_type_conversion(Type_Context* tcx, Type* target_type, Ast* node, Operator op=Op_Assign) {
    if (target_type->kind == TypeKind_Any) {
        return node;
    }
    
    
    Ast* result = node;
    Type* initial_type = node->type;
    
    // TODO(Alexander): we don't want to change types without doing explicit cast
    // otherwise the backend gets confused and outputs wrong code.
    if (type_check_assignment(tcx, target_type, initial_type, node->kind == Ast_Value, empty_span, op, false)) {
        
        if (type_equals(initial_type, target_type)) {
            node->type = target_type;
        } else {
            bool can_be_converted_by_value = node->kind == Ast_Value;
            
            // TODO(Alexander): make sure we don't convert string value to a number
            if (node->Value.type == Value_string || node->Value.type == Value_cstring) {
                if (!(target_type->kind == TypeKind_Basic &&
                      target_type->Basic.flags == BasicFlag_String)) {
                    can_be_converted_by_value = false;
                }
            }
            
            if (can_be_converted_by_value) {
                node->type = target_type;
                node->Value = value_cast(node->Value, 
                                         target_type->kind == TypeKind_Basic ? 
                                         target_type->Basic.kind : Basic_s64);
                // HACK(Alexander): auto converting string to cstring
                if (is_string(node->Value)) {
                    if (target_type && target_type->kind == TypeKind_Basic &&
                        target_type->Basic.kind == Basic_cstring) {
                        node->Value.data.cstr = string_to_cstring(node->Value.data.str);
                        node->Value.type = Value_cstring;
                    }
                }
                
                
            } else {
                // TODO(Alexander): temporary use of calloc we need to find a better way to manipulate the Ast
                Ast* cast_node = (Ast*) calloc(1, sizeof(Ast));
                cast_node->kind = Ast_Cast_Expr;
                cast_node->Cast_Expr.type = (Ast*) calloc(1, sizeof(Ast));
                cast_node->Cast_Expr.type->kind = Ast_None;
                cast_node->Cast_Expr.expr = node;
                cast_node->type = target_type;
                
                result = cast_node;
                //pln("type conversion on AST node:\n%", f_ast(expr));
            }
        }
    }
    
    return result;
}

// NOTE(Alexander): parent_type is used to coerce a non-typed literal to a specific type
Type*
type_infer_expression(Type_Context* tcx, Ast* expr, Type* parent_type, bool report_error) {
    Type* result = 0;
    
    switch (expr->kind) {
        case Ast_None: {
            result = t_void;
        } break;
        
        
        case Ast_Value: {
            if (expr->type) {
                result = expr->type;
            }
            
            if (!result) {
                if (parent_type && parent_type->kind == TypeKind_Basic) {
                    result = parent_type;
                    expr->Value = value_cast(expr->Value, result->Basic.kind);
                } else {
                    result = type_infer_value(tcx, expr->Value);
                }
            }
            
            result = normalize_basic_types(result);
            
            // HACK(Alexander): auto converting string to cstring
            if (is_string(expr->Value)) {
                if (parent_type && parent_type->kind == TypeKind_Basic &&
                    parent_type->Basic.kind == Basic_cstring) {
                    result = parent_type;
                    expr->Value.data.cstr = string_to_cstring(expr->Value.data.str);
                    expr->Value.type = Value_cstring;
                }
            }
            
            type_check_value(tcx, result, expr->Value, expr->span, report_error);
            expr->type = result;
        } break;
        
        case Ast_Ident: {
            string_id ident = ast_unwrap_ident(expr);
            
            Type* type = map_get(tcx->locals, ident);
            
            if (type) {
                result = type;
                
            } else {
                type = map_get(tcx->globals, ident);
                
                if (type) {
                    result = type;
                    
                } else {
                    
                    type = load_type_declaration(tcx, ident, expr->span, false);
                    
                    if (type) {
                        result = t_type;
                        expr->kind = Ast_Exported_Type;
                        expr->Exported_Type = export_type_info(&tcx->type_info_packer, type);
                        
                    } else {
                        if (tcx->set_undeclared_to_s64) {
                            result = t_s64;
                        } else if (report_error) {
                            // NOTE(Alexander): copypasta
                            type_error(tcx, 
                                       string_print("`%` is an undeclared identifier", 
                                                    f_string(vars_load_string(expr->Ident))),
                                       expr->span);
                        }
                    }
                }
            }
            expr->type = result;
        } break;
        
        case Ast_Unary_Expr: {
            if (parent_type) {
                if (expr->Unary_Expr.op == Op_Dereference) {
                    parent_type = type_wrap_pointer(tcx, parent_type);
                }
            }
            
            Operator op = expr->Unary_Expr.op;
            Type* type = type_infer_expression(tcx, expr->Unary_Expr.first, parent_type, report_error);
            
            
            if (op == Op_Negate) {
                Overloaded_Operator_List overloads =
                    map_get(tcx->overloaded_operators, type);
                
                if (overloads.is_valid) {
                    for_array(overloads.ops, overload, _) {
                        if (overload->op == op) {
                            expr->Unary_Expr.overload = overload->func;
                        }
                    }
                }
            }
            
            if (type) {
                switch (op) {
                    case Op_Dereference: {
                        if (type->kind == TypeKind_Pointer) {
                            result = type->Pointer;
                        } else {
                            if (report_error) {
                                //pln("%", f_ast(expr));
                                type_error(tcx,
                                           string_print("cannot dereference type `%`", f_type(type)),
                                           expr->span);
                            }
                        }
                    } break;
                    
                    case Op_Address_Of: {
                        result = type_wrap_pointer(tcx, type);
                    } break;
                    
                    default: {
                        result = type;
                    } break;
                }
                
                expr->type = result;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Operator op = expr->Binary_Expr.op;
            Type* first_type = type_infer_expression(tcx, 
                                                     expr->Binary_Expr.first,
                                                     0,
                                                     report_error);
            Type* second_type = type_infer_expression(tcx, 
                                                      expr->Binary_Expr.second,
                                                      first_type,
                                                      report_error);
            
            if (!first_type || !second_type) {
                break;
            }
            
            // TODO(Alexander): find another way to make this matching more robust and potentially more efficient
            Overloaded_Operator_List overloads =
                map_get(tcx->overloaded_operators, expr->Binary_Expr.first->type);
            if (overloads.is_valid) {
                for_array(overloads.ops, overload, _) {
                    if (overload->op == op &&
                        type_check_assignment(tcx, overload->rhs, 
                                              expr->Binary_Expr.second->type, 
                                              is_ast_value(expr->Binary_Expr.second),
                                              empty_span, Op_Assign, false)) {
                        expr->Binary_Expr.overload = overload->func;
                        second_type = overload->rhs;
                    }
                }
            }
            
            if (expr->Binary_Expr.overload) {
                Type* overload = expr->Binary_Expr.overload;
                assert(overload->kind == TypeKind_Function);
                result = overload->Function.return_type;
                
            } else if (operator_is_assign(expr->Binary_Expr.op)) {
                
                if (first_type->kind == TypeKind_Function && 
                    second_type->kind == TypeKind_Function) {
                    // TODO(Alexander): HACK right-hand side is always the "real" function type
                    //                  the left-hand side could be just a typedef function type
                    result = second_type;
                    first_type = result;
                } else {
                    result = first_type;
                    second_type = result;
                }
                
            } else {
                if (first_type->kind == TypeKind_Enum) { 
                    first_type = first_type->Enum.type;
                }
                
                if (second_type->kind == TypeKind_Enum) { 
                    second_type = second_type->Enum.type;
                }
                
                if (first_type->kind == TypeKind_Basic && 
                    second_type->kind == TypeKind_Basic) {
                    
                    if (op == Op_Logical_Or || op == Op_Logical_And) {
                        result = t_bool;
                        
                    } else if ((first_type->Basic.flags & BasicFlag_Floating) == 
                               (second_type->Basic.flags & BasicFlag_Floating)) {
                        if (first_type->size > second_type->size) {
                            result = first_type;
                            if (expr->Binary_Expr.first->kind == Ast_Value) {
                                result = second_type;
                            }
                        } else {
                            result = second_type;
                            if (expr->Binary_Expr.second->kind == Ast_Value) {
                                result = first_type;
                            }
                        }
                        
                    } else if (first_type->Basic.flags & BasicFlag_Floating) {
                        result = first_type;
                    } else {
                        result = second_type;
                    }
                    
#if 0
                    if (parent_type && result != parent_type) {
                        //pln("  % >= %", f_int(first_type->size), f_int(second_type->size));
                        bool is_parent_floating = false;
                        if (parent_type->kind == TypeKind_Basic) {
                            is_parent_floating = is_bitflag_set(parent_type->Basic.flags, BasicFlag_Floating);
                        }
                        
                        if (parent_type->size >= result->size && 
                            (is_result_floating == is_parent_floating)) {
                            result = parent_type;
                            first_type = result;
                            second_type = result;
                        }
                    }
#endif
                } else if (first_type->kind == TypeKind_Pointer ||
                           second_type->kind == TypeKind_Pointer) {
                    
                    result = first_type->kind == TypeKind_Pointer ? first_type : second_type;
                } else {
                    if (parent_type && !operator_is_comparator(op) && op != Op_Logical_And && op != Op_Logical_Or) {
                        result = parent_type;
                    } else {
                        s32 first_size = first_type->size;
                        s32 second_size = second_type->size;
                        result = first_size > second_size ?
                            first_type : second_type;
                    }
                    //result = normalize_basic_types(t_smm);
                }
                
                first_type = result;
                second_type = result;
            }
            
            if (result) {
                expr->Binary_Expr.first = auto_type_conversion(tcx, first_type, expr->Binary_Expr.first, op);
                expr->Binary_Expr.second = auto_type_conversion(tcx, second_type, expr->Binary_Expr.second, op);
                
                if (operator_is_comparator_table[expr->Binary_Expr.op] ||
                    op == Op_Logical_And || op == Op_Logical_Or) {
                    result = t_bool;
                }
                expr->type = result;
                
                //pln("Binary_Expr after:%\n% = % op %\n", f_ast(expr), f_type(result), f_type(first_type), f_type(second_type));
            }
        } break;
        
        case Ast_Ternary_Expr: {
            Type* first_type = type_infer_expression(tcx, 
                                                     expr->Ternary_Expr.first,
                                                     t_bool,
                                                     report_error);
            Type* second_type = type_infer_expression(tcx, 
                                                      expr->Ternary_Expr.second,
                                                      0,
                                                      report_error);
            Type* third_type = type_infer_expression(tcx, 
                                                     expr->Ternary_Expr.third,
                                                     second_type,
                                                     report_error);
            if (first_type && second_type && third_type) {
                result = second_type;
                expr->type = result;
            }
        } break;
        
        case Ast_Call_Expr: {
            Type* function_type = 0;
            const string_id ident = try_unwrap_ident(expr->Call_Expr.ident);
            
            //if (ident == vars_save_cstring("CreateFileA")) {
            //__debugbreak();
            //}
            
            array(Type*)* actual_arg_types = 0;
            array(bool)* actual_arg_is_value = 0;
            Ast* actual_args = expr->Call_Expr.args;
            for_compound(actual_args, actual_arg) {
                Type* actual_type = 0;
                
                if (is_valid_ast(actual_arg->Argument.assign)) {
                    actual_type = type_infer_expression(tcx, 
                                                        actual_arg->Argument.assign, 
                                                        0, 
                                                        report_error);
                    
                } else if (is_valid_ast(actual_arg->Argument.type)) {
                    
                    actual_type = create_type_from_ast(tcx, actual_arg->Argument.type, report_error).type;
                }
                
                if (!actual_type) {
                    if (report_error) {
                        pln("%", f_ast(expr));
                        type_error(tcx, string_lit("argument is malformed"), actual_arg->span);
                    }
                    return 0;
                }
                
                actual_arg->type = actual_type;
                array_push(actual_arg_types, actual_type);
                bool arg_is_value = is_ast_value(actual_arg);
                array_push(actual_arg_is_value, arg_is_value);
            }
            
            if (ident) {
                int highest_score = 0;
                Type* function_with_highest_score = 0;
                
                Overloaded_Function_List* overload = &map_get(tcx->overloaded_functions, ident);
                if (overload && overload->is_valid) {
                    for_array_v(overload->functions, overload_func, _) {
                        //if (ident == vars_save_cstring("DEBUG_read_tmx_map_data")) {
                        //__debugbreak();
                        //}
                        Function_Match_Result match = match_function_args(tcx, 
                                                                          overload_func,
                                                                          actual_arg_types,
                                                                          actual_arg_is_value,
                                                                          false);
                        
                        if (match.score > highest_score) {
                            highest_score = match.score;
                            
                            if (match.accepted) {
                                function_type = overload_func;
                            } else {
                                //pln("highest score = %", f_int(highest_score));
                                function_with_highest_score = overload_func;
                            }
                        }
                    }
                    
                    
                    if (!function_type) {
                        if (report_error) {
                            // TODO(Alexander): can we improve this e.g. showing closest matched function?
                            type_error(tcx, string_print("no overloaded function matched `%`", f_var(ident)), expr->span);
                            if (function_with_highest_score) {
                                // TODO(Alexander): should have a way to do follow up information
                                type_error(tcx, string_print("did you mean to call: `%`", f_type(function_with_highest_score)), expr->span);
                            }
                        }
                        return 0;
                    }
                }
            }
            
            if (!function_type) {
                function_type = type_infer_expression(tcx, 
                                                      expr->Call_Expr.ident, 
                                                      parent_type, 
                                                      report_error);
            }
            
            
            if (!function_type) {
                return 0;
            }
            
            if (function_type->kind == TypeKind_Pointer) {
                function_type = function_type->Pointer;
            }
            
            if (function_type->kind != TypeKind_Function) {
                if (report_error) {
                    string_id function_ident = try_unwrap_ident(expr->Call_Expr.ident);
                    type_error(tcx, string_print("`%` is not a function", f_var(function_ident)),
                               expr->span);
                }
                return 0;
            }
            
            Type_Function* t_func = &function_type->Function;
            
            if (t_func->ident == Sym___debugbreak) {
                __debugbreak();
            }
            
            //if (t_func->ident == vars_save_cstring("vec3")) {
            //pln("%", f_ast(expr));
            //__debugbreak();
            //}
            
            
            {
                smm arg_index = 0;
                smm formal_arg_count = array_count(t_func->arg_types);
                for_compound(expr->Call_Expr.args, arg) {
                    //pln("% < % = %", f_smm(arg_index), f_smm(formal_arg_count), f_bool(arg_index < formal_arg_count));
                    if (arg_index < formal_arg_count && is_valid_ast(arg->Argument.assign)) {
                        constant_folding_of_expressions(tcx, arg->Argument.assign);
                        Type* formal_type = t_func->arg_types[arg_index];
                        arg->Argument.assign = auto_type_conversion(tcx, formal_type, arg->Argument.assign);
                        arg->type = arg->Argument.assign->type;
                        arg_index++;
                    }
                }
            }
            
            
            expr->Call_Expr.function_type = function_type;
            result = normalize_basic_types(t_func->return_type);
            t_func->return_type = result;
            expr->type = result;
            
            // HACK(Alexander): for now print_format pushes the format type first then the value 
            if (function_type && t_func->intrinsic) {
                void* intrinsic = t_func->intrinsic;
                
                if (intrinsic == &print && !expr->Call_Expr.added_format_types) {
                    
                    
                    //pln("%", f_ast(expr));
                    //__debugbreak();
                    
                    s32 arg_index = 0;
                    Ast* prev_compound = 0;
                    Ast* curr_compound = actual_args;
                    while (curr_compound && curr_compound->Compound.node) {
                        Ast* actual_arg = curr_compound->Compound.node;
                        if (prev_compound) {
                            Format_Type fmt_type = convert_type_to_format_type(actual_arg->type);
                            // TODO(Alexander): refactor this later when the AST storage gets updated
                            
                            Ast* fmt_arg = arena_push_struct(&tcx->type_arena, Ast);
                            fmt_arg->kind = Ast_Argument;
                            fmt_arg->type = t_s32;
                            
                            Ast* fmt_value = arena_push_struct(&tcx->type_arena, Ast);
                            fmt_value->kind = Ast_Value;
                            fmt_value->type = t_s32;
                            fmt_value->Value = create_signed_int_value(fmt_type);
                            fmt_arg->Argument.assign = fmt_value;
                            
                            Ast* fmt_compound = arena_push_struct(&tcx->type_arena, Ast);
                            fmt_compound->kind = Ast_Compound;
                            fmt_compound->Compound.node = fmt_arg;
                            fmt_compound->Compound.next = curr_compound;
                            prev_compound->Compound.next = fmt_compound;
                        }
                        
                        prev_compound = curr_compound;
                        curr_compound = curr_compound->Compound.next;
                    }
                    
                    expr->Call_Expr.added_format_types = true;
                } else if (intrinsic == &type_sizeof || intrinsic == &type_alignof) {
                    intrin_type_def* intrinsic_proc = (intrin_type_def*) intrinsic;
                    
                    s32 arg_index = 0;
                    Ast* prev_compound = 0;
                    Ast* curr_compound = actual_args;
                    if (curr_compound) {
                        if (curr_compound->Compound.next && curr_compound->Compound.next->kind == Ast_Compound) {
                            
                            //pln("%", f_ast(curr_compound->Compound.next));
                            // TODO(Alexander): error handling
                            assert(0 && "expected only one param");
                            
                        } else if (curr_compound->Compound.node) {
                            Ast* actual_arg = curr_compound->Compound.node;
                            expr->kind = Ast_Value;
                            //pln("% (size = %)", f_ast(actual_arg), f_umm(intrinsic_proc(actual_arg->type)));
                            expr->Value =
                                create_unsigned_int_value(intrinsic_proc(actual_arg->type));
                        }
                    }
                    
#if 0
                } else if (intrinsic == &type_info) {
                    if (t_func->return_type) {
                        Ast* type_arg = expr->Call_Expr.args;
                        if (type_arg->kind == Ast_Compound) {
                            type_arg = type_arg->Compound.node;
                        }
                        
                        Type* actual_type = (Type*) type_arg->Value.data.data;
                        Type_Info* info = type_info(&tcx->type_info_packer, type_arg->type);
                        expr->kind = Ast_Value;
                        expr->Value = {};
                        expr->Value.type = Value_pointer;
                        expr->Value.data.data = info;
                    } else {
                        if (report_error) {
                            type_error(tcx, 
                                       string_lit("cannot use `type_info` without type introspection"),
                                       expr->span);
                            result = 0;
                        }
                    }
#endif
                } else if (intrinsic == &type_of) {
                    Ast* type_arg = expr->Call_Expr.args;
                    if (type_arg->kind == Ast_Compound) {
                        type_arg = type_arg->Compound.node;
                    }
                    
                    Type* actual_type = type_arg->type;
                    expr->kind = Ast_Exported_Type;
                    expr->Exported_Type = export_type_info(&tcx->type_info_packer, actual_type);
                }
            }
        } break;
        
        case Ast_Cast_Expr: {
            Type* type = expr->type;
            if (!type) {
                type = create_type_from_ast(tcx, expr->Cast_Expr.type, report_error).type;
            }
            
            if (type) {
                Type* actual_type = type_infer_expression(tcx, expr->Cast_Expr.expr, type, report_error);
                if (actual_type) {
                    expr->type = type;
                    result = expr->type;
                }
            }
        } break;
        
        case Ast_Paren_Expr: {
            result = type_infer_expression(tcx, expr->Paren_Expr.expr, parent_type, report_error);
            expr->type = result;
        } break;
        
        case Ast_Index_Expr: {
            if (type_infer_expression(tcx, expr->Index_Expr.index, t_smm, report_error)) {
                Type* type = type_infer_expression(tcx, expr->Index_Expr.array, parent_type, report_error);
                if (type) {
                    
                    if (type->kind == TypeKind_Array) {
                        result = type->Array.type;
                    } else if (type->kind == TypeKind_Pointer) {
                        result = type->Pointer;
                    } else {
                        if (report_error) {
                            type_error(tcx, string_lit("index operator expects an array"), expr->span);
                        }
                    }
                    expr->type = result;
                }
            }
        } break;
        
        case Ast_Field_Expr: {
            Type* type = type_infer_expression(tcx, expr->Field_Expr.var, 0, report_error);
            string_id field_ident = ast_unwrap_ident(expr->Field_Expr.field);
            
            if (!type) {
                break;
            }
            
            if (type->kind == TypeKind_Pointer) {
                type = type->Pointer; // dereference the pointer
                
                if (!type) {
                    break;
                }
            }
            
            switch (type->kind) {
                case TypeKind_Union:
                case TypeKind_Struct: {
                    smm index = map_get_index(type->Struct_Like.ident_to_index, field_ident);
                    if (index >= 0) {
                        result = type->Struct_Like.types[index];
                    }
                } break;
                
                case TypeKind_Enum: {
                    smm index = map_get_index(type->Enum.values, field_ident);
                    if (index >= 0) {
                        Value value = type->Enum.values[index].value;
                        expr->kind = Ast_Value;
                        expr->Value = value;
                        result = type;
                    }
                } break;
                
                case TypeKind_Array: {
                    // NOTE(Alexander): Array are actually structs that has 2-3 fields, 
                    // - data: which is just a raw pointer to the first element
                    // - count: the number of elements 
                    // - capacity (only applicable for growable arrays): the number of allocated elements
                    
                    switch (field_ident) {
                        case Sym_data: {
                            result = type_wrap_pointer(tcx, type->Array.type);
                        } break;
                        
                        case Sym_count: {
                            result = normalize_basic_types(t_smm);
                            
                            if (type->Array.capacity > 0) {
                                Value capacity = {};
                                capacity.type = Value_signed_int;
                                capacity.data.signed_int = type->Array.capacity;
                                
                                expr->kind = Ast_Value;
                                expr->Value = capacity;
                            }
                        } break;
                        
                        case Sym_capacity: {
                            if (type->Array.is_dynamic) {
                                result = normalize_basic_types(t_smm);
                            }
                        } break;
                    }
                } break;
                
                case TypeKind_Basic: {
                    if (type->Basic.kind == Basic_string) {
                        // NOTE(Alexander): string are actually structs that has 2 fields
                        // - data: the actual underlying storage pointer (not a cstring not null terminated)
                        // - count: the number of characters in the string
                        
                        
                        switch (field_ident) {
                            case Sym_data: {
                                result = type_wrap_pointer(tcx, t_u8);
                            } break;
                            
                            case Sym_count: {
                                result = normalize_basic_types(t_smm);
                            } break;
                        }
                    }
                } break;
            }
            
            
            if (result) {
                expr->type = result;
                
            } else {
                if (report_error) {
                    pln("%", f_ast(expr));
                    type_error(tcx, string_print("type `%` doesn't have field `%`", f_type(type), f_var(field_ident)), expr->span);
                }
            }
        } break;
        
        case Ast_Aggregate_Expr: {
            if (parent_type && parent_type->kind == TypeKind_Array) {
                Type* type = parent_type->Array.type;
                
                result = parent_type;
                
                smm count = 0;
                for_compound(expr->Aggregate_Expr.elements, it) {
                    count++;
                    if (it->kind == Ast_Argument) {
                        if (it->Argument.ident || it->Argument.type) {
                            type_error(tcx, string_lit("malformed array literal"), it->span);
                            result = 0;
                            break;
                        }
                        it = it->Argument.assign;
                    }
                    
                    if (!type_infer_expression(tcx, it, type, report_error)) {
                        result = 0;
                        break;
                    }
                }
                
                if (parent_type->Array.capacity > 0) {
                    if (parent_type->Array.capacity < count) {
                        if (report_error) {
                            type_error(tcx, string_print("array literal expects at least `%` values, found `%`",
                                                         f_smm(parent_type->Array.capacity), f_smm(count)),
                                       expr->span);
                        }
                        result = 0;
                    }
                } else {
                    parent_type->Array.capacity = count;
                    if (!parent_type->Array.is_dynamic) {
                        if (parent_type->Array.capacity > 0) {
                            parent_type->Array.is_inplace = true;
                            parent_type->size = (s32) (parent_type->Array.type->size * 
                                                       parent_type->Array.capacity);
                        } else {
                            if (report_error) {
                                // TODO(Alexander): improve this error message
                                type_error(tcx, string_print("cannot assign empty array literal to fixed-array of unknown capacity, did you mean to use a dynamic array [..]T?"), 
                                           expr->span);
                            }
                        }
                    }
                }
                
                expr->type = result;
            } else if (parent_type && parent_type->kind == TypeKind_Struct &&
                       array_count(parent_type->Struct_Like.types) > 0) {
                
                if (match_struct_like_args(tcx,
                                           parent_type,
                                           0, (int) array_count(parent_type->Struct_Like.types),
                                           expr->Aggregate_Expr.elements,
                                           report_error)) {
                    
                    result = parent_type;
                    expr->type = result;
                }
                
            } else if (parent_type && parent_type->kind == TypeKind_Union &&
                       array_count(parent_type->Struct_Like.offsets) > 0) {
                
                int first_index = 0;
                int last_index = 0;
                
                for_array_v(parent_type->Struct_Like.offsets, offset, curr_index) {
                    last_index = curr_index;
                    
                    if (offset == 0) {
                        if (last_index - first_index > 0) {
                            if (match_struct_like_args(tcx,
                                                       parent_type,
                                                       first_index,
                                                       last_index,
                                                       expr->Aggregate_Expr.elements,
                                                       false)) {
                                
                                result = parent_type;
                                expr->type = result;
                                expr->Aggregate_Expr.first_index = first_index;
                            }
                        }
                        
                        if (result) {
                            break;
                        }
                        
                        first_index = curr_index;
                    }
                }
                last_index = (int) array_count(parent_type->Struct_Like.offsets);
                
                if (!result && last_index - first_index > 0) {
                    if (match_struct_like_args(tcx,
                                               parent_type,
                                               first_index,
                                               last_index,
                                               expr->Aggregate_Expr.elements,
                                               false)) {
                        
                        result = parent_type;
                        expr->type = result;
                        expr->Aggregate_Expr.first_index = first_index;
                    }
                }
                
                
                if (!result) {
                    // TODO(Alexander): improve this by finding closest match and point (>= 1) error(s)
                    if (report_error) {
                        type_error(tcx,
                                   string_print("mismatched fields for union `%`", 
                                                f_type(parent_type)), expr->span);
                    }
                    
                }
                
                
            } else {
                if (report_error) {
                    pln("%", f_ast(expr));
                    pln("%", f_type(parent_type));
                    type_error(tcx, string_print("cannot assign aggregate initializer to non-aggregate type `%`",
                                                 f_type(parent_type)), 
                               expr->span);
                }
            }
        } break;
        
        case Ast_Tuple_Expr: {
            unimplemented;
        } break;
    }
    
    return result;
}


struct Type_Struct_Like {
    array(Type*)* types;
    array(string_id)* idents;
    array(umm)* offsets;
    
    Ident_Mapper* ident_to_index;
    
    smm offset;
    smm size;
    smm align;
    
    b32 has_error;
};

inline Type_Struct_Like
convert_type_to_struct_like(Type* type) {
    Type_Struct_Like result = {};
    
    if (type->kind == TypeKind_Struct) {
        result.types = type->Struct_Like.types;
        result.idents = type->Struct_Like.idents;
        result.offsets = type->Struct_Like.offsets;
        result.ident_to_index = type->Struct_Like.ident_to_index;
    } else if (type->kind == TypeKind_Union) {
        result.types = type->Struct_Like.types;
        result.idents = type->Struct_Like.idents;
        result.offsets = type->Struct_Like.offsets;
        result.ident_to_index = type->Struct_Like.ident_to_index;
    }
    
    return result;
}

bool
match_struct_like_args(Type_Context* tcx, Type* formal_type, int first_field, int last_field, Ast* args, bool report_error) {
    Type_Struct_Like formal = convert_type_to_struct_like(formal_type);
    int formal_field_count = last_field - first_field;
    
    //pln("%: % - %", f_type(formal_type), f_int(last_field), f_int(first_field));
    
    cstring entity_name = formal_type->kind == TypeKind_Struct ? "struct" : "union";
    
    bool has_ident = false;
    int next_field_index = first_field;
    for_compound(args, field) {
        int field_index = next_field_index++;
        
        if (field->Argument.ident) {
            string_id ident = ast_unwrap_ident(field->Argument.ident);
            
            int idx = (int) map_get_index(formal.ident_to_index, ident);
            if (idx == -1) {
                if (report_error) {
                    type_error(tcx, string_print("`%` is an undeclared identifier in % `%`",
                                                 f_string(vars_load_string(ident)),
                                                 f_cstring(entity_name),
                                                 f_type(formal_type)),
                               field->Argument.ident->span);
                }
                formal.has_error = true;
                continue;
            }
            
            field_index = formal.ident_to_index[idx].value;
            has_ident = true;
        } else if (has_ident) {
            if (report_error) {
                type_error(tcx, string_print("cannot combine named and anonymous values in % literal",
                                             f_cstring(entity_name)), field->span);
            }
            formal.has_error = true;
            break;
        }
        
        if (field_index < first_field || field_index >= last_field) {
            if (report_error) {
                string_id field_ident = formal.idents[field_index];
                type_error(tcx, string_print("cannot use unrelated field `%` in % literal",
                                             f_var(field_ident), 
                                             f_cstring(entity_name)), field->span);
            }
            formal.has_error = true;
            break;
        }
        
        Type* field_type = 0;
        if (next_field_index <= last_field) {
            field_type = formal.types[field_index];
        } else {
            if (report_error) {
                type_error(tcx, string_print("too many fields in `%`, expected only `%`",
                                             f_type(formal_type),
                                             f_int(formal_field_count)),
                           args->span);
            }
            formal.has_error = true;
            break;
        }
        
        Type* actual_type = 0;
        if (field->Argument.assign) {
            
            constant_folding_of_expressions(tcx, field->Argument.assign);
            actual_type = type_infer_expression(tcx, field->Argument.assign, field_type, report_error);
        } else {
            actual_type = type_infer_expression(tcx, field->Argument.ident, field_type, report_error);
        }
        
        if (!actual_type) {
            formal.has_error = true;
            break;
        }
    }
    
    return !formal.has_error;
}

internal inline void 
push_type_to_struct_like(Type_Struct_Like* dest, Type* type, string_id ident, int pack) {
    if (pack != 0) {
        dest->offset = align_forward(dest->offset, pack > 0 ? pack : type->align);
    }
    
    s32 index = (s32) array_count(dest->types);
    array_push(dest->types, type);
    array_push(dest->idents, ident);
    array_push(dest->offsets, dest->offset);
    map_put(dest->ident_to_index, ident, index);
    
    dest->offset += type->size;
    dest->size = max(dest->size, dest->offset);
    
    if (type->align > dest->align) {
        dest->align = type->align;
    }
}

internal Type_Struct_Like
create_type_struct_like_from_ast(Type_Context* tcx, 
                                 Ast* arguments, 
                                 bool is_union,
                                 bool report_error,
                                 int pack) {
    
    Type_Struct_Like result = {};
    
    for_compound(arguments, argument) {
        assert(argument->kind == Ast_Argument);
        
        if (!argument->Argument.type) {
            break;
        }
        
        Ast* ast_type = argument->Argument.type;
        Type* type = create_type_from_ast(tcx, ast_type, report_error).type;
        
        if (!type || type->size == 0) {
            result.has_error = true;
            return result;
        }
        
        switch (argument->Argument.ident->kind) {
            
            case Ast_Ident: {
                string_id ident = ast_unwrap_ident(argument->Argument.ident);
                if (type) {
                    push_type_to_struct_like(&result, type, ident, pack);
                } else {
                    result.has_error = true;
                }
            } break;
            
            case Ast_Compound: {
                
                if (type) {
                    for_compound(argument->Argument.ident, ast_ident) {
                        assert(ast_ident->kind == Ast_Ident);
                        string_id ident = ast_ident->Ident;
                        push_type_to_struct_like(&result, type, ident, pack);
                        
                        if (is_union) {
                            result.offset = 0;
                        }
                    }
                } else {
                    result.has_error = true;
                }
            } break;
            
            default: {
                if (type->kind == TypeKind_Struct) {
                    for_array_v(type->Struct_Like.idents, field_ident, field_index) {
                        Type* field_type = type->Struct_Like.types[field_index];
                        push_type_to_struct_like(&result, field_type, field_ident, pack);
                    }
                } else if (type->kind == TypeKind_Union) {
                    smm prev_offset = result.offset;
                    smm next_offset = result.offset;
                    for_array_v(type->Struct_Like.idents, field_ident, field_index) {
                        Type* field_type = type->Struct_Like.types[field_index];
                        smm field_offset = type->Struct_Like.offsets[field_index];
                        
                        if (field_offset == 0) {
                            if (result.offset > next_offset) {
                                next_offset = result.offset;
                            }
                            result.offset = prev_offset;
                        }
                        
                        push_type_to_struct_like(&result, field_type, field_ident, pack);
                    }
                    
                    if (result.offset > next_offset) {
                        next_offset = result.offset;
                    }
                    result.offset = next_offset;
                    //pln("%", f_ast(ast_type));
                    //unimplemented;
                } else if (type->kind == TypeKind_Function) {
                    string_id ident = type->Function.ident;
                    if (ident) {
                        push_type_to_struct_like(&result, type, ident, pack);
                    } else {
                        if (report_error) {
                            // TODO(Alexander): need to come up with a good error message?
                            // maybe this isn't a possible case?
                            unimplemented;
                            //type_error(tcx, string_lit(""))
                        }
                        result.has_error = true;
                    }
                } else {
                    // TODO(Alexander): should be a type error or something
                    unimplemented;
                }
            }
        }
        
        if (is_union) {
            result.offset = 0;
        }
    }
    
    return result;
}

Type*
load_type_declaration(Type_Context* tcx, string_id ident, Span span, bool report_error) {
    Type* result = 0;
    
    if (is_builtin_type_keyword(ident)) {
        if (ident == Kw_void) {
            result = t_void;
        } else if (ident == Kw_Type) {
            result = t_type;
        } else if (ident == Kw_auto) {
            unimplemented;
        } else {
            int index = ident - Kw_bool + 1;
            assert(index >= 0 && index < fixed_array_count(basic_type_definitions));
            
            result = &basic_type_definitions[index];
            result = normalize_basic_types(result);
        }
    } else {
        result = map_get(tcx->local_type_table, ident);
        if (!result) {
            result = map_get(tcx->global_type_table, ident);
            if (!result && report_error) {
                // NOTE(Alexander): copypasta, where?
                type_error(tcx, 
                           string_print("`%` is an undeclared identifier", 
                                        f_string(vars_load_string(ident))),
                           span);
            }
        }
    }
    
    return result;
}

Type*
save_operator_overload(Type_Context* tcx, Type* type, Operator op, Span span, bool report_error) {
    assert(type->kind == TypeKind_Function);
    // TODO(Alexander): support for Unary operator overloading
    
    int arg_count = (int) array_count(type->Function.arg_types);
    if (arg_count == 1 || arg_count == 2) {
        Type* lhs = type->Function.arg_types[0];
        Type* rhs = 0;
        
        // TODO(Alexander): we validate if the operator is possible to override
        if (arg_count == 2) {
            rhs = type->Function.arg_types[1];
        } else {
            if (op == Op_Subtract) {
                op = Op_Negate;
            } else {
                if (report_error) {
                    type_error(tcx, string_print("unary operator `%` is not overloadable", 
                                                 f_cstring(operator_strings[op])), span);
                }
                return 0;
            }
            
        }
        
        Overloaded_Operator_List* overload = &map_get(tcx->overloaded_operators, lhs);
        if (!overload || !overload->is_valid) {
            Overloaded_Operator_List new_overload = {};
            new_overload.is_valid = true;
            map_put(tcx->overloaded_operators, lhs, new_overload);
            overload = &map_get(tcx->overloaded_operators, lhs);
        }
        assert(overload);
        
        Operator_Overload overloaded_op = {};
        overloaded_op.op = op;
        overloaded_op.rhs = rhs;
        overloaded_op.func = type;
        array_push(overload->ops, overloaded_op);
        
        // TODO(Alexander): check ambiguity with previous entries
    } else {
        type_error(tcx,
                   string_print("expected `1` or `2` arguments for operator overload found `%`", 
                                f_int(arg_count)), span);
    }
    
    return type;
}

Type*
save_type_declaration(Type_Context* tcx, string_id ident, Type* type, Span span, bool report_error) {
    
    smm old_index = map_get_index(tcx->global_type_table, ident);
    if (old_index != -1) {
        Type* old_type = tcx->global_type_table[old_index].value;
        
        if (type != old_type) {
            
            if (type->kind == TypeKind_Function &&
                old_type->kind == TypeKind_Function) {
                
                // TODO(Alexander): func overload
                Overloaded_Function_List* overload = &map_get(tcx->overloaded_functions, ident);
                if (!overload || !overload->is_valid) {
                    Overloaded_Function_List new_overload = {};
                    new_overload.is_valid = true;
                    map_put(tcx->overloaded_functions, ident, new_overload);
                    overload = &map_get(tcx->overloaded_functions, ident);
                    array_push(overload->functions, old_type);
                }
                assert(overload);
                
                //if (ident == vars_save_cstring("abs")) {
                //__debugbreak();
                //}
                
                for_array_v(overload->functions, overloaded_fn, overload_index) {
                    if (match_function_args(tcx, overloaded_fn, 
                                            type->Function.arg_types,
                                            0, report_error, true).accepted) {
                        
                        // TODO(Alexander): we might not have stored the unit yet so this error isn't going to do anything
                        if (overloaded_fn->Function.unit) {
                            //if (type->Function.unit) {
                            if (report_error) {
                                type_error(tcx,
                                           string_print("cannot redeclare function `%` with the same arguments", 
                                                        f_var(type->Function.ident)), span);
                            }
                            return 0;
                        } else {
                            // TODO(Alexander): we need to better keep track of where default arguments are stored.
                            overloaded_fn->Function.unit = type->Function.unit;
                        }
                        
                        if (!type->Function.unit) {
                            return overloaded_fn;
                        }
                    }
                }
                
                array_push(overload->functions, type);
                //pln("Added overload `%`", f_type(type));
                
            } else if (type->kind == TypeKind_Struct &&
                       old_type->kind == TypeKind_Struct) {
                
                // TODO(Alexander): hack to get forward declares working
                if (array_count(type->Struct_Like.types) == 0) {
                    // do nothing
                } else if (array_count(old_type->Struct_Like.types) == 0) {
                    memcpy(old_type, type, sizeof(Type));
                } else {
                    type_error(tcx,
                               string_print("cannot redeclare previous declaration `%`",
                                            f_string(vars_load_string(ident))),
                               span);
                }
                
                type = old_type;
            } else if (type->kind == TypeKind_Union &&
                       old_type->kind == TypeKind_Union) {
                
                // TODO(Alexander): hack to get forward declares working
                if (array_count(type->Struct_Like.types) == 0) {
                    // do nothing
                } else if (array_count(old_type->Struct_Like.types) == 0) {
                    memcpy(old_type, type, sizeof(Type));
                } else {
                    type_error(tcx,
                               string_print("cannot redeclare previous declaration `%`",
                                            f_string(vars_load_string(ident))),
                               span);
                }
                
                type = old_type;
            } else {
                if (old_type->kind && !type_equals(old_type, type)) {
                    type_error(tcx,
                               string_print("cannot redeclare `%` with different type, previous type was `%`",
                                            f_var(ident), f_type(old_type)), 
                               span);
                }
            }
        }
    }
    
    if (type) {
        // TODO(Alexander): distinguish between types and types of variables
        // E.g. in `int main(...)`, main is not a type it's a global variable
        // but `typedef u32 string_id;` in this case string_id is a type.
        map_put(tcx->global_type_table, ident, type);
        map_put(tcx->globals, ident, type);
    }
    
    return type;
}

Type*
save_type_declaration_from_ast(Type_Context* tcx, string_id ident, Ast* ast, bool report_error) {
    
    Type* type = create_type_from_ast(tcx, ast, report_error).type;
    if (type) {
        
        
        if (ident == Kw_operator) {
            Operator op = ast->Function_Type.overload_operator;
            ast->type = type;
            type = save_operator_overload(tcx, type, op, ast->span, report_error);
            
        } else {
            
            if (type->kind == TypeKind_Struct && type->ident == 0) {
                // TODO: investigate why this is triggered, potentially a forward struct declaration
                //pln("%: %", f_type(type), f_ast(ast));
            }
            
            type = save_type_declaration(tcx, ident, type, ast->span, report_error);
        }
        
        if (type) {
            ast->type = type;
        }
    }
    
    return type;
}

Create_Type_From_Ast_Result
create_type_from_ast(Type_Context* tcx, Ast* ast, bool report_error) {
    assert(is_ast_type(ast));
    
    Create_Type_From_Ast_Result result = {};
    switch (ast->kind) {
        case Ast_Named_Type: {
            string_id ident = ast->Named_Type->Ident;
            result.type = load_type_declaration(tcx, ident, ast->span, report_error);
            ast->type = result.type;
        } break;
        
        case Ast_Array_Type: {
            // Array capacity
            smm capacity = 0;
            if (ast->Array_Type.shape) {
                Value capacity_value = constant_folding_of_expressions(tcx, ast->Array_Type.shape);
                
                if (is_integer(capacity_value)) {
                    capacity = value_to_smm(capacity_value);
                } else {
                    if (report_error) {
                        type_error(tcx, string_lit("array shape should be an integer"), ast->span);
                    }
                    
                    break;
                }
            }
            
            Type* elem_type = create_type_from_ast(tcx, ast->Array_Type.elem_type, report_error).type;
            if (elem_type) {
                result.type = arena_push_struct(&tcx->type_arena, Type);
                result.type->kind = TypeKind_Array;
                result.type->Array.type = elem_type;
                result.type->Array.capacity = capacity;
                result.type->Array.is_dynamic = ast->Array_Type.is_dynamic;
                
                if (!result.type->Array.is_dynamic && result.type->Array.capacity > 0) {
                    // NOTE(Alexander): fixed size arrays with known size should be allocated directly
                    // TODO(Alexander): arch dep
                    result.type->size = (s32) (elem_type->size*result.type->Array.capacity);
                    result.type->align = elem_type->align;
                    result.type->Array.is_inplace = true;
                    //result.type->size = elem_type->size * (s32) result.type->Array.capacity;
                    //result.type->align = elem_type->align;
                } else {
                    result.type->size = sizeof(void*) + sizeof(smm);
                    // TODO(Alexander): for dynamic arrays we probably just want a pointer to this struct
                    //                  to avoid dangling pointers from reallocations!
                    if (result.type->Array.is_dynamic) {
                        result.type->size += sizeof(smm);
                    }
                    result.type->align = alignof(void*);
                }
            }
        } break;
        
        case Ast_Pointer_Type: {
            // TODO(Alexander): do we really want to store a new type declaration
            // for each time we use a pointer?!???
            
            Type* ptr_type = create_type_from_ast(tcx, ast->Pointer_Type, report_error).type;
            if (ptr_type) {
                result.type = type_wrap_pointer(tcx, ptr_type);
            }
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
            result.type = arena_push_struct(&tcx->type_arena, Type);
            result.type->kind = TypeKind_Function;
            result.type->Function.is_variadic = false;
            Type* return_type = create_type_from_ast(tcx, ast->Function_Type.return_type, report_error).type;
            if (return_type) {
                result.type->Function.return_type = return_type;
            } else {
                return result;
            }
            
            // NOTE(Alexander): Loads in the function arguments
            Ast* ast_arguments = ast->Function_Type.arguments;
            Type_Function* func = &result.type->Function;
            smm offset = 0;
            //if (ast_unwrap_ident(ast->Function_Type.ident) == vars_save_cstring("DEBUG_write_tmx_map")) {
            //__debugbreak();
            //}
            for_compound(ast_arguments, ast_argument) {
                assert(ast_argument->kind == Ast_Argument);
                if (!ast_argument->Argument.type) {
                    break;
                }
                
                if (result.type->Function.is_variadic) {
                    if (report_error) {
                        type_error(tcx,
                                   string_lit("variable arguments `...` has to be the last argument"), 
                                   ast_argument->span);
                    }
                    return result;
                }
                
                Ast* ast_argument_type = ast_argument->Argument.type;
                
                Type* type = 0;
                if (ast_argument_type->kind == Ast_Ellipsis) {
                    result.type->Function.is_variadic = true;
                    type = load_type_declaration(tcx, vars_save_cstring("Var_Args"), 
                                                 ast_argument_type->span, false);
                    if (!type) {
                        type = t_type;
                    }
                    
                } else {
                    type = create_type_from_ast(tcx, ast_argument_type, report_error).type;
                }
                
                if (type) {
                    string_id ident = ast_argument->Argument.ident->Ident;
                    s32 arg_index = (s32) array_count(func->arg_idents);
                    map_put(func->ident_to_index, ident, arg_index);
                    array_push(func->arg_idents, ident);
                    array_push(func->arg_types, type);
                    
                    if (is_valid_ast(ast_argument->Argument.assign)) {
                        Ast* default_arg = ast_argument->Argument.assign;
                        type_infer_expression(tcx, default_arg, type, true);
                        constant_folding_of_expressions(tcx, default_arg);
                        array_push(func->default_args, default_arg);
                        
                    } else if (!result.type->Function.is_variadic) {
                        if (func->first_default_arg_index == arg_index) {
                            func->first_default_arg_index++;
                        } else {
                            if (report_error) {
                                type_error(tcx, string_print("missing default argument for parameter %",
                                                             f_int(arg_index)), ast_argument->span);
                            }
                        }
                    }
                } else {
                    array_free(func->arg_idents);
                    array_free(func->arg_types);
                    return {};
                }
            }
            
            if (ast->Function_Type.ident && ast->Function_Type.ident->kind == Ast_Ident) {
                result.type->Function.ident = ast_unwrap_ident(ast->Function_Type.ident);
            }
            result.type->size = sizeof(smm);
            result.type->align = alignof(smm);
            
            if (ast->Function_Type.attributes) {
                
                // Parse attributes
                Parsed_Attribute attr = parse_attribute(ast->Function_Type.attributes);
                string library_name = {};
                string library_function_name = {};
                while (attr.is_valid) {
                    if (attr.ident == Sym_link) {
                        if (!result.type->Function.ident) {
                            attr.is_valid = false;
                        }
                        
                        if (attr.values[0].type != Value_string) {
                            attr.is_valid = false;
                        }
                        
                        library_name = attr.values[0].data.str;
                        
                        if (!attr.is_valid) {
                            type_error(tcx, string_lit("@link attribute is malformed"), ast->span);
                            break;
                        }
                    } else if (attr.ident == Sym_extern_name) {
                        if (!result.type->Function.ident) {
                            attr.is_valid = false;
                        }
                        
                        if (attr.values[0].type != Value_string) {
                            attr.is_valid = false;
                        }
                        
                        library_function_name= attr.values[0].data.str;
                        
                        if (!attr.is_valid) {
                            type_error(tcx, string_lit("@extern_name attribute is malformed"), ast->span);
                            break;
                        }
                    } else if (attr.ident == Sym_dump_bytecode) {
                        result.type->Function.dump_bytecode = true;
                    }
                    attr = parse_attribute(attr.next);
                }
                
                // Link against library
                if (library_name.count > 0) {
                    string_id library_id = vars_save_string(library_name);
                    string_id library_function_id = result.type->Function.ident;
                    if (library_function_name.count == 0) {
                        library_function_name = vars_load_string(library_function_id);
                    } else {
                        library_function_id = vars_save_string(library_function_name);
                    }
                    
                    //pln("LIB: %, FUNC: %", f_string(library_name), f_string(library_function_name));
                    
                    {
                        cstring name = string_to_cstring(library_function_name);
                        cstring library = string_to_cstring(library_name);
                        
                        func->intrinsic = DEBUG_get_external_procedure_address(library, name);
                        
                        cstring_free(library);
                        cstring_free(name);
                    }
                    
                    Library_Imports import = map_get(tcx->import_table.libs, library_id);
                    import.is_valid = true;
                    
                    Library_Function lib_func = {};
                    lib_func.name = library_function_id;
                    lib_func.pointer = func->intrinsic;
                    lib_func.type = result.type;
                    array_push(import.functions, lib_func);
                    map_put(tcx->import_table.libs, library_id, import);
                    
                    //pln("% = 0x%", f_cstring(name), f_u64_HEX(func->intrinsic));
                    if (!func->intrinsic) {
                        type_error(tcx,
                                   string_print("procedure `%` is not found in library `%`",
                                                f_string(library_function_name),
                                                f_string(library_name)),
                                   ast->span);
                    }
                }
            }
        } break;
        
        case Ast_Struct_Type: {
            int pack = -1;
            for_compound(ast->Function_Type.attributes, attr) {
                string_id ident = ast_unwrap_ident(attr->Attribute.ident);
                if (ident == Sym_pack) {
                    pack = 0;
                }
            }
            
            string_id ident = try_unwrap_ident(ast->Struct_Type.ident);
            //if (ident == vars_save_cstring("Collision")) {
            //__debugbreak();
            //}
            
            // TODO(Alexander): We need to look at registering all the types first and add a is_valid flag or something,
            // because this succeeds with an invalid struct of size 0 which shouldn't be possible.
            Ast* fields = ast->Struct_Type.fields;
            Type_Struct_Like struct_like = create_type_struct_like_from_ast(tcx, fields, false, report_error, pack);
            if (!struct_like.has_error) {
                if (array_count(struct_like.types) > 0) {
                    
                    //if (ident == vars_save_cstring("Entity")) {
                    //for_array_v(struct_like.idents, ident2, index) {
                    //pln("Entity.% + % (size = %, align = %)", f_var(ident2), f_int(struct_like.offsets[index]),f_int(struct_like.types[index]->size),f_int(struct_like.types[index]->align));
                    //} 
                    //}
                    
                    result.type = arena_push_struct(&tcx->type_arena, Type);
                    result.type->kind = TypeKind_Struct;
                    result.type->Struct_Like.types = struct_like.types;
                    result.type->Struct_Like.idents = struct_like.idents;
                    result.type->Struct_Like.offsets = struct_like.offsets;
                    result.type->Struct_Like.ident_to_index = struct_like.ident_to_index;
                    result.type->ident = ident;
                    result.type->size = (s32) struct_like.size;
                    result.type->align = (s32) struct_like.align;
                    
                } else if (ident) {
                    
                    // Forward declare
                    result.type = load_type_declaration(tcx, ident, ast->span, report_error);
                }
            }
        } break;
        
        case Ast_Union_Type: {
            Ast* fields = ast->Union_Type.fields;
            Type_Struct_Like struct_like = create_type_struct_like_from_ast(tcx, fields, true, report_error, -1);
            if (!struct_like.has_error) {
                result.type = arena_push_struct(&tcx->type_arena, Type);
                result.type->kind = TypeKind_Union;
                result.type->Struct_Like.types = struct_like.types;
                result.type->Struct_Like.idents = struct_like.idents;
                result.type->Struct_Like.offsets = struct_like.offsets;
                result.type->Struct_Like.ident_to_index = struct_like.ident_to_index;
                result.type->ident = try_unwrap_ident(ast->Struct_Type.ident);
                result.type->size = (s32) struct_like.size;
                result.type->align = (s32) struct_like.align;
            }
        } break;
        
        case Ast_Enum_Type: {
            result.type = arena_push_struct(&tcx->type_arena, Type);
            result.type->kind = TypeKind_Enum;
            
            Type* type;
            if (ast->Enum_Type.elem_type && ast->Enum_Type.elem_type->kind != Ast_None) {
                type = create_type_from_ast(tcx, ast->Enum_Type.elem_type, report_error).type;
                if (type->kind != TypeKind_Basic) {
                    type_error(tcx, string_lit("enums can only be defined as primitive types"),
                               ast->span);
                    break;
                }
            } else {
                // TODO(Alexander): should we force the user to spec. the type, or pick lowest possible?
                type = t_s64;
            }
            
            result.type->Enum.type = type;
            result.type->size = type->size;
            result.type->align = type->align;
            result.type->ident = ast_unwrap_ident(ast->Enum_Type.ident);
            
            Value value;
            if (is_bitflag_set(type->Basic.flags, BasicFlag_Unsigned)) {
                value.type = Value_unsigned_int;
                value.data.unsigned_int = 0;
            } else {
                value.type = Value_signed_int;
                value.data.signed_int = 0;
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
                    
                    if (is_bitflag_set(type->Basic.flags, BasicFlag_Unsigned)) {
                        value.type = Value_unsigned_int;
                    } else {
                        value.type = Value_signed_int;
                    }
                }
                
                string_id ident = argument->Argument.ident->Ident;
                
                // TODO(Alexander): NEED TO HANDLE THE TYPE TABLE HASH MAP MEMORY
                map_put(result.type->Enum.values, ident, value);
                value.data.signed_int++;
            }
        } break;
        
        case Ast_Typedef: {
            assert(ast->Typedef.ident->kind == Ast_Ident);
            
            string_id ident = ast_unwrap_ident(ast->Typedef.ident);
            
            // TODO(Alexander): we don't care about locality of typedefs at the moment (always global)
            //result = map_get(tcx->global_type_table, ident);
            
            //if (!result) {
            //result = create_type_from_ast(tcx, ast->Typedef.type, report_error);
            result.type = save_type_declaration_from_ast(tcx, ident, ast->Typedef.type, report_error);
            //} else {
            //type_error(tcx, string_print("`%` is already defined", f_var(ident)), ast->span);
            //}
        } break;
        
        case Ast_Const_Type: {
            result = create_type_from_ast(tcx, ast->Const_Type, report_error);
            result.mods |= AstDeclModifier_Const;
        } break;
        
        case Ast_Volatile_Type: {
            result = create_type_from_ast(tcx, ast->Volatile_Type, report_error);
            result.mods |= AstDeclModifier_Volatile;
        } break;
        
        case Ast_Local_Persist_Type: {
            result = create_type_from_ast(tcx, ast->Local_Persist_Type, report_error);
            result.mods |= AstDeclModifier_Local_Persist;
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return result;
}

Type*
type_infer_statement(Type_Context* tcx, Ast* stmt, bool report_error) {
    Type* result = 0;
    
    
    switch (stmt->kind) {
        case Ast_Assign_Stmt: {
            Create_Type_From_Ast_Result created_type = create_type_from_ast(tcx, stmt->Assign_Stmt.type, report_error);
            
            Type* expected_type = created_type.type;
            if (!expected_type) {
                // TODO(Alexander): we can add support for auto types
                return result;
            }
            
            stmt->Assign_Stmt.mods = created_type.mods;
            
            Type* found_type = type_infer_expression(tcx, 
                                                     stmt->Assign_Stmt.expr, 
                                                     expected_type,
                                                     report_error);
            
            constant_folding_of_expressions(tcx, stmt->Assign_Stmt.expr);
            
            if (found_type) {
                if (expected_type) {
                    result = expected_type;
                    if (is_valid_ast(stmt->Assign_Stmt.expr)) {
                        stmt->Assign_Stmt.expr =
                            auto_type_conversion(tcx, expected_type, stmt->Assign_Stmt.expr);
                    }
                    
                    //if (stmt->Assign_Stmt.expr && 
                    //stmt->Assign_Stmt.expr->kind == Ast_Value) {
                    //stmt->Assign_Stmt.expr->type = result;
                    //}
                    
                } else {
                    result = found_type;
                }
                
                stmt->Assign_Stmt.ident->type = result;
                
                Ast* ast_ident = stmt->Assign_Stmt.ident;
                while (ast_ident) {
                    string_id ident = 0;
                    if (ast_ident->kind == Ast_Compound) {
                        if (ast_ident->Compound.node && ast_ident->Compound.node->kind == Ast_Ident) {
                            ident = ast_unwrap_ident(ast_ident->Compound.node);
                            ast_ident = ast_ident->Compound.next;
                        } else {
                            break;
                        }
                    } else if (ast_ident->kind == Ast_Ident) {
                        ident = ast_unwrap_ident(ast_ident);
                        ast_ident = 0;
                    } else {
                        break;
                    }
                    
                    if (tcx->block_depth > 0) {
                        if (!push_local(tcx, ident, result, stmt->span, report_error)) {
                            result = 0;
                        }
                    } else {
                        
                        if (map_get(tcx->globals, ident) && report_error) {
                            result = 0;
                            if (report_error) {
                                type_error(tcx,
                                           string_print("cannot redeclare previous declaration `%`",
                                                        f_string(vars_load_string(ident))),
                                           stmt->span);
                            }
                        } else {
                            map_put(tcx->globals, ident, result);
                        }
                    }
                }
                
                stmt->type = result;
            }
            
        } break;
        
        case Ast_Expr_Stmt: {
            result = type_infer_expression(tcx, stmt->Expr_Stmt, 0, report_error);
            constant_folding_of_expressions(tcx, stmt->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            result = t_void;
            
            push_type_scope(tcx);
            
            // TODO(Alexander): create a new scope
            Ast* stmts = stmt->Block_Stmt.stmts;
            for_compound(stmts, it) {
                result = type_infer_statement(tcx, it, report_error);
                if (!result) {
                    break;
                }
            }
            
            pop_type_scope(tcx);
        } break;
        
        case Ast_Decl_Stmt: {
            string_id ident = ast_unwrap_ident(stmt->Decl_Stmt.ident);
            
            Type* decl_type = stmt->Decl_Stmt.type->type;
            if (!decl_type) {
                decl_type = create_type_from_ast(tcx, stmt->Decl_Stmt.type, report_error).type;
                
                if (!decl_type) {
                    break;
                }
            }
            
            if (!stmt->Decl_Stmt.stmt || stmt->Decl_Stmt.stmt->kind == Ast_None) {
                stmt->type = decl_type;
                result = decl_type;
                map_put(tcx->local_type_table, ident, result);
            } else {
                Type* found_type = type_infer_statement(tcx, stmt->Decl_Stmt.stmt, report_error);
                
                if (found_type) {
                    if (decl_type->kind == TypeKind_Function) {
                        if (!decl_type->Function.unit) {
                            pln("TODO: this node has no compilation unit attached!!!%", f_ast(stmt));
                            // TODO(Alexander): we need to resolve this
                            //unimplemented;
                        }
                    }
                    
                    result = decl_type;
                    stmt->type = result;
                    map_put(tcx->locals, ident, result);
                }
            }
        } break;
        
        case Ast_If_Stmt: {
            Type* cond = type_infer_expression(tcx, stmt->If_Stmt.cond, t_bool, report_error);
            constant_folding_of_expressions(tcx, stmt->If_Stmt.cond);
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
        
        case Ast_Break_Stmt:
        case Ast_Continue_Stmt: {
            result = t_void;
        } break;
        
        case Ast_For_Stmt: {
            push_type_scope(tcx);
            
            Type* init = type_infer_statement(tcx, stmt->For_Stmt.init, report_error);
            Type* cond = type_infer_expression(tcx, stmt->For_Stmt.cond, t_bool, report_error);
            constant_folding_of_expressions(tcx, stmt->For_Stmt.cond);
            Type* update = type_infer_expression(tcx, stmt->For_Stmt.update, 0, report_error);
            Type* block = type_infer_statement(tcx, stmt->For_Stmt.block, report_error);
            
            if (init && cond && update && block) {
                result = init;
            }
            
            pop_type_scope(tcx);
        } break;
        
        case Ast_While_Stmt: {
            Type* cond = type_infer_expression(tcx, stmt->While_Stmt.cond, t_bool, report_error);
            constant_folding_of_expressions(tcx, stmt->While_Stmt.cond);
            Type* block = type_infer_statement(tcx, stmt->While_Stmt.block, report_error);
            
            if (cond && block) {
                result = cond;
            }
        } break;
        
        case Ast_Switch_Stmt: {
            Type* cond = type_infer_expression(tcx, stmt->Switch_Stmt.cond, 0, report_error);
            constant_folding_of_expressions(tcx, stmt->Switch_Stmt.cond);
            result = cond;
            
            bool has_default = false;
            
            for_compound(stmt->Switch_Stmt.cases, it) {
                assert(it->kind == Ast_Switch_Case);
                
                if (!it->Switch_Case.cond || it->Switch_Case.cond->kind == Ast_None) {
                    if (has_default) {
                        if (report_error) {
                            type_error(tcx, string_lit("cannot define more than one default case"),
                                       it->span);
                        }
                        return 0;
                    }
                    has_default = true;
                    
                } else {
                    Value case_cond = constant_folding_of_expressions(tcx, it->Switch_Case.cond);
                    if (!is_integer(case_cond)) {
                        if (report_error) {
                            if (!type_infer_expression(tcx, it->Switch_Case.cond, 0, true)) {
                                type_error(tcx, string_print("expected integral case condition, found `%`",
                                                             f_value(&case_cond)),
                                           it->Switch_Case.cond->span);
                            }
                        }
                        return 0;
                    }
                }
                
                if (it->Switch_Case.stmt && !type_infer_statement(tcx, 
                                                                  it->Switch_Case.stmt, 
                                                                  report_error)) {
                    return 0;
                }
            }
            
        } break;
        
        case Ast_Return_Stmt: {
            if (tcx->return_type) {
                if (is_valid_ast(stmt->Return_Stmt.expr)) {
                    result = type_infer_expression(tcx, stmt->Return_Stmt.expr, 
                                                   tcx->return_type, report_error);
                    constant_folding_of_expressions(tcx, stmt->Return_Stmt.expr);
                    if (stmt->Return_Stmt.expr->type) {
                        stmt->Return_Stmt.expr = auto_type_conversion(tcx, tcx->return_type, stmt->Return_Stmt.expr);
                        result = stmt->Return_Stmt.expr->type;
                    } else {
                        result = 0;
                    }
                } else {
                    result = t_void;
                }
                stmt->type = result;
            } else {
                if (report_error) {
                    type_error(tcx, string_lit("cannot use `return` outside of a function"),
                               stmt->span);
                }
            }
        } break;
    }
    
    return result;
}

bool
type_check_assignment(Type_Context* tcx, Type* lhs, Type* rhs, bool is_rhs_value, Span span, Operator op, bool report_error) {
    assert(lhs && rhs);
    
    //if (lhs->kind == TypeKind_Any || rhs->kind == TypeKind_Any) {
    //return true;
    //}
    
    if (lhs->kind == TypeKind_Enum) {
        //pln("lhs - before: %", f_type(lhs));
        lhs = lhs->Enum.type;
        //pln("lhs - after: %", f_type(lhs));
        //pln("rhs - before: %", f_type(rhs));
    }
    
    if (rhs->kind == TypeKind_Enum) {
        rhs = rhs->Enum.type;
    }
    
    if (is_rhs_value && lhs->kind == TypeKind_Pointer) {
        return true;
    }
    
    if (lhs->kind == TypeKind_Pointer && 
        rhs->kind == TypeKind_Basic && 
        rhs->Basic.flags & BasicFlag_Integer) {
        
        return true;
    }
    
    if (lhs->kind != rhs->kind) {
        if (report_error) {
            type_error_mismatch(tcx, lhs, rhs, span);
        }
        return false;
    }
    
    switch (lhs->kind) {
        case TypeKind_Void: break;
        
        case TypeKind_Basic: {
            
            if (operator_is_comparator(op) && 
                lhs->Basic.flags & BasicFlag_Integer &&
                rhs->Basic.flags & BasicFlag_Integer) {
                if ((lhs->Basic.flags & BasicFlag_Unsigned) != (rhs->Basic.flags & BasicFlag_Unsigned)) {
                    if (report_error) {
                        type_error(tcx, 
                                   string_lit("comparison with both signed and unsigned integers"),
                                   span);
                    }
                    return false;
                }
            }
            
            bool lossy = false;
            
            
            if ((lhs->Basic.flags & (BasicFlag_Floating | BasicFlag_Integer)) &&
                (rhs->Basic.flags & (BasicFlag_Floating | BasicFlag_Integer))) {
                
                if (is_rhs_value) {
                    return true;
                }
                
                bool lhs_float = lhs->Basic.flags & BasicFlag_Floating;
                bool rhs_float = rhs->Basic.flags & BasicFlag_Floating;
                if (lhs_float == rhs_float) {
                    lossy = lhs->size < rhs->size;
                } else {
                    lossy = rhs_float;
                }
                
            } else if (rhs->Basic.kind == Basic_string) {
                lossy = (lhs->Basic.flags & BasicFlag_String) == 0;
                
            } else if (rhs->Basic.kind == Basic_cstring) {
                lossy = rhs->Basic.kind != Basic_cstring;
            }
            
            
#if 0
            bool lossy = lhs->size < rhs->size;
            if (!lossy && ) {
                lossy = rhs->Basic.flags & BasicFlag_Integer;
            }
            
            if (!lossy && rhs->Basic.flags & BasicFlag_Floating) {
                lossy = lhs->Basic.flags & BasicFlag_Integer;
            }
            
            if (!lossy && rhs->Basic.flags & BasicFlag_Integer) {
                lossy = (lhs->Basic.flags & BasicFlag_Floating | BasicFlag_Integer);
            }
#endif
            
            
            if (op == Op_Logical_And || op == Op_Logical_Or) {
                if (((lhs->Basic.flags & BasicFlag_String) || (lhs->Basic.flags & BasicFlag_Integer)) &&
                    ((rhs->Basic.flags & BasicFlag_String) || (rhs->Basic.flags & BasicFlag_Integer))) {
                    lossy = false;
                }
            }
            
            if (lossy) {
                if (report_error) {
                    type_error(tcx, 
                               string_print("conversion from `%` to `%`, possible loss of data",
                                            f_type(rhs), f_type(lhs)),
                               span);
                }
                return false;
            }
        } break;
        
        case TypeKind_Array: {
            bool success = type_equals(lhs->Array.type, rhs->Array.type);
            
            if (lhs->Array.capacity != rhs->Array.capacity) {
                success = false;
            }
            
            if (lhs->Array.is_dynamic != rhs->Array.is_dynamic) {
                success = false;
            }
            
            if (!success) {
                if (report_error) {
                    type_error_mismatch(tcx, lhs, rhs, span);
                }
                return false;
            }
            
        } break;
        
        case TypeKind_Struct:
        case TypeKind_Union: {
            // TODO(Alexander): this will not work for anonymous structs
            if (lhs != rhs) {
                if (report_error) {
                    type_error_mismatch(tcx, lhs, rhs, span);
                }
                return false;
            }
        } break;
        
        case TypeKind_Pointer: {
            if (lhs->Pointer && lhs->Pointer->kind == TypeKind_Void) {
                return true;
                
            } else {
                // Strict type check for non void* assignment
                bool success = type_equals(lhs->Pointer, rhs->Pointer);
                if (!success) {
                    if (report_error) {
                        type_error_mismatch(tcx, lhs, rhs, span);
                    }
                    return false;
                }
            }
        } break;
        
        case TypeKind_Function: {
            array(Type*)* lhs_args = lhs->Function.arg_types;
            array(Type*)* rhs_args = rhs->Function.arg_types;
            
            if (array_count(lhs_args) != array_count(rhs_args)) {
                return false;
            }
            
            for_array_v(lhs->Function.arg_types, la, arg_index) {
                if (!type_equals(la, rhs_args[arg_index])) {
                    return false;
                }
            }
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return true;
}

bool
type_check_expression(Type_Context* tcx, Ast* expr) {
    bool result = true;
    
    switch (expr->kind) {
        case Ast_Call_Expr: {
            Type* function_type = expr->Call_Expr.function_type;
            assert(function_type->kind == TypeKind_Function);
            
            Type_Function* t_func = &function_type->Function;
            
#if 0
            if (!t_func->unit && !t_func->intrinsic) {
                type_error(tcx, 
                           string_print("function `%` has no body", f_var(t_func->ident)),
                           expr->span);
            }
#endif
            
            
            int formal_arg_count = (int) array_count(t_func->arg_types);
            int actual_arg_count = 0;
            {
                for_compound(expr->Call_Expr.args, _) {
                    actual_arg_count++;
                }
            }
            
            if (actual_arg_count == 0 && t_func->first_default_arg_index > 0) {
                type_error(tcx, 
                           string_print("function `%` expected % argument(s)",
                                        f_var(t_func->ident),
                                        f_int(formal_arg_count)),
                           expr->span);
                return false;
                
            } else if (actual_arg_count < t_func->first_default_arg_index ||
                       (actual_arg_count > formal_arg_count && !t_func->is_variadic)) {
                //pln("arg_index >= % && arg_index <= % && variadic = %", t_func->first_default_arg_index, formal_arg_count, t_func->is_variadic);
                
                
                // NOTE(Alexander): it is allowed to have more arguments only if the function is variadic
                type_error(tcx, string_print("function `%` did not take % arguments, expected % arguments", 
                                             f_var(t_func->ident), 
                                             f_int(actual_arg_count), 
                                             f_int(t_func->first_default_arg_index)),
                           expr->span);
                return false;
            }
            
            
            int arg_index = 0;
            for_compound(expr->Call_Expr.args, arg) {
                type_check_expression(tcx, arg->Argument.assign);
                
                if (arg_index < formal_arg_count) {
                    if (t_func->is_variadic && arg_index == formal_arg_count - 1) {
                        continue;
                    }
                    
                    
                    if (arg->Argument.ident && arg->Argument.ident->kind == Ast_Ident) {
                        string_id arg_ident = ast_unwrap_ident(arg->Argument.ident);
                        smm index = map_get_index(t_func->ident_to_index, arg_ident);
                        if (index > 0) {
                            arg_index = t_func->ident_to_index[index].value;
                            assert(arg_index < array_count(t_func->arg_types));
                        } else {
                            type_error(tcx, 
                                       string_print("undeclared parameter with name `%`", 
                                                    f_var(arg_ident)), 
                                       expr->span);
                            result = false;
                            break;
                        }
                    }
                    Type* arg_type = t_func->arg_types[arg_index];
                    if (!type_equals(arg_type, arg->Argument.assign->type)) {
                        type_error_mismatch(tcx, arg_type, 
                                            arg->Argument.assign->type, 
                                            arg->Argument.assign->span);
                        
                        break;
                    }
                }
                
                arg_index++;
            }
            
            
            if (t_func->is_variadic) {
                // TODO(Alexander): hack put ast nodes in other place
                Ast* var_args = arena_push_struct(&tcx->type_arena, Ast);
                var_args->kind = Ast_Exported_Type;
                var_args->Exported_Type = export_var_args_info(&tcx->type_info_packer,
                                                               formal_arg_count - 1,
                                                               expr->Call_Expr.args);
                expr->Call_Expr.var_args = var_args;
            }
        } break;
        
        case Ast_Unary_Expr: {
            
            result = type_check_expression(tcx, expr->Binary_Expr.first);
            
            if (expr->Unary_Expr.overload) {
                break;
            }
            
            Operator op = expr->Unary_Expr.op;
            Type* first = expr->Unary_Expr.first->type;
            
            
            switch (op) {
                case Op_Negate: {
                    
                    if (first->kind == TypeKind_Pointer || first->kind == TypeKind_Function) {
                        
                    } else if (first->kind == TypeKind_Basic && (first->Basic.flags & (BasicFlag_Integer | BasicFlag_Floating))) {
                    } else {
                        type_error(tcx, 
                                   string_print("unary operator `%` expects integral value, found `%`", 
                                                f_cstring(operator_strings[op]), 
                                                f_type(first)),
                                   expr->span);
                    }
                    
                } break;
            }
            
        } break;
        
        case Ast_Binary_Expr: {
            result = type_check_expression(tcx, expr->Binary_Expr.first);
            result = result && type_check_expression(tcx, expr->Binary_Expr.second);
            
            if (expr->Binary_Expr.overload) {
                break;
            }
            
            Operator op = expr->Binary_Expr.op;
            Type* first = expr->Binary_Expr.first->type;
            Type* second = expr->Binary_Expr.second->type;
            
            if (first->kind == TypeKind_Pointer || first->kind == TypeKind_Function) {
                if (op == Op_Add || 
                    op == Op_Subtract ||
                    op == Op_Add_Assign || 
                    op == Op_Subtract_Assign ||
                    op == Op_Logical_And ||
                    op == Op_Logical_Or) {
                    
                    if (second->kind == TypeKind_Basic) {
                        if ((second->Basic.flags & BasicFlag_Integer) == 0) {
                            type_error(tcx, 
                                       string_print("`% % %` expects integral value on right-hand side", 
                                                    f_type(first), 
                                                    f_cstring(operator_strings[op]), 
                                                    f_type(second)),
                                       expr->span);
                        }
                    } else if (second->kind != TypeKind_Pointer && second->kind != TypeKind_Function) {
                        type_error(tcx, 
                                   string_print("operator `%` expects integral or pointer on right-hand side, found `%`", 
                                                f_cstring(operator_strings[op]),
                                                f_type(second)),
                                   expr->span);
                    }
                } else if (operator_is_comparator(op)) {
                    if (second->kind != TypeKind_Pointer && second->kind != TypeKind_Function) {
                        
                        if (expr->Binary_Expr.second->kind == Ast_Value) {
                            expr->Binary_Expr.second->type = first;
                        } else {
                            type_error(tcx, 
                                       string_print("operator `%` expects pointer on right-hand side, found `%`", 
                                                    f_cstring(operator_strings[op]),
                                                    f_type(second)),
                                       expr->span);
                        }
                    }
                } else if (op == Op_Assign) {
                    result = result && type_check_assignment(tcx, first, second, expr->Binary_Expr.second->kind == Ast_Value, expr->Binary_Expr.second->span);
                } else {
                    type_error(tcx, 
                               string_print("operator `%` is not supported for `%` on left-hand side",
                                            f_cstring(operator_strings[op]),
                                            f_type(first)),
                               expr->span);
                }
                
            } else if (first->kind == TypeKind_Basic || 
                       first->kind == TypeKind_Enum || 
                       op == Op_Assign) {
                
                if (type_check_assignment(tcx, first, second, is_ast_value(expr->Binary_Expr.second), expr->Binary_Expr.second->span, op)) {
                    if (!type_equals(first, second)) {
                        pln("%", f_ast(expr));
                        type_error_mismatch(tcx, first, second, expr->Binary_Expr.second->span);
                        result = false;
                    }
                } else {
                    pln("%", f_ast(expr));
                }
            } else {
                type_error(tcx, string_print("unexpected type `%` on left-hand side for operator `%`", 
                                             f_type(first), f_cstring(operator_strings[op])), 
                           expr->Binary_Expr.first->span);
                result = false;
            }
        } break;
        
        case Ast_Cast_Expr: {
            result = type_check_expression(tcx, expr->Cast_Expr.expr);
            // TODO(Alexander): do we want any checking here?
        } break;
        
        case Ast_Paren_Expr: {
            result = type_check_expression(tcx, expr->Paren_Expr.expr);
        } break;
        
        case Ast_Index_Expr: {
            assert(expr->Index_Expr.array);
            assert(expr->Index_Expr.index);
            Type* type = expr->Index_Expr.array->type;
            
            if (expr->Index_Expr.index->kind == Ast_Value) {
                Value index_value = expr->Index_Expr.index->Value;
                smm index = value_to_smm(index_value);
                if (index < 0) {
                    type_error(tcx, string_print("invalid index `%`", f_smm(index)), expr->span);
                    result = false;
                }
                
                if (type->Array.capacity > 0) {
                    if (index > type->Array.capacity - 1) {
                        type_error(tcx, 
                                   string_print("array index `%` exceeds capacity `%`",
                                                f_smm(index), f_smm(type->Array.capacity)),
                                   expr->span);
                    }
                } else {
                    // TODO(Alexander): insert runtime checks
                }
            } else {
                // TODO(Alexander): insert runtime checks
            }
            
        } break;
    }
    
    return result;
}

bool
type_check_statement(Type_Context* tcx, Ast* stmt) {
    bool result = true;
    
    switch (stmt->kind) {
        case Ast_None: break;
        
        case Ast_Assign_Stmt: {
            if (stmt->Assign_Stmt.expr) {
                result = result && type_check_expression(tcx, stmt->Assign_Stmt.expr);
            }
            Type* expected_type = stmt->type;
            assert(expected_type && "compiler bug: assign statement has no type");
            
            Type* found_type = stmt->Assign_Stmt.expr->type;
            if (found_type) {
                result = result &&  type_check_assignment(tcx, expected_type, found_type, is_ast_value(stmt->Assign_Stmt.expr),
                                                          stmt->Assign_Stmt.expr->span);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            result = result && type_check_expression(tcx, stmt->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                result = result && type_check_statement(tcx, it);
            }
        } break;
        
        case Ast_Decl_Stmt: {
            if (stmt->Decl_Stmt.stmt) {
                result = result && type_check_statement(tcx, stmt->Decl_Stmt.stmt);
            }
        } break;
        
        case Ast_If_Stmt: {
            result = result && type_check_expression(tcx, stmt->If_Stmt.cond);
            result = result && type_check_statement(tcx, stmt->If_Stmt.then_block);
            result = result && type_check_statement(tcx, stmt->If_Stmt.else_block);
            // TODO(Alexander): check that the condition is a boolean
        } break;
        
        case Ast_Break_Stmt:
        case Ast_Continue_Stmt: {
            // TODO(Alexander): check that this statement is inside a loop
        } break;
        
        case Ast_For_Stmt: {
            result = result && type_check_statement(tcx, stmt->For_Stmt.init);
            result = result && type_check_expression(tcx, stmt->For_Stmt.cond);
            result = result && type_check_expression(tcx, stmt->For_Stmt.update);
            result = result && type_check_statement(tcx, stmt->For_Stmt.block);
            // TODO(Alexander): check that the condition is a boolean
        } break;
        
        case Ast_While_Stmt: {
            result = result && type_check_expression(tcx, stmt->While_Stmt.cond);
            result = result && type_check_statement(tcx, stmt->While_Stmt.block);
            // TODO(Alexander): check that the condition is a boolean
        } break;
        
        case Ast_Switch_Stmt: {
            //__debugbreak();
            result = result && type_check_expression(tcx, stmt->Switch_Stmt.cond);
            result = result && type_check_assignment(tcx, t_s64, 
                                                     stmt->Switch_Stmt.cond->type, false, 
                                                     stmt->Switch_Stmt.cond->span);
            for_compound(stmt->Switch_Stmt.cases, it) {
                if (it->Switch_Case.stmt) {
                    result = result && type_check_statement(tcx, it->Switch_Case.stmt);
                }
            }
        } break;
        
        case Ast_Return_Stmt: {
            assert(tcx->return_type);
            
            Type* found_type = stmt->type;
            if (!found_type) {
                type_error(tcx, string_lit("compiler bug! No return type was found in return statement"), stmt->span);
                pln("%", f_ast(stmt));
            }
            
            result = result && type_check_expression(tcx, stmt->Return_Stmt.expr);
            result = result && type_check_assignment(tcx, tcx->return_type, found_type, 
                                                     is_ast_value(stmt->Return_Stmt.expr),
                                                     stmt->Return_Stmt.expr->span);
            
            if (!result) {
                pln("%", f_ast(stmt));
            }
            
        } break;
        
        default: assert(0 && stmt->kind);
    }
    
    return result;
}

bool
type_infer_ast(Type_Context* tcx, Interp* interp, Compilation_Unit* cu,
               bool report_error) {
    
    bool result = true;
    Ast* ast = cu->ast;
    cu->interp = interp;
    
    tcx->return_type = 0;
    if (ast->kind == Ast_Decl_Stmt) {
        Type* type = ast->Decl_Stmt.type->type;
        ast->type = type;
        
        if (type && type->kind == TypeKind_Function) {
            push_type_scope(tcx);
            type->Function.unit = cu;
            
            // Store the arguments in local context
            Type_Function* func = &type->Function;
            if (func->arg_idents) {
                for (int arg_index = 0; 
                     arg_index < array_count(func->arg_idents);
                     arg_index++) {
                    
                    Type* arg_type = func->arg_types[arg_index];
                    if (arg_type) {
                        string_id ident = func->arg_idents[arg_index];
                        // TODO(Alexander): maybe we need to also store spans in procedure type?
                        if (!push_local(tcx, ident, arg_type, ast->span, report_error)) {
                            result = false;
                        }
                    }
                }
            }
            
            tcx->return_type = type->Function.return_type;
        }
    }
    
    if (!result) {
        return result;
    }
    
    if (is_ast_type(ast)) {
        string_id ident = cu->ident;
        Type* type = save_type_declaration_from_ast(tcx, ident, ast, report_error);
        if (!type) {
            result = false;
        }
        
    } else if (is_ast_stmt(ast)) {
        Type* type = type_infer_statement(tcx, ast, report_error);
        result = type;
    } else {
        assert(0 && "illegal type: expected X_Stmt or any X_Type node");
    }
    
    while (tcx->active_scope) {
        pop_type_scope(tcx);
    }
    
    map_free(tcx->local_type_table);
    map_free(tcx->locals);
    
    tcx->block_depth = 0;
    
    return result;
}

bool
type_check_ast(Type_Context* tcx, Compilation_Unit* cu) {
    Ast* ast = cu->ast;
    if (ast->kind == Ast_Decl_Stmt) {
        Type* type = ast->Decl_Stmt.type->type;
        if (type && type->kind == TypeKind_Function) {
            tcx->return_type = type->Function.return_type;
        }
    }
    
    bool result = type_check_statement(tcx, ast);
    if (result) {
        
        if (cu->ast->kind != Ast_Decl_Stmt) {
            //pln("interp statement:%\n", f_ast(ast));
            Interp_Value interp_result = interp_statement(cu->interp, ast);
            cu->interp_result = interp_result.value;
        }
    }
    
    return result;
}

void
flush_stdout() {
    fflush(stdout);
}

void
DEBUG_setup_intrinsic_types(Type_Context* tcx) {
    
#define _push_intrinsic(type, name, _is_variadic, interp_intrinsic_fp, intrinsic_fp, _return_type) \
Type* type = arena_push_struct(&tcx->type_arena, Type); \
{ \
type->kind = TypeKind_Function; \
type->Function.is_variadic = _is_variadic; \
    \
string_id ident = vars_save_cstring(#name); \
type->Function.ident = ident; \
type->Function.unit = 0; \
type->Function.interp_intrinsic = interp_intrinsic_fp; \
type->Function.intrinsic = intrinsic_fp; \
type->Function.return_type = normalize_basic_types(_return_type); \
    \
save_type_declaration(tcx, ident, type, empty_span, false); \
}
#define push_intrinsic(name, is_variadic, interp_intrinsic_fp, intrinsic_fp, _return_type) \
_push_intrinsic(intrin_##name, name, is_variadic, interp_intrinsic_fp, intrinsic_fp, _return_type) 
    
#define _push_intrinsic_arg(intrin_name, arg_name, arg_type) \
{ \
string_id arg_ident = vars_save_cstring(#arg_name); \
array_push(intrin_name->Function.arg_idents, arg_ident); \
array_push(intrin_name->Function.arg_types, normalize_basic_types(arg_type)); \
intrin_name->Function.first_default_arg_index++; \
}
#define push_intrinsic_arg(name, arg_name, arg_type) _push_intrinsic_arg(intrin_##name, arg_name, arg_type)
    
    // TODO(Alexander): these are kind of temporary, since we don't really have
    // the ability to create these functions yet, need FFI!
    // We will still have intrinsics but these intrinsics are just for debugging
    
#if 0
    // Intrinsic syntax: void print_format(string format...)
    // e.g. print_format %, lucky number is %\n", "world", 7);
    push_intrinsic(print, true, &interp_intrinsic_print, &print, t_void);
    push_intrinsic_arg(print, format, t_cstring);
#endif
    
    // Intrinsic syntax: void debug_break()
    // Inserts a breakpoint (e.g. int3 on x64) to enable debugger
    push_intrinsic(debug_break, false, &interp_intrinsic_debug_break, &interp_intrinsic_debug_break, t_void);
    
    // Intrinsic syntax: void __debugbreak()
    // Inserts a breakpoint (e.g. int3 on x64) to enable debugger
    push_intrinsic(__debug_break, false, &interp_intrinsic_debug_break, &interp_intrinsic_debug_break, t_void);
    
#if 0
    // Intrinsic syntax: void __assert(int test, cstring msg cstring file, smm line)
    // Assets that expr is true, used as test case
    push_intrinsic(__assert, false, &interp_intrinsic_assert, &intrinsic_assert, t_bool);
    push_intrinsic_arg(__assert, test, t_int);
    push_intrinsic_arg(__assert, msg, t_cstring);
    push_intrinsic_arg(__assert, file, t_cstring);
    push_intrinsic_arg(__assert, line, t_smm);
#endif
    
    // Intrinsic syntax: umm sizeof(T)
    push_intrinsic(sizeof, false, 0, &type_sizeof, t_umm);
    push_intrinsic_arg(sizeof, T, t_any);
    
    // Intrinsic syntax: umm alignof(T)
    push_intrinsic(alignof, false, 0, &type_alignof, t_umm);
    push_intrinsic_arg(alignof, T, t_any);
    
    push_intrinsic(type_of, false, 0, &type_of, t_type);
    push_intrinsic_arg(type_of, T, t_any);
    
    /******************************************************
    /* X64 architecture specific intrinsics
/******************************************************/
    
    // Intrinsic syntax: u64 rdtsc()
    push_intrinsic(rdtsc, false, 0, &x64_intrin_rdtsc, t_u64);
}

s32
type_check_ast_file(Type_Context* tcx, Ast_File* ast_file, Interp* interp) {
    
    // Forward declare structs to be filled in later
    for_array(ast_file->units, cu, _a) {
        Ast* ast = cu->ast;
        if ((ast->kind == Ast_Struct_Type || ast->kind == Ast_Union_Type) && cu->ident) {
            // Forward declare structs and unions so they can refer to themselves
            smm type_index = map_get_index(tcx->global_type_table, cu->ident);
            if (type_index == -1) {
                Type* type = arena_push_struct(&tcx->type_arena, Type);
                type->kind = ast->kind == Ast_Struct_Type ? TypeKind_Struct : TypeKind_Union;
                map_put(tcx->global_type_table, cu->ident, type);
            }
        }
    }
    
    DEBUG_setup_intrinsic_types(tcx);
    
    // Push all compilation units to queue
    array(Compilation_Unit*)* queue = 0;
    
    for_array(ast_file->units, cu, _b) {
        if (is_ast_type(cu->ast)) {
            array_push(queue, cu);
        }
    }
    // Push actual declarations last
    for_array(ast_file->units, cu, _c) {
        if (is_ast_stmt(cu->ast)) {
            array_push(queue, cu);
        }
    }
    
    // NOTE(Alexander): exit condition: assumes that when all items in queue failed then we can
    //                  nolonger process any further and have to report errors and exit.
    smm last_queue_count = 0;
    while (last_queue_count != array_count(queue)) {
        smm queue_count = array_count(queue);
        for (smm queue_index = 0; queue_index < queue_count; queue_index++) {
            Compilation_Unit* comp_unit = queue[queue_index];
            
            //if (comp_unit->ident == 1514) {
            //__debugbreak();
            //}
            
            if (!type_infer_ast(tcx, interp, comp_unit, false)) {
                // Failed, retry later
                array_push(queue, comp_unit);
            }
        }
        
        // Remove processed nodes in queue
        array_remove_n(queue, 0, queue_count);
        last_queue_count = queue_count;
    }
    
    // NOTE(Alexander): anything left in the queue we report errors for
    for_array_v(queue, comp_unit, _d) {
        //if (tcx.error_count > 10) {
        //pln("Found more than 10 errors, exiting...");
        //return tcx.error_count;
        //}
        
        type_infer_ast(tcx, interp, comp_unit, true);
    }
    
    if (tcx->error_count == 0) {
        // Run type checking on statements
        for_array(ast_file->units, cu, _e) {
            if (is_ast_stmt(cu->ast)) {
                type_check_ast(tcx, cu);
            }
        }
    }
    
    return tcx->error_count;
}


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
    
    //if (type->kind == TypeKind_Enum) {
    //type = type->Enum.type;
    //if (!type) {
    //type = t_s64; // TODO(Alexander): what is the default type for Enums?
    //} 
    //}
    
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
                        
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
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
                        type_warning(tcx, string_format("expected type `%` signed/ unsigned mismatch with `%`", 
                                                        f_type(type), f_value(&value)), span);
                        
                    } else if (value.data.unsigned_int > type->Basic.limits.max_unsigned_value) {
                        
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
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
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
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
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
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
        
        case Value_ast_node: {
            compiler_bug("It shouldn't be possible to use ast node as a type");
        } break;
    }
    
    if (report_error && !result) {
        type_error(tcx, string_format("expected type `%` is not compatible with `%`", 
                                      f_type(type), f_value(&value)), span);
    }
    
    return result;
}

// TODO(Alexander): this is a compile opt technique, this belongs in different file
Value
constant_folding_of_expressions(Ast* ast) {
    assert(is_ast_expr(ast) || 
           ast->kind == Ast_Value || 
           ast->kind == Ast_Ident || 
           ast->kind == Ast_None);
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
                    // float -X-> int (is a no no, it has to be an explicit cast)
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
                        
                    } else if (is_integer(first) && is_integer(second)) {
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
        
        case Ast_Paren_Expr: {
            result = constant_folding_of_expressions(ast->Paren_Expr.expr);
        } break;
    }
    
    if (result.type != Value_void) {
        ast->kind = Ast_Value;
        ast->Value.value = result;
        //ast->type = 0;
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
                    expr->Value.value = value_cast(expr->Value.value, result->Basic.kind);
                } else {
                    result = type_infer_value(tcx, expr->Value.value);
                }
            }
            
            result = normalize_basic_types(result);
            
            // HACK(Alexander): auto converting string to cstring
            if (is_string(expr->Value.value)) {
                if (parent_type && parent_type->kind == TypeKind_Basic &&
                    parent_type->Basic.kind == Basic_cstring) {
                    result = parent_type;
                    expr->Value.value.data.cstr = string_to_cstring(expr->Value.value.data.str);
                    expr->Value.value.type = Value_cstring;
                }
            }
            
            type_check_value(tcx, result, expr->Value.value, expr->span, report_error);
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
                    if (tcx->set_undeclared_to_s64) {
                        result = t_s64;
                    } else if (report_error) {
                        // NOTE(Alexander): copypasta
                        type_error(tcx, 
                                   string_format("`%` is an undeclared identifier", 
                                                 f_string(vars_load_string(expr->Ident))),
                                   expr->span);
                    }
                }
            }
            expr->type = result;
        } break;
        
        case Ast_Unary_Expr: {
            if (parent_type) {
                if (expr->Unary_Expr.op == UnaryOp_Dereference) {
                    Type* new_parent_type = arena_push_struct(&tcx->type_arena, Type);
                    new_parent_type->kind = TypeKind_Pointer;
                    new_parent_type->Pointer = parent_type;
                    new_parent_type->size = sizeof(umm);
                    new_parent_type->align = alignof(umm);
                    parent_type = new_parent_type;
                }
            }
            
            Type* type = type_infer_expression(tcx, expr->Unary_Expr.first, parent_type, report_error);
            
            if (type) {
                switch (expr->Unary_Expr.op) {
                    case UnaryOp_Dereference: {
                        if (type->kind == TypeKind_Pointer) {
                            result = type->Pointer;
                        } else {
                            if (report_error) {
                                pln("%", f_ast(expr));
                                type_error(tcx,
                                           string_format("cannot dereference type `%`", f_type(type)),
                                           expr->span);
                            }
                        }
                    } break;
                    
                    case UnaryOp_Address_Of: {
                        result = arena_push_struct(&tcx->type_arena, Type);
                        result->kind = TypeKind_Pointer;
                        result->Pointer = type;
                        result->size = sizeof(umm);
                        result->align = alignof(umm);
                    } break;
                    
                    default: {
                        result = type;
                    } break;
                }
                
                expr->type = result;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Binary_Op op = expr->Binary_Expr.op;
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
            
            if (is_binary_assign(expr->Binary_Expr.op)) {
                
                
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
                
                if (first_type->kind == TypeKind_Basic && 
                    second_type->kind == TypeKind_Basic) {
                    
                    if ((first_type->Basic.flags & BasicFlag_String) ||
                        (second_type->Basic.flags & BasicFlag_String)) {
                        if (op == BinaryOp_Logical_Or || op == BinaryOp_Logical_And) {
                            result = t_bool;
                        } else {
                            result = second_type;
                        }
                        
                    } else if ((first_type->Basic.flags & BasicFlag_Floating) == 
                               (second_type->Basic.flags & BasicFlag_Floating)) {
                        result = first_type->size > second_type->size ? first_type : second_type;
                        
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
                } else {
                    if (parent_type) {
                        result = parent_type;
                    } else {
                        result = first_type;
                    }
                    //result = normalize_basic_types(t_smm);
                }
                
                first_type = result;
                second_type = result;
            }
            
            if (result) {
                
                // TODO(Alexander): we don't want to change types without doing explicit cast
                // otherwise the backend gets confused and outputs wrong code.
                if (type_check_assignment(tcx, first_type, expr->Binary_Expr.first->type,
                                          empty_span, op, false)) {
                    
                    if (type_equals(expr->Binary_Expr.first->type, first_type)) {
                        expr->Binary_Expr.first->type = first_type;
                    } else {
                        if (expr->Binary_Expr.first->kind == Ast_Value) {
                            expr->Binary_Expr.first->type = first_type;
                        } else {
                            // TODO(Alexander): temporary use of calloc we need to find a better way to manipulate the Ast
                            Ast* cast_node = (Ast*) calloc(1, sizeof(Ast));
                            cast_node->kind = Ast_Cast_Expr;
                            cast_node->Cast_Expr.type = (Ast*) calloc(1, sizeof(Ast));
                            cast_node->Cast_Expr.type->kind = Ast_None;
                            cast_node->Cast_Expr.expr = expr->Binary_Expr.first;
                            cast_node->type = first_type;
                            
                            expr->Binary_Expr.first = cast_node;
                            
                            //pln("first AST:\n%", f_ast(expr));
                        }
                    }
                }
                
                if (type_check_assignment(tcx, second_type, expr->Binary_Expr.second->type,
                                          empty_span, op, false)) {
                    
                    if (type_equals(expr->Binary_Expr.second->type, second_type)) {
                        expr->Binary_Expr.second->type = second_type;
                    } else {
                        if (expr->Binary_Expr.second->kind == Ast_Value) {
                            expr->Binary_Expr.second->type = second_type;
                        } else {
                            // TODO(Alexander): temporary use of calloc we need to find a better way to manipulate the Ast
                            Ast* cast_node = (Ast*) calloc(1, sizeof(Ast));
                            cast_node->kind = Ast_Cast_Expr;
                            cast_node->Cast_Expr.type = (Ast*) calloc(1, sizeof(Ast));
                            cast_node->Cast_Expr.type->kind = Ast_None;
                            cast_node->Cast_Expr.expr = expr->Binary_Expr.second;
                            cast_node->type = second_type;
                            
                            expr->Binary_Expr.second = cast_node;
                            
                            //pln("second AST:\n%", f_ast(expr));
                        }
                        
                    }
                }
                
                if (binary_is_comparator_table[expr->Binary_Expr.op]) {
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
            
            Type* function_type = type_infer_expression(tcx, 
                                                        expr->Call_Expr.ident, 
                                                        parent_type, 
                                                        report_error);
            
            if (!function_type) {
                break;
            }
            if (function_type->kind != TypeKind_Function) {
                if (report_error) {
                    string_id function_ident = ast_unwrap_ident(expr->Call_Expr.ident);
                    type_error(tcx, string_format("`%` is not a function", f_var(function_ident)),
                               expr->span);
                }
                break;
            }
            
            
            expr->Call_Expr.function_type = function_type;
            result = function_type->Function.return_type;
            expr->type = result;
            
            // Arguments
            // TODO(Alexander): nice to have: support for keyworded args, default args
            Type_Function* proc = &function_type->Function;
            Ast* actual_args = expr->Call_Expr.args;
            s32 arg_index = 0;
            {
                for_compound(actual_args, actual_arg) {
                    Type* formal_type = 0;
                    
                    if (arg_index < array_count(proc->arg_idents)) {
                        string_id ident = proc->arg_idents[arg_index];
                        formal_type = proc->arg_types[arg_index];
                    }
                    
                    constant_folding_of_expressions(actual_arg->Argument.assign);
                    Type* actual_type = type_infer_expression(tcx, 
                                                              actual_arg->Argument.assign, 
                                                              formal_type, 
                                                              report_error);
                    
                    
                    if (actual_type) {
                        if (formal_type && !type_equals(formal_type, actual_type)) {
                            if (actual_arg->Argument.assign->kind == Ast_Value) {
                                actual_arg->Argument.assign->type = formal_type;
                                actual_type = formal_type;
                            }
                        }
                        
                        actual_arg->type = actual_type;
                    } else {
                        result = 0;
                        break;
                    }
                    
                    arg_index++;
                }
            }
            
            // HACK(Alexander): for now print_format pushes the format type first then the value 
            if (result && proc->intrinsic) {
                if (proc->intrinsic == &print_format) {
                    //pln("ast = %", f_ast(actual_args));
                    
                    arg_index = 0;
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
                            fmt_value->Value.value = create_signed_int_value(fmt_type);
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
                } else if (proc->intrinsic == &type_sizeof || proc->intrinsic == &type_alignof) {
                    intrin_type_def* intrinsic_proc = (intrin_type_def*) proc->intrinsic;
                    
                    arg_index = 0;
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
                            expr->Value.value =
                                create_unsigned_int_value(intrinsic_proc(actual_arg->type));
                        }
                    }
                }
            }
        } break;
        
        case Ast_Cast_Expr: {
            Type* type = expr->type;
            if (!type) {
                type = create_type_from_ast(tcx, expr->Cast_Expr.type, report_error);
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
            type_infer_expression(tcx, expr->Index_Expr.index, t_smm, report_error);
            Type* type = type_infer_expression(tcx, expr->Index_Expr.array, parent_type, report_error);
            if (type) {
                if (type->kind == TypeKind_Array) {
                    result = type->Array.type;
                } else {
                    if (report_error) {
                        type_error(tcx, string_lit("index operator expects an array"), expr->span);
                    }
                }
                expr->type = result;
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
            
            //if (type->kind == TypeKind_Pointer) {
            //pln("%", f_ast(expr));
            //}
            
            // TODO(Alexander): add support for unions etc.
            
            switch (type->kind) {
                case TypeKind_Struct: {
                    smm index = map_get_index(type->Struct.ident_to_index, field_ident);
                    if (index >= 0) {
                        result = type->Struct.types[index];
                    }
                } break;
                
                case TypeKind_Enum: {
                    smm index = map_get_index(type->Enum.values, field_ident);
                    if (index >= 0) {
                        Value value = type->Enum.values[index].value;
                        expr->kind = Ast_Value;
                        expr->Value.value = value;
                    }
                    result = type;
                } break;
                
                case TypeKind_Array: {
                    // NOTE(Alexander): Array are actually structs that has 2-3 fields, 
                    // - data: which is just a raw pointer to the first element
                    // - count: the number of elements 
                    // - capacity (only applicable for growable arrays): the number of allocated elements
                    
                    switch (field_ident) {
                        case Sym_data: {
                            result = arena_push_struct(&tcx->type_arena, Type);
                            result->kind = TypeKind_Pointer;
                            result->Pointer = type->Array.type; 
                        } break;
                        
                        case Sym_count: {
                            result = normalize_basic_types(t_smm);
                            
                            if (type->Array.capacity > 0) {
                                Value capacity = {};
                                capacity.type = Value_signed_int;
                                capacity.data.signed_int = type->Array.capacity;
                                
                                expr->kind = Ast_Value;
                                expr->Value.value = capacity;
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
                                result = arena_push_struct(&tcx->type_arena, Type);
                                result->kind = TypeKind_Pointer;
                                result->Pointer = t_u8;
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
                    type_error(tcx, string_format("type `%` doesn't have field `%`", f_type(type), f_var(field_ident)), expr->span);
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
                    if (!type_infer_expression(tcx, it, type, report_error)) {
                        result = 0;
                        break;
                    }
                }
                
                if (parent_type->Array.capacity > 0) {
                    if (parent_type->Array.capacity < count) {
                        if (report_error) {
                            type_error(tcx, string_format("array literal expects at least `%` values, found `%`",
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
                                type_error(tcx, string_format("cannot assign empty array literal to fixed-array of unknown capacity, did you mean to use a dynamic array [..]T?"), 
                                           expr->span);
                            }
                        }
                    }
                }
                
                expr->type = result;
            } else if (parent_type && parent_type->kind == TypeKind_Struct) {
                
                Type* type = parent_type;
                bool has_ident = false;
                bool has_errors = false;
                int next_field_index = 0;
                for_compound(expr->Aggregate_Expr.elements, field) {
                    int field_index = next_field_index++;
                    
                    if (field->Argument.ident) {
                        string_id ident = ast_unwrap_ident(field->Argument.ident);
                        smm idx = map_get_index(type->Struct.ident_to_index, ident);
                        if (idx == -1) {
                            if (report_error) {
                                type_error(tcx, string_format("`%` undeclared identifier in struct `%`",
                                                              f_string(vars_load_string(ident)),
                                                              f_string(vars_load_string(type->ident))),
                                           expr->span);
                            }
                            continue;
                        }
                        field_index = type->Struct.ident_to_index[idx].value;
                        has_ident = true;
                    } else if (has_ident) {
                        type_error(tcx, string_lit("cannot combine named and anonymous values in struct literal"),
                                   expr->span);
                        has_errors = true;
                        break;
                    }
                    
                    Type* field_type = 0;
                    if (next_field_index <= array_count(type->Struct.types)) {
                        field_type = type->Struct.types[field_index];
                    } else {
                        if (report_error) {
                            type_error(tcx, string_format("too many fields `%`, expected only `%`",
                                                          f_string(vars_load_string(type->ident)),
                                                          f_int(array_count(type->Struct.types))),
                                       expr->span);
                        }
                        has_errors = true;
                        break;
                    }
                    
                    if (field->Argument.assign) {
                        
                        constant_folding_of_expressions(field->Argument.assign);
                        type_infer_expression(tcx, field->Argument.assign, field_type, report_error);
                    } else {
                        type_infer_expression(tcx, field->Argument.ident, field_type, report_error);
                    }
                }
                
                if (!has_errors) {
                    expr->type = type;
                    result = type;
                }
                
            } else if (parent_type && parent_type->kind == TypeKind_Union) { 
                Type* type = parent_type;
                bool has_ident = false;
                bool has_errors = false;
                int next_field_index = 0;
                for_compound(expr->Aggregate_Expr.elements, field) {
                    
                }
                
            } else {
                if (report_error) {
                    pln("%", f_ast(expr));
                    pln("%", f_type(parent_type));
                    type_error(tcx, string_format("cannot assign aggregate initializer to non-aggregate type"), 
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


internal inline void 
push_type_to_struct_like(Type_Struct_Like* dest, Type* type, string_id ident) {
    dest->offset = align_forward(dest->offset, type->align);
    
    s32 index = (s32) array_count(dest->types);
    array_push(dest->types, type);
    array_push(dest->idents, ident);
    array_push(dest->offsets, dest->offset);
    map_put(dest->ident_to_index, ident, index);
    
    //string_id testid = vars_save_cstring("material");
    //if (ident == testid) {
    //pln("type->size = %", f_int(type->size));
    //}
    
    dest->offset += type->size;
    dest->size = dest->offset;
    
    if (type->align > dest->align) {
        dest->align = type->align;
    }
}


internal Type_Struct_Like
create_type_struct_like_from_ast(Type_Context* tcx, 
                                 Ast* arguments, 
                                 bool report_error) {
    
    Type_Struct_Like result = {};
    
    for_compound(arguments, argument) {
        assert(argument->kind == Ast_Argument);
        
        if (!argument->Argument.type) {
            break;
        }
        
        Ast* ast_type = argument->Argument.type;
        Type* type = create_type_from_ast(tcx, ast_type, report_error);
        
        if (!type) {
            result.has_error = true;
            return result;
        }
        
        switch (argument->Argument.ident->kind) {
            
            case Ast_Ident: {
                assert(argument->Argument.ident->kind == Ast_Ident);
                string_id ident = argument->Argument.ident->Ident;
                if (type) {
                    push_type_to_struct_like(&result, type, ident);
                } else {
                    result.has_error = true;
                }
            } break;
            
            case Ast_Compound: {
                
                if (type) {
                    for_compound(argument->Argument.ident, ast_ident) {
                        assert(ast_ident->kind == Ast_Ident);
                        string_id ident = ast_ident->Ident;
                        push_type_to_struct_like(&result, type, ident);
                    }
                } else {
                    result.has_error = true;
                }
            } break;
            
            default: {
                if (type->kind == TypeKind_Struct) {
                    for_array_v(type->Struct.idents, field_ident, field_index) {
                        Type* field_type = type->Struct.types[field_index];
                        push_type_to_struct_like(&result, field_type, field_ident);
                    }
                } else if (type->kind == TypeKind_Union) {
                    for_array_v(type->Union.idents, field_ident, field_index) {
                        Type* field_type = type->Union.types[field_index];
                        push_type_to_struct_like(&result, field_type, field_ident);
                    }
                    //pln("%", f_ast(ast_type));
                    //unimplemented;
                } else
                    // TODO(Alexander): should be a type error or something
                    unimplemented;
            }
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
                           string_format("`%` is an undeclared identifier", 
                                         f_string(vars_load_string(ident))),
                           span);
            }
        } 
    }
    
    return result;
}

Type*
type_wrap_pointer(Type_Context* tcx, Type* type) {
    Type* result = arena_push_struct(&tcx->type_arena, Type);
    result->kind = TypeKind_Pointer;
    result->Pointer = type;
    result->size = sizeof(smm);
    result->align = alignof(smm);
    return result;
}

Type*
create_type_from_ast(Type_Context* tcx, Ast* ast, bool report_error) {
    assert(is_ast_type(ast));
    
    Type* result = 0;
    switch (ast->kind) {
        case Ast_Named_Type: {
            string_id ident = ast->Named_Type->Ident;
            result = load_type_declaration(tcx, ident, ast->span, report_error);
            ast->type = result;
        } break;
        
        case Ast_Array_Type: {
            // Array capacity
            smm capacity = 0;
            if (ast->Array_Type.shape) {
                Value capacity_value = constant_folding_of_expressions(ast->Array_Type.shape);
                
                if (is_integer(capacity_value)) {
                    capacity = value_to_smm(capacity_value);
                } else {
                    if (report_error) {
                        type_error(tcx, string_lit("array shape should be an integer"), ast->span);
                    }
                    
                    break;
                }
            }
            
            Type* elem_type = create_type_from_ast(tcx, ast->Array_Type.elem_type, report_error);
            if (elem_type) {
                result = arena_push_struct(&tcx->type_arena, Type);
                result->kind = TypeKind_Array;
                result->Array.type = elem_type;
                result->Array.capacity = capacity;
                result->Array.is_dynamic = ast->Array_Type.is_dynamic;
                
                if (!result->Array.is_dynamic && result->Array.capacity > 0) {
                    // NOTE(Alexander): fixed size arrays with known size should be allocated directly
                    // TODO(Alexander): arch dep
                    result->size = (s32) (elem_type->size*result->Array.capacity);
                    result->align = elem_type->align;
                    result->Array.is_inplace = true;
                    //result->size = elem_type->size * (s32) result->Array.capacity;
                    //result->align = elem_type->align;
                } else {
                    result->size = sizeof(void*) + sizeof(smm);
                    // TODO(Alexander): for dynamic arrays we probably just want a pointer to this struct
                    //                  to avoid dangling pointers from reallocations!
                    if (result->Array.is_dynamic) {
                        result->size += sizeof(smm);
                    }
                    result->align = alignof(void*);
                }
            }
        } break;
        
        case Ast_Pointer_Type: {
            // TODO(Alexander): do we really want to store a new type declaration
            // for each time we use a pointer?!???
            
            Type* ptr_type = create_type_from_ast(tcx, ast->Pointer_Type, report_error);
            if (ptr_type) {
                result = type_wrap_pointer(tcx, ptr_type);
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
            result = arena_push_struct(&tcx->type_arena, Type);
            result->kind = TypeKind_Function;
            result->Function.is_variadic = false;
            Type* return_type = create_type_from_ast(tcx, ast->Function_Type.return_type, report_error);
            if (return_type) {
                result->Function.return_type = return_type;
            } else {
                return 0;
            }
            
            // NOTE(Alexander): Loads in the function arguments
            Ast* ast_arguments = ast->Function_Type.arguments;
            Type_Function* func = &result->Function;
            smm offset = 0;
            for_compound(ast_arguments, ast_argument) {
                assert(ast_argument->kind == Ast_Argument);
                if (!ast_argument->Argument.type) {
                    break;
                }
                
                Type* type = create_type_from_ast(tcx, ast_argument->Argument.type, report_error);
                if (type) {
                    string_id ident = ast_argument->Argument.ident->Ident;
                    s32 arg_index = (s32) array_count(func->arg_idents);
                    map_put(func->ident_to_index, ident, arg_index);
                    array_push(func->arg_idents, ident);
                    array_push(func->arg_types, type);
                    
                    if (!ast_argument->Argument.assign) {
                        if (func->first_default_arg_index + 1 == arg_index) {
                            func->first_default_arg_index++;
                        } else {
                            if (report_error) {
                                type_error(tcx, string_format("missing default argument for parameter %",
                                                              f_int(arg_index)), ast_argument->span);
                            }
                        }
                    }
                } else {
                    array_free(func->arg_idents);
                    array_free(func->arg_types);
                    return 0;
                }
            }
            
            if (ast->Function_Type.ident && ast->Function_Type.ident->kind == Ast_Ident) {
                result->Function.ident = ast_unwrap_ident(ast->Function_Type.ident);
            }
            result->size = sizeof(smm);
            result->align = alignof(smm);
            
            for_compound(ast->Function_Type.attributes, attr) {
                string_id ident = ast_unwrap_ident(attr->Attribute.ident);
                if (ident == Sym_link) {
                    bool error = false;
                    if (!result->Function.ident) {
                        error = true;
                    }
                    
                    // TODO(Alexander): we need a better way to read attributes
                    Ast* link = attr->Attribute.expr;
                    if (link && link->kind == Ast_Call_Expr) {
                        
                        Ast* first_arg = link->Call_Expr.args ? link->Call_Expr.args->Compound.node : 0;
                        if (first_arg && first_arg->kind == Ast_Argument) {
                            
                            Ast* target = first_arg->Argument.assign;
                            if (target && target->kind == Ast_Value &&
                                target->Value.value.type == Value_string) {
                                
                                cstring library = string_to_cstring(target->Value.value.data.str);
                                vars_load_string(result->Function.ident);
                                //pln("LoadLibraryA(\"%\")", f_cstring(library));
                                
                                cstring name = string_to_cstring(vars_load_string(result->Function.ident));
                                func->intrinsic = DEBUG_get_external_procedure_address(library, name); 
                                //pln("% = 0x%", f_cstring(name), f_u64_HEX(func->intrinsic));
                                if (!func->intrinsic) {
                                    type_error(tcx,
                                               string_format("procedure `%` is not found in library `%`",
                                                             f_cstring(name),
                                                             f_cstring(library)),
                                               ast->span);
                                }
                                
                                cstring_free(library);
                                cstring_free(name);
                            } else {
                                error = true;
                            }
                        } else {
                            error = true;
                        }
                    } else {
                        error = true;
                    }
                    
                    if (error) {
                        type_error(tcx, string_lit("@link attribute is malformed"), ast->span);
                    }
                }
            }
        } break;
        
        case Ast_Struct_Type: {
            Ast* fields = ast->Struct_Type.fields;
            Type_Struct_Like struct_like = create_type_struct_like_from_ast(tcx, fields, report_error);
            if (!struct_like.has_error) {
                result = arena_push_struct(&tcx->type_arena, Type);
                result->kind = TypeKind_Struct;
                result->Struct.types = struct_like.types;
                result->Struct.idents = struct_like.idents;
                result->Struct.offsets = struct_like.offsets;
                result->Struct.ident_to_index = struct_like.ident_to_index;
                result->ident = try_unwrap_ident(ast->Struct_Type.ident);
                result->size = (s32) struct_like.size;
                result->align = (s32) struct_like.align;
            }
        } break;
        
        case Ast_Union_Type: {
            Ast* fields = ast->Union_Type.fields;
            Type_Struct_Like struct_like = create_type_struct_like_from_ast(tcx, fields, report_error);
            if (!struct_like.has_error) {
                result = arena_push_struct(&tcx->type_arena, Type);
                result->kind = TypeKind_Union;
                result->Union.types = struct_like.types;
                result->Union.idents = struct_like.idents;
                result->ident = try_unwrap_ident(ast->Struct_Type.ident);
                result->size = (s32) struct_like.size;
                result->align = (s32) struct_like.align;
            }
        } break;
        
        case Ast_Enum_Type: {
            result = arena_push_struct(&tcx->type_arena, Type);
            result->kind = TypeKind_Enum;
            
            Type* type;
            if (ast->Enum_Type.elem_type && ast->Enum_Type.elem_type->kind != Ast_None) {
                type = create_type_from_ast(tcx, ast->Enum_Type.elem_type, report_error);
                if (type->kind != TypeKind_Basic) {
                    type_error(tcx, string_lit("enums can only be defined as primitive types"),
                               ast->span);
                    break;
                }
            } else {
                // TODO(Alexander): should we force the user to spec. the type, or pick lowest possible?
                type = t_s64;
            }
            
            result->Enum.type = type;
            result->size = type->size;
            result->align = type->align;
            result->ident = ast_unwrap_ident(ast->Enum_Type.ident);
            
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
                map_put(result->Enum.values, ident, value);
                value.data.signed_int++;
            }
        } break;
        
        case Ast_Typedef: {
            if (ast->Typedef.ident->kind == Ast_Compound) {
                // NOTE(Alexander): c craziess
                
                for_compound(ast->Typedef.ident, ty) {
                    switch (ty->kind) {
                        case Ast_Pointer_Type: {
                            Ast* ptr_ty = ty->Pointer_Type;
                            assert(ptr_ty->kind == Ast_Named_Type && "unsupported");
                            string_id ident = ast_unwrap_ident(ptr_ty->Named_Type);
                            
                            // TODO(Alexander): copy pasta below
                            // TODO(Alexander): we don't care about locality of typedefs at the moment (always global)
                            result = map_get(tcx->global_type_table, ident);
                            if (!result) {
                                result = create_type_from_ast(tcx, ast->Typedef.type, report_error);
                                result = type_wrap_pointer(tcx, result);
                                if (result) {
                                    map_put(tcx->global_type_table, ident, result);
                                }
                            } else {
                                type_error(tcx, 
                                           string_format("`%` is already defined", f_var(ident)),
                                           ast->span);
                            }
                        } break;
                        
                        case Ast_Ident: {
                            // TODO(Alexander): copy pasta above/below
                            string_id ident = ast_unwrap_ident(ty);
                            result = map_get(tcx->global_type_table, ident);
                            if (!result) {
                                result = create_type_from_ast(tcx, ast->Typedef.type, report_error);
                                if (result) {
                                    map_put(tcx->global_type_table, ident, result);
                                }
                            } else {
                                type_error(tcx, 
                                           string_format("`%` is already defined", f_var(ident)),
                                           ast->span);
                            }
                        } break;
                    }
                }
            } else {
                string_id ident = ast_unwrap_ident(ast->Typedef.ident);
                
                // TODO(Alexander): copy pasta above
                // TODO(Alexander): we don't care about locality of typedefs at the moment (always global)
                result = map_get(tcx->global_type_table, ident);
                
                if (!result) {
                    result = create_type_from_ast(tcx, ast->Typedef.type, report_error);
                    if (result) {
                        map_put(tcx->global_type_table, ident, result);
                    }
                } else {
                    type_error(tcx, string_format("`%` is already defined", f_var(ident)), ast->span);
                }
            }
        } break;
        
        case Ast_Const_Type: {
            result = create_type_from_ast(tcx, ast->Const_Type, report_error);
        } break;
    }
    
    return result;
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
                // TODO(Alexander): we can add support for auto types
                return result;
            }
            
            Type* found_type = type_infer_expression(tcx, 
                                                     stmt->Assign_Stmt.expr, 
                                                     expected_type,
                                                     report_error);
            
            if (found_type) {
                if (expected_type) {
                    result = expected_type;
                    
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
                                           string_format("cannot redeclare previous declaration `%`",
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
                decl_type = create_type_from_ast(tcx, stmt->Decl_Stmt.type, report_error);
                
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
                            // TODO(Alexander): we need to resolve this
                            unimplemented;
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
            constant_folding_of_expressions(stmt->For_Stmt.cond);
            Type* update = type_infer_expression(tcx, stmt->For_Stmt.update, 0, report_error);
            Type* block = type_infer_statement(tcx, stmt->For_Stmt.block, report_error);
            
            if (init && cond && update && block) {
                result = init;
            }
            
            pop_type_scope(tcx);
        } break;
        
        case Ast_While_Stmt: {
            Type* cond = type_infer_expression(tcx, stmt->While_Stmt.cond, t_bool, report_error);
            Type* block = type_infer_statement(tcx, stmt->While_Stmt.block, report_error);
            
            if (cond && block) {
                result = cond;
            }
        } break;
        
        case Ast_Return_Stmt: {
            result = type_infer_expression(tcx, stmt->Return_Stmt.expr, tcx->return_type, report_error);
            stmt->type = result;
        } break;
    }
    
    return result;
}

bool
type_check_assignment(Type_Context* tcx, Type* lhs, Type* rhs, Span span, Binary_Op op, bool report_error) {
    assert(lhs && rhs);
    
    if (lhs->kind != rhs->kind) {
        if (report_error) {
            type_error_mismatch(tcx, lhs, rhs, span);
        }
        return false;
    }
    
    switch (lhs->kind) {
        case TypeKind_Basic: {
            if (binary_is_comparator_table[op] && 
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
            
            bool lossy = lhs->size < rhs->size;
            if (!lossy && lhs->Basic.flags & BasicFlag_Floating) {
                lossy = rhs->Basic.flags & BasicFlag_Integer;
            }
            
            if (!lossy && rhs->Basic.flags & BasicFlag_Floating) {
                lossy = lhs->Basic.flags & BasicFlag_Integer;
            }
            
            if (!lossy && rhs->Basic.flags & BasicFlag_Integer) {
                lossy = lhs->Basic.flags & BasicFlag_Floating;
            }
            
            if (!lossy && rhs->Basic.flags & BasicFlag_String) {
                lossy = (lhs->Basic.flags & BasicFlag_String) == 0;
            }
            
            if (lhs->Basic.flags & BasicFlag_String || rhs->Basic.flags & BasicFlag_String) {
                if (op == BinaryOp_Logical_And || op == BinaryOp_Logical_Or) {
                    lossy = ((lhs->Basic.flags & BasicFlag_String) == 0 &&
                             (lhs->Basic.flags & BasicFlag_Integer) == 0 &&
                             (rhs->Basic.flags & BasicFlag_String) == 0 &&
                             (rhs->Basic.flags & BasicFlag_Integer) == 0);
                }
            }
            
            if (lossy) {
                if (report_error) {
                    type_error(tcx, 
                               string_format("conversion from `%` to `%`, possible loss of data",
                                             f_type(rhs), f_type(lhs)),
                               span);
                }
                return false;
            }
        } break;
        
        case TypeKind_Array: {
            bool success = type_check_assignment(tcx, lhs->Pointer, rhs->Pointer, span, op, false);
            
            if (lhs->Array.capacity != rhs->Array.capacity) {
                success = false;
            }
            
            if (lhs->Array.is_dynamic != rhs->Array.is_dynamic) {
                success = false;
            }
            
            if (!success) {
                type_error_mismatch(tcx, lhs, rhs, span);
                return false;
            }
            
        } break;
        
        case TypeKind_Struct: {
            // TODO(Alexander): this will not work for anonymous structs
            if (lhs != rhs) {
                if (report_error) {
                    type_error_mismatch(tcx, lhs, rhs, span);
                }
                return false;
            }
        } break;
        
        case TypeKind_Pointer: {
            bool success = type_check_assignment(tcx, lhs->Pointer, rhs->Pointer, span, op, false);
            if (!success) {
                type_error_mismatch(tcx, lhs, rhs, span);
                return false;
            }
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
            int formal_arg_count = (int) array_count(t_func->arg_types);
            
            int arg_index = 0;
            for_compound(expr->Call_Expr.args, arg) {
                type_check_expression(tcx, arg->Argument.assign);
                
                if (arg_index < formal_arg_count) {
                    if (arg->Argument.ident && arg->Argument.ident->kind == Ast_Ident) {
                        string_id arg_ident = ast_unwrap_ident(arg->Argument.ident);
                        smm index = map_get_index(t_func->ident_to_index, arg_ident);
                        if (index > 0) {
                            arg_index = t_func->ident_to_index[index].value;
                            assert(arg_index < array_count(t_func->arg_types));
                        } else {
                            type_error(tcx, 
                                       string_format("undeclared parameter with name `%`", 
                                                     f_var(arg_ident)), 
                                       expr->span);
                            result = false;
                            break;
                        }
                    }
                    Type* arg_type = t_func->arg_types[arg_index];
                    type_check_assignment(tcx, arg_type, 
                                          arg->Argument.assign->type,
                                          arg->Argument.assign->span);
                }
                
                arg_index++;
            }
            
            if (arg_index == 0 && t_func->first_default_arg_index > 0) {
                type_error(tcx, 
                           string_format("function `%` expected % argument(s)",
                                         f_var(t_func->ident),
                                         f_int(formal_arg_count)),
                           expr->span);
                result = false;
                
            } else if (arg_index < t_func->first_default_arg_index && 
                       arg_index > formal_arg_count && 
                       !t_func->is_variadic) {
                //pln("arg_index >= % && arg_index <= % && variadic = %", t_func->first_default_arg_index, formal_arg_count, t_func->is_variadic);
                
                
                // NOTE(Alexander): it is allowed to have more arguments only if the function is variadic
                type_error(tcx, string_format("function `%` did not take % arguments, expected % arguments", 
                                              f_var(t_func->ident), 
                                              f_int(arg_index), 
                                              f_int(t_func->first_default_arg_index)),
                           expr->span);
                result = false;
            }
        } break;
        
        case Ast_Binary_Expr: {
            result = type_check_expression(tcx, expr->Binary_Expr.first);
            result = result && type_check_expression(tcx, expr->Binary_Expr.second);
            
            Binary_Op op = expr->Binary_Expr.op;
            Type* first = expr->Binary_Expr.first->type;
            Type* second = expr->Binary_Expr.second->type;
            
            if (first->kind == TypeKind_Pointer) {
                if (op == BinaryOp_Add || 
                    op == BinaryOp_Subtract ||
                    op == BinaryOp_Add_Assign || 
                    op == BinaryOp_Subtract_Assign) {
                    
                    if (second->kind == TypeKind_Basic) {
                        if ((second->Basic.flags & BasicFlag_Integer) == 0) {
                            type_error(tcx, 
                                       string_format("`% % %` expects integral value on right-hand side", 
                                                     f_type(first), 
                                                     f_cstring(binary_op_strings[op]), 
                                                     f_type(second)),
                                       expr->span);
                        }
                    } else if (second->kind != TypeKind_Pointer) {
                        type_error(tcx, 
                                   string_format("operator `%` expects integral or pointer on right-hand side, found `%`", 
                                                 f_cstring(binary_op_strings[op]),
                                                 f_type(second)),
                                   expr->span);
                    }
                } else if (binary_is_comparator_table[op]) {
                    if (second->kind != TypeKind_Pointer) {
                        type_error(tcx, 
                                   string_format("operator `%` expects pointer on right-hand side, found `%`", 
                                                 f_cstring(binary_op_strings[op]),
                                                 f_type(second)),
                                   expr->span);
                    }
                } else if (op == BinaryOp_Assign) {
                    result = result && type_check_assignment(tcx, first, second, expr->Binary_Expr.second->span);
                } else {
                    type_error(tcx, 
                               string_format("operator `%` is not supported for `%` on left-hand side",
                                             f_cstring(binary_op_strings[op]),
                                             f_type(first)),
                               expr->span);
                }
            } else {
                if (!type_equals(first, second)) {
                    pln("%", f_ast(expr));
                    type_error_mismatch(tcx, first, second, expr->Binary_Expr.second->span);
                    result = false;
                }
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
                Value index_value = expr->Index_Expr.index->Value.value;
                smm index = value_to_smm(index_value);
                if (index < 0) {
                    type_error(tcx, string_format("invalid index `%`", f_smm(index)), expr->span);
                    result = false;
                }
                
                if (type->Array.capacity > 0) {
                    if (index > type->Array.capacity - 1) {
                        type_error(tcx, 
                                   string_format("array index `%` exceeds capacity `%`",
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
        case Ast_Assign_Stmt: {
            if (stmt->Assign_Stmt.expr) {
                type_check_expression(tcx, stmt->Assign_Stmt.expr);
            }
            Type* expected_type = stmt->type;
            assert(expected_type && "compiler bug: assign statement has no type");
            
            Type* found_type = stmt->Assign_Stmt.expr->type;
            if (found_type) {
                result = type_check_assignment(tcx, expected_type, found_type,
                                               stmt->Assign_Stmt.expr->span);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            result = type_check_expression(tcx, stmt->Expr_Stmt);
        } break;
        
        case Ast_Return_Stmt: {
            Type* found_type = stmt->Return_Stmt.expr->type;
            result = result && type_check_expression(tcx, stmt->Return_Stmt.expr);
            result = result && type_check_assignment(tcx, tcx->return_type, found_type,
                                                     stmt->Return_Stmt.expr->span);
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                type_check_statement(tcx, it);
            }
        } break;
        
        case Ast_Decl_Stmt: {
            if (stmt->Decl_Stmt.stmt) {
                type_check_statement(tcx, stmt->Decl_Stmt.stmt);
            }
        } break;
        
        case Ast_If_Stmt: {
            type_check_expression(tcx, stmt->If_Stmt.cond);
            type_check_statement(tcx, stmt->If_Stmt.then_block);
            type_check_statement(tcx, stmt->If_Stmt.else_block);
            // TODO(Alexander): check that the condition is a boolean
        } break;
        
        case Ast_Break_Stmt:
        case Ast_Continue_Stmt: {
            // TODO(Alexander): check that this statement is inside a loop
        } break;
        
        case Ast_For_Stmt: {
            type_check_statement(tcx, stmt->For_Stmt.init);
            type_check_expression(tcx, stmt->For_Stmt.cond);
            type_check_expression(tcx, stmt->For_Stmt.update);
            type_check_statement(tcx, stmt->For_Stmt.block);
            // TODO(Alexander): check that the condition is a boolean
        } break;
        
        case Ast_While_Stmt: {
            type_check_expression(tcx, stmt->While_Stmt.cond);
            type_check_statement(tcx, stmt->While_Stmt.block);
            // TODO(Alexander): check that the condition is a boolean
        } break;
        
        case Ast_None: break;
        
        default: assert(0 && stmt->kind);
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
        
        if (type && type->kind == TypeKind_Function) {
            push_type_scope(tcx);
            type->Function.unit = comp_unit;
            
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
    
    Temporary_Memory temp_memory = begin_temporary_memory(&tcx->type_arena);
    
    if (is_ast_type(ast)) {
        string_id ident = comp_unit->ident;
        
        Type* type = create_type_from_ast(tcx, ast, report_error);
        if (type) {
            smm type_index = map_get_index(tcx->global_type_table, ident);
            
            if (type_index != -1) {
                Type* old_type = tcx->global_type_table[type_index].value;
                
                if (type->kind == TypeKind_Struct &&
                    old_type->kind == TypeKind_Struct && type != old_type) {
                    
                    // TODO(Alexander): hack to get forward declares working
                    if (array_count(type->Struct.types) == 0) {
                        // do nothing
                    } else if (array_count(old_type->Struct.types) == 0) {
                        memcpy(old_type, type, sizeof(Type));
                    } else {
                        type_error(tcx,
                                   string_format("cannot redeclare previous declaration `%`",
                                                 f_string(vars_load_string(ident))),
                                   ast->span);
                    }
                    
                    type = old_type;
                }
            }
            
            ast->type = type;
            // TODO(Alexander): distinguish between types and types of variables
            // E.g. in `int main(...)`, main is not a type it's a global variable
            // but `typedef u32 string_id;` in this case string_id is a type.
            map_put(tcx->global_type_table, comp_unit->ident, type);
            map_put(tcx->globals, comp_unit->ident, type);
        } else {
            result = false;
        }
        
    } else if (is_ast_stmt(ast)) {
        Type* type = type_infer_statement(tcx, ast, report_error);
        if (type) {
            result = type_check_statement(tcx, ast);
        } else {
            // NOTE(Alexander): we are missing type info somewhere fail and retry later
            result = false;
        }
    } else {
        assert(0 && "illegal type: expected X_Stmt or any X_Type node");
    }
    
    if (!result) {
        end_temporary_memory(&temp_memory);
    }
    
    
    while (tcx->active_scope) {
        pop_type_scope(tcx);
    }
    
    map_free(tcx->local_type_table);
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
        // Intrinsic syntax: void print_format(string format...)
        // e.g. print_format %, lucky number is %\n", "world", 7);
        Type* type = arena_push_struct(&tcx->type_arena, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = true;
        
        string_id arg0_ident = vars_save_cstring("format");
        array_push(type->Function.arg_idents, arg0_ident);
        array_push(type->Function.arg_types, t_cstring);
        
        string_id ident = vars_save_cstring("print_format");
        type->Function.unit = 0;
        type->Function.interp_intrinsic = &interp_intrinsic_print_format;
        type->Function.intrinsic = &print_format;
        type->Function.ident = ident;
        type->Function.return_type = t_void;
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax: void debug_break()
        // Inserts a breakpoint (e.g. int3 on x64) to enable debugger
        Type* type = arena_push_struct(&tcx->type_arena, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = false;
        
        string_id ident = vars_save_cstring("debug_break");
        type->Function.unit = 0;
        type->Function.interp_intrinsic = &interp_intrinsic_debug_break;
        type->Function.intrinsic = &interp_intrinsic_debug_break;
        type->Function.ident = ident;
        type->Function.return_type = t_void;
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax: void assert(s32 expr)
        // Assets that expr is true, used as test case
        Type* type = arena_push_struct(&tcx->type_arena, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = false;
        
        string_id arg0_ident = vars_save_cstring("expr");
        array_push(type->Function.arg_idents, arg0_ident);
        array_push(type->Function.arg_types, t_s32);
        
        string_id ident = vars_save_cstring("assert");
        type->Function.unit = 0;
        type->Function.interp_intrinsic = &interp_intrinsic_assert;
        type->Function.intrinsic = &intrinsic_assert;
        type->Function.ident = ident;
        type->Function.return_type = t_void;
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax: f32 sqrt(f32 num)
        Type* type = arena_push_struct(&tcx->type_arena, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = false;
        
        string_id arg0_ident = vars_save_cstring("num");
        array_push(type->Function.arg_idents, arg0_ident);
        array_push(type->Function.arg_types, t_f32);
        
        string_id ident = vars_save_cstring("sqrt");
        type->Function.unit = 0;
        type->Function.interp_intrinsic = 0;
        type->Function.intrinsic = &sqrtf;
        type->Function.ident = ident;
        type->Function.return_type = t_f32;
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax:  f32 round_f32(f32 num)
        Type* type = arena_push_struct(&tcx->type_arena, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = false;
        
        string_id arg0_ident = vars_save_cstring("num");
        array_push(type->Function.arg_idents, arg0_ident);
        array_push(type->Function.arg_types, t_f32);
        
        string_id ident = vars_save_cstring("round_f32");
        type->Function.unit = 0;
        type->Function.interp_intrinsic = 0;
        type->Function.intrinsic = &roundf;
        type->Function.ident = ident;
        type->Function.return_type = t_f32;
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax: f32 random_f32(f32 num)
        Type* type = arena_push_struct(&tcx->type_arena, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = false;
        
        string_id ident = vars_save_cstring("random_f32");
        type->Function.unit = 0;
        type->Function.interp_intrinsic = 0;
        type->Function.intrinsic = &random_f32;
        type->Function.ident = ident;
        type->Function.return_type = t_f32;
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax: umm sizeof(T)
        Type* type = arena_push_struct(&tcx->type_arena, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = false;
        
        string_id ident = vars_save_cstring("sizeof");
        type->Function.unit = 0;
        type->Function.interp_intrinsic = 0;
        type->Function.intrinsic = &type_sizeof;
        type->Function.ident = ident;
        type->Function.return_type = normalize_basic_types(t_umm);
        
        map_put(tcx->globals, ident, type);
    }
    
    {
        // Intrinsic syntax: umm alignof(T)
        Type* type = arena_push_struct(&tcx->type_arena, Type);
        type->kind = TypeKind_Function;
        type->Function.is_variadic = false;
        
        string_id ident = vars_save_cstring("alignof");
        type->Function.unit = 0;
        type->Function.interp_intrinsic = 0;
        type->Function.intrinsic = &type_alignof;
        type->Function.ident = ident;
        type->Function.return_type = normalize_basic_types(t_umm);
        
        map_put(tcx->globals, ident, type);
    }
}

s32
type_check_ast_file(Ast_File* ast_file) {
    Type_Context tcx = {};
    
    DEBUG_setup_intrinsic_types(&tcx);
    
    // Push all compilation units to queue
    array(Compilation_Unit*)* queue = 0;
    
    for_array(ast_file->units, it, _a) {
        Ast* decl = it->ast;
        if (decl->kind != Ast_Decl_Stmt) {
            array_push(queue, it);
        }
    }
    // Push actual declarations last
    for_array(ast_file->units, it, _b) {
        Ast* decl = it->ast;
        if (decl->kind == Ast_Decl_Stmt) {
            array_push(queue, it);
        }
    }
    
    // NOTE(Alexander): exit condition: assumes that when all items in queue failed then we can
    //                  nolonger process any further and have to report errors and exit.
    smm last_queue_count = 0;
    while (last_queue_count != array_count(queue)) {
        smm queue_count = array_count(queue);
        for (smm queue_index = 0; queue_index < queue_count; queue_index++) {
            Compilation_Unit* comp_unit = queue[queue_index];
            
            if (!type_check_ast(&tcx, comp_unit, false)) {
                // Failed, retry later
                array_push(queue, comp_unit);
            }
        }
        
        // Remove processed nodes in queue
        array_remove_n(queue, 0, queue_count);
        last_queue_count = queue_count;
    }
    
    // NOTE(Alexander): anything left in the queue we report errors for
    for_array_v(queue, comp_unit, _) {
        type_check_ast(&tcx, comp_unit, true);
    }
    
    return tcx.error_count;
}

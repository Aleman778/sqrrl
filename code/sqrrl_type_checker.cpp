
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
        case Value_signed_int: return t_int;
        case Value_unsigned_int: return t_uint;
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
type_check_value(Type_Context* tcx, Type* type, Value value) {
    bool result = true;
    
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
                                                        f_type(type), f_value(&value)));
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
                                                        f_type(type), f_value(&value)));
                        
                    } else if (value.data.unsigned_int > type->Basic.limits.max_unsigned_value) {
                        
                        type_warning(tcx, string_format("expected type `%` cannot fit in value `%`", 
                                                        f_type(type), f_value(&value)));
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
                                                        f_type(type), f_value(&value)));
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
                                                        f_type(type), f_value(&value)));
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
            result = expr->type;
            if (!result) {
                if (parent_type) {
                    result = parent_type;
                    if (result->kind == TypeKind_Basic) {
                        expr->Value.value = value_cast(expr->Value.value, result->Basic.kind);
                    }
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
                    
                } else {
                    if (tcx->set_undeclared_to_s64) {
                        result = t_s64;
                    } else if (report_error) {
                        // NOTE(Alexander): copypasta
                        type_error(tcx, string_format("`%` is an undeclared identifier", 
                                                      f_string(vars_load_string(expr->Ident))));
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
                                type_error(tcx, string_format("cannot dereference type `%`", f_type(type)));
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
            
            Type* second_type = 0;
            Type* first_type = type_infer_expression(tcx, 
                                                     expr->Binary_Expr.first,
                                                     0,
                                                     report_error);
            if (first_type) {
                second_type = type_infer_expression(tcx, 
                                                    expr->Binary_Expr.second,
                                                    first_type,
                                                    report_error);
            }
            
            if (is_binary_assign(expr->Binary_Expr.op)) {
                if (first_type && second_type) {
                    result = first_type;
                }
            } else {
                
                if (first_type && second_type) {
                    // TODO(Alexander): refactor this later when the AST storage gets updated
                    //Ast* fmt_arg = arena_push_struct(&tcx->type_arena, Ast);
                    
                    bool is_result_floating = false;
                    if (first_type->kind == TypeKind_Basic && 
                        second_type->kind == TypeKind_Basic) {
                        
                        bool is_first_floating = is_bitflag_set(first_type->Basic.flags, BasicFlag_Floating);
                        bool is_second_floating = is_bitflag_set(second_type->Basic.flags, BasicFlag_Floating);
                        
                        // NOTE(Alexander): Type rules
                        // float + int -> float + float;
                        // int + float -> float + float;
                        if (is_first_floating || is_second_floating) {
                            is_result_floating = true;
                            if (!is_first_floating) {
                                first_type = second_type;
                                result = second_type;
                            } else if (!is_second_floating) {
                                first_type = first_type;
                                result = first_type;
                            }
                        }
                        
                        //pln("parent_type: %", f_type(parent_type));
                        //pln("  % + %", f_type(first_type), f_type(second_type));
                        if (first_type->size > second_type->size) {
                            result = first_type;
                            second_type = result;
                        } else {
                            result = second_type;
                            first_type = result;
                        }
                        
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
                    } else {
                        if (parent_type) {
                            result = parent_type;
                        } else {
                            result = first_type;
                        }
                    }
                }
            }
            
            // TODO(Alexander): we don't want to change types without doing explicit cast
            // otherwise the backend gets confused and outputs wrong code.
            //expr->Binary_Expr.first->type = first_type;
            //expr->Binary_Expr.second->type = second_type;
            //pln("-> %", f_type(result));
            
            if (binary_is_comparator_table[expr->Binary_Expr.op]) {
                result = t_bool;
            }
            expr->type = result;
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
                    type_error(tcx, string_format("`%` is not a function", f_var(function_ident)));
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
                        actual_arg->type = actual_type;
                    } else {
                        result = 0;
                        break;
                    }
                    
                    arg_index++;
                }
            }
            
            // HACK(Alexander): for now print_format pushes the format type first then the value 
            if (result && proc->intrinsic == &print_format) {
                
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
                    arg_index++;
                }
            }
            
        } break;
        
        case Ast_Cast_Expr: {
            Type* type = create_type_from_ast(tcx, expr->Cast_Expr.type, report_error);
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
            type_infer_expression(tcx, expr->Index_Expr.index, parent_type, report_error);
            Type* type = type_infer_expression(tcx, expr->Index_Expr.array, parent_type, report_error);
            if (type) {
                if (type->kind == TypeKind_Array) {
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
            
            // TODO(Alexander): add support for unions, enums etc.
            if (type->kind == TypeKind_Struct) {
                smm index = map_get_index(type->Struct.ident_to_index, field_ident);
                if (index >= 0) {
                    result = type->Struct.types[index];
                    if (result) {
                        
                    }
                }
            } else if (type->kind == TypeKind_Array) {
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
                
            } else {
                if (report_error) {
                    type_error(tcx, string_format("type `%` doesn't have any fields", f_type(type)));
                }
                
                break;
            }
            
            if (result) {
                expr->type = result;
                
            } else {
                if (report_error) {
                    type_error(tcx, string_format("type `%` doesn't have field `%`", f_type(type), f_var(field_ident)));
                }
            }
        } break;
        
        case Ast_Array_Expr: {
            
            if (parent_type && parent_type->kind == TypeKind_Array) {
                Type* type = parent_type->Array.type;
                
                result = parent_type;
                
                smm count = 0;
                for_compound(expr->Array_Expr.elements, it) {
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
                                                          f_smm(parent_type->Array.capacity), f_smm(count)));
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
                                type_error(tcx, string_format("cannot assign empty array literal to fixed-array of unknown capacity, did you mean to use a dynamic array [..]T?"));
                            }
                        }
                    }
                }
                
                expr->type = result;
            } else {
                if (report_error) {
                    type_error(tcx, string_format("cannot assign array literal to non-array type"));
                }
            }
        } break;
        
        case Ast_Struct_Expr: {
            string_id struct_ident = ast_unwrap_ident(expr->Struct_Expr.ident);
            Type* type = load_type_declaration(tcx, struct_ident, report_error);
            if (type || parent_type) {
                if (!type) {
                    type = parent_type;
                }
                
                if (!(type && type->kind == TypeKind_Struct)) {
                    return 0;
                }
                
                bool has_ident = false;
                bool has_errors = false;
                int next_field_index = 0;
                for_compound(expr->Struct_Expr.fields, field) {
                    int field_index = next_field_index++;
                    
                    if (field->Argument.ident) {
                        string_id ident = ast_unwrap_ident(field->Argument.ident);
                        smm idx = map_get_index(type->Struct.ident_to_index, ident);
                        if (idx == -1) {
                            if (report_error) {
                                type_error(tcx, string_format("`%` undeclared identifier in struct `%`",
                                                              f_string(vars_load_string(ident)),
                                                              f_string(vars_load_string(type->ident))));
                            }
                            continue;
                        }
                        field_index = type->Struct.ident_to_index[idx].value;
                        has_ident = true;
                    } else if (has_ident) {
                        type_error(tcx, string_lit("cannot combine named and anonymous values in struct literal"));
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
                                                          f_int(array_count(type->Struct.types))));
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
                
            } else if (report_error) {
                type_error(tcx, string_lit("missing type specifier"));
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
        
        switch (argument->Argument.ident->kind) {
            
            case Ast_Ident: {
                
                assert(argument->Argument.ident->kind == Ast_Ident);
                string_id ident = argument->Argument.ident->Ident;
                Type* type = create_type_from_ast(tcx, ast_type, report_error);
                if (type) {
                    push_type_to_struct_like(&result, type, ident);
                } else {
                    result.has_error = true;
                }
            } break;
            
            case Ast_Compound: {
                
                Type* type = create_type_from_ast(tcx, ast_type, report_error);
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
                unimplemented;
            }
        }
    }
    
    return result;
}


Type*
load_type_declaration(Type_Context* tcx, string_id ident, bool report_error) {
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
                type_error(tcx, string_format("`%` is an undeclared identifier", 
                                              f_string(vars_load_string(ident))));
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
            result = load_type_declaration(tcx, ident, report_error);
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
                        type_error(tcx, string_lit("array shape should be an integer"));
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
                result = arena_push_struct(&tcx->type_arena, Type);
                result->kind = TypeKind_Pointer;
                result->Pointer = ptr_type;
                result->size = sizeof(smm);
                result->align = alignof(smm);
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
                } else {
                    array_free(func->arg_idents);
                    array_free(func->arg_types);
                    return 0;
                }
            }
            
            assert(ast->Function_Type.ident && ast->Function_Type.ident->kind == Ast_Ident);
            result->Function.ident = ast_unwrap_ident(ast->Function_Type.ident);
            result->size = sizeof(smm);
            result->align = alignof(smm);
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
                result->ident = ast_unwrap_ident(ast->Struct_Type.ident);
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
                    type_error(tcx, string_lit("enums can only be defined as primitive types"));
                    break;
                }
            } else {
                // TODO(Alexander): should we force the user to spec. the type, or pick lowest possible?
                type = t_s64;
            }
            
            result->Enum.type = type;
            
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
                // NOTE(Alexander): c crazyiess
                unimplemented;
            } else {
                string_id ident = ast_unwrap_ident(ast->Typedef.ident);
                
                // TODO(Alexander): we don't care about locality of typedefs at the moment (always global)
                result = map_get(tcx->global_type_table, ident);
                
                if (!result) {
                    result = create_type_from_ast(tcx, ast->Typedef.type, report_error);
                    if (result) {
                        map_put(tcx->global_type_table, ident, result);
                    }
                } else {
                    type_error(tcx, string_format("`%` is already defined"));
                }
            }
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
                
                string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
                stmt->Assign_Stmt.ident->type = result;
                
                if (tcx->block_depth > 0) {
                    if (!push_local(tcx, ident, result, report_error)) {
                        result = 0;
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
                        map_put(tcx->globals, ident, result);
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
            
            if (stmt->Decl_Stmt.stmt->kind == Ast_None) {
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
type_check_assignment(Type_Context* tcx, Type* lhs, Type* rhs, bool comparator=false, bool report_error=true) {
    assert(lhs && rhs);
    
    if (lhs->kind != rhs->kind) {
        if (report_error) {
            type_error_mismatch(tcx, lhs, rhs);
        }
        return false;
    }
    
    switch (lhs->kind) {
        case TypeKind_Basic: {
            if (comparator && 
                lhs->Basic.flags & BasicFlag_Integer &&
                rhs->Basic.flags & BasicFlag_Integer) {
                if ((lhs->Basic.flags & BasicFlag_Unsigned) != (rhs->Basic.flags & BasicFlag_Unsigned)) {
                    if (report_error) {
                        type_error(tcx, string_lit("comparison with both signed and unsigned integers"));
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
            
            if (lossy) {
                if (report_error) {
                    type_error(tcx, string_format("conversion from `%` to `%`, possible loss of data",
                                                  f_type(rhs), f_type(lhs)));
                }
                return false;
            }
        } break;
        
        case TypeKind_Struct: {
            // TODO(Alexander): this will not work for anonymous structs
            if (lhs != rhs) {
                if (report_error) {
                    type_error_mismatch(tcx, lhs, rhs);
                }
                return false;
            }
        } break;
        
        case TypeKind_Pointer: {
            bool success = type_check_assignment(tcx, lhs->Pointer, rhs->Pointer, comparator, false);
            if (!success) {
                type_error_mismatch(tcx, lhs, rhs);
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
                if (arg_index >= formal_arg_count) {
                    break;
                }
                
                if (arg->Argument.ident && arg->Argument.ident->kind == Ast_Ident) {
                    string_id arg_ident = ast_unwrap_ident(arg->Argument.ident);
                    smm index = map_get_index(t_func->ident_to_index, arg_ident);
                    if (index > 0) {
                        arg_index = t_func->ident_to_index[index].value;
                        assert(arg_index < array_count(t_func->arg_types));
                    } else {
                        type_error(tcx, string_format("undeclared parameter with name `%`", 
                                                      f_var(arg_ident)));
                        result = false;
                        break;
                    }
                }
                Type* arg_type = t_func->arg_types[arg_index];
                type_check_assignment(tcx, arg_type, arg->Argument.assign->type);
                
                arg_index++;
            }
            
            if (arg_index == 0 && formal_arg_count > 0) {
                type_error(tcx, string_format("function `%` expected % argument(s)",
                                              f_var(t_func->ident),
                                              f_int(formal_arg_count)));
                result = false;
                
            } else if (arg_index != formal_arg_count && !t_func->is_variadic) {
                
                // NOTE(Alexander): it is allowed to have more arguments only if the function is variadic
                type_error(tcx, string_format("function `%` did not take % arguments, expected % arguments", 
                                              f_var(t_func->ident), 
                                              f_int(arg_index), 
                                              f_int(formal_arg_count)));
                result = false;
            }
        } break;
        
        case Ast_Binary_Expr: {
            type_check_expression(tcx, expr->Binary_Expr.first);
            type_check_expression(tcx, expr->Binary_Expr.second);
            
            Binary_Op op = expr->Binary_Expr.op;
            type_check_assignment(tcx, 
                                  expr->Binary_Expr.first->type, 
                                  expr->Binary_Expr.second->type, 
                                  binary_is_comparator_table[op]);
        } break;
        
        case Ast_Cast_Expr: {
            type_check_expression(tcx, expr->Cast_Expr.expr);
            // TODO(Alexander): do we want any checking here?
        } break;
        
        case Ast_Paren_Expr: {
            type_check_expression(tcx, expr->Paren_Expr.expr);
        } break;
        
        case Ast_Index_Expr: {
            assert(expr->Index_Expr.array);
            assert(expr->Index_Expr.index);
            Type* type = expr->Index_Expr.array->type;
            Value index_value = expr->Index_Expr.index->Value.value;
            smm index = value_to_smm(index_value);
            if (index < 0) {
                type_error(tcx, string_format("invalid index `%`", f_smm(index)));
                result = 0;
            }
            
            if (type->Array.capacity > 0) {
                if (index > type->Array.capacity - 1) {
                    type_error(tcx, string_format("array index `%` exceeds capacity `%`",
                                                  f_smm(index), f_smm(type->Array.capacity)));
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
                result = type_check_assignment(tcx, expected_type, found_type);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            result = type_check_expression(tcx, stmt->Expr_Stmt);
        } break;
        
        case Ast_Return_Stmt: {
            Type* found_type = stmt->Return_Stmt.expr->type;
            result = result && type_check_expression(tcx, stmt->Return_Stmt.expr);
            result = result && type_check_assignment(tcx, tcx->return_type, found_type);
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                type_check_statement(tcx, it);
            }
        } break;
        
        case Ast_Decl_Stmt: {
            type_check_statement(tcx, stmt->Decl_Stmt.stmt);
        } break;
        
        case Ast_If_Stmt: {
            type_check_expression(tcx, stmt->If_Stmt.cond);
            type_check_statement(tcx, stmt->If_Stmt.then_block);
            type_check_statement(tcx, stmt->If_Stmt.else_block);
            // TODO(Alexander): check that the condition is a boolean
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
                        if (!push_local(tcx, ident, arg_type, report_error)) {
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
        // Intrinsic syntax: print_format(string format...)
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
        // Intrinsic syntax: debug_break()
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
        // Intrinsic syntax: assert(s32 expr)
        // Assets that expr is true, used as test case for 
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
        // Intrinsic syntax:  sqrt(f32 num)
        // Assets that expr is true, used as test case for 
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
        // Intrinsic syntax:  random_f32(
        // Assets that expr is true, used as test case for 
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

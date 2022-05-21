

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
        x = 1316134913ll; // error: doesn't fit in int
    }
    
    {
        u32 a = 10;
        s64 b = -40;
        int x = a + b;
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
typer_infer_by_value(Value value) {
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
typer_infer_by_expression(Ast* expr) {
    Type* result = 0;
    
    switch (expr->kind) {
        case Ast_Value: {
            
            result = expr->Value.type;
            if (!result) {
                result = typer_infer_by_value(expr->Value.value);
                expr->Value.type = result;
            }
        } break;
        
        
        case Ast_Binary_Expr: {
            Type* first = typer_infer_by_expression(expr->Binary_Expr.first);
            Type* second = typer_infer_by_expression(expr->Binary_Expr.second);
            
            if (first->kind == Type_Primitive) {
                
            }
            
        } break;
        
    }
    
    return 0;
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
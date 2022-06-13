
Value
value_cast(Value value, Primitive_Type_Kind type_kind) {
    // TODO(Alexander): handle type errors here
    Value result = {};
    
    switch (type_kind) {
        case PrimitiveType_int:
        case PrimitiveType_s8:
        case PrimitiveType_s16:
        case PrimitiveType_s32:
        case PrimitiveType_b32:
        case PrimitiveType_s64:
        case PrimitiveType_smm: {
            result.data.signed_int = value_to_s64(value);
            result.type = Value_signed_int;
        } break;
        
        case PrimitiveType_uint:
        case PrimitiveType_u8:
        case PrimitiveType_u16:
        case PrimitiveType_u32:
        case PrimitiveType_u64:
        case PrimitiveType_umm: {
            result.data.unsigned_int = value_to_u64(value);
            result.type = Value_unsigned_int;
        } break;
        
        case PrimitiveType_f32:
        case PrimitiveType_f64: {
            result.data.floating = value_to_f64(value);
            result.type = Value_floating;
        } break;
        
        case PrimitiveType_bool: {
            result.data.boolean = value_to_bool(value);
            result.type = Value_boolean;
        } break;
        
        default: {
            assert(0 && "invalid primitive type");
        } break;
    }
    
    return result;
}

inline s64
value_integer_binary_operation(Value first, Value second, Binary_Op op) {
    switch (op) {
        case BinaryOp_Multiply: 
        case BinaryOp_Multiply_Assign: {
            return first.data.signed_int * second.data.signed_int;
        }
        
        case BinaryOp_Divide:
        case BinaryOp_Divide_Assign:{
            return first.data.signed_int / second.data.signed_int;
        }
        
        case BinaryOp_Modulo:
        case BinaryOp_Modulo_Assign:{
            return first.data.signed_int % second.data.signed_int;
        }
        
        case BinaryOp_Add:
        case BinaryOp_Add_Assign:{
            return first.data.signed_int + second.data.signed_int;
        }
        
        case BinaryOp_Subtract:
        case BinaryOp_Subtract_Assign:{
            return first.data.signed_int - second.data.signed_int;
        }
        
        case BinaryOp_Shift_Left:
        case BinaryOp_Shift_Left_Assign: {
            return first.data.signed_int << second.data.signed_int;
        }
        
        case BinaryOp_Shift_Right:
        case BinaryOp_Shift_Right_Assign:{
            return first.data.signed_int >> second.data.signed_int;
        }
        
        case BinaryOp_Bitwise_And:
        case BinaryOp_Bitwise_And_Assign:{
            return first.data.signed_int & second.data.signed_int;
        }
        
        case BinaryOp_Bitwise_Or:
        case BinaryOp_Bitwise_Or_Assign:{
            return first.data.signed_int | second.data.signed_int;
        }
        
        case BinaryOp_Bitwise_Xor:
        case BinaryOp_Bitwise_Xor_Assign:{
            return first.data.signed_int ^ second.data.signed_int;
        }
        
        case BinaryOp_Less_Than: {
            return first.data.signed_int < second.data.signed_int;
        }
        
        case BinaryOp_Less_Equals: {
            return first.data.signed_int <= second.data.signed_int;
        }
        
        case BinaryOp_Greater_Than: {
            return first.data.signed_int > second.data.signed_int;
        }
        
        case BinaryOp_Greater_Equals: {
            return first.data.signed_int >= second.data.signed_int;
        }
        
        case BinaryOp_Equals: {
            return first.data.signed_int == second.data.signed_int;
        }
        
        case BinaryOp_Not_Equals: {
            return first.data.signed_int != second.data.signed_int;
        }
        
        case BinaryOp_Assign: {
            return second.data.signed_int;
        }
        
        case BinaryOp_Logical_And: {
            return first.data.boolean && second.data.boolean;
        } break;
        
        case BinaryOp_Logical_Or: {
            return first.data.boolean || second.data.boolean;
        } break;
        
        default: {
            assert(0 && "unsupported binary operator");
        }
    }
    
    return 0;
}

inline Value
value_floating_binary_operation(Value first, Value second, Binary_Op op) {
    Value result;
    result.type = Value_floating;
    
    switch (op) {
        case BinaryOp_Multiply: 
        case BinaryOp_Multiply_Assign: {
            result.data.floating = first.data.floating * second.data.floating;
        } break;
        
        case BinaryOp_Divide:
        case BinaryOp_Divide_Assign:{
            result.data.floating = first.data.floating / second.data.floating;
        } break;
        
        case BinaryOp_Add:
        case BinaryOp_Add_Assign:{
            result.data.floating = first.data.floating + second.data.floating;
        } break;
        
        case BinaryOp_Subtract:
        case BinaryOp_Subtract_Assign:{
            result.data.floating = first.data.floating - second.data.floating;
        } break;
        
        case BinaryOp_Less_Than: {
            result.data.boolean = first.data.floating < second.data.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Less_Equals: {
            result.data.boolean = first.data.floating <= second.data.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Greater_Than: {
            result.data.boolean = first.data.floating > second.data.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Greater_Equals: {
            result.data.boolean = first.data.floating >= second.data.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Equals: {
            result.data.boolean = first.data.floating == second.data.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Not_Equals: {
            result.data.boolean = first.data.floating != second.data.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Assign: {
            result.data.floating = second.data.floating;
        } break;
        
        default: {
            assert(0 && "unimplemented");
        }
    }
    
    return result;
}

// TODO(Alexander): print actual types from memory by specifiying the type as well
void string_builder_push(String_Builder* sb, Value* value) {
    switch (value->type) {
        case Value_boolean: {
            string_builder_push(sb, value->data.boolean ? "true" : "false");
        } break;
        
        case Value_signed_int: {
            string_builder_push_format(sb, "%", f_s64(value->data.signed_int));
        } break;
        
        case Value_unsigned_int: {
            string_builder_push_format(sb, "%", f_u64(value->data.unsigned_int));
        } break;
        
        case Value_floating: {
            string_builder_push_format(sb, "%", f_float(value->data.floating));
        } break;
        
        case Value_pointer: {
            string_builder_push_cformat(sb, "0x%I64X", value->data.pointer);
        } break;
        
        case Value_array: {
            string_builder_push_cformat(sb, "0x%I64X", (smm) value->data.array.elements);
            // TODO(Alexander): can't know what elements there are without the type!
            //printf("[");
            //Array_Value* arr = &val->array;
            //for (smm index = 0; index < arr->count; index++) {
            //if (index > 0) printf(", ");
            //print_value(&arr->elements[index]);
            //}
            //printf("]");
        } break;
        
        case Value_string: {
            string_builder_push_format(sb, "\"%\"", f_string(value->data.str));
        } break;
    }
}


void print_value(Value* value) {
    String_Builder sb = {};
    string_builder_alloc(&sb, 20);
    string_builder_push(&sb, value);
    string result = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(result));
    string_builder_free(&sb);
}
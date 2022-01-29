
Value
value_cast(Value value, Type* type) {
    // TODO(Alexander): handle type errors here
    switch (type->kind) {
        case TypeKind_Primitive: {
            switch (type->Primitive.kind) {
                case PrimitiveTypeKind_int:
                case PrimitiveTypeKind_s8:
                case PrimitiveTypeKind_s16:
                case PrimitiveTypeKind_s32:
                case PrimitiveTypeKind_b32:
                case PrimitiveTypeKind_s64:
                case PrimitiveTypeKind_smm: {
                    value.signed_int = value_to_s64(value);
                    value.type = Value_signed_int;
                } break;
                
                case PrimitiveTypeKind_uint:
                case PrimitiveTypeKind_u8:
                case PrimitiveTypeKind_u16:
                case PrimitiveTypeKind_u32:
                case PrimitiveTypeKind_u64:
                case PrimitiveTypeKind_umm: {
                    value.unsigned_int = value_to_u64(value);
                    value.type = Value_unsigned_int;
                } break;
                
                case PrimitiveTypeKind_f32:
                case PrimitiveTypeKind_f64: {
                    value.floating = value_to_f64(value);
                    value.type = Value_floating;
                } break;
                
                case PrimitiveTypeKind_bool: {
                    value.boolean = value_to_bool(value);
                    value.type = Value_boolean;
                } break;
                
                default: {
                    assert(0 && "invalid primitive type");
                } break;
                
            } break;
        }
        
        default: {
            assert(0 && "not a value");
        } break;
    }
    
    return value;
}

inline s64
value_integer_binary_operation(Value first, Value second, Binary_Op op) {
    switch (op) {
        case BinaryOp_Multiply: 
        case BinaryOp_Multiply_Assign: {
            return first.signed_int * second.signed_int;
        }
        
        case BinaryOp_Divide:
        case BinaryOp_Divide_Assign:{
            return first.signed_int / second.signed_int;
        }
        
        case BinaryOp_Modulo:
        case BinaryOp_Modulo_Assign:{
            return first.signed_int % second.signed_int;
        }
        
        case BinaryOp_Add:
        case BinaryOp_Add_Assign:{
            return first.signed_int + second.signed_int;
        }
        
        case BinaryOp_Subtract:
        case BinaryOp_Subtract_Assign:{
            return first.signed_int - second.signed_int;
        }
        
        case BinaryOp_Shift_Left:
        case BinaryOp_Shift_Left_Assign: {
            return first.signed_int << second.signed_int;
        }
        
        case BinaryOp_Shift_Right:
        case BinaryOp_Shift_Right_Assign:{
            return first.signed_int >> second.signed_int;
        }
        
        case BinaryOp_Bitwise_And:
        case BinaryOp_Bitwise_And_Assign:{
            return first.signed_int & second.signed_int;
        }
        
        case BinaryOp_Bitwise_Or:
        case BinaryOp_Bitwise_Or_Assign:{
            return first.signed_int | second.signed_int;
        }
        
        case BinaryOp_Bitwise_Xor:
        case BinaryOp_Bitwise_Xor_Assign:{
            return first.signed_int ^ second.signed_int;
        }
        
        case BinaryOp_Less_Than: {
            return first.signed_int < second.signed_int;
        }
        
        case BinaryOp_Less_Equals: {
            return first.signed_int <= second.signed_int;
        }
        
        case BinaryOp_Greater_Than: {
            return first.signed_int > second.signed_int;
        }
        
        case BinaryOp_Greater_Equals: {
            return first.signed_int >= second.signed_int;
        }
        
        case BinaryOp_Equals: {
            return first.signed_int == second.signed_int;
        }
        
        case BinaryOp_Not_Equals: {
            return first.signed_int != second.signed_int;
        }
        
        case BinaryOp_Assign: {
            return second.signed_int;
        }
        
        default: {
            assert(0 && "unimplemented");
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
            result.floating = first.floating * second.floating;
        } break;
        
        case BinaryOp_Divide:
        case BinaryOp_Divide_Assign:{
            result.floating = first.floating / second.floating;
        } break;
        
        case BinaryOp_Add:
        case BinaryOp_Add_Assign:{
            result.floating = first.floating + second.floating;
        } break;
        
        case BinaryOp_Subtract:
        case BinaryOp_Subtract_Assign:{
            result.floating = first.floating - second.floating;
        } break;
        
        case BinaryOp_Less_Than: {
            result.boolean = first.floating < second.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Less_Equals: {
            result.boolean = first.floating <= second.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Greater_Than: {
            result.boolean = first.floating > second.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Greater_Equals: {
            result.boolean = first.floating >= second.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Equals: {
            result.boolean = first.floating == second.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Not_Equals: {
            result.boolean = first.floating != second.floating;
            result.type = Value_boolean;
        } break;
        
        case BinaryOp_Assign: {
            result.floating = second.floating;
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
            string_builder_push(sb, value->boolean ? "true" : "false");
        } break;
        
        case Value_signed_int: {
            string_builder_push_format(sb, "%", f_s64(value->signed_int));
        } break;
        
        case Value_unsigned_int: {
            string_builder_push_format(sb, "%", f_u64(value->unsigned_int));
        } break;
        
        case Value_floating: {
            string_builder_push_format(sb, "%", f_float(value->floating));
        } break;
        
        case Value_pointer: {
            string_builder_push_cformat(sb, "0x%I64X", value->pointer);
        } break;
        
        case Value_array: {
            string_builder_push_cformat(sb, "0x%I64X", (smm) value->array.elements);
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
            string_builder_push_format(sb, "%", f_string(value->str));
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
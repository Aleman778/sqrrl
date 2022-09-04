
void
value_store_in_memory(Type* type, void* dest, Value_Data src) {
    switch (type->kind) {
        case TypeKind_Basic: {
            switch (type->Basic.kind) {
                case Basic_s1:
                case Basic_s8:   *((s8*) dest) = (s8)  src.signed_int; break;
                case Basic_s16: *((s16*) dest) = (s16) src.signed_int; break;
                case Basic_s32: *((s32*) dest) = (s32) src.signed_int; break;
                case Basic_s64: *((s64*) dest) = (s64) src.signed_int; break;
                
                case Basic_u8:   *((u8*) dest) = (u8)  src.unsigned_int; break;
                case Basic_u16: *((u16*) dest) = (u16) src.unsigned_int; break;
                case Basic_u32: *((u32*) dest) = (u32) src.unsigned_int; break;
                case Basic_u64: *((u64*) dest) = (u64) src.unsigned_int; break;
                
                case Basic_f32: *((f32*) dest) = (f32) src.floating; break;
                case Basic_f64: *((f64*) dest) = (f64) src.floating; break;
                
                case Basic_string: *((string*) dest) = src.str; break;
                case Basic_cstring: *((smm*) dest) = src.pointer; break;
                
                default: {
                    // TODO(Alexander): we probably shouldn't even store these in the first place!!!
                    assert("cannot use architecture or context dependant type");
                } break;
            }
            
            case TypeKind_Array: {
                // NOTE(Alexander): ugh little bit ugly hack to get this to work
                smm* array_data = (smm*) dest;
                *array_data++ = src.array.count;
                void** elements = (void**) array_data;
                *elements = src.array.elements;
            } break;
            
            
            case TypeKind_Function: 
            case TypeKind_Pointer:
            case TypeKind_Struct: 
            case TypeKind_Union: {
                *((smm*) dest) = src.pointer;
            } break;
            
            default: {
                assert(0 && "invalid type"); 
            } break;
        }
    }
}

Value_Type 
value_type_from_basic_flags(u32 flags) {
    Value_Type result = Value_void;
    if (is_bitflag_set(flags, BasicFlag_Integer)) {
        if (is_bitflag_set(flags, BasicFlag_Unsigned)) {
            result = Value_unsigned_int;
        } else {
            result = Value_signed_int;
        }
    } else if (is_bitflag_set(flags, BasicFlag_Floating)) {
        result = Value_floating;
    }
    return result;
}

Value
value_load_from_memory(Type* type, void* data) {
    Value result = {};
    
    switch (type->kind) {
        case TypeKind_Basic: {
            result.type = value_type_from_basic_flags(type->Basic.flags);
            
            switch (type->Basic.kind) {
                case Basic_s1:  result.data.signed_int =  *((s8*) data); break;
                case Basic_s8:  result.data.signed_int =  *((s8*) data); break;
                case Basic_s16: result.data.signed_int = *((s16*) data); break;
                case Basic_s32: result.data.signed_int = *((s32*) data); break;
                case Basic_s64: result.data.signed_int = *((s64*) data); break;
                
                case Basic_u8:  result.data.unsigned_int =  *((u8*) data); break;
                case Basic_u16: result.data.unsigned_int = *((u16*) data); break;
                case Basic_u32: result.data.unsigned_int = *((u32*) data); break;
                case Basic_u64: result.data.unsigned_int = *((u64*) data); break;
                
                case Basic_f32: result.data.floating = *((f32*) data); break;
                case Basic_f64: result.data.floating = *((f64*) data); break;
                
                case Basic_string: {
                    result.data.str = *((string*) data);
                    result.type = Value_string;
                } break;
                case Basic_cstring: {
                    result.data.mstr = (Memory_String) data;
                    result.type = Value_memory_string;
                } break;
                
                default: {
                    // TODO(Alexander): we probably shouldn't even store these in the first place!!!
                    assert("cannot use architecture or context dependant type");
                } break;
            }
        }
        
        case TypeKind_Array: {
            result.type = Value_array;
            
            if (type->Array.capacity <= 0) {
                smm* mdata = (smm*) data;
                result.data.array.count = *mdata++;
                result.data.array.elements = *((void**) mdata);
            } else {
                result.data.array.count = type->Array.capacity;
                result.data.array.elements = data;
            }
        } break;
        
        case TypeKind_Function:
        case TypeKind_Pointer:
        case TypeKind_Struct:
        case TypeKind_Union: {
            result.data.pointer = *((smm*) data);
            result.type = Value_pointer;
        } break;
    }
    
    return result;
}

Value
value_cast(Value value, Basic_Type type) {
    Value result = {};
    
    u32 flags = basic_type_definitions[type].Basic.flags;;
    result.type = value_type_from_basic_flags(flags);
    
    switch (type) {
        case Basic_s1: {
            result.data.boolean = value_to_bool(value);
        } break;
        
        case Basic_s8: {
            result.data.signed_int = (s8) value_to_s64(value);
        } break;
        
        case Basic_s16: {
            result.data.signed_int = (s16) value_to_s64(value);
        } break;
        
        case Basic_s32:
        case Basic_int: {
            result.data.signed_int = (s32) value_to_s64(value);
        } break;
        
        case Basic_s64: {
            result.data.signed_int = value_to_s64(value);
        } break;
        
        case Basic_smm: {
            result.data.signed_int = value_to_s64(value);
        } break;
        
        case Basic_u8: {
            result.data.unsigned_int = (u8) value_to_u64(value);
        } break;
        
        case Basic_u16: {
            result.data.unsigned_int = (u16) value_to_u64(value);
        } break;
        
        case Basic_u32:
        case Basic_uint: {
            result.data.unsigned_int = (u32) value_to_u64(value);
        } break;
        
        case Basic_u64: {
            result.data.unsigned_int = value_to_u64(value);
        } break;
        
        case Basic_umm: {
            result.data.unsigned_int = value_to_u64(value);
        } break;
        
        case Basic_f32: {
            result.data.floating = (f32) value_to_f64(value);
        } break;
        
        case Basic_f64: {
            result.data.floating = value_to_f64(value);
        } break;
        
        default: {
            assert(0 && "invalid type");
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
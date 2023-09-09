
void
wasm_push_valtype(Buffer* buf, Bytecode_Type type) {
    switch (type.kind) {
        case BC_TYPE_INT: 
        case BC_TYPE_PTR: push_u8(buf, 0x7E); break;
        case BC_TYPE_FLOAT: push_u8(buf, (type.size == 8) ? 0x7C : 0x7D); break;
        default: unimplemented;
    }
}

void
wasm_local_get(Buffer* buf, u32 local_index) {
    push_u8(buf, 0x20); // local.get
    push_leb128_u32(buf, local_index);
}

void
wasm_local_set(Buffer* buf, u32 local_index) {
    push_u8(buf, 0x21); // local.set
    push_leb128_u32(buf, local_index);
}

inline void
wasm_push_stack_pointer(Buffer* buf) {
    push_u8(buf, 0x23); // global.get
    push_leb128_u32(buf, 0);
}

void
wasm_i32_const(Buffer* buf, s32 val) {
    push_u8(buf, 0x41);
    push_leb128_s64(buf, val);
}

void
wasm_i64_const(Buffer* buf, s64 val) {
    push_u8(buf, 0x42);
    push_leb128_s64(buf, val);
}

void
wasm_f32_const(Buffer* buf, f32 val) {
    u32* raw_data = (u32*) &val;
    push_u8(buf, 0x43);
    push_u32(buf, *raw_data);
}

void
wasm_f64_const(Buffer* buf, f64 val) {
    u64* raw_data = (u64*) &val;
    push_u8(buf, 0x44);
    push_u64(buf, *raw_data);
}

inline void
wasm_i32_load(Buffer* buf, u32 offset) {
    push_u8(buf, 0x28);
    push_leb128_u32(buf, 2);
    push_leb128_u32(buf, offset);
}

inline void
wasm_i64_load(Buffer* buf, u32 offset) {
    push_u8(buf, 0x29);
    push_leb128_u32(buf, 3);
    push_leb128_u32(buf, offset);
}

inline void
wasm_i32_store(Buffer* buf, u32 offset) {
    push_u8(buf, 0x36); // i32.store
    push_leb128_u32(buf, 2);
    push_leb128_u32(buf, offset);
}

inline void
wasm_i64_store8(Buffer* buf, u32 offset) {
    push_u8(buf, 0x3C); // i64.store8
    push_leb128_u32(buf, 0);
    push_leb128_u32(buf, offset);
}

inline void
wasm_i64_store16(Buffer* buf, u32 offset) {
    push_u8(buf, 0x3D); // i64.store16
    push_leb128_u32(buf, 1);
    push_leb128_u32(buf, offset);
}


inline void
wasm_i64_store32(Buffer* buf, u32 offset) {
    push_u8(buf, 0x3E); // i64.store32
    push_leb128_u32(buf, 2);
    push_leb128_u32(buf, offset);
}


inline void
wasm_i64_store(Buffer* buf, u32 offset) {
    push_u8(buf, 0x37); // i64.store
    push_leb128_u32(buf, 3);
    push_leb128_u32(buf, offset);
}

inline void
wasm_i32_add(Buffer* buf) {
    push_u8(buf, 0x6A);
}

inline void
wasm_i64_add(Buffer* buf) {
    push_u8(buf, 0x7C);
}

void
wasm_load_extend(Buffer* buf, Bytecode_Type type, u32 offset) {
    // NOTE(Alexander): you have to first push i32 memory offset
    
    if (type.kind == BC_TYPE_FLOAT) {
        if (type.size == 8) {
            push_u8(buf, 0x2B); // f64.load
            push_leb128_u32(buf, 3);
        } else if (type.size == 4) {
            push_u8(buf, 0x2A); // f32.load
            push_leb128_u32(buf, 2);
        } else {
            unimplemented;
        }
    } else {
        if (type.flags & BC_FLAG_SIGNED) {
            switch (type.size) {
                case 1: {
                    push_u8(buf, 0x30); // i64.load8_s
                    push_leb128_u32(buf, 0);
                } break;
                
                case 2: {
                    push_u8(buf, 0x32); // i64.load16_s
                    push_leb128_u32(buf, 1);
                } break;
                
                case 4: {
                    push_u8(buf, 0x34); // i64.load32_s
                    push_leb128_u32(buf, 2);
                } break;
                
                default: {
                    push_u8(buf, 0x29); // i64.load
                    push_leb128_u32(buf, 3);
                } break;
            }
        } else {
            switch (type.size) {
                case 1: {
                    push_u8(buf, 0x31); // i64.load8_u
                    push_leb128_u32(buf, 0);
                } break;
                
                case 2: {
                    push_u8(buf, 0x33); // i64.load16_u
                    push_leb128_u32(buf, 1);
                } break;
                
                case 4: {
                    push_u8(buf, 0x35); // i64.load32_u
                    push_leb128_u32(buf, 2);
                } break;
                
                default: {
                    push_u8(buf, 0x29); // i64.load
                    push_leb128_u32(buf, 3);
                } break;
            }
        }
    } 
    
    push_leb128_u32(buf, offset);
}

inline void
wasm_load_register_extend(Bytecode_Function* func, Buffer* buf, int register_index) {
    Bytecode_Type type = register_type(func, register_index);
    wasm_push_stack_pointer(buf);
    wasm_load_extend(buf, type, register_index*8);
}

void
wasm_load_register(Bytecode_Function* func, Buffer* buf, int register_index) {
    wasm_push_stack_pointer(buf);
    
    Bytecode_Type type = register_type(func, register_index);
    if (type.kind == BC_TYPE_FLOAT) {
        if (type.size == 8) {
            push_u8(buf, 0x2B); // f64.load
            push_leb128_u32(buf, 3);
        } else {
            push_u8(buf, 0x2A); // f32.load
            push_leb128_u32(buf, 2);
        } 
    } else {
        push_u8(buf, 0x29); // i64.load
        push_leb128_u32(buf, 3);
    }
    push_leb128_u32(buf, register_index*8);
}

void
wasm_prepare_store(Buffer* buf, int swap_local=-1) {
    if (swap_local >= 0) {
        wasm_local_set(buf, swap_local);
    }
    
    // Push stack pointer
    push_u8(buf, 0x23); // global.get
    push_leb128_u32(buf, 0);
    
    if (swap_local >= 0) {
        wasm_local_get(buf, swap_local);
    }
}

void
wasm_store_register(Buffer* buf, Bytecode_Type type, int register_index) {
    if (type.kind == BC_TYPE_FLOAT) {
        if (type.size == 8) {
            push_u8(buf, 0x39); // f64.store
            push_leb128_u32(buf, 3);
        } else {
            push_u8(buf, 0x38); // f32.store
            push_leb128_u32(buf, 2);
        } 
    } else {
        push_u8(buf, 0x37); // i64.store
        push_leb128_u32(buf, 3);
    }
    push_leb128_u32(buf, register_index*8);
}

u32
wasm_memory_offset(WASM_Assembler* wasm, u32 offset, Bytecode_Memory_Kind kind) {
    switch (kind) {
        case BC_MEM_READ_ONLY: offset += wasm->rdata_offset; break;
        case BC_MEM_READ_WRITE: offset += wasm->data_offset; break;
        default: unimplemented;
    }
    return offset;
}

#if 0
void
wasm_push_addr_of(WASM_Assembler* wasm, Buffer* buf, Bytecode_Operand op, Bytecode_Type type) {
    if (type == BytecodeType_void) {
        type = BytecodeType_i32;
    }
    
    if (op.kind == BytecodeOperand_stack) {
        push_u8(buf, 0x23); // global.get
        push_leb128_u32(buf, 0);
        if (type == BytecodeType_i64) {
            push_u8(buf, 0xAD); // i64.extend_i32_u
        }
        push_u8(buf, 0x41 + type - 1); // i32.const
        push_leb128_s32(buf, wasm->stack_offsets[op.stack_index] + op.memory_offset);
        push_u8(buf, wasm_binary_opcodes[type - 1]); // i32.add
        
    } else if (op.kind == BytecodeOperand_memory) {
        push_u8(buf, 0x41 + type - 1); // i32.const
        u32 offset = wasm_memory_offset(wasm, op.memory_offset, op.memory_kind);
        push_leb128_s32(buf, offset);
        
    } else {
        assert(0 && "ADDR_OF: invalid right-hand size argument");
    }
}
#endif

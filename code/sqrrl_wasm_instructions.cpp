
void
wasm_push_valtype(Buffer* buf, Bytecode_Type type) {
    switch (type) {
        case BytecodeType_i32: 
        case BytecodeType_i64: push_u8(buf, 0x7E); break;
        case BytecodeType_f32: push_u8(buf, 0x7D); break;
        case BytecodeType_f64: push_u8(buf, 0x7C); break;
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

void
wasm_const_i64(Buffer* buf, s64 val) {
    push_u8(buf, 0x42); // i64.const
    push_leb128_s64(buf, val);
}

void
wasm_push_load(Buffer* buf, Bytecode_Flags type_flags, u32 offset) {
    unimplemented;
}

void
wasm_load_i64(Buffer* buf, Bytecode_Flags type_flags, u32 offset) {
    switch (type_flags & (BC_SIZE_MASK | BC_FLAG_SIGNED | BC_FLAG_FLOAT)) {
        case BC_FLAG_64BIT: {
            push_u8(buf, 0x29); // i64.load
            push_leb128_u32(buf, 3);
        } break;
        
        case BC_FLAG_8BIT | BC_FLAG_SIGNED: {
            push_u8(buf, 0x30); // i64.load8_s
            push_leb128_u32(buf, 0);
        } break;
        
        case BC_FLAG_8BIT: {
            push_u8(buf, 0x31); // i64.load8_u
            push_leb128_u32(buf, 0);
        } break;
        
        case BC_FLAG_16BIT | BC_FLAG_SIGNED: {
            push_u8(buf, 0x32); // i64.load16_s
            push_leb128_u32(buf, 1);
        } break;
        
        case BC_FLAG_16BIT: {
            push_u8(buf, 0x33); // i64.load16_u
            push_leb128_u32(buf, 1);
        } break;
        
        case BC_FLAG_32BIT | BC_FLAG_SIGNED: {
            push_u8(buf, 0x34); // i64.load32_s
            push_leb128_u32(buf, 2);
        } break;
        
        case BC_FLAG_32BIT: {
            push_u8(buf, 0x35); // i64.load32_u
            push_leb128_u32(buf, 2);
        } break;
        
        case BC_FLAG_32BIT | BC_FLAG_FLOAT: {
            push_u8(buf, 0x2A); // f32.load
            push_leb128_u32(buf, 2);
        } break;
        
        case BC_FLAG_64BIT | BC_FLAG_FLOAT: {
            push_u8(buf, 0x2B); // f64.load
            push_leb128_u32(buf, 3);
        } break;
        
        default: unimplemented;
    }
    
    push_leb128_u32(buf, offset);
}

inline void
wasm_push_stack_pointer(Buffer* buf) {
    push_u8(buf, 0x23); // global.get
    push_leb128_u32(buf, 0);
}

inline void
wasm_load_register(Buffer* buf, Bytecode_Flags type_flags, int register_index) {
    wasm_push_stack_pointer(buf);
    wasm_load_i64(buf, type_flags, register_index*8);
}


void
wasm_prepare_store(Buffer* buf, int swap_local_i64=-1) {
    // Setup stack pointer
    
    if (swap_local_i64 >= 0) {
        wasm_local_set(buf, swap_local_i64);
    }
    
    // Push stack pointer
    push_u8(buf, 0x23); // global.get
    push_leb128_u32(buf, 0);
    
    if (swap_local_i64 >= 0) {
        // Push src on top of the stack
        wasm_local_get(buf, swap_local_i64);
    }
}

void
wasm_store_register(Buffer* buf, Bytecode_Flags type_flags, int register_index) {
    if (type_flags & BC_FLAG_FLOAT) {
        if (type_flags & BC_FLAG_64BIT) {
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

void
wasm_tmp_local(WASM_Assembler* wasm, Buffer* buf, Bytecode_Type type) {
    switch (type) {
        case BytecodeType_i32: {
            push_leb128_u32(buf, wasm->tmp_local_i32);
        } break;
        
        case BytecodeType_i64: {
            push_leb128_u32(buf, wasm->tmp_local_i64);
        } break;
        
        case BytecodeType_f32: {
            push_leb128_u32(buf, wasm->tmp_local_f32);
        } break;
        
        case BytecodeType_f64: {
            push_leb128_u32(buf, wasm->tmp_local_f64);
        } break;
    }
}

u32
wasm_memory_offset(WASM_Assembler* wasm, u32 offset, Bytecode_Memory_Kind kind) {
    if (kind == BytecodeMemory_read_only) {
        offset += wasm->rdata_offset;
    } else if (kind == BytecodeMemory_read_write) {
        offset += wasm->data_offset;
    } else {
        unimplemented;
    }
    return offset;
}

void
wasm_load_value(WASM_Assembler* wasm, Buffer* buf, Bytecode_Operand op, Bytecode_Type type, int bitsize=0) {
    push_u8(buf, 0x23); // global.get
    push_leb128_u32(buf, 0);
    
    //wasm_push_load(buf, type, op.register_index * 8, bitsize);
}

void
wasm_prepare_store_old(WASM_Assembler* wasm, Buffer* buf, Bytecode_Operand dest, Bytecode_Operand src, Bytecode_Type type, int swap_local_i64=-1) {
    // Setup stack pointer
    
    if (swap_local_i64 >= 0) {
        wasm_local_set(buf, swap_local_i64);
    }
    
    // Push stack pointer
    push_u8(buf, 0x23); // global.get
    push_leb128_u32(buf, 0);
    
    if (swap_local_i64 >= 0) {
        // Push src on top of the stack
        wasm_local_get(buf, swap_local_i64);
    }
}


void
wasm_store_value(WASM_Assembler* wasm, Buffer* buf, Bytecode_Operand dest, Bytecode_Type type, int bitsize=0) {
    switch (type) {
        case BytecodeType_i64: {
            push_u8(buf, 0x37); // i64.store
            push_leb128_u32(buf, 3);
        } break;
        
        case BytecodeType_f32: {
            push_u8(buf, 0x38); // f32.store
            push_leb128_u32(buf, 2);
        } break;
        
        case BytecodeType_f64: {
            push_u8(buf, 0x39); // f64.store
            push_leb128_u32(buf, 3);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    push_leb128_u32(buf, dest.register_index*8);
}

void
wasm_extend(Buffer* buf, Bytecode_Flags type_flags, int reg_index) {
    switch (type_flags & BC_SIZE_PLUS_SIGNED_MASK) {
        case BC_FLAG_8BIT | BC_FLAG_SIGNED: {
            push_u8(buf, 0xC0); // i32.extend_s8
        } break;
        
        case BC_FLAG_8BIT: {
            wasm_load_register(buf, type_flags, reg_index);
            wasm_const_i64(buf, 0xFF);
            push_u8(buf, 0x83); // i64.and x, 0xFF
        } break;
        
        case BC_FLAG_16BIT: {
            
        } break;
    }
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

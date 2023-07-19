

enum WASM_Section {
    WASMSection_Custom,
    WASMSection_Type,
    WASMSection_Import,
    WASMSection_Function,
    WASMSection_Table,
    WASMSection_Memory,
    WASMSection_Global,
    WASMSection_Export,
    WASMSection_Start,
    WASMSection_Element,
    WASMSection_Code,
    WASMSection_Data,
    WASMSection_Data_Count,
    
    WASMSection_Count,
};

const cstring wasm_section_names[] = {
    "custom", "type", "import", "function", "table", "memory", "global",
    "export", "start", "element", "code", "data", "data count", "count"
};

void
wasm_push_valtype(Buffer* buf, Bytecode_Type type) {
    switch (type) {
        case BytecodeType_i32: push_u8(buf, 0x7F); break;
        case BytecodeType_i64: push_u8(buf, 0x7E); break;
        case BytecodeType_f32: push_u8(buf, 0x7D); break;
        case BytecodeType_f64: push_u8(buf, 0x7C); break;
        default: unimplemented;
    }
}

void
wasm_push_value(Buffer* buf, Bytecode_Operand op) {
    switch (op.kind) {
        case BytecodeOperand_const_i32: {
            push_u8(buf, 0x41); // i32.const
            push_leb128_s32(buf, op.const_i32);
        } break;
        
        case BytecodeOperand_const_i64: {
            push_u8(buf, 0x42); // i64.const
            push_leb128_s64(buf, op.const_i64);
        } break;
        
        case BytecodeOperand_const_f32: {
            push_u8(buf, 0x43); // i32.const
            push_u32(buf, op.const_i32);
        } break;
        
        case BytecodeOperand_const_f64: {
            push_u8(buf, 0x44); // i64.const
            push_u64(buf, op.const_i64);
        } break;
        
        case BytecodeOperand_stack: {
            push_u8(buf, 0x20); // local.get
            push_leb128_u32(buf, op.stack_index);
        } break;
    }
}

s64
convert_bytecode_insn_to_wasm(Buffer* buf, Bytecode* bc, Bytecode_Instruction* insn, s64 rip) {
    switch (insn->opcode) {
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            Bytecode_Operand* args = (Bytecode_Operand*) (call + 1);
            Bytecode_Function* func = bc->functions[call->func_index];
            Bytecode_Type* arg_types = (Bytecode_Type*) (func + 1);
            
            for (int i = 0; i < (int) func->arg_count; i++) {
                wasm_push_value(buf, args[i]);
            }
            
            push_u8(buf, 0x10);
            push_leb128_u32(buf, func->type_index);
        } break;
        
        case BC_RETURN: {
            wasm_push_value(buf, bc_unary_first(insn));
            
            push_u8(buf, 0x0F); // return
        } break;
        
        case BC_MOV: {
            wasm_push_value(buf, bc_binary_second(insn));
            
            Bytecode_Operand dest = bc_binary_first(insn);
            switch (dest.kind) {
                case BytecodeOperand_stack: {
                    push_u8(buf, 0x21); // local.set
                    push_leb128_u32(buf, dest.stack_index);
                } break;
            }
        } break;
        
        case BC_ADD: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x6A, 0x7C, 0x92, 0xA0 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_SUB: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x6B, 0x7D, 0x93, 0xA1 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_MUL: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x6C, 0x7E, 0x94, 0xA2 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_IDIV: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x6D, 0x7F, 0x95, 0xA3 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_DIV: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x6E, 0x80, 0x96, 0xA4 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_AND: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x71, 0x83 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_OR: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x72, 0x84 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_XOR: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x73, 0x85 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_SHL: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x74, 0x86 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_SAR: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x75, 0x87 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_SHR: {
            wasm_push_value(buf, bc_binary_first(insn));
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0x76, 0x88 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_WRAP_I64: {
            assert(insn->type == BytecodeType_i32);
            wasm_push_value(buf, bc_binary_second(insn));
            push_u8(buf, 0xA7); // i32.wrap_i64
        } break;
        
        case BC_EXTEND_U8: {
            Bytecode_Operand mask = {};
            mask.kind = insn->type == BytecodeType_i32 ? 
                BytecodeOperand_const_i32 : BytecodeOperand_const_i64;
            mask.const_i64 = 0xFF;
            wasm_push_value(buf, bc_binary_second(insn));
            wasm_push_value(buf, mask);
            const u8 opcodes[] = { 0x71, 0x83 };
            push_u8(buf, opcodes[insn->type - 1]); // and x, 0xFF
        } break;
        
        case BC_EXTEND_S8: {
            assert(insn->type == BytecodeType_i32);
            wasm_push_value(buf, bc_binary_second(insn));
            push_u8(buf, 0xC0); // i32.extend_s8
        } break;
        
        case BC_CONVERT_F32_S: {
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0xA8, 0xAF };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_CONVERT_F64_S: {
            wasm_push_value(buf, bc_binary_second(insn));
            const u8 opcodes[] = { 0xAA, 0xB0 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
    }
    
    return rip;
}

s64
convert_bytecode_function_to_wasm(Buffer* buf, Bytecode* bc, Bytecode_Function* func, s64 rip) {
    Bytecode_Instruction* curr = iter_bytecode_instructions(func, 0);
    while (curr->kind) {
        rip = convert_bytecode_insn_to_wasm(buf, bc, curr, rip);
        curr = iter_bytecode_instructions(func, curr);
    }
    
    return rip;
}

void
wasm_push_string(Buffer* buf, string str) {
    push_leb128_u32(buf, (u32) str.count);
    memmove(&buf->data[buf->curr_used], str.data, str.count);
    buf->curr_used += str.count;
}

void
string_builder_pad(String_Builder* sb, smm from_byte, smm width) {
    smm used = sb->curr_used - from_byte;
    for (smm i = used; i < width; i++) {
        string_builder_push(sb, " ");
    }
}

void
string_builder_pad_string(String_Builder* sb, cstring str, smm width) { 
    smm start = sb->curr_used;
    string_builder_push(sb, str);
    string_builder_pad(sb, start, width);
}

// TODO: find a better place for this
void
print_bytes(String_Builder* sb, u8* bytes, int count, int pad=0) {
    for (int byte_index = 0; byte_index < count; byte_index++) {
        if (byte_index != 0 && byte_index % 16 == 0) {
            string_builder_push(sb, "\n");
            string_builder_pad(sb, sb->curr_used, pad);
        }
        
        u8 byte = ((u8*) bytes)[byte_index];
        if (byte > 0xF) {
            string_builder_push_cformat(sb, "%hhx ", byte);
        } else {
            string_builder_push_cformat(sb, "0%hhx ", byte);
        }
    }
}



struct WASM_Section_Marker {
    WASM_Section section;
    u32 offset;
    u32 size;
};

struct WASM_Debug {
    WASM_Section_Marker markers[WASMSection_Count];
    int marker_count;
};

void
DEBUG_wasm_begin_section(WASM_Debug* debug, WASM_Section section, Buffer* buf) {
    if (debug->marker_count < fixed_array_count(debug->markers)) {
        int marker_index = debug->marker_count;
        debug->markers[marker_index].section = section;
        debug->markers[marker_index].offset = (u32) buf->curr_used;
        
        if (marker_index > 0) {
            int last_offset = debug->markers[marker_index - 1].offset;
            debug->markers[marker_index - 1].size = (u32) (buf->curr_used - last_offset);
        }
        
        debug->marker_count++;
    }
}

void
DEBUG_wasm_end(WASM_Debug* debug, Buffer* buf) {
    String_Builder sb = {};
    int left_panel_width = 15;
    
    string_builder_push_format(&sb, "\nWASM (% bytes):\n", f_umm(buf->curr_used));
    
    string_builder_pad_string(&sb, "Signature: ", left_panel_width);
    print_bytes(&sb, buf->data, 8);
    
    if (debug->marker_count > 0) {
        int marker_index = debug->marker_count - 1;
        int last_offset = debug->markers[marker_index].offset;
        debug->markers[marker_index].size = (u32) (buf->curr_used - last_offset);
    }
    
    
    
    for (int marker_index = 0; 
         marker_index < debug->marker_count; 
         marker_index++) {
        
        WASM_Section_Marker marker = debug->markers[marker_index];
        smm start = sb.curr_used + 1;
        string_builder_push_format(&sb, "\n%: ", f_cstring(wasm_section_names[marker.section]));
        string_builder_pad(&sb, start, left_panel_width);
        print_bytes(&sb, buf->data + marker.offset, marker.size, left_panel_width);
        
    }
    
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
}

void
wasm_set_vec_size(Buffer* buf, smm vec_first_byte) {
    // TODO: this is perhaps a bit of a hack to push the leb128 size
    smm size = buf->curr_used - vec_first_byte - 4;
    buf->curr_used = vec_first_byte;
    push_leb128_u32(buf, (u32) size);
    memmove(&buf->data[buf->curr_used], 
            &buf->data[vec_first_byte + 4], size);
    buf->curr_used += size;
}


void
convert_to_wasm_module(Bytecode* bc, s64 stk_usage, Buffer* buf) {
    
    // Define module
    push_u32(buf, 0x6D736100); // Signature (.asm)
    push_u32(buf, 0x1); // Version
    
    WASM_Debug debug = {};
    
    // Type section (1)
    DEBUG_wasm_begin_section(&debug, WASMSection_Type, buf);
    push_u8(buf, WASMSection_Type);
    smm type_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    
    u32 num_types = (u32) array_count(bc->functions);
    push_leb128_u32(buf, num_types); // number of types
    for_array_v(bc->functions, func, _1) {
        
        smm first_byte = buf->curr_used;
        
        push_u8(buf, 0x60); // functype tag
        
        // arguments
        Bytecode_Type* arg_types = (Bytecode_Type*) (func + 1);
        push_leb128_u32(buf, func->arg_count);
        for (u32 i = 0; i < func->arg_count; i++) {
            wasm_push_valtype(buf, arg_types[i]);
        }
        
        // return values
        // TODO: multiple return types
        Bytecode_Type* ret_types = arg_types + func->arg_count;
        push_leb128_u32(buf, func->ret_count);
        for (u32 i = 0; i < func->ret_count; i++) {
            wasm_push_valtype(buf, ret_types[i]);
        }
        
#if 0
        pln("Compiling function ID=%:", f_int(_1));
        for (smm byte_index = first_byte; byte_index < buf->curr_used; byte_index++) {
            u8 byte = buf->data[byte_index];
            if (byte > 0xF) {
                printf("%hhX ", byte);
            } else {
                printf("0%hhX ", byte);
            }
            
            if ((byte_index - first_byte) % 40 == 39) {
                printf("\n");
            }
        }
        printf("\n\n");
#endif
    }
    wasm_set_vec_size(buf, type_section_start);
    
    // Import section (2)
    DEBUG_wasm_begin_section(&debug, WASMSection_Import, buf);
    push_u8(buf, WASMSection_Import);
    push_leb128_u32(buf, 1); // section size
    push_leb128_u32(buf, 0); // num imports: empty, for now
    // TODO: implement imports
    
    // Function section (3)
    DEBUG_wasm_begin_section(&debug, WASMSection_Function, buf);
    push_u8(buf, WASMSection_Function);
    smm function_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, (u32) array_count(bc->functions));
    for (int i = 0; i < array_count(bc->functions); i++) {
        push_leb128_u32(buf, i); // typeidx (funcidx[i] = typeidx[i])
    }
    wasm_set_vec_size(buf, function_section_start);
    
    // Export section (7)
    DEBUG_wasm_begin_section(&debug, WASMSection_Export, buf);
    push_u8(buf, WASMSection_Export);
    push_leb128_u32(buf, 14); // section size
    push_leb128_u32(buf, 1); // number of exports
    
    // - export[0] (for funcidx 1)
    wasm_push_string(buf, string_lit("helloworld")); // name of export
    push_u8(buf, 0x00); // 0x00 = funcidx
    push_leb128_u32(buf, bc->entry_func_index); // funcidx of entrypoint
    
    // Code section (10)
    DEBUG_wasm_begin_section(&debug, WASMSection_Code, buf);
    push_u8(buf, WASMSection_Code);
    smm code_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, (u32) array_count(bc->functions));
    
    s64 rip = 0;
    for_array_v(bc->functions, func, _2) {
        
        smm function_start = buf->curr_used;
        push_u32(buf, 0); // reserve space for size
        push_leb128_u32(buf, (u32) array_count(func->stack)); // number of locals
        
        for_array_v(func->stack, stack, stack_index) {
            push_leb128_u32(buf, 1); 
            wasm_push_valtype(buf, stack.type);
        }
        
        rip = convert_bytecode_function_to_wasm(buf, bc, func, rip);
        //if (type->Function.return_type) {
        // TODO(Alexander): should we only call return on functions with return type?
        //push_u8(buf, 0x0F); // return
        //}
        
        push_u8(buf, 0x0B); // end marker
        
        wasm_set_vec_size(buf, function_start);
    }
    
    wasm_set_vec_size(buf, code_section_start);
    
    DEBUG_wasm_end(&debug, buf);
}
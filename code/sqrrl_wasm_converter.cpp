
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
wasm_load_value(WASM_Assembler* wasm, Buffer* buf, Bytecode_Operand op, Bytecode_Type type, int bitsize=0) {
    switch (op.kind) {
        case BytecodeOperand_const: {
            switch (type) {
                case BytecodeType_i32: {
                    push_u8(buf, 0x41); // i32.const
                    push_leb128_s32(buf, op.const_i32);
                } break;
                
                case BytecodeType_i64: {
                    push_u8(buf, 0x42); // i64.const
                    push_leb128_s64(buf, op.const_i64);
                } break;
                
                case BytecodeType_f32: {
                    push_u8(buf, 0x43); // i32.const
                    push_u32(buf, op.const_i32);
                } break;
                
                case BytecodeType_f64: {
                    push_u8(buf, 0x44); // i64.const
                    push_u64(buf, op.const_i64);
                } break;
            }
        } break;
        
        case BytecodeOperand_stack: {
            // TODO(Alexander): add stack pointer
            push_u8(buf, 0x41); // i32.const
            push_leb128_u32(buf, 0);
            
            switch (type) {
                case BytecodeType_i32: {
                    if (bitsize == 8) {
                        push_u8(buf, 0x2C); // i32.load8_s
                        push_leb128_u32(buf, 0);
                        
                    } else {
                        push_u8(buf, 0x28); // i32.load
                        push_leb128_u32(buf, 2);
                    }
                } break;
                
                default: unimplemented;
            }
            
            push_leb128_u32(buf, wasm->stack_offsets[op.stack_index] + op.memory_offset);
            
            //push_u8(buf, 0x20); // local.get
            //push_leb128_u32(buf, op.stack_index);
        } break;
        
        case BytecodeOperand_register: {
            // noop
        } break;
        
        default: {
            unimplemented;
        } break;
    }
}

void
wasm_prepare_store(WASM_Assembler* wasm, Buffer* buf, Bytecode_Operand dest) {
    switch (dest.kind) {
        case BytecodeOperand_stack: {
            // Setup stack pointer
            push_u8(buf, 0x41); // i32.const
            push_leb128_u32(buf, 0);
        } break;
    }
}

void
wasm_store_value(WASM_Assembler* wasm, Buffer* buf, Bytecode_Operand dest, Bytecode_Type type, int bitsize=0) {
    switch (dest.kind) {
        case BytecodeOperand_stack: {
            switch (type) {
                case BytecodeType_i32: {
                    if (bitsize == 8) {
                        push_u8(buf, 0x3A); // i32.store
                        push_leb128_u32(buf, 0);
                    } else {
                        push_u8(buf, 0x36); // i32.store8
                        push_leb128_u32(buf, 2);
                    }
                } break;
            }
            
            push_leb128_u32(buf, wasm->stack_offsets[dest.stack_index] + dest.memory_offset);
        } break;
    }
}

int
convert_bytecode_insn_to_wasm(WASM_Assembler* wasm, Buffer* buf, Bytecode* bc, Bytecode_Function* func, Bytecode_Instruction* insn, u32 block_depth) {
    switch (insn->opcode) {
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            Bytecode_Operand* args = (Bytecode_Operand*) (call + 1);
            Bytecode_Function* call_func = bc->functions[call->func_index];
            Bytecode_Type* arg_types = (Bytecode_Type*) (call_func + 1);
            
            for (int i = 0; i < (int) call_func->arg_count; i++) {
                wasm_load_value(wasm, buf, args[i], insn->type);
            }
            
            push_u8(buf, 0x10);
            push_leb128_u32(buf, call_func->type_index);
        } break;
        
        case BC_RETURN: {
            wasm_load_value(wasm, buf, bc_unary_first(insn), insn->type);
            push_u8(buf, 0x0F); // return
            //push_u8(buf, 0x0C); // br
            //push_leb128_u32(buf, (u32) block_depth);
        } break;
        
        case BC_BLOCK: {
            push_u8(buf, 0x02);
            push_u8(buf, 0x40);
            block_depth++;
        } break;
        
        case BC_LOOP: {
            push_u8(buf, 0x03);
            push_u8(buf, 0x40);
            block_depth++;
        } break;
        
        case BC_END: {
            push_u8(buf, 0x0B);
            block_depth--;
        } break;
        
        case BC_BRANCH: {
            Bytecode_Branch* branch = (Bytecode_Branch*) insn;
            if (branch->cond.kind) {
                wasm_load_value(wasm, buf, branch->cond, BytecodeType_i32);
                push_u8(buf, 0x0D); // br_if
            } else {
                push_u8(buf, 0x0C); // br
            }
            
            assert(branch->label_index <= block_depth);
            push_leb128_u32(buf, block_depth - branch->label_index);
        } break;
        
#if 0
        case BC_BLOCK: {
            if (((Bytecode_Block*) insn)->label_index != 0) {
                Bytecode_Block* block = (Bytecode_Block*) insn;
                int next_block_depth = block->wasm.blocks + block->wasm.loops;
                int new_blocks = next_block_depth - block_depth;
                pln("new_blocks = %", f_int(new_blocks));
                
                // end previous blocks
                for (int i = 0; i < -new_blocks; i++) {
                    push_u8(buf, 0x0B);
                    pln("END_BLOCK");
                }
                
                // create new blocks
                for (int i = 0; i < new_blocks; i++) {
                    if (block_depth + i > (int) block->wasm.blocks) {
                        push_u8(buf, 0x02);
                        push_u8(buf, 0x40);
                        pln("BLOCK");
                    } else {
                        push_u8(buf, 0x03);
                        push_u8(buf, 0x40);
                        pln("LOOP");
                    }
                }
                
                block_depth = next_block_depth;
            }
        } break;
#endif
        
        case BC_MOV: {
            Bytecode_Operand dest = bc_binary_first(insn);
            wasm_prepare_store(wasm, buf, dest);
            wasm_load_value(wasm, buf, bc_binary_second(insn), insn->type, 0);
            wasm_store_value(wasm, buf, dest, insn->type);
        } break;
        
        case BC_MOV_8: {
            Bytecode_Operand dest = bc_binary_first(insn);
            wasm_prepare_store(wasm, buf, dest);
            wasm_load_value(wasm, buf, bc_binary_second(insn), insn->type, 8);
            wasm_store_value(wasm, buf, dest, insn->type, 8);
        } break;
        
        case BC_INC: {
            Bytecode_Operand first = bc_unary_first(insn);
            wasm_load_value(wasm, buf, first, insn->type);
            Bytecode_Operand val = {};
            val.kind = BytecodeOperand_const;
            val.const_i64 = 1;
            wasm_load_value(wasm, buf, val, insn->type);
            push_u8(buf, wasm_binary_opcodes[insn->type - 1]);
            
            //wasm_save_value(wasm, buf)
            switch (first.kind) {
                case BytecodeOperand_stack: {
                    push_u8(buf, 0x21); // local.set
                    push_leb128_u32(buf, first.stack_index);
                } break;
            }
        } break;
        
        case BC_ADD:
        case BC_SUB:
        case BC_MUL:
        case BC_DIV_S:
        case BC_DIV_U:
        case BC_MOD_S:
        case BC_MOD_U:
        case BC_AND:
        case BC_OR:
        case BC_XOR:
        case BC_SHL:
        case BC_SAR:
        case BC_SHR: {
            Bytecode_Operand first = bc_binary_first(insn);
            wasm_load_value(wasm, buf, first, insn->type);
            wasm_load_value(wasm, buf, bc_binary_second(insn), insn->type);
            push_u8(buf, wasm_binary_opcodes[(insn->opcode - BC_ADD)*4 + insn->type - 1]);
            
            switch (first.kind) {
                case BytecodeOperand_stack: {
                    push_u8(buf, 0x21); // local.set
                    push_leb128_u32(buf, first.stack_index);
                } break;
            }
        } break;
        
        case BC_EQ:
        case BC_GT_S:
        case BC_GT_U:
        case BC_GE_S:
        case BC_GE_U:
        case BC_LT_U:
        case BC_LT_S:
        case BC_LE_U:
        case BC_LE_S:
        case BC_NEQ: {
            wasm_load_value(wasm, buf, bc_binary_first(insn), insn->type);
            wasm_load_value(wasm, buf, bc_binary_second(insn), insn->type);
            Bytecode_Branch* branch = (Bytecode_Branch*) ((u8*) insn + insn->next_insn);
            int opcode_index;
            if (branch->opcode == BC_BRANCH) {
                // Invert the condition for the branch
                int op = BC_NEQ - insn->opcode;
                opcode_index = (BC_NEQ - insn->opcode)*4 + insn->type - 1;
            } else {
                opcode_index = (insn->opcode - BC_EQ)*4 + insn->type - 1;
            }
            
            push_u8(buf, wasm_comparator_opcodes[opcode_index]);
        } break;
        
        case BC_WRAP_I64: {
            assert(insn->type == BytecodeType_i32);
            wasm_load_value(wasm, buf, bc_binary_second(insn), BytecodeType_i64);
            push_u8(buf, 0xA7); // i32.wrap_i64
        } break;
        
        case BC_EXTEND_U8: {
            Bytecode_Operand mask = {};
            mask.kind = BytecodeOperand_const;
            mask.const_i64 = 0xFF;
            
            wasm_load_value(wasm, buf, bc_binary_second(insn), insn->type);
            wasm_load_value(wasm, buf, mask, insn->type);
            const u8 opcodes[] = { 0x71, 0x83 };
            push_u8(buf, opcodes[insn->type - 1]); // and x, 0xFF
        } break;
        
        case BC_EXTEND_S8: {
            assert(insn->type == BytecodeType_i32);
            wasm_load_value(wasm, buf, bc_binary_second(insn), insn->type);
            push_u8(buf, 0xC0); // i32.extend_s8
        } break;
        
        case BC_CONVERT_F32_S: {
            wasm_load_value(wasm, buf, bc_binary_second(insn), BytecodeType_f32);
            const u8 opcodes[] = { 0xA8, 0xAF };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        case BC_CONVERT_F64_S: {
            wasm_load_value(wasm, buf, bc_binary_second(insn), BytecodeType_f64);
            const u8 opcodes[] = { 0xAA, 0xB0 };
            push_u8(buf, opcodes[insn->type - 1]);
        } break;
        
        default: {
            assert(0 && "invalid bytecode instruction");
        } break;
    }
    
    return block_depth;
}

void
convert_bytecode_function_to_wasm(WASM_Assembler* wasm, Buffer* buf, Bytecode* bc, Bytecode_Function* func) {
    // Local stack
    wasm->stack_offsets = (u32*) calloc(array_count(func->stack), sizeof(u32));
    wasm->stack_usage = 0;
    for (int i = 0; i < array_count(func->stack); i++) {
        Stack_Entry stk = func->stack[i];
        wasm->stack_usage = (u32) align_forward(wasm->stack_usage, stk.align);
        wasm->stack_offsets[i] = wasm->stack_usage;
        wasm->stack_usage += stk.size;
    }
    
    u32 block_depth = 0;
    for_bc_insn(func, insn) {
        block_depth = convert_bytecode_insn_to_wasm(wasm, buf, bc, func, insn, block_depth);
    }
    
    if (func->ret_count == 1) {
        // TODO(Alexander): multiple returns
        Bytecode_Type* return_types = (Bytecode_Type*) (func + 1);
        
        Bytecode_Operand op = {};
        op.kind = BytecodeOperand_const;
        wasm_load_value(wasm, buf, op, return_types[0]);
    }
    push_u8(buf, 0x0F); // return
    
    free(wasm->stack_offsets);
}

void
wasm_push_string(Buffer* buf, string str) {
    push_leb128_u32(buf, (u32) str.count);
    memmove(&buf->data[buf->curr_used], str.data, str.count);
    buf->curr_used += str.count;
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
    
    // Memory section (5)
    DEBUG_wasm_begin_section(&debug, WASMSection_Memory, buf);
    push_u8(buf, WASMSection_Memory);
    push_leb128_u32(buf, 3); // section size
    push_leb128_u32(buf, 1); // num memories (max 1 allowed atm.
    push_u8(buf, 0x00); // 0x00 = min bound
    push_leb128_u32(buf, 1); // min number of 64-kB pages
    // TODO(Alexander): calculate the actual minimum used up by the data packer.
    
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
    
    WASM_Assembler wasm = {};
    
    for_array_v(bc->functions, func, _2) {
        
        smm function_start = buf->curr_used;
        push_u32(buf, 0); // reserve space for size
        push_leb128_u32(buf, (u32) array_count(func->stack)); // number of locals
        
        for_array_v(func->stack, stack, stack_index) {
            push_leb128_u32(buf, 1); 
            wasm_push_valtype(buf, stack.type);
        }
        
        convert_bytecode_function_to_wasm(&wasm, buf, bc, func);
        
        push_u8(buf, 0x0B); // end marker
        
        wasm_set_vec_size(buf, function_start);
    }
    
    wasm_set_vec_size(buf, code_section_start);
    
    DEBUG_wasm_end(&debug, buf);
}
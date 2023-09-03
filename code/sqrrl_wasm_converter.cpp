
int
convert_bytecode_insn_to_wasm(WASM_Assembler* wasm, Buffer* buf, Bytecode* bc, Bytecode_Function* func, Bytecode_Instruction* insn, u32 block_depth) {
    Bytecode_Binary* bc_insn = (Bytecode_Binary*) insn;
    
    switch (insn->opcode) {
        case BC_DEBUG_BREAK: {
            // noop, doesn't exist in wasm AFAIK
        } break;
        
        case BC_INT_CONST: {
            wasm_prepare_store(buf);
            wasm_i64_const(buf, (s64) bc_insn->const_i64);
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        case BC_F32_CONST: {
            wasm_prepare_store(buf);
            wasm_f32_const(buf, bc_insn->const_f32);
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        case BC_F64_CONST: {
            wasm_prepare_store(buf);
            wasm_f64_const(buf, bc_insn->const_f64);
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        case BC_LOCAL: {
            // Push local on the stack
            s64 disp = wasm->stack_offset_for_locals;
            disp = (s64) align_forward(disp, bc_insn->arg1_index);
            wasm->stack_offset_for_locals = disp + bc_insn->arg0_index;
            
            // TODO(Alexander): 64-bit addressing support!
            wasm_prepare_store(buf);
            wasm_push_stack_pointer(buf);
            wasm_i32_const(buf, (s32) disp);
            wasm_i32_add(buf);
            push_u8(buf, 0xAC); // i64.extend_i32_s
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        case BC_PTR_STRUCT_FIELD: {
            // TODO(Alexander): 64-bit addressing support!
            wasm_prepare_store(buf);
            wasm_push_stack_pointer(buf);
            wasm_i64_load(buf, bc_insn->arg0_index*8);
            wasm_i64_const(buf, bc_insn->arg1_index);
            wasm_i64_add(buf);
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        case BC_LOAD: {
            // TODO(Alexander): 64-bit addressing support!
            wasm_prepare_store(buf);
            wasm_push_stack_pointer(buf);
            wasm_i32_load(buf, bc_insn->arg0_index*8);
            wasm_i64_load(buf, 0);
            Bytecode_Type ptr_type = { BC_TYPE_PTR, 8, 0 };
            wasm_store_register(buf, ptr_type, bc_insn->res_index);
        } break;
        
        case BC_STORE: {
            // TODO(Alexander): 64-bit addressing support!
            Bytecode_Type val_type = register_type(func, bc_insn->arg1_index);
            wasm_push_stack_pointer(buf); 
            wasm_i32_load(buf, bc_insn->arg0_index*8); // [ptr]
            wasm_push_stack_pointer(buf);
            wasm_load_extend(buf, val_type, bc_insn->arg1_index*8); // [ptr (arg0), val (arg1)] 
            wasm_store_register(buf, val_type, 0); // []: ptr <- val
        } break;
        
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            Bytecode_Function* call_func = bc->functions[call->func_index];
            Bytecode_Function_Arg* formal_args = function_arg_types(call_func);
            
            int* args = bc_call_args(call);
            for (int i = 0; i < (int) call_func->arg_count; i++) {
                int arg_index = args[i];
                wasm_load_register(func, buf, arg_index);
            }
            
            push_u8(buf, 0x10); // call
            push_leb128_u32(buf, call_func->type_index);
            
            if (call_func->ret_count > 0) {
                assert(call_func->ret_count == 1 && "TODO: multiple returns");
                int res_index = args[call_func->arg_count];
                wasm_prepare_store(buf, wasm->tmp_local_i64);
                wasm_store_register(buf, register_type(func, res_index), res_index);
            }
        } break;
        
        case BC_RETURN: {
            //if (func->ret_count == 1 && func->stack[func->arg_count].size > 8) {
            //wasm_push_addr_of(wasm, buf, bc_unary_first(insn), insn->type);
            //} else {
            wasm_load_register(func, buf, bc_insn->arg0_index);
            // TODO(Alexander): add support for f32 and f64!!!
            wasm_local_set(buf, wasm->tmp_local_i64);
            
            //push_u8(buf, 0x0F); // return
            push_u8(buf, 0x0C); // br
            push_leb128_u32(buf, (u32) block_depth - 1);
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
            if (branch->cond >= 0) {
                wasm_push_stack_pointer(buf);
                
                // Condition is always stored as i32
                push_u8(buf, 0x28); // i32.load
                push_leb128_u32(buf, 2);
                push_leb128_u32(buf, branch->cond*8);
                push_u8(buf, 0x0D); // br_if
            } else {
                push_u8(buf, 0x0C); // br
            }
            
            assert(branch->label_index <= block_depth);
            push_leb128_u32(buf, block_depth - branch->label_index - 1);
        } break;
        
        case BC_INC: {
            wasm_prepare_store(buf);
            wasm_load_register_extend(func, buf, bc_insn->arg0_index);
            wasm_i64_const(buf, 1);
            push_u8(buf, 0x7C); // i64.add
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        case BC_DEC: {
            wasm_prepare_store(buf);
            wasm_load_register_extend(func, buf, bc_insn->arg0_index);
            wasm_i64_const(buf, 1);
            push_u8(buf, 0x7D); // i64.sub
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        //case BC_ADDR_OF: {
        //wasm_push_addr_of(wasm, buf, bc_binary_second(insn), insn->type);
        //} break;
        
        case BC_DEREF: {
            unimplemented;
            //wasm_load_value(wasm, buf, bc_binary_second(insn), BytecodeType_i32);
            //wasm_push_load(buf, insn->type, 0);
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
            Bytecode_Type type = register_type(func, bc_insn->arg0_index);
            wasm_prepare_store(buf);
            wasm_load_register_extend(func, buf, bc_insn->arg0_index);
            wasm_load_register_extend(func, buf, bc_insn->arg1_index);
            
            int type_index = 1;
            if (type.kind == BC_TYPE_FLOAT) {
                type_index = (type.size == 8) ? 3 : 2;
            }
            push_u8(buf, wasm_binary_opcodes[(insn->opcode - BC_ADD)*4 + type_index]);
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
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
            Bytecode_Type type = register_type(func, bc_insn->arg0_index);
            wasm_prepare_store(buf);
            wasm_load_register_extend(func, buf, bc_insn->arg0_index);
            wasm_load_register_extend(func, buf, bc_insn->arg1_index);
            Bytecode_Branch* branch = (Bytecode_Branch*) ((u8*) insn + insn->next_insn);
            int type_index = 1;
            if (type.kind == BC_TYPE_FLOAT) {
                type_index = (type.size == 8) ? 3 : 2;
            }
            
            int opcode_index;
            if (branch->opcode == BC_BRANCH) {
                // Invert the condition for the branch
                int op = BC_NEQ - insn->opcode;
                opcode_index = (BC_NEQ - insn->opcode)*4 + type_index;
            } else {
                opcode_index = (insn->opcode - BC_EQ)*4 + type_index;
            }
            
            push_u8(buf, wasm_comparator_opcodes[opcode_index]);
            
            // Comparator always outputs a i32
            int res_index = bc_insn->res_index;
            push_u8(buf, 0x36); // i32.store
            push_leb128_u32(buf, 2);
            push_leb128_u32(buf, res_index*8);
        } break;
        
        case BC_TRUNCATE: {
            Bytecode_Type type = register_type(func, bc_insn->arg0_index);
            if (type.size == 8) {
                wasm_load_register(func, buf, bc_insn->arg0_index);
                push_u8(buf, 0xA7); // i32.wrap_i64
            } else {
                unimplemented;
            }
        } break;
        
        case BC_EXTEND: {
            wasm_prepare_store(buf);
            wasm_load_register_extend(func, buf, bc_insn->arg0_index);
            // TODO(Alexander): for now we store everything in memory but
            // if we stop doing that we need to fully implement this extend function!
            //wasm_extend(buf, src_type, bc_insn->arg0_index); // not needed now!
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        case BC_FLOAT_TO_INT: {
            Bytecode_Type type = register_type(func, bc_insn->arg0_index);
            wasm_prepare_store(buf);
            wasm_load_register(func, buf, bc_insn->arg0_index);
            push_u8(buf, (type.size == 8) ? 0xB0 : 0xAF);
            wasm_store_register(buf, register_type(func, bc_insn->res_index), bc_insn->res_index);
        } break;
        
        // 64-bit version of above
        //wasm_load_value(wasm, buf, bc_binary_second(insn), BytecodeType_f64);
        //const u8 opcodes[] = { 0xAA, 0xB0 };
        //push_u8(buf, opcodes[insn->type - 1]);
        
        case BC_MEMSET: {
            unimplemented;
#if 0
            Bytecode_Memory* mem = (Bytecode_Memory*) insn;
            
            if (mem->src.kind == BytecodeOperand_register) {
                push_u8(buf, 0x21); // local.set
                push_leb128_u32(buf, wasm->tmp_local_i32);
            }
            
            //wasm_push_addr_of(wasm, buf, mem->dest, BytecodeType_i32);
            
            if (mem->src.kind == BytecodeOperand_register) {
                push_u8(buf, 0x20); // local.get
                push_leb128_u32(buf, wasm->tmp_local_i32);
            }
            
            if (mem->src.kind != BytecodeOperand_register) {
                //wasm_push_addr_of(wasm, buf, mem->src, BytecodeType_i32);
            } else {
                //wasm_load_value(wasm, buf, mem->src, BytecodeType_i32);
            }
            
            push_u8(buf, 0x41); // i32.const
            push_leb128_s32(buf, mem->size);
            push_u32(buf, 0x00000AFC); // memory.copy
#endif
        } break;
        
        default: {
            assert(0 && "invalid bytecode instruction");
        } break;
    }
    
    return block_depth;
}

void
convert_bytecode_function_to_wasm(WASM_Assembler* wasm, Buffer* buf, Bytecode* bc, Bytecode_Function* func) {
    u32 block_depth = 0;
    
    // Local stack
    u32 register_count = func->register_count;
    wasm->stack_size = register_count*8;
    wasm->stack_offset_for_locals = wasm->stack_size;
    for_bc_insn(func, insn) {
        if (insn->opcode == BC_LOCAL) {
            Bytecode_Binary* bc_insn = (Bytecode_Binary*) insn;
            wasm->stack_size = (s32) align_forward(wasm->stack_size, bc_insn->arg1_index);
            wasm->stack_size += bc_insn->arg0_index;
        }
    }
    
    // Prologue
    push_u8(buf, 0x02); // block
    push_u8(buf, 0x40);
    block_depth++;
    push_u8(buf, 0x23); // global.get
    push_leb128_u32(buf, 0);
    push_u8(buf, 0x41); // i32.const
    push_leb128_s32(buf, (s32) wasm->stack_size);
    push_u8(buf, 0x6B); // i32.sub
    push_u8(buf, 0x24); // global.set
    push_leb128_u32(buf, 0);
    
    // Save parameters to local variables
    Bytecode_Function_Arg* formal_args = function_arg_types(func);
    for (int register_index = 0; register_index < func->arg_count; register_index++) {
        // TODO: copy larger structs
        //if (stk.size <= 8) {
        //wasm_prepare_store(buf, op, op, arg_types[i]);
        //push_u8(buf, 0x20); // local.get
        //push_leb128_u32(buf, op.stack_index);
        //wasm_store_value(wasm, buf, op, arg_types[i], stk.size*8);
        
        wasm_prepare_store(buf);
        wasm_local_get(buf, register_index);
        wasm_store_register(buf, register_type(func, register_index), register_index);
        
        
#if 0 // TODO(Alexander): copy larger structures
        u32 offset = register_index*8;
        // dest
        push_u8(buf, 0x23); // global.get
        push_leb128_u32(buf, 0);
        if (offset != 0) {
            push_u8(buf, 0x41); // i32.const
            push_leb128_s32(buf, offset);
            push_u8(buf, 0x6A); // i32.add
        }
        
        // src
        push_u8(buf, 0x20); // local.get
        push_leb128_u32(buf, register_index);
        
        // size
        push_u8(buf, 0x41); // i32.const
        push_leb128_s32(buf, 8);
        push_u32(buf, 0x00000AFC); // memory.copy
#endif
    }
    
    //Bytecode_Type* args = 
    //Bytecode_Operand dest = bc_binary_first(insn);
    //Bytecode_Operand src = bc_binary_second(insn);
    //wasm_prepare_store(buf, dest, src, insn->type);
    //wasm_load_value(wasm, buf, src, insn->type, 0);
    //wasm_store_value(wasm, buf, dest, insn->type);
    
    for_bc_insn(func, insn) {
        block_depth = convert_bytecode_insn_to_wasm(wasm, buf, bc, func, insn, block_depth);
    }
    
    // Epilogue
    push_u8(buf, 0x0B); // end of block
    block_depth--;
    push_u8(buf, 0x23); // global.get
    push_leb128_u32(buf, 0);
    push_u8(buf, 0x41); // i32.const
    push_leb128_s32(buf, (s32) wasm->stack_size);
    push_u8(buf, 0x6A); // i32.add
    push_u8(buf, 0x24); // global.set
    push_leb128_u32(buf, 0);
    
    if (func->ret_count == 1) {
        // TODO(Alexander): multiple returns
        Bytecode_Type type = formal_args[func->arg_count].type;
        // TODO(Alexander): add support for f32 and f64!!!
        wasm_local_get(buf, wasm->tmp_local_i64);
    }
    
    push_u8(buf, 0x0F); // return
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
    
    string_builder_pad_string(&sb, "signature: ", left_panel_width);
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
convert_to_wasm_module(Bytecode* bc, Data_Packer* data_packer, s64 stk_usage, Buffer* buf) {
    
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
        Bytecode_Function_Arg* formal_args = function_arg_types(func);
        push_leb128_u32(buf, func->arg_count);
        for (int i = 0; i < func->arg_count; i++) {
            wasm_push_valtype(buf, formal_args[i].type);
        }
        
        // return values
        Bytecode_Function_Arg* return_args = function_ret_types(func);
        push_leb128_u32(buf, func->ret_count);
        for (int i = 0; i < func->ret_count; i++) {
            wasm_push_valtype(buf, return_args[i].type);
        }
    }
    wasm_set_vec_size(buf, type_section_start);
    
    // Import section (2)
    DEBUG_wasm_begin_section(&debug, WASMSection_Import, buf);
    push_u8(buf, WASMSection_Import);
    smm import_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, (u32) array_count(bc->imports)); // num imports
    for (int i = 0; i < array_count(bc->imports); i++) {
        pln("%::%", f_var(bc->imports[i].module), f_var(bc->imports[i].function));
        // TODO(Alexander): imports are hardcoded for now
        wasm_push_string(buf, vars_load_string(bc->imports[i].module));
        wasm_push_string(buf, vars_load_string(bc->imports[i].function));
        push_u8(buf, 0x00); // importdesc:typeidx
        push_leb128_u32(buf, i);
    }
    wasm_set_vec_size(buf, import_section_start);
    
    // Function section (3)
    u32 function_count = (u32) (array_count(bc->functions) - array_count(bc->imports));
    DEBUG_wasm_begin_section(&debug, WASMSection_Function, buf);
    push_u8(buf, WASMSection_Function);
    smm function_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, function_count);
    for (int i = (int) array_count(bc->imports); i < array_count(bc->functions); i++) {
        push_leb128_u32(buf, i); // typeidx (NOTE: assumes funcidx = typeidx)
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
    
    // Global section (6)
    DEBUG_wasm_begin_section(&debug, WASMSection_Global, buf);
    push_u8(buf, WASMSection_Global);
    smm global_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, 1); // number of globals
    push_u16(buf, 0x017F); // mut i32
    push_u8(buf, 0x41); // i32.const
    u32 stack_size = kilobytes(32); // half a WASM page // TODO(Alexander): make configurable
    push_leb128_s32(buf, stack_size); // stack space
    push_u8(buf, 0x0B); // end
    wasm_set_vec_size(buf, global_section_start);
    
    // Export section (7)
    DEBUG_wasm_begin_section(&debug, WASMSection_Export, buf);
    push_u8(buf, WASMSection_Export);
    smm export_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, 2); // number of exports
    
    // - export[0] (for funcidx 1)
    wasm_push_string(buf, string_lit("main")); // name of export
    push_u8(buf, 0x00); // 0x00 = funcidx
    push_leb128_u32(buf, bc->entry_func_index); // funcidx of entrypoint
    
    // - export[1] (for memidx 0)
    wasm_push_string(buf, string_lit("memory")); // name of export
    push_u8(buf, 0x02); // 0x00 = memidx
    push_leb128_u32(buf, 0); // main memidx
    wasm_set_vec_size(buf, export_section_start);
    
    // Code section (10)
    DEBUG_wasm_begin_section(&debug, WASMSection_Code, buf);
    push_u8(buf, WASMSection_Code);
    smm code_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, function_count);
    
    WASM_Assembler wasm = {};
    wasm.rdata_offset = stack_size;
    wasm.data_offset = stack_size + (u32) arena_total_used(&data_packer->rdata_arena);
    
    for_array_v(bc->functions, func, _2) {
        if (func->is_imported) {
            continue;
        }
        
        smm function_start = buf->curr_used;
        push_u32(buf, 0); // reserve space for size
        
        // TODO(Alexander): hack, use tmp locals can be used for reordering the operand stack
        //push_leb128_u32(buf, (u32) array_count(func->stack)); // number of locals
        push_leb128_u32(buf, 4); // number of locals
        
        //for_array_v(func->stack, stack, stack_index) {
        //push_leb128_u32(buf, 1); 
        //wasm_push_valtype(buf, stack.type);
        //}
        
        push_leb128_u32(buf, 1); 
        push_u8(buf, 0x7D);
        wasm.tmp_local_i32 = func->arg_count;
        
        push_leb128_u32(buf, 1); 
        push_u8(buf, 0x7E);
        wasm.tmp_local_i64 = func->arg_count + 1;
        
        push_leb128_u32(buf, 1); 
        push_u8(buf, 0x7B);
        wasm.tmp_local_f32 = func->arg_count + 2;
        
        push_leb128_u32(buf, 1); 
        push_u8(buf, 0x7C);
        wasm.tmp_local_f64 = func->arg_count + 3;
        
        convert_bytecode_function_to_wasm(&wasm, buf, bc, func);
        
        push_u8(buf, 0x0B); // end marker
        
        wasm_set_vec_size(buf, function_start);
    }
    wasm_set_vec_size(buf, code_section_start);
    
    // Data section (11)
    DEBUG_wasm_begin_section(&debug, WASMSection_Data, buf);
    push_u8(buf, WASMSection_Data);
    smm data_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, 1); // num data segments
    
    // Segment 0 - rdata + data arenas
    // TODO(Alexander): there probably isn't much point in separating these two
    // since there is no memory overwrite protection supported.
    push_u8(buf, 0); // active memory 0
    push_u8(buf, 0x41); // i32.const
    push_leb128_s32(buf, wasm.rdata_offset); // memory offset (expr)
    push_u8(buf, 0x0B); // end
    u32 memory_size = (u32) (arena_total_used(&data_packer->rdata_arena) +
                             arena_total_used(&data_packer->data_arena));
    push_leb128_u32(buf, memory_size); // size of memory
    u8* dest = buf->data + buf->curr_used;
    u32 copied_size = (u32) copy_memory_block(dest, data_packer->rdata_arena.current_block);
    dest = buf->data + buf->curr_used + copied_size;
    copied_size += (u32) copy_memory_block(dest, data_packer->data_arena.current_block);
    assert(memory_size == copied_size);
    buf->curr_used += memory_size;
    
    
    wasm_set_vec_size(buf, data_section_start);
    
    
    DEBUG_wasm_end(&debug, buf);
}
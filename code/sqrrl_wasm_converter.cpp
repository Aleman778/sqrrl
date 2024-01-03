
int
convert_bytecode_insn_to_wasm(WASM_Assembler* wasm, Buffer* buf, Bytecode* module, Bytecode_Function* func, Bytecode_Instruction* insn, u32 block_depth) {
    Bytecode_Binary* bc = (Bytecode_Binary*) insn;
    
    switch (insn->opcode) {
        case BC_DEBUG_BREAK: {
            // noop, doesn't exist in wasm AFAIK
        } break;
        
        case BC_DROP: {
            // TODO(Alexander): implement this later
        } break;
        
        case BC_INT_CONST: {
            wasm_prepare_store(buf);
            wasm_i64_const(buf, (s64) ((Bytecode_Const_Int*) bc)->val);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_F32_CONST: {
            wasm_prepare_store(buf);
            wasm_f32_const(buf, ((Bytecode_Const_F32*) bc)->val);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_F64_CONST: {
            wasm_prepare_store(buf);
            wasm_f64_const(buf, ((Bytecode_Const_F64*) bc)->val);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_LOCAL: {
            // Push local on the stack
            s64 disp = wasm->stack_offset_for_locals;
            disp = (s64) align_forward(disp, bc->b_index);
            wasm->stack_offset_for_locals = disp + bc->a_index;
            
            // TODO(Alexander): 64-bit addressing support!
            wasm_prepare_store(buf);
            wasm_push_stack_pointer(buf);
            wasm_i32_const(buf, (s32) disp);
            wasm_i32_add(buf);
            push_u8(buf, 0xAC); // i64.extend_i32_s
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_FUNCTION: {
            wasm_prepare_store(buf);
            wasm_i64_const(buf, bc->a_index);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_GLOBAL: {
            Bytecode_Global* g = &module->globals[bc->a_index];
            wasm_prepare_store(buf);
            switch (g->kind) {
                case BC_MEM_READ_ONLY: {
                    wasm_i64_const(buf, wasm->rdata_offset + g->offset);
                } break;
                
                case BC_MEM_READ_WRITE: {
                    wasm_i64_const(buf, wasm->data_offset + g->offset);
                } break;
            }
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_ARRAY_ACCESS: {
            Bytecode_Array_Access* array_access = (Bytecode_Array_Access*) bc;
            wasm_prepare_store(buf);
            wasm_push_stack_pointer(buf);
            wasm_i64_load(buf, bc->a_index*8);
            wasm_push_stack_pointer(buf);
            wasm_i64_load(buf, bc->b_index*8);
            if (array_access->stride > 1) {
                wasm_i64_const(buf, array_access->stride);
                wasm_i64_mul(buf);
            }
            wasm_i64_add(buf);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_FIELD_ACCESS: {
            wasm_prepare_store(buf);
            wasm_push_stack_pointer(buf);
            wasm_i64_load(buf, bc->a_index*8);
            wasm_i64_const(buf, bc->b_index);
            wasm_i64_add(buf);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_LEA: {
            // TODO(Alexander): 64-bit addressing support!
            wasm_prepare_store(buf);
            wasm_push_stack_pointer(buf);
            wasm_i32_const(buf, bc->a_index*8);
            wasm_i32_add(buf);
            push_u8(buf, 0xAD); // i64.extend_i32_u
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_COPY: {
            // TODO(Alexander): 64-bit addressing support!
            wasm_prepare_store(buf);
            wasm_load_register(wasm, buf, bc->a_index);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_LOAD: {
            // TODO(Alexander): 64-bit addressing support!
            wasm_prepare_store(buf);
            wasm_push_stack_pointer(buf);
            wasm_i32_load(buf, bc->a_index*8);
            wasm_i64_load(buf, 0);
            Bytecode_Type ptr_type = { BC_TYPE_INT, 8, 0 };
            wasm_store_register(wasm, buf, ptr_type, bc->res_index);
            wasm->slots[bc->res_index] = { bc->type };
        } break;
        
        case BC_STORE: {
            // TODO(Alexander): 64-bit addressing support!
            Bytecode_Type val_type = wasm_register_type(wasm, bc->a_index);
            wasm_push_stack_pointer(buf); 
            wasm_i32_load(buf, bc->res_index*8); // [ptr]
            wasm_push_stack_pointer(buf);
            wasm_load_extend(buf, val_type, bc->a_index*8); // [ptr (a), val (b)] 
            
            if (val_type.kind == BC_TYPE_FLOAT) {
                if (val_type.size == 8) {
                    wasm_f64_store(buf, 0);
                } else {
                    wasm_f32_store(buf, 0);
                }
            } else {
                switch (val_type.size) {
                    case 1: {
                        wasm_i64_store8(buf, 0);
                    } break;
                    
                    case 2: {
                        wasm_i64_store16(buf, 0);
                    } break;
                    
                    case 4: {
                        wasm_i64_store32(buf, 0);
                    } break;
                    
                    default: {
                        wasm_i64_store(buf, 0);
                    } break;
                }
            }
        } break;
        
        case BC_CALL:
        case BC_CALL_INDIRECT: {
            Bytecode_Call* call = (Bytecode_Call*) bc;
            Bytecode_Function* target = module->functions[call->func_index];
            
            int* args = (int*) bc_call_args(call);
            for (int arg_index = target->ret_count;
                 arg_index < call->arg_count;
                 arg_index++) {
                int src_index = args[arg_index];
                wasm_load_register(wasm, buf, src_index);
            }
            
            if (bc->opcode == BC_CALL_INDIRECT) {
                unimplemented;
                // TODO(Alexander): we need support for finding type declarations in bytecode for just the signature of indirect calls.
                //Bytecode_Call_Indirect* indirect_call = (Bytecode_Call_Indirect*) bc;
                //wasm_i32_load(buf, indirect_call->func_ptr_index*8);
                //push_u8(buf, 0x11); // call_indirect
            } else {
                push_u8(buf, 0x10); // call
                push_leb128_u32(buf, call->func_index);
            }
            
            if (target->ret_count == 1) {
                wasm_prepare_store(buf, wasm_tmp_local(wasm, bc->type));
                wasm_store_register(wasm, buf, bc->type, args[0]);
            }
        } break;
        
        case BC_RETURN: {
            if (bc->res_index >= 0) {
                wasm_load_register_extend(wasm, buf, bc->res_index);
                wasm_local_set(buf, wasm_tmp_local(wasm, bc->type));
            }
            
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
                wasm_i32_load(buf, branch->cond*8);
                push_u8(buf, 0x0D); // br_if
            } else {
                push_u8(buf, 0x0C); // br
            }
            
            assert(branch->label_index <= block_depth);
            push_leb128_u32(buf, block_depth - branch->label_index - 1);
        } break;
        
        case BC_INC: {
            wasm_prepare_store(buf);
            wasm_load_register_extend(wasm, buf, bc->a_index);
            wasm_i64_const(buf, 1);
            push_u8(buf, 0x7C); // i64.add
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_DEC: {
            wasm_prepare_store(buf);
            wasm_load_register_extend(wasm, buf, bc->a_index);
            wasm_i64_const(buf, 1);
            wasm_i64_sub(buf);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_NEG: {
            Bytecode_Type type = wasm_register_type(wasm, bc->a_index);
            if (type.kind & BC_TYPE_FLOAT) {
                wasm_prepare_store(buf);
                wasm_load_register_extend(wasm, buf, bc->a_index);
                wasm_float_neg(buf, type.size);
                wasm_store_register(wasm, buf, bc->type, bc->res_index);
                
            } else {
                wasm_prepare_store(buf);
                wasm_i64_const(buf, 0);
                wasm_load_register_extend(wasm, buf, bc->a_index);
                wasm_i64_sub(buf);
                wasm_store_register(wasm, buf, bc->type, bc->res_index);
            }
        } break;
        
        case BC_NOT: {
            wasm_prepare_store(buf);
            wasm_i64_const(buf, -1);
            wasm_load_register_extend(wasm, buf, bc->a_index);
            wasm_i64_xor(buf);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
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
            Bytecode_Type type = bc->type;
            wasm_prepare_store(buf);
            wasm_load_register_extend(wasm, buf, bc->a_index);
            wasm_load_register_extend(wasm, buf, bc->b_index);
            
            int type_index = 1;
            if (type.kind == BC_TYPE_FLOAT) {
                type_index = (type.size == 8) ? 3 : 2;
            }
            push_u8(buf, wasm_binary_opcodes[(insn->opcode - BC_ADD)*4 + type_index]);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
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
            Bytecode_Type type = wasm_register_type(wasm, bc->a_index);
            wasm_prepare_store(buf);
            wasm_load_register_extend(wasm, buf, bc->a_index);
            wasm_load_register_extend(wasm, buf, bc->b_index);
            Bytecode_Branch* branch = (Bytecode_Branch*) ((u8*) insn + insn->next_insn);
            int type_index = 1;
            if (type.kind == BC_TYPE_FLOAT) {
                type_index = (type.size == 8) ? 3 : 2;
            }
            
            int opcode_index = (insn->opcode - BC_EQ)*4 + type_index;
            push_u8(buf, wasm_comparator_opcodes[opcode_index]);
            
            // Comparator always outputs a i32
            wasm_i32_store(buf, bc->res_index*8);
        } break;
        
        case BC_TRUNCATE: {
            Bytecode_Type type = wasm_register_type(wasm, bc->a_index);
            if (type.size == 8) {
                wasm_load_register(wasm, buf, bc->a_index);
                push_u8(buf, 0xA7); // i32.wrap_i64
            } else {
                unimplemented;
            }
        } break;
        
        case BC_EXTEND: {
            wasm_prepare_store(buf);
            wasm_load_register_extend(wasm, buf, bc->a_index);
            // TODO(Alexander): for now we store everything in memory but
            // if we stop doing that we need to fully implement this extend function!
            //wasm_extend(buf, src_type, bc->a_index); // not needed now!
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        case BC_FLOAT_TO_INT: {
            Bytecode_Type type = wasm_register_type(wasm, bc->a_index);
            wasm_prepare_store(buf);
            wasm_load_register(wasm, buf, bc->a_index);
            push_u8(buf, (type.size == 8) ? 0xB0 : 0xAF);
            wasm_store_register(wasm, buf, bc->type, bc->res_index);
        } break;
        
        // 64-bit version of above
        //wasm_load_value(wasm, buf, bc_binary_second(insn), BytecodeType_f64);
        //const u8 opcodes[] = { 0xAA, 0xB0 };
        //push_u8(buf, opcodes[insn->type - 1]);
        
        case BC_MEMCPY: {
            wasm_push_stack_pointer(buf);
            wasm_i32_load(buf, bc->res_index*8);
            
            wasm_push_stack_pointer(buf);
            wasm_i32_load(buf, bc->a_index*8);
            
            wasm_i32_const(buf, bc->b_index);
            push_u32(buf, 0x00000AFC); // memory.copy [dest: i32, src: i32, size: i32]
        } break;
        
        case BC_MEMSET: {
            wasm_push_stack_pointer(buf);
            wasm_i32_load(buf, bc->res_index*8);
            
            wasm_i32_const(buf, bc->a_index);
            
            push_u8(buf, 0x41); // i32.const
            push_leb128_s32(buf, bc->b_index);
            push_u24(buf, 0x000BFC); // memory.fill [dest: i32, val: i32, size: i32]
        } break;
        
        default: {
            assert(0 && "invalid bytecode instruction");
        } break;
    }
    
    return block_depth;
}

void
convert_bytecode_function_to_wasm(WASM_Assembler* wasm, Buffer* buf, Bytecode* module, Bytecode_Function* func) {
    free_arena(&wasm->arena);
    
    pln("\nBuilding WASM for `%`...", f_var(module->function_names[func->type_index]));
    
    u32 block_depth = 0;
    
    // Local stack
    u32 register_count = func->register_count;
    wasm->stack_size = register_count*8;
    wasm->stack_offset_for_locals = wasm->stack_size;
    for_bc_insn(func, insn) {
        if (insn->opcode == BC_LOCAL) {
            Bytecode_Binary* bc = (Bytecode_Binary*) insn;
            wasm->stack_size = (s32) align_forward(wasm->stack_size, bc->b_index);
            wasm->stack_size += bc->a_index;
        }
    }
    
    wasm->slots = arena_push_array_of_structs(&wasm->arena, register_count, WASM_Slot);
    
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
        wasm_store_register(wasm, buf, formal_args[register_index].type, register_index);
        
        
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
        block_depth = convert_bytecode_insn_to_wasm(wasm, buf, module, func, insn, block_depth);
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
        Bytecode_Type type = function_ret_types(func)[0].type;
        wasm_local_get(buf, wasm_tmp_local(wasm, type));
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
    //pln("%", f_string(s));
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
convert_to_wasm_module(Bytecode* module, Data_Packer* data_packer, s64 stk_usage, Buffer* buf) {
    
    // TODO(Alexander): make configurable
    u32 stack_size = kilobytes(32); // half a WASM page
    WASM_Assembler wasm = {};
    wasm.rdata_offset = stack_size;
    wasm.data_offset = stack_size + (u32) arena_total_used(&data_packer->rdata_arena);
    u32 memory_size = (u32) (arena_total_used(&data_packer->rdata_arena) +
                             arena_total_used(&data_packer->data_arena));
    
    // Define module
    push_u32(buf, 0x6D736100); // Signature (.asm)
    push_u32(buf, 0x1); // Version
    
    WASM_Debug debug = {};
    
    // Type section (1)
    DEBUG_wasm_begin_section(&debug, WASMSection_Type, buf);
    push_u8(buf, WASMSection_Type);
    smm type_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    
    u32 num_types = (u32) array_count(module->functions);
    push_leb128_u32(buf, num_types); // number of types
    for_array_v(module->functions, func, _1) {
        
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
    push_leb128_u32(buf, (u32) array_count(module->imports)); // num imports
    
    pln("\n\n// Auto-generated code for WASM-JS interface");
    pln("const importObject = {");
    string_id last_module = 0;
    for (int i = 0; i < array_count(module->imports); i++) {
        Bytecode_Import* import = &module->imports[i];
        //pln("import %::%", f_var(import->module), f_var(import->name));
        
        if (import->kind == BC_IMPORT_FUNC) {
            Bytecode_Function* func = module->functions[import->func_index];
            
            if (last_module != import->module) {
                if (last_module != 0) pln("    },");
                pln("    %: {", f_var(import->module));
                last_module = import->module;
            }
            
            print("        %: (", f_var(import->name));
            
            for (int arg_index = 0; arg_index < func->arg_count; arg_index++) {
                print("a%", f_int(arg_index));
                if (arg_index < func->arg_count - 1) {
                    print(", ");
                }
            }
            
            print(") => %(", f_var(import->name));
            
#if 0
            if (import->module == vars_save_cstring("gl")) {
                //string name = vars_load_string(import->name);
                //char first = 'a' + (name.data[2] - 'A');
                //name = string_view(name.data + 3, name.data + name.count);
                //print(") => gl.%", f_char(first));
                //print("%(", f_string(name));
                
                
                
            } else {
                string name = vars_load_string(import->name);
                char first = 'A' + (name.data[0] - 'a');
                name = string_view(name.data + 1, name.data + name.count);
                
                print(") => %", f_var(import->module));
                print("%", f_char(first));
                print("%(", f_string(name));
            }
#endif
            
            Bytecode_Function_Arg* args = function_arg_types(func);
            for (int arg_index = 0; arg_index < func->arg_count; arg_index++) {
                if (args[arg_index].type.kind != BC_TYPE_FLOAT) {
                    print("Number(a%)", f_int(arg_index));
                } else {
                    print("a%", f_int(arg_index));
                }
                if (arg_index < func->arg_count - 1) {
                    print(", ");
                }
            }
            
            print("),\n");
        }
        
        wasm_push_string(buf, vars_load_string(module->imports[i].module));
        wasm_push_string(buf, vars_load_string(module->imports[i].name));
        push_u8(buf, 0x00); // importdesc:typeidx
        push_leb128_u32(buf, i);
    }
    if (last_module != 0) pln("    },");
    pln("};");
    wasm_set_vec_size(buf, import_section_start);
    
    // Function section (3)
    u32 function_count = (u32) (array_count(module->functions) - array_count(module->imports));
    DEBUG_wasm_begin_section(&debug, WASMSection_Function, buf);
    push_u8(buf, WASMSection_Function);
    smm function_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, function_count);
    for (int i = (int) array_count(module->imports); i < array_count(module->functions); i++) {
        push_leb128_u32(buf, i); // typeidx (NOTE: assumes funcidx = typeidx)
    }
    wasm_set_vec_size(buf, function_section_start);
    
    // Table section (4)
    DEBUG_wasm_begin_section(&debug, WASMSection_Table, buf);
    push_u8(buf, WASMSection_Table);
    smm table_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, 1); // num tables (we only need one table containing all funcrefs)
    push_u8(buf, 0x70); // 0x70 - funcref
    push_u8(buf, 0x01); // 0x01 - define min and max
    push_leb128_u32(buf, (u32) array_count(module->functions));
    push_leb128_u32(buf, (u32) array_count(module->functions));
    wasm_set_vec_size(buf, table_section_start);
    
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
    push_leb128_u32(buf, 2); // number of globals
    
    // Stack pointer
    push_u16(buf, 0x017F); // mut i32
    push_u8(buf, 0x41); // i32.const
    push_leb128_s32(buf, stack_size); // stack grows downwards
    push_u8(buf, 0x0B); // end
    
    // Heap pointer
    push_u16(buf, 0x017F); // mut i32
    push_u8(buf, 0x41); // i32.const
    push_leb128_s32(buf, stack_size + memory_size); // heap grows upwards
    push_u8(buf, 0x0B); // end
    
    wasm_set_vec_size(buf, global_section_start);
    
    // Export section (7)
    DEBUG_wasm_begin_section(&debug, WASMSection_Export, buf);
    push_u8(buf, WASMSection_Export);
    smm export_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, (u32) array_count(module->exports) + 2);
    
    // - export relevant functions
    for_array(module->exports, ex, _11) {
        wasm_push_string(buf, vars_load_string(ex->function)); // name of export
        push_u8(buf, 0x00); // 0x00 = funcidx
        push_leb128_u32(buf, ex->func_index); // funcidx of entrypoint
    }
    
    // - export table
    wasm_push_string(buf, string_lit("functions")); // name of export
    push_u8(buf, 0x01); // 0x01 = tableidx
    push_leb128_u32(buf, 0); // function table 0
    
    // - export memory
    wasm_push_string(buf, string_lit("memory")); // name of export
    push_u8(buf, 0x02); // 0x02 = memidx
    push_leb128_u32(buf, 0); // main memidx
    
    wasm_set_vec_size(buf, export_section_start);
    
    // Element section (9)
    DEBUG_wasm_begin_section(&debug, WASMSection_Element, buf);
    push_u8(buf, WASMSection_Element);
    smm element_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, 1); // num tables
    push_leb128_u32(buf, 0); // 0 - elem option 0 (mode active)
    wasm_i32_const(buf, 0); // table0 + offset 0
    push_u8(buf, 0x0B); // end
    push_leb128_u32(buf, (u32) array_count(module->functions)); // num funcrefs
    for (int i = 0; i < array_count(module->functions); i++) {
        push_leb128_u32(buf, i); // funcidx
    }
    wasm_set_vec_size(buf, element_section_start);
    
    // Code section (10)
    DEBUG_wasm_begin_section(&debug, WASMSection_Code, buf);
    push_u8(buf, WASMSection_Code);
    smm code_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, function_count);
    
    for_array_v(module->functions, func, _2) {
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
        push_u8(buf, 0x7F);
        int num_locals = func->arg_count;
        wasm.tmp_local_i32 = num_locals;
        
        push_leb128_u32(buf, 1); 
        push_u8(buf, 0x7E);
        wasm.tmp_local_i64 = num_locals + 1;
        
        push_leb128_u32(buf, 1); 
        push_u8(buf, 0x7D);
        wasm.tmp_local_f32 = num_locals + 2;
        
        push_leb128_u32(buf, 1); 
        push_u8(buf, 0x7C);
        wasm.tmp_local_f64 = num_locals + 3;
        
        convert_bytecode_function_to_wasm(&wasm, buf, module, func);
        
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
    wasm_i32_const(buf, wasm.rdata_offset);
    push_u8(buf, 0x0B); // end
    
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

u8*
convert_wasm_insn_to_text(String_Builder* sb, Bytecode* module, u8* begin, u8* curr, u8* end, int block_depth) {
    string_builder_push_format(sb, "%: ", f_u64_HEX(curr - begin));
    for (int i = 0; i < block_depth; i++) string_builder_push(sb, "  ");
    u8 opcode = *curr++;
    switch (opcode) {
        default: string_builder_push_format(sb, "(%) ???", f_u64_HEX(opcode)); break;
        
        case 0x02: string_builder_push_format(sb, "block bt=%", f_u64_HEX(*curr++)); break;
        case 0x03: string_builder_push_format(sb, "loop bt=%", f_u64_HEX(*curr++)); break;
        case 0x0B: string_builder_push(sb, "end"); break;
        case 0x0C: string_builder_push_format(sb, "br %", f_s64(decode_leb128_u32(&curr, end))); break;
        case 0x0D: string_builder_push_format(sb, "br_if %", f_s64(decode_leb128_u32(&curr, end))); break;
        case 0x0F: string_builder_push(sb, "return"); break;
        case 0x10: string_builder_push_format(sb, "call %", f_var(module->function_names[decode_leb128_u32(&curr, end)])); break;
        
        case 0x41: string_builder_push_format(sb, "i32.const %", f_s64(decode_leb128_s64(&curr, end))); break;
        case 0x42: string_builder_push_format(sb, "i64.const %", f_s64(decode_leb128_s64(&curr, end))); break;
        
        case 0x6A: string_builder_push(sb, "i32.add"); break;
        case 0x6B: string_builder_push(sb, "i32.sub"); break;
        
        case 0xFC: {
            if (*curr++ == 0x0B) {
                curr++;
                string_builder_push(sb, "memory.fill");
            } else {
                curr += 2;
                string_builder_push(sb, "memory.copy");
            }
        } break;
        
#define WASM_CTL(mnemonic, opcode) \
case opcode: string_builder_push(sb, mnemonic); curr++; break;
        
#define WASM_BIN(mnemonic, opcode) \
case opcode: string_builder_push(sb, mnemonic); break;
        
#define WASM_VAR(mnemonic, opcode) \
case opcode: string_builder_push_format(sb, mnemonic " %", f_uint(decode_leb128_u32(&curr, end))); break;
        
#define WASM_MEM(mnemonic, opcode) \
case opcode: string_builder_push_format(sb, mnemonic " offset=%, align=%", \
f_uint(decode_leb128_u32(&curr, end)), \
f_uint(pow(2, decode_leb128_u32(&curr, end)))); break;
        
        
        DEF_WASM_INSTRUCTIONS
    }
    string_builder_push(sb, "\n");
    
    return curr;
}

void
convert_wasm_code_to_text(String_Builder* sb, Bytecode* module, u8* begin, u8* curr, u8* end, u8* typeidx) {
    
    u32 func_count = decode_leb128_u32(&curr, end);
    
    for (u32 func_index = 0; func_index < func_count; func_index++) {
        u32 func_size = decode_leb128_u32(&curr, end);
        u8* func_end = curr + func_size;
        u32 num_locals = decode_leb128_u32(&curr, end);
        for (u32 i = 0; i < num_locals; i++) {
            decode_leb128_u32(&curr, end);
            curr++;
        }
        
        
        u32 type_index = decode_leb128_u32(&typeidx, end);
        string_builder_push_format(sb, "\n%:\n", f_var(module->function_names[type_index]));
        
        int block_depth = 0;
        while (curr < func_end) {
            u8 opcode = *curr;
            if (opcode == 0x0B) block_depth--;
            
            curr = convert_wasm_insn_to_text(sb, module, begin, curr, func_end, block_depth);
            
            if (opcode == 0x02 || opcode == 0x03) block_depth++;
        }
    }
}

void
convert_wasm_binary_to_text(String_Builder* sb, Bytecode* module, u8* begin, u8* end) {
    u8* curr = begin;
    
    u32 magic_number = *((u32*) curr); curr += 4;
    u32 version      = *((u32*) curr); curr += 4;
    
    if (magic_number != WASM_MAGIC_NUMBER) {
        pln("error: invalid wasm magic number");
        return;
    }
    
    if (version != WASM_VERSION) {
        pln("error: unsupported version of wasm got %, expected 0x1", f_u64_HEX(version));
        return;
    }
    
    u8* typeidx = 0;
    
    while (curr < end) {
        u8 section = *curr++;
        int section_size = decode_leb128_u32(&curr, end);
        if (section == WASMSection_Function) {
            typeidx = curr;
            decode_leb128_u32(&typeidx, typeidx + section_size);
            
        } else if (section == WASMSection_Code) {
            convert_wasm_code_to_text(sb, module, begin, curr, curr + section_size, typeidx);
        }
        
        curr += section_size;
        //pln("section size: %", f_int(section_size));
    }
}
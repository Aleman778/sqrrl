
struct X64_Compiled_Code {
    u8* main_function_ptr;
    PE_Executable pe_executable;
};

X64_Compiled_Code
convert_bytecode_to_x64_machine_code(Bytecode* bytecode, Buffer* buf, 
                                     Data_Packer* data_packer,
                                     Library_Import_Table* import_table,
                                     Compiler_Task compiler_task) {
    
    X64_Compiled_Code result;
    
    X64_Assembler x64 = {};
    x64.bytecode = bytecode;
    x64.data_packer = data_packer;
    x64.use_absolute_ptrs = compiler_task == CompilerTask_Run;
    // TODO(Alexander): employ better allocation strategy!
    x64.functions = (X64_Function*) calloc(array_count(bytecode->functions), 
                                           sizeof(X64_Function));
    x64.global_code_ptrs = (u8**) calloc(array_count(bytecode->globals), sizeof(u8*));
    
    result.main_function_ptr = buf->data;
    for_array_v(bytecode->functions, func, func_index) {
        if (compiler_task == CompilerTask_Build) {
            if (func_index < array_count(bytecode->imports)) {
                s64 jump_src = bytecode->imports[func_index].rdata_offset;
                
                // Indirect jump to library function
                push_u8(buf, 0xFF); // FF /4 	JMP r/m64 	M
                x64_rip_relative(buf, 4, jump_src);
            }
        }
        
        if (bytecode->entry_func_index == func_index) {
            result.main_function_ptr = buf->data + buf->curr_used;
        }
        
        convert_bytecode_function_to_x64_machine_code(&x64, func, buf);
    }
    
    // Patch relative jump addresses
    for_array_v(x64.jump_patches, patch, _pi) {
        s32 rel32 = (s32) (*patch.target - (patch.origin + 4));
        //pln("% - % = %", f_u64_HEX(*patch.target), f_u64_HEX(patch.origin + 4), f_int(rel32));
        *((s32*) patch.origin) = rel32;
    }
    
    Memory_Arena build_arena = {};
    if (compiler_task == CompilerTask_Build) {
        // NOTE(Alexander): Build initial PE executable
        result.pe_executable = convert_to_pe_executable(&build_arena,
                                                        buf->data, (u32) buf->curr_used,
                                                        import_table,
                                                        data_packer,
                                                        result.main_function_ptr);
        PE_Executable* pe = &result.pe_executable;
        s64 base_address = pe->text_section->virtual_address;
        if (pe->rdata_section) {
            x64.read_only_data_offset = (s64) buf->data + (pe->rdata_section->virtual_address - 
                                                           base_address);
        }
        if (pe->data_section) {
            x64.read_write_data_offset = (s64) buf->data + (pe->data_section->virtual_address - 
                                                            base_address);
        }
        
        
        // Patch relative data addresses
        for_array_v(x64.data_patches, patch, _pi1) {
            s64 section_offset = (s64) patch.data.relative_ptr - (s64) (patch.origin + 4);
            switch (patch.data.section) {
                case Read_Data_Section: {
                    *((u32*) patch.origin) = (u32) (x64.read_only_data_offset + section_offset);
                } break;
                
                case Data_Section: {
                    *((u32*) patch.origin) = (u32) (x64.read_write_data_offset + section_offset);
                } break;
                
                default: unimplemented;
            }
        }
        
        // Patch global variable addresses
        for (int global_index = 0; global_index < array_count(bytecode->globals); global_index++) {
            Bytecode_Global* g = &bytecode->globals[global_index];
            u8* origin = x64.global_code_ptrs[global_index];
            
            s64 section_offset = (s64) g->offset - (s64) (origin + 4);
            switch (g->kind) {
                case BC_MEM_READ_ONLY: {
                    *((u32*) origin) = (u32) (x64.read_only_data_offset + section_offset);
                } break;
                
                case BC_MEM_READ_WRITE: {
                    *((u32*) origin) = (u32) (x64.read_write_data_offset + section_offset);
                } break;
                
                default: unimplemented;
            }
        }
    }
    
    
    return result;
}

void
convert_bytecode_function_to_x64_machine_code(X64_Assembler* x64, Bytecode_Function* func, Buffer* buf) {
    assert(x64->bytecode && x64->functions && x64->data_packer && "X64 assembler is not setup correctly");
    
    if (func->is_imported || func->is_intrinsic) {
        return;
    }
    
    Bytecode_Function_Arg* formal_args = function_arg_types(func);
    
    // TODO(Alexander): most  of the code here is windows x64 calling convention specific
    
    // Setup labels
    x64->curr_function = &x64->functions[func->type_index];
    x64->curr_function->code = buf->data + buf->curr_used;
    if (!x64->curr_function->labels) {
        x64->curr_function->labels = (u8**) calloc(func->block_count + 1, sizeof(u8*));
    }
    x64->label_index = 1; // NOTE(Alexander): treat label 0 as end of function
    
    // Windows calling convention stack setup
    s32 register_count = (s32) func->register_count;
    s32 stack_caller_args = func->max_caller_arg_count*8;
    x64->current_stack_displacement_for_bytecode_registers = stack_caller_args;
    
    s32 stack_size = stack_caller_args + register_count*8;
    
    // Push larger structures in our stack frame
    s32 stack_callee_args_copy = stack_size;
    for (int arg_index = 0; arg_index < (int) func->arg_count; arg_index++) {
        Bytecode_Type arg_type = formal_args[arg_index].type;
        // TODO(Alexander): we need a better way to know when to copy a pointers data!!!
        if (arg_type.kind == BC_TYPE_PTR) {
            stack_size += arg_type.size;
        }
    }
    
    x64->current_stack_displacement_for_locals = stack_size;
    for_bc_insn(func, insn) {
        if (insn->opcode == BC_LOCAL) {
            Bytecode_Binary* bc_insn = (Bytecode_Binary*) insn;
            stack_size = (s32) align_forward(stack_size, bc_insn->arg1_index);
            stack_size += bc_insn->arg0_index;
        }
    }
    
    // Align stack by 16-bytes (excluding 8 bytes for return address)
    stack_size = (s32) align_forward(stack_size + 8, 16) - 8;
    s32 stack_callee_args = stack_size + 8;
    
    
    // Save HOME registers (TODO: move this it's windows calling convention only)
    // TODO(Alexander): this doens't handle returns larger than 8 bytes
    for (int i = min((int) func->arg_count - 1, 4); i >= 0; i--) {
        X64_Reg dest = int_arg_registers_ccall_windows[i];
        x64_move_register_to_memory(buf, X64_RSP, i*8 + 8, dest);
    }
    
    // Prologue
    x64_rex(buf, REX_W); // sub rsp, stack_size
    push_u8(buf, 0x81);
    x64_modrm_direct(buf, 5, X64_RSP);
    push_u32(buf, stack_size);
    
    // Setup local stack (excluding arguments and return values)
    
    // TODO: implement this!!!
    //if (func->ret_count == 1 && formal_args[func->arg_count].size > 8) {
    //stack_caller_args += 8;
    //x64->stack[func->arg_count] = stack_caller_args;
    //}
    
    
    // Copy registers to our local stack space
    // TODO(Alexander): we should find a way to reference the old stack frame!!!
    for (int arg_index = 0; arg_index < (int) func->arg_count; arg_index++) {
        s64 src = stack_callee_args + arg_index*8;
        s64 dest = register_displacement(x64, arg_index);
        x64_move_memory_to_register(buf, X64_RAX, X64_RSP, src);
        x64_move_register_to_memory(buf, X64_RSP, dest, X64_RAX);
    }
    
    for_bc_insn(func, curr) {
        convert_bytecode_insn_to_x64_machine_code(x64, buf, func, curr);
    }
    
    // Epilogue
    x64->curr_function->labels[0] = buf->data + buf->curr_used;
    x64_rex(buf, REX_W); // add rsp, stack_usage
    push_u8(buf, 0x81);
    x64_modrm_direct(buf, 0, X64_RSP);
    push_u32(buf, stack_size);
    push_u8(buf, 0xC3); // RET near
}

void 
convert_bytecode_insn_to_x64_machine_code(X64_Assembler* x64, Buffer* buf, 
                                          Bytecode_Function* func, 
                                          Bytecode_Instruction* insn) {
    s64 rip = 0;
    Bytecode_Binary* bc = (Bytecode_Binary*) insn;
    
    switch (bc->opcode) {
        case BC_DEBUG_BREAK: {
            push_u8(buf, 0xCC);
        } break;
        
        case BC_INT_CONST: {
            s64 immediate = bc->const_i64;
            if (immediate < S32_MIN || immediate > S32_MAX) {
                x64_move_rax_u64(buf, immediate);
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
            } else {
                // REX.W + C7 /0 id 	MOV r/m64, imm32 	MI
                push_u8(buf, 0xC7);
                s64 displacement = register_displacement(x64, bc->res_index);
                x64_modrm(buf, 0, X64_RSP, displacement, 0);
                push_u32(buf, (u32) immediate);
            }
        } break;
        
        case BC_F32_CONST: {
            Exported_Data f32_data = export_struct(x64->data_packer, f32, Read_Data_Section);
            *((f32*) f32_data.data) = bc->const_f32;
            x64_move_const_to_float_register(x64, buf, X64_XMM5, f32_data, 4);
            x64_move_float_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), 
                                              X64_XMM5, sizeof(f32));
        } break;
        
        case BC_F64_CONST: {
            Exported_Data f64_data = export_struct(x64->data_packer, f64, Read_Data_Section);
            *((f64*) f64_data.data) = bc->const_f64;
            x64_move_const_to_float_register(x64, buf, X64_XMM5, f64_data, 8);
            x64_move_float_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), 
                                              X64_XMM5, sizeof(f64));
        } break;
        
        case BC_LOCAL: {
            s64 disp = x64->current_stack_displacement_for_locals;
            disp = (s64) align_forward(disp, bc->arg1_index);
            x64->current_stack_displacement_for_locals = disp + bc->arg0_index;
            x64_lea(buf, X64_RAX, X64_RSP, disp);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_GLOBAL: {
            // REX.W + 8D /r 	LEA r64,m 	RM
            x64_rex(buf, REX_W);
            push_u8(buf, 0x8D);
            x64_modrm_global_data(x64, buf, X64_RAX, bc->arg0_index);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_STRUCT_FIELD: {
            x64_move_memory_to_register(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index));
            x64_add64_immediate(buf, X64_RAX, bc->arg1_index);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_ARRAY_INDEX: {
            // TODO(Alexander): HACK we should utilize SIB somehow!
            Bytecode_Type type = register_type(func, bc->arg0_index);
            x64_move_memory_to_register(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index));
            x64_add64_immediate(buf, X64_RAX, bc->arg1_index * type.size);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_STORE: {
            // TODO(Alexander): this assumes that size of data in memory is at least 8 bytes,
            // if we make it less then we will overwrite data outside our scope.
            x64_move_memory_to_register(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index));
            x64_move_memory_to_register(buf, X64_RCX, X64_RSP, register_displacement(x64, bc->arg1_index));
            
            Bytecode_Type type = register_type(func, bc->arg1_index);
            switch (type.size) {
                case 1: {
                    x64_move8_register_to_memory(buf, X64_RAX, 0, X64_RCX);
                } break;
                
                case 2: {
                    x64_move16_register_to_memory(buf, X64_RAX, 0, X64_RCX);
                } break;
                
                case 4: {
                    x64_move32_register_to_memory(buf, X64_RAX, 0, X64_RCX);
                } break;
                
                default: {
                    x64_move_register_to_memory(buf, X64_RAX, 0, X64_RCX);
                } break;
            }
        } break;
        
        case BC_LOAD: {
            x64_move_memory_to_register(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index));
            x64_move_memory_to_register(buf, X64_RAX, X64_RAX, 0);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            Bytecode_Function* target = x64->bytecode->functions[call->func_index];
            Bytecode_Function_Arg* formal_args = function_arg_types(func);
            
            int* args = bc_call_args(call);
            for (int arg_index = target->arg_count - 1; arg_index >= 0; arg_index--) {
                int src_index = args[arg_index];
                X64_Reg dest = X64_RAX;
                if (arg_index < 4) {
                    dest = int_arg_registers_ccall_windows[arg_index];
                }
                
                x64_move_memory_to_register(buf, dest, X64_RSP,
                                            register_displacement(x64, src_index));
                if (arg_index >= 4) {
                    x64_move_register_to_memory(buf, X64_RSP, arg_index*8, dest);
                }
            }
            
            
            u32 return_virtual_register = 0;
            if (target->ret_count > 0) {
                return_virtual_register = args[target->arg_count];
            }
            
            // NOTE(Alexander): make sure to allocate space for HOME registers even if they aren't used!
            if (target->is_imported) {
                
                // Indirect function call from pointer
                //x64_spill_register(x64, buf, X64_RAX);
                x64_move_rax_u64(buf, (u64) target->code_ptr);
                
                // FF /2 	CALL r/m64 	M 	
                push_u8(buf, 0xFF);
                x64_modrm_direct(buf, 2, X64_RAX);
            } else {
                
                // Direct function call
                // E8 cd 	CALL rel32 	D
                push_u8(buf, 0xE8);
                x64_jump_address(x64, buf, &x64->functions[call->func_index].code);
            }
            
            if (target->ret_count > 0) {
                // TODO(Alexander): Add support for floats!
                x64_move_register_to_memory(buf, X64_RSP,
                                            register_displacement(x64, return_virtual_register),
                                            X64_RAX);
                
                //x64_alloc_register(x64, buf, return_virtual_register, X64_RAX, return_rt);
            }
        } break;
        
        case BC_RETURN: {
            int res_index = bc->arg0_index;
            if (res_index > 0) {
                Bytecode_Type type = register_type(func, res_index);
                x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, res_index), 
                                type.size, type.flags & BC_FLAG_SIGNED);
            }
            
            if (insn->next_insn) {
                // jump to end of function
                push_u8(buf, 0xE9);
                x64_jump_address_for_label(x64, buf, func, 0);
            }
        } break;
        
        // TODO(Alexander): with type flags we don't need separate instructions for U8. S8 etc.
        case BC_EXTEND: {
            Bytecode_Type type = register_type(func, bc->arg0_index);
            x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index),
                            type.size, type.flags & BC_FLAG_SIGNED);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_FLOAT_TO_INT: {
            Bytecode_Type type = register_type(func, bc->arg0_index);
            x64_move_memory_to_float_register(buf, X64_XMM0, X64_RSP, register_displacement(x64, bc->arg0_index), type.size);
            x64_convert_float_to_int(buf, X64_RAX, X64_XMM0, type.size);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_INC:
        case BC_DEC: {
            Bytecode_Type type = register_type(func, bc->arg0_index);
            assert(type.kind != BC_TYPE_FLOAT && "inc is not implemented for float types");
            bool is_signed = type.flags & BC_FLAG_SIGNED;
            
            x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index), type.size, is_signed);
            if (bc->opcode == BC_INC) {
                x64_inc(buf, X64_RAX);
            } else {
                x64_dec(buf, X64_RAX);
            }
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_NEG: {
            Bytecode_Type type = register_type(func, bc->arg0_index);
            if (type.kind & BC_TYPE_FLOAT) {
                // XOR the sign float bit
                Exported_Data sign_mask = export_size(x64->data_packer, Read_Data_Section, 
                                                      type.size, type.size);
                if (type.size == 8) {
                    *((u64*) sign_mask.data) = 0x8000000000000000ull;
                } else {
                    *((u32*) sign_mask.data) = 0x80000000;
                }
                
                x64_move_memory_to_float_register(buf, X64_XMM4, X64_RSP,
                                                  register_displacement(x64, bc->arg0_index), type.size);
                x64_move_const_to_float_register(x64, buf, X64_XMM5, sign_mask, type.size);
                x64_xorps(buf, X64_XMM4, X64_XMM5, type.size);
                x64_move_float_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_XMM4, type.size);
            } else {
                x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index), type.size, true);
                x64_neg(buf, X64_RAX);
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
            }
        } break;
        
        case BC_NOT: {
            Bytecode_Type type = register_type(func, bc->arg0_index);
            x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index), type.size, true);
            x64_not(buf, X64_RAX);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_ADD:
        case BC_SUB:
        case BC_MUL:
        case BC_DIV_S:
        case BC_DIV_U: {
            Bytecode_Type type = register_type(func, bc->arg0_index);
            if (type.kind == BC_TYPE_FLOAT) {
                x64_move_memory_to_float_register(buf, X64_XMM4, X64_RSP, register_displacement(x64, bc->arg0_index), type.size);
                x64_move_memory_to_float_register(buf, X64_XMM5, X64_RSP, register_displacement(x64, bc->arg1_index), type.size);
                
                switch (bc->opcode) {
                    case BC_ADD: x64_addss(buf, X64_XMM4, X64_XMM5, type.size); break; 
                    case BC_SUB: x64_subss(buf, X64_XMM4, X64_XMM5, type.size); break; 
                    case BC_MUL: x64_mulss(buf, X64_XMM4, X64_XMM5, type.size); break; 
                    case BC_DIV_S: // TODO(Alexander): we should only have DIV!!!
                    case BC_DIV_U: x64_divss(buf, X64_XMM4, X64_XMM5, type.size); break;
                    
                    default: unimplemented;
                }
                x64_move_float_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index),
                                                  X64_XMM4, type.size);
                
            } else {
                bool is_signed = type.flags & BC_FLAG_SIGNED;
                // TODO(Alexander): size and signed flags need to be set here!
                x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index), type.size, is_signed);
                x64_move_extend(buf, X64_RCX, X64_RSP, register_displacement(x64, bc->arg1_index), type.size, is_signed);
                
                switch (bc->opcode) {
                    case BC_ADD: x64_add64(buf, X64_RAX, X64_RCX); break; 
                    case BC_SUB: x64_sub64(buf, X64_RAX, X64_RCX); break; 
                    case BC_MUL: x64_mul64(buf, X64_RAX, X64_RCX); break; 
                    case BC_DIV_S: x64_div64(buf, X64_RAX, X64_RCX, true); break; // TODO(Alexander): we might use flags here type flags instead of DIV_S/ DIV_U?
                    case BC_DIV_U: x64_div64(buf, X64_RAX, X64_RCX, false); break; 
                    
                    default: unimplemented;
                }
                
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
            }
        } break;
        
        case BC_EQ:
        case BC_NEQ:
        case BC_GT_S:
        case BC_GT_U:
        case BC_GE_S:
        case BC_GE_U:
        case BC_LT_S:
        case BC_LT_U:
        case BC_LE_S:
        case BC_LE_U: {
            Bytecode_Type type = register_type(func, bc->arg0_index);
            bool is_signed = type.flags & BC_FLAG_SIGNED;
            x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index),
                            type.size, is_signed);
            x64_move_extend(buf, X64_RCX, X64_RSP, register_displacement(x64, bc->arg1_index),
                            type.size, is_signed);
            x64_cmp64(buf, X64_RAX, X64_RCX);
            
            Bytecode_Branch* branch = (Bytecode_Branch*) ((u8*) insn + insn->next_insn);
            if (branch->opcode == BC_BRANCH) {
                // jcc
                push_u16(buf, x64_jcc_opcodes[BC_NEQ - insn->opcode]);
                x64_jump_address_for_label(x64, buf, func, branch->label_index);
            } else {
                // setcc
                push_u24(buf, x64_setcc_opcodes[insn->opcode - BC_EQ]); 
            }
        } break;
        
        case BC_BRANCH: {
            // NOTE(Alexander): conditional branch is handled by operations above
            Bytecode_Branch* branch = (Bytecode_Branch*) insn;
            if (branch->cond < 0) {
                push_u8(buf, 0xE9);
                x64_jump_address_for_label(x64, buf, func, branch->label_index);
            }
        } break;
        
        case BC_LOOP:
        case BC_BLOCK: {
            X64_Block block = {};
            block.label_index = x64->label_index++;
            block.is_loop = bc->opcode == BC_LOOP;
            array_push(x64->block_stack, block);
            
            if (block.is_loop) {
                x64->curr_function->labels[block.label_index] = buf->data + buf->curr_used;
            }
        } break;
        
        case BC_END: {
            X64_Block block = array_pop(x64->block_stack);
            if (!block.is_loop) {
                x64->curr_function->labels[block.label_index] = buf->data + buf->curr_used;
            }
        } break;
        
        case BC_MEMCPY: {
            x64_move_memory_to_register(buf, X64_RDI, X64_RSP, register_displacement(x64, bc->res_index));
            x64_move_memory_to_register(buf, X64_RSI, X64_RSP, register_displacement(x64, bc->arg0_index));
            x64_move_immediate_to_register(buf, X64_RCX, bc->arg1_index);
            x64_rep_movsb(buf, X64_RDI, X64_RSI, X64_RCX);
        } break;
        
        case BC_X64_RDTSC: {
            //Ic_Raw_Type rt = convert_bytecode_type_to_x64(insn->type);
            //x64_alloc_register(x64, buf, dest.register_index, X64_RAX, rt);
            //x64_spill_register(x64, buf, X64_RDX);
            
            push_u16(buf, 0x310F); // rdtsc edx:eax
            
            // REX.W + C1 /4 ib  -> shl rdx, 32
            x64_rex(buf, REX_W);
            push_u8(buf, 0xC1);
            x64_modrm_direct(buf, 4, X64_RDX);
            push_u8(buf, 32);
            
            // REX.W + 09 /r  -> or rax, rdx
            x64_rex(buf, REX_W);
            push_u8(buf, 0x09);
            x64_modrm_direct(buf, X64_RDX, X64_RAX);
        } break;
        
        default: {
            assert(0 && "invalid bytecode instruction");
        } break;
    }
}

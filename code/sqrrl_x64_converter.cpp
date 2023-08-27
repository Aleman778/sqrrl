
u8*
convert_bytecode_to_x64_machine_code(Bytecode* bc, Buffer* buf, 
                                     Data_Packer* data_packer,
                                     Library_Import_Table* import_table,
                                     Compiler_Task compiler_task) {
    
    X64_Assembler x64 = {};
    x64.bytecode = bc;
    x64.data_packer = data_packer;
    x64.use_absolute_ptrs = compiler_task == CompilerTask_Run;
    x64.functions = (X64_Function*) calloc(array_count(bc->functions), 
                                           sizeof(X64_Function));
    
    u8* asm_buffer_main = buf->data;
    for_array_v(bc->functions, func, func_index) {
        if (compiler_task == CompilerTask_Build) {
            if (func_index < array_count(bc->imports)) {
                s64 jump_src = bc->imports[func_index].rdata_offset;
                
                // Indirect jump to library function
                push_u8(buf, 0xFF); // FF /4 	JMP r/m64 	M
                x64_rip_relative(buf, 4, jump_src);
            }
        }
        
        if (bc->entry_func_index == func_index) {
            asm_buffer_main = buf->data + buf->curr_used;
        }
        
        convert_bytecode_function_to_x64_machine_code(&x64, func, buf);
    }
    
    Memory_Arena build_arena = {};
    PE_Executable pe;
    if (compiler_task == CompilerTask_Build) {
        // NOTE(Alexander): Build initial PE executable
        pe = convert_to_pe_executable(&build_arena,
                                      buf->data, (u32) buf->curr_used,
                                      import_table,
                                      data_packer,
                                      asm_buffer_main);
        s64 base_address = pe.text_section->virtual_address;
        if (pe.rdata_section) {
            x64.read_only_data_offset = (s64) buf->data + (pe.rdata_section->virtual_address - 
                                                           base_address);
        }
        if (pe.data_section) {
            x64.read_write_data_offset = (s64) buf->data + (pe.data_section->virtual_address - 
                                                            base_address);
        }
    }
    
    if (compiler_task == CompilerTask_Build) {
        // NOTE(Alexander): write the PE executable to file
        File_Handle exe_file = DEBUG_open_file_for_writing("simple.exe");
        write_pe_executable_to_file(exe_file, &pe);
        DEBUG_close_file(exe_file);
        pln("\nWrote executable: simple.exe");
        
        //Read_File_Result exe_data = DEBUG_read_entire_file("simple.exe");
        //pe_dump_executable(create_string(exe_data.contents_size, (u8*) exe_data.contents));
    }
    
    return asm_buffer_main;
}


Ic_Raw_Type
convert_bytecode_type_to_x64(Bytecode_Type type) {
    switch (type) {
        case BytecodeType_i32: return IC_S32;
        case BytecodeType_i64: return IC_S64;
        case BytecodeType_f32: return IC_F32;
        case BytecodeType_f64: return IC_F64;
    }
    return 0;
}

Ic_Raw_Type
convert_bytecode_type_to_x64(Bytecode_Type type, int size) {
    switch (type) {
        case BytecodeType_i32: 
        case BytecodeType_i64: {
            switch (size) {
                case 1: return IC_S8;
                case 2: return IC_S16;
                case 4: return IC_S32;
                default: return IC_S64;
            }
        } break;
        
        case BytecodeType_f32: 
        case BytecodeType_f64: {
            switch (size) {
                case 4: return IC_F32;
                case 8: return IC_F64;
                default: unimplemented;
            }
        } break;
    }
    return 0;
}

Ic_Arg
convert_bytecode_operand_to_x64(X64_Assembler* x64, Buffer* buf, Bytecode_Operand op, Bytecode_Type type) {
    Ic_Raw_Type rt = convert_bytecode_type_to_x64(type);
    Ic_Arg result = {};
    switch (op.kind) {
        case BytecodeOperand_const: {
            
            switch (type) {
                case BytecodeType_i32: {
                    result.type = IC_DISP | rt;
                    result.disp = op.const_i32; 
                } break;
                
                case BytecodeType_i64: {
                    result.type = IC_DISP | rt;
                    result.disp = op.const_i64; 
                } break;
                
                case BytecodeType_f32: {
                    void* data = arena_push_struct(&x64->data_packer->rdata_arena, f32);
                    u32 relative_ptr = (u32) arena_relative_pointer(&x64->data_packer->rdata_arena, data);
                    *((f32*) data) = op.const_f32;
                    result.type = IC_RIP_DISP32 | rt;
                    if (x64->use_absolute_ptrs) {
                        result.disp = (s64) data;
                    } else {
                        result.data.disp = relative_ptr;
                        result.data.area = IcDataArea_Read_Only;
                    }
                } break;
                
                case BytecodeType_f64: {
                    void* data = arena_push_struct(&x64->data_packer->rdata_arena, f64);
                    u32 relative_ptr = (u32) arena_relative_pointer(&x64->data_packer->rdata_arena, data);
                    *((f64*) data) = op.const_f64;
                    result.type = IC_RIP_DISP32 | rt;
                    if (x64->use_absolute_ptrs) {
                        result.disp = (s64) data;
                    } else {
                        result.data.disp = relative_ptr;
                        result.data.area = IcDataArea_Read_Only;
                    }
                } break;
            }
        } break;
        
        case BytecodeOperand_register: {
            unimplemented;
            //result = x64->virtual_registers[op.register_index];
            
            if (!result.type) {
                // TODO(Alexander): only allocaing RAX for now!
                X64_Reg reg = (rt & IC_FLOAT) ? X64_XMM0 : X64_RAX;
                //result = x64_alloc_register(x64, buf, op.register_index, reg, rt);
            }
            assert(result.type && "register not allocated");
        } break;
        
        case BytecodeOperand_memory: {
            result.type = IC_RIP_DISP32 | rt;
            result.reg = X64_RIP;
            
            if (x64->use_absolute_ptrs) {
                result.disp = (s64) op.memory_absolute;
            } else {
                switch (op.memory_kind) {
                    case BytecodeMemory_read_write: {
                        result.disp = (s64) x64->read_write_data_offset + op.memory_offset;
                    } break;
                    
                    case BytecodeMemory_read_only: {
                        result.disp = (s64) x64->read_only_data_offset + op.memory_offset;
                    } break;
                }
            }
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return result;
}

void
convert_bytecode_function_to_x64_machine_code(X64_Assembler* x64, Bytecode_Function* func, Buffer* buf) {
    assert(x64->bytecode && x64->functions && x64->data_packer && "X64 assembler is not setup correctly");
    
    if (func->is_imported || func->is_intrinsic) {
        return;
    }
    
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
    x64->stack_displacement_for_caller_arguments = 0;
    
    // Align stack by 16-bytes (excluding 8 bytes for return address)
    s32 stack_size = stack_caller_args + register_count*8 + 8;
    stack_size = (s32) align_forward(stack_size, 16) - 8;
    s32 stack_callee_args = stack_size + 8;
    
    Bytecode_Function_Arg* formal_args = (Bytecode_Function_Arg*) (func + 1);
    
    // Save HOME registers (TODO: move this it's windows calling convention only)
    // TODO(Alexander): this doens't handle returns larger than 8 bytes
    for (int i = (int) min(func->arg_count - 1, 4); i >= 0; i--) {
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
    
    // Copy registers to our local stack space (TODO: move this it's windows x64 calling convention)
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
    
    const int flag_floating = bit(31);
    int opcode = insn->opcode;
    if (insn->type == BytecodeType_f32 ||
        insn->type == BytecodeType_f64) {
        opcode |= flag_floating;
    }
    
    Bytecode_Binary* bc = (Bytecode_Binary*) insn;
    
    switch (opcode) {
        case BC_DEBUG_BREAK: {
            push_u8(buf, 0xCC);
        } break;
        
        case BC_ALLOCA: {
            // noop! All registers are stack based
        } break;
        
        case BC_CONST: {
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
        
        case BC_STORE:
        case BC_STORE_8: {
            // TODO(Alexander): this assumes that size of data in memory is at least 8 bytes,
            // if we make it less then we will overwrite data outside our scope.
            x64_move_memory_to_register(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg1_index));
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->arg0_index), X64_RAX);
        } break;
        
        case BC_CALL | flag_floating:
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            Bytecode_Operand* args = (Bytecode_Operand*) (call + 1);
            Bytecode_Function* target = x64->bytecode->functions[call->func_index];
            Bytecode_Function_Arg* formal_args = (Bytecode_Function_Arg*) (func + 1);
            
            int local_arg_stack_usage = 0;
            int first_arg_index = 0;
            if (target->ret_count == 1 && formal_args[target->arg_count].size > 8) {
                first_arg_index = 1;
                local_arg_stack_usage += 8;
            }
            
            // TODO(Alexander): this is part of windows calling convention, move this
            for (int i = (int) target->arg_count - 1; i >= 0; i--) {
                int src_index = args[i].register_index;
                int arg_index = first_arg_index + i;
                X64_Reg dest = X64_RAX;
                if (arg_index < 4) {
                    dest = int_arg_registers_ccall_windows[arg_index];
                }
                
                x64_move_memory_to_register(buf, dest, X64_RSP,
                                            register_displacement(x64, src_index));
                if (arg_index >= 4) {
                    x64_move_register_to_memory(buf, X64_RSP, i*8, dest);
                }
            }
            local_arg_stack_usage += target->arg_count*8;
            
            
            Ic_Raw_Type return_rt = 0;
            u32 return_virtual_register = 0;
            if (target->ret_count > 0) {
                return_rt = convert_bytecode_type_to_x64(insn->type);
                return_virtual_register = args[target->arg_count].register_index;
            }
            
#if 0
            if (first_arg_index == 1) {
                // Create temporary space for storing the return value of the function
                Bytecode_Function_Arg arg = formal_args[target->arg_count];
                local_arg_stack_usage = (int) align_forward(local_arg_stack_usage, arg.align);
                
                Ic_Arg result = ic_stk(IC_T64, local_arg_stack_usage);
                
                X64_Reg reg = int_arg_registers_ccall_windows[0];
                x64_spill_register(x64, buf, reg);
                x64_lea(buf, reg, result.reg, result.disp, rip);
                
                local_arg_stack_usage += arg.size;
                
                if (target->ret_count) {
                    x64_spill_register(x64, buf, X64_RAX);
                    //x64->virtual_registers[return_virtual_register] = result;
                }
            }
#endif
            
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
                push_u32(buf, (u32) (x64->functions[call->func_index].code - 
                                     (buf->data + buf->curr_used + 4)));
                
            }
            
            if (first_arg_index == 0 && target->ret_count > 0) {
                x64_move_register_to_memory(buf, X64_RSP,
                                            register_displacement(x64, return_virtual_register),
                                            X64_RAX);
                
                //x64_alloc_register(x64, buf, return_virtual_register, X64_RAX, return_rt);
            }
        } break;
        
        case BC_RETURN: {
            Bytecode_Function_Arg* formal_args = (Bytecode_Function_Arg*) (func + 1);
            
            int res_index = bc_unary_first(insn).register_index;
            
            Bytecode_Function_Arg arg = formal_args[func->arg_count];
            if (arg.size > 8) {
                unimplemented;
#if 0
                s32 dest = x64->stack[func->arg_count];
                
                // TODO(Alexander): we should make a more general memcpy function
                x64_mov(buf, IC_REG | IC_S64, X64_RCX, 0, IC_DISP | IC_S32, 0, arg.size, 0);
                x64_mov(buf, IC_REG | IC_S64, X64_RDI, 0, IC_STK | IC_S64, X64_RSP, dest, 0);
                if (val.type & IC_STK) {
                    x64_lea(buf, X64_RSI, X64_RSP, val.disp, 0);
                } else {
                    unimplemented;
                }
                push_u16(buf, 0xA4F3); // F3 A4 	REP MOVS m8, m8 	ZO
#endif
            } else {
                x64_move_memory_to_register(buf, X64_RAX, X64_RSP, register_displacement(x64, res_index));
            }
            
            if (insn->next_insn) {
                // jump to end of function
                push_u8(buf, 0xE9);
                x64_push_label_rel32(x64, buf, func, 0);
            }
        } break;
        
#if 0
        case BC_MOV | flag_floating: {
            Bytecode_Operand first = bc_binary_first(insn);
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            
            x64_fmov(buf, 
                     dest.type, dest.reg, dest.disp,
                     src.type, src.reg, src.disp, rip);
        } break;
        
        case BC_MOV_8: {
            Bytecode_Operand first = bc_binary_first(insn);
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            dest.type = (IC_TF_MASK & dest.type) | IC_S8;
            src.type = (IC_TF_MASK & src.type) | IC_S8;
            
            x64_mov(buf, 
                    dest.type, dest.reg, dest.disp,
                    src.type, src.reg, src.disp, rip);
        } break;
        
        case BC_INC: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_unary_first(insn), insn->type);
            x64_unary(buf, first.type, first.reg, first.disp, 0xFF, 0, rip);
        } break;
        
        case BC_ADDR_OF: {
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            assert(dest.type & IC_REG);
            
            if (src.type & IC_RIP_DISP32) {
                //x64_lea(buf, dest.reg, X64_RIP, src.disp, rip);
            } else {
                //x64_lea(buf, dest.reg, src.reg, src.disp, rip);
            }
            unimplemented;
        } break;
        
        case BC_DEREF: {
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            assert(dest.type & IC_REG);
            
            if (src.type & IC_REG) {
                src.type = (src.type & IC_RT_MASK) | IC_STK;
            }
            
            x64_mov(buf, 
                    dest.type, dest.reg, dest.disp,
                    src.type, src.reg, src.disp, rip);
        } break;
#endif
        
        case BC_ADD:
        case BC_SUB:
        case BC_MUL:
        case BC_DIV_S:
        case BC_DIV_U:{
            // TODO(Alexander): size and signed flags need to be set here!
            x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index),
                            register_type(func, bc->arg0_index));
            x64_move_extend(buf, X64_RCX, X64_RSP, register_displacement(x64, bc->arg1_index),
                            register_type(func, bc->arg0_index));
            
            switch (opcode) {
                case BC_ADD: x64_add64(buf, X64_RAX, X64_RCX); break; 
                case BC_SUB: x64_sub64(buf, X64_RAX, X64_RCX); break; 
                case BC_MUL: x64_mul64(buf, X64_RAX, X64_RCX); break; 
                case BC_DIV_S: x64_div64(buf, X64_RAX, X64_RCX, true); break; // TODO(Alexander): we might use flags here type flags instead of DIV_S/ DIV_U?
                case BC_DIV_U: x64_div64(buf, X64_RAX, X64_RCX, false); break; 
            }
            
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_ADD | flag_floating: {
            //x64_float_binary(x64, buf, (Bytecode_Binary*) insn, 0x58, rip);
            unimplemented;
        } break;
        
#if 0
        case BC_SUB: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       5, 0x28, rip);
        } break;
        
        case BC_SUB | flag_floating: {
            //x64_float_binary(x64, buf, (Bytecode_Binary*) insn, 0x5C, rip);
            unimplemented;
        } break;
        
        case BC_MUL: {
            //x64_mul(x64, buf, (Bytecode_Binary*) insn, rip);
            unimplemented;
        } break;
        
        case BC_MUL | flag_floating: {
            //x64_float_binary(x64, buf, (Bytecode_Binary*) insn, 0x59, rip);
            unimplemented;
        } break;
        
        case BC_DIV_S:
        case BC_DIV_U: {
            //x64_div(x64, buf, (Bytecode_Binary*) insn, false, rip);
            unimplemented;
        } break;
        
        case BC_DIV_S | flag_floating:
        case BC_DIV_U | flag_floating: {
            //x64_float_binary(x64, buf, (Bytecode_Binary*) insn, 0x5E, rip);
            unimplemented;
        } break;
        
        case BC_MOD_S:
        case BC_MOD_U: {
            //x64_div(x64, buf, (Bytecode_Binary*) insn, true, rip);
            unimplemented;
        } break;
        
        case BC_AND: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       4, 0x20, rip);
        } break;
        
        case BC_OR: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       1, 0x8, rip);
        } break;
        
        case BC_XOR: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       6, 0x30, rip);
        } break;
        
        case BC_SHL: {
            x64_shr(x64, buf, (Bytecode_Binary*) insn, 4, rip);
        } break;
        
        case BC_SHR: {
            x64_shr(x64, buf, (Bytecode_Binary*) insn, 5, rip);
        } break;
        
        case BC_SAR: {
            x64_shr(x64, buf, (Bytecode_Binary*) insn, 7, rip);
        } break;
        
        case BC_WRAP_I64: {
            Bytecode_Operand first = bc_binary_first(insn);
            if (first.kind == BytecodeOperand_register) {
                Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
                //x64->virtual_registers[first.register_index] = src;
                unimplemented;
                
            }
            // noop
        } break;
        
#endif
        // TODO(Alexander): with type flags we don't need separate instructions for U8. S8 etc.
        case BC_EXTEND: {
            x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->arg0_index),
                            register_type(func, bc->arg0_index));
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
#if 0
        
        case BC_CONVERT_F32_S:
        case BC_CONVERT_F64_S: {
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            src.raw_type = (opcode == BC_CONVERT_F32_S) ? IC_F32 : IC_F64;
            
            x64_convert_float_to_int_type(buf, 
                                          dest.type, dest.reg, dest.disp,
                                          src.type, src.reg, src.disp, rip);
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
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            
            // cmp
            x64_binary(buf,
                       dest.type, dest.reg, dest.disp,
                       src.type, src.reg, src.disp, 7, 0x38, rip);
            
            Bytecode_Branch* branch = (Bytecode_Branch*) ((u8*) insn + insn->next_insn);
            if (branch->opcode == BC_BRANCH) {
                // jcc
                push_u16(buf, x64_jcc_opcodes[BC_NEQ - insn->opcode]);
                x64_push_label_rel32(x64, buf, func, branch->label_index);
            } else {
                // setcc
                push_u24(buf, x64_setcc_opcodes[insn->opcode - BC_EQ]); 
            }
        } break;
#endif
        
        case BC_BRANCH: {
            // NOTE(Alexander): conditional branch is handled by operations above
            Bytecode_Branch* branch = (Bytecode_Branch*) insn;
            if (!branch->cond.kind) {
                push_u8(buf, 0xE9);
                x64_push_label_rel32(x64, buf, func, branch->label_index);
            }
        } break;
        
        case BC_LOOP:
        case BC_BLOCK: {
            X64_Block block = {};
            block.label_index = x64->label_index++;
            block.is_loop = opcode == BC_LOOP;
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
        
        case BC_MEMORY_COPY: {
            Bytecode_Memory* mem = (Bytecode_Memory*) insn;
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, mem->dest, BytecodeType_i64);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, mem->src, BytecodeType_i64);
            
            //x64_string_op(x64, buf, 
            //dest.type, dest.reg, dest.disp, 
            //src.type, src.reg, src.disp,
            //mem->size, 0xA4F3, rip, X64_RSI);
            unimplemented;
        } break;
        
        case BC_RDTSC: {
            Bytecode_Operand dest = bc_unary_first(insn);
            assert(dest.kind == BytecodeOperand_register);
            
            Ic_Raw_Type rt = convert_bytecode_type_to_x64(insn->type);
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

#if 0
inline void
x64_unary(Buffer* buf, Ic_Type t, s64 r, s64 d, u8 opcode, u8 reg_field, s64 rip) {
    if (t & IC_T64) {
        x64_rex(buf, REX_W);
    }
    // F7 /3 	NEG r/m32 	M
    push_u8(buf, opcode);
    x64_modrm(buf, t, d, reg_field, r, rip);
}

inline void
x64_binary(Buffer* buf,
           Ic_Type t1, s64 r1, s64 d1, 
           Ic_Type t2, s64 r2, s64 d2,
           u8 reg_field, u8 opcode, s64 rip) {
    
    if (t1 & IC_DISP) {
        Ic_Type tmpt = IC_REG + (t1 & IC_RT_MASK);
        s64 tmpr = X64_RAX;
        x64_mov(buf, tmpt, tmpr, 0, t1, r1, d1, rip);
        t1 = tmpt;
        r1 = tmpr;
        d1 = 0;
    }
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_DISP) {
                if (d2 > U32_MAX) {
                    unimplemented;
                }
                
                if (t1 & IC_T64) {
                    x64_rex(buf, REX_W);
                }
                
                // 81 /0 id 	ADD r/m32, imm32 	MI
                //push_u8(buf, 0x81);
                push_u8(buf, (t1 & IC_T8) ? 0x80 : 0x81);
                push_u8(buf, 0xC0 | (reg_field << 3) | (u8) r1);
                if (t1 & IC_T8) {
                    push_u8(buf, (u8) d2);
                } else {
                    push_u32(buf, (u32) d2);
                }
                
            } else if (t2 & IC_STK_RIP_REG) {
                if (t1 & IC_T64) {
                    x64_rex(buf, REX_W);
                }
                
                // 03 /r 	ADD r32, r/m32 	RM
                push_u8(buf, (t1 & IC_T8) ? opcode + 2 : opcode + 3);
                x64_modrm(buf, t2, d2, r1, r2, rip);
            } else {
                unimplemented;
            }
        } break;
        
        case IC_STK:
        case IC_RIP_DISP32: {
            if (t2 & IC_DISP) {
                
                if ((u64) d2 > U32_MAX && (d2 != -1 || t2 & IC_UINT)) {
                    unimplemented;
                } else if (t2 & IC_UINT && (s32) d2 < 0) {
                    Ic_Type tmpt = IC_T32 + IC_REG;
                    s64 tmpr = X64_RAX;
                    x64_mov(buf, tmpt, tmpr, 0, t2, r2, d2, rip);
                    x64_binary(buf, t1, r1, d1, tmpt, tmpr, 0, reg_field, opcode, rip);
                } else {
                    if (t1 & IC_T64) {
                        x64_rex(buf, REX_W);
                    }
                    
                    // 81 /0 id 	ADD r/m32, imm32 	MI
                    push_u8(buf, (t1 & IC_T8) ? 0x80 : 0x81);
                    //push_u8(buf, 0xC7);
                    s64 disp_size = t1 & IC_T8 ? 1 : 4;
                    x64_modrm(buf, t1, d1, reg_field, r1, rip + disp_size);
                    if (t1 & IC_T8) {
                        push_u8(buf, (u8) d2);
                    } else {
                        push_u32(buf, (u32) d2);
                    }
                }
            } else if (t2 & IC_REG) {
                if (t1 & IC_T16) {
                    push_u8(buf, X64_OP_SIZE_PREFIX);
                }
                
                if (t1 & IC_T64) {
                    x64_rex(buf, REX_W);
                }
                
                // 01 /r 	ADD r/m32, r32 	MR
                push_u8(buf, (t1 & IC_T8) ? opcode : opcode + 1);
                x64_modrm(buf, t1, d1, r2, r1, rip);
                
            } else if (t2 & IC_STK_RIP) {
                s64 tmpr = X64_RAX;
                if (r2 == tmpr || r1 == tmpr) {
                    tmpr = X64_RCX;
                }
                x64_mov(buf, IC_REG + (t2 & IC_RT_MASK), tmpr, 0, t2, r2, d2, rip);
                x64_binary(buf, t1, r1, d1, IC_REG + (t1 & IC_RT_MASK), tmpr, 0, reg_field, opcode, rip);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}

#endif
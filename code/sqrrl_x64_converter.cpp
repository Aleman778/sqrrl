
X64_Assembler
convert_bytecode_to_x64_machine_code(Bytecode* bytecode, Buffer* buf, 
                                     Data_Packer* data_packer,
                                     bool use_absolute_ptrs) {
    
    X64_Assembler x64 = {};
    x64.bytecode = bytecode;
    x64.data_packer = data_packer;
    x64.use_absolute_ptrs = use_absolute_ptrs;
    // TODO(Alexander): employ better allocation strategy!
    x64.functions = (X64_Function*) calloc(array_count(bytecode->functions), 
                                           sizeof(X64_Function));
    
    u8* main_function_ptr = buf->data;
    for_array_v(bytecode->functions, func, func_index) {
        //pln("Compiling function `%`...", f_var(bytecode->function_names[func->type_index]));
        if (bytecode->entry_func_index == func_index) {
            main_function_ptr = buf->data + buf->curr_used;
        }
        convert_bytecode_function_to_x64_machine_code(&x64, func, buf);
        
        if (!use_absolute_ptrs && func->type_index < array_count(bytecode->imports)) {
            push_u16(buf, 0x25FF); // FF 25    JMP RIP-relative
            x64_create_u32_patch(&x64, buf, X64_PATCH_DYNAMIC_LIBRARY, func->type_index);
        }
    }
    
    // Patch relative jump addresses
    for_array_v(x64.jump_patches, patch, _pi) {
        s32 rel32 = (s32) (*patch.target - (patch.origin + 4));
        //pln("% - % = %", f_u64_HEX(*patch.target), f_u64_HEX(patch.origin + 4), f_int(rel32));
        *((s32*) patch.origin) = rel32;
    }
    
    return x64;
}

void
x64_patch_pe_rva(X64_Assembler* x64, PE_Executable* pe, Buffer* buf) {
    s64 iat_rva   = (s64) buf->data + (s64) pe->iat_rva   - (s64) pe->text_rva;
    s64 rdata_rva = (s64) buf->data + (s64) pe->rdata_rva - (s64) pe->text_rva;
    s64 data_rva  = (s64) buf->data + (s64) pe->data_rva  - (s64) pe->text_rva;
    
    // Patch relative data addresses
    for_array_v(x64->address_patches, patch, _pi1) {
        s64 section_offset = (s64) patch.data - (s64) (patch.origin + 4);
        switch (patch.kind) {
            case X64_PATCH_READ_ONLY_DATA: {
                *((s32*) patch.origin) = (s32) (rdata_rva + section_offset);
            } break;
            
            case X64_PATCH_READ_WRITE_DATA: {
                *((s32*) patch.origin) = (s32) (data_rva + section_offset);
            } break;
            
            case X64_PATCH_DYNAMIC_LIBRARY: {
                s64 iat_offset = x64->bytecode->imports[patch.data].iat_offset;
                section_offset = iat_offset - (s64) (patch.origin + 4);
                *((s32*) patch.origin) = (s32) (iat_rva + section_offset);
            } break;
            
            default: unimplemented;
        }
    }
}

void
convert_bytecode_function_to_x64_machine_code(X64_Assembler* x64, Bytecode_Function* func, Buffer* buf) {
    assert(x64->bytecode && x64->functions && x64->data_packer && "X64 assembler is not setup correctly");
    
    x64->curr_function = &x64->functions[func->type_index];
    x64->curr_function->code = buf->data + buf->curr_used;
    
    if (func->is_imported || func->is_intrinsic) {
        return;
    }
    
    func->code_ptr = buf->data + buf->curr_used;
    
    arena_clear(&x64->arena);
    x64->slots = arena_push_array_of_structs(&x64->arena, func->register_count, X64_Slot);
    
    // TODO(Alexander): most  of the code here is windows x64 calling convention specific
    
    // Setup labels
    if (!x64->curr_function->labels) {
        x64->curr_function->labels = (u8**) calloc(func->block_count + 1, sizeof(u8*));
    }
    x64->label_index = 1; // NOTE(Alexander): treat label 0 as end of function
    
    // Windows calling convention stack setup
    s32 register_count = (s32) func->register_count;
    s32 stack_caller_args = func->max_caller_arg_count*8;
    x64->current_stack_size = stack_caller_args;
    x64->max_stack_size = x64->current_stack_size;
    
    // Save HOME registers (TODO: move this it's windows calling convention only)
    // TODO(Alexander): this doens't handle returns larger than 8 bytes
    Bytecode_Function_Arg* formal_args = function_arg_types(func);
    for (int arg_index = min((int) func->arg_count - 1, 3); arg_index >= 0; arg_index--) {
        Bytecode_Function_Arg formal_arg = formal_args[arg_index];
        bool is_float = formal_arg.type.kind == BC_TYPE_FLOAT;
        if (is_float) {
            X64_VReg dest = float_arg_registers_ccall_windows[arg_index];
            x64_move_float_register_to_memory(buf, X64_RSP, arg_index*8 + 8, dest,
                                              formal_arg.type.size);
        } else {
            X64_Reg dest = int_arg_registers_ccall_windows[arg_index];
            x64_move_register_to_memory(buf, X64_RSP, arg_index*8 + 8, dest);
        }
    }
    
    // Prologue
    x64_rex(buf, REX_W); // sub rsp, stack_size
    push_u8(buf, 0x81);
    x64_modrm_direct(buf, 5, X64_RSP);
    u32* prologue_stack_size = (u32*) (buf->data + buf->curr_used);
    push_u32(buf, 0);
    
    // Copy registers to our local stack space
    s32** callee_args_disp = 0;
    if (func->arg_count > 0) {
        callee_args_disp = arena_push_array_of_structs(&x64->arena, func->arg_count, s32*);
        for (int arg_index = 0; arg_index < (int) func->arg_count; arg_index++) {
            Bytecode_Function_Arg formal_arg = formal_args[arg_index];
            
            s64 src = arg_index*8;
            s32 size = formal_arg.size;
            s32 align = formal_arg.align;
            if (arg_index == 0 && func->return_as_first_arg) {
                // Only copy the pointer from return argument
                size = 8; align = 8;
            }
            
            if (size < 8) {
                size = 8; align = 8;
            }
            
            s64 dest = register_stack_alloc(x64, arg_index, formal_arg.type, size, align, size > 8);
            if (size == 8) {
                // Load source value
                // REX.W + 8B /r 	MOV RAX, RSP + disp 	RM
                callee_args_disp[arg_index] =
                    x64_move_memory_to_register_disp(buf, X64_RAX, X64_RSP, src);
                x64_move_register_to_memory(buf, X64_RSP, dest, X64_RAX);
                
            } else {
                // TODO(Alexander): we shouldn't copy this!!!
                x64_lea(buf, X64_RDI, X64_RSP, dest);
                callee_args_disp[arg_index] =
                    x64_move_memory_to_register_disp(buf, X64_RSI, X64_RSP, src);
                x64_move_immediate_to_register(buf, X64_RCX, size);
                x64_rep_movsb(buf, X64_RDI, X64_RSI, X64_RCX);
            }
        }
    }
    s32* variadic_data_ptr = 0;
    if (func->is_variadic) {
        // Set the Var_Args data pointer
        int var_args = func->arg_count - 1;
        variadic_data_ptr = x64_lea_patch_disp(buf, X64_RAX, X64_RSP, func->arg_count*8);
        X64_Slot dest = get_slot(x64, var_args);
        x64_move_register_to_memory(buf, X64_RSP, dest.disp, X64_RAX);
        
        //pln("%: arg_count = %", f_var(x64->bytecode->function_names[func->type_index]), f_int());
    }
    
    for_bc_insn(func, curr) {
        convert_bytecode_insn_to_x64_machine_code(x64, buf, func, curr);
    }
    
    // Align stack by 16-bytes (excluding 8 bytes for return address)
    s32 stack_size = (s32) x64->max_stack_size;
    stack_size = (s32) align_forward(stack_size + 8, 16) - 8;
    s32 stack_callee_args = stack_size + 8;
    
    // Patch stack offsets
    for (int i = 0; i < func->arg_count; i++) {
        *callee_args_disp[i] += stack_callee_args;
        //pln("%", f_int(*callee_args_disp[i]));
    } 
    if (variadic_data_ptr) {
        *variadic_data_ptr += stack_callee_args;
    }
    
    
    // Epilogue
    x64->curr_function->labels[0] = buf->data + buf->curr_used;
    x64_rex(buf, REX_W); // add rsp, stack_usage
    push_u8(buf, 0x81);
    x64_modrm_direct(buf, 0, X64_RSP);
    push_u32(buf, stack_size); *prologue_stack_size = stack_size;
    push_u8(buf, 0xC3); // RET near
}

void
convert_windows_x64_argument_list_to_x64_machine_code(X64_Assembler* x64, Buffer* buf, int arg_count, int* args, int var_arg_start=-1) {
    for (int arg_index = arg_count - 1; arg_index >= 0; arg_index--) {
        int src_index = args[arg_index];
        Bytecode_Type type = get_slot(x64, src_index).type;
        bool is_float = type.kind == BC_TYPE_FLOAT;
        
        if (is_float) {
            X64_VReg dest = X64_XMM0;
            dest = float_arg_registers_ccall_windows[arg_index];
            x64_move_slot_to_float_register(x64, buf, (X64_VReg) dest, src_index);
            if (arg_index >= 4 || arg_index >= var_arg_start) {
                x64_move_float_register_to_memory(buf, X64_RSP, arg_index*8, (X64_VReg) dest,
                                                  type.size);
            }
            
        } else {
            X64_Reg dest = X64_RAX;
            if (arg_index < 4) {
                dest = int_arg_registers_ccall_windows[arg_index];
            }
            x64_move_slot_to_register(x64, buf, dest, src_index);
            
            if (arg_index >= 4 || arg_index >= var_arg_start) {
                x64_move_register_to_memory(buf, X64_RSP, arg_index*8, dest);
                
            }
        }
    }
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
            s64 immediate = ((Bytecode_Const_Int*) bc)->val;
            
            s32 dest_disp = register_displacement(x64, bc->res_index, bc->type);
            if (immediate < S32_MIN || immediate > S32_MAX) {
                x64_move_rax_u64(buf, immediate);
                x64_move_register_to_memory(buf, X64_RSP, dest_disp, X64_RAX);
            } else {
                // REX.W + C7 /0 id 	MOV r/m64, imm32 	MI
                x64_rex(buf, REX_W);
                push_u8(buf, 0xC7);
                x64_modrm(buf, 0, X64_RSP, dest_disp, 0);
                push_u32(buf, (u32) immediate);
            }
        } break;
        
        case BC_F32_CONST: {
            Exported_Data f32_data = export_struct(x64->data_packer, f32, Read_Data_Section);
            *((f32*) f32_data.data) = ((Bytecode_Const_F32*) bc)->val;
            x64_move_const_to_float_register(x64, buf, X64_XMM5, f32_data, 4);
            x64_move_float_register_to_memory(buf, X64_RSP,
                                              register_displacement(x64, bc->res_index, bc->type), 
                                              X64_XMM5, sizeof(f32));
        } break;
        
        case BC_F64_CONST: {
            Exported_Data f64_data = export_struct(x64->data_packer, f64, Read_Data_Section);
            *((f64*) f64_data.data) = ((Bytecode_Const_F64*) bc)->val;
            x64_move_const_to_float_register(x64, buf, X64_XMM5, f64_data, 8);
            x64_move_float_register_to_memory(buf, X64_RSP, 
                                              register_displacement(x64, bc->res_index, bc->type), 
                                              X64_XMM5, sizeof(f64));
        } break;
        
        case BC_LOCAL: {
            Bytecode_Local* local = (Bytecode_Local*) bc;
            register_stack_alloc(x64, local->res_index, local->type, local->size, local->align, true);
        } break;
        
        case BC_GLOBAL: {
            Bytecode_Global* g = &x64->bytecode->globals[bc->a_index];
            
            Exported_Data data = {};
            data.data = g->address;
            data.relative_ptr = g->offset;
            switch (g->kind) {
                case BC_MEM_READ_ONLY:  data.section = Read_Data_Section; break;
                case BC_MEM_READ_WRITE: data.section = Data_Section; break;
                default: unimplemented;
            }
            
            // REX.W + 8D /r 	LEA r64,m 	RM
            x64_rex(buf, REX_W);
            push_u8(buf, 0x8D);
            x64_modrm_exported_data(x64, buf, X64_RAX, data);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_FUNCTION: {
            int func_index = bc->a_index;
            
            // REX.W + 8D /r 	LEA r64,m 	RM
            x64_rex(buf, REX_W);
            push_u8(buf, 0x8D);
            push_u8(buf, X64_RBP);
            x64_jump_address(x64, buf, &x64->functions[func_index].code);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_ARRAY_ACCESS: {
            Bytecode_Array_Access* array_access = (Bytecode_Array_Access*) bc;
            
            int stride = array_access->stride;
            if (is_power_of_two(stride) && stride < 8) {
                u8 scale = (u8) intrin_index_of_first_set_bit((u32) stride);
                x64_move_slot_to_register(x64, buf, X64_RAX, array_access->index);
                
                X64_Slot src = get_slot(x64, array_access->base);
                s32 src_reg, src_disp;
                if (src.kind == X64_SLOT_RSP_DISP32_INPLACE) {
                    src_reg = X64_RSP;
                    src_disp = src.disp;
                } else {
                    x64_move_slot_to_register(x64, buf, X64_RCX, array_access->base);
                    src_reg = X64_RCX;
                    src_disp = 0;
                }
                
                // LEA RAX, [RSP + (index * scale) + src_disp]
                x64_rex(buf, REX_W);
                push_u8(buf, 0x8D);
                push_u8(buf, MODRM_INDIRECT_DISP32 | X64_RSP&7); // reg=RAX, rm=RSP (for SIB)
                push_u8(buf, (scale << 6) | src_reg&7); // [RSP + (RAX * scale) + disp32]
                push_u32(buf, src_disp); // TODO(Alexander): we can optimize this to use DISP8 too!
                
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
                
            } else {
                x64_move_slot_to_register(x64, buf, X64_RAX, array_access->base);
                x64_move_slot_to_register(x64, buf, X64_RCX, array_access->index);
                if (stride > 1) {
                    x64_mul64_immediate(buf, X64_RCX, X64_RCX, stride);
                }
                x64_add64(buf, X64_RAX, X64_RCX);
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
            }
        } break;
        
        case BC_FIELD_ACCESS: {
            Bytecode_Field_Access* field_access = (Bytecode_Field_Access*) bc;
            X64_Slot src = get_slot(x64, field_access->base);
            
            if (src.kind == X64_SLOT_RSP_DISP32_INPLACE) {
                assert(x64->slots[bc->res_index].kind == X64_SLOT_EMPTY);
                src.disp += field_access->offset;
                x64->slots[field_access->res_index] = src;
                
            } else {
                x64_move_memory_to_register(buf, X64_RAX, X64_RSP, 
                                            register_displacement(x64, field_access->base));
                x64_add64_immediate(buf, X64_RAX, field_access->offset);
                x64_move_register_to_memory(buf, X64_RSP, 
                                            register_displacement(x64, field_access->res_index), X64_RAX);
            }
        } break;
        
        case BC_STORE: {
            // TODO(Alexander): this assumes that size of data in memory is at least 8 bytes,
            // if we make it less then we will overwrite data outside our scope.
            
            X64_Slot ptr = get_slot(x64, bc->res_index);
            if (ptr.kind == X64_SLOT_RSP_DISP32 ||
                ptr.kind == X64_SLOT_RSP_DISP32_INPLACE) {
                
                X64_Reg dest = X64_RSP; s32 dest_disp = ptr.disp;
                
                x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
                
                if (ptr.kind != X64_SLOT_RSP_DISP32_INPLACE) {
                    x64_move_memory_to_register(buf, X64_RCX, dest, dest_disp);
                    dest = X64_RCX; 
                    dest_disp = 0;
                }
                
                Bytecode_Type type = get_slot(x64, bc->a_index).type;
                switch (type.size) {
                    case 1: {
                        x64_move8_register_to_memory(buf, dest, dest_disp, X64_RAX);
                    } break;
                    
                    case 2: {
                        x64_move16_register_to_memory(buf, dest, dest_disp, X64_RAX);
                    } break;
                    
                    case 4: {
                        x64_move32_register_to_memory(buf, dest, dest_disp, X64_RAX);
                    } break;
                    
                    default: {
                        x64_move_register_to_memory(buf, dest, dest_disp, X64_RAX);
                    } break;
                }
            } else {
                unimplemented;
            }
        } break;
        
        case BC_LOAD: {
            X64_Slot src = get_slot(x64, bc->a_index);
            
            //push_u8(buf, 0xCC);
            if (src.kind == X64_SLOT_RSP_DISP32_INPLACE) {
                x64_move_memory_to_register(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->a_index));
                
            } else if (src.kind == X64_SLOT_RSP_DISP32) {
                x64_move_memory_to_register(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->a_index));
                x64_move_memory_to_register(buf, X64_RAX, X64_RAX, 0);
                
            } else {
                unimplemented;
            }
            x64_move_register_to_memory(buf, X64_RSP,
                                        register_displacement(x64, bc->res_index, bc->type),
                                        X64_RAX);
        } break;
        
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) bc;
            Bytecode_Function* target = x64->bytecode->functions[call->func_index];
            int* args = (int*) bc_call_args(call);
            convert_windows_x64_argument_list_to_x64_machine_code(x64, buf, 
                                                                  call->arg_count - target->ret_count, 
                                                                  args + target->ret_count, 
                                                                  target->arg_count);
            
            if (target->is_imported && x64->use_absolute_ptrs) {
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
            
            if (target->ret_count == 1) {
                // TODO(Alexander): Add support for floats!
                if (bc->type.kind == BC_TYPE_FLOAT) {
                    x64_move_float_register_to_memory(buf, X64_RSP,
                                                      register_displacement(x64, args[0], bc->type),
                                                      X64_XMM0, bc->type.size);
                } else {
                    x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, args[0], bc->type),
                                                X64_RAX);
                }
            }
        } break;
        
        case BC_CALL_INDIRECT: {
            Bytecode_Call_Indirect* call = (Bytecode_Call_Indirect*) bc;
            
            int* args = bc_call_args(call);
            convert_windows_x64_argument_list_to_x64_machine_code(x64, buf, call->arg_count - call->ret_count, 
                                                                  args + call->ret_count);
            
            // Indirect function call from register
            x64_move_memory_to_register(buf, X64_RAX, X64_RSP,
                                        register_displacement(x64, call->func_ptr_index));
            
            // FF /2 	CALL r/m64 	M 	
            push_u8(buf, 0xFF);
            x64_modrm_direct(buf, 2, X64_RAX);
            
            if (call->ret_count == 1) {
                // TODO(Alexander): Add support for floats!
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, args[0]),
                                            X64_RAX);
            }
        } break;
        
        case BC_RETURN: {
            if (bc->res_index >= 0) {
                if (bc->type.kind == BC_TYPE_FLOAT) {
                    x64_move_slot_to_float_register(x64, buf, X64_XMM0, bc->res_index);
                } else {
                    x64_move_slot_to_register(x64, buf, X64_RAX, bc->res_index);
                }
            }
            
            if (insn->next_insn) {
                // jump to end of function
                push_u8(buf, 0xE9);
                x64_jump_address_for_label(x64, buf, func, 0);
            }
        } break;
        
        case BC_EXTEND: {
            x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_RAX);
        } break;
        
        case BC_TRUNCATE: {
            X64_Slot src = get_slot(x64, bc->a_index);
            src.type = bc->type;
            set_slot(x64, bc->res_index, src);
            
            //Bytecode_Type type = register_type(func, bc->res_index);
            
            //x64_move_extend(buf, X64_RAX, X64_RSP, register_displacement(x64, bc->a_index),
            //type.size, type.flags & BC_FLAG_SIGNED);
            //x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index), X64_RAX);
        } break;
        
        case BC_FLOAT_TO_INT: {
            X64_Slot src = get_slot(x64, bc->a_index);
            x64_move_slot_to_float_register(x64, buf, X64_XMM0, bc->a_index);
            x64_convert_float_to_int(buf, X64_RAX, X64_XMM0, src.type.size);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_RAX);
        } break;
        
        case BC_INT_TO_FLOAT: {
            x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
            x64_convert_int_to_float(buf, X64_XMM0, X64_RAX, bc->type.size);
            x64_move_float_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type),
                                              X64_XMM0, bc->type.size);
        } break;
        
        case BC_FLOAT_TO_FLOAT: {
            X64_Slot src = get_slot(x64, bc->a_index);
            x64_move_slot_to_float_register(x64, buf, X64_XMM0, bc->a_index);
            x64_convert_float_to_float(buf, X64_XMM0, X64_XMM0, src.type.size);
            x64_move_float_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_XMM0, bc->type.size);
        } break;
        
        case BC_INC:
        case BC_DEC: {
            assert(bc->type.kind != BC_TYPE_FLOAT && "inc is not implemented for float types");
            x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
            if (bc->opcode == BC_INC) {
                x64_inc(buf, X64_RAX);
            } else {
                x64_dec(buf, X64_RAX);
            }
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_RAX);
        } break;
        
        case BC_NEG: {
            if (bc->type.kind & BC_TYPE_FLOAT) {
                // XOR the sign float bit
                Exported_Data sign_mask = export_size(x64->data_packer, Read_Data_Section, 
                                                      bc->type.size, bc->type.size);
                if (bc->type.size == 8) {
                    *((u64*) sign_mask.data) = 0x8000000000000000ull;
                } else {
                    *((u32*) sign_mask.data) = 0x80000000;
                }
                
                x64_move_slot_to_float_register(x64, buf, X64_XMM4, bc->a_index);
                x64_move_const_to_float_register(x64, buf, X64_XMM5, sign_mask, bc->type.size);
                x64_xorps(buf, X64_XMM4, X64_XMM5, bc->type.size);
                x64_move_float_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_XMM4, bc->type.size);
            } else {
                x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
                x64_neg(buf, X64_RAX);
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_RAX);
            }
        } break;
        
        case BC_NOT: {
            assert(bc->type.kind != BC_TYPE_FLOAT && "not is not implemented for float types");
            x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
            x64_not(buf, X64_RAX);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_RAX);
        } break;
        
        case BC_COPY: {
            X64_Slot src = get_slot(x64, bc->a_index);
            x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, src.type), X64_RAX);
        } break;
        
        case BC_OR:
        case BC_AND:
        case BC_ADD:
        case BC_SUB:
        case BC_MUL:
        case BC_DIV_S:
        case BC_DIV_U:
        case BC_MOD_S:
        case BC_MOD_U: {
            if (bc->type.kind == BC_TYPE_FLOAT) {
                x64_move_slot_to_float_register(x64, buf, X64_XMM4, bc->a_index);
                x64_move_slot_to_float_register(x64, buf, X64_XMM5, bc->b_index);
                
                switch (bc->opcode) {
                    case BC_ADD: x64_addss(buf, X64_XMM4, X64_XMM5, bc->type.size); break; 
                    case BC_SUB: x64_subss(buf, X64_XMM4, X64_XMM5, bc->type.size); break; 
                    case BC_MUL: x64_mulss(buf, X64_XMM4, X64_XMM5, bc->type.size); break; 
                    case BC_DIV_S: // TODO(Alexander): we should only have DIV!!!
                    case BC_DIV_U: x64_divss(buf, X64_XMM4, X64_XMM5, bc->type.size); break;
                    
                    default: unimplemented;
                }
                x64_move_float_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type),
                                                  X64_XMM4, bc->type.size);
                
            } else {
                x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
                x64_move_slot_to_register(x64, buf, X64_RCX, bc->b_index);
                
                X64_Reg result = X64_RAX;
                switch (bc->opcode) {
                    case BC_OR:  x64_or64(buf, X64_RAX, X64_RCX); break;
                    case BC_AND: x64_and64(buf, X64_RAX, X64_RCX); break;
                    case BC_ADD: x64_add64(buf, X64_RAX, X64_RCX); break; 
                    case BC_SUB: x64_sub64(buf, X64_RAX, X64_RCX); break; 
                    case BC_MUL: x64_mul64(buf, X64_RAX, X64_RCX); break; 
                    case BC_DIV_S: x64_div64(buf, X64_RAX, X64_RCX, true); break; // TODO(Alexander): we might use flags here type flags instead of DIV_S/ DIV_U?
                    case BC_DIV_U: x64_div64(buf, X64_RAX, X64_RCX, false); break; 
                    
                    case BC_MOD_S: { // TODO(Alexander): we might use flags here type flags instead of DIV_S/ DIV_U?
                        x64_div64(buf, X64_RAX, X64_RCX, true); 
                        result = X64_RDX; 
                    } break;
                    
                    case BC_MOD_U: {
                        x64_div64(buf, X64_RAX, X64_RCX, false);
                        result = X64_RDX;
                    } break;
                    
                    default: unimplemented;
                }
                
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), result);
            }
        } break;
        
        case BC_SHL:
        case BC_SHR:
        case BC_SAR: {
            x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
            x64_move_slot_to_register(x64, buf, X64_RCX, bc->b_index);
            switch (bc->opcode) {
                case BC_SHL: x64_shl(buf, X64_RAX, X64_RCX); break;
                case BC_SHR: x64_shr(buf, X64_RAX, X64_RCX); break;
                case BC_SAR: x64_sar(buf, X64_RAX, X64_RCX); break;
            }
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_RAX);
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
            Bytecode_Type type = get_slot(x64, bc->a_index).type;
            
            if (type.kind == BC_TYPE_FLOAT) {
                x64_move_slot_to_float_register(x64, buf, X64_XMM4, bc->a_index);
                x64_move_slot_to_float_register(x64, buf, X64_XMM5, bc->b_index);
                x64_ucomiss(buf, X64_XMM4, X64_XMM5, type.size);
                
            } else {
                x64_move_slot_to_register(x64, buf, X64_RAX, bc->a_index);
                x64_move_slot_to_register(x64, buf, X64_RCX, bc->b_index);
                x64_cmp64(buf, X64_RAX, X64_RCX);
            }
            
            Bytecode_Branch* branch = (Bytecode_Branch*) ((u8*) insn + insn->next_insn);
            if (branch->opcode == BC_BRANCH) {
                // jcc
                push_u16(buf, x64_jcc_opcodes[insn->opcode - BC_EQ]);
                x64_jump_address_for_label(x64, buf, func, branch->label_index);
            } else {
                // setcc
                push_u24(buf, x64_setcc_opcodes[insn->opcode - BC_EQ]); 
                x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, bc->res_index, bc->type), X64_RAX);
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
            x64_move_slot_to_register(x64, buf, X64_RDI, bc->res_index);
            x64_move_slot_to_register(x64, buf, X64_RSI, bc->a_index);
            x64_move_immediate_to_register(buf, X64_RCX, bc->b_index);
            x64_rep_movsb(buf, X64_RDI, X64_RSI, X64_RCX);
        } break;
        
        case BC_MEMSET: {
            x64_move_slot_to_register(x64, buf, X64_RDI, bc->res_index);
            x64_move_immediate_to_register(buf, X64_RAX, bc->a_index);
            x64_move_immediate_to_register(buf, X64_RCX, bc->b_index);
            x64_rep_stosb(buf, X64_RDI, X64_RAX, X64_RCX);
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
            
            Bytecode_Call_Indirect* call = (Bytecode_Call_Indirect*) bc;
            int* args = bc_call_args(call);
            x64_move_register_to_memory(buf, X64_RSP, register_displacement(x64, args[0], bc->type), X64_RAX);
        } break;
        
        default: {
            assert(0 && "invalid X64 instruction");
        } break;
    }
}

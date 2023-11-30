
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
        if (bytecode->entry_func_index == func_index) {
            main_function_ptr = buf->data + buf->curr_used;
        }
        convert_bytecode_function_to_x64_machine_code(&x64, func, buf);
        
        if (!use_absolute_ptrs && func->type_index < array_count(bytecode->imports)) {
            Bytecode_Import* import = &bytecode->imports[func->type_index];
            
            if (import->kind == BC_IMPORT_FUNC) {
                push_u16(buf, 0x25FF); // FF 25    JMP RIP-relative
                x64_create_u32_patch(&x64, buf, X64_PATCH_DYNAMIC_LIBRARY, import->func_index);
                
            } else if (import->kind == BC_IMPORT_GLOBAL) {
                assert(import->global_index < array_count(bytecode->globals));
                Bytecode_Global* g = &bytecode->globals[import->global_index];
                push_u16(buf, 0x25FF); // FF 25    JMP RIP-relative
                x64_create_u32_patch(&x64, buf, X64_PATCH_READ_WRITE_DATA, g->offset);
                
            } else {
                verify_not_reached();
            }
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
    pln("Compiling function `%`...", f_var(x64->bytecode->function_names[func->type_index]));
    
    if (x64->bytecode->function_names[func->type_index] == Sym_main) {
        //__debugbreak();
    }
    
    func->code_ptr = buf->data + buf->curr_used;
    
    arena_clear(&x64->arena);
    x64->slots = arena_push_array_of_structs(&x64->arena, func->register_count, X64_Slot);
    
    // Reset registers
    for (int reg_id = 0; reg_id < X64_REG_COUNT; reg_id++) {
        x64->allocated_registers[reg_id] = -1;
        x64->registers_used[reg_id] = false;
    }
    
    // TODO(Alexander): most  of the code here is windows x64 calling convention specific
    
    // Setup labels
    if (!x64->curr_function->labels) {
        x64->curr_function->labels = (u8**) calloc(func->block_count + 1, sizeof(u8*));
    }
    x64->label_index = 1; // NOTE(Alexander): treat label 0 as end of function
    
    // Windows calling convention stack setup
    if (func->max_caller_arg_count > 0 && func->max_caller_arg_count < 4) {
        // Reserve spaces for HOME registers at minimum
        func->max_caller_arg_count = 4;
    }
    s32 stack_size = func->max_caller_arg_count*8;
    
    // Register allocation pass
    Bytecode_Function_Arg* formal_args = function_arg_types(func);
    for (int arg_index = 0; arg_index < func->arg_count; arg_index++) {
        Bytecode_Function_Arg arg = formal_args[arg_index];
        
        bool use_stack = true; 
        if (arg_index < 4 && is_power_of_two(arg.size & 0xF)) {
            if (arg.type.kind == BC_TYPE_FLOAT) {
                X64_Reg home_reg = float_arg_registers_ccall_windows[arg_index];
                x64_allocate_float_register(x64, arg.type, arg_index, home_reg);
                use_stack = false;
            } else {
                X64_Reg home_reg = int_arg_registers_ccall_windows[arg_index];
                x64_allocate_register(x64, arg.type, arg_index, home_reg);
                use_stack = false;
            }
        }
        
        if (use_stack) {
            x64_spill_slot(x64, arg_index);
        }
    }
    x64->tmp_registers = arena_push_array_of_structs(&x64->arena, func->insn_count*2, X64_Reg);
    int insn_index = 0;
    for_bc_insn(func, curr) {
        stack_size = x64_simple_register_allocator(x64, curr, insn_index++, stack_size);
    }
    
    // Calculate stack space used by spilled register
    for (int slot_index = func->arg_count; slot_index < func->register_count; slot_index++) {
        X64_Slot slot = x64->slots[slot_index];
        if (slot.kind == X64_SLOT_RSP_DISP32) {
            // TODO(Alexander): for simplicity we always use 8-bytes for spilled registers
            s32 disp = (s32) align_forward(stack_size, 8);
            stack_size = disp + 8;
            x64->slots[slot_index].disp = disp;
        }
    }
    
    // Align stack by 16-bytes (excluding 8 bytes for return address)
    if (stack_size > 0) {
        stack_size = (s32) align_forward(stack_size + 8, 16) - 8;
    }
    
    // Save HOME registers (TODO: move this it's windows calling convention only)
    // TODO(Alexander): this doens't handle returns larger than 8 bytes
    for (int arg_index = min((int) func->arg_count - 1, 3); arg_index >= 0; arg_index--) {
        Bytecode_Function_Arg formal_arg = formal_args[arg_index];
        bool is_float = formal_arg.type.kind == BC_TYPE_FLOAT;
        if (is_float) {
            X64_Reg dest = float_arg_registers_ccall_windows[arg_index];
            x64_move_float_register_to_memory(buf, X64_RSP, arg_index*8 + 8, dest,
                                              formal_arg.type.size);
        } else {
            X64_Reg dest = int_arg_registers_ccall_windows[arg_index];
            x64_move_register_to_memory(buf, X64_RSP, arg_index*8 + 8, dest);
        }
    }
    
    // TODO(Alexander): windows calling convention, set calle saved to
    // RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 because they are nonvolatile
    bool callee_saved[X64_REG_COUNT] = {};
    {
        for (int i = 0; i < X64_REG_COUNT; i++) {
            if (x64->registers_used[i] && x64_windows_nonvolatile_reg[i]) {
                callee_saved[i] = true;
            }
        }
        
        bool is_callee_reg_saved[X64_REG_COUNT] = {};
        for (int slot_index = 0; slot_index < func->register_count; slot_index++) {
            X64_Slot slot = x64->slots[slot_index];
            if (slot.kind == X64_SLOT_REG && x64_windows_nonvolatile_reg[slot.reg]) {
                callee_saved[slot.reg] = true;
            }
        }
    }
    
    // Saved registers increases our stack size thus increasing offset to caller args
    s32 caller_arg_stack = stack_size + 8;
    for (u8 reg = X64_RAX; reg < X64_REG_COUNT; reg++) {
        if (callee_saved[reg]) {
            caller_arg_stack += 8 + ((reg & X64_XMM0) >> 1);
        }
    }
    for (int arg_index = 0; arg_index < func->arg_count; arg_index++) {
        X64_Slot slot = x64->slots[arg_index];
        if (slot.kind == X64_SLOT_RSP_DISP32_INPLACE ||
            slot.kind == X64_SLOT_RSP_DISP32) {
            x64->slots[arg_index].disp = caller_arg_stack + arg_index*8;
        }
    }
    pln("arg_count: %", f_int(func->arg_count));
    pln("stack_size: %", f_int(stack_size));
    pln("caller_arg_stack: %", f_int(caller_arg_stack));
    
#if 1
    // Register dump
    pln("\nRegister dump:");
    for (int slot_index = 0; slot_index < func->register_count; slot_index++) {
        X64_Slot slot = get_slot(x64, slot_index);
        switch (slot.kind) {
            case X64_SLOT_RSP_DISP32_INPLACE: {
                pln("r%: [RSP + %] (inplace)", f_int(slot_index), f_int(slot.disp));
            } break;
            
            case X64_SLOT_RSP_DISP32: {
                pln("r%: [RSP + %]", f_int(slot_index), f_int(slot.disp));
            } break;
            
            case X64_SLOT_REG: {
                pln("r%: %", f_int(slot_index), f_cstring(register_names[slot.reg]));
            } break;
        }
    }
#endif
    
    // Save callee saved registers
    for (int i = 0; i < X64_REG_COUNT; i++) {
        // NOTE(Alexander): reverse order, push XMM registers first to ensure 16-byte alignment
        X64_Reg reg = (X64_Reg) (X64_REG_COUNT - i - 1);
        if (callee_saved[reg]) {
            pln("callee_saved: %", f_cstring(register_names[reg]));
            if (reg & X64_XMM0) {
                x64_sub64_immediate(buf, X64_RSP, 16);
                
                // F3 0F 7F /r MOVDQU xmm2/m128, xmm1
                // TODO(Alexander): we should align the stack here and use MOVDQA instead!
                push_u8(buf, 0xF3);
                if (reg&8) x64_rex(buf, 0, reg);
                push_u16(buf, 0x7F0F);
                x64_modrm(buf, reg, X64_RSP, 0);
            } else {
                // 50+rd 	PUSH r64
                x64_rex(buf, REX_W, reg, reg);
                push_u8(buf, 0x50+(reg&7));
            }
        }
    }
    
    // Prologue
    if (stack_size > 0) {
        x64_sub64_immediate(buf, X64_RSP, stack_size);
    }
    
    // Set Var_Args pointer if used
    if (func->is_variadic) {
        // Set the Var_Args data pointer
        // TODO(Alexander): we should use the register allocate to free RAX and RCX
        int var_args = func->arg_count - 1;
        x64_lea(buf, X64_RAX, X64_RSP, caller_arg_stack + func->arg_count*8);
        X64_Slot dest = get_slot(x64, var_args);
        x64_move_memory_to_register(buf, X64_RCX, X64_RSP, dest.disp);
        x64_move_register_to_memory(buf, X64_RCX, 0, X64_RAX);
    }
    
    // Codegen pass
    insn_index = 0;
    for_bc_insn(func, curr) {
        convert_bytecode_insn_to_x64_machine_code(x64, buf, func, curr, insn_index++);
    }
    
    x64->curr_function->labels[0] = buf->data + buf->curr_used;
    
    // Epilogue
    if (stack_size > 0) {
        x64_add64_immediate(buf, X64_RSP, stack_size);
    }
    
    // Restore callee saved registers
    for (int i = 0; i < X64_REG_COUNT; i++) {
        X64_Reg reg = (X64_Reg) i;
        if (callee_saved[reg]) {
            if (reg & X64_XMM0) {
                x64_add64_immediate(buf, X64_RSP, 16);
                
                // F3 0F 6F /r MOVDQU xmm1, xmm2/m128
                // TODO(Alexander): we should align the stack here and use MOVDQA instead!
                push_u8(buf, 0xF3);
                if (reg&8) x64_rex(buf, 0, reg);
                push_u16(buf, 0x6F0F);
                x64_modrm(buf, reg, X64_RSP, 0);
            } else {
                // 58+rd 	POP r64
                x64_rex(buf, REX_W, (u8) reg, (u8) reg);
                push_u8(buf, 0x58+(reg&7));
            }
        }
    }
    
    push_u8(buf, 0xC3); // RET near
}

void
x64_allocate_windows_x64_argument_list(X64_Assembler* x64, Bytecode_Type ret_type, int ret_count, int* args, int arg_count) {
    // Spill HOME registers (if possible avoid it for correclty mapped registers)
    int actual_arg_count = arg_count - ret_count;
    for (int arg_index = 0; arg_index < 4; arg_index++) {
        X64_Reg home_float = float_arg_registers_ccall_windows[arg_index];
        X64_Reg home_int = int_arg_registers_ccall_windows[arg_index];
        
        if (arg_index < actual_arg_count) {
            int src_index = args[arg_index + ret_count];
            Bytecode_Type type = get_slot(x64, src_index).type;
            bool is_float = type.kind == BC_TYPE_FLOAT;
            
            if (is_float) {
                x64_spill(x64, home_float, src_index);
                x64_spill(x64, home_int);
                
            } else {
                x64_spill(x64, home_int, src_index);
                x64_spill(x64, home_float);
            }
        } else {
            x64_spill(x64, home_float);
            x64_spill(x64, home_int);
        }
    }
    
    // Spill other volatile registers
    x64_spill(x64, X64_RAX, args[0]);
    x64_spill(x64, X64_R10);
    x64_spill(x64, X64_R11);
    x64_spill(x64, X64_XMM4);
    x64_spill(x64, X64_XMM5);
    
    // Allocate return register RAX/XMM0
    if (ret_count == 1) {
        if (ret_type.kind == BC_TYPE_FLOAT) {
            x64_allocate_float_register(x64, ret_type, args[0], X64_XMM0);
        } else {
            x64_allocate_register(x64, ret_type, args[0], X64_RAX);
        }
    }
}


s32
x64_simple_register_allocator(X64_Assembler* x64, Bytecode_Instruction* bc_insn, int bc_index, s32 stack_usage) {
    Bytecode_Binary* bc = (Bytecode_Binary*) bc_insn;
    X64_Reg* tmp = x64->tmp_registers + bc_index*2;
    tmp[0] = X64_REG_COUNT;
    tmp[1] = X64_REG_COUNT;
    
    switch (bc->opcode) {
        case BC_NOOP: 
        case BC_BRANCH:
        case BC_LOOP:
        case BC_BLOCK:
        case BC_END: break;
        
        case BC_DEBUG_BREAK: {
            //__debugbreak();
        } break;
        
        case BC_DROP: {
            x64_drop(x64, bc->res_index);
        } break;
        
        case BC_INT_CONST: {
            s64 immediate = ((Bytecode_Const_Int*) bc)->val;
            if (immediate < S32_MIN || immediate > S32_MAX) {
                x64_spill(x64, X64_RAX);
            }
            x64_allocate_register(x64, bc->type, bc->res_index, X64_RAX);
        } break;
        
        case BC_F32_CONST: {
            tmp[0] = x64_allocate_tmp_float_register(x64, bc->res_index, X64_XMM4);
            x64_allocate_float_register(x64, bc->type, bc->res_index, tmp[0]);
        } break;
        
        case BC_F64_CONST: {
            tmp[0] = x64_allocate_tmp_float_register(x64, bc->res_index, X64_XMM4);
            x64_allocate_float_register(x64, bc->type, bc->res_index, tmp[0]);
        } break;
        
        case BC_LOCAL: {
            Bytecode_Local* local = (Bytecode_Local*) bc;
            stack_usage = x64_allocate_stack(x64, local->type, local->res_index, local->size, local->align, stack_usage);
        } break;
        
        case BC_GLOBAL:
        case BC_FUNCTION: {
            tmp[0] = x64_allocate_tmp_register(x64, -1, X64_RAX);
            x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
        } break;
        
        case BC_ARRAY_ACCESS: {
            Bytecode_Array_Access* array_access = (Bytecode_Array_Access*) bc;
            
            int stride = array_access->stride;
            if (is_power_of_two(stride) && stride < 8) {
                
                tmp[0] = x64_allocate_tmp_register(x64, array_access->index, X64_RAX);
                X64_Slot base_slot = get_slot(x64, array_access->base);
                if (base_slot.kind != X64_SLOT_RSP_DISP32_INPLACE) {
                    tmp[1] = x64_allocate_tmp_register(x64, array_access->base, X64_RCX);
                }
                x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
            } else {
                tmp[0] = x64_allocate_tmp_register(x64, array_access->index, X64_RAX);
                tmp[1] = x64_allocate_tmp_register(x64, array_access->base, X64_RCX);
                x64_allocate_register(x64, bc->type, bc->res_index, tmp[1]);
            }
        } break;
        
        case BC_FIELD_ACCESS: {
            Bytecode_Field_Access* field_access = (Bytecode_Field_Access*) bc;
            X64_Slot src = get_slot(x64, field_access->base);
            
            if (src.kind == X64_SLOT_RSP_DISP32_INPLACE) {
                assert(x64->slots[bc->res_index].kind == X64_SLOT_EMPTY);
                src.disp += field_access->offset;
                set_slot(x64, field_access->res_index, src);
                pln("r%: disp = %", f_int(field_access->res_index), f_int(src.disp));
                
            } else {
                tmp[0] = x64_allocate_tmp_register(x64, field_access->base, X64_RAX);
                x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
            }
        } break;
        
        case BC_STORE: {
            X64_Slot ptr = get_slot(x64, bc->res_index);
            if (ptr.type.kind == BC_TYPE_FLOAT) {
                tmp[0] = x64_allocate_tmp_float_register(x64, bc->a_index, X64_XMM4);
                if (ptr.kind != X64_SLOT_RSP_DISP32_INPLACE) {
                    tmp[1] = x64_allocate_tmp_register(x64, bc->res_index, X64_RAX);
                }
                
            } else {
                tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
                if (ptr.kind != X64_SLOT_RSP_DISP32_INPLACE) {
                    tmp[1] = x64_allocate_tmp_register(x64, -1, X64_RCX);
                }
                assert(tmp[0] != tmp[1]);
            }
        } break;
        
        case BC_LOAD: {
            X64_Slot ptr = get_slot(x64, bc->a_index);
            if (bc->type.kind == BC_TYPE_FLOAT) {
                tmp[0] = x64_allocate_tmp_float_register(x64, -1, X64_XMM0);
                if (ptr.kind != X64_SLOT_RSP_DISP32_INPLACE) {
                    tmp[1] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
                }
                x64_allocate_float_register(x64, bc->type, bc->res_index, tmp[0]);
            } else {
                tmp[0] = x64_allocate_tmp_register(x64, -1, X64_RAX);
                if (ptr.kind != X64_SLOT_RSP_DISP32_INPLACE) {
                    tmp[1] = x64_allocate_tmp_register(x64, bc->a_index, X64_RCX);
                }
                x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
            }
        } break;
        
        case BC_LEA: {
            // TODO(Alexander): should we pass the size/align of the type here?
            x64_spill_slot(x64, bc->a_index);
            
            tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
            x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
        } break;
        
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) bc;
            Bytecode_Function* target = x64->bytecode->functions[call->func_index];
            int* args = (int*) bc_call_args(call);
            x64_allocate_windows_x64_argument_list(x64, bc->type, target->ret_count, args, call->arg_count);
        } break;
        
        case BC_CALL_INDIRECT: {
            Bytecode_Call_Indirect* call = (Bytecode_Call_Indirect*) bc;
            int* args = bc_call_args(call);
            x64_allocate_windows_x64_argument_list(x64, bc->type, call->ret_count, args, call->arg_count);
        } break;
        
        case BC_RETURN: {
            // TODO(Alexander): don't think we need anything now?
        } break;
        
        case BC_COPY: {
            X64_Slot src = get_slot(x64, bc->a_index);
            Bytecode_Type type = bc->type.kind != BC_TYPE_VOID ? bc->type : src.type;
            tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
            x64_allocate_register(x64, type, bc->res_index, tmp[0]);
        } break;
        
        case BC_EXTEND:
        case BC_TRUNCATE:
        case BC_INC:
        case BC_DEC:
        case BC_NOT: {
            tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
            x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
        } break;
        
        case BC_FLOAT_TO_INT: {
            tmp[0] = x64_allocate_tmp_float_register(x64, bc->a_index, X64_XMM4);
            tmp[1] = x64_allocate_tmp_register(x64, bc->res_index, X64_RAX);
            x64_allocate_register(x64, bc->type, bc->res_index, tmp[1]);
        } break;
        
        case BC_INT_TO_FLOAT: {
            tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
            tmp[1] = x64_allocate_tmp_float_register(x64, bc->res_index, X64_XMM4);
            x64_allocate_float_register(x64, bc->type, bc->res_index, tmp[1]);
        } break;
        
        case BC_FLOAT_TO_FLOAT: {
            tmp[0] = x64_allocate_tmp_float_register(x64, bc->a_index, X64_XMM4);
            x64_allocate_float_register(x64, bc->type, bc->res_index, tmp[0]);
        } break;
        
        case BC_NEG: {
            if (bc->type.kind == BC_TYPE_FLOAT) {
                tmp[0] = x64_allocate_tmp_float_register(x64, bc->a_index, X64_XMM4);
                tmp[1] = x64_allocate_tmp_float_register(x64, bc->res_index, X64_XMM5);
                x64_allocate_float_register(x64, bc->type, bc->res_index, tmp[0]);
            } else {
                tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
                x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
            }
        } break;
        
        case BC_OR:
        case BC_AND:
        case BC_ADD:
        case BC_SUB:
        case BC_MUL: {
            if (bc->type.kind == BC_TYPE_FLOAT) {
                tmp[0] = x64_allocate_tmp_float_register(x64, bc->a_index, X64_XMM4);
                tmp[1] = x64_allocate_tmp_float_register(x64, bc->b_index, X64_XMM5);
                pln("float arith: tmp[0] = %, tmp[1] = %", f_int(tmp[0]), f_int(tmp[1]));
                x64_allocate_float_register(x64, bc->type, bc->res_index, tmp[0]);
                
            } else {
                tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
                tmp[1] = x64_allocate_tmp_register(x64, bc->b_index, X64_RCX);
                pln("int arith: tmp[0] = %, tmp[1] = %", f_int(tmp[0]), f_int(tmp[1]));
                x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
            }
        } break;
        
        case BC_DIV_S:
        case BC_DIV_U:
        case BC_MOD_S:
        case BC_MOD_U: {
            if (bc->type.kind == BC_TYPE_FLOAT) {
                tmp[0] = x64_allocate_tmp_float_register(x64, bc->a_index, X64_XMM4);
                tmp[1] = x64_allocate_tmp_float_register(x64, bc->b_index, X64_XMM5);
                pln("float arith: tmp[0] = %, tmp[1] = %", f_int(tmp[0]), f_int(tmp[1]));
                x64_allocate_float_register(x64, bc->type, bc->res_index, tmp[0]);
                
            } else {
                x64_spill(x64, X64_RAX, bc->a_index);
                x64_spill(x64, X64_RDX);
                tmp[0] = X64_RAX;
                tmp[1] = x64_allocate_tmp_register(x64, bc->b_index, X64_RCX);
                
                bool is_mod = bc->opcode == BC_MOD_S || bc->opcode == BC_MOD_U;
                x64_allocate_register(x64, bc->type, bc->res_index, is_mod ? X64_RDX : X64_RAX);
            }
        } break;
        
        case BC_SHL:
        case BC_SHR:
        case BC_SAR: {
            x64_spill(x64, X64_RCX);
            tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
            tmp[1] = X64_RCX;
            x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
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
                tmp[0] = x64_allocate_tmp_float_register(x64, bc->a_index, X64_XMM4);
                tmp[1] = x64_allocate_tmp_float_register(x64, bc->b_index, X64_XMM5);
                x64_allocate_register(x64, bc->type, bc->res_index, X64_RAX);
                
            } else {
                tmp[0] = x64_allocate_tmp_register(x64, bc->a_index, X64_RAX);
                tmp[1] = x64_allocate_tmp_register(x64, bc->b_index, X64_RCX);
                x64_allocate_register(x64, bc->type, bc->res_index, tmp[0]);
            }
        } break;
        
        case BC_MEMCPY:
        case BC_MEMSET: {
            // NOTE(Alexander): all needed for the rep movsb/ stosb instruction
            // TODO(Alexander): we can implement a faster version for size small sizes
            x64_spill(x64, X64_RDI);
            x64_spill(x64, X64_RSI);
            x64_spill(x64, X64_RCX);
        } break;
        
        case BC_X64_RDTSC: {
            // NOTE(Alexander): rdtsc puts lower part in EAX and upper part in EDX
            x64_spill(x64, X64_RAX);
            x64_spill(x64, X64_RDX);
            x64_allocate_register(x64, bc->type, bc->res_index, X64_RAX);
        } break;
        
        default: {
            assert(0 && "invalid X64 instruction");
        } break;
    }
    
    // Flag tmp registers as allocated
    for (u8 i = 0; i < X64_REG_COUNT; i++) {
        if (x64->allocated_registers[i] == -2) {
            x64->allocated_registers[i] = -1;
        }
    }
    
    return stack_usage;
}

void
convert_windows_x64_argument_list_to_x64_machine_code(X64_Assembler* x64, Buffer* buf, int arg_count, int* args, int var_arg_start=-1) {
    for (int arg_index = arg_count - 1; arg_index >= 0; arg_index--) {
        int src_index = args[arg_index];
        Bytecode_Type type = get_slot(x64, src_index).type;
        bool is_float = type.kind == BC_TYPE_FLOAT;
        
        if (is_float) {
            X64_Reg dest = X64_XMM0;
            if (arg_index < 4) {
                dest = float_arg_registers_ccall_windows[arg_index];
            }
            x64_move_slot_to_float_register(x64, buf, dest, src_index);
            if (arg_index >= 4 || arg_index >= var_arg_start) {
                x64_move_float_register_to_memory(buf, X64_RSP, arg_index*8, dest, type.size);
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
                                          Bytecode_Instruction* bc_insn,
                                          int bc_index) {
    Bytecode_Binary* bc = (Bytecode_Binary*) bc_insn;
    X64_Reg* tmp = x64->tmp_registers + bc_index*2;
    switch (bc->opcode) {
        case BC_NOOP: 
        case BC_DROP:
        case BC_LOCAL: break;
        
        case BC_DEBUG_BREAK: {
            push_u8(buf, 0xCC);
        } break;
        
        case BC_INT_CONST: {
            s64 immediate = ((Bytecode_Const_Int*) bc)->val;
            if (immediate < S32_MIN || immediate > S32_MAX) {
                x64_move_rax_u64(buf, immediate);
                x64_move_register_to_slot(x64, buf, bc->res_index, X64_RAX);
                
            } else {
                X64_Slot dest = get_slot(x64, bc->res_index);
                if (dest.kind == X64_SLOT_RSP_DISP32) {
                    // REX.W + C7 /0 id 	MOV r/m64, imm32 	MI
                    x64_rex(buf, REX_W);
                    push_u8(buf, 0xC7);
                    x64_modrm(buf, 0, X64_RSP, dest.disp);
                    push_u32(buf, (u32) immediate);
                    
                } else if (dest.kind == X64_SLOT_REG) {
                    x64_move_immediate_to_register(buf, dest.reg, (s32) immediate);
                } else {
                    verify_not_reached();
                }
            }
        } break;
        
        case BC_F32_CONST: {
            Exported_Data f32_data = export_struct(x64->data_packer, f32, Read_Data_Section);
            *((f32*) f32_data.data) = ((Bytecode_Const_F32*) bc)->val;
            x64_move_const_to_float_register(x64, buf, tmp[0], f32_data, 4);
            x64_move_float_register_to_slot(x64, buf, bc->res_index, tmp[0]);
        } break;
        
        case BC_F64_CONST: {
            Exported_Data f64_data = export_struct(x64->data_packer, f64, Read_Data_Section);
            *((f64*) f64_data.data) = ((Bytecode_Const_F64*) bc)->val;
            x64_move_const_to_float_register(x64, buf, tmp[0], f64_data, 8);
            x64_move_float_register_to_slot(x64, buf, bc->res_index, tmp[0]);
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
            x64_rex(buf, REX_W, tmp[0]);
            push_u8(buf, 0x8D);
            x64_modrm_exported_data(x64, buf, tmp[0], data);
            
            x64_move_register_to_slot(x64, buf, bc->res_index, tmp[0]);
        } break;
        
        case BC_FUNCTION: {
            int func_index = bc->a_index;
            
            // REX.W + 8D /r 	LEA r64,m 	RM
            x64_rex(buf, REX_W, tmp[0]);
            push_u8(buf, 0x8D);
            x64_rip_rel(buf, tmp[0]);
            x64_jump_address(x64, buf, &x64->functions[func_index].code);
            
            x64_move_register_to_slot(x64, buf, bc->res_index, tmp[0]);
        } break;
        
        case BC_ARRAY_ACCESS: {
            Bytecode_Array_Access* array_access = (Bytecode_Array_Access*) bc;
            
            int stride = array_access->stride;
            if (is_power_of_two(stride) && stride < 8) {
                X64_Reg index = tmp[0], base = tmp[1];
                x64_move_slot_to_register(x64, buf, index, array_access->index);
                
                s64 disp = 0;
                X64_Slot base_slot = get_slot(x64, array_access->base);
                if (base_slot.kind == X64_SLOT_RSP_DISP32_INPLACE) {
                    base = X64_RSP;
                    disp = base_slot.disp;
                } else {
                    x64_move_slot_to_register(x64, buf, base, array_access->base);
                }
                
                u8 scale = (u8) intrin_index_of_first_set_bit((u32) stride);
                x64_lea_sib(buf, index, scale, index, base, disp);
                x64_move_register_to_slot(x64, buf, bc->res_index, index);
                
            } else {
                X64_Reg index = tmp[0], base = tmp[1];
                x64_move_slot_to_register(x64, buf, index, array_access->index);
                x64_move_slot_to_register(x64, buf, base, array_access->base);
                if (stride > 1) {
                    x64_mul64_immediate(buf, index, index, stride);
                }
                x64_add64(buf, base, index);
                x64_move_register_to_slot(x64, buf, bc->res_index, base);
            }
        } break;
        
        case BC_FIELD_ACCESS: {
            Bytecode_Field_Access* field_access = (Bytecode_Field_Access*) bc;
            X64_Slot src = get_slot(x64, field_access->base);
            
            if (src.kind != X64_SLOT_RSP_DISP32_INPLACE) {
                X64_Reg base = tmp[0];
                x64_move_slot_to_register(x64, buf, base, field_access->base);
                x64_add64_immediate(buf, base, field_access->offset);
                x64_move_register_to_slot(x64, buf, bc->res_index, base);
            }
        } break;
        
        case BC_STORE: {
            X64_Slot ptr = get_slot(x64, bc->res_index);
            
            s32 disp = ptr.disp;
            X64_Reg dest = X64_RSP; 
            X64_Reg src = tmp[0];
            
            if (ptr.kind != X64_SLOT_RSP_DISP32_INPLACE) {
                disp = 0;
                dest = tmp[1];
                x64_move_slot_to_register(x64, buf, dest, bc->res_index);
            }
            
            Bytecode_Type type = get_slot(x64, bc->a_index).type;
            if (type.kind == BC_TYPE_FLOAT) {
                x64_move_slot_to_float_register(x64, buf, src, bc->a_index);
                x64_move_float_register_to_memory(buf, dest, disp, src, type.size);
                
            } else {
                x64_move_slot_to_register(x64, buf, src, bc->a_index);
                
                switch (type.size) {
                    case 1: x64_move8_register_to_memory(buf, dest, disp, src); break;
                    case 2: x64_move16_register_to_memory(buf, dest, disp, src); break;
                    case 4: x64_move32_register_to_memory(buf, dest, disp, src); break;
                    default: x64_move_register_to_memory(buf, dest, disp, src); break;
                }
            }
        } break;
        
        case BC_LOAD: {
            X64_Slot ptr = get_slot(x64, bc->a_index);
            if (bc->type.kind == BC_TYPE_FLOAT) {
                if (ptr.kind == X64_SLOT_RSP_DISP32_INPLACE) {
                    x64_move_slot_to_float_register(x64, buf, tmp[0], bc->a_index);
                    
                } else {
                    x64_move_slot_to_register(x64, buf, tmp[1], bc->a_index);
                    x64_move_memory_to_float_register(buf, tmp[0], tmp[1], 0, bc->type.size);
                }
                x64_move_float_register_to_slot(x64, buf, bc->res_index, tmp[0]);
                
            } else {
                if (ptr.kind == X64_SLOT_RSP_DISP32_INPLACE) {
                    x64_move_memory_to_register(buf, tmp[0], X64_RSP, ptr.disp);
                    
                } else {
                    x64_move_slot_to_register(x64, buf, tmp[1], bc->a_index);
                    x64_move_memory_to_register(buf, tmp[0], tmp[1], 0);
                }
                x64_move_register_to_slot(x64, buf, bc->res_index, tmp[0]);
            }
        } break;
        
        case BC_LEA: {
            X64_Slot src = get_slot(x64, bc->a_index);
            assert(src.kind == X64_SLOT_RSP_DISP32 ||
                   src.kind == X64_SLOT_RSP_DISP32_INPLACE);
            x64_lea(buf, tmp[0], X64_RSP, src.disp);
            x64_move_register_to_slot(x64, buf, bc->res_index, tmp[0]);
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
                if (bc->type.kind == BC_TYPE_FLOAT) {
                    x64_move_float_register_to_slot(x64, buf, args[0], X64_XMM0);
                } else {
                    x64_move_register_to_slot(x64, buf, args[0], X64_RAX);
                }
            }
        } break;
        
        case BC_CALL_INDIRECT: {
            Bytecode_Call_Indirect* call = (Bytecode_Call_Indirect*) bc;
            
            int* args = bc_call_args(call);
            convert_windows_x64_argument_list_to_x64_machine_code(x64, buf, call->arg_count - call->ret_count, 
                                                                  args + call->ret_count);
            
            // Indirect function call from register
            x64_move_slot_to_register(x64, buf, X64_RAX, call->func_ptr_index);
            
            // FF /2 	CALL r/m64 	M 	
            push_u8(buf, 0xFF);
            x64_modrm_direct(buf, 2, X64_RAX);
            
            if (call->ret_count == 1) {
                if (bc->type.kind == BC_TYPE_FLOAT) {
                    x64_move_float_register_to_slot(x64, buf, args[0], X64_XMM0);
                } else {
                    x64_move_register_to_slot(x64, buf, args[0], X64_RAX);
                }
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
            
            if (bc->next_insn) {
                // jump to end of function
                push_u8(buf, 0xE9);
                x64_jump_address_for_label(x64, buf, func, 0);
            }
        } break;
        
        case BC_COPY:
        case BC_EXTEND:
        case BC_TRUNCATE: {
            // TODO(Alexander): can we optimize truncate, it shouldn't need any move
            X64_Reg src = tmp[0];
            x64_move_slot_to_register(x64, buf, src, bc->a_index);
            x64_move_register_to_slot(x64, buf, bc->res_index, src);
        } break;
        
        case BC_FLOAT_TO_INT: {
            X64_Reg float_reg = tmp[0], int_reg = tmp[1];
            X64_Slot src = get_slot(x64, bc->a_index);
            x64_move_slot_to_float_register(x64, buf, float_reg, bc->a_index);
            x64_convert_float_to_int(buf, int_reg, float_reg, src.type.size);
            x64_move_register_to_slot(x64, buf, bc->res_index, int_reg);
        } break;
        
        case BC_INT_TO_FLOAT: {
            X64_Reg int_reg = tmp[0], float_reg = tmp[1];
            X64_Reg src = x64_move_slot_to_register(x64, buf, int_reg, bc->a_index);
            x64_convert_int_to_float(buf, float_reg, int_reg, bc->type.size);
            x64_move_float_register_to_slot(x64, buf, bc->res_index, float_reg);
        } break;
        
        case BC_FLOAT_TO_FLOAT: {
            X64_Slot src = get_slot(x64, bc->a_index);
            verify(src.type.size != bc->type.size);
            x64_move_slot_to_float_register(x64, buf, tmp[0], bc->a_index);
            x64_convert_float_to_float(buf, tmp[0], tmp[0], src.type.size);
            x64_move_float_register_to_slot(x64, buf, bc->res_index, tmp[0]);
        } break;
        
        case BC_INC:
        case BC_DEC: {
            assert(bc->type.kind != BC_TYPE_FLOAT && "inc/dec is not implemented for float types");
            x64_move_slot_to_register(x64, buf, tmp[0], bc->a_index);
            if (bc->opcode == BC_INC) {
                x64_inc(buf, tmp[0]);
            } else {
                x64_dec(buf, tmp[0]);
            }
            x64_move_register_to_slot(x64, buf, bc->res_index, tmp[0]);
        } break;
        
        case BC_NEG: {
            if (bc->type.kind == BC_TYPE_FLOAT) {
                // XOR the sign float bit
                Exported_Data sign_mask = export_size(x64->data_packer, Read_Data_Section, 
                                                      bc->type.size, bc->type.size);
                if (bc->type.size == 8) {
                    *((u64*) sign_mask.data) = 0x8000000000000000ull;
                } else {
                    *((u32*) sign_mask.data) = 0x80000000;
                }
                
                x64_move_slot_to_float_register(x64, buf, tmp[0], bc->a_index);
                x64_move_const_to_float_register(x64, buf, tmp[1], sign_mask, bc->type.size);
                x64_xorps(buf, tmp[0], tmp[1], bc->type.size);
                x64_move_float_register_to_slot(x64, buf, bc->res_index, tmp[0]);
            } else {
                x64_move_slot_to_register(x64, buf, tmp[0], bc->a_index);
                x64_neg(buf, tmp[0]);
                x64_move_register_to_slot(x64, buf, bc->res_index, tmp[0]);
            }
        } break;
        
        case BC_NOT: {
            assert(bc->type.kind != BC_TYPE_FLOAT && "not is not implemented for float types");
            x64_move_slot_to_register(x64, buf, tmp[0], bc->a_index);
            x64_not(buf, tmp[0]);
            x64_move_register_to_slot(x64, buf, bc->res_index, tmp[0]);
        } break;
        
        case BC_OR:
        case BC_AND:
        case BC_ADD:
        case BC_SUB:
        case BC_MUL: 
        case BC_DIV_S:
        case BC_DIV_U:
        case BC_MOD_S:
        case BC_MOD_U:
        case BC_SHL:
        case BC_SHR:
        case BC_SAR: {
            X64_Reg a = tmp[0], b = tmp[1];
            if (bc->type.kind == BC_TYPE_FLOAT) {
                x64_move_slot_to_float_register(x64, buf, a, bc->a_index);
                x64_move_slot_to_float_register(x64, buf, b, bc->b_index);
                
                switch (bc->opcode) {
                    case BC_ADD: x64_addss(buf, a, b, bc->type.size); break; 
                    case BC_SUB: x64_subss(buf, a, b, bc->type.size); break; 
                    case BC_MUL: x64_mulss(buf, a, b, bc->type.size); break; 
                    case BC_DIV_S: // TODO(Alexander): we should only have DIV!!!
                    case BC_DIV_U: x64_divss(buf, a, b, bc->type.size); break;
                    default: unimplemented;
                }
                x64_move_float_register_to_slot(x64, buf, bc->res_index, a);
                
            } else {
                x64_move_slot_to_register(x64, buf, a, bc->a_index);
                x64_move_slot_to_register(x64, buf, b, bc->b_index);
                
                switch (bc->opcode) {
                    case BC_OR:  x64_or64(buf, a, b); break;
                    case BC_AND: x64_and64(buf, a, b); break;
                    case BC_ADD: x64_add64(buf, a, b); break;
                    case BC_SUB: x64_sub64(buf, a, b); break;
                    case BC_MUL: x64_mul64(buf, a, b); break;
                    case BC_DIV_S: x64_div64(buf, a, b, true); break;
                    case BC_DIV_U: x64_div64(buf, a, b, false); break;
                    case BC_MOD_S: x64_div64(buf, a, b, true); break;
                    case BC_MOD_U: x64_div64(buf, a, b, false); break;
                    case BC_SHL: x64_shl(buf, a, b); break;
                    case BC_SHR: x64_shr(buf, a, b); break;
                    case BC_SAR: x64_sar(buf, a, b); break;
                    default: unimplemented;
                }
                
                bool is_mod = bc->opcode == BC_MOD_S || bc->opcode == BC_MOD_U;
                x64_move_register_to_slot(x64, buf, bc->res_index, is_mod ? X64_RDX : a);
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
            Bytecode_Type type = get_slot(x64, bc->a_index).type;
            if (type.kind == BC_TYPE_FLOAT) {
                x64_move_slot_to_float_register(x64, buf, X64_XMM4, bc->a_index);
                x64_move_slot_to_float_register(x64, buf, X64_XMM5, bc->b_index);
                x64_ucomiss(buf, X64_XMM4, X64_XMM5, type.size);
                
            } else {
                X64_Reg a = tmp[0], b = tmp[1];
                x64_move_slot_to_register(x64, buf, a, bc->a_index);
                x64_move_slot_to_register(x64, buf, b, bc->b_index);
                x64_cmp64(buf, a, b);
            }
            
            Bytecode_Branch* branch = (Bytecode_Branch*) ((u8*) bc + bc->next_insn);
            if (branch->opcode == BC_BRANCH) {
                // jcc
                push_u16(buf, x64_jcc_opcodes[bc->opcode - BC_EQ]);
                x64_jump_address_for_label(x64, buf, func, branch->label_index);
            } else {
                // setcc
                push_u24(buf, x64_setcc_opcodes[bc->opcode - BC_EQ]); 
                x64_move_register_to_slot(x64, buf, bc->res_index, X64_RAX);
            }
        } break;
        
        case BC_BRANCH: {
            // NOTE(Alexander): conditional branch is handled by operations above
            Bytecode_Branch* branch = (Bytecode_Branch*) bc;
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
            x64_move_register_to_slot(x64, buf, args[0], X64_RAX);
        } break;
        
        default: {
            assert(0 && "invalid X64 instruction");
        } break;
    }
}

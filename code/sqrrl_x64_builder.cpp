
inline bool
x64_register_allocation_check_interference(Interference_Graph* interference_graph,
                                           X64_Register_Node* node) {
    for_array(node->interference, dep_id, _) {
        X64_Register_Node* other_node = &map_get(interference_graph, *dep_id);
        assert(node != other_node && "detected short cycle");
        if (node->physical_register == other_node->physical_register) {
            return false;
        }
    }
    
    return true;
}

void
x64_perform_register_allocation(X64_Builder* x64) {
    for_map (x64->interference_graph, it) {
        X64_Register_Node* node = &it->value;
        if (!node->is_allocated) {
            
            // Allocate the regsiter if possible
            if (node->is_vector_register) {
                for (int physical_reg_index = 0;
                     physical_reg_index < fixed_array_count(x64_vector_register_table);
                     physical_reg_index++) {
                    
                    node->physical_register = x64_vector_register_table[physical_reg_index];
                    node->is_allocated = x64_register_allocation_check_interference(x64->interference_graph, node);
                    if (node->is_allocated) break;
                }
                //assert(node->is_allocated && "out of registers to allocate");
            } else {
                for (int physical_reg_index = 0;
                     physical_reg_index < fixed_array_count(x64_int_register_table);
                     physical_reg_index++) {
                    
                    node->physical_register = x64_int_register_table[physical_reg_index];
                    node->is_allocated = x64_register_allocation_check_interference(x64->interference_graph, node);
                    if (node->is_allocated) break;
                }
                //assert(node->is_allocated && "out of registers to allocate");
            }
            
            if (!node->is_allocated) {
                // NOTE(Alexander): no more free registers needs to be spilled
                // TODO(Alexander): we need to handle, spilling
                node->is_spilled = true;
            }
        }
    }
    
#define ALLOC_REG(operand) \
if (operand_is_register(operand.kind) || operand_is_memory(operand.kind)) { \
if (!operand.is_allocated) { \
X64_Register_Node* node = \
&map_get(x64->interference_graph, operand.virtual_register); \
operand.reg = node->physical_register; \
operand.is_allocated = true; \
} \
}
    
    for_x64_basic_block(x64->first_basic_block, insn, insn_index, {
                            ALLOC_REG(insn->op0);
                            ALLOC_REG(insn->op1);
                            ALLOC_REG(insn->op2);});
}


// NOTE(Alexander): makes sure that when memory gets fragmented that we start a new block
// TODO(Alexander): introduce allocators so we can reuse arena_push_size (mostly copypasta)
void*
x64_text_push_size(X64_Builder* x64, umm size, umm align, umm flags=0) {
    Memory_Arena* arena = &x64->text_section_arena;
    umm current = (umm) (arena->base + arena->curr_used);
    umm offset = align_forward(current, align) - (umm) arena->base;
    
    if (offset + size > arena->size) {
        if (arena->min_block_size == 0) {
            arena->min_block_size = ARENA_DEFAULT_BLOCK_SIZE;
        }
        
        arena->base = (u8*) calloc(1, arena->min_block_size);
        arena->curr_used = 0;
        arena->prev_used = 0;
        arena->size = arena->min_block_size;
        
        Bc_Label label = create_unique_bc_label(&x64->label_indices, 
                                                x64->curr_basic_block->label.ident);
        x64_push_basic_block(x64, label);
        
        current = (umm) arena->base + arena->curr_used;
        offset = align_forward(current, align) - (umm) arena->base;
    } 
    
    void* result = arena->base + offset;
    arena->prev_used = arena->curr_used;
    arena->curr_used = offset + size;
    return result;
}

void
x64_fix_memory_to_memory_instruction(X64_Builder* x64, X64_Instruction* insn) {
    if (operand_is_memory(insn->op0.kind) && operand_is_memory(insn->op1.kind)) {
        
        X64_Operand_Kind reg_kind = X64Operand_r64;
        switch (insn->op1.kind) {
            case X64Operand_m8: reg_kind = X64Operand_r8; break;
            case X64Operand_m16: reg_kind = X64Operand_r16; break;
            case X64Operand_m32: reg_kind = X64Operand_r32; break;
            case X64Operand_m64: reg_kind = X64Operand_r64; break;
        }
        
        // We push a new instruction and copy over the old one onto that
        X64_Instruction* mov_insn = insn;
        insn = x64_text_push_struct(x64, X64_Instruction);
        x64->curr_basic_block->count++;
        x64->instruction_count++;
        *insn = *mov_insn;
#if BUILD_DEBUG
        insn->comment = "fixed memory to memory in above instruction";
#endif
        
        
        mov_insn->opcode = X64Opcode_mov;
        mov_insn->op0 = x64_allocate_temporary_register(x64, reg_kind);
        mov_insn->op1 = insn->op1;
        
        if (insn->opcode == X64Opcode_movss || 
            insn->opcode == X64Opcode_movsd) {
            mov_insn->opcode = insn->opcode;
            mov_insn->op0.kind = X64Operand_xmm;
        }
        
        insn->op1 = mov_insn->op0;
    }
}


X64_Instruction*
_x64_push_instruction(X64_Builder* x64, X64_Opcode opcode, cstring comment = 0) {
    assert(x64->curr_basic_block);
    
    if (x64->curr_instruction) {
        // NOTE(Alexander): Make sure we aren't reading and writing to memory at once
        X64_Instruction* insn = x64->curr_instruction;
        x64_fix_memory_to_memory_instruction(x64, insn);
    }
    
    X64_Instruction* insn = x64_text_push_struct(x64, X64_Instruction);
    insn->opcode = opcode;
    x64->curr_basic_block->count++;
    x64->curr_instruction = insn;
    x64->instruction_count++;
    
#if BUILD_DEBUG
    insn->comment = comment;
#else
    (void) comment;
#endif
    
    return insn;
}


X64_Basic_Block*
x64_push_basic_block(X64_Builder* x64, Bc_Label label) {
    //pln("x64_text_push_basic_block: label = %, %",
    //f_string(vars_load_string(label.ident)), f_int(label.index));
    Memory_Arena* arena = &x64->text_section_arena;
    if (!arena_can_fit_size(arena, sizeof(X64_Basic_Block) + sizeof(X64_Instruction),
                            max(alignof(X64_Basic_Block), alignof(X64_Instruction)))) {
        arena_grow(arena);
    }
    
    X64_Basic_Block* block = arena_push_struct(arena, X64_Basic_Block);
    block->label = label;
    if (x64->curr_basic_block) {
        x64->curr_basic_block->next = block;
    } else {
        x64->first_basic_block = block;
    }
    x64->curr_basic_block = block;
    
    X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_label);
    insn->op0.kind = X64Operand_basic_block;
    insn->op0.basic_block = block;
    
    block->first = x64->curr_instruction;
    block->count = 1;
    
    return block;
}

X64_Operand
x64_build_immediate(X64_Builder* x64, s64 value, Basic_Type type) {
    X64_Operand result = {};
    
    switch (type) {
        case Basic_bool:
        case Basic_s8:
        case Basic_u8: {
            result.kind = X64Operand_imm8;
            result.imm8 = (s8) value;
        } break;
        
        case Basic_s16:
        case Basic_u16: {
            result.kind = X64Operand_imm16;
            result.imm16 = (s16) value;
        } break;
        
        case Basic_s32:
        case Basic_u32: {
            result.kind = X64Operand_imm32;
            result.imm32 = (s32) value;
        } break;
        
        case Basic_s64:
        case Basic_u64: {
            // NOTE(Alexander): if possible use 32-bit immediate
            if (value < S32_MIN || value > S32_MAX) {
                result.kind = X64Operand_imm64;
                result.imm64 = (s64) value;
            } else {
                // NOTE(Alexander): this will be sign-extended
                result.kind = X64Operand_imm32;
                result.imm32 = (s32) value;
            }
        } break;
        
        default: assert(0 && "unsupported immediate type");
    }
    
    return result;
}

inline X64_Operand
x64_build_jump_target(X64_Builder* x64, Bc_Label label) {
    X64_Operand result = {};
    result.kind = X64Operand_jump_target;
    result.jump_target = label;
    return result;
}

inline X64_Operand
x64_build_data_target(X64_Builder* x64, Bc_Label label, Type* type) {
    X64_Operand result = {};
    
    if (type->kind == TypeKind_Basic) {
        switch (type->Basic.kind) {
            case Basic_u8:
            case Basic_s8: {
                result.kind = X64Operand_m8_target; 
            } break;
            
            case Basic_u16: 
            case Basic_s16: {
                result.kind = X64Operand_m16_target; 
            } break;
            
            case Basic_u32: 
            case Basic_s32:
            case Basic_f32: {
                result.kind = X64Operand_m32_target; 
            } break;
            
            case Basic_u64:
            case Basic_s64:
            case Basic_f64:
            case Basic_string:
            case Basic_cstring: {
                result.kind = X64Operand_m64_target; 
            } break;
            
            default: assert("invalid data target type");
        }
    } else {
        result.kind = X64Operand_m64_target;
    }
    result.jump_target = label;
    return result;
}

inline X64_Operand
x64_build_data_target(X64_Builder* x64, Bc_Label label, X64_Operand_Kind kind) {
    X64_Operand result = {};
    result.kind = kind;
    result.jump_target = label;
    return result;
}

inline X64_Operand
x64_build_stack_offset(X64_Builder* x64, 
                       Bc_Type type, 
                       s64 stack_offset, 
                       X64_Register reg = X64Register_rbp) {
    X64_Operand result = {};
    result.kind = x64_get_memory_kind(type);
    result.reg = reg;
    result.disp64 = stack_offset;
    result.is_allocated = true;
    return result;
}

X64_Operand
x64_build_physical_register(X64_Builder* x64, X64_Register reg, X64_Operand_Kind kind) {
    // TODO(Alexander): we need to tell the register allocate which physical register
    //                  to preallocate for this virtual register.
    X64_Operand result = {};
    result.kind = kind;
    
    // NOTE(Alexander): we don't need to consider RSP and RBP in register allocation
    // since they should never be allocated anyways.
    if (reg == X64Register_rbp || reg == X64Register_rsp) {
        result.reg = reg;
        result.is_allocated = true;
    } else {
        result.virtual_register = x64_allocate_specific_register(x64, reg);
    }
    return result;
}

// NOTE(Alexander): forward declare
X64_Instruction* x64_build_setcc(X64_Builder* x64, Bc_Instruction* cmp);

X64_Operand
x64_build_operand(X64_Builder* x64, Bc_Operand operand, Bc_Type type) {
    X64_Operand result = {};
    
    switch (operand.kind) {
        case BcOperand_Register:
        case BcOperand_Memory: {
            
            smm vreg_index = map_get_index(x64->allocated_virtual_registers, operand.Register);
            if (vreg_index != -1) {
                // TODO(Alexander): this is a hack: currently we need this to store the arguments passed to us
                result = x64->allocated_virtual_registers[vreg_index].value;
                return result;
            } else {
                // Check if our result is from compare instruction
                if (x64->curr_compare_insn) {
                    Bc_Operand cmp_dest = x64->curr_compare_insn->dest;
                    if (cmp_dest.Register == operand.Register) {
                        Bc_Instruction* cmp = x64->curr_compare_insn;
                        x64->curr_compare_insn = 0;
                        X64_Instruction* insn = x64_build_setcc(x64, cmp);
                        insn->op0 = x64_build_operand(x64, cmp_dest, t_bool);
                    }
                }
                
                result.virtual_register = operand.Register;
                result.kind = operand.kind == BcOperand_Register ? 
                    x64_get_register_kind(type) : x64_get_memory_kind(type);
                x64_allocate_virtual_register(x64, 
                                              result.virtual_register,
                                              operand_is_vector_register(result.kind));
                
                bool is_active = false;
                for_array_v(x64->active_virtual_registers, vreg, _) { 
                    if (vreg == result.virtual_register) {
                        is_active = true;
                        break;
                    }
                }
                
                if (!is_active) {
                    array_push(x64->active_virtual_registers, result.virtual_register);
                    
#ifdef DEBUG_X64_REGISTER_ALLOC
                    smm idx = map_get_index(x64->bc_register_live_lengths, result.virtual_register);
                    if (idx == -1) {
                        pln("Warning: bytecode register r% has no live length", f_int(result.virtual_register));
                    }
#endif
                }
            }
        } break;
        
        case BcOperand_Stack: {
            s32 stack_offset = map_get(x64->stack_offsets, operand.Register);
            result = x64_build_stack_offset(x64, type, stack_offset);
        } break;
        
        case BcOperand_Int: {
            assert(type->kind == TypeKind_Basic || type->kind == TypeKind_Pointer);
            if (type->kind == TypeKind_Pointer) {
                result = x64_build_immediate(x64, operand.Signed_Int, Basic_s64); // TODO(Alexander): arch dep!
            } else if (type->kind == TypeKind_Basic) {
                result = x64_build_immediate(x64, operand.Signed_Int, type->Basic.kind);
            }
            
            if (result.kind == X64Operand_imm64) {
                // NOTE(Alexander): we can only move imm64 to register
                X64_Operand temp_reg = x64_allocate_temporary_register(x64, type);
                
                X64_Instruction* tmp_mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                tmp_mov_insn->op0 = temp_reg;
                tmp_mov_insn->op1 = result;
                result = temp_reg;
            }
            
        } break;
        
        case BcOperand_Float: {
            assert(type->kind == TypeKind_Basic);
            assert(type->size != 0 && "bad size");
            assert(type->align != 0 && "bad align");
            
            // TODO(Alexander): this will create new constant for each and every value, even if they are the same
            // Store the float as read-only data
            void* data = arena_push_size(&x64->rodata_section_arena, type->size, type->align);
            u32 offset = (u32) ((umm) data - (umm) x64->rodata_section_arena.base);
            Bc_Label label = create_unique_bc_label(&x64->label_indices,
                                                    Sym___const);
            value_store_in_memory(type, data, operand.Const);
            map_put(x64->rodata_offsets, label, offset);
            
            X64_Opcode mov_opcode = (type == t_f64) ? X64Opcode_movsd : X64Opcode_movss;
            X64_Operand temp_reg = x64_allocate_temporary_register(x64, type);
            X64_Instruction* mov_insn = x64_push_instruction(x64, mov_opcode);
            mov_insn->op0 = temp_reg;
            mov_insn->op1 = x64_build_data_target(x64, label, type);
            result = temp_reg;
        } break;
        
        case BcOperand_Label: {
            result = x64_build_data_target(x64, operand.Label, type);
        } break;
    }
    
    return result;
}

X64_Instruction*
x64_build_compare(X64_Builder* x64, Bc_Instruction* cmp)  {
    // NOTE(Alexander): the dest type is actually the source
    Type* type = cmp->dest_type;
    X64_Opcode opcode = X64Opcode_cmp;
    X64_Operand first = x64_build_operand(x64, cmp->src0, type);
    X64_Operand second = x64_build_operand(x64, cmp->src1, type);
    
    if (type->kind == TypeKind_Basic && 
        is_bitflag_set(type->Basic.flags, BasicFlag_Floating)) {
        opcode = (type == t_f64) ? X64Opcode_ucomisd : X64Opcode_ucomiss;
        
        if (operand_is_memory(first.kind)) {
            // NOTE(Alexander): float comparison doesn't support memory operand on the left-hand side
            X64_Opcode mov_opcode = (type == t_f64) ? X64Opcode_movsd : X64Opcode_movss;
            X64_Operand temp_reg = x64_allocate_temporary_register(x64, type);
            X64_Instruction* mov_insn = x64_push_instruction(x64, mov_opcode);
            mov_insn->op0 = temp_reg;
            mov_insn->op1 = first;
            first = temp_reg;
        }
    } else {
        if (operand_is_immediate(first.kind)) {
            X64_Operand temp_reg = x64_allocate_temporary_register(x64, type);
            X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
            mov_insn->op0 = temp_reg;
            mov_insn->op1 = first;
            first = temp_reg;
        }
    }
    
    X64_Instruction* insn = x64_push_instruction(x64, opcode);
    insn->op0 = first;
    insn->op1 = second;
    return insn;
    
}

X64_Opcode
x64_build_jump_opcode(X64_Builder* x64, Bc_Opcode opcode, Type* type) {
    X64_Opcode result = X64Opcode_noop;
    
    if (type->kind == TypeKind_Basic && 
        is_bitflag_set(type->Basic.flags, BasicFlag_Floating)) {
        switch (opcode) {
            case Bytecode_cmpeq:  result = X64Opcode_je;  break;
            case Bytecode_cmpneq: result = X64Opcode_jne; break;
            case Bytecode_cmple:  result = X64Opcode_jbe; break;
            case Bytecode_cmplt:  result = X64Opcode_jb;  break;
            case Bytecode_cmpge:  result = X64Opcode_jae; break;
            case Bytecode_cmpgt:  result = X64Opcode_ja;  break;
            default: assert(0 && "invalid opcode");
        }
    } else {
        switch (opcode) {
            case Bytecode_cmpeq:  result = X64Opcode_je;  break;
            case Bytecode_cmpneq: result = X64Opcode_jne; break;
            case Bytecode_cmple:  result = X64Opcode_jle; break;
            case Bytecode_cmplt:  result = X64Opcode_jl;  break;
            case Bytecode_cmpge:  result = X64Opcode_jge; break;
            case Bytecode_cmpgt:  result = X64Opcode_jg;  break;
            default: assert(0 && "invalid opcode");
        }
    }
    
    return result;
}


X64_Instruction*
x64_build_setcc(X64_Builder* x64, Bc_Instruction* cmp) {
    assert(cmp->dest.kind == BcOperand_Register);
    
    x64_build_compare(x64, cmp);
    X64_Opcode set_opcode = X64Opcode_noop;
    
    Type* type = cmp->dest_type;
    if (type->kind == TypeKind_Basic && 
        is_bitflag_set(type->Basic.flags, BasicFlag_Floating)) {
        switch (cmp->opcode) {
            case Bytecode_cmpeq:  set_opcode = X64Opcode_sete;  break;
            case Bytecode_cmpneq: set_opcode = X64Opcode_setne; break;
            case Bytecode_cmple:  set_opcode = X64Opcode_setbe; break;
            case Bytecode_cmplt:  set_opcode = X64Opcode_setb;  break;
            case Bytecode_cmpge:  set_opcode = X64Opcode_setae; break;
            case Bytecode_cmpgt:  set_opcode = X64Opcode_seta;  break;
            default: assert(0 && "invalid opcode");
        }
    } else {
        switch (cmp->opcode) {
            case Bytecode_cmpeq:  set_opcode = X64Opcode_sete;  break;
            case Bytecode_cmpneq: set_opcode = X64Opcode_setne; break;
            case Bytecode_cmple:  set_opcode = X64Opcode_setle; break;
            case Bytecode_cmplt:  set_opcode = X64Opcode_setl;  break;
            case Bytecode_cmpge:  set_opcode = X64Opcode_setge; break;
            case Bytecode_cmpgt:  set_opcode = X64Opcode_setg;  break;
            default: assert(0 && "invalid opcode");
        }
    }
    
    return x64_push_instruction(x64, set_opcode);
}

void
x64_build_instruction_from_bytecode(X64_Builder* x64, Bc_Instruction* bc) {
    switch (bc->opcode) {
        case Bytecode_stack_alloc: break;
        case Bytecode_label: break;
        case Bytecode_noop: break;
        
        case Bytecode_copy: {
            X64_Operand src = x64_build_operand(x64, bc->src0, bc->dest_type);
            
            X64_Opcode mov_opcode = X64Opcode_mov;
            if (bc->dest_type == t_f32) {
                mov_opcode = X64Opcode_movss;
            } else if (bc->dest_type == t_f64) {
                mov_opcode = X64Opcode_movsd;
            }
            
            X64_Instruction* insn = x64_push_instruction(x64, mov_opcode);
            insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
            insn->op1 = src;
            
            if (bc->dest.kind == BcOperand_Memory) {
                insn->op0.kind = x64_get_memory_kind(bc->dest_type);
            }
        } break;
        
        
        case Bytecode_memcpy: {
            // TODO(Alexander): right now we store it on the stack which 
            // grows downwards, maybe we want to always do this?
            //int dest_dir = bc->dest.kind == BcOperand_Stack ? -1 : 1;
            //int src_dir = bc->src0.kind  == BcOperand_Stack ? -1 : 1;
            int dest_dir = -1;
            int src_dir = -1;
            
            X64_Operand dest;
            if (x64->first_arg_as_return && 
                bc->dest.Register == x64->return_value_register) {
                X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                mov_insn->op0 = x64_allocate_temporary_register(x64, t_s64);
                mov_insn->op1 = x64_build_operand(x64, bc->dest, t_s64);
                dest = mov_insn->op0;
                dest.kind = X64Operand_m64;
                //dest_dir = -1;
                //src_dir = 1;
            } else {
                dest = x64_build_operand(x64, bc->dest, t_s64);
            }
            
            X64_Operand src = x64_build_operand(x64, bc->src0, t_s64);
            s64 size = bc->src1.Signed_Int;
            
            // TODO(Alexander): we should switch rep stob instead of copying dwords, alignment problem
            s64 offset = 0;
            while (offset < size) {
                s64 remaining = size - offset;
                Type* data_type = t_s32;
                if (remaining == 1) {
                    data_type = t_s8;
                } else if (remaining == 2) {
                    data_type = t_s16;
                } else if (remaining == 4) {
                    data_type = t_s32;
                }
                
                X64_Operand temp_reg = x64_allocate_temporary_register(x64, data_type);
                X64_Operand_Kind memory_kind = x64_get_memory_kind(data_type);
                
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
                insn->op0 = temp_reg;
                insn->op1 = src;
                insn->op1.kind = memory_kind;
                insn->op1.disp64 += offset*src_dir;
                
                insn = x64_push_instruction(x64, X64Opcode_mov);
                insn->op0 = dest;
                insn->op0.kind = memory_kind;
                insn->op0.disp64 += offset*dest_dir;
                insn->op1 = temp_reg;
                
                x64_free_virtual_register(x64, temp_reg.virtual_register);
                
                offset += 4;
            }
        } break;
        
        case Bytecode_load: {
            // mov op0, [op1]
            X64_Operand src = x64_build_operand(x64, bc->src0, bc->dest_type);
            
            if (operand_is_memory(src.kind)) {
                // NOTE(Alexander): first load the pointer that we want to dereference
                Type ptr_type = {};
                ptr_type.kind = TypeKind_Pointer;
                ptr_type.Pointer = bc->dest_type;
                
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
                insn->op0 = x64_allocate_temporary_register(x64, &ptr_type);
                insn->op1 = src;
                src = insn->op0;
            }
            
            
            X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
            insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
            insn->op1 = src;
            insn->op1.kind = x64_get_memory_kind(bc->dest_type);
            
        } break;
        
        case Bytecode_load_address: {
            X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_lea);
            insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
            insn->op1 = x64_build_operand(x64, bc->src0, bc->dest_type);
            
        } break;
        
        case Bytecode_store: {
            // mov [op0], op1
            Type ptr_type = {};
            ptr_type.kind = TypeKind_Pointer;
            ptr_type.Pointer = bc->dest_type;
            
            X64_Operand dest = x64_build_operand(x64, bc->dest, &ptr_type);
            
            if (operand_is_memory(dest.kind)) {
                // NOTE(Alexander): first load the pointer that we want to dereference
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
                insn->op0 = x64_allocate_temporary_register(x64, &ptr_type);
                insn->op1 = dest;
                dest = insn->op0;
            }
            
            X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
            insn->op0 = dest;
            insn->op0.kind = x64_get_memory_kind(bc->dest_type);
            insn->op1 = x64_build_operand(x64, bc->src0, bc->dest_type);
        } break;
        
        case Bytecode_field: {
            X64_Operand dest = x64_build_operand(x64, bc->dest, bc->dest_type); // used later
            X64_Operand src = x64_build_operand(x64, bc->src0, bc->dest_type);
            s64 offset = bc->src1.Signed_Int;
            //int src_dir = bc->src0.kind == BcOperand_Stack ? -1 : 1;
            //src.disp64 -= offset*src_dir;
            src.disp64 -= offset;
            src.kind = dest.kind;
            map_put(x64->allocated_virtual_registers, bc->dest.Register, src);
            
            if (bc->src0.kind == BcOperand_Memory) {
                // TODO(Alexander): expend the length of src, since we are using it later on
                u64 extended_length = map_get(x64->bc_register_live_lengths, bc->dest.Register);
                map_put(x64->bc_register_live_lengths, bc->src0.Register, extended_length);
            }
        } break;
        
        case Bytecode_index: {
            Type* type = bc->dest_type;
            X64_Operand dest = x64_build_operand(x64, bc->dest, bc->dest_type);
            
            // TODO(Alexander): using SIB byte we can probably improve this!
            X64_Operand index = x64_build_operand(x64, bc->src1, t_s32);
            if (operand_is_immediate(index.kind)) {
                assert(bc->src0.kind == BcOperand_Stack);
                
                X64_Operand result = x64_build_operand(x64, bc->src0, bc->dest_type);
                result.disp64 -= type->size * index.imm64;
                map_put(x64->allocated_virtual_registers, bc->dest.Register, result);
            } else {
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_imul);
                insn->op0 = index;
                insn->op1 = x64_build_immediate(x64, type->size, Basic_s32);
            }
        } break;
        
        case Bytecode_neg: {
            Type* type = bc->dest_type;
            if (type->kind == TypeKind_Basic &&
                is_bitflag_set(type->Basic.flags, BasicFlag_Floating)) {
                
                // Use xor to set the sign bit of the floating point value
                void* data = arena_push_size(&x64->rodata_section_arena, 16, 16);
                u32 offset = (u32) ((umm) data - (umm) x64->rodata_section_arena.base);
                Bc_Label label = create_unique_bc_label(&x64->label_indices, Sym___const);
                map_put(x64->rodata_offsets, label, offset);
                if (type == t_f64) {
                    u64* values = (u64*) data;
                    for (int i = 0; i < 2; i++) *values++ = 0x8000000000000000ull;
                } else {
                    u32* values = (u32*) data;
                    for (int i = 0; i < 4; i++) *values++ = 0x80000000;
                }
                
                X64_Operand dest = x64_build_operand(x64, bc->dest, bc->dest_type); 
                X64_Operand src = x64_build_operand(x64, bc->src0, bc->dest_type); 
                
                X64_Opcode mov_opcode = (type == t_f64) ? X64Opcode_movsd : X64Opcode_movss;
                X64_Instruction* mov_insn = x64_push_instruction(x64, mov_opcode); 
                mov_insn->op0 = dest;
                mov_insn->op1 = src;
                
                X64_Opcode xor_opcode = (type == t_f64) ? X64Opcode_xorpd : X64Opcode_xorps;
                X64_Instruction* neg_insn = x64_push_instruction(x64, xor_opcode);
                neg_insn->op0 = mov_insn->op0;
                neg_insn->op1 = x64_build_data_target(x64, label, X64Operand_m128_target);
                
            } else {
                
                X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov); 
                mov_insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type); 
                mov_insn->op1 = x64_build_operand(x64, bc->src0, bc->dest_type);
                
                X64_Instruction* neg_insn = x64_push_instruction(x64, X64Opcode_neg);
                neg_insn->op0 = mov_insn->op0;
            }
        } break;
        
        case Bytecode_not: {
            X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov); 
            mov_insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type); 
            mov_insn->op1 = x64_build_operand(x64, bc->src0, bc->dest_type);
            
            X64_Instruction* xor_insn = x64_push_instruction(x64, X64Opcode_xor);
            xor_insn->op0 = mov_insn->op0;
            assert(bc->dest_type->kind == TypeKind_Basic);
            xor_insn->op1 = x64_build_immediate(x64, -1, bc->dest_type->Basic.kind);
            
            X64_Instruction* and_insn = x64_push_instruction(x64, X64Opcode_and);
            and_insn->op0 = xor_insn->op0;
            and_insn->op1 = x64_build_immediate(x64, 1, bc->dest_type->Basic.kind);
        } break;
        
#define BINARY_CASE(opcode) \
X64_Operand dest = x64_build_operand(x64, bc->dest, bc->dest_type); \
X64_Operand src0 = x64_build_operand(x64, bc->src0, bc->dest_type); \
X64_Operand src1 = x64_build_operand(x64, bc->src1, bc->dest_type); \
X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov); \
mov_insn->op0 = dest; \
mov_insn->op1 = src0; \
        \
X64_Instruction* op_insn = x64_push_instruction(x64, opcode); \
op_insn->op0 = dest; \
op_insn->op1 = src1;
        
        case Bytecode_add: {
            BINARY_CASE(X64Opcode_add);
        } break;
        
        case Bytecode_sub: {
            BINARY_CASE(X64Opcode_sub);
        } break;
        
        case Bytecode_and: {
            BINARY_CASE(X64Opcode_and);
        } break;
        
        case Bytecode_or: {
            BINARY_CASE(X64Opcode_or);
        } break;
        
        case Bytecode_mul: {
            if (is_bc_operand_value(bc->src0.kind) || is_bc_operand_value(bc->src1.kind)) {
                X64_Operand_Kind operand_kind = x64_get_register_kind(bc->dest_type);
                X64_Opcode mul_opcode = X64Opcode_imul; // TODO(Alexander): check signedness
                
                Bc_Operand reg;
                Bc_Operand imm;
                if (is_bc_operand_value(bc->src1.kind)) {
                    reg = bc->src0;
                    imm = bc->src1;
                } else {
                    reg = bc->src1;
                    imm = bc->src0;
                }
                
                if (is_bc_operand_value(reg.kind)) {
                    X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                    mov_insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
                    mov_insn->op1 = x64_build_operand(x64, reg, bc->dest_type);
                    reg = bc->dest;
                }
                
                X64_Instruction* mul_insn = x64_push_instruction(x64, mul_opcode);
                mul_insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
                mul_insn->op1 = x64_build_operand(x64, reg, bc->dest_type);
                mul_insn->op2 = x64_build_operand(x64, imm, bc->dest_type);
                
            } else {
                BINARY_CASE(X64Opcode_imul); // TODO(Alexander): check signedness
            }
        } break;
        
        case Bytecode_div:
        case Bytecode_mod: {
            X64_Operand_Kind operand_kind = x64_get_register_kind(bc->dest_type);
            X64_Opcode div_opcode = X64Opcode_idiv; // TODO(Alexander): check signedness
            
            // RAX is used as input and RDX:RAX is used as output
            u64 rax = x64_allocate_specific_register(x64, X64Register_rax);
            u64 rdx = x64_allocate_specific_register(x64, X64Register_rdx);
            
            X64_Operand rax_op = {};
            rax_op.kind = operand_kind;
            rax_op.virtual_register = rax;
            
            // Move source1 into RAX
            X64_Instruction* mov_rax_insn = x64_push_instruction(x64, X64Opcode_mov);
            mov_rax_insn->op0 = rax_op;
            mov_rax_insn->op1 = x64_build_operand(x64, bc->src0, bc->dest_type);
            // TODO(Alexander): are we sure this will become a register?
            
            // Convert RAX into RDX:RAX
            // TODO(Alexander): check type to pick one of cwd, cdq, cqo
            switch (bc->dest_type->size) {
                case 8: x64_push_instruction(x64, X64Opcode_cqo); break;
                case 4: x64_push_instruction(x64, X64Opcode_cdq); break;
                case 2: x64_push_instruction(x64, X64Opcode_cwd); break;
                default: assert(0 && "invalid type for div or mod")
            }
            
            X64_Operand divisor = x64_build_operand(x64, bc->src1, bc->dest_type);
            if (operand_is_immediate(divisor.kind)) {
                X64_Operand temp_reg = x64_allocate_temporary_register(x64, bc->dest_type);
                X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                mov_insn->op0 = temp_reg;
                mov_insn->op1 = divisor;
                
                divisor = temp_reg;
            }
            
            // Perform division
            X64_Instruction* div_insn = x64_push_instruction(x64, div_opcode);
            div_insn->op0 = divisor;
            
            // Store the result back into dest
            X64_Instruction* mov_dest_insn = x64_push_instruction(x64, X64Opcode_mov);
            mov_dest_insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
            if (bc->opcode == Bytecode_div) {
                mov_dest_insn->op1 = rax_op;
            } else {
                X64_Operand rdx_op = {};
                rdx_op.kind = operand_kind;
                rdx_op.virtual_register = rdx;
                
                mov_dest_insn->op1 = rdx_op;
            }
            
            x64_free_virtual_register(x64, rax);
            x64_free_virtual_register(x64, rdx);
        } break;
        
#define BINARY_FLOAT_CASE(op_f32, op_f64) \
Type* type = bc->dest_type; \
X64_Opcode mov_opcode = (type == t_f64) ? X64Opcode_movsd : X64Opcode_movss; \
X64_Operand dest = x64_build_operand(x64, bc->dest, bc->dest_type); \
X64_Operand src0 = x64_build_operand(x64, bc->src0, bc->dest_type); \
X64_Operand src1 = x64_build_operand(x64, bc->src1, bc->dest_type); \
X64_Instruction* mov_insn = x64_push_instruction(x64, mov_opcode); \
mov_insn->op0 = dest; \
mov_insn->op1 = src0; \
        \
X64_Opcode opcode = (type == t_f64) ? op_f64 : op_f32; \
X64_Instruction* op_insn = x64_push_instruction(x64, opcode); \
op_insn->op0 = dest; \
op_insn->op1 = src1;
        
        case Bytecode_fadd: {
            BINARY_FLOAT_CASE(X64Opcode_addss, X64Opcode_addsd);
        } break;
        
        case Bytecode_fsub: {
            BINARY_FLOAT_CASE(X64Opcode_subss, X64Opcode_subsd);
        } break;
        
        case Bytecode_fmul: {
            BINARY_FLOAT_CASE(X64Opcode_mulss, X64Opcode_mulsd);
        } break;
        
        case Bytecode_fdiv: {
            BINARY_FLOAT_CASE(X64Opcode_divss, X64Opcode_divsd);
        } break;
        
        case Bytecode_cmpeq:
        case Bytecode_cmpneq:
        case Bytecode_cmple:
        case Bytecode_cmplt:
        case Bytecode_cmpge:
        case Bytecode_cmpgt:{
            x64->curr_compare_insn = bc;
        } break;
        
        case Bytecode_goto: {
            Bc_Label jump_label = bc->dest.Label;
            Bc_Basic_Block* next_block = get_bc_basic_block(x64->bytecode, x64->curr_bc_basic_block->next_byte_offset);
            
            if (next_block->label.index != jump_label.index) {
                assert(jump_label.ident != Kw_invalid);
                X64_Instruction* jump_insn = x64_push_instruction(x64, X64Opcode_jmp);
                jump_insn->op0 = x64_build_jump_target(x64, jump_label);
            }
        } break;
        
        case Bytecode_branch: {
            
            X64_Opcode jump_opcode = X64Opcode_jmp;
            Bc_Label jump_label = {};
            
            Bc_Instruction* cmp = x64->curr_compare_insn;
            if (cmp) {
                // NOTE(Alexander): the dest type is actually the source
                x64_build_compare(x64, cmp);
                jump_opcode = x64_build_jump_opcode(x64, cmp->opcode, cmp->dest_type);
            } else {
                X64_Instruction* test_insn = x64_push_instruction(x64, X64Opcode_cmp);
                test_insn->op0 = x64_build_operand(x64, bc->dest, t_bool);
                test_insn->op1 = x64_build_immediate(x64, 1, Basic_bool);
                jump_opcode = X64Opcode_je;
            }
            
            assert(bc->src0.kind == BcOperand_Label);
            assert(bc->src1.kind == BcOperand_Label);
            Bc_Label then_label = bc->src0.Label;
            Bc_Label else_label = bc->src1.Label;
            assert(then_label.ident == else_label.ident && "branches can't jump to another declaration");
            
            Bc_Basic_Block* next_block = get_bc_basic_block(x64->bytecode, x64->curr_bc_basic_block->next_byte_offset);
            assert(next_block->label.index == then_label.index ||
                   next_block->label.index == else_label.index && 
                   "assumes one block is next to this one");
            
            if (then_label.index > else_label.index) {
                jump_label = then_label;
            } else {
                // swap the condition and branch to else block instead
                jump_label = else_label;
                jump_opcode = x64_opcode_invert_jump_condition(jump_opcode);
            }
            
            x64->curr_compare_insn = 0;
            
            
            if (jump_label.ident != Kw_invalid) {
                X64_Instruction* jump_insn = x64_push_instruction(x64, jump_opcode);
                jump_insn->op0 = x64_build_jump_target(x64, jump_label);
            }
        } break;
        
        case Bytecode_truncate: {
            s32 dest_size = bc_type_to_size(bc->dest_type);
            s32 src_size = bc_type_to_size(bc->src1.Type);
            
            assert(dest_size <= src_size);
            
            X64_Operand src = x64_build_operand(x64, bc->src0, bc->dest_type);
            if (operand_is_register(src.kind)){ 
                if (bc->dest.kind == BcOperand_Register) {
                    map_put(x64->allocated_virtual_registers, bc->dest.Register, src);
                }
            } else {
                X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                mov_insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
                mov_insn->op1 = src;
            }
        } break;
        
        case Bytecode_sign_extend: {
            s32 dest_size = bc_type_to_size(bc->dest_type);
            s32 src_size = bc_type_to_size(bc->src1.Type);
            assert(dest_size >= src_size);
            
            if (dest_size != src_size) {
                X64_Operand source_operand = x64_build_operand(x64, bc->src0, bc->src1.Type);
                
                // TODO(Alexander): is this needed anymore, check if build_operand do this?
                if (x64->curr_compare_insn && bc->src1.Type == t_bool) {
                    Bc_Instruction* cmp = x64->curr_compare_insn;
                    X64_Instruction* insn = x64_build_setcc(x64, cmp);
                    
                    X64_Operand temp_reg = x64_allocate_temporary_register(x64, bc->dest_type);
                    insn->op0 = temp_reg;
                    x64->curr_compare_insn = 0;
                    
                    source_operand = temp_reg;
                }
                
                X64_Opcode opcode = (src_size < 4) ? X64Opcode_movsx : X64Opcode_movsxd;
                X64_Instruction* mov_insn = x64_push_instruction(x64, opcode);
                mov_insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
                mov_insn->op1 = source_operand;
            } else {
                X64_Operand dest = x64_build_operand(x64, bc->src0, bc->dest_type);
                if (bc->dest.kind == BcOperand_Register) {
                    map_put(x64->allocated_virtual_registers, bc->dest.Register, dest);
                }
            }
        } break;
        
        case Bytecode_zero_extend: {
            s32 dest_size = bc_type_to_size(bc->dest_type);
            s32 src_size = bc_type_to_size(bc->src1.Type);
            assert(dest_size >= src_size);
            
            if (dest_size != src_size) {
                X64_Operand src = x64_build_operand(x64, bc->src0, bc->dest_type);
                
                if (src_size < 4) {
                    X64_Instruction* movzx_insn = x64_push_instruction(x64, X64Opcode_movzx);
                    movzx_insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
                    movzx_insn->op1 = src;
                }
                
                X64_Operand dest = x64_build_operand(x64, bc->dest, bc->dest_type);
                if (bc->dest.kind == BcOperand_Register) {
                    map_put(x64->allocated_virtual_registers, bc->dest.Register, dest);
                }
            } else {
                X64_Operand dest = x64_build_operand(x64, bc->src0, bc->dest_type);
                if (bc->dest.kind == BcOperand_Register) {
                    map_put(x64->allocated_virtual_registers, bc->dest.Register, dest);
                }
            }
        } break;
        
        case Bytecode_float_to_sint: {
            // TODO(Alexander): cpuid feature flag SSE2
            X64_Opcode opcode = (bc->src1.Type == t_f64) ? 
                X64Opcode_cvttsd2si : X64Opcode_cvttss2si;
            
            X64_Instruction* insn = x64_push_instruction(x64, opcode);
            insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
            insn->op1 = x64_build_operand(x64, bc->src0, bc->src1.Type);
        } break;
        
        case Bytecode_float_to_uint: {
            unimplemented;
        } break;
        
        case Bytecode_sint_to_float: {
            // TODO(Alexander): cpuid feature flag SSE2
            X64_Opcode opcode = (bc->dest_type == t_f64) ? 
                X64Opcode_cvtsi2sd : X64Opcode_cvtsi2ss;
            
            X64_Instruction* insn = x64_push_instruction(x64, opcode);
            insn->op0 = x64_build_operand(x64, bc->dest, bc->dest_type);
            insn->op1 = x64_build_operand(x64, bc->src0, bc->src1.Type);
        } break;
        
        case Bytecode_uint_to_float: {
            unimplemented;
        } break;
        
        case Bytecode_float_extend: {
            unimplemented;
        } break;
        
        case Bytecode_float_truncate: {
            unimplemented;
        } break;
        
        case Bytecode_call: {
            // Set the target to jump to
            assert(bc->src0.Type->kind == TypeKind_Function);
            Type* function_type = bc->src0.Type;
            
            // TODO(Alexander): uses the x64 microsoft calling convention
            // https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention
            const X64_Register int_argument_registers[] = {
                X64Register_rcx, X64Register_rdx, X64Register_r8, X64Register_r9
            };
            const X64_Register vector_argument_registers[] = {
                X64Register_xmm0, X64Register_xmm1, X64Register_xmm2, X64Register_xmm3
            };
            assert(fixed_array_count(int_argument_registers) == 
                   fixed_array_count(vector_argument_registers));
            
            array(X64_Argument)* arguments = 0;
            
            // Return type
            Type* return_type = function_type->Function.return_type;
            bool use_first_reg_as_return = false;
            if (return_type->size > 8) {
                // TODO(Alexander): types that cannot fit in RAX, requires that the caller
                // allocates space for the return value and passes reference to it
                X64_Argument return_arg = {};
                return_arg.type = bc->dest_type;
                return_arg.src = x64_build_stack_offset(x64, 
                                                        bc->dest_type, 
                                                        -x64->return_value_stack_offset);
                use_first_reg_as_return = true;
                array_push(arguments, return_arg);
            }
            
            // Allocate registers on stack where rcx, rdx, r8, r9 setup by callee
            s32 stack_offset = 4*8;
            
            for (int arg_index = 0; 
                 arg_index < array_count(bc->src1.Argument_List);
                 ++arg_index) {
                
                Bc_Argument* arg = bc->src1.Argument_List + arg_index;
                X64_Operand src = x64_build_operand(x64, arg->src, arg->type);
                
                X64_Argument x64_arg = {};
                x64_arg.src = src;
                x64_arg.type = arg->type;
                array_push(arguments, x64_arg);
            }
            
            for (int arg_index = 0; 
                 arg_index < array_count(arguments);
                 ++arg_index) {
                
                if (arg_index >= fixed_array_count(int_argument_registers)) {
                    break;
                }
                
                X64_Argument* arg = arguments + arg_index;
                bool use_vector_register = (arg->type->kind == TypeKind_Basic && 
                                            is_bitflag_set(arg->type->Basic.flags, BasicFlag_Floating));
                u64 vreg;
                if (use_vector_register) {
                    vreg = x64_allocate_specific_register(x64, vector_argument_registers[arg_index]);
                    arg->vector_vreg = vreg;
                } else {
                    vreg = x64_allocate_specific_register(x64, int_argument_registers[arg_index]);
                    arg->vreg = vreg;
                }
                
                X64_Instruction* insn;
                s32 size = bc_type_to_size(arg->type);
                assert(size > 0 && "bad size");
                if (size == 1 || size == 2 || size == 4 || size == 8) {
                    X64_Opcode mov_opcode = X64Opcode_mov;
                    if (use_vector_register) {
                        mov_opcode = (arg->type == t_f64) ? X64Opcode_movsd : X64Opcode_movss;
                    }
                    insn = x64_push_instruction(x64, mov_opcode);
                    insn->op0.kind = x64_get_register_kind(arg->type);
                } else {
                    assert(!use_vector_register);
                    
                    // Pass argument by reference
                    X64_Opcode opcode = X64Opcode_lea;
                    if (operand_is_register(arg->src.kind)) {
                        opcode = X64Opcode_mov;
                    }
                    insn = x64_push_instruction(x64, opcode);
                    insn->op0.kind = x64_get_register_kind(t_s64); // TODO: arch dep size of type
                }
                insn->op0.virtual_register = vreg;
                insn->op1 = arg->src;
                
                
                if (use_vector_register) {
                    // TODO(Alexander): a quirk with varargs and unprototyped functions, dup xmm to int
                    u64 int_vreg = x64_allocate_specific_register(x64, int_argument_registers[arg_index]);
                    arg->vreg = int_vreg;
                    
                    X64_Opcode opcode = (arg->type == t_f64) ? X64Opcode_movq : X64Opcode_movd;
                    X64_Instruction* dup_insn = x64_push_instruction(x64, opcode);
                    dup_insn->op0.kind = x64_get_register_kind(arg->type == t_f64 ? t_s64 : t_s32);
                    dup_insn->op0.virtual_register = int_vreg;
                    dup_insn->op1 = insn->op0;
                }
            }
            
            for (int arg_index = (int) 4;
                 arg_index < array_count(arguments);
                 ++arg_index) {
                
                X64_Argument* arg = arguments + arg_index;
                
                s32 size = bc_type_to_size(arg->type);
                if (!(size == 1 || size == 2 || size == 4 || size == 8)) {
                    // Pass argument by reference
                    X64_Instruction* tmp_insn = x64_push_instruction(x64, X64Opcode_lea);
                    tmp_insn->op0 = x64_allocate_temporary_register(x64, t_s64); // TODO: arch dep size of type
                    tmp_insn->op1 = arg->src;
                    arg->src = tmp_insn->op0;
                    // TODO(Alexander): I think, windows calling convention always align types by at least 8 bytes
                    size = 8;
                }
                
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
                insn->op0 = x64_build_stack_offset(x64, arg->type, stack_offset, X64Register_rsp);
                insn->op1 = arg->src;
                stack_offset += 8;
            }
            
            if (function_type->Function.intrinsic) {
                
                if (function_type->Function.intrinsic == &interp_intrinsic_debug_break) {
                    if (x64->is_debugger_present) {
                        x64_push_instruction(x64, X64Opcode_int3);
                    }
                    
                } else {
                    // TODO(Alexander): r64 hardcoded 
                    X64_Operand_Kind call_address = X64Operand_r64;
                    
                    X64_Instruction* mov_imm64_insn = x64_push_instruction(x64, X64Opcode_mov);
                    mov_imm64_insn->op0 = x64_allocate_temporary_register(x64, call_address);
                    mov_imm64_insn->op1.kind = X64Operand_imm64;
                    mov_imm64_insn->op1.imm64 = (smm) function_type->Function.intrinsic;
                    
                    X64_Instruction* call_insn = x64_push_instruction(x64, X64Opcode_call);
                    call_insn->op0.kind = call_address;
                    call_insn->op0 = mov_imm64_insn->op0;
                    
                    x64_free_virtual_register(x64, mov_imm64_insn->op0.virtual_register);
                }
            } else {
                X64_Instruction* call_insn = x64_push_instruction(x64, X64Opcode_call);
                call_insn->op0.kind = X64Operand_jump_target;
                // TODO(Alexander): for function overloading we need to detect which one to use
                call_insn->op0.jump_target = { function_type->Function.ident, 0 };
            }
            
            // Free allocated registers in order
            // TODO(Alexander): should we auto clean these by using temporary register?
            for (int arg_index = 0; arg_index < array_count(arguments); arg_index++) {
                X64_Argument* arg = arguments + arg_index;
                if (arg->vreg) {
                    x64_free_virtual_register(x64, arg->vreg);
                }
                if (arg->vector_vreg) {
                    x64_free_virtual_register(x64, arg->vector_vreg);
                }
            }
            
            const X64_Register volatile_registers[] = {
                X64Register_rax, X64Register_rcx, X64Register_rdx, X64Register_r8, 
                X64Register_r9, X64Register_r10, X64Register_r11, X64Register_xmm0,
                X64Register_xmm1, X64Register_xmm2, X64Register_xmm3, X64Register_xmm4,
                X64Register_xmm5
            };
            
            // Flush volatile registers to make sure it won't be used after this call
            for (int i = 0; i < fixed_array_count(volatile_registers); i++) {
                x64_flush_register(x64, volatile_registers[i]);
            }
            
            const X64_Register non_volatile_registers[] = {
                X64Register_rbx, X64Register_rbp, X64Register_rdi, X64Register_rsi,
                X64Register_rsp, X64Register_r12, X64Register_r13, X64Register_r14,
                X64Register_r15, X64Register_xmm6, X64Register_xmm7, X64Register_xmm8,
                X64Register_xmm9, X64Register_xmm10, X64Register_xmm11, X64Register_xmm12,
                X64Register_xmm13, X64Register_xmm14, X64Register_xmm15
            };
            
            // Store the return value
            if (function_type->Function.return_type->kind != TypeKind_Void) {
                if (use_first_reg_as_return) {
                    X64_Operand dest = array_first(arguments)->src;
                    map_put(x64->allocated_virtual_registers, bc->dest.Register, dest);
                } else {
                    X64_Operand dest = {};
                    dest.kind = x64_get_register_kind(bc->dest_type);
                    dest.is_allocated = true;
                    if (return_type->kind == TypeKind_Basic &&
                        is_bitflag_set(return_type->Basic.flags, BasicFlag_Floating)) {
                        dest.reg = X64Register_xmm0;
                    } else {
                        dest.reg = X64Register_rax;
                    }
                    
                    x64_allocate_specific_register(x64, dest.reg, bc->dest.Register);
                    map_put(x64->allocated_virtual_registers, bc->dest.Register, dest);
                }
            }
            
            array_free(arguments);
        } break;
        
        case Bytecode_ret: {
            if (bc->dest.kind != BcOperand_None) {
                Type* dest_type = bc->dest_type;
                
                X64_Opcode mov_opcode = X64Opcode_mov;
                if (dest_type->kind == TypeKind_Basic &&
                    is_bitflag_set(dest_type->Basic.flags, BasicFlag_Floating)) {
                    mov_opcode = (dest_type == t_f64) ? X64Opcode_movsd : X64Opcode_movss;
                }
                X64_Instruction* mov_insn = x64_push_instruction(x64, mov_opcode);
                X64_Operand_Kind operand_kind = x64_get_register_kind(bc->dest_type);
                mov_insn->op0 = x64_build_physical_register(x64, X64Register_rax, operand_kind);
                mov_insn->op1 = x64_build_operand(x64, bc->dest, bc->dest_type);
            }
        } break;
    }
}

void
x64_text_push_prologue(X64_Builder* x64, s32 stack_frame_size) {
    // push rbp
    // mov rbp rsp
    // sub rbp stack_size (only relevant for non-leaf functions)
    x64->stack_frame_size = stack_frame_size;
    
    X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_push);
    insn->op0 = x64_build_physical_register(x64, X64Register_rbp, X64Operand_r64);
    
    insn = x64_push_instruction(x64, X64Opcode_mov);
    insn->op0 = x64_build_physical_register(x64, X64Register_rbp, X64Operand_r64);
    insn->op1 = x64_build_physical_register(x64, X64Register_rsp, X64Operand_r64);
    
    
    insn = x64_push_instruction(x64, X64Opcode_sub);
    insn->op0 = x64_build_physical_register(x64, X64Register_rsp, X64Operand_r64);
    insn->op1.imm32 = stack_frame_size;
    insn->op1.kind = X64Operand_imm32;
}


void
x64_text_push_epilogue(X64_Builder* x64) {
    // add rsp stack_size (only relevant for non-leaf functions)
    // pop rbp
    // ret
    
    X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_add);
    insn->op0 = x64_build_physical_register(x64, X64Register_rsp, X64Operand_r64);
    insn->op1.imm32 = x64->stack_frame_size;
    insn->op1.kind = X64Operand_imm32;
    
    insn = x64_push_instruction(x64, X64Opcode_pop);
    insn->op0 = x64_build_physical_register(x64, X64Register_rbp, X64Operand_r64);
    
    x64_push_instruction(x64, X64Opcode_ret);
    //pln("bb.count = %", f_umm(x64->curr_basic_block->count));
}

struct Stack_Info {
    s32 local_size;
    s32 argument_size;
    s32 return_value_size;
    b32 first_arg_as_return;
};

Stack_Info
x64_analyze_stack(X64_Builder* x64, Bc_Basic_Block* function_block, Bc_Procedure_Info* proc) {
    Stack_Info result = {};
    
    Bc_Basic_Block* it_block = function_block;
    smm it_index = 0;
    while (it_block) {
        while (it_index < it_block->instruction_count) {
            Bc_Instruction* bc = get_first_bc_instruction(it_block) + it_index;
            
            switch (bc->opcode) {
                case Bytecode_stack_alloc: {
                    assert(bc->dest.kind == BcOperand_Stack);
                    assert(bc->src0.kind == BcOperand_Type);
                    assert(bc->src1.kind == BcOperand_None);
                    
                    s32 size = bc_type_to_size(bc->src0.Type);
                    s32 align = bc_type_to_align(bc->src0.Type);
                    assert(size > 0 && "bad size");
                    assert(align > 0 && "bad align");
                    
                    if (bc->dest.Register == proc->first_return_reg && size > 8) {
                        // TODO(Alexander): hack to fix return through first argument
                        // in the windows x64 callign convention.
                        // We don't need to allocate the entire data, the caller is responsible
                        // for allocating the reutrn value, RCX will hold a pointer
                        result.first_arg_as_return = true;
                        size = 8; // TODO(Alexander): arch dep!
                        align = 8;
                    }
                    
                    s32 stack_offset = (s32) align_forward((umm) result.local_size, align);
                    // NOTE(Alexander): we use negative because stack grows downwards
                    map_put(x64->stack_offsets, bc->dest.Register, -(stack_offset + align));
                    
                    stack_offset += size;
                    result.local_size = stack_offset;
                    
                    //pln("stack_offset(size = %, align = %, %) = %", f_umm(size), f_umm(align), f_u32(bc->dest.Register), f_s64(stack_offset));
                } break;
                
                case Bytecode_call: {
                    // TODO(Alexander): this is based on windows x64 calling convention
                    s32 stack_offset = 6*6; // push RSP, return address and paramter (rcx, rdx, r8, r9) home
                    
                    if (bc->dest_type && bc->dest_type->size > 8 && 
                        bc->dest_type->size > result.return_value_size) {
                        result.return_value_size = bc->dest_type->size;
                    }
                    
                    for (int arg_index = (int) array_count(bc->src1.Argument_List) - 1;
                         arg_index >= 4; 
                         --arg_index) {
                        
                        Bc_Argument* arg = bc->src1.Argument_List + arg_index;
                        s32 size = bc_type_to_size(arg->type);
                        // TODO(Alexander): I think, windows calling convention always align types by at least 8 bytes
                        s32 align = (s32) align_forward(size, 8);
                        stack_offset = (s32) align_forward(stack_offset + size, align);
                    }
                    
                    result.argument_size = max(result.argument_size, stack_offset);
                } break;
                
                case Bytecode_memory_alloc: {
                    
                } break;
            }
            it_index++;
        }
        it_block = get_bc_basic_block(x64->bytecode, it_block->next_byte_offset);
        it_index = 0;
    }
    
    return result;
}

internal inline void
x64_free_virtual_register_if_dead(X64_Builder* x64, Bc_Operand operand, u32 curr_bc_instruction) {
    if (operand.kind == BcOperand_Register || operand.kind == BcOperand_Memory) {
        u64 live_length = map_get(x64->bc_register_live_lengths, operand.Register);
        //if (live_length > 0) pln("trying to free bytecode register: % (len = %)", f_u32(operand.Register), f_u32(live_length));
        if (live_length > 0 && live_length <= curr_bc_instruction) {
            //pln("free dead register");
            x64_free_virtual_register(x64, operand.Register);
        }
    } else if (operand.kind == BcOperand_Argument_List) {
        for_array(operand.Argument_List, arg, _) {
            x64_free_virtual_register_if_dead(x64, arg->src, curr_bc_instruction);
        }
    }
}

void
x64_build_procedure(X64_Builder* x64, Bytecode* bytecode, Bc_Basic_Block* first_block,
                    Bc_Procedure_Info* proc) {
    array_free(x64->active_virtual_registers);
    x64->return_value_register = proc->first_return_reg;
    
    Stack_Info stack = x64_analyze_stack(x64, first_block, proc);
    x64->first_arg_as_return = stack.first_arg_as_return;
    
    // Compute the stack frame size
    s32 stack_frame = stack.local_size;
    if (stack.return_value_size > 0) {
        stack_frame = (s32) align_forward(stack_frame, DEFAULT_ALIGNMENT);
        x64->return_value_stack_offset = stack_frame;
        stack_frame += stack.return_value_size;
    }
    stack_frame += stack.argument_size;
    
    // Align to 16 byte boundary (for windows x64 calling convention)
    // https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#alignment
    stack_frame = (s32) align_forward(stack_frame, 16);
    
    x64_push_basic_block(x64, first_block->label);
    // TODO(Alexander): callee should save volatile registers
    
    // Map virtual registers to an argument based on calling convention
    Bc_Instruction* label_insn = get_first_bc_instruction(first_block);
    if (label_insn) {
        // TODO(Alexander): uses the x64 microsoft calling convention
        // https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention
        const X64_Register int_argument_registers[] = {
            X64Register_rcx, X64Register_rdx, X64Register_r8, X64Register_r9
        };
        const X64_Register vector_argument_registers[] = {
            X64Register_xmm0, X64Register_xmm1, X64Register_xmm2, X64Register_xmm3
        };
        
        array(Bc_Argument)* arg_list = label_insn->src1.Argument_List;
        
        Type* return_type = label_insn->src0.Type;
        if (return_type && return_type->size > 8) {
            // Return value cannot fit in RAX, pointer is instead passed as first argument
            Bc_Argument arg = {};
            arg.type = return_type;
            arg.src.kind = BcOperand_Register;
            arg.src.Register = proc->first_return_reg;
            array_insert(arg_list, arg, 0);
        }
        
        // First 4 register paramters, save HOME registers
        int stack_offset = 4*8; // NOTE(Alexander): skip over return address
        for (int arg_index = (int) min(array_count(arg_list), fixed_array_count(int_argument_registers)) - 1;
             arg_index >= 0; 
             --arg_index) {
            
            Bc_Argument* arg = arg_list + arg_index;
            X64_Operand_Kind type_kind = x64_get_register_kind(arg->type);
            
            bool use_vector_register = (arg->type->kind == TypeKind_Basic && 
                                        is_bitflag_set(arg->type->Basic.flags, BasicFlag_Floating));
            X64_Operand value;
            if (use_vector_register) {
                value = x64_build_physical_register(x64, vector_argument_registers[arg_index],
                                                    type_kind);
            } else {
                value = x64_build_physical_register(x64, int_argument_registers[arg_index],
                                                    type_kind);
            }
            
            X64_Opcode mov_opcode = X64Opcode_mov;
            if (arg->type == t_f32) {
                mov_opcode = X64Opcode_movss;
            } else if (arg->type == t_f64) {
                mov_opcode = X64Opcode_movsd;
            }
            
            X64_Instruction* insn = x64_push_instruction(x64, mov_opcode);
            insn->op0 = x64_build_stack_offset(x64, arg->type, stack_offset, X64Register_rsp);
            insn->op1 = value;
            stack_offset -= 8;
            
            map_put(x64->allocated_virtual_registers, arg->src.Register, insn->op1);
        }
        
        // Push arguments next arguments to stack
        stack_offset = 8*6; // skip over return address, saved RBP, and home registers (RCX, RDX, R8, R9)
        for (int arg_index = (int) array_count(arg_list) - 1;
             arg_index >= 4; 
             --arg_index) {
            
            Bc_Argument* arg = arg_list + arg_index;
            X64_Operand value = x64_build_stack_offset(x64, arg->type, stack_offset);
            map_put(x64->allocated_virtual_registers, arg->src.Register, value);
            stack_offset = stack_offset + 8;
        }
    }
    
    // Prologue
    x64_text_push_prologue(x64, stack_frame);
    
    // TODO(Alexander): windows x64 specific calling convention, fix for when first argument
    // is a pointer to the return value.
    if (stack.first_arg_as_return) {
        // Move pointer to the reutrn
        X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
        s64 offset = map_get(x64->stack_offsets, proc->first_return_reg);
        insn->op0 = x64_build_stack_offset(x64, t_s64, offset); // TODO(Alexander): arch dep!
        insn->op1 = x64_build_physical_register(x64, X64Register_rcx, X64Operand_r64);
    }
    
    Bc_Basic_Block* curr_block = first_block;
    u32 curr_bc_instruction = 0;
    u32 curr_block_insn = 0;
    while (curr_block) {
        x64->curr_bc_basic_block = curr_block;
        while (curr_block_insn < curr_block->instruction_count) {
            Bc_Instruction* insn = get_first_bc_instruction(curr_block) + curr_block_insn;
            x64_build_instruction_from_bytecode(x64, insn);
            
            //pln("\n\nInstruction: %", f_u32(curr_bc_instruction));
            x64_free_virtual_register_if_dead(x64, insn->dest, curr_bc_instruction);
            x64_free_virtual_register_if_dead(x64, insn->src0, curr_bc_instruction);
            x64_free_virtual_register_if_dead(x64, insn->src1, curr_bc_instruction);
            
            for_array_v(x64->temp_registers, vreg, _) {
                x64_free_virtual_register(x64, vreg);
            }
            array_free(x64->temp_registers);
            
            curr_block_insn++;
            curr_bc_instruction++;
        }
        
        // NOTE(Alexander): Make sure we aren't reading and writing to memory at once
        X64_Instruction* insn = x64->curr_instruction;
        x64_fix_memory_to_memory_instruction(x64, insn);
        
        curr_block = get_bc_basic_block(bytecode, curr_block->next_byte_offset);
        curr_block_insn = 0;
        
        if (curr_block) {
            x64_push_basic_block(x64, curr_block->label);
        }
    }
    
    // Epilogue
    x64_text_push_epilogue(x64);
    
#ifdef DEBUG_X64_REGISTER_ALLOC
    // Check if we forgot to free any registers
    int allocated_count = (int) array_count(x64->active_virtual_registers);
    if (allocated_count > 0) {
        
        pln("Warning: x64 backend forgot to free `%` virtual registers", f_int(allocated_count));
        print_format("[");
        for_array(x64->active_virtual_registers, it, index) {
            
            X64_Register_Node* node = &map_get(x64->interference_graph, *it);
            print_format("r%", f_int(*it));
            if (node && node->is_allocated) {
                print_format(" (%)", f_cstring(x64_register_name_table[node->physical_register]));
            }
            if (node) {
                int interference = (int) array_count(node->interference);
                print_format(" <-> %", f_int(interference));
            }
            
            if (index < allocated_count - 1) {
                print_format(", ");
            }
        } 
        pln("]");
    }
#endif
}

void
x64_build_data_storage(X64_Builder* x64, Bc_Label label, Value_Data value, Bc_Type type) {
    x64_push_basic_block(x64, label);
    
    X64_Operand value_operand = {};
    
    if (type->kind == TypeKind_Basic) {
        switch (type->Basic.kind) {
            case Basic_bool:
            case Basic_s8:
            case Basic_u8: {
                X64_Instruction* insn = x64_push_instruction(x64, X64Directive_db);
                value_operand.kind = X64Operand_imm8;
                value_operand.imm8 = (s8) value.signed_int;
                insn->op0 = value_operand;
            } break;
            
            case Basic_u16:
            case Basic_s16: {
                X64_Instruction* insn = x64_push_instruction(x64, X64Directive_dw);
                value_operand.kind = X64Operand_imm16;
                value_operand.imm16 = (s16) value.signed_int;
                insn->op0 = value_operand;
            } break;
            
            case Basic_u32:
            case Basic_s32:
            case Basic_f32: {
                X64_Instruction* insn = x64_push_instruction(x64, X64Directive_dd);
                value_operand.kind = X64Operand_imm32;
                value_operand.imm32 = (s32) value.signed_int;
                insn->op0 = value_operand;
            } break;
            
            case Basic_u64:
            case Basic_s64:
            case Basic_f64:{
                X64_Instruction* insn = x64_push_instruction(x64, X64Directive_dq);
                value_operand.kind = X64Operand_imm64;
                value_operand.imm64 = (s64) value.signed_int;
                insn->op0 = value_operand;
            } break;
            
            case Basic_string: {
                X64_Instruction* insn = x64_push_instruction(x64, X64Directive_db);
                value_operand.kind = X64Operand_string_literal;
                value_operand.string_literal = value.mstr;
                insn->op0 = value_operand;
            } break;
        }
    } else {
        
        int offset = 0;
        while (offset < type->size) {
            int size = min(type->size - offset, 8);
            
            value_operand.kind = X64Operand_imm64;
            if (size == 8) {
                value_operand.imm64 = *((s64*) value.data);
            } else if (size == 4) {
                value_operand.imm64 = (s64) *((s32*) value.data);
            } else if (size == 2) {
                value_operand.imm64 = (s64) *((s16*) value.data);
            } else if (size == 1) {
                value_operand.imm64 = (s64) *((s8*) value.data);
            } else {
                assert(0 && "bad size");
            }
            
            X64_Instruction* insn = x64_push_instruction(x64, X64Directive_dq);
            insn->op0 = value_operand;
            
            offset += size;
        }
    }
}


inline void
sting_builder_push(String_Builder* sb, X64_Register_Node* node) {
    string_builder_push_format(sb, "r%", f_u32(node->virtual_register));
    if (node->is_allocated) {
        string_builder_push_format(sb, "_%", 
                                   f_cstring(x64_register_name_table[node->physical_register]));
    }
}

string
x64_interference_graph_to_graphviz_dot(X64_Builder* x64) {
    String_Builder sb = {};
    string_builder_push(&sb, "strict graph G {\n");
    for_map(x64->interference_graph, it) {
        X64_Register_Node* node = &it->value;
        
        for_array(node->interference, dep_id, index) {
            
            
            X64_Register_Node* other_node = &map_get(x64->interference_graph, *dep_id);
            string_builder_push(&sb, "  ");
            sting_builder_push(&sb, node);
            string_builder_push(&sb, " -- ");
            sting_builder_push(&sb, other_node);
            string_builder_push(&sb, "\n");
        }
    }
    string_builder_push(&sb, "}");
    
    return string_builder_to_string_nocopy(&sb);
}

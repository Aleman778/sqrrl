

struct X64_Assembler {
    u8* bytes;
    umm curr_used;
    umm size;
    
};

inline void
push_u8(X64_Assembler* assembler, u8 value) {
    // TODO(Alexander): check if we need to reallocate this
    assembler->bytes[assembler->curr_used] = value;
    assembler->curr_used++;
}

inline void
modify_u8(X64_Assembler* assembler, umm index, u8 value) {
    assert(assembler->curr_used > index);
    assembler->bytes[index] = value;
}


internal void
x64_assemble_instruction(X64_Assembler* assembler,
                         X64_Instruction* insn, 
                         X64_Encoding* encoding) {
    
    assert(encoding->is_valid && "illegal instruction");
    
    u8 rex_prefix = 0b01000000;
    bool use_rex_prefix = encoding->use_rex_prefix;
    if (encoding->use_rex_w) {
        rex_prefix |= 1 << 3;
        use_rex_prefix = true;
    }
    
    u8 modrm = encoding->modrm_mod;
    s32 displacement = 0;
    s32 displacement_bytes = 0;
    if (encoding->modrm_mod != ModRM_not_used) {
        u8 reg = 0;
        // TODO(Alexander): need to update REX prefix to support upper 8 regs
        if (encoding->modrm_reg >= 0b00000111) {
            reg = encoding->modrm_reg >> 3;
        } else {
            X64_Operand* modrm_reg = &insn->operands[encoding->modrm_reg];
            reg = x64_register_id_table[modrm_reg->reg] % 8;
        }
        X64_Operand* modrm_rm = &insn->operands[encoding->modrm_rm];
        u8 rm = x64_register_id_table[modrm_rm->reg] % 8;
        
        if (encoding->modrm_mod != ModRM_direct) {
            displacement = modrm_rm->disp32;
        }
        
        
        if (displacement != 0) {
            if (displacement >= S8_MIN && displacement <= S8_MAX) {
                modrm = ModRM_indirect_disp8;
                displacement_bytes = 1;
            } else {
                modrm = ModRM_indirect_disp32;
                displacement_bytes = 4;
            }
        }
        
        modrm = modrm | (reg << 3) | rm;
    }
    
    // Prefix
    if (encoding->use_prefix) {
        push_u8(assembler, encoding->prefix);
    }
    
    // REX prefix
    if (use_rex_prefix) {
        push_u8(assembler, rex_prefix);
    }
    
    // Opcode
    if (encoding->use_0f_prefix) {
        push_u8(assembler, 0x0F);
    }
    
    u8 primary_opcode = encoding->primary_opcode;
    if (encoding->use_opcode_addend) {
        X64_Operand* addend_operand = &insn->operands[encoding->opcode_addend];
        // TODO(Alexander): need to update REX prefix to support upper 8 regs
        u8 addend = x64_register_id_table[addend_operand->reg] % 8;
        primary_opcode += addend;
    }
    push_u8(assembler, primary_opcode);
    
    if (encoding->secondary_opcode) {
        push_u8(assembler, encoding->secondary_opcode);
    }
    
    // ModRM
    if (encoding->modrm_mod != ModRM_not_used) {
        push_u8(assembler, modrm);
        
        if (encoding->modrm_mod != ModRM_direct && displacement != 0) {
            // TODO(Alexander): might now work for cross compiling
            u8* disp_bytes = (u8*) &displacement;
            for (s32 byte_index = 0; byte_index < displacement_bytes; byte_index++) {
                push_u8(assembler, disp_bytes[byte_index]);
            }
        }
    }
    
    // Immediate
    if (encoding->imm_size > 0) {
        s64 imm = insn->operands[encoding->imm_op].imm64;
        u8* imm_bytes = (u8*) &imm;
        for (s32 byte_index = 0; byte_index < encoding->imm_size; byte_index++) {
            push_u8(assembler, imm_bytes[byte_index]);
        }
    }
}

void
x64_assemble_to_machine_code(X64_Assembler* assembler, 
                             X64_Instruction_Def_Table* x64_instruction_definitions,
                             X64_Basic_Block* basic_block) {
    
    // TODO(Alexander): we can build this in sqrrl_x64_builder.cpp instead
    map(Bc_Register, X64_Basic_Block*)* named_basic_blocks = 0;
    {
        X64_Basic_Block* curr_block = basic_block; 
        while (curr_block) {
            map_put(named_basic_blocks, curr_block->label, curr_block);
            curr_block = curr_block->next;
        }
    }
    
    map(umm, X64_Basic_Block*)* jump_targets = 0;
    
    // Assemble to machine code 
    X64_Basic_Block* curr_block = basic_block; 
    while (curr_block) {
        curr_block->code.offset = assembler->curr_used;
        curr_block->code.bytes = assembler->bytes + assembler->curr_used;
        curr_block->code.size = 0;
        
        for (umm insn_index = 0; insn_index < curr_block->count; insn_index++) {
            X64_Instruction* insn = curr_block->first + insn_index;
            if (insn->opcode == X64Opcode_label) {
                continue;
            }
            
            X64_Instruction_Index index = {};
            index.opcode = (u8) insn->opcode;
            index.op0 = (u8) insn->op0.kind;
            index.op1 = (u8) insn->op1.kind;
            index.op2 = (u8) insn->op2.kind;
            
            if (insn->opcode >= X64Opcode_jmp && insn->opcode <= X64Opcode_jz) {
                
                if (insn->op0.kind == X64Operand_jump_target) {
                    // TODO(Alexander): jump targets are resolved later
                    
                    X64_Basic_Block* target = map_get(named_basic_blocks, insn->op0.jump_target);
                    assert(target != 0);
                    map_put(jump_targets, assembler->curr_used, target);
                    index.op0 = (u8) X64Operand_rel8;
                }
                
            }
            
            X64_Encoding encoding = map_get(x64_instruction_definitions, index);
            
            x64_assemble_instruction(assembler, insn, &encoding);
        }
        
        curr_block->code.size = assembler->curr_used - curr_block->code.offset;
        curr_block = curr_block->next;
    }
    
    // Patch relative jump distances
    for_map(jump_targets, it) {
        umm source = it->key;
        X64_Basic_Block* target = it->value;
        
        const s32 jump_insn_size = 2;
        s32 rel32 = (s32) target->code.offset - (s32) source - jump_insn_size;
        
        if (rel32 >= S8_MIN && rel32 <= S8_MAX) {
            s8 rel8 = (s8) rel32;
            modify_u8(assembler, source + 1, rel8);
        } else {
            assert(0 && "relative jump doesn't fit in rel8 value");
        }
    }
}
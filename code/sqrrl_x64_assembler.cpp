

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
modify_u8(X64_Assembler* assembler, umm byte_index, u8 value) {
    assert(assembler->curr_used > byte_index);
    assembler->bytes[byte_index] = value;
}

inline void
modify_u32(X64_Assembler* assembler, umm byte_index, u32 value) {
    assert(assembler->curr_used > byte_index);
    *((u32*) (assembler->bytes + byte_index)) = value;
}


internal void
x64_assemble_instruction(X64_Assembler* assembler,
                         X64_Instruction* insn, 
                         X64_Encoding* encoding) {
    
    if (!encoding->is_valid) {
        String_Builder sb = {};
        string_builder_push(&sb, insn);
        string text = string_builder_to_string_nocopy(&sb);
        pln("\n\nIllegal instruction found:\n  %", f_string(text));
        string_builder_free(&sb);
    }
    
    // Setup rex prefix
    u8 rex_prefix = 0b01000000;
    bool use_rex_prefix = encoding->use_rex_prefix;
    if (encoding->use_rex_w) {
        rex_prefix |= 1 << 3;
        use_rex_prefix = true;
    }
    
    // Primary opcode addend
    u8 primary_opcode = encoding->primary_opcode;
    if (encoding->use_opcode_addend) {
        X64_Operand* addend_operand = &insn->operands[encoding->opcode_addend];
        u8 addend = (u8) x64_register_id_table[addend_operand->reg];
        if (addend >= 8) {
            // set REX.B = 1
            rex_prefix |= 1;
            use_rex_prefix = true;
        }
        addend %= 8;
        primary_opcode += addend;
    }
    
    // SIB and ModRM setup
    u8 sib = 0;
    bool use_sib = false;
    
    u8 modrm = encoding->modrm_mod;
    s32 displacement = 0;
    s32 displacement_bytes = 0;
    if (encoding->modrm_mod != ModRM_not_used) {
        u8 reg = 0;
        
        if (encoding->modrm_reg >= 0b00000111) {
            reg = encoding->modrm_reg >> 3;
        } else {
            X64_Operand* modrm_reg = &insn->operands[encoding->modrm_reg];
            reg = (u8) x64_register_id_table[modrm_reg->reg];
            if (reg >= 8) {
                use_rex_prefix = true;
                rex_prefix |= 1 << 2; // Set REX.R
            }
            reg %= 8;
        }
        X64_Operand* modrm_rm = &insn->operands[encoding->modrm_rm];
        u8 rm = (u8) x64_register_id_table[modrm_rm->reg];
        if (rm >= 8) {
            use_rex_prefix = true;
            rex_prefix |= 1; // Set REX.B
        }
        rm %= 8;
        
        // NOTE(Alexander): If RSP is used we are forced to use SIB
        if (rm == 0b100 && encoding->modrm_mod != ModRM_direct) {
            use_sib = true;
            sib = (rm << 3) | rm;
        }
        
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
    push_u8(assembler, primary_opcode);
    
    if (encoding->secondary_opcode) {
        push_u8(assembler, encoding->secondary_opcode);
    }
    
    // ModRM
    if (encoding->modrm_mod != ModRM_not_used) {
        push_u8(assembler, modrm);
    }
    
    // SIB
    if (use_sib) {
        push_u8(assembler, sib);
    }
    
    // Displacement
    if (encoding->modrm_mod != ModRM_direct && displacement != 0) {
        // TODO(Alexander): might now work for cross compiling
        u8* disp_bytes = (u8*) &displacement;
        for (s32 byte_index = 0; byte_index < displacement_bytes; byte_index++) {
            // TODO(Alexander): little-endian
            push_u8(assembler, disp_bytes[byte_index]);
        }
    }
    
    // Immediate
    if (encoding->imm_size > 0) {
        s64 imm = insn->operands[encoding->imm_op].imm64;
        u8* imm_bytes = (u8*) &imm;
        
        for (s32 byte_index = 0; byte_index < encoding->imm_size; byte_index++) {
            // TODO(Alexander): little-endian
            push_u8(assembler, imm_bytes[byte_index]);
        }
    }
}

struct X64_Asm_Jump_Target {
    X64_Instruction* insn;
    X64_Basic_Block* block;
    X64_Operand_Kind rel_kind;
    u32 insn_size;
};

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
    
    map(umm, X64_Asm_Jump_Target)* jump_targets = 0;
    
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
            
            if (insn->op0.kind == X64Operand_jump_target) {
                
                // TODO(Alexander): jump targets are resolved later
                X64_Asm_Jump_Target asm_jump_target;
                asm_jump_target.insn = insn;
                asm_jump_target.block = map_get(named_basic_blocks, insn->op0.jump_target);
                assert(asm_jump_target.block != 0);
                
                if (insn->opcode == X64Opcode_call) {
                    index.op0 = (u8) X64Operand_rel32;
                    asm_jump_target.rel_kind = X64Operand_rel32;
                } else {
                    index.op0 = (u8) X64Operand_rel8;
                    asm_jump_target.rel_kind = X64Operand_rel8;
                }
                
                map_put(jump_targets, assembler->curr_used, asm_jump_target);
            }
            
            umm prev_used = assembler->curr_used;
            X64_Encoding encoding = map_get(x64_instruction_definitions, index);
            x64_assemble_instruction(assembler, insn, &encoding);
            
            if (insn->op0.kind == X64Operand_jump_target) {
                X64_Asm_Jump_Target* asm_jump_target = &map_get(jump_targets, prev_used);
                asm_jump_target->insn_size = (u32) (assembler->curr_used - prev_used);
            }
        }
        
        curr_block->code.size = assembler->curr_used - curr_block->code.offset;
        curr_block = curr_block->next;
    }
    
    // Patch relative jump distances
    for_map(jump_targets, it) {
        umm source = it->key;
        X64_Asm_Jump_Target* asm_jump_target = &it->value;
        
        s32 rel32 = (s32) asm_jump_target->block->code.offset - (s32) source - asm_jump_target->insn_size;
        
        if (asm_jump_target->rel_kind == X64Operand_rel8) {
            if (rel32 >= S8_MIN && rel32 <= S8_MAX) {
                s8 rel8 = (s8) rel32;
                modify_u8(assembler, source + asm_jump_target->insn_size - 1, rel8);
            } else {
                assert(0 && "relative jump doesn't fit in rel8 value");
            }
        } else if (asm_jump_target->rel_kind == X64Operand_rel32) {
            modify_u32(assembler, source + asm_jump_target->insn_size - 4, rel32);
        } else {
            assert(0 && "invalid jump target kind");
        }
    }
}

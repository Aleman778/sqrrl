
internal inline bool
x64_8bit_register_swap(X64_Operand operand) {
    return operand.kind == X64Operand_r8 && (operand.reg == X64Register_rsp ||
                                             operand.reg == X64Register_rbp ||
                                             operand.reg == X64Register_rsi ||
                                             operand.reg == X64Register_rdi);
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
    
    // NOTE(Alexander): to use spl, bpl, sil, dil we need to add rex prefix
    if (x64_8bit_register_swap(insn->op0) || x64_8bit_register_swap(insn->op1)) {
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
        
        u8 rm = 0;
        if (modrm_rm->reg == X64Register_rip) {
            rm = 0b101;
            encoding->modrm_mod = ModRM_indirect;
            displacement = modrm_rm->disp32;
            displacement_bytes = 4;
        } else {
            
            rm = (u8) x64_register_id_table[modrm_rm->reg];
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
    if (encoding->modrm_mod != ModRM_direct && displacement_bytes > 0) {
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

void
x64_assemble_to_machine_code(X64_Assembler* assembler, 
                             X64_Instruction_Def_Table* x64_instruction_definitions,
                             X64_Basic_Block* basic_block) {
    
    // TODO(Alexander): we can build this in sqrrl_x64_builder.cpp instead
    map(Bc_Label, X64_Basic_Block*)* named_basic_blocks = 0;
    {
        X64_Basic_Block* curr_block = basic_block; 
        while (curr_block) {
            map_put(named_basic_blocks, curr_block->label, curr_block);
            curr_block = curr_block->next;
        }
    }
    
    map(umm, X64_Asm_Label_Target)* label_targets = 0;
    
    // Assemble to machine code 
    X64_Basic_Block* curr_block = basic_block; 
    while (curr_block) {
        curr_block->code.offset = assembler->curr_used;
        curr_block->code.bytes = assembler->bytes + assembler->curr_used;
        curr_block->code.size = 0;
        
        for (umm insn_index = 0; insn_index < curr_block->count; insn_index++) {
            X64_Instruction* insn = curr_block->first + insn_index;
            
            // Handle custom x64 opcodes and directives
            switch (insn->opcode) {
                case X64Opcode_label: {
                    X64_Basic_Block* block = insn->op0.basic_block;
                    if (block->label.index == 0) {
                        assert(map_get_index(assembler->label_offsets, block->label.ident) == -1 && "duplicate label");
                        map_put(assembler->label_offsets, block->label.ident, assembler->curr_used);
                    }
                } continue; // go to next instruction
                
                case X64Opcode_int3: {
                    push_u8(assembler, 0xCC);
                } continue; // go to next instruction
                
                case X64Directive_db: {
                    if (insn->op0.kind == X64Operand_imm8) {
                        push_u8(assembler, insn->op0.imm8);
                    } else if (insn->op0.kind == X64Operand_string_literal) {
                        umm count = memory_string_count(insn->op0.string_literal);
                        for (int i = 0; i < count; i++) {
                            push_u8(assembler, insn->op0.string_literal[i]);
                        }
                        push_u8(assembler, 0);
                    }
                } continue; // go to next instruction
            }
            
            X64_Instruction_Index index = {};
            index.opcode = (u8) insn->opcode;
            index.op0 = (u8) insn->op0.kind;
            index.op1 = (u8) insn->op1.kind;
            index.op2 = (u8) insn->op2.kind;
            
            
            // TODO(Alexander): this is a bit janky, data targets could appear anywhere
            if (insn->op0.kind == X64Operand_jump_target) {
                
                // TODO(Alexander): jump targets are resolved later
                X64_Asm_Label_Target asm_label_target;
                asm_label_target.address_align = 0;
                asm_label_target.insn = insn;
                asm_label_target.block = map_get(named_basic_blocks, insn->op0.jump_target);
                assert(asm_label_target.block != 0);
                
                if (insn->opcode == X64Opcode_call) {
                    index.op0 = (u8) X64Operand_rel32;
                    asm_label_target.operand = X64Operand_rel32;
                } else {
                    index.op0 = (u8) X64Operand_rel8;
                    asm_label_target.operand = X64Operand_rel8;
                }
                
                map_put(label_targets, assembler->curr_used, asm_label_target);
                
            } else if (insn->op1.kind == X64Operand_data_target) {
                
                X64_Asm_Label_Target asm_label_target = {};
                asm_label_target.address_align = 0;
                asm_label_target.insn = insn;
                asm_label_target.block = map_get(named_basic_blocks, insn->op1.jump_target);
                assert(asm_label_target.block != 0);
                
                switch (insn->op0.kind) {
                    
                    case X64Operand_r32: {
                        index.op1 = (u8) X64Operand_m32;
                        insn->op1.reg = X64Register_rip;
                        asm_label_target.operand = X64Operand_rel32;
                        asm_label_target.address_align = 1;
                    } break;
                    
                    
                    case X64Operand_r64: {
                        index.op1 = (u8) X64Operand_m64;
                        insn->op1.reg = X64Register_rip;
                        asm_label_target.operand = X64Operand_rel32;
                        asm_label_target.address_align = 1;
                    } break;
                    
                    default: {
                        unimplemented; 
                    } break;
                }
                
                map_put(label_targets, assembler->curr_used, asm_label_target);
            }
            
            umm prev_used = assembler->curr_used;
            X64_Encoding encoding = map_get(x64_instruction_definitions, index);
            x64_assemble_instruction(assembler, insn, &encoding);
            
            if (insn->op0.kind == X64Operand_jump_target || insn->op1.kind == X64Operand_data_target) {
                X64_Asm_Label_Target* asm_label_target = &map_get(label_targets, prev_used);
                asm_label_target->insn_size = (u32) (assembler->curr_used - prev_used);
            }
        }
        
        curr_block->code.size = assembler->curr_used - curr_block->code.offset;
        curr_block = curr_block->next;
    }
    
    // Patch relative jump distances
    for_map(label_targets, it) {
        umm source = it->key;
        X64_Asm_Label_Target* asm_label_target = &it->value;
        
        
        s64 abs64 = (s64) asm_label_target->block->code.offset;
        s64 rel64 = abs64 - (s64) source - asm_label_target->insn_size;
        
        if (asm_label_target->address_align) {
            rel64 = align_forward(rel64, asm_label_target->address_align);
        }
        
        switch (asm_label_target->operand) {
            
            case X64Operand_rel8: {
                if (rel64 >= S8_MIN && rel64 <= S8_MAX) {
                    s8 rel8 = (s8) rel64;
                    modify_u8(assembler, source + asm_label_target->insn_size - 1, rel8);
                } else {
                    pln("problem: rel64 is %!", f_s64(rel64));
                    assert(0 && "relative jump doesn't fit in rel8 value");
                }
            } break;
            
            case X64Operand_rel32: {
                if (rel64 >= S32_MIN && rel64 <= S32_MAX) {
                    s32 rel32 = (s32) rel64;
                    modify_u32(assembler, source + asm_label_target->insn_size - 4, rel32);
                } else {
                    pln("problem: rel64 is %!", f_s64(rel64));
                    assert(0 && "relative jump doesn't fit in rel32 value");
                }
            } break;
            
            case X64Operand_imm64: {
                s64 imm64 = (s64) assembler->bytes + abs64;
                modify_u64(assembler, source + asm_label_target->insn_size - 8, imm64);
            } break;
            
            default: {
                assert(0 && "invalid label target kind");
            } break;
        }
    }
}

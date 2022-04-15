

struct X64_Machine_Code {
    u8* bytes;
    umm count;
};


X64_Machine_Code
x64_assemble_to_machine_code(X64_Instruction_Def_Table* x64_instruction_definitions,
                             X64_Basic_Block* basic_block, 
                             void* backing_buffer) {
    
    X64_Machine_Code result = {};
    
    umm machine_code_count = 1024;
    result.bytes = (u8*) backing_buffer;
    u8* curr = result.bytes;
    
    // TODO(Alexander): int3 breakpoint for debugging
    *curr++ = 0xCC;
    result.count++;
    
    X64_Basic_Block* curr_block = basic_block; 
    while (curr_block) {
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
            X64_Encoding encoding = map_get(x64_instruction_definitions, index);
            
            assert(encoding.is_valid && "illegal instruction");
            
            u8 rex_prefix = 0b01000000;
            bool use_rex_prefix = encoding.use_rex_prefix;
            if (encoding.use_rex_w) {
                rex_prefix |= 1 << 3;
                use_rex_prefix = true;
            }
            
            u8 modrm = encoding.modrm_mod;
            s32 displacement = 0;
            s32 displacement_bytes = 0;
            if (encoding.modrm_mod != ModRM_not_used) {
                u8 reg = 0;
                // TODO(Alexander): need to update REX prefix to support upper 8 regs
                if (encoding.modrm_reg >= 0b00000111) {
                    reg = encoding.modrm_reg >> 3;
                } else {
                    X64_Operand* modrm_reg = &insn->operands[encoding.modrm_reg];
                    reg = x64_register_id_table[modrm_reg->reg] % 8;
                }
                X64_Operand* modrm_rm = &insn->operands[encoding.modrm_rm];
                u8 rm = x64_register_id_table[modrm_rm->reg] % 8;
                
                if (encoding.modrm_mod != ModRM_direct) {
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
            if (encoding.use_prefix) {
                *curr++ = encoding.prefix;
                result.count++;
            }
            
            // REX prefix
            if (use_rex_prefix) {
                *curr++ = rex_prefix;
                result.count++;
            }
            
            // Opcode
            if (encoding.use_0f_prefix) {
                *curr++ = 0x0F;
                result.count++;
            }
            
            u8 primary_opcode = encoding.primary_opcode;
            if (encoding.use_opcode_addend) {
                X64_Operand* addend_operand = &insn->operands[encoding.opcode_addend];
                // TODO(Alexander): need to update REX prefix to support upper 8 regs
                u8 addend = x64_register_id_table[addend_operand->reg] % 8;
                primary_opcode += addend;
            }
            *curr++ = primary_opcode;
            result.count++;
            
            if (encoding.secondary_opcode) {
                *curr++ = encoding.secondary_opcode;
                result.count++;
            }
            
            // ModRM
            if (encoding.modrm_mod != ModRM_not_used) {
                *curr++ = modrm;
                result.count++;
                
                if (encoding.modrm_mod != ModRM_direct && displacement != 0) {
                    // TODO(Alexander): might now work for cross compiling
                    u8* disp_bytes = (u8*) &displacement;
                    for (s32 byte_index = 0; byte_index < displacement_bytes; byte_index++) {
                        *curr++ = disp_bytes[byte_index];
                        result.count++;
                    }
                }
            }
            
            // Immediate
            if (encoding.imm_size > 0) {
                s64 imm = insn->operands[encoding.imm_op].imm64;
                u8* imm_bytes = (u8*) &imm;
                for (s32 byte_index = 0; byte_index < encoding.imm_size; byte_index++) {
                    *curr++ = imm_bytes[byte_index];
                    result.count++;
                }
                
            }
        }
        
        curr_block = curr_block->next;
    }
    
    return result;
}

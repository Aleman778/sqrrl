

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
    
    *curr++ = 0xCC;
    result.count++;
    
#if 0
    map(X64_Instruction_Index, X64_Encoding)* x64_instruction_definitions = 0;
    
    {
        // 0x8A: mov r8, r8/rm8
        X64_Encoding encoding = {};
        encoding.rex_prefix_mandatory = false;
        encoding.opcode = 0x8B;
        encoding.modrm_mod = ModRM_direct;
        encoding.modrm_reg = 0;
        encoding.modrm_rm = 1;
        encoding.is_valid = true;
        
        X64_Instruction_Index index = {};
        index.opcode = X64Opcode_mov;
        index.op0 = X64Operand_r8;
        index.op1 = X64Operand_r8;
        encoding.opcode = 0x8A;
        encoding.modrm_mod = ModRM_direct;
        map_put(x64_instruction_definitions, index, encoding);
        index.op1 = X64Operand_rm8;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        // 0x88: mov rm8, r8
        encoding.opcode = 0x88;
        encoding.modrm_reg = 1;
        encoding.modrm_rm = 0;
        index.op0 = X64Operand_rm8;
        index.op1 = X64Operand_r8;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        // 0x8B: mov r, r/rm
        encoding.opcode = 0x8B;
        encoding.modrm_reg = 0;
        encoding.modrm_rm = 1;
        index.op0 = X64Operand_r16;
        index.op1 = X64Operand_r16;
        encoding.modrm_mod = ModRM_direct;
        map_put(x64_instruction_definitions, index, encoding);
        index.op1 = X64Operand_rm16;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        index.op0 = X64Operand_r32;
        index.op1 = X64Operand_r32;
        encoding.modrm_mod = ModRM_direct;
        map_put(x64_instruction_definitions, index, encoding);
        index.op1 = X64Operand_rm32;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        index.op0 = X64Operand_r64;
        index.op1 = X64Operand_r64;
        encoding.modrm_mod = ModRM_direct;
        encoding.rex_prefix_mandatory = true;
        map_put(x64_instruction_definitions, index, encoding);
        index.op1 = X64Operand_rm64;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        // 0x89: mov rm, r
        encoding.opcode = 0x89;
        encoding.modrm_reg = 0;
        encoding.modrm_rm = 1;
        encoding.modrm_mod = ModRM_indirect;
        
        index.op0 = X64Operand_rm16;
        index.op1 = X64Operand_r16;
        map_put(x64_instruction_definitions, index, encoding);
        
        index.op0 = X64Operand_rm32;
        index.op1 = X64Operand_r32;
        map_put(x64_instruction_definitions, index, encoding);
        
        index.op0 = X64Operand_rm64;
        index.op1 = X64Operand_r64;
        encoding.rex_prefix_mandatory = true;
        map_put(x64_instruction_definitions, index, encoding);
    }
    
    {
        // 0xFF /6: push r/rm
        X64_Encoding encoding = {};
        encoding.rex_prefix_mandatory = false;
        encoding.opcode = 0xFF;
        encoding.modrm_mod = ModRM_direct;
        encoding.modrm_reg = 6 << 3;
        encoding.modrm_rm = 0;
        encoding.is_valid = true;
        
        X64_Instruction_Index index = {};
        index.opcode = X64Opcode_push;
        index.op0 = X64Operand_r16;
        encoding.modrm_mod = ModRM_direct;
        map_put(x64_instruction_definitions, index, encoding);
        index.op0 = X64Operand_rm16;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        index.op0 = X64Operand_r32;
        encoding.modrm_mod = ModRM_direct;
        map_put(x64_instruction_definitions, index, encoding);
        index.op0 = X64Operand_rm32;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        index.op0 = X64Operand_r64;
        encoding.modrm_mod = ModRM_direct;
        encoding.rex_prefix_mandatory = true;
        map_put(x64_instruction_definitions, index, encoding);
        index.op0 = X64Operand_rm64;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
    }
    
    {
        // 0x8F /6: pop r/rm
        X64_Encoding encoding = {};
        encoding.rex_prefix_mandatory = false;
        encoding.opcode = 0x8F;
        encoding.modrm_mod = ModRM_direct;
        encoding.modrm_reg = 0b00000111;
        encoding.modrm_rm = 0;
        encoding.is_valid = true;
        
        X64_Instruction_Index index = {};
        index.opcode = X64Opcode_pop;
        index.op0 = X64Operand_r16;
        encoding.modrm_mod = ModRM_direct;
        map_put(x64_instruction_definitions, index, encoding);
        index.op0 = X64Operand_rm16;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        index.op0 = X64Operand_r32;
        encoding.modrm_mod = ModRM_direct;
        map_put(x64_instruction_definitions, index, encoding);
        index.op0 = X64Operand_rm32;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
        
        index.op0 = X64Operand_r64;
        encoding.modrm_mod = ModRM_direct;
        encoding.rex_prefix_mandatory = true;
        map_put(x64_instruction_definitions, index, encoding);
        index.op0 = X64Operand_rm64;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instruction_definitions, index, encoding);
    }
#endif
    
    X64_Basic_Block* curr_block = basic_block; 
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
        pln("is_valid: %", f_bool(encoding.is_valid));
        
        assert(encoding.is_valid && "illegal instruction");
        
        u8 rex_prefix = 0b01000000;
        bool use_rex_prefix = encoding.rex_prefix_mandatory;
        if (use_rex_prefix) {
            // TODO(Alexander): 64-bit operand size should be the name of this instead
            // or check size of operands
            rex_prefix |= 1 << 3;
        }
        
        
        u8 modrm = encoding.modrm_mod;
        s32 displacement = 0;
        s32 displacement_bytes = 0;
        if (encoding.modrm_mod != ModRM_not_used) {
            // TODO(Alexander): need to update REX prefix to support upper 8 regs
            u8 reg = 0;
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
        
        if (use_rex_prefix) {
            *curr++ = rex_prefix;
            result.count++;
        }
        
        *curr++ = encoding.opcode;
        result.count++;
        
        if (encoding.modrm_mod != ModRM_not_used) {
            *curr++ = modrm;
            result.count++;
        }
        
        if (encoding.modrm_mod != ModRM_direct && displacement != 0) {
            // TODO(Alexander): might now work for cross compiling
            u8* disp_bytes = (u8*) &displacement;
            for (s32 byte_index = 0; byte_index < displacement_bytes; byte_index++) {
                *curr++ = disp_bytes[byte_index];
                result.count++;
            }
        }
    }
    
    return result;
}

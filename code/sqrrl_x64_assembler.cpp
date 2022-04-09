

struct X64_Machine_Code {
    u8* bytes;
    umm count;
};


X64_Machine_Code
x64_assemble_to_machine_code(X64_Basic_Block* basic_block, void* backing_buffer) {
    X64_Machine_Code result = {};
    
    umm machine_code_count = 1024;
    result.bytes = (u8*) backing_buffer;
    u8* curr = result.bytes;
    
    *curr++ = 0xCC;
    result.count++;
    
    map(X64_Instruction_Index, X64_Encoding)* x64_instuction_encodings = 0;
    
    {
        // mov r32, r32
        X64_Instruction_Index index = {};
        index.opcode = X64Opcode_mov;
        index.op0 = X64Operand_r32;
        index.op1 = X64Operand_r32;
        
        X64_Encoding encoding = {};
        encoding.rex_prefix_mandatory = false;
        encoding.opcode = 0x8B;
        encoding.modrm_mod = ModRM_direct;
        encoding.modrm_reg = 0;
        encoding.modrm_rm = 1;
        encoding.is_valid = true;
        
        map_put(x64_instuction_encodings, index, encoding);
        
        // mov r32, rm32
        index.op1 = X64Operand_rm32;
        encoding.modrm_mod = ModRM_indirect;
        map_put(x64_instuction_encodings, index, encoding);
    }
    
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
        X64_Encoding encoding = map_get(x64_instuction_encodings, index);
        pln("is_valid: %", f_bool(encoding.is_valid));
        
        if (!encoding.is_valid) {
            continue;
        }
        
        
        u8 rex_prefix = 0b11000000;
        bool use_rex_prefix = encoding.rex_prefix_mandatory;
        
        
        u8 modrm = encoding.modrm_mod;
        s32 displacement = 0;
        s32 displacement_bytes = 0;
        if (encoding.modrm_mod != ModRM_not_used) {
            X64_Operand* modrm_reg = &insn->operands[encoding.modrm_reg];
            X64_Operand* modrm_rm = &insn->operands[encoding.modrm_rm];
            
            // TODO(Alexander): need to update REX prefix to support upper 8 regs
            u8 reg = x64_register_id_table[modrm_reg->reg] % 8;
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

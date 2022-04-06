


struct X64_Machine_Code {
    u8* bytes;
    umm count;
};


X64_Machine_Code
x64_assemble_to_machine_code(X64_Basic_Block* basic_block) {
    X64_Machine_Code result = {};
    
    umm machine_code_count = 1024;
    u8* machine_code = (u8*) malloc(machine_code_count);
    u8* curr = machine_code;
    
    map(X64_Instruction_Index, X64_Encoding)* x64_instuction_encodings = 0;
    
    {
        // mov rm32, r32
        X64_Instruction_Index index = {};
        index.opcode = X64Opcode_mov;
        index.op0 = X64Operand_r32;
        index.op1 = X64Operand_r32;
        
        X64_Encoding encoding = {};
        encoding.rex_prefix_mandatory = false;
        encoding.modrm_mod = ModRM_direct;
        encoding.operands[0] = X64EncodedOperand_reg;
        encoding.operands[1] = X64EncodedOperand_rm;
        encoding.is_valid = true;
        
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
        
        
        u8 rex_prefix = 0b11000000;
        bool use_rex_prefix = encoding.rex_prefix_mandatory;
        
        u8 modrm = encoding->modrm_mod << 6;
        if (encoding.operands[0]) {
            for (int operand_index = 0;
                 operand_index < fixed_array_count(encoding.operands);
                 operand_index++) {
                
                switch (encoding.operands[operand_index]) {
                    case X64EncodedOperand_reg:  {
                        u8 reg = x64_register_id_table[insn->operands[operand_index].reg];
                        if (reg >= 8) {
                            reg -= 8;
                            use_rex_prefix = true;
                        }
                        modrm |= reg << 3;
                    } break;
                    
                }
            }
        }
        
        
#if 0
        // Rex prefix
        bool rex_w = false;
        bool rex_r = false;
        bool rex_x = false;
        bool rex_b = false;
        
        // ModR/M
        bool is_indirect = false;
        s8 reg = -1;
        s8 rm = -1;
        
        if (insn->opcode == X64Opcode_mov) {
            
            // Lets start with encoding mov instruction
            switch (insn->op0.kind) {
                
                case X64Operand_r8:
                case X64Operand_r15:
                case X64Operand_r32:
                case X64Operand_r64: {
                    reg = insn->op0;
                } break;
                
                case X64Operand_rm8:
                case X64Operand_rm15:
                case X64Operand_rm32:
                case X64Operand_rm64: 
                case X64Operand_m8:
                case X64Operand_m15:
                case X64Operand_m32:
                case X64Operand_m64: {
                    rm = insn->op0;
                } break;
            }
        }
#endif
    }
    
    return result;
}

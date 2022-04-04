


struct X64_Machine_Code {
    u8* bytes;
    umm count;
};


X64_Machine_Code
x64_assemble_to_machine_code(X64_Basic_Block* basic_block) {
    X64_Machine_Code result = {};
    
    Memory_Arena arena = {};
    
    
    map(X64_Instruction_Index, X64_Encoding)* x64_instuction_encodings = 0;
    
    
    {
        // mov rm32, r32
        X64_Instruction_Index index = {};
        index.opcode = X64Opcode_mov;
        index.op0 = X64Operand_r32;
        index.op1 = X64Operand_r32;
        
        X64_Encoding encoding = {};
        encoding.rex_w = false;
        encoding.is_valid = true;
        //encoding.modrm = 0b11000000;
        
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
        
        
        bool rex_r = false;
        bool rex_b = false;
        
        //if (is_ 
        
        pln("is_valid: %", f_bool(encoding.is_valid));
        
    }
    
    return result;
}


typedef string_map(X64_Operand_Kind) X64_Operand_Kind_Table;

struct X64_Assembler {
    u8* bytes;
    umm curr_used;
    umm size;
    
    map(string_id, umm)* label_offsets;
};

struct X64_Asm_Label_Target {
    X64_Instruction* insn;
    X64_Basic_Block* block;
    X64_Operand_Kind operand;
    s32 address_align;
    u32 insn_size;
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

inline void
modify_u64(X64_Assembler* assembler, umm byte_index, u64 value) {
    assert(assembler->curr_used > byte_index);
    *((u64*) (assembler->bytes + byte_index)) = value;
}

X64_Instruction_Def_Table* parse_x86_64_definitions();

void x64_assemble_to_machine_code(X64_Assembler* assembler, 
                                  X64_Instruction_Def_Table* x64_instruction_definitions,
                                  X64_Basic_Block* basic_block);


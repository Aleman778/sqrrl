
typedef map(Bc_Register, u32) Bc_Live_Length_Table;

struct Bc_Builder {
    Memory_Arena arena;
    
    Bc_Basic_Block* global_block;
    
    // Current building block
    Bc_Basic_Block* curr_declaration;
    Bc_Basic_Block* curr_declaration_epilogue;
    Bc_Instruction* curr_instruction;
    Bc_Basic_Block* curr_basic_block;
    Bc_Operand curr_return_dest;
    u32 curr_local_count;
    u32 instruction_count;
    map(string_id, Bc_Operand)* ident_to_operand;
    
    Bc_Live_Length_Table* live_lengths;
    
    Bc_Label_To_Value_Table* declarations;
};

void bc_push_instruction(Bc_Builder* bc, Bc_Opcode opcode);
void bc_push_operand(Bc_Builder* bc, Bc_Operand operand);

inline Bc_Instruction*
bc_push_branch(Bc_Builder* bc, Bc_Operand* cond) {
    // Branch
    Bc_Operand stub = {};
    stub.kind = BcOperand_Basic_Block;
    bc_push_instruction(bc, Bytecode_branch);
    if (cond) {
        bc_push_operand(bc, *cond);
        bc_push_operand(bc, stub);
        bc_push_operand(bc, stub);
    } else {
        bc_push_operand(bc, stub);
    }
    
    return bc->curr_instruction;
}

inline Bc_Operand
bc_get_unique_register_operand(Bc_Builder* bc, Bc_Type type) {
    Bc_Operand result;
    
    Bc_Register reg;
    reg.ident = bc->curr_declaration ? bc->curr_declaration->label.ident : 0;
    reg.index = bc->curr_local_count++;
    
    result.kind = BcOperand_Register;
    result.type = type;
    result.Register = reg;
    return result;
}

inline Bc_Register
bc_get_unique_register(Bc_Builder* bc) {
    Bc_Register result;
    result.ident = bc->curr_declaration ? bc->curr_declaration->label.ident : 0;
    result.index = bc->curr_local_count++;
    return result;
}

Bc_Basic_Block* bc_push_basic_block(Bc_Builder* bc, Bc_Register label = {});

void bc_build_from_ast(Bc_Builder* bc, Ast_File* ast_file);


struct Bc_Interp_Scope {
    map(Bc_Register, Value_Data)* registers;
    Bc_Basic_Block* curr_block;
    smm curr_block_insn;
    Bc_Register return_register;
};

struct Bc_Interp {
    // TODO(Alexander): we shouldn't need the builder, we need to extract only the bytecode 
    Bytecode* code;
    
    array(Bc_Interp_Scope)* scopes;
    Bc_Decl_Table* declarations;
    
    Memory_Arena stack;
    smm base_pointer;
    Value_Data return_value;
};


Value_Data bc_interp_bytecode(Bc_Interp* interp, string_id entry_point = Sym_main);
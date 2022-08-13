
struct X64_Register_Node {
    u64 virtual_register;
    X64_Register physical_register;
    array(u64)* interference;
    b32 is_allocated;
    b32 is_spilled;
};

typedef map(u64, X64_Register_Node) Interference_Graph;

struct X64_Builder {
    Memory_Arena arena;
    X64_Basic_Block* first_basic_block;
    
    umm instruction_count;
    X64_Instruction* curr_instruction;
    X64_Basic_Block* curr_basic_block;
    Bc_Instruction* curr_compare_insn;
    
    map(u64, s32)* stack_offsets;
    s32 curr_stack_offset;
    
    Memory_Arena rodata_segment_arena;
    
    Bc_Label_Index_Table* label_indices;
    u64 next_free_virtual_register;
    map(u64, X64_Operand)* allocated_virtual_registers; // TODO(Alexander): needs to be renamed
    Bc_Live_Length_Table* bc_register_live_lengths;
    array(u64)* active_virtual_registers;
    
    Interference_Graph* interference_graph;
};

void x64_build_function(X64_Builder* x64, Bytecode* bytecode, Bc_Basic_Block* first_block);

void x64_build_data_storage(X64_Builder* x64, 
                            Bc_Label label, 
                            Value_Data value, 
                            Bc_Type type);

void x64_perform_register_allocation(X64_Builder* x64);

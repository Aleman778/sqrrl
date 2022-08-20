
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
    Bc_Basic_Block* curr_bc_basic_block;
    X64_Basic_Block* curr_basic_block;
    Bc_Instruction* curr_compare_insn;
    
    map(u64, s32)* stack_offsets;
    s32 curr_stack_offset;
    map(u64, X64_Operand)* allocated_virtual_registers;
    array(u64)* active_virtual_registers;
    array(u64)* temp_registers;
    
    
    Memory_Arena rodata_segment_arena;
    
    Bc_Label_Index_Table* label_indices;
    u64 next_free_virtual_register;
    Bc_Live_Length_Table* bc_register_live_lengths;
    
    Bytecode* bytecode;
    
    Interference_Graph* interference_graph;
};

inline void
x64_add_interference(X64_Builder* x64, u64 a, u64 b) {
    X64_Register_Node* node_a = &map_get(x64->interference_graph, a);
    X64_Register_Node* node_b = &map_get(x64->interference_graph, b);
    array_push(node_a->interference, b);
    array_push(node_b->interference, a);
}


inline void
x64_allocate_virtual_register(X64_Builder* x64, u64 virtual_register) {
    if (map_get_index(x64->interference_graph, virtual_register) != -1) {
        return;
    }
    
    {
        //pln("x64_allocate_virtual_register: r%", f_u32(virtual_register));
        
        X64_Register_Node new_node = {};
        new_node.virtual_register = virtual_register;
        map_put(x64->interference_graph, virtual_register, new_node);
    }
    
    X64_Register_Node* node = &map_get(x64->interference_graph, virtual_register);
    for_array(x64->active_virtual_registers, other_vreg, other_index) {
        X64_Register_Node* other_node = &map_get(x64->interference_graph, *other_vreg);
        array_push(node->interference, other_node->virtual_register);
        array_push(other_node->interference, node->virtual_register);
        //pln("interference % -> %", 
        //f_u32(node->virtual_register),
        //f_u32(other_node->virtual_register));
    }
}

inline u64
x64_allocate_temporary_register(X64_Builder* x64) {
    u64 virtual_register = x64->next_free_virtual_register++;
    x64_allocate_virtual_register(x64, virtual_register);
    array_push(x64->temp_registers, virtual_register);
    return virtual_register;
}

inline X64_Operand
x64_allocate_temporary_register(X64_Builder* x64, Bc_Type type) {
    X64_Operand result = {};
    result.kind = x64_get_register_kind(type);
    result.virtual_register = x64_allocate_temporary_register(x64);
    return result;
}

inline X64_Operand
x64_allocate_temporary_register(X64_Builder* x64, X64_Operand_Kind kind) {
    X64_Operand result = {};
    result.kind = kind;
    result.virtual_register = x64_allocate_temporary_register(x64);
    return result;
}


inline void
x64_allocate_specific_register(X64_Builder* x64, X64_Register physical_register, u64 virtual_register) {
    x64_allocate_virtual_register(x64, virtual_register);
    
    X64_Register_Node* node = &map_get(x64->interference_graph, virtual_register);
    node->physical_register = physical_register;
    node->is_allocated = true;
    
    array_push(x64->active_virtual_registers, virtual_register);
}

inline u64
x64_allocate_specific_register(X64_Builder* x64, X64_Register physical_register) {
    u64 virtual_register = x64_allocate_temporary_register(x64);
    x64_allocate_specific_register(x64, physical_register, virtual_register);
    return virtual_register;
}

inline void
x64_free_virtual_register(X64_Builder* x64, u64 virtual_register) {
    s32 found_index = -1;
    for_array(x64->active_virtual_registers, it, it_index) {
        if (*it == virtual_register) {
            found_index = it_index;
            break;
        }
    }
    if (found_index >= 0) {
        //pln("free virtual register: r%", f_u64(virtual_register));
        array_swap_remove(x64->active_virtual_registers, found_index);
    }
}

inline bool
operand_is_unallocated_register(X64_Operand operand) {
    return (operand_is_register(operand.kind) || 
            operand_is_memory(operand.kind)) && !operand.is_allocated;
}

#if BUILD_DEBUG
// TODO(Alexander): ugh macro nonsense
#define S1(x) #x
#define S2(x) S1(x)
#define x64_push_instruction(x64, opcode) _x64_push_instruction(x64, opcode, __FILE__ ":" S2(__LINE__))
#else 
#define x64_push_instruction(x64, opcode) _x64_push_instruction(x64, opcode) 
#endif 


#define x64_push_struct(x64, type, ...) \
(type*) x64_push_size(x64, (umm) sizeof(type), (umm) alignof(type), __VA_ARGS__)

X64_Basic_Block* x64_push_basic_block(X64_Builder* x64, Bc_Label label);

void x64_build_function(X64_Builder* x64, Bytecode* bytecode, Bc_Basic_Block* first_block);

void x64_build_data_storage(X64_Builder* x64, 
                            Bc_Label label, 
                            Value_Data value, 
                            Bc_Type type);

void x64_perform_register_allocation(X64_Builder* x64);


typedef map(Bc_Register, u64) Bc_Live_Length_Table;
typedef map(Bc_Label, Bc_Decl) Bc_Decl_Table;

struct Bc_Label_Index_Table {
    map(string_id, u32)* ident_to_index;
};

struct Bc_Builder {
    Bytecode code;
    Memory_Arena code_arena;
    
    Memory_Arena data_arena;
    String_Interner stored_strings;
    
    Bc_Basic_Block* global_block;
    
    // Current building block
    string_id curr_decl;
    Bc_Label curr_prologue;
    Bc_Label curr_epilogue;
    Bc_Label curr_bb;
    Bc_Label_Index_Table label_indices;
    Bc_Instruction* curr_instruction;
    Bc_Basic_Block* curr_basic_block;
    Bc_Operand curr_return_dest;
    Bc_Type curr_return_type;
    Bc_Register next_register;
    u64 instruction_count;
    u32 curr_declaration_index;
    
    // TODO(Alexander): Bc_Argument can be renamed later
    map(string_id, Bc_Argument)* local_variable_mapper;
    
    Bc_Live_Length_Table* live_lengths;
    
    Bc_Decl_Table* declarations;
    
    string_map(Memory_String)* string_storage;
};

void bc_build_from_ast(Bc_Builder* bc, Ast_File* ast_file);
Bc_Type bc_build_type(Bc_Builder* bc, Type* type);

Bc_Basic_Block* bc_push_basic_block(Bc_Builder* bc, Bc_Label label = {});
Bc_Instruction* bc_push_instruction(Bc_Builder* bc, Bc_Opcode opcode);

inline Bc_Operand
create_unique_bc_register(Bc_Builder* bc) {
    Bc_Operand result;
    result.kind = BcOperand_Register;
    result.Register = bc->next_register++;
    return result;
}

inline Bc_Label
create_unique_bc_label(Bc_Label_Index_Table* label_indices, string_id label_ident) {
    Bc_Label result;
    result.ident = label_ident;
    result.index = map_get(label_indices->ident_to_index, result.ident);
    map_put(label_indices->ident_to_index, result.ident, result.index + 1);
    return result;
}

inline Bc_Label
create_unique_bc_label(Bc_Builder* bc) {
    return create_unique_bc_label(&bc->label_indices, bc->curr_decl);
}

inline Bc_Label
create_unique_bc_label(Bc_Builder* bc, string_id ident) {
    return create_unique_bc_label(&bc->label_indices, ident);
}

inline Bc_Operand
bc_type_op(Bc_Type type) {
    Bc_Operand result;
    result.kind = BcOperand_Type;
    result.Type = type;
    return result;
}

inline Bc_Operand
bc_signed_int_op(s64 value) {
    Bc_Operand result;
    result.kind = BcOperand_Int;
    result.Signed_Int = value;
    return result;
}

inline Bc_Operand
bc_unsigned_int_op(u64 value) {
    Bc_Operand result;
    result.kind = BcOperand_Int;
    result.Unsigned_Int = value;
    return result;
}


inline Bc_Operand
bc_float_op(f64 value) {
    Bc_Operand result;
    result.kind = BcOperand_Float;
    result.Float = value;
    return result;
}

inline Bc_Operand
bc_string_op(Memory_String value) {
    Bc_Operand result;
    result.kind = BcOperand_String;
    result.String = value;
    return result;
}

inline Bc_Operand
bc_label_op(Bc_Label label) {
    Bc_Operand result = {};
    result.kind = BcOperand_Label;
    result.Label = label;
    return result;
}

inline Bc_Operand
bc_stack_alloc(Bc_Builder* bc, Bc_Type value_type) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_stack_alloc);
    insn->dest = create_unique_bc_register(bc);
    insn->dest.kind = BcOperand_Stack;
    insn->dest_type = value_type;
    insn->src0 = bc_type_op(value_type);
    return insn->dest;
}

inline Bc_Operand
bc_stack_alloc_aggregate(Bc_Builder* bc, Bc_Type value_type, Type* base_type, umm count) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_stack_alloc);
    insn->dest = create_unique_bc_register(bc);
    insn->dest.kind = BcOperand_Stack;
    insn->dest_type = value_type;
    insn->src0.kind = BcOperand_Aggregate_Type;
    insn->src0.Aggregate_Type.base = base_type;
    insn->src0.Aggregate_Type.count = count;
    return insn->dest;
}

inline void
bc_load(Bc_Builder* bc, Bc_Operand dest, Bc_Operand src, Bc_Type type) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_load);
    insn->dest_type = type;
    insn->dest = dest;
    insn->src0 = src;
}

inline void
bc_copy(Bc_Builder* bc, Bc_Operand dest, Bc_Operand src, Bc_Type type) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_copy);
    insn->dest_type = type;
    insn->dest = dest;
    insn->src0 = src;
    
    if (type->kind == TypeKind_Struct || type->kind == TypeKind_Array) {
        if (!(type->size == 1 || type->size == 2 || type->size == 4 || type->size == 8)) {
            insn->opcode = Bytecode_memcpy;
            insn->src1 = bc_signed_int_op(type->size);
        }
    }
}

inline void
bc_store(Bc_Builder* bc, Bc_Operand dest, Bc_Operand src, Bc_Type type) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_store);
    insn->dest_type = type;
    insn->dest = dest;
    insn->src0 = src;
}

inline Bc_Operand
bc_index(Bc_Builder* bc, Bc_Operand arr, Bc_Operand index, Bc_Type type) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_index);
    insn->dest = create_unique_bc_register(bc);
    insn->dest.kind = BcOperand_Memory;
    insn->dest_type = type;
    insn->src0 = arr;
    insn->src1 = index;
    return insn->dest;
}

#if 0
inline Bc_Operand
bc_load(Bc_Builder* bc, Bc_Operand src, Bc_Type value_type) {
    Bc_Operand result = src;
    
    return result;
}

inline void
bc_store(Bc_Builder* bc, Bc_Operand dest, Bc_Operand src, Bc_Type type) {
    //assert(dest.kind == BcOperand_Stack);
    
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_store);
    insn->dest_type = type;
    insn->dest = dest;
    insn->src0 = src;
}
#endif

inline Bc_Operand
bc_ret(Bc_Builder* bc, Bc_Operand first, Bc_Type type) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_ret);
    insn->dest = first;
    insn->dest_type = type;
    return insn->dest;
}

inline Bc_Operand
bc_unary(Bc_Builder* bc, Bc_Opcode opcode, Bc_Operand first, Bc_Type type) {
    Bc_Instruction* insn = bc_push_instruction(bc, opcode);
    insn->dest = create_unique_bc_register(bc);
    insn->dest_type = type;
    insn->src0 = first;
    return insn->dest;
}

inline Bc_Operand
bc_binary(Bc_Builder* bc, Bc_Opcode opcode, Bc_Operand first, Bc_Operand second, Bc_Type type) {
    Bc_Instruction* insn = bc_push_instruction(bc, opcode);
    insn->dest = create_unique_bc_register(bc);
    insn->dest_type = type;
    insn->src0 = first;
    insn->src1 = second;
    return insn->dest;
}

inline void
bc_label(Bc_Builder* bc, Bc_Label label) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_label);
    insn->dest = bc_label_op(label);
}

inline void
bc_goto(Bc_Builder* bc, Bc_Label label) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_goto);
    insn->dest = bc_label_op(label);
}

inline void
bc_branch(Bc_Builder* bc, Bc_Operand cond, 
          Bc_Label label_true, Bc_Label label_false) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_branch);
    insn->dest = cond;
    insn->src0 = bc_label_op(label_true);
    insn->src1 = bc_label_op(label_false);
}

inline Bc_Operand
bc_call(Bc_Builder* bc, Bc_Type proc_type, array(Bc_Argument)* args, Bc_Type return_type) {
    Bc_Instruction* insn = bc_push_instruction(bc, Bytecode_call);
    if (return_type->kind != TypeKind_Void) {
        insn->dest = create_unique_bc_register(bc);
        insn->dest_type = return_type;
    }
    insn->src0 = bc_type_op(proc_type);
    insn->src1.kind = BcOperand_Argument_List;
    insn->src1.Argument_List = args;
    return insn->dest;
}
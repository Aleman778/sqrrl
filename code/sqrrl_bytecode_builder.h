
struct Bytecode_Builder {
    Bc_Bucket* bucket;
    
    Bc_Module module;
    
    
    array(Bc_Type)* register_types;
};

int
bc_allocate_register(Bytecode_Builder* bc, Bc_Type type) {
    int result = (int) array_count(bc->register_types);
    array_push(bc->register_types, type);
    
    return result;
}

Bc*
bc_instruction(Bytecode_Builder* bc, Opcode opcode,
               int res_index=-1, int a_index=-1, int b_index=-1) {
    
    Bc_Bucket* bucket = bc->bucket;
    
    if (!bucket || bucket->count >= BC_INSTRUCTION_BUCKET_SIZE) {
        auto next_bucket = (Bc_Bucket*) calloc(1, BC_INSTRUCTION_BUCKET_SIZE);
        if (bucket) {
            bucket->next = next_bucket;
        }
        bucket = next_bucket;
        
        pln("allocating new bucket with capacity: % instructions (instruction size: %)", f_int(BC_INSTRUCTIONS_PER_BUCKET), f_int(sizeof(Bc)));
    }
    Bc* result = (Bc*) (bucket + 1) + bucket->count++;
    result->opcode = opcode;
    result->res_index = res_index;
    result->a_index = a_index;
    result->b_index = b_index;
    
    if (!bc->module.first_bucket) {
        bc->module.first_bucket = bucket;
    }
    
    bc->bucket = bucket;
    return result;
}

#if 0

struct Bc_Local {
    int index;
    bool is_ref;
};


struct Bytecode_Builder {
    Memory_Arena arena;
    
    map(string_id, int)* globals;
    map(string_id, Bc_Local)* locals;
    array(bool)* registers;
    array(array(int)*)* block_scopes;
    
    Bytecode_Procedure* curr_procedure;
    Bytecode_Instruction* curr_insn;
    
    Data_Packer* data_packer;
    //Interp* interp;
    
    Bytecode_Module module;
    
    u32 block_depth;
    
    u32 next_type_index;
    u32 next_register_index;
};

#if 0
void emit_value_expression(Bytecode_Builder* bc, Ast* expr, int result=-1);

int emit_value_fetch_expression(Bytecode_Builder* bc, Ast* expr);

int emit_reference_expression(Bytecode_Builder* bc, Ast* expr);

void emit_initializing_expression(Bytecode_Builder* bc, Ast* expr, int dest_ptr);

inline void emit_unary_increment(Bytecode_Builder* bc, Ast_Type* type, int result, bool increment);

inline void emit_binary_expression(Bytecode_Builder* bc, Bytecode_Operator opcode,
                                   Ast* lexpr, Ast* rexpr, int result);
inline void emit_assignment_expression(Bytecode_Builder* bc, Bytecode_Operator opcode,
                                       Ast* lexpr, Ast* rexpr, int result=-1);

inline void emit_zero_compare(Bytecode_Builder* bc, Ast_Type* type, int result, int value, bool invert_condition);

void emit_condition_expression(Bytecode_Builder* bc, Ast* cond, int result, bool invert_condition);

void emit_type_cast(Bytecode_Builder* bc, Ast* expr, int result);
inline void emit_array_type_cast(Bytecode_Builder* bc, Ast_Type* t_dest, Ast_Type* t_src, Ast* src_ast, int array_ptr);

void emit_procedure_call(Bytecode_Builder* bc, Ast_Type* type, array(Ast*)* args, Ast* var_args,
                         int result_index, int procedure_ptr_index);

void emit_statement(Bytecode_Builder* bc, Ast* stmt, s32 break_label, s32 continue_label);

Bytecode_Procedure* emit_procedure(Bytecode_Builder* bc, Bytecode_Procedure* func, Ast* ast,
                                   bool is_main, bool insert_debug_break);

void emit_initializer_procedure(Bytecode_Builder* bc);

void validate_bytecode(Bytecode* bytecode);
#endif

Bytecode_Procedure* add_bytecode_procedure(Bytecode_Builder* bc, Ast_Type* type);

Bytecode_Instruction* add_bytecode_instruction(Bytecode_Builder* bc, 
                                               Bytecode_Operator opcode, 
                                               umm size, umm align, cstring loc);

int add_bytecode_global(Bytecode_Builder* bc, Exported_Data exported_data);
int add_bytecode_global(Bytecode_Builder* bc, 
                        Bytecode_Memory_Kind kind, 
                        smm size, smm align,
                        void* init=0, Ast* initializer=0);

#define S1(x) #x
#define S2(x) S1(x)
#define BC_COMMENT __FILE__ ":" S2(__LINE__)

#define bc_instruction(bc, opcode, T) \
(T*) add_bytecode_instruction(bc, opcode, sizeof(##T), alignof(##T))

#define bc_instruction_varindices(bc, opcode, T, arg_count) \
(T*) add_bytecode_instruction(bc, opcode, sizeof(##T)+sizeof(int)*(arg_count), alignof(##T))


Bytecode_Instruction* add_bytecode_instruction(Bytecode_Builder* bc, 
                                               Bytecode_Operator opcode, 
                                               umm size, umm align);

inline int
add_register(Bytecode_Builder* bc, Ast_Type* type=0) {
    assert(bc->curr_procedure);
    int result = (int) array_count(bc->registers);
    array_push(bc->registers, true);
    bc->curr_procedure->register_count = result + 1;
    //pln("Added register r% (largest r%)", f_int(result),
    //f_int(bc->curr_procedure->register_count));
    return result;
}

inline void
drop_register(Bytecode_Builder* bc, int index) {
    assert(bc->curr_procedure);
    assert(index >= 0 && index < array_count(bc->registers) && "invalid register");
    
    if (bc->registers[index]) {
        Bytecode_Result* insn = bc_instruction(bc, BC_DROP, Bytecode_Result);
        insn->res_index = index;
        bc->registers[index] = false;
        //pln("Dropped register r%", f_int(index));
        // TODO: add drop instruction
        //return bc->curr_procedure->register_count++;
    }
}

int
begin_tmp_scope(Bytecode_Builder* bc) {
    return (int) array_count(bc->registers);
}

void
end_tmp_scope(Bytecode_Builder* bc, int first) { 
    for (int i = (int) array_count(bc->registers) - 1; i >= first; i--) {
        drop_register(bc, i);
    }
    //pln("Drop to: %", f_int(bc->next_register));
}

inline int
bc_const_int(Bytecode_Builder* bc, Ast_Type* type, int res_index, s64 val) {
    Bytecode_Const_Int* insn = bc_instruction(bc, BC_INT_CONST, Bytecode_Const_Int);
    insn->type = to_bytecode_type(type);
    insn->res_index = res_index;
    insn->val = val;
    return insn->res_index;
}

inline int
bc_const_f32(Bytecode_Builder* bc, int res_index, f32 val) {
    Bytecode_Const_F32* insn = bc_instruction(bc, BC_F32_CONST, Bytecode_Const_F32);
    insn->type = BC_F32;
    insn->res_index = res_index;
    insn->val = val;
    return insn->res_index;
}

inline int
bc_const_f64(Bytecode_Builder* bc, int res_index, f64 val) {
    Bytecode_Const_F64* insn = bc_instruction(bc, BC_F64_CONST, Bytecode_Const_F64);
    insn->type = BC_F64;
    insn->res_index = res_index;
    insn->val = val;
    return insn->res_index;
}

inline int
bc_const_zero(Bytecode_Builder* bc, Ast_Type* type, int res_index) {
    if (type->storage == TYPE_F32) {
        bc_const_f32(bc, res_index, 0);
    } else if (type->storage == TYPE_F64) {
        bc_const_f64(bc, res_index, 0);
    } else {
        bc_const_int(bc, type, res_index, 0);
    }
    return res_index;
}

inline int
bc_return(Bytecode_Builder* bc, Ast_Type* type, int res_index) {
    Bytecode_Result* insn = bc_instruction(bc, BC_RETURN, Bytecode_Result);
    insn->type = to_bytecode_type(type);
    insn->res_index = res_index;
    return insn->res_index;
}

inline int
bc_unary_arith(Bytecode_Builder* bc, Bytecode_Type type, Bytecode_Operator opcode, int res_index, int a_index) {
    Bytecode_Unary* insn = bc_instruction(bc, opcode, Bytecode_Unary);
    insn->type = type;
    insn->res_index = res_index;
    insn->a_index = a_index;
    return insn->res_index;
}

inline int
bc_binary_arith(Bytecode_Builder* bc, Bytecode_Type type, Bytecode_Operator opcode,
                int res_index, int a_index, int b_index) {
    assert(opcode != BC_COPY);
    Bytecode_Binary* insn = bc_instruction(bc, opcode, Bytecode_Binary);
    insn->type = type;
    insn->res_index = res_index;
    insn->a_index = a_index;
    insn->b_index = b_index;
    return insn->res_index;
}

inline int
bc_assignment(Bytecode_Builder* bc, Bytecode_Operator opcode, Bytecode_Type type, int dest, int src, cstring comment=0) {
    Bytecode_Assign* insn = bc_instruction(bc, opcode, Bytecode_Assign);
    insn->type = type;
    insn->dest_index = dest;
    insn->src_index = src;
    insn->comment = comment;
    return insn->dest_index;
}

#define bc_load(bc, type, dest, src) _bc_load(bc, type, dest, src, BC_COMMENT)

inline int 
_bc_load(Bytecode_Builder* bc, Ast_Type* type, int dest, int src, cstring comment=0) {
    //Bytecode_Type bc_type = register_type(bc->curr_procedure, src);
    //assert(bc_type.kind == BC_PTR && "expected BC_PTR to load");
    return bc_assignment(bc, BC_LOAD, to_bytecode_type(type), dest, src, comment);
}

inline int
bc_lea(Bytecode_Builder* bc, int dest, int src) {
    return bc_assignment(bc, BC_LEA, BC_PTR, dest, src);
}

inline int
bc_store(Bytecode_Builder* bc, int dest, int src) {
    return bc_assignment(bc, BC_STORE, BC_VOID, dest, src);
}

inline int
bc_copy(Bytecode_Builder* bc, int dest, int src) {
    return bc_assignment(bc, BC_COPY, BC_VOID, dest, src);
}

inline int
bc_cast(Bytecode_Builder* bc, Bytecode_Operator opcode, Ast_Type* type, int dest, int src) {
    return bc_assignment(bc, opcode, to_bytecode_type(type), dest, src);
}

inline int 
bc_field_access(Bytecode_Builder* bc, int res_index, int base, s32 offset) {
    Bytecode_Field_Access* insn = bc_instruction(bc, BC_FIELD_ACCESS, Bytecode_Field_Access);
    insn->type = BC_PTR;
    insn->res_index = res_index;
    insn->base = base;
    insn->offset = offset;
    return insn->res_index;
}

inline void
bc_memcpy(Bytecode_Builder* bc, int dest, int src, int size) {
    Bytecode_Memcpy* insn = bc_instruction(bc, BC_MEMCPY, Bytecode_Memcpy);
    insn->dest_index = dest;
    insn->src_index = src;
    insn->size = size;
}

inline void
bc_memset(Bytecode_Builder* bc, int dest, int value, int size) {
    Bytecode_Memset* insn = bc_instruction(bc, BC_MEMSET, Bytecode_Memset);
    insn->dest_index = dest;
    insn->value = value;
    insn->size = size;
}

inline int 
bc_array_access(Bytecode_Builder* bc, Ast_Type* elem_type, 
                int res_index, int base, int index) {
    
    //Bytecode_Type bc_type = register_type(bc->curr_procedure, base);
    //assert(bc_type.kind == BC_PTR && "expected array base to be BC_PTR");
    
    Bytecode_Array_Access* insn = bc_instruction(bc, BC_ARRAY_ACCESS, Bytecode_Array_Access);
    insn->type = BC_PTR;
    insn->res_index = res_index;
    insn->base = base;
    insn->index = index;
    insn->stride = (int) get_array_element_size(elem_type);
    return insn->res_index;
}

inline void
bc_branch_if(Bytecode_Builder* bc, int label_index, int cond) {
    assert(cond >= 0 && "missing condition");
    Bytecode_Branch* branch = bc_instruction(bc, BC_BRANCH, Bytecode_Branch);
    branch->label_index = label_index;
    branch->cond = cond;
}

inline void
bc_branch(Bytecode_Builder* bc, int label_index) {
    Bytecode_Branch* branch = bc_instruction(bc, BC_BRANCH, Bytecode_Branch);
    branch->label_index = label_index;
    branch->cond = -1;
}

internal inline void
_bc_copy_registers(void* dest, void* src, smm count) {
    memcpy(dest, src, sizeof(int)*count);
}

inline void
bc_call(Bytecode_Builder* bc, Ast_Type* return_type, u32 proc_index, array(int)* args) {
    Bytecode_Call* call = bc_instruction_varindices(bc, BC_CALL, Bytecode_Call, array_count(args));
    call->type = to_bytecode_type(return_type);
    call->proc_index = proc_index;
    call->arg_count = (s32) array_count(args);
    _bc_copy_registers(bc_call_args(call), args, array_count(args));
}

inline void
bc_call_indirect(Bytecode_Builder* bc, Ast_Type* return_type, int proc_ptr_index, s32 ret_count, array(int)* args) {
    Bytecode_Call_Indirect* call = bc_instruction_varindices(bc, BC_CALL_INDIRECT, Bytecode_Call_Indirect, array_count(args));
    call->type = to_bytecode_type(return_type);
    call->proc_ptr_index = proc_ptr_index;
    call->ret_count = ret_count;
    call->arg_count = (s32) array_count(args);
    _bc_copy_registers(bc_call_args(call), args, array_count(args));
}

inline int*
bc_intrinsic(Bytecode_Builder* bc, Bytecode_Operator opcode, s32 ret_count, s32 arg_count) {
    Bytecode_Call_Indirect* call = bc_instruction_varindices(bc, opcode, Bytecode_Call_Indirect, arg_count);
    call->proc_ptr_index = -1;
    call->ret_count = ret_count;
    call->arg_count = arg_count;
    return bc_call_args(call);
}

inline int
bc_global(Bytecode_Builder* bc, int res_index, int global_index) {
    Bytecode_Assign* insn = bc_instruction(bc, BC_GLOBAL, Bytecode_Assign);
    insn->type = BC_PTR;
    insn->dest_index = res_index;
    insn->src_index = global_index;
    return insn->dest_index;
}

inline int
bc_local(Bytecode_Builder* bc, Ast_Type* type) {
    assert(bc->curr_procedure);
    int result = add_register(bc, t_void_ptr);
    Bytecode_Local* insn =  bc_instruction(bc, BC_LOCAL, Bytecode_Local);
    insn->type = BC_PTR;
    insn->res_index = result;
    insn->size = type->size;
    insn->align = type->align;
    return result;
}

inline int
bc_procedure(Bytecode_Builder* bc, int res_index, int proc_index) {
    assert(bc->curr_procedure);
    Bytecode_Assign* insn =  bc_instruction(bc, BC_FUNCTION, Bytecode_Assign);
    insn->type = BC_PTR;
    insn->dest_index = res_index;
    insn->src_index = proc_index;
    return insn->dest_index;
}

inline int
bc_begin_block(Bytecode_Builder* bc, Bytecode_Operator opcode=BC_BLOCK) {
    bc_instruction(bc, opcode, Bytecode_Instruction);
    bc->curr_procedure->block_count++;
    bc->block_depth++;
    return begin_tmp_scope(bc);
}

inline int
bc_begin_loop_block(Bytecode_Builder* bc) {
    return bc_begin_block(bc, BC_LOOP);
}

inline void
bc_end_block(Bytecode_Builder* bc, int first_register=-1) {
    assert(bc->block_depth > 0);
    bc_instruction(bc, BC_END, Bytecode_Instruction);
    bc->block_depth--;
    if (first_register >= 0) {
        end_tmp_scope(bc, first_register);
    }
}

void string_builder_dump_bytecode_type(String_Builder* sb, Bytecode_Type type);
void print_bytecode_type(Bytecode_Type type);
void string_builder_dump_bytecode_globals(String_Builder* sb, Bytecode* bc);
void string_bc_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn);
void string_bc_dump_bytecode(String_Builder* sb, Bytecode* bc, Bytecode_Procedure* func, Ast_Type* type=0);
void string_builder_dump_bytecode_procedure(String_Builder* sb, Bytecode* bc, Bytecode_Procedure* func);
void dump_bytecode(Bytecode* bc);

#endif
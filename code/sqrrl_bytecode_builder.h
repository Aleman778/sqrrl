
struct Bc_Local {
    int index;
    bool is_ref;
};

struct Bytecode_Builder {
    Memory_Arena arena;
    
    map(string_id, Bc_Local)* locals;
    map(string_id, int)* globals;
    
    Bytecode_Function* curr_function;
    Bytecode_Instruction* curr_insn;
    
    Data_Packer* data_packer;
    Interp* interp;
    
    Bytecode bytecode;
    
    u32 block_depth;
    
    u32 next_type_index;
    u32 next_register_index;
};

inline Bytecode_Operator
to_bytecode_opcode(Operator op, Type* type) {
    bool is_signed = (type->kind == TypeKind_Basic &&
                      type->Basic.flags & (BasicFlag_Unsigned | BasicFlag_Floating));
    int op_index = op*2 + (is_signed ? 1 : 0);
    return bytecode_operator_table[op_index];
}

Bytecode_Type
to_bytecode_type(Type* type) {
    Bytecode_Type result = {};
    
    switch (type->kind) {
        case TypeKind_Basic: {
            result.size = (u8) type->size;
            
            switch (type->Basic.kind) {
                case Basic_s8:;
                case Basic_s16:
                case Basic_s32:
                case Basic_s64: {
                    result.kind = BC_TYPE_INT;
                    result.flags = BC_FLAG_SIGNED;
                } break;
                
                case Basic_bool:
                case Basic_u8:
                case Basic_u16:
                case Basic_u32:
                case Basic_u64: {
                    result.kind = BC_TYPE_INT;
                } break;
                
                case Basic_f32:
                case Basic_f64: {
                    result.kind = BC_TYPE_FLOAT;
                } break;
                
                case Basic_string: {
                    result.kind = BC_TYPE_PTR;
                } break;
                
                case Basic_cstring: {
                    result.kind = BC_TYPE_PTR;
                } break;
                
                default: unimplemented; break;
            }
        } break;
        
        case TypeKind_Struct:
        case TypeKind_Union: 
        case TypeKind_Function:
        case TypeKind_Type:
        case TypeKind_Array:
        case TypeKind_Pointer: {
            result.kind = BC_TYPE_PTR;
        } break;
        
        case TypeKind_Enum: {
            return to_bytecode_type(type->Enum.type);
        } break;
    }
    
    return result;
}

inline int
add_register(Bytecode_Builder* bc, Type* type=0) {
    assert(bc->curr_function);
    //pln("Added register r%", f_int(bc->curr_function->register_count));
    return bc->curr_function->register_count++;
}

inline void
drop_register(Bytecode_Builder* bc, int index) {
    assert(bc->curr_function);
    assert(index >= 0 && "invalid register");
    //pln("Dropped register r%", f_int(index));
    // TODO: add drop instruction
    //return bc->curr_function->register_count++;
}

void emit_value_expression(Bytecode_Builder* bc, Ast* expr, int result=-1);

int emit_value_fetch_expression(Bytecode_Builder* bc, Ast* expr);

int emit_reference_expression(Bytecode_Builder* bc, Ast* expr);

void emit_initializing_expression(Bytecode_Builder* bc, Ast* expr, int dest_ptr);

inline void emit_unary_increment(Bytecode_Builder* bc, Type* type, int result, bool increment);

inline void emit_binary_expression(Bytecode_Builder* bc, Bytecode_Operator opcode,
                                   Ast* lexpr, Ast* rexpr, int result);
inline void emit_assignment_expression(Bytecode_Builder* bc, Bytecode_Operator opcode,
                                       Ast* lexpr, Ast* rexpr, int result=-1);

inline void emit_zero_compare(Bytecode_Builder* bc, Type* type, int result, int value, bool invert_condition);

void emit_condition_expression(Bytecode_Builder* bc, Ast* cond, int result, bool invert_condition);

void emit_type_cast(Bytecode_Builder* bc, Ast* expr, int result);
inline void emit_array_type_cast(Bytecode_Builder* bc, Type* t_dest, Type* t_src, Ast* src_ast, int array_ptr);

void emit_function_call(Bytecode_Builder* bc, Type* type, array(Ast*)* args, Ast* var_args,
                        int result_index, int function_ptr_index);

void emit_statement(Bytecode_Builder* bc, Ast* stmt, s32 break_label, s32 continue_label);

Bytecode_Function* emit_function(Bytecode_Builder* bc, Bytecode_Function* func, Ast* ast,
                                 bool is_main, bool insert_debug_break);

Bytecode_Function* add_bytecode_function(Bytecode_Builder* bc, Type* type);

Bytecode_Instruction* add_bytecode_insn(Bytecode_Builder* bc, 
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
bc_const_int(Bytecode_Builder* bc, Type* type, int res_index, s64 val) {
    Bytecode_Const_Int* insn = bc_instruction(bc, BC_INT_CONST, Bytecode_Const_Int);
    insn->type = to_bytecode_type(type);
    insn->res_index = res_index;
    insn->val = val;
    return insn->res_index;
}

inline int
bc_const_f32(Bytecode_Builder* bc, int res_index, f32 val) {
    Bytecode_Const_F32* insn = bc_instruction(bc, BC_F32_CONST, Bytecode_Const_F32);
    insn->type = bc_type(BC_TYPE_FLOAT, 0, 4);
    insn->res_index = res_index;
    insn->val = val;
    return insn->res_index;
}

inline int
bc_const_f64(Bytecode_Builder* bc, int res_index, f64 val) {
    Bytecode_Const_F64* insn = bc_instruction(bc, BC_F64_CONST, Bytecode_Const_F64);
    insn->type = bc_type(BC_TYPE_FLOAT, 0, 8);
    insn->res_index = res_index;
    insn->val = val;
    return insn->res_index;
}

inline int
bc_const_zero(Bytecode_Builder* bc, Type* type, int res_index) {
    if (type->kind == TypeKind_Basic) {
        if (type->Basic.kind == Basic_f32) {
            bc_const_f32(bc, res_index, 0);
        } else if (type->Basic.kind == Basic_f64) {
            bc_const_f64(bc, res_index, 0);
        } else {
            bc_const_int(bc, type, res_index, 0);
        } 
    } else {
        bc_const_int(bc, type, res_index, 0);
    }
    return res_index;
}

inline int
bc_return(Bytecode_Builder* bc, Type* type, int res_index) {
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
_bc_load(Bytecode_Builder* bc, Type* type, int dest, int src, cstring comment=0) {
    //Bytecode_Type bc_type = register_type(bc->curr_function, src);
    //assert(bc_type.kind == BC_TYPE_PTR && "expected BC_TYPE_PTR to load");
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
bc_cast(Bytecode_Builder* bc, Bytecode_Operator opcode, Type* type, int dest, int src) {
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
bc_array_access(Bytecode_Builder* bc, Type* elem_type, 
                int res_index, int base, int index) {
    
    //Bytecode_Type bc_type = register_type(bc->curr_function, base);
    //assert(bc_type.kind == BC_TYPE_PTR && "expected array base to be BC_TYPE_PTR");
    
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
bc_call(Bytecode_Builder* bc, Type* return_type, u32 func_index, array(int)* args) {
    Bytecode_Call* call = bc_instruction_varindices(bc, BC_CALL, Bytecode_Call, array_count(args));
    call->type = to_bytecode_type(return_type);
    call->func_index = func_index;
    call->arg_count = (s32) array_count(args);
    _bc_copy_registers(bc_call_args(call), args, array_count(args));
}

inline void
bc_call_indirect(Bytecode_Builder* bc, Type* return_type, int func_ptr_index, s32 ret_count, array(int)* args) {
    Bytecode_Call_Indirect* call = bc_instruction_varindices(bc, BC_CALL_INDIRECT, Bytecode_Call_Indirect, array_count(args));
    call->type = to_bytecode_type(return_type);
    call->func_ptr_index = func_ptr_index;
    call->ret_count = ret_count;
    call->arg_count = (s32) array_count(args);
    _bc_copy_registers(bc_call_args(call), args, array_count(args));
}

inline int*
bc_intrinsic(Bytecode_Builder* bc, Bytecode_Operator opcode, s32 ret_count, s32 arg_count) {
    Bytecode_Call_Indirect* call = bc_instruction_varindices(bc, opcode, Bytecode_Call_Indirect, arg_count);
    call->func_ptr_index = -1;
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
bc_local(Bytecode_Builder* bc, Type* type) {
    assert(bc->curr_function);
    int result = add_register(bc, t_void_ptr);
    Bytecode_Local* insn =  bc_instruction(bc, BC_LOCAL, Bytecode_Local);
    insn->type = BC_PTR;
    insn->res_index = result;
    insn->size = type->size;
    insn->align = type->align;
    return result;
}

inline int
bc_function(Bytecode_Builder* bc, int res_index, int func_index) {
    assert(bc->curr_function);
    Bytecode_Assign* insn =  bc_instruction(bc, BC_FUNCTION, Bytecode_Assign);
    insn->type = BC_PTR;
    insn->dest_index = res_index;
    insn->src_index = func_index;
    return insn->dest_index;
}

inline void
bc_begin_block(Bytecode_Builder* bc, Bytecode_Operator opcode=BC_BLOCK) {
    bc_instruction(bc, opcode, Bytecode_Instruction);
    bc->curr_function->block_count++;
    bc->block_depth++;
}

inline void
bc_begin_loop_block(Bytecode_Builder* bc) {
    bc_begin_block(bc, BC_LOOP);
}

inline void
bc_end_block(Bytecode_Builder* bc) {
    assert(bc->block_depth > 0);
    bc_instruction(bc, BC_END, Bytecode_Instruction);
    bc->block_depth--;
}

void string_builder_dump_bytecode_type(String_Builder* sb, Bytecode_Type type);
void print_bytecode_type(Bytecode_Type type);
void string_builder_dump_bytecode_globals(String_Builder* sb, Bytecode* bc);
void string_bc_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn);
void string_bc_dump_bytecode(String_Builder* sb, Bytecode* bc, Bytecode_Function* func, Type* type=0);
void string_builder_dump_bytecode_function(String_Builder* sb, Bytecode* bc, Bytecode_Function* func);
void dump_bytecode(Bytecode* bc);
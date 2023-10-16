
struct Bc_Local {
    int ptr; // if ptr == -1 then use val instead
    int val;
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
to_bytecode_type(Bytecode_Builder* bc, Type* type) {
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
                    result.size = 0;
                } break;
                
                case Basic_cstring: {
                    result.kind = BC_TYPE_PTR;
                    result.size = 0;
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
            return to_bytecode_type(bc, type->Enum.type);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return result;
}

inline Type*
normalize_type_for_casting(Type* type) {
    // Make similar types 
    if (type->kind == TypeKind_Pointer || 
        type->kind == TypeKind_Type || 
        type == t_cstring) {
        
        type = t_s64;
    }
    
    if (type->kind == TypeKind_Enum) { 
        type = type->Enum.type;
    }
    
    return type;
}

inline int
add_bytecode_register(Bytecode_Builder* bc, Type* type) {
    assert(bc->curr_function);
    int result = bc->curr_function->register_count++;
    Bytecode_Type reg_type = to_bytecode_type(bc, type);
    array_push(bc->curr_function->register_types, reg_type);
    return result;
}

void emit_value_expression(Bytecode_Builder* bc, Ast* expr, int result=-1);

int emit_value_fetch_expression(Bytecode_Builder* bc, Ast* expr, int result=-1);

int emit_reference_expression(Bytecode_Builder* bc, Ast* expr);

void emit_initializing_expression(Bytecode_Builder* bc, Ast* expr, int dest_ptr);

inline void emit_binary_expression(Bytecode_Builder* bc, Bytecode_Operator opcode,
                                   Ast* lexpr, Ast* rexpr, int result);
inline void emit_assignment_expression(Bytecode_Builder* bc, Bytecode_Operator opcode,
                                       Ast* lexpr, Ast* rexpr, int result=-1);

inline void emit_zero_compare(Bytecode_Builder* bc, Type* type, int result, int value, bool invert_condition);

void emit_condition_expression(Bytecode_Builder* bc, Ast* cond, int result, bool invert_condition);

void emit_type_cast(Bytecode_Builder* bc, Ast* expr, int result);
inline void emit_array_type_cast(Bytecode_Builder* bc, Type* t_dest, Type* t_src, Ast* src_ast, int array_ptr);

int emit_function_call(Bytecode_Builder* bc, Type* type, array(Ast*)* args, int function_ptr=-1);

void emit_statement(Bytecode_Builder* bc, Ast* stmt, s32 break_label, s32 continue_label);

Bytecode_Function* emit_function(Bytecode_Builder* bc, Bytecode_Function* func, Ast* ast,
                                 bool is_main, bool insert_debug_break);

Bytecode_Function* add_bytecode_function(Bytecode_Builder* bc, Type* type);

Bytecode_Instruction* add_bytecode_insn(Bytecode_Builder* bc, 
                                        Bytecode_Operator opcode, 
                                        umm size, umm align, cstring loc);

int add_bytecode_global(Bytecode_Builder* bc, 
                        Bytecode_Memory_Kind kind, 
                        smm size, smm align, 
                        void* init=0);

#define S1(x) #x
#define S2(x) S1(x)
#define BC_COMMENT __FILE__ ":" S2(__LINE__)

#define bc_instruction(bc, opcode, T, comment) \
(T*) add_bytecode_instruction(bc, opcode, sizeof(##T), alignof(##T), comment)

Bytecode_Instruction* add_bytecode_instruction(Bytecode_Builder* bc, 
                                               Bytecode_Operator opcode, 
                                               umm size, umm align, cstring comment);

#define bc_const_int(bc, result_index, val) bc_insn_const_int(bc, result_index, val, BC_COMMENT)

inline int
bc_insn_const_int(Bytecode_Builder* bc, int result_index, s64 val, cstring comment) {
    Bytecode_Const_Int* insn = bc_instruction(bc, BC_INT_CONST, Bytecode_Const_Int, comment);
    insn->res_index = result_index;
    insn->val = val;
    insn->comment = comment;
    return insn->res_index;
}

#define bc_const_f32(bc, result_index, val) \
_bc_const_f32(bc, result_index, val, BC_COMMENT)

inline int
_bc_const_f32(Bytecode_Builder* bc, int result_index, f32 val, cstring comment) {
    Bytecode_Const_F32* insn = bc_instruction(bc, BC_F32_CONST, Bytecode_Const_F32, comment);
    insn->res_index = result_index;
    insn->val = val;
    insn->comment = comment;
    return insn->res_index;
}

#define bc_const_f64(bc, result_index, val) \
bc_instruction_const_f64(bc, result_index, val, BC_COMMENT)

inline int
_bc_const_f64(Bytecode_Builder* bc, int result_index, f64 val, cstring comment) {
    Bytecode_Const_F64* insn = bc_instruction(bc, BC_F64_CONST, Bytecode_Const_F64, comment);
    insn->res_index = result_index;
    insn->val = val;
    insn->comment = comment;
    return insn->res_index;
}

#define bc_unary_arith(bc, result_and_a_index) \
_bc_unary_arith(bc, result_and_a_index, BC_COMMENT)

int
_bc_unary_arith(Bytecode_Builder* bc, Bytecode_Operator opcode, int result_index, int a_index, cstring comment) {
    Bytecode_Unary* insn = bc_instruction(bc, opcode, Bytecode_Unary, comment);
    insn->res_index = result_index;
    insn->a_index = a_index;
    insn->comment = comment;
    return insn->res_index;
}


#define bc_binary_arith(bc, result_index, a_index, b_index) \
_bc_binary_arith(bc, result_index, a_index, b_index, BC_COMMENT)

int
_bc_binary_arith(Bytecode_Builder* bc, Bytecode_Operator opcode, int result_index, int a_index, int b_index, cstring comment) {
    Bytecode_Binary* insn = bc_instruction(bc, opcode, Bytecode_Binary, comment);
    insn->res_index = result_index;
    insn->a_index = a_index;
    insn->b_index = b_index;
    insn->comment = comment;
    return insn->res_index;
}


#define bc_load(bc, dest, src) \
_bc_load(bc, dest, src, BC_COMMENT)

inline int 
_bc_load(Bytecode_Builder* bc, int dest, int src, cstring comment) {
    Bytecode_Type bc_type = register_type(bc->curr_function, src);
    assert(bc_type.kind == BC_TYPE_PTR && "expected BC_TYPE_PTR to load");
    
    Bytecode_Assign* insn = bc_instruction(bc, BC_LOAD, Bytecode_Assign, comment);
    insn->dest_index = dest;
    insn->src_index = src;
    insn->comment = comment;
    return insn->dest_index;
}

#define bc_store(bc, dest, src) \
_bc_store(bc, dest, src, BC_COMMENT)

inline void
_bc_store(Bytecode_Builder* bc, int dest, int src,
          cstring comment) {
    Bytecode_Assign* insn = bc_instruction(bc, BC_STORE, Bytecode_Assign, comment);
    insn->dest_index = dest;
    insn->src_index = src;
    insn->comment = comment;
}

#define bc_field_access(bc, result, base, offset) \
_bc_field_access(bc, result, base, offset, BC_COMMENT)

inline int 
_bc_field_access(Bytecode_Builder* bc, int result, int base, s32 offset, cstring comment) {
    
    Bytecode_Field_Access* insn = bc_instruction(bc, BC_FIELD_ACCESS, Bytecode_Field_Access, comment);
    insn->res_index = result;
    insn->base = base;
    insn->offset = offset;
    insn->comment = comment;
    return insn->res_index;
}

#define bc_memcpy(bc, dest, src, size, ...) \
_bc_memcpy(bc, dest, src, size, BC_COMMENT, __VA_ARGS__)

inline void
_bc_memcpy(Bytecode_Builder* bc, int dest, int src, int size, cstring comment, int is_fixed_size=true) {
    Bytecode_Memcpy* insn = bc_instruction(bc, BC_MEMCPY, Bytecode_Memcpy, comment);
    insn->dest_index = dest;
    insn->src_index = src;
    insn->size_index = size;
    insn->is_fixed_size = is_fixed_size;
    insn->comment = comment;
}

#define bc_array_access(bc, type, result, base, index) \
_bc_array_access(bc, type, result, base, index, BC_COMMENT)

inline int 
_bc_array_access(Bytecode_Builder* bc, Type* elem_type, 
                 int result, int base, int index, cstring comment) {
    
    Bytecode_Type bc_type = register_type(bc->curr_function, base);
    assert(bc_type.kind == BC_TYPE_PTR && "expected array base to be BC_TYPE_PTR");
    
    Bytecode_Array_Access* insn = bc_instruction(bc, BC_ARRAY_ACCESS, Bytecode_Array_Access, comment);
    insn->res_index = result;
    insn->base = base;
    insn->index = index;
    insn->stride = (int) align_forward(elem_type->size, elem_type->align);
    insn->comment = comment;
    return insn->res_index;
}

#define bc_branch(bc, label_index, cond) \
_bc_branch(bc, label_index, cond, BC_COMMENT)

inline void
_bc_branch(Bytecode_Builder* bc, int label_index, int cond, cstring comment) {
    Bytecode_Branch* branch = bc_instruction(bc, BC_BRANCH, Bytecode_Branch, comment);
    branch->label_index = label_index;
    branch->cond = cond;
    branch->comment = comment;
}

#define bc_call(bc, func_index, reg_count) \
_bc_call(bc, func_index, reg_count, BC_COMMENT)

int*
_bc_call(Bytecode_Builder* bc, u32 func_index, int reg_count, cstring comment) {
    Bytecode_Call* call = (Bytecode_Call*) 
        add_bytecode_instruction(bc, BC_CALL, 
                                 sizeof(Bytecode_Binary) + sizeof(int)*reg_count,
                                 alignof(Bytecode_Binary), comment);
    call->func_index = func_index;
    return bc_call_args(call);
}

#define bc_call_indirect(bc, func_ptr_index, ret_count, arg_count) \
_bc_call_indirect(bc, BC_CALL_INDIRECT, func_ptr_index, ret_count, arg_count, BC_COMMENT)

#define bc_call_intrinsic(bc, intrinsic_opcode, ret_count, arg_count) \
_bc_call_indirect(bc, intrinsic_opcode, -1, ret_count, arg_count, BC_COMMENT)

int*
_bc_call_indirect(Bytecode_Builder* bc, Bytecode_Operator opcode, int func_ptr_index, 
                  s32 ret_count, s32 arg_count, cstring comment) {
    
    int reg_count = ret_count + arg_count;
    Bytecode_Call_Indirect* call = (Bytecode_Call_Indirect*) 
        add_bytecode_instruction(bc, opcode, sizeof(Bytecode_Binary) + sizeof(int)*reg_count,
                                 alignof(Bytecode_Binary), comment);
    call->func_ptr_index = func_ptr_index;
    call->ret_count = ret_count;
    call->arg_count = arg_count;
    return bc_call_args(call);
}

#define bc_global(bc, result_index, global_index) \
_bc_global(bc, result_index, global_index, BC_COMMENT)

inline int
_bc_global(Bytecode_Builder* bc, int result_index, int global_index, cstring comment) {
    Bytecode_Assign* result = bc_instruction(bc, BC_GLOBAL, Bytecode_Assign, comment);
    result->dest_index = result_index;
    result->src_index = global_index;
    result->comment = comment;
    return result->dest_index;
}

#define bc_local(bc, type) \
_bc_local(bc, type, BC_COMMENT)

inline int
_bc_local(Bytecode_Builder* bc, Type* type, cstring comment) {
    assert(bc->curr_function);
    int result = add_bytecode_register(bc, t_void_ptr);
    Bytecode_Local* insn =  bc_instruction(bc, BC_LOCAL, Bytecode_Local, comment);
    insn->res_index = result;
    insn->size = type->size;
    insn->align = type->align;
    return result;
}

#define bc_function(bc, result_index, func_index) \
_bc_function(bc, result_index, func_index, BC_COMMENT)

inline int
_bc_function(Bytecode_Builder* bc, int result_index, int func_index, cstring comment) {
    assert(bc->curr_function);
    Bytecode_Assign* insn =  bc_instruction(bc, BC_FUNCTION, Bytecode_Assign, comment);
    insn->dest_index = result_index;
    insn->src_index = func_index;
    return insn->dest_index;
}


#define bc_begin_block(bc) _bc_begin_block(bc, BC_BLOCK, BC_COMMENT)
#define bc_begin_loop_block(bc) _bc_begin_block(bc, BC_LOOP, BC_COMMENT)
#define bc_end_block(bc) _bc_end_block(bc, BC_COMMENT)

inline void
_bc_begin_block(Bytecode_Builder* bc, Bytecode_Operator opcode, cstring comment) {
    bc_instruction(bc, opcode, Bytecode_Instruction, comment);
    bc->curr_function->block_count++;
    bc->block_depth++;
}

inline void
_bc_end_block(Bytecode_Builder* bc, cstring comment) {
    assert(bc->block_depth > 0);
    bc_instruction(bc, BC_END, Bytecode_Instruction, comment);
    bc->block_depth--;
}

void string_bc_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn);
void string_bc_dump_bytecode(String_Builder* sb, Bytecode* bc, Bytecode_Function* func, Type* type=0);
void string_builder_dump_bytecode_function(String_Builder* sb, Bytecode* bc, Bytecode_Function* func);
void dump_bytecode(Bytecode* bc);
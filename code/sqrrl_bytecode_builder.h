
struct Bytecode_Builder {
    Memory_Arena arena;
    
    map(string_id, int)* locals;
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

bool
is_aggregate_type(Type* type) {
    return (type->kind == TypeKind_Struct ||
            type->kind == TypeKind_Union ||
            type->kind == TypeKind_Array);
}

int convert_lvalue_expression_to_bytecode(Bytecode_Builder* bc, Ast* expr);

int convert_expression_to_bytecode(Bytecode_Builder* bc, Ast* expr);

int convert_condition_to_bytecode(Bytecode_Builder* bc, Ast* cond, bool invert_condition=false);

int convert_type_cast_to_bytecode(Bytecode_Builder* bc, Ast* expr);

int convert_function_call_to_bytecode(Bytecode_Builder* bc, Type* type, array(Ast*)* args, int function_ptr=-1);

void convert_statement_to_bytecode(Bytecode_Builder* bc, Ast* stmt, s32 break_label, s32 continue_label);

Bytecode_Function* convert_function_to_bytecode(Bytecode_Builder* bc, Bytecode_Function* func, Ast* ast,
                                                bool is_main, bool insert_debug_break);

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

Bytecode_Function* add_bytecode_function(Bytecode_Builder* bc, Type* type);

Bytecode_Instruction* add_bytecode_insn(Bytecode_Builder* bc, 
                                        Bytecode_Operator opcode, 
                                        Bytecode_Instruction_Kind kind, 
                                        umm size, umm align, cstring loc);

int add_bytecode_global(Bytecode_Builder* bc, 
                        Bytecode_Memory_Kind kind, 
                        smm size, smm align, 
                        void* init=0);


inline int
add_bytecode_register(Bytecode_Builder* bc, Type* type) {
    assert(bc->curr_function);
    int result = bc->curr_function->register_count++;
    Bytecode_Type reg_type = to_bytecode_type(bc, type);
    array_push(bc->curr_function->register_types, reg_type);
    return result;
}

#define S1(x) #x
#define S2(x) S1(x)
#define add_insn(bc, opcode) add_bytecode_insn(bc, opcode, \
BytecodeInstructionKind_Base, \
sizeof(Bytecode_Instruction), \
alignof(Bytecode_Instruction), \
__FILE__ ":" S2(__LINE__));

#define add_insn_t(bc, opcode, T) (Bytecode_##T*) add_bytecode_insn(bc, opcode, \
BytecodeInstructionKind_##T, \
sizeof(Bytecode_##T), \
alignof(Bytecode_##T), \
__FILE__ ":" S2(__LINE__));

#define bc_instruction_call(bc, target_cu, arg_count) \
(Bytecode_Binary*) add_bytecode_insn(bc, target_cu ? BC_CALL : BC_CALL_INDIRECT, \
BytecodeInstructionKind_Binary, \
sizeof(Bytecode_Binary) + sizeof(int)*arg_count, \
alignof(Bytecode_Binary), \
__FILE__ ":" S2(__LINE__))

#define bc_instruction(bc, opcode, res, arg0, arg1) \
_bc_instruction(bc, opcode, res, arg0, arg1, __FILE__ ":" S2(__LINE__))

inline Bytecode_Binary*
_bc_instruction(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                int res, int arg0, int arg1,
                cstring comment) {
    
    Bytecode_Binary* insn = add_insn_t(bc, opcode, Binary);
    insn->res_index = res;
    insn->arg0_index = arg0;
    insn->arg1_index = arg1;
    insn->comment = comment;
    return insn;
}

#define bc_instruction_const_int(bc, type, val, ...) \
_bc_instruction_const_int(bc, type, val, __FILE__ ":" S2(__LINE__), __VA_ARGS__)

inline int
_bc_instruction_const_int(Bytecode_Builder* bc, Type* type, s64 val, cstring comment, int res_index=-1) {
    Bytecode_Binary* insn = add_insn_t(bc, BC_INT_CONST, Binary);
    insn->res_index = res_index >= 0 ? res_index : add_bytecode_register(bc, type);
    insn->const_i64 = val;
    insn->comment = comment;
    return insn->res_index;
}

#define bc_instruction_const_f32(bc, val) \
_bc_instruction_const_f32(bc, val, __FILE__ ":" S2(__LINE__))

inline int
_bc_instruction_const_f32(Bytecode_Builder* bc, f32 val, cstring comment) {
    Bytecode_Binary* insn = add_insn_t(bc, BC_F32_CONST, Binary);
    insn->res_index = add_bytecode_register(bc, t_f32);
    insn->const_f32 = val;
    insn->comment = comment;
    return insn->res_index;
}

#define bc_instruction_const_f64(bc, val) \
_bc_instruction_const_f64(bc, val, __FILE__ ":" S2(__LINE__))

inline int
_bc_instruction_const_f64(Bytecode_Builder* bc, f64 val, cstring comment) {
    Bytecode_Binary* insn = add_insn_t(bc, BC_F64_CONST, Binary);
    insn->res_index = add_bytecode_register(bc, t_f64);
    insn->const_f64 = val;
    insn->comment = comment;
    return insn->res_index;
}

#define bc_instruction_load(bc, type, src) \
_bc_instruction_load(bc, type, src, __FILE__ ":" S2(__LINE__))

inline int 
_bc_instruction_load(Bytecode_Builder* bc, Type* type, int src, cstring comment) {
    Bytecode_Type bc_type = register_type(bc->curr_function, src);
    assert(bc_type.kind == BC_TYPE_PTR && "expected BC_TYPE_PTR to load");
    
    Bytecode_Binary* insn = add_insn_t(bc, BC_LOAD, Binary);
    insn->res_index = add_bytecode_register(bc, type);
    insn->arg0_index = src;
    insn->arg1_index = -1;
    insn->comment = comment;
    return insn->res_index;
}

#define bc_instruction_store(bc, dest, src) \
_bc_instruction_store(bc, dest, src, __FILE__ ":" S2(__LINE__))

inline void
_bc_instruction_store(Bytecode_Builder* bc, int dest, int src,
                      cstring comment) {
    Bytecode_Binary* result = add_insn_t(bc, BC_STORE, Binary);
    result->res_index = -1;
    result->arg0_index = dest;
    result->arg1_index = src;
    result->comment = comment;
}

#define bc_instruction_array_accesss(bc, type, base, index) \
_bc_instruction_array_accesss(bc, type, base, index, __FILE__ ":" S2(__LINE__))

inline int 
_bc_instruction_array_accesss(Bytecode_Builder* bc, Type* type, int base, int index, cstring comment) {
    Bytecode_Type bc_type = register_type(bc->curr_function, base);
    assert(bc_type.kind == BC_TYPE_PTR && "expected array base to be BC_TYPE_PTR");
    
    Bytecode_Binary* insn = add_insn_t(bc, BC_ARRAY_ACCESS, Binary);
    insn->res_index = add_bytecode_register(bc, t_void_ptr);
    insn->arg0_index = base;
    insn->arg1_index = index;
    insn->stride = (int) align_forward(type->size, type->align);
    insn->comment = comment;
    return insn->res_index;
}

#define bc_instruction_branch(bc, label_index, cond) \
_bc_instruction_branch(bc, label_index, cond, __FILE__ ":" S2(__LINE__))

inline void
_bc_instruction_branch(Bytecode_Builder* bc, int label_index, int cond, cstring comment) {
    Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
    branch->label_index = label_index;
    branch->cond = cond;
    branch->comment = comment;
}

inline int
bc_instruction_global(Bytecode_Builder* bc, int global_index) {
    Bytecode_Binary* result = add_insn_t(bc, BC_GLOBAL, Binary);
    result->res_index = add_bytecode_register(bc, t_void_ptr);
    result->arg0_index = global_index;
    result->comment = 0; // TODO(Alexander): add comment
    
    return result->res_index;
}

#define bc_instruction_local(bc, type) \
_bc_instruction_local(bc, type, __FILE__ ":" S2(__LINE__))

inline int
_bc_instruction_local(Bytecode_Builder* bc, Type* type, cstring comment) {
    assert(bc->curr_function);
    //int result = bc->curr_function->register_count++;
    //Bytecode_Type reg_type = {};
    //reg_type.kind = BC_TYPE_PTR;
    //if (type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace) {
    //reg_type.size = (s32) align_forward(type->Array.type->size, type->Array.type->align);
    //} else {
    //reg_type.size = (s32) align_forward(type->size, type->align);
    //}
    //array_push(bc->curr_function->register_types, reg_type);
    
    int result = add_bytecode_register(bc, t_void_ptr);
    bc->curr_function->register_types[result].flags |= BC_FLAG_LOCAL;
    _bc_instruction(bc, BC_LOCAL, result, type->size, type->align, comment);
    return result;
}

inline void
begin_block(Bytecode_Builder* bc, Bytecode_Operator opcode=BC_BLOCK) {
    add_insn(bc, opcode);
    bc->curr_function->block_count++;
    bc->block_depth++;
}

inline void
end_block(Bytecode_Builder* bc) {
    assert(bc->block_depth > 0);
    add_insn(bc, BC_END);
    bc->block_depth--;
}

void string_bc_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn);
void string_bc_dump_bytecode(String_Builder* sb, Bytecode* bc, Bytecode_Function* func, Type* type=0);
void string_builder_dump_bytecode_function(String_Builder* sb, Bytecode* bc, Bytecode_Function* func);
void dump_bytecode(Bytecode* bc);
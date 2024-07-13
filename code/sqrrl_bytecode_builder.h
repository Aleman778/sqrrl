
struct Bytecode_Builder {
    Bc_Bucket* bucket;
    
    Bc_Module module;
    
    Ast_Function* curr_func;
};

Bc_Arg emit_bytecode_for_expression(Bytecode_Builder* bc, Ast_Expression* expr);

#define BC_REG_ACCUMULATOR 0
#define BC_REG_STACK_PTR 1

inline Bc_Type
bc_type(Ast_Type* type) {
    switch (type->storage) {
        // TODO(Alexander): implement more types
        case TYPE_S8:
        case TYPE_U8:
        case TYPE_S16:
        case TYPE_U16:
        case TYPE_S32:
        case TYPE_U32: return BC_I32;
        
        default: unimplemented;
    }
    
    return BC_I64;
}

inline Bc_Arg
bc_register(u8 reg) {
    Bc_Arg result = {};
    result.mode = BC_REG;
    result.reg = reg;
    return result;
}

inline Bc_Arg
bc_immediate(s32 disp) {
    Bc_Arg result = {};
    result.mode = BC_DISP;
    result.disp = disp;
    return result;
}

inline Bc_Arg
bc_stack(s32 disp) {
    Bc_Arg result = {};
    result.mode = BC_STK;
    result.reg = BC_REG_STACK_PTR;
    result.disp = disp;
    return result;
}

inline Bc_Arg
bc_stack_alloc(Bytecode_Builder* bc, s32 size, s32 align) {
    assert(bc->curr_func && "cannot stack allocate outside function scope");
    bc->curr_func->stack_size = align_forward(bc->curr_func->stack_size, align);
    Bc_Arg result = bc_stack(safe_truncate_to_s32(bc->curr_func->stack_size));
    bc->curr_func->stack_size += size;
    return result;
}

Bc*
bc_instruction(Bytecode_Builder* bc, Opcode opcode, Bc_Type type,
               Bc_Arg res={}, Bc_Arg a={}, Bc_Arg b={}) {
    
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
    result->res = res;
    result->a = a;
    result->b = b;
    
    if (!bc->module.first_bucket) {
        bc->module.first_bucket = bucket;
    }
    
    bc->bucket = bucket;
    return result;
}

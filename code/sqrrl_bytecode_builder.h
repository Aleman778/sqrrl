
struct Bytecode_Builder {
    Memory_Arena arena;
    
    Bytecode bytecode;
    
    array(string_id)* function_names;
    
    u32 next_type_index; 
    
    Bytecode_Function* curr_function;
};

Bytecode_Type
to_bytecode_type(Type* type) {
    switch (type->kind) {
        case TypeKind_Basic: {
            
            switch (type->Basic.kind) {
                case Basic_bool:
                case Basic_s8:
                case Basic_u8:
                case Basic_s16:
                case Basic_u16:
                case Basic_s32:
                case Basic_u32:
                case Basic_int:
                case Basic_uint: return BytecodeType_i32;
                
                case Basic_s64: 
                case Basic_u64: 
                case Basic_smm: 
                case Basic_umm: 
                case Basic_string:
                case Basic_cstring:  return BytecodeType_i64;
                
                case Basic_f32: return BytecodeType_f32;
                case Basic_f64: return BytecodeType_i64;
                
                default: unimplemented; break;
            }
        } break;
        
        case TypeKind_Struct:
        case TypeKind_Union: // TODO(Alexander): should structs/ unions be a different type than 64-bit int?
        case TypeKind_Type:
        case TypeKind_Pointer: {
            return BytecodeType_i64;
        } break;
        
        case TypeKind_Enum: {
            return to_bytecode_type(type->Enum.type);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return BytecodeType_i32;
}

Bytecode_Function*
begin_bytecode_function(Bytecode_Builder* builder, Type* type) {
    assert(type && type->kind == TypeKind_Function && "not a function type");
    
    int ret_count = is_valid_type(type->Function.return_type) ? 1 : 0;
    int arg_count = (int) array_count(type->Function.arg_types);
    int size = sizeof(Bytecode_Function) + ret_count + arg_count;
    
    Bytecode_Function* func = (Bytecode_Function*) arena_push_size(&builder->arena, size, 
                                                                   alignof(Bytecode_Function));
    func->type_index = builder->next_type_index++;
    func->ret_count = ret_count;
    func->arg_count = arg_count;
    
    Bytecode_Type* curr_type = (Bytecode_Type*) (func + 1);
    if (ret_count > 0) {
        assert(ret_count == 1 && "TODO: multiple arguments");
        *curr_type++ = to_bytecode_type(type->Function.return_type);
    }
    
    for (int i = 0; i < arg_count; i++) {
        *curr_type++ = to_bytecode_type(type->Function.arg_types[i]);
    }
    
    array_push(builder->bytecode.functions, func);
    array_push(builder->function_names, type->Function.ident);
    
    builder->curr_function = func;
    return func;
}

void
end_bytecode_function(Bytecode_Builder* builder) {
    builder->curr_function = 0;
}


#define add_insn(bc, opcode) add_bytecode_insn(bc, opcode, \
sizeof(Bytecode_Instruction), \
alignof(Bytecode_Instruction), \
__FILE__ ":" S2(__LINE__));

Bytecode_Instruction*
add_bytecode_insn(Bytecode_Builder* builder, Bytecode_Operator opcode, umm size, umm align, cstring loc) {
    assert(builder->curr_function && "cannot add instruction outside function scope");
    
    Bytecode_Function* func = builder->curr_function;
    func->insn_count++;
    
    // TODO(Alexander): when we run out of memory we need to make sure we have pointer to next instruction
    Bytecode_Instruction* insn = (Bytecode_Instruction*) arena_push_size(&builder->arena, size, align);
    insn->opcode = opcode;
    insn->kind = BytecodeInstructionKind_Base;
    insn->comment = loc;
    
    return insn;
}

#if 0
inline Intermediate_Code*
ic_add_orphan(Bytecode_Builder* bc, Ic_Opcode opcode = IC_NOOP, smm size=sizeof(Intermediate_Code)) {
    // TODO(Alexander): temporary bump allocation for now
    Intermediate_Code* result = (Intermediate_Code*) calloc(1, size);
    result->opcode = opcode;
    return result;
}

#define S1(x) #x
#define S2(x) S1(x)
#define ic_add(bc, opcode) ic_add_insn(bc, opcode, __FILE__ ":" S2(__LINE__))
#define ic_label(bc, bb) ic_label_insn(bc, bb, __FILE__ ":" S2(__LINE__))
#define ic_jump(bc, opcode, target_bb) ic_jump_insn(bc, opcode, target_bb, __FILE__ ":" S2(__LINE__))
#define ic_call(bc) (Ic_Call*) ic_add_insn(bc, IC_CALL, __FILE__ ":" S2(__LINE__), sizeof(Ic_Call))


inline Intermediate_Code*
ic_add_orphan(Bytecode_Builder* bc, Ic_Opcode opcode = IC_NOOP, smm size=sizeof(Intermediate_Code)) {
    // TODO(Alexander): temporary bump allocation for now
    Intermediate_Code* result = (Intermediate_Code*) calloc(1, size);
    result->opcode = opcode;
    return result;
}

#define S1(x) #x
#define S2(x) S1(x)
#define ic_add(bc, opcode) ic_add_insn(bc, opcode, __FILE__ ":" S2(__LINE__))
#define ic_label(bc, bb) ic_label_insn(bc, bb, __FILE__ ":" S2(__LINE__))
#define ic_jump(bc, opcode, target_bb) ic_jump_insn(bc, opcode, target_bb, __FILE__ ":" S2(__LINE__))
#define ic_call(bc) (Ic_Call*) ic_add_insn(bc, IC_CALL, __FILE__ ":" S2(__LINE__), sizeof(Ic_Call))
#define ic_bc(bc) (Ic_Func*) ic_add_insn(bc, IC_FUNC, __FILE__ ":" S2(__LINE__), sizeof(Ic_Func))


internal void
ic_add_to_basic_block(Bytecode_Builder* bc, Intermediate_Code* ic, Ic_Basic_Block* bb) {
    if (!bb->ic_first) {
        bb->ic_first = ic;
    }
    if (!bc->ic_first) {
        bc->ic_first = ic;
    }
    
    if (bb->ic_last) {
        bb->ic_last->next = ic;
    }
    bb->ic_last = ic;
    bc->ic_last = ic;
}

void
ic_label_insn(Bytecode_Builder* bc, Ic_Basic_Block* bb, cstring comment=0) {
    Intermediate_Code* result = ic_add_orphan(bc, IC_LABEL);
    result->data = bb;
    
    if (!bc->bb_first) {
        bc->bb_first = bb;
    }
    
    if (bc->bb_last) {
        bc->bb_last->next = bb;
    }
    
    bb->index = bc->bb_index++;
    bb->ic_last = bc->ic_last;
    bc->bb_last = bb;
    
    ic_add_to_basic_block(bc, result, bb);
    
    result->comment = comment;
}

Intermediate_Code*
ic_add_insn(Bytecode_Builder* bc,
            Ic_Opcode opcode = IC_NOOP, 
            cstring comment=0,
            smm size=sizeof(Intermediate_Code)) {
    
    Intermediate_Code* result = ic_add_orphan(bc, opcode, size);
    
    Ic_Basic_Block* bb = bc->bb_last;
    assert(bb && "missing label");
    ic_add_to_basic_block(bc, result, bb);
    
    result->comment = comment;
    
    return result;
}

void
ic_jump_insn(Bytecode_Builder* bc, Ic_Opcode opcode,
             Ic_Basic_Block* target_bb, cstring comment=0) {
    
    assert(opcode >= IC_JMP && opcode <= IC_JNE);
    Ic_Jump* ic = (Ic_Jump*) ic_add_insn(bc, opcode, comment, sizeof(Ic_Jump));
    ic->target = target_bb;
}
#endif

void string_builder_dump_bytecode_insn(String_Builder* sb, Bytecode_Instruction* insn);
void string_builder_dump_bytecode(String_Builder* sb, Bytecode_Function* func, Type* type=0);
void dump_bytecode(Compilation_Unit* cu);
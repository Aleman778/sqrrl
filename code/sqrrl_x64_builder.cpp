
struct X64_Builder {
    Memory_Arena arena;
    
    umm instruction_count;
    X64_Instruction* curr_instruction;
    
    u64 allocated_register_mask;
    map(Bc_Register, u32)* allocated_registers;
    map(u32, s32)* stack_offsets;
    s32 curr_stack_offset;
};


X64_Instruction*
push_instruction(X64_Builder* x64, X64_Opcode opcode) {
    X64_Instruction* insn = arena_push_struct(&x64->arena, X64_Instruction);
    pln("sizeof = %", f_umm(sizeof(X64_Instruction)));
    insn->opcode = opcode;
    x64->curr_instruction = insn;
    x64->instruction_count++;
    return insn;
}


X64_Operand*
push_operand(X64_Builder* x64) {
    assert(x64->curr_instruction);
    X64_Instruction* insn = x64->curr_instruction;
    X64_Operand* result = 0;
    
    // HACK(Alexander): this is not really pushing at the moment
    // we are just inserting the next slot available
    if (!insn->op0.kind) {
        result = &insn->op0;
    } else if (!insn->op1.kind) {
        result = &insn->op1;
    } else if (!insn->op2.kind) {
        result = &insn->op2;
    } else {
        assert(0 && "reached maximum allowed operands per instruction");
    }
    
    return result;
}


X64_Operand*
push_register_operand(X64_Builder* x64, X64_Register reg) {
    
    
    X64_Operand* result = push_operand(x64);
    result->reg = reg;
    
    if (register_is_gpr(reg)) {
        int size = register_size_table[reg];
        switch (size) {
            case 1: result->kind = X64Operand_r8; break;
            case 2: result->kind = X64Operand_r16; break;
            case 4: result->kind = X64Operand_r32; break;
            case 8: result->kind = X64Operand_r64; break;
            default: assert(0 && "invalid register size"); break;
        }
    }
    
    return result;
}

X64_Operand*
push_immediate_operand(X64_Builder* x64, Bc_Value value, Bc_Type type) {
    X64_Operand* result = push_operand(x64);
    
    switch (type.kind) {
        case BcTypeKind_s1:
        case BcTypeKind_s8:
        case BcTypeKind_u8: {
            result->kind = X64Operand_imm8;
            result->imm8 = (s8) value.signed_int;
        } break;
        
        case BcTypeKind_s16:
        case BcTypeKind_u16: {
            result->kind = X64Operand_imm16;
            result->imm16 = (s16) value.signed_int;
        } break;
        
        case BcTypeKind_s32:
        case BcTypeKind_u32: {
            result->kind = X64Operand_imm32;
            result->imm32 = (s32) value.signed_int;
        } break;
        
        case BcTypeKind_s64:
        case BcTypeKind_u64: {
            result->kind = X64Operand_imm64;
            result->imm64 = (s64) value.signed_int;
        } break;
    }
    
    return result;
}

X64_Operand
x64_allocate_register(X64_Builder* x64, Bc_Type type) {
    X64_Operand result = {};
    
    switch (type.kind) {
        case BcTypeKind_s1:
        case BcTypeKind_s8:
        case BcTypeKind_u8: {
            result.kind = X64Operand_r8;
        } break;
        
        
        case BcTypeKind_s16:
        case BcTypeKind_u16: {
            result.kind = X64Operand_r16;
        } break;
        
        case BcTypeKind_s32:
        case BcTypeKind_u32: {
            result.kind = X64Operand_r32;
        } break;
        
        case BcTypeKind_s64:
        case BcTypeKind_u64: {
            result.kind = X64Operand_r64;
        } break;
        
        case BcTypeKind_f32:
        case BcTypeKind_f64: {
            result.kind = X64Operand_mm;
        } break;
    }
    
    return result;
}

void
x64_build_instruction(X64_Builder* x64, Bc_Instruction* bc) {
    switch (bc->opcode) {
        case Bytecode_stack_alloc: break;
        
        case Bytecode_store: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind != BcOperand_None);
            assert(bc->src1.kind == BcOperand_None);
            
            umm stack_offset = map_get(x64->stack_offsets, bc->dest.Register.index);
            
            //x64_push_instruction(x64, X64Opcode_MOV);
            //x64_push_rm_operand();
            
        } break;
        
        case Bytecode_add: {
            Bc_Type type = bc->src0.type;
            
        } break;
    }
}

void
x64_push_prologue(X64_Builder* x64) {
    // push rbp
    // mov rbp rsp
    // sub rbp stack_size (only relevant for non-leaf functions)
    
    
    push_instruction(x64, X64Opcode_push);
    push_register_operand(x64, X64Register_rbp);
    
    push_instruction(x64, X64Opcode_mov);
    push_register_operand(x64, X64Register_rbp);
    push_register_operand(x64, X64Register_rsp);
    
    //push_instruction(x64, X64Opcode_sub);
    //push_register_operand(x64, X64Register_rbp);
    //push_immediate_operand(x64, stack_size);
}


void
x64_push_epilogue(X64_Builder* x64) {
    // add rsp stack_size (only relevant for non-leaf functions)
    // pop rbp
    // ret
    
    push_instruction(x64, X64Opcode_pop);
    push_register_operand(x64, X64Register_rbp);
    
    push_instruction(x64, X64Opcode_ret);
}

#define for_basic_block(x64, first_block, curr_block, curr_block_insn, function) { \
Bc_Basic_Block* curr_block = first_block; \
umm curr_block_insn = 0; \
\
while (curr_block) { \
while (curr_block_insn++ < curr_block->count) { \
function(x64, block->first + offset); \
} \
curr_block = curr_block->next; \
curr_block_insn = 0; \
} \
}

void
x64_analyse_function(X64_Builder* x64, Bc_Instruction* bc) {
    switch (bc->opcode) {
        case Bytecode_stack_alloc: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Type);
            assert(bc->src1.kind == BcOperand_None);
            
            Type* type = bc_type_to_type(bc->src0.type);
            assert(type->cached_size > 0 && "bad size");
            assert(type->cached_align > 0 && "bad align");
            
            s32 stack_offset = (s32) align_forward((umm) x64->curr_stack_offset, type->cached_align);
            map_put(x64->stack_offsets, bc->dest.kind, stack_offset);
            x64->curr_stack_offset = stack_offset + type->cached_size;
            
            pln("stack_offset(size = %, align = %, %) = %", f_umm(type->cached_size), f_umm(type->cached_align), f_u32(bc->dest.Register.index), f_s64(stack_offset));
        } break;
        
    }
}

void
x64_build_function(X64_Builder* x64, Bc_Basic_Block* first_block) {
    for_basic_block(x64, first_block, block, offset, x64_analyse_function);
    
    x64_push_prologue(x64);
    for_basic_block(x64, first_block, block, offset, x64_build_instruction);
    x64_push_epilogue(x64);
}
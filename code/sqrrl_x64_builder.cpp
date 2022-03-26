


struct X64_Builder {
    Memory_Arena arena;
    
    u64 allocated_register_mask;
    map(Bc_Register, u32)* allocated_registers;
    map(u32, s32)* stack_offsets;
    s32 curr_stack_offset;
};

X64_Operand
bc_operand_to_x64_operand(Bc_Operand operand) {
    X64_Operand result = {};
    
    switch (operand.kind) {
        case BcOperand_Register: {
            
            
        } break;
        
        case BcOperand_Value: {
            switch (operand.type.kind) {
                case BcTypeKind_s1:
                case BcTypeKind_s8:
                case BcTypeKind_u8: {
                    result.kind = X64Operand_imm8;
                } break;
                
                
                case BcTypeKind_s16:
                case BcTypeKind_u16: {
                    result.kind = X64Operand_imm16;
                } break;
                
                case BcTypeKind_s32:
                case BcTypeKind_u32: {
                    result.kind = X64Operand_imm32;
                } break;
                
                case BcTypeKind_s64:
                case BcTypeKind_u64: {
                    result.kind = X64Operand_imm64;
                } break;
            }
            
        } break;
        
        default: {
            //unimplemented;
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
x64_analyse_stack(X64_Builder* x64, Bc_Instruction* bc) {
    if (bc->opcode == Bytecode_stack_alloc) {
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
    }
}

void
x64_build_function(X64_Builder* x64, Bc_Basic_Block* first_block) {
    for_basic_block(x64, first_block, block, offset, x64_analyse_stack);
    for_basic_block(x64, first_block, block, offset, x64_build_instruction);
}

struct X64_Builder {
    Memory_Arena arena;
    
    umm instruction_count;
    X64_Instruction* curr_instruction;
    X64_Basic_Block* curr_basic_block;
    
    map(u32, s32)* stack_offsets;
    s32 curr_stack_offset;
    
    u32 next_free_virtual_register;
    map(Bc_Register, u32)* allocated_virtual_registers;
};

struct X64_Register_Node {
    u32 virtual_register;
    X64_Register physical_register;
    array(X64_Register_Node*)* dependencies;
    b32 is_allocated;
};

u32
x64_allocate_virtual_register(X64_Builder* x64, Bc_Register ident) {
    // TODO(Alexander): do we need 0 to be invalid? Only needed for checking for 0 on map_get
    if (x64->next_free_virtual_register == 0) {
        x64->next_free_virtual_register++;
    }
    
    u32 result = x64->next_free_virtual_register++;
    if (ident.ident) {
        map_put(x64->allocated_virtual_registers, ident, result);
    }
    
    // TODO(Alexander): build interference graph node
    
    return result;
}


X64_Instruction*
x64_push_instruction(X64_Builder* x64, X64_Opcode opcode) {
    X64_Instruction* insn = arena_push_struct(&x64->arena, X64_Instruction);
    insn->opcode = opcode;
    x64->curr_instruction = insn;
    x64->instruction_count++;
    return insn;
}


X64_Basic_Block*
x64_push_basic_block(X64_Builder* x64, Bc_Register label) {
    X64_Basic_Block* block = arena_push_struct(&x64->arena, X64_Basic_Block);
    if (x64->curr_basic_block) {
        x64->curr_basic_block->next = block;
    }
    x64->curr_basic_block = block;
    
    X64_Instruction* insn =x64_push_instruction(x64, X64Opcode_label);
    insn->op0.kind = X64Operand_basic_block;
    insn->op0.basic_block = block;
    
    
    block->label = label;
    block->first = x64->curr_instruction;
    block->count = 0;
    return block;
}

X64_Operand
x64_build_immediate(X64_Builder* x64, Bc_Value value, Bc_Type type) {
    X64_Operand result = {};
    
    switch (type.kind) {
        case BcTypeKind_s1:
        case BcTypeKind_s8:
        case BcTypeKind_u8: {
            result.kind = X64Operand_imm8;
            result.imm8 = (s8) value.signed_int;
        } break;
        
        case BcTypeKind_s16:
        case BcTypeKind_u16: {
            result.kind = X64Operand_imm16;
            result.imm16 = (s16) value.signed_int;
        } break;
        
        case BcTypeKind_s32:
        case BcTypeKind_u32: {
            result.kind = X64Operand_imm32;
            result.imm32 = (s32) value.signed_int;
        } break;
        
        case BcTypeKind_s64:
        case BcTypeKind_u64: {
            result.kind = X64Operand_imm64;
            result.imm64 = (s64) value.signed_int;
        } break;
        // TODO(Alexander): need to support floats
    }
    
    return result;
}

X64_Operand
x64_build_register(X64_Builder* x64, Bc_Register ident, Bc_Type type) {
    X64_Operand result = {};
    
    u32 reg = map_get(x64->allocated_virtual_registers, ident);
    pln("loaded reg(r%) = %", f_u32(ident.index), f_u32(reg));
    if (reg) {
        result.virtual_register = reg;
    } else {
        result.virtual_register = x64_allocate_virtual_register(x64, ident);
    }
    
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

X64_Operand
x64_build_stack_offset(X64_Builder* x64, s64 stack_offset, X64_Register reg = X64Register_rbp) {
    X64_Operand result = {};
    result.kind = X64Operand_rm64;
    result.reg = reg;
    result.disp64 = stack_offset;
    result.is_allocated = true;
    return result;
}

X64_Operand
x64_build_physical_register(X64_Builder* x64, X64_Register reg, X64_Operand_Kind kind) {
    // TODO(Alexander): we need to tell the register allocate which physical register
    //                  to preallocate for this virtual register.
    
    X64_Operand result = {};
    result.kind = kind;
    
    // NOTE(Alexander): we don't need to consider RSP and RBP in register allocation
    // since they should never be allocated anyways.
    if (reg == X64Register_rbp || reg == X64Register_rsp) {
        result.reg = reg;
        result.is_allocated = true;
    } else {
        result.virtual_register = x64_allocate_virtual_register(x64, { Kw_invalid, 0 });
    }
    return result;
}

X64_Operand
x64_build_operand(X64_Builder* x64, Bc_Operand* operand) {
    X64_Operand result = {};
    
    switch (operand->kind) {
        case BcOperand_Register: {
            result = x64_build_register(x64, operand->Register, operand->type);
        } break;
        
        case BcOperand_Value: {
            result = x64_build_immediate(x64, operand->Value, operand->type);
        } break;
    }
    
    return result;
}

void
x64_build_instruction_from_bytecode(X64_Builder* x64, Bc_Instruction* bc) {
    switch (bc->opcode) {
        case Bytecode_stack_alloc: break;
        
        case Bytecode_store: {
            smm stack_index = map_get_index(x64->stack_offsets, bc->dest.Register.index);
            assert(stack_index != -1 && "not stored on stack");
            s32 stack_offset = x64->stack_offsets[stack_index].value;
            // TODO(Alexander): should also support memory addresses
            
            X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
            insn->op0 = x64_build_stack_offset(x64, stack_offset);
            insn->op1 = x64_build_operand(x64, &bc->src0);
        } break;
        
        case Bytecode_load: {
            Bc_Type type = bc->src0.type;
            
            smm stack_index = map_get_index(x64->stack_offsets, bc->src1.Register.index);
            assert(stack_index != -1 && "not stored on stack");
            umm stack_offset = x64->stack_offsets[stack_index].value;
            
            X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
            insn->op0 = x64_build_operand(x64, &bc->dest);
            insn->op1 = x64_build_stack_offset(x64, stack_offset);
            
            
        } break;
        
        case Bytecode_add: {
            Bc_Type type = bc->src0.type;
            
            X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
            mov_insn->op0 = x64_build_operand(x64, &bc->dest);
            mov_insn->op1 = x64_build_operand(x64, &bc->src0);
            
            X64_Instruction* add_insn = x64_push_instruction(x64, X64Opcode_add);
            add_insn->op0 = mov_insn->op0;
            add_insn->op1 = x64_build_operand(x64, &bc->src1);
        } break;
        
        case Bytecode_ret: {
            X64_Instruction* mov_insn =x64_push_instruction(x64, X64Opcode_mov);
            // TODO(Alexander): should not always be r64
            mov_insn->op0 = x64_build_physical_register(x64, X64Register_rax, X64Operand_r64);
            mov_insn->op1 = x64_build_operand(x64, &bc->src0);
        } break;
    }
}

void
x64_push_prologue(X64_Builder* x64) {
    // push rbp
    // mov rbp rsp
    // sub rbp stack_size (only relevant for non-leaf functions)
    
    
    X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_push);
    insn->op0 = x64_build_physical_register(x64, X64Register_rbp, X64Operand_r64);
    
    insn = x64_push_instruction(x64, X64Opcode_mov);
    insn->op0 = x64_build_physical_register(x64, X64Register_rbp, X64Operand_r64);
    insn->op1 = x64_build_physical_register(x64, X64Register_rsp, X64Operand_r64);
    
    //push_instruction(x64, X64Opcode_sub);
    //push_register_operand(x64, X64Register_rbp);
    //push_immediate_operand(x64, stack_size);
}


void
x64_push_epilogue(X64_Builder* x64) {
    // add rsp stack_size (only relevant for non-leaf functions)
    // pop rbp
    // ret
    
    X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_pop);
    insn->op0 = x64_build_physical_register(x64, X64Register_rbp, X64Operand_r64);
    
    x64_push_instruction(x64, X64Opcode_ret);
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
            stack_offset += stack_offset + type->cached_size;
            // NOTE(Alexander): we use negative because stack grows downwards
            map_put(x64->stack_offsets, bc->dest.kind, -stack_offset);
            x64->curr_stack_offset = stack_offset;
            
            pln("stack_offset(size = %, align = %, %) = %", f_umm(type->cached_size), f_umm(type->cached_align), f_u32(bc->dest.Register.index), f_s64(stack_offset));
        } break;
        
    }
}

void
x64_build_function(X64_Builder* x64, Bc_Basic_Block* first_block) {
    for_basic_block(x64, first_block, block, offset, x64_analyse_function);
    
    x64_push_prologue(x64);
    for_basic_block(x64, first_block, block, offset, x64_build_instruction_from_bytecode);
    x64_push_epilogue(x64);
}
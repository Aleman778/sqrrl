
struct X64_Register_Node {
    u32 virtual_register;
    X64_Register physical_register;
    array(u32)* interference;
    b32 is_allocated;
    b32 is_spilled;
};

typedef map(u32, X64_Register_Node) Interference_Graph;

struct X64_Builder {
    Memory_Arena arena;
    X64_Basic_Block* first_basic_block;
    
    umm instruction_count;
    X64_Instruction* curr_instruction;
    X64_Basic_Block* curr_basic_block;
    Bc_Instruction* curr_compare_insn;
    
    map(u32, s32)* stack_offsets;
    s32 curr_stack_offset;
    
    u32 next_free_virtual_register;
    map(Bc_Register, X64_Operand)* allocated_virtual_registers; // TODO(Alexander): needs to be renamed
    Bc_Live_Length_Table* bc_register_live_lengths;
    array(u32)* active_virtual_registers;
    
    Interference_Graph* interference_graph;
};

inline bool
x64_register_allocation_check_interference(Interference_Graph* interference_graph,
                                           X64_Register_Node* node) {
    for_array(node->interference, dep_id, _) {
        X64_Register_Node* other_node = &map_get(interference_graph, *dep_id);
        if (node->physical_register == other_node->physical_register) {
            return false;
        }
    }
    
    return true;
}

void
x64_perform_register_allocation(X64_Builder* x64) {
    for_map (x64->interference_graph, it) {
        X64_Register_Node* node = &it->value;
        
        if (!node->is_allocated) {
            for (int physical_reg_index = 0;
                 physical_reg_index < fixed_array_count(x64_gpr_register_table);
                 physical_reg_index++) {
                
                node->physical_register = x64_gpr_register_table[physical_reg_index];
                node->is_allocated = x64_register_allocation_check_interference(x64->interference_graph, node);
                if (node->is_allocated) break;
            }
            
            if (!node->is_allocated) {
                // NOTE(Alexander): no more free registers needs to be spilled
                node->is_spilled = true;
            }
        }
    }
    
#define ALLOC_REG(operand) \
if (operand_is_register(operand.kind) || operand_is_memory(operand.kind)) { \
if (!operand.is_allocated) { \
X64_Register_Node* node = \
&map_get(x64->interference_graph, operand.virtual_register); \
operand.reg = node->physical_register; \
operand.is_allocated = true; \
} \
}
    
    for_x64_basic_block(x64->first_basic_block, insn, insn_index, {
                            ALLOC_REG(insn->op0);
                            ALLOC_REG(insn->op1);
                            ALLOC_REG(insn->op2);});
}

inline void
x64_add_interference(X64_Builder* x64, u32 a, u32 b) {
    X64_Register_Node* node_a = &map_get(x64->interference_graph, a);
    X64_Register_Node* node_b = &map_get(x64->interference_graph, b);
    array_push(node_a->interference, b);
    array_push(node_b->interference, a);
}

u32
x64_allocate_virtual_register(X64_Builder* x64) {
    // TODO(Alexander): do we need 0 to be invalid? Only needed for checking for 0 on map_get
    if (x64->next_free_virtual_register == 0) {
        x64->next_free_virtual_register++;
    }
    
    u32 result = x64->next_free_virtual_register++;
    //pln("x64_allocate_virtual_register: r%", f_u32(result));
    
    {
        X64_Register_Node node = {};
        node.virtual_register = result;
        map_put(x64->interference_graph, result, node);
    }
    
    // Create interferences with all other active registers
    X64_Register_Node* node = &map_get(x64->interference_graph, result);
    for_array(x64->active_virtual_registers, other_vreg, other_index) {
        X64_Register_Node* other_node = &map_get(x64->interference_graph, *other_vreg);
        array_push(node->interference, other_node->virtual_register);
        array_push(other_node->interference, node->virtual_register);
        pln("interference % -> %", 
            f_u32(node->virtual_register),
            f_u32(other_node->virtual_register));
    }
    
    return result;
}

u32
x64_allocate_specific_register(X64_Builder* x64, X64_Register physical_register) {
    u32 virtual_register = x64_allocate_virtual_register(x64);
    X64_Register_Node* node = &map_get(x64->interference_graph, virtual_register);
    node->physical_register = physical_register;
    node->is_allocated = true;
    
    return virtual_register;
}


inline bool
operand_is_unallocated_register(X64_Operand operand) {
    return (operand_is_register(operand.kind) || 
            operand_is_memory(operand.kind)) && !operand.is_allocated;
}

#if BUILD_DEBUG
// TODO(Alexander): ugh macro nonsense
#define S1(x) #x
#define S2(x) S1(x)
#define x64_push_instruction(x64, opcode) _x64_push_instruction(x64, opcode, __FILE__ ":" S2(__LINE__))
#else 
#define x64_push_instruction(x64, opcode) _x64_push_instruction(x64, opcode) 
#endif 

// NOTE(Alexander): forward declare
X64_Operand x64_build_virtual_register(X64_Builder* x64, X64_Operand_Kind kind);
X64_Basic_Block* x64_push_basic_block(X64_Builder* x64, Bc_Register label);


#define x64_push_struct(x64, type, ...) \
(type*) x64_push_size(x64, (umm) sizeof(type), (umm) alignof(type), __VA_ARGS__)


// NOTE(Alexander): makes sure that when memory gets fragmented that we start a new block
// TODO(Alexander): introduce allocators so we can reuse arena_push_size (mostly copypasta)
void*
x64_push_size(X64_Builder* x64, umm size, umm align, umm flags=0) {
    Memory_Arena* arena = &x64->arena;
    umm current = (umm) (arena->base + arena->curr_used);
    umm offset = align_forward(current, align) - (umm) arena->base;
    
    if (offset + size > arena->size) {
        if (arena->min_block_size == 0) {
            arena->min_block_size = ARENA_DEFAULT_BLOCK_SIZE;
        }
        
        arena->base = (u8*) calloc(1, arena->min_block_size);
        arena->curr_used = 0;
        arena->prev_used = 0;
        arena->size = arena->min_block_size;
        
        // TODO(Alexander): should probably not use the same label
        x64_push_basic_block(x64, x64->curr_basic_block->label);
        
        current = (umm) arena->base + arena->curr_used;
        offset = align_forward(current, align) - (umm) arena->base;
    } 
    
    void* result = arena->base + offset;
    arena->prev_used = arena->curr_used;
    arena->curr_used = offset + size;
    return result;
}


X64_Instruction*
_x64_push_instruction(X64_Builder* x64, X64_Opcode opcode, cstring comment = 0) {
    assert(x64->curr_basic_block);
    
    if (x64->curr_instruction) {
        // NOTE(Alexander): Make sure we aren't reading and writing to memory at once
        X64_Instruction* insn = x64->curr_instruction;
        if (operand_is_memory(insn->op0.kind) && operand_is_memory(insn->op1.kind)) {
            
            X64_Operand_Kind reg_kind = X64Operand_r64;
            switch (insn->op1.kind) {
                case X64Operand_m8: reg_kind = X64Operand_r8; break;
                case X64Operand_m16: reg_kind = X64Operand_r16; break;
                case X64Operand_m32: reg_kind = X64Operand_r32; break;
                case X64Operand_m64: reg_kind = X64Operand_r64; break;
            }
            
            // We push a new instruction and copy over the old one onto that
            X64_Instruction* mov_insn = insn;
            insn = x64_push_struct(x64, X64_Instruction);
            x64->curr_basic_block->count++;
            x64->instruction_count++;
            *insn = *mov_insn;
#if BUILD_DEBUG
            insn->comment = 0;
#endif
            
            mov_insn->opcode = X64Opcode_mov;
            mov_insn->op0 = x64_build_virtual_register(x64, reg_kind);
            mov_insn->op1 = insn->op1;
            
            insn->op1 = mov_insn->op0;
        }
    }
    
    X64_Instruction* insn = x64_push_struct(x64, X64_Instruction);
    insn->opcode = opcode;
    x64->curr_basic_block->count++;
    x64->curr_instruction = insn;
    x64->instruction_count++;
    
#if BUILD_DEBUG
    insn->comment = comment;
#else
    (void) comment;
#endif
    
    return insn;
}


X64_Basic_Block*
x64_push_basic_block(X64_Builder* x64, Bc_Register label) {
    X64_Basic_Block* block = arena_push_struct(&x64->arena, X64_Basic_Block);
    if (x64->curr_basic_block) {
        x64->curr_basic_block->next = block;
    } else {
        x64->first_basic_block = block;
    }
    x64->curr_basic_block = block;
    
    X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_label);
    insn->op0.kind = X64Operand_basic_block;
    insn->op0.basic_block = block;
    
    
    block->label = label;
    block->first = x64->curr_instruction;
    block->count = 1;
    return block;
}

X64_Operand
x64_build_immediate(X64_Builder* x64, Value_Data value, Bc_Type type) {
    X64_Operand result = {};
    
    switch (type.kind) {
        case BcType_s1:
        case BcType_s8:
        case BcType_u8: {
            result.kind = X64Operand_imm8;
            result.imm8 = (s8) value.signed_int;
        } break;
        
        case BcType_s16:
        case BcType_u16: {
            result.kind = X64Operand_imm16;
            result.imm16 = (s16) value.signed_int;
        } break;
        
        case BcType_s32:
        case BcType_u32: {
            result.kind = X64Operand_imm32;
            result.imm32 = (s32) value.signed_int;
        } break;
        
        case BcType_s64:
        case BcType_u64: {
            result.kind = X64Operand_imm64;
            result.imm64 = (s64) value.signed_int;
        } break;
        // TODO(Alexander): need to support floats
    }
    
    return result;
}

X64_Operand
x64_build_jump_target(X64_Builder* x64, Bc_Register label) {
    X64_Operand result = {};
    result.kind = X64Operand_jump_target;
    result.jump_target = label;
    return result;
}


X64_Operand
x64_build_stack_offset(X64_Builder* x64, 
                       Bc_Type type, 
                       s64 stack_offset, 
                       X64_Register reg = X64Register_rbp) {
    X64_Operand result = {};
    result.kind = x64_get_memory_kind(type);
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
        result.virtual_register = x64_allocate_virtual_register(x64);
        X64_Register_Node* node = &map_get(x64->interference_graph, result.virtual_register);
        node->physical_register = reg;
        node->is_allocated = true;
    }
    return result;
}

X64_Operand
x64_build_virtual_register(X64_Builder* x64, X64_Operand_Kind kind) {
    X64_Operand result = {};
    result.kind = kind;
    result.virtual_register = x64_allocate_virtual_register(x64);
    return result;
}

X64_Operand
x64_build_operand(X64_Builder* x64, Bc_Operand* operand) {
    X64_Operand result = {};
    
    switch (operand->kind) {
        case BcOperand_Register: {
            result = map_get(x64->allocated_virtual_registers, operand->Register);
            if (result.kind == X64Operand_None) {
                result.virtual_register = x64_allocate_virtual_register(x64);
                result.kind = x64_get_register_kind(operand->type);
                map_put(x64->allocated_virtual_registers, operand->Register, result);
                array_push(x64->active_virtual_registers, result.virtual_register);
            }
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
        case Bytecode_label: break;
        case Bytecode_noop: break;
        
        case Bytecode_store: {
            smm stack_index = map_get_index(x64->stack_offsets, bc->dest.Register.index);
            
            if (stack_index == -1) {
                X64_Operand src = map_get(x64->allocated_virtual_registers, bc->src0.Register);
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
                insn->op0 = x64_build_operand(x64, &bc->dest);
                insn->op0.kind = x64_get_memory_kind(bc->src0.type);
                insn->op1 = src;
                
            } else {
                s32 stack_offset = x64->stack_offsets[stack_index].value;
                // TODO(Alexander): should also support memory addresses
                
                X64_Operand source_operand = x64_build_operand(x64, &bc->src0);
                
                if (source_operand.kind == X64Operand_imm64) {
                    // NOTE(Alexander): we can only move imm64 to register
                    
                    X64_Operand temp_reg = {};
                    temp_reg.virtual_register = x64_allocate_virtual_register(x64);
                    temp_reg.kind = x64_get_register_kind(bc->dest.type);
                    
                    X64_Instruction* tmp_mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                    tmp_mov_insn->op0 = temp_reg;
                    tmp_mov_insn->op1 = source_operand;
                    
                    source_operand = temp_reg;
                }
                
                if (x64->curr_compare_insn && operand_is_register(source_operand.kind)) {
                    Bc_Instruction* cmp = x64->curr_compare_insn;
                    if (cmp->dest.kind == BcOperand_Register) {
                        X64_Operand* cmp_dest =
                            &map_get(x64->allocated_virtual_registers, cmp->dest.Register);
                        if (cmp_dest && source_operand.virtual_register == cmp_dest->virtual_register) {
                            
                            X64_Instruction* cmp_insn = x64_push_instruction(x64, X64Opcode_cmp);
                            cmp_insn->op0 = x64_build_operand(x64, &cmp->src0);
                            cmp_insn->op1 = x64_build_operand(x64, &cmp->src1);
                            
                            // NOTE(Alexander): we use inverted condition and so we only jump when condition is false
                            X64_Opcode set_opcode = X64Opcode_sete;
                            switch (cmp->opcode) {
                                case Bytecode_cmpeq:  set_opcode = X64Opcode_sete;  break;
                                case Bytecode_cmpneq: set_opcode = X64Opcode_setne; break;
                                case Bytecode_cmple:  set_opcode = X64Opcode_setle; break;
                                case Bytecode_cmplt:  set_opcode = X64Opcode_setl;  break;
                                case Bytecode_cmpge:  set_opcode = X64Opcode_setge; break;
                                case Bytecode_cmpgt:  set_opcode = X64Opcode_setg;  break;
                            }
                            
                            X64_Instruction* insn = x64_push_instruction(x64, set_opcode);
                            insn->op0 = x64_build_stack_offset(x64, bc->src0.type, stack_offset);
                            x64->curr_compare_insn = 0;
                            return;
                        }
                    }
                }
                
                if (bc->src0.type.ptr_depth > 0 &&
                    bc->dest.type.ptr_depth > bc->src0.type.ptr_depth) {
                    
                    smm ptr_stack_index = map_get_index(x64->stack_offsets, bc->src0.Register.index);
                    assert(ptr_stack_index != -1 && "not stored on stack");
                    s32 ptr_stack_offset = x64->stack_offsets[stack_index].value;
                    
                    assert(bc->dest.type.ptr_depth - bc->src0.type.ptr_depth == 1);
                    
                    X64_Operand temp_reg = {};
                    temp_reg.virtual_register = x64_allocate_virtual_register(x64);
                    temp_reg.kind = x64_get_register_kind(bc->src0.type);
                    
                    X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_lea);
                    insn->op0 = temp_reg;
                    insn->op1 = x64_build_stack_offset(x64, bc->src0.type, ptr_stack_offset);
                    
                    source_operand = temp_reg;
                }
                
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
                insn->op0 = x64_build_stack_offset(x64, bc->src0.type, stack_offset);
                insn->op1 = source_operand;
            }
        } break;
        
        case Bytecode_load: {
            
            Bc_Type type = bc->src0.type;
            
            smm stack_index = map_get_index(x64->stack_offsets, bc->src1.Register.index);
            if (stack_index == -1) {
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
                insn->op0 = x64_build_operand(x64, &bc->dest);
                insn->op1 = x64_build_operand(x64, &bc->src1);
                //insn->op0 = x64_operand_type_cast(insn->op0, type);
                insn->op0.kind = X64Operand_r64;
                
                
                map_put(x64->allocated_virtual_registers, bc->src1.Register, insn->op0);
                
                X64_Operand result = insn->op0;
                result.kind = x64_get_memory_kind(type);
                map_put(x64->allocated_virtual_registers, bc->dest.Register, result);
            } else {
                umm stack_offset = x64->stack_offsets[stack_index].value;
                
                //X64_Operand result = x64_build_operand(x64, &bc->dest);
                //result.kind = x64_get_memory_kind(bc->src0.type);
                X64_Operand result = x64_build_stack_offset(x64, bc->src0.type, stack_offset);
                map_put(x64->allocated_virtual_registers, bc->dest.Register, result);
            }
        } break;
        
        case Bytecode_assign: {
            X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
            insn->op0 = x64_build_operand(x64, &bc->dest);
            insn->op1 = x64_build_operand(x64, &bc->src0);
        } break;
        
#define BINARY_CASE(opcode) \
Bc_Type type = bc->src0.type; \
        \
X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov); \
mov_insn->op0 = x64_build_operand(x64, &bc->dest); \
mov_insn->op1 = x64_build_operand(x64, &bc->src0); \
        \
X64_Instruction* add_insn = x64_push_instruction(x64, opcode); \
add_insn->op0 = mov_insn->op0; \
add_insn->op1 = x64_build_operand(x64, &bc->src1); \
        
        case Bytecode_add: {
            BINARY_CASE(X64Opcode_add);
        } break;
        
        case Bytecode_sub: {
            BINARY_CASE(X64Opcode_sub);
        } break;
        
        case Bytecode_and: {
            BINARY_CASE(X64Opcode_and);
        } break;
        
        case Bytecode_or: {
            BINARY_CASE(X64Opcode_or);
        } break;
        
        case Bytecode_mul: {
            if (bc->src0.kind == BcOperand_Value || bc->src1.kind == BcOperand_Value) {
                Bc_Type type = bc->src0.type;
                X64_Operand_Kind operand_kind = x64_get_register_kind(type);
                X64_Opcode mul_opcode = X64Opcode_imul; // TODO(Alexander): check signedness
                
                Bc_Operand* reg;
                Bc_Operand* imm;
                if (bc->src1.kind == BcOperand_Value) {
                    reg = &bc->src0;
                    imm = &bc->src1;
                } else {
                    reg = &bc->src1;
                    imm = &bc->src0;
                }
                
                if (reg->kind == BcOperand_Value) {
                    X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                    mov_insn->op0 = x64_build_operand(x64, &bc->dest);
                    mov_insn->op1 = x64_build_operand(x64, reg);
                    reg = &bc->dest;
                }
                
                X64_Instruction* mul_insn = x64_push_instruction(x64, mul_opcode);
                mul_insn->op0 = x64_build_operand(x64, &bc->dest);
                mul_insn->op1 = x64_build_operand(x64, reg);
                mul_insn->op2 = x64_build_operand(x64, imm);
                
            } else {
                BINARY_CASE(X64Opcode_imul); // TODO(Alexander): check signedness
            }
        } break;
        
        case Bytecode_div: {
            Bc_Type type = bc->src0.type;
            X64_Operand_Kind operand_kind = x64_get_register_kind(type);
            X64_Opcode div_opcode = X64Opcode_idiv; // TODO(Alexander): check signedness
            
            // RAX is used as input and RDX:RAX is used as output
            u32 rax = x64_allocate_specific_register(x64, X64Register_rax);
            u32 rdx = x64_allocate_specific_register(x64, X64Register_rdx);
            x64_add_interference(x64, rax, rdx);
            x64_add_interference(x64, rdx, rax);
            
            X64_Operand rax_op = {};
            rax_op.kind = operand_kind;
            rax_op.virtual_register = rax;
            
            // Move source1 into RAX
            X64_Instruction* mov_rax_insn = x64_push_instruction(x64, X64Opcode_mov);
            mov_rax_insn->op0 = rax_op;
            mov_rax_insn->op1 = x64_build_operand(x64, &bc->src0);
            // TODO(Alexander): are we sure this will become a register?
            x64_add_interference(x64, rax, mov_rax_insn->op1.virtual_register);
            
            // Convert RAX into RDX:RAX
            // TODO(Alexander): check type to pick one of cwd, cwq, cwo
            x64_push_instruction(x64, X64Opcode_cwd);
            
            // Perform division
            X64_Instruction* div_insn = x64_push_instruction(x64, div_opcode);
            div_insn->op0 = x64_build_operand(x64, &bc->src1);
            
            x64_add_interference(x64, rax, div_insn->op0.virtual_register);
            x64_add_interference(x64, rdx, div_insn->op0.virtual_register);
            
            // Store the result back into dest
            X64_Instruction* mov_dest_insn = x64_push_instruction(x64, X64Opcode_mov);
            mov_dest_insn->op0 = x64_build_operand(x64, &bc->dest);
            mov_dest_insn->op1 = rax_op;
            
            x64_add_interference(x64, rax, mov_dest_insn->op0.virtual_register);
        } break;
        
        
        case Bytecode_cmpeq:
        case Bytecode_cmpneq:
        case Bytecode_cmple:
        case Bytecode_cmplt:
        case Bytecode_cmpge:
        case Bytecode_cmpgt: {
            x64->curr_compare_insn = bc;
        } break;
        
        case Bytecode_branch: {
            
            X64_Opcode jump_opcode = X64Opcode_jmp;
            Bc_Register jump_label = {};
            
            Bc_Instruction* cmp = x64->curr_compare_insn;
            if (cmp) {
                X64_Instruction* cmp_insn = x64_push_instruction(x64, X64Opcode_cmp);
                cmp_insn->op0 = x64_build_operand(x64, &cmp->src0);
                cmp_insn->op1 = x64_build_operand(x64, &cmp->src1);
                
                // NOTE(Alexander): we use inverted condition and so we only jump when condition is false
                switch (cmp->opcode) {
                    case Bytecode_cmpeq:  jump_opcode = X64Opcode_je;  break;
                    case Bytecode_cmpneq: jump_opcode = X64Opcode_jne; break;
                    case Bytecode_cmple:  jump_opcode = X64Opcode_jle; break;
                    case Bytecode_cmplt:  jump_opcode = X64Opcode_jl;  break;
                    case Bytecode_cmpge:  jump_opcode = X64Opcode_jge; break;
                    case Bytecode_cmpgt:  jump_opcode = X64Opcode_jg;  break;
                }
            } else if (bc->dest.type.kind == BcType_s1) {
                X64_Instruction* test_insn = x64_push_instruction(x64, X64Opcode_cmp);
                test_insn->op0 = x64_build_operand(x64, &bc->dest);
                Value_Data true_value;
                true_value.boolean = true;
                test_insn->op1 = x64_build_immediate(x64, true_value, bc->dest.type);
                jump_opcode = X64Opcode_je;
            }
            
            
            if (bc->src0.kind != BcOperand_None && bc->src1.kind != BcOperand_None) {
                // TODO(Alexander): for now we will assume that true_label is always following instruction
                
                Bc_Basic_Block* true_block = bc->src0.Basic_Block;
                Bc_Basic_Block* false_block = bc->src1.Basic_Block;
                
                if ((smm) true_block == (smm) (bc + 1)) {
                    jump_label = false_block->label;
                    jump_opcode = x64_opcode_invert_jump_condition(jump_opcode);
                } else if ((smm) false_block == (smm) (bc + 1)) {
                    jump_label = true_block->label;
                } else {
                    // TODO(Alexander): we need to generate more instructions for this
                    unimplemented;
                }
                x64->curr_compare_insn = 0;
            } else if (jump_opcode == X64Opcode_jmp) {
                // Unconditional jump
                Bc_Basic_Block* block = bc->dest.Basic_Block;
                if ((smm) block == (smm) (bc + 1)) {
                    // NOTE(Alexander): we don't need to jump to next instruction
                    jump_label.ident = Kw_invalid;
                } else {
                    jump_label = block->label;
                }
            }
            
            if (jump_label.ident != Kw_invalid) {
                X64_Instruction* jump_insn = x64_push_instruction(x64, jump_opcode);
                jump_insn->op0 = x64_build_jump_target(x64, jump_label);
            }
        } break;
        
        case Bytecode_truncate: {
            if (bc->src0.kind == BcOperand_Value) {
                X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                mov_insn->op0 = x64_build_operand(x64, &bc->dest);
                mov_insn->op1 = x64_build_operand(x64, &bc->src0);
            } else {
                X64_Operand src = map_get(x64->allocated_virtual_registers, bc->src0.Register);
                src = x64_operand_type_cast(src, bc->src1.type);
                map_put(x64->allocated_virtual_registers, bc->dest.Register, src);
            }
        } break;
        
        case Bytecode_sign_extend: {
            X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_movsx);
            mov_insn->op0 = x64_build_operand(x64, &bc->dest);
            mov_insn->op1 = x64_build_operand(x64, &bc->src0);
        } break;
        
        case Bytecode_zero_extend: {
            X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_movzx);
            mov_insn->op0 = x64_build_operand(x64, &bc->dest);
            mov_insn->op1 = x64_build_operand(x64, &bc->src0);
        } break;
        
        case Bytecode_cast_fp_to_sint: {
            unimplemented;
        } break;
        
        case Bytecode_cast_fp_to_uint: {
            unimplemented;
        } break;
        
        case Bytecode_cast_sint_to_fp: {
            unimplemented;
        } break;
        
        case Bytecode_cast_uint_to_fp: {
            unimplemented;
        } break;
        
        case Bytecode_fp_extend: {
            unimplemented;
        } break;
        
        case Bytecode_fp_truncate: {
            unimplemented;
        } break;
        
        case Bytecode_call: {
            // TODO(Alexander): uses the x64 microsoft calling convention
            // https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention
            const X64_Register gpr_registers[] = {
                X64Register_rcx, X64Register_rdx, X64Register_r8, X64Register_r9
            };
            
            // Push registers in order
            for (int arg_index = 0; arg_index < array_count(bc->src1.Argument_List); arg_index++) {
                if (arg_index >= fixed_array_count(gpr_registers)) {
                    break;
                }
                
                Bc_Operand* arg = bc->src1.Argument_List + arg_index;
                
                X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                X64_Operand_Kind kind = x64_get_register_kind(arg->type);
                mov_insn->op0 = x64_build_physical_register(x64, gpr_registers[arg_index], kind);
                mov_insn->op1 = x64_build_operand(x64, arg);
                
            }
            
            // Push onto stack in reverse order (right-to-left)
            s32 stack_offset = x64->curr_stack_offset;
            for (int arg_index = (int) array_count(bc->src1.Argument_List) - 1;
                 arg_index >= fixed_array_count(gpr_registers) && arg_index >= 0; 
                 arg_index--) {
                
                
                Bc_Operand* arg = bc->src1.Argument_List + arg_index;
                s32 size = bc_type_to_size(arg->type.kind);
                stack_offset = (s32) align_forward(stack_offset, size);
                
                X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
                X64_Operand_Kind kind = x64_get_memory_kind(arg->type);
                mov_insn->op0 = x64_build_stack_offset(x64, arg->type, stack_offset, X64Register_rsp);
                mov_insn->op1 = x64_build_operand(x64, arg);
                
                stack_offset += size;
            }
            
            X64_Instruction* call_insn = x64_push_instruction(x64, X64Opcode_call);
            call_insn->op0.kind = X64Operand_jump_target;
            call_insn->op0.jump_target = bc->src0.Register;
            
            // Make sure we setup the result as RAX
            X64_Operand dest = {};
            dest.kind = x64_get_register_kind(bc->dest.type);
            dest.is_allocated = true;
            dest.reg = X64Register_rax;
            map_put(x64->allocated_virtual_registers, bc->dest.Register, dest);
        } break;
        
        case Bytecode_ret: {
            X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
            X64_Operand_Kind operand_kind = x64_get_register_kind(bc->src0.type);
            mov_insn->op0 = x64_build_physical_register(x64, X64Register_rax, operand_kind);
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
    //pln("bb.count = %", f_umm(x64->curr_basic_block->count));
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
            stack_offset += type->cached_size;
            // NOTE(Alexander): we use negative because stack grows downwards
            map_put(x64->stack_offsets, bc->dest.Register.index, -stack_offset);
            x64->curr_stack_offset = stack_offset;
            
            //pln("stack_offset(size = %, align = %, %) = %", f_umm(type->cached_size), f_umm(type->cached_align), f_u32(bc->dest.Register.index), f_s64(stack_offset));
        } break;
        
    }
}

internal inline void
x64_free_virtual_register(X64_Builder* x64, Bc_Operand* operand, u32 curr_bc_instruction) {
    if (operand->kind == BcOperand_Register) {
        X64_Operand allocated = map_get(x64->allocated_virtual_registers, operand->Register);
        if (operand_is_register(allocated.kind)) {
            u32 live_length = map_get(x64->bc_register_live_lengths, operand->Register);
            //if (live_length > 0) pln("trying to free bytecode register: % (len = %)", f_u32(operand->Register.index), f_u32(live_length));
            if (live_length > 0 && live_length <= curr_bc_instruction) {
                bool found = false;
                for_array(x64->active_virtual_registers, it, it_index) {
                    if (*it == allocated.virtual_register) {
                        found = true;
                    }
                }
                if (found) {
                    //pln("free virtual register: r%", f_u32(allocated.virtual_register));
                    array_swap_remove(x64->active_virtual_registers, it_index);
                }
            }
        }
    }
}

void
x64_build_function(X64_Builder* x64, Bc_Basic_Block* first_block) {
    x64->curr_stack_offset = 0;
    for_bc_basic_block(first_block, insn, insn_index, x64_analyse_function(x64, insn));
    
    x64_push_basic_block(x64, first_block->label);
    x64_push_prologue(x64);
    
    
    // TODO(Alexander): uses the x64 microsoft calling convention
    // https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention
    const X64_Register gpr_registers[] = {
        X64Register_rcx, X64Register_rdx, X64Register_r8, X64Register_r9
    };
    
    // TODO(Alexander): callee should save volatile registers
    
    // Push registers in order
    for (int arg_index = 0; arg_index < array_count(first_block->args); arg_index++) {
        if (arg_index >= fixed_array_count(gpr_registers)) {
            break;
        }
        
        u32 reg = first_block->args[arg_index];
        
        // TODO(Alexander): r32 is hardcoded for now, we need to store the argument types
        X64_Operand arg = x64_build_physical_register(x64, gpr_registers[arg_index], X64Operand_r32);
        Bc_Register reg_ident = { first_block->label.ident, reg };
        map_put(x64->allocated_virtual_registers, reg_ident, arg);
    }
    
    // Push onto stack in reverse order (right-to-left)
    s32 stack_offset = 8*2; // push RSP + return address
    for (int arg_index = (int) array_count(first_block->args) - 1;
         arg_index >= fixed_array_count(gpr_registers) && arg_index >= 0;
         arg_index--) {
        u32 reg = first_block->args[arg_index];
        
        // TODO(Alexander): s32 is hardcoded for now, we need to store the argument types
        Bc_Type type = { BcType_s32, 0 };
        s32 size = bc_type_to_size(type.kind);
        stack_offset = (s32) align_forward(stack_offset, bc_type_to_size(type.kind));
        
        X64_Operand arg = x64_build_stack_offset(x64, type, stack_offset);
        Bc_Register reg_ident = { first_block->label.ident, reg };
        map_put(x64->allocated_virtual_registers, reg_ident, arg);
        
        stack_offset += size;
    }
    
    Bc_Basic_Block* curr_block = first_block;
    u32 curr_bc_instruction = 0;
    u32 curr_block_insn = 0;
    while (curr_block) {
        while (curr_block_insn < curr_block->count) {
            Bc_Instruction* insn = curr_block->first + curr_block_insn;
            x64_build_instruction_from_bytecode(x64, insn);
            
            //pln("\n\nInstruction: %", f_u32(curr_bc_instruction));
            x64_free_virtual_register(x64, &insn->dest, curr_bc_instruction);
            x64_free_virtual_register(x64, &insn->src0, curr_bc_instruction);
            x64_free_virtual_register(x64, &insn->src1, curr_bc_instruction);
            
            curr_block_insn++;
            curr_bc_instruction++;
        }
        curr_block = curr_block->next;
        curr_block_insn = 0;
        
        if (curr_block) {
            x64_push_basic_block(x64, curr_block->label);
        }
    }
    x64_push_epilogue(x64);
}

void
x64_build_data_storage(X64_Builder* x64, Bc_Register label, Value_Data value, Type* type) {
    x64_push_basic_block(x64, label);
    
    X64_Operand value_operand = {};
    
    if (type->kind == Type_Primitive) {
        switch (type->Primitive.kind) {
            case PrimitiveType_u8:
            case PrimitiveType_s8: {
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_db);
                value_operand.kind = X64Operand_imm32;
                value_operand.imm8 = (s8) value.signed_int;
                insn->op0 = value_operand;
            } break;
            
            case PrimitiveType_u16:
            case PrimitiveType_s16: {
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_dw);
                value_operand.kind = X64Operand_imm32;
                value_operand.imm16 = (s16) value.signed_int;
                insn->op0 = value_operand;
            } break;
            
            case PrimitiveType_int: // arch dep?
            case PrimitiveType_uint: // arch dep?
            case PrimitiveType_u32:
            case PrimitiveType_s32:
            case PrimitiveType_f32: {
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_dd);
                value_operand.kind = X64Operand_imm32;
                value_operand.imm32 = (s32) value.signed_int;
                insn->op0 = value_operand;
            } break;
            
            case PrimitiveType_umm: // arch dep.
            case PrimitiveType_u64:
            case PrimitiveType_smm: // arch dep.
            case PrimitiveType_s64:
            case PrimitiveType_f64:{
                X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_dq);
                value_operand.kind = X64Operand_imm32;
                value_operand.imm64 = (s64) value.signed_int;
                insn->op0 = value_operand;
            } break;
        }
    }
}


inline void
sting_builder_push(String_Builder* sb, X64_Register_Node* node) {
    string_builder_push_format(sb, "r%", f_u32(node->virtual_register));
    if (node->is_allocated) {
        string_builder_push_format(sb, "_%", 
                                   f_cstring(x64_register_name_table[node->physical_register]));
    }
}

string
x64_interference_graph_to_graphviz_dot(X64_Builder* x64) {
    String_Builder sb = {};
    string_builder_push(&sb, "strict graph G {\n");
    for_map(x64->interference_graph, it) {
        X64_Register_Node* node = &it->value;
        
        for_array(node->interference, dep_id, index) {
            
            
            X64_Register_Node* other_node = &map_get(x64->interference_graph, *dep_id);
            string_builder_push(&sb, "  ");
            sting_builder_push(&sb, node);
            string_builder_push(&sb, " -- ");
            sting_builder_push(&sb, other_node);
            string_builder_push(&sb, "\n");
        }
    }
    string_builder_push(&sb, "}");
    
    return string_builder_to_string_nocopy(&sb);
}


struct X64_Register_Node {
    u32 virtual_register;
    X64_Register physical_register;
    array(u32)* interference;
    b32 is_allocated;
    b32 is_spilled;
};

struct X64_Builder {
    Memory_Arena arena;
    X64_Basic_Block* first_basic_block;
    
    umm instruction_count;
    X64_Instruction* curr_instruction;
    X64_Basic_Block* curr_basic_block;
    
    map(u32, s32)* stack_offsets;
    s32 curr_stack_offset;
    
    u32 next_free_virtual_register;
    map(Bc_Register, u32)* allocated_virtual_registers;
    
    map(u32, X64_Register_Node)* interference_graph;
};

inline bool
x64_register_allocation_check_interference(X64_Builder* x64, X64_Register_Node* node) {
    for_array(node->interference, dep_id, _) {
        X64_Register_Node* other_node = &map_get(x64->interference_graph, *dep_id);
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
                node->is_allocated = x64_register_allocation_check_interference(x64, node);
                if (node->is_allocated) break;
            }
            
            if (!node->is_allocated) {
                // NOTE(Alexander): no more free registers needs to be spilled
                node->is_spilled = true;
            }
        }
    }
    
    
    X64_Basic_Block* curr_block = x64->first_basic_block; 
    for (umm insn_index = 0; insn_index < curr_block->count; insn_index++) {
        X64_Instruction* insn = curr_block->first + insn_index;
        
#define ALLOC_REG(operand) \
if (operand_is_register(operand.kind)) { \
if (!operand.is_allocated) { \
X64_Register_Node* node = \
&map_get(x64->interference_graph, operand.virtual_register); \
operand.reg = node->physical_register; \
operand.is_allocated = true; \
} \
}
        
        ALLOC_REG(insn->op0);
        ALLOC_REG(insn->op1);
        ALLOC_REG(insn->op2);
    }
}

void
x64_add_interference(X64_Builder* x64, u32 a, u32 b) {
    X64_Register_Node* node_a = &map_get(x64->interference_graph, a);
    X64_Register_Node* node_b = &map_get(x64->interference_graph, b);
    array_push(node_a->interference, b);
    array_push(node_b->interference, a);
}

u32
x64_allocate_virtual_register(X64_Builder* x64, Bc_Register ident = {}) {
    // TODO(Alexander): do we need 0 to be invalid? Only needed for checking for 0 on map_get
    if (x64->next_free_virtual_register == 0) {
        x64->next_free_virtual_register++;
    }
    
    u32 result = x64->next_free_virtual_register++;
    if (ident.ident) {
        map_put(x64->allocated_virtual_registers, ident, result);
    }
    
    X64_Register_Node node = {};
    node.virtual_register = result;
    map_put(x64->interference_graph, result, node);
    
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
    return operand_is_register(operand.kind) && !operand.is_allocated;
}

X64_Instruction*
x64_push_instruction(X64_Builder* x64, X64_Opcode opcode) {
    X64_Instruction* prev_insn = x64->curr_instruction;
    if (prev_insn) {
        // Connect interfering edges
        u32 r0 = operand_is_unallocated_register(prev_insn->op0) ? prev_insn->op0.virtual_register: 0;
        u32 r1 = operand_is_unallocated_register(prev_insn->op1) ? prev_insn->op1.virtual_register: 0;
        u32 r2 = operand_is_unallocated_register(prev_insn->op2) ? prev_insn->op2.virtual_register: 0;
        
        X64_Register_Node* n0 = r0 ? &map_get(x64->interference_graph, r0) : 0;
        X64_Register_Node* n1 = r1 ? &map_get(x64->interference_graph, r1) : 0;
        X64_Register_Node* n2 = r2 ? &map_get(x64->interference_graph, r2) : 0;
        
        if (n0 && n1) {
            array_push(n0->interference, n1->virtual_register);
            array_push(n1->interference, n0->virtual_register);
        }
        if (n0 && n2) {
            array_push(n0->interference, n2->virtual_register);
            array_push(n2->interference, n0->virtual_register);
        }
        if (n1 && n2) {
            array_push(n1->interference, n2->virtual_register);
            array_push(n2->interference, n1->virtual_register);
        }
    }
    
    assert(x64->curr_basic_block);
    x64->curr_basic_block->count++;
    
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

X64_Operand_Kind
x64_get_register_kind(Bc_Type_Kind type_kind) {
    switch (type_kind) {
        case BcTypeKind_s1:
        case BcTypeKind_s8:
        case BcTypeKind_u8: return X64Operand_r8;
        
        case BcTypeKind_s16:
        case BcTypeKind_u16: return X64Operand_r16;
        
        case BcTypeKind_s32:
        case BcTypeKind_u32: return X64Operand_r32;
        
        case BcTypeKind_s64:
        case BcTypeKind_u64: return X64Operand_r64;
        
        case BcTypeKind_f32:
        case BcTypeKind_f64: return X64Operand_mm;
    }
    
    return X64Operand_None;
}

X64_Operand
x64_build_register(X64_Builder* x64, Bc_Register ident, Bc_Type type) {
    X64_Operand result = {};
    
    u32 reg = map_get(x64->allocated_virtual_registers, ident);
    //pln("loaded reg(r%) = %", f_u32(ident.index), f_u32(reg));
    if (reg) {
        result.virtual_register = reg;
    } else {
        result.virtual_register = x64_allocate_virtual_register(x64, ident);
    }
    
    result.kind = x64_get_register_kind(type.kind);
    
    return result;
}

X64_Operand
x64_build_stack_offset(X64_Builder* x64, 
                       Bc_Type type, 
                       s64 stack_offset, 
                       X64_Register reg = X64Register_rbp) {
    X64_Operand result = {};
    switch (type.kind) {
        case BcTypeKind_s1:
        case BcTypeKind_s8:
        case BcTypeKind_u8: {
            result.kind = X64Operand_m64;
        } break;
        
        case BcTypeKind_s16:
        case BcTypeKind_u16: {
            result.kind = X64Operand_m16;
        } break;
        
        case BcTypeKind_s32:
        case BcTypeKind_u32: {
            result.kind = X64Operand_m32;
        } break;
        
        case BcTypeKind_s64:
        case BcTypeKind_u64: {
            result.kind = X64Operand_m64;
        } break;
        
        // TODO(Alexander): floating point
    }
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
x64_build_virtual_register(X64_Builder* x64, u32 virtual_register, X64_Operand_Kind kind) {
    X64_Operand result = {};
    result.kind = kind;
    result.virtual_register = x64_allocate_virtual_register(x64, { Kw_invalid, 0 });
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
            insn->op0 = x64_build_stack_offset(x64, bc->src0.type, stack_offset);
            insn->op1 = x64_build_operand(x64, &bc->src0);
        } break;
        
        case Bytecode_load: {
            Bc_Type type = bc->src0.type;
            
            smm stack_index = map_get_index(x64->stack_offsets, bc->src1.Register.index);
            assert(stack_index != -1 && "not stored on stack");
            umm stack_offset = x64->stack_offsets[stack_index].value;
            
            X64_Instruction* insn = x64_push_instruction(x64, X64Opcode_mov);
            insn->op0 = x64_build_operand(x64, &bc->dest);
            insn->op1 = x64_build_stack_offset(x64, bc->src0.type, stack_offset);
            
            
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
        
        case Bytecode_mul: {
            if (bc->src0.kind == BcOperand_Value || bc->src1.kind == BcOperand_Value) {
                Bc_Type type = bc->src0.type;
                X64_Operand_Kind operand_kind = x64_get_register_kind(type.kind);
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
            X64_Operand_Kind operand_kind = x64_get_register_kind(type.kind);
            X64_Opcode div_opcode = X64Opcode_idiv; // TODO(Alexander): check signedness
            
            // RAX is used as input and RDX:RAX is used as output
            u32 rax = x64_allocate_specific_register(x64, X64Register_rax);
            u32 rdx = x64_allocate_specific_register(x64, X64Register_rdx);
            x64_add_interference(x64, rax, rdx);
            x64_add_interference(x64, rdx, rax);
            
            // Move source1 into RAX
            X64_Instruction* mov_rax_insn = x64_push_instruction(x64, X64Opcode_mov);
            mov_rax_insn->op0 = x64_build_virtual_register(x64, rax, operand_kind);
            mov_rax_insn->op1 = x64_build_operand(x64, &bc->src0);
            x64_add_interference(x64, rax, mov_rax_insn->op1.virtual_register);
            
            // Convert RAX into RDX:RAX
            // TODO(Alexander): check type to pick one of cwd, cwq, cwo
            x64_push_instruction(x64, X64Opcode_cwd);
            
            // Perform division
            X64_Instruction* div_insn = x64_push_instruction(x64, div_opcode);
            div_insn->op0 = x64_build_operand(x64, &bc->src1);
            
            x64_add_interference(x64, rax, div_insn->op1.virtual_register);
            x64_add_interference(x64, rdx, div_insn->op1.virtual_register);
            
            // Store the result back into dest
            X64_Instruction* mov_dest_insn = x64_push_instruction(x64, X64Opcode_mov);
            mov_dest_insn->op0 = x64_build_operand(x64, &bc->dest);
            mov_dest_insn->op1 = x64_build_virtual_register(x64, rax, operand_kind);
            
            x64_add_interference(x64, rax, mov_dest_insn->op0.virtual_register);
        };
        
        case Bytecode_ret: {
            X64_Instruction* mov_insn = x64_push_instruction(x64, X64Opcode_mov);
            // TODO(Alexander): should not always be r64
            X64_Operand_Kind operand_kind = x64_get_register_kind(bc->src0.type.kind);
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
            
            //pln("stack_offset(size = %, align = %, %) = %", f_umm(type->cached_size), f_umm(type->cached_align), f_u32(bc->dest.Register.index), f_s64(stack_offset));
        } break;
        
    }
}

void
x64_build_function(X64_Builder* x64, Bc_Basic_Block* first_block) {
    for_basic_block(x64, first_block, block, offset, x64_analyse_function);
    
    x64_push_basic_block(x64, first_block->label);
    x64_push_prologue(x64);
    for_basic_block(x64, first_block, block, offset, x64_build_instruction_from_bytecode);
    x64_push_epilogue(x64);
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


enum X64_Reg: u8 {
    X64_RAX,
    X64_RCX,
    X64_RDX,
    X64_RBX,
    X64_RSP,
    X64_RBP,
    X64_RSI,
    X64_RDI,
    X64_R8,
    X64_R9,
    X64_R10,
    X64_R11,
    X64_R12,
    X64_R13,
    X64_R14,
    X64_R15,
    
    X64_XMM0,
    X64_XMM1,
    X64_XMM2,
    X64_XMM3,
    X64_XMM4,
    X64_XMM5,
    X64_XMM6,
    X64_XMM7,
    X64_XMM8,
    X64_XMM9,
    X64_XMM10,
    X64_XMM11,
    X64_XMM12,
    X64_XMM13,
    X64_XMM14,
    X64_XMM15,
    
    X64_REG_COUNT,
};

bool x64_windows_nonvolatile_reg[] = {
    false, // X64_RAX
    false, // X64_RCX
    false, // X64_RDX
    true,  // X64_RBX
    true,  // X64_RSP
    true,  // X64_RBP
    true,  // X64_RSI
    true,  // X64_RDI
    false, // X64_R8
    false, // X64_R9
    false, // X64_R10
    false, // X64_R11
    true,  // X64_R12
    true,  // X64_R13
    true,  // X64_R14
    true,  // X64_R15
    false, // X64_RIP
    
    false, // X64_XMM0
    false, // X64_XMM1
    false, // X64_XMM2
    false, // X64_XMM3
    false, // X64_XMM4
    false, // X64_XMM5
    true, // X64_XMM6
    true, // X64_XMM7
    true, // X64_XMM8
    true, // X64_XMM9
    true, // X64_XMM10
    true, // X64_XMM11
    true, // X64_XMM12
    true, // X64_XMM13
    true, // X64_XMM14
    true, // X64_XMM15
};

global const cstring register_names[] {
    "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI",
    "R8",  "R9",  "R10", "R11", "R12", "R13", "R14", "R15",
    "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", 
    "XMM6", "XMM7", "XMM8", "XMM9", "XMM10", "XMM11", 
    "XMM12", "XMM13", "XMM14", "XMM15",
};

struct X64_Block {
    u32 label_index;
    bool is_loop;
};

struct X64_Function {
    u8* code;
    u8** labels;
};

enum X64_Slot_Kind : u8 {
    X64_SLOT_EMPTY,
    X64_SLOT_SPILL,
    X64_SLOT_RSP_DISP32,
    X64_SLOT_REG,
    X64_SLOT_IMM32
};

struct X64_Slot {
    X64_Slot_Kind kind;
    Bytecode_Type type;
    
    union {
        s32 disp;
        s32 imm32;
        X64_Reg reg;
    };
};

struct X64_Jump_Patch {
    u8* origin;
    u8** target;
};

enum X64_Patch_Kind {
    X64_PATCH_NONE,
    X64_PATCH_READ_ONLY_DATA,
    X64_PATCH_READ_WRITE_DATA,
    X64_PATCH_DYNAMIC_LIBRARY,
};

struct X64_Patch {
    u8* origin;
    s32 data;
    X64_Patch_Kind kind;
};

// TODO(Alexander): optimize this structure to minimize collisions with callee saved and HOME registers...
// TODO(Alexander): also we need some heauristic to optimize register allocation
X64_Reg x64_tmp_gpr_registers[] = {
    X64_RAX, X64_RCX, X64_RDX, // X64_RBX, X64_RSI, X64_RDI, X64_R8,
    //X64_R9, X64_R10, X64_R11, X64_R12, X64_R13, X64_R14, X64_R15,
};

X64_Reg x64_tmp_xmm_registers[] = {
    X64_XMM4, X64_XMM5, X64_XMM0, X64_XMM1, X64_XMM2, X64_XMM3,
    //X64_XMM6, X64_XMM7, X64_XMM8, X64_XMM9, X64_XMM10, X64_XMM11,
    //X64_XMM12, X64_XMM13, X64_XMM14, X64_XMM15
};

struct X64_Assembler {
    Bytecode* bytecode;
    u32* stack;
    
    Memory_Arena arena;
    //X64_Slot* slots;
    
    X64_Slot* slots;
    X64_Reg* tmp_registers;
    int allocated_registers[X64_REG_COUNT];
    bool registers_used[X64_REG_COUNT];
    
    Data_Packer* data_packer;
    
    X64_Function* curr_function;
    X64_Function* functions;
    array(X64_Block)* block_stack;
    u32 label_index;
    
    array(X64_Jump_Patch)* jump_patches;
    array(X64_Patch)* address_patches;
    
    bool use_absolute_ptrs;
};

inline int
x64_next_free_register(X64_Assembler* x64, X64_Reg tmp_reg_list[], int tmp_reg_list_count) {
    for (int i = 0; i < tmp_reg_list_count; i++) {
        u8 reg = tmp_reg_list[i];
        if (x64->allocated_registers[reg] <= -1) {
            return i;
        }
    }
    return -1;
}

inline int
x64_next_free_tmp_register(X64_Assembler* x64, X64_Reg tmp_reg_list[], int tmp_reg_list_count) {
    for (int i = 0; i < tmp_reg_list_count; i++) {
        u8 reg = tmp_reg_list[i];
        if (x64->allocated_registers[reg] == -1) {
            return i;
        }
    }
    return -1;
}

inline int
x64_last_allocated_register(X64_Assembler* x64, X64_Reg tmp_reg_list[], int tmp_reg_list_count) {
    for (int i = tmp_reg_list_count - 1; i >= 0; i--) {
        u8 reg = tmp_reg_list[i];
        if (x64->allocated_registers[reg] >= 0) {
            return i;
        }
    }
    return -1;
}

inline void
_x64_allocate_register(X64_Assembler* x64, Bytecode_Type type, int reg_index, 
                       X64_Reg preferred_reg, X64_Reg tmp_reg_list[], int tmp_reg_list_count) {
    X64_Slot* slot = &x64->slots[reg_index];
    if (slot->kind == X64_SLOT_EMPTY) {
        slot->type = type;
        slot->kind = X64_SLOT_SPILL;
        
        if (!(type.flags & BC_FLAG_UNIQUE_REGISTER)) {
            return;
        }
        
        pln("pref: % r%", f_cstring(register_names[preferred_reg]), f_int(x64->allocated_registers[preferred_reg]));
        if (x64->allocated_registers[preferred_reg] <= -1) {
            x64->allocated_registers[preferred_reg] = reg_index;
            slot->kind = X64_SLOT_REG;
            slot->reg = preferred_reg;
            
        } else {
            int i = x64_next_free_register(x64, tmp_reg_list, tmp_reg_list_count);
            if (i != -1) {
                slot->reg = tmp_reg_list[i];
                slot->kind = X64_SLOT_REG;
            }
        }
        
        if (slot->kind == X64_SLOT_REG) {
            x64->allocated_registers[slot->reg] = reg_index;
            pln("allocated % for r% (size %)", f_cstring(register_names[slot->reg]), f_int(x64->allocated_registers[slot->reg]), f_int(type.size));
        }
    }
}

inline void
x64_allocate_register(X64_Assembler* x64, Bytecode_Type type, int reg_index, X64_Reg preferred_reg) {
    assert(!(preferred_reg & 0x10) && "expected preferred_reg to be int register");
    _x64_allocate_register(x64, type, reg_index, preferred_reg, 
                           x64_tmp_gpr_registers, fixed_array_count(x64_tmp_gpr_registers));
}

inline void
x64_allocate_float_register(X64_Assembler* x64, Bytecode_Type type, int reg_index, X64_Reg preferred_reg) {
    assert(preferred_reg & 0x10 && "expected preferred_reg to be float register");
    _x64_allocate_register(x64, type, reg_index, preferred_reg, 
                           x64_tmp_xmm_registers, fixed_array_count(x64_tmp_xmm_registers));
}

inline s32
x64_allocate_stack(X64_Assembler* x64, Bytecode_Type type, int reg_index,
                   s32 size, s32 align, s32 stack_usage) {
    X64_Slot* slot = &x64->slots[reg_index];
    if (slot->kind == X64_SLOT_EMPTY) {
        slot->type = type;
        slot->kind = X64_SLOT_RSP_DISP32;
        
        s32 disp = (s32) align_forward(stack_usage, align);
        stack_usage = disp + size;
        slot->disp = disp;
    }
    return stack_usage;
}

inline void
x64_spill_slot(X64_Assembler* x64, int reg_index, int unallocated_val=-1) {
    X64_Slot* slot = &x64->slots[reg_index];
    if (slot->kind == X64_SLOT_REG) {
        x64->allocated_registers[slot->reg] = unallocated_val;
    }
    slot->kind = X64_SLOT_SPILL;
}

inline void
x64_spill(X64_Assembler* x64, X64_Reg reg, int for_reg_index = -1) {
    int reg_index = x64->allocated_registers[reg];
    if (reg_index >= 0 && reg_index != for_reg_index) {
        pln("SPILL r% for %", f_int(reg_index), f_cstring(register_names[reg]));
        // HACK(Alexander): 
        x64_spill_slot(x64, reg_index, -2);
    } else if (reg_index == -1) {
        x64->allocated_registers[reg] = -2;
    }
    x64->registers_used[reg] = true;
}

inline X64_Reg
_x64_allocate_tmp_register(X64_Assembler* x64, int reg_index,
                           X64_Reg preferred_reg, X64_Reg tmp_reg_list[], int tmp_reg_list_count) {
    X64_Slot slot = {};
    if (reg_index >= 0) {
        slot = x64->slots[reg_index];
    }
    X64_Reg reg; 
    if (slot.kind != X64_SLOT_REG) {
        if (x64->allocated_registers[preferred_reg] == -1) {
            reg = preferred_reg;
        } else {
            int i = x64_next_free_tmp_register(x64, tmp_reg_list, tmp_reg_list_count);
            if (i == -1) {
                i = x64_last_allocated_register(x64, tmp_reg_list, tmp_reg_list_count);
                x64_spill(x64, tmp_reg_list[i], reg_index);
            }
            verify(i != -1 && "ran out of registers");
            reg = tmp_reg_list[i];
            
            //x64_spill(x64, preferred_reg);
            //reg = preferred_reg;
        }
        
        x64->allocated_registers[reg] = -2;
    } else {
        reg = slot.reg;
        if (slot.type.flags & BC_FLAG_UNIQUE_REGISTER) {
            x64->allocated_registers[reg] = -2;
        }
    }
    x64->registers_used[reg] = true;
    
    return reg;
}

inline void
x64_drop_if_register(X64_Assembler* x64, int reg_index) {
    X64_Slot slot = x64->slots[reg_index];
    if (slot.kind == X64_SLOT_REG) {
        X64_Reg reg = slot.reg;
        if (slot.type.flags & BC_FLAG_UNIQUE_REGISTER) {
            x64->allocated_registers[reg] = -1;
        }
    }
}

inline X64_Reg
x64_allocate_tmp_register(X64_Assembler* x64, int reg_index, X64_Reg preferred_reg) {
    assert(!(preferred_reg & 0x10) && "expected preferred_reg to be int register");
    return _x64_allocate_tmp_register(x64, reg_index, preferred_reg, 
                                      x64_tmp_gpr_registers, fixed_array_count(x64_tmp_gpr_registers));}

inline X64_Reg
x64_allocate_tmp_float_register(X64_Assembler* x64, int reg_index, X64_Reg preferred_reg) {
    assert(preferred_reg & 0x10 && "expected preferred_reg to be float register");
    return _x64_allocate_tmp_register(x64, reg_index, preferred_reg,
                                      x64_tmp_xmm_registers, fixed_array_count(x64_tmp_xmm_registers));
}

inline X64_Reg
x64_drop_tmp_register(X64_Assembler* x64, X64_Reg reg) {
    x64->allocated_registers[reg] = -1;
}

inline void
x64_drop(X64_Assembler* x64, int reg_index) {
    // TODO(Alexander): do we need this? We kind of have the same behavior when calling _x64_allocate_tmp_register
#if 0
    X64_Slot slot = x64->slots[reg_index];
    if (slot.kind == X64_SLOT_REG && (slot.type.flags & BC_FLAG_UNIQUE_REGISTER)) {
        x64->allocated_registers[slot.reg] = -1;
    }
#endif
}


inline X64_Slot
get_slot(X64_Assembler* x64, int reg_index) {
    assert(reg_index >= 0);
    return x64->slots[reg_index];
}

inline void
set_slot(X64_Assembler* x64, int reg_index, X64_Slot slot) {
    assert(reg_index >= 0);
    x64->slots[reg_index] = slot;
}

inline s32
x64_register_displacement(X64_Assembler* x64, int slot_index, Bytecode_Type type=BC_PTR) {
    //return x64->
    return 0;
}








#if 0

inline s32
stack_alloc(X64_Assembler* x64, int slot_index, Bytecode_Type type, 
            s32 size, s32 align, bool store_inplace) {
    s32 result = x64->current_stack_size;
    result = (s32) align_forward(result, align);
    x64->current_stack_size = result + size;
    //pln("stack alloc: r% - after x64->current_stack_size % (size %, align %)", f_int(slot_index), 
    //f_int(x64->current_stack_size), f_int(size), f_int(align));
    x64->max_stack_size = max(x64->max_stack_size, x64->current_stack_size);
    X64_Slot_Kind slot_kind = store_inplace ? X64_SLOT_RSP_DISP32 : X64_SLOT_RSP_DISP32;
    set_slot(x64, slot_index, slot_kind, type, result);
    
    return result;
}

inline s32
register_displacement(X64_Assembler* x64, int slot_index, Bytecode_Type type=BC_PTR) {
    X64_Slot slot = get_slot(x64, slot_index);
    //pln("register_displacement - r%, %, size % (disp %)", f_int(slot_index), f_cstring(bc_type_names[type.kind]), f_int(type.size), f_int(slot.disp));
    if (slot.kind == X64_SLOT_EMPTY) {
        return stack_alloc(x64, slot_index, type, 8, 8, false);
    }
    
    assert(slot.kind == X64_SLOT_RSP_DISP32 ||
           slot.kind == X64_SLOT_RSP_DISP32);
    slot.type = type;
    set_slot(x64, slot_index, slot);
    return slot.disp;
}

inline void x64_move_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src);

inline void
x64_spill(X64_Assembler* x64, X64_Reg reg) {
    int slot_index = x64->allocated_registers[reg];
    X64_Slot* slot = &x64->slots[slot_index];
    if (slot->is_reg) {
        x64->allocated_registers[slot->reg] = -1;
        
        int last_allocated = x64->free_gpr_count - 1;
        int free_index = last_allocated;
        for (; free_index >= 0; free_index--) {
            if (x64->free_gpr[free_index] == slot->reg) {
                break;
            }
        }
        
        if (last_allocated != free_index) {
            X64_Reg tmp = x64->free_gpr[last_allocated];
            x64->free_gpr[last_allocated] = x64->free_gpr[free_index];
            x64->free_gpr[free_index] = tmp;
        }
        x64->free_gpr_count--;
    }
    
    slot->is_reg = false;
}

void
spill_register(X64_Assembler* x64, Buffer* buf, X64_Reg reg) {
    int slot_index = x64->allocated_registers[reg];
    if (slot_index >= 0) {
        Bytecode_Type type = get_slot(x64, slot_index).type;
        drop_slot(x64, slot_index);
        s32 disp = stack_alloc(x64, slot_index, type, 8, 8, false);
        x64_move_register_to_memory(buf, X64_RSP, disp, reg);
    }
}

X64_Reg
alloc_tmp_register(X64_Assembler* x64, Buffer* buf, X64_Reg spill_reg) {
    if (x64->free_gpr_count < X64_TMP_GPR_COUNT) {
        X64_Reg reg = x64->free_gpr[x64->free_gpr_count];
        //x64->allocated_registers[reg] = slot_index;
        return reg;
    } else {
        spill_register(x64, buf, spill_reg);
        return spill_reg;
    }
}

X64_Slot
alloc_register(X64_Assembler* x64, Buffer* buf, int slot_index, Bytecode_Type type) {
    bool spill = true;
    if (type.flags & BC_FLAG_UNIQUE_REGISTER && x64->free_gpr_count < X64_TMP_GPR_COUNT) {
        X64_Reg reg = x64->free_gpr[x64->free_gpr_count++];
        x64->allocated_registers[reg] = slot_index;
        set_slot(x64, slot_index, type, reg);
        spill = false;
    }
    
    if (spill) {
        stack_alloc(x64, slot_index, type, 8, 8, false);
    }
    
    return get_slot(x64, slot_index);
}

// TODO: register_alloc -> X64_Reg
//       stack_alloc -> s32 (disp)
//       slot_alloc -> X64_Slot (calls register_alloc and stack_alloc)
//X64_Slot
//alloc_slot() {
#endif

// TODO(Alexander): we should probably return something more approporiate.
X64_Assembler convert_bytecode_to_x64_machine_code(Bytecode* bytecode, 
                                                   Buffer* buf, 
                                                   Data_Packer* data_packer,
                                                   bool is_absolute_ptrs);

void convert_bytecode_function_to_x64_machine_code(X64_Assembler* x64,
                                                   Bytecode_Function* func,
                                                   Buffer* buf);


s32 x64_simple_register_allocator(X64_Assembler* x64, Bytecode_Instruction* bc_insn, int bc_index, s32 stack_usage);

void convert_bytecode_insn_to_x64_machine_code(X64_Assembler* x64, 
                                               Buffer* buf,
                                               Bytecode_Function* func,
                                               Bytecode_Instruction* insn,
                                               int bc_index);


global const X64_Reg int_arg_registers_ccall_windows[] {
    X64_RCX, X64_RDX, X64_R8, X64_R9
};

global const X64_Reg float_arg_registers_ccall_windows[] {
    X64_XMM0, X64_XMM1, X64_XMM2, X64_XMM3
};


#define X64_OP_SIZE_PREFIX 0x66

#define REX_PATTERN 0x40
#define REX_W bit(3)
#define REX_R bit(2)
#define REX_X bit(1)
#define REX_B bit(0)

#define MODRM_DIRECT 0xC0
#define MODRM_INDIRECT_DISP8 0x40
#define MODRM_INDIRECT_DISP32 0x80

global const u32 x64_setcc_opcodes[] = {
    // BC_EQ, BC_GT_S,  BC_GT_U,  BC_GE_S,  BC_GE_U,
    0xC0940F, 0xC09F0F, 0xC0970F, 0xC09D0F, 0xC0930F,
    // BC_LT_U, BC_LT_S, BC_LE_U, BC_LE_S,  BC_NEQ,
    0xC0920F, 0xC09C0F, 0xC0960F, 0xC09E0F, 0xC0950F
};

global const u16 x64_jcc_opcodes[] = {
    0x840F, 0x8F0F, 0x870F, 0x8D0F, 0x830F, 0x820F, 0x8C0F, 0x860F, 0x8E0F, 0x850F
};

//inline void
//x64_rex(Buffer* buf, u8 flags) {
//push_u8(buf, REX_PATTERN | flags);
//}

inline void
x64_rex(Buffer* buf, u8 flags, u8 r_reg=0, u8 b_rm=0, u8 x_index=0) {
    push_u8(buf, REX_PATTERN | flags | ((r_reg&8)>>1) | ((x_index&8)>>2) | (b_rm&8)>>3);
}

inline void
x64_sib(Buffer* buf, u8 scale, u8 index, u8 base) {
    push_u8(buf, scale << 6 | index << 3 | base);
}

inline void
x64_rip_rel(Buffer* buf, u8 reg) {
    push_u8(buf, ((u8) (reg&7)<<3) | (u8) X64_RBP);
}

inline void
x64_rip_relative(Buffer* buf, s64 r, s64 data) {
    push_u8(buf, ((u8) (r&7)<<3) | (u8) X64_RBP);
    
    u8* x64_machine_code_ptr = buf->data + buf->curr_used;
    s64 disp = data - (s64) x64_machine_code_ptr - 4;
    
    push_u32(buf, (u32) disp);
}

inline void
x64_jump_address(X64_Assembler* x64, Buffer* buf, u8** target) {
    if (*target) {
        push_u32(buf, (u32) (*target - (buf->data + buf->curr_used + 4)));
    } else {
        X64_Jump_Patch patch = {};
        patch.origin = buf->data + buf->curr_used;
        patch.target = target;
        array_push(x64->jump_patches, patch);
        push_u32(buf, 0);
    }
}

inline void
x64_jump_address_for_label(X64_Assembler* x64, Buffer* buf, Bytecode_Function* func, u32 label_index) {
    if (label_index > 0) {
        label_index = x64->block_stack[label_index - 1].label_index;
    }
    
    x64_jump_address(x64, buf, &x64->curr_function->labels[label_index]);
}

inline void
x64_modrm(Buffer* buf, u8 reg, u8 rm, s64 disp) {
    reg = reg&7;
    rm = rm&7;
    if (disp < S8_MIN || disp > S8_MAX) {
        push_u8(buf, MODRM_INDIRECT_DISP32 | (reg<<3) | rm&7);
        if (rm == X64_RSP) {
            push_u8(buf,  (rm << 3) | rm);
        }
        push_u32(buf, (u32) disp);
    } else {
        push_u8(buf, MODRM_INDIRECT_DISP8 | (reg<<3) | rm&7);
        if (rm == X64_RSP) {
            push_u8(buf,  (rm << 3) | rm);
        }
        push_u8(buf, (u8) disp);
    }
}

inline void
x64_modrm_sib(Buffer* buf, u8 reg, u8 scale, u8 index, u8 base, s64 disp) {
    push_u8(buf, MODRM_INDIRECT_DISP32 | ((reg&7)<<3) | X64_RSP&7); // reg=RAX, rm=RSP (for SIB)
    push_u8(buf, (scale << 6) | ((index&7) << 3) | (base&7)); // [base + (index * scale) + disp]
    push_u32(buf, (u32) disp); // TODO(Alexander): we can optimize this to use DISP8 too!
}

inline void
x64_modrm_direct(Buffer* buf, u8 reg, u8 rm) {
    push_u8(buf, MODRM_DIRECT | ((reg&7)<<3) | (rm&7));
}

inline void
x64_modrm_rip_relative(Buffer* buf, void* data) {
    s64 disp = (s64) data - (s64) (buf->data + buf->curr_used + 4);
    push_u32(buf, (u32) disp);
}

inline void
x64_create_u32_patch(X64_Assembler* x64, Buffer* buf, X64_Patch_Kind kind, u32 data) {
    X64_Patch patch = {};
    patch.origin = buf->data + buf->curr_used;
    patch.kind = kind; 
    patch.data = data;
    array_push(x64->address_patches, patch);
    push_u32(buf, 0);
}

inline void
x64_modrm_exported_data(X64_Assembler* x64, Buffer* buf, u8 reg, Exported_Data data) {
    push_u8(buf, ((u8) (reg&7)<<3) | (u8) X64_RBP);
    if (x64->use_absolute_ptrs) {
        x64_modrm_rip_relative(buf, data.data);
    } else {
        X64_Patch_Kind kind = X64_PATCH_NONE;
        switch (data.section) {
            case Read_Data_Section: kind = X64_PATCH_READ_ONLY_DATA; break;
            case Data_Section:      kind = X64_PATCH_READ_WRITE_DATA; break;
            default: verify_not_reached();
        }
        x64_create_u32_patch(x64, buf, kind, data.relative_ptr);
    }
}


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
    
    X64_RIP,
};

enum X64_VReg: u8 {
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
    X64_SLOT_RSP_DISP32_INPLACE,
    X64_SLOT_RSP_DISP32,
    X64_SLOT_RAX,
};

struct X64_Slot {
    X64_Slot_Kind kind;
    Bytecode_Type type;
    s32 disp;
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

struct X64_Assembler {
    Bytecode* bytecode;
    u32* stack;
    
    Memory_Arena arena;
    X64_Slot* slots;
    int allocated_slots[X64_REG_COUNT];
    
    Data_Packer* data_packer;
    
    X64_Function* curr_function;
    X64_Function* functions;
    array(X64_Block)* block_stack;
    u32 label_index;
    
    u32 curr_bytecode_insn_index;
    
    array(X64_Jump_Patch)* jump_patches;
    array(X64_Patch)* address_patches;
    
    s32 current_stack_size;
    s32 max_stack_size;
    
    bool use_absolute_ptrs;
};

inline X64_Slot
get_slot(X64_Assembler* x64, int register_index) {
    assert(register_index >= 0);
    return x64->slots[register_index];
}

inline void
set_slot(X64_Assembler* x64, int register_index, X64_Slot_Kind kind, Bytecode_Type type, s32 disp) {
    x64->slots[register_index] = { kind, type, disp };
}

inline void
set_slot(X64_Assembler* x64, int register_index, X64_Slot slot) {
    x64->slots[register_index] = slot;
}

inline s32
register_stack_alloc(X64_Assembler* x64, int register_index, Bytecode_Type type, 
                     s32 size, s32 align, bool store_inplace) {
    s32 result = x64->current_stack_size;
    result = (s32) align_forward(result, align);
    x64->current_stack_size = result + size;
    x64->max_stack_size = max(x64->max_stack_size, x64->current_stack_size);
    X64_Slot_Kind slot_kind = store_inplace ? X64_SLOT_RSP_DISP32_INPLACE : X64_SLOT_RSP_DISP32;
    set_slot(x64, register_index, slot_kind, type, result);
    
    return result;
}

inline s32
register_displacement(X64_Assembler* x64, int register_index, Bytecode_Type type=BC_PTR) {
    X64_Slot slot = get_slot(x64, register_index);
    //pln("register_displacement - r%, %, size % (disp %)", f_int(register_index), f_cstring(bc_type_names[type.kind]), f_int(type.size), f_int(slot.disp));
    if (slot.kind == X64_SLOT_EMPTY) {
        return register_stack_alloc(x64, register_index, type, 8, 8, false);
    }
    
    slot.type = type;
    set_slot(x64, register_index, slot);
    return slot.disp;
}

// TODO(Alexander): we should probably return something more approporiate.
X64_Assembler convert_bytecode_to_x64_machine_code(Bytecode* bytecode, 
                                                   Buffer* buf, 
                                                   Data_Packer* data_packer,
                                                   bool is_absolute_ptrs);

void convert_bytecode_function_to_x64_machine_code(X64_Assembler* x64,
                                                   Bytecode_Function* func,
                                                   Buffer* buf);

void convert_bytecode_insn_to_x64_machine_code(X64_Assembler* x64, 
                                               Buffer* buf,
                                               Bytecode_Function* func,
                                               Bytecode_Instruction* insn);


global const X64_Reg int_arg_registers_ccall_windows[] {
    X64_RCX, X64_RDX, X64_R8, X64_R9
};

global const X64_VReg float_arg_registers_ccall_windows[] {
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
x64_rex(Buffer* buf, u8 flags, u8 reg=0, u8 rm=0) {
    push_u8(buf, REX_PATTERN | flags | ((reg&8)>>1) | (rm&8)>>3);
}

inline void
x64_sib(Buffer* buf, u8 scale, u8 index, u8 base) {
    push_u8(buf, scale << 6 | index << 3 | base);
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
x64_modrm(Buffer* buf, u8 reg, u8 rm, s64 disp, s64 rip) {
    reg = reg % 16; 
    rm = rm % 16;
    
    if (disp < S8_MIN || disp > S8_MAX) {
        push_u8(buf, MODRM_INDIRECT_DISP32 | (( reg&7)<<3) |  rm&7);
        if (rm == X64_RSP) {
            push_u8(buf,  (rm << 3) |  rm);
        }
        push_u32(buf, (u32) disp);
    } else {
        push_u8(buf, MODRM_INDIRECT_DISP8 | (( reg&7)<<3) |  rm&7);
        if (rm == X64_RSP) {
            push_u8(buf,  (rm << 3) |  rm);
        }
        push_u8(buf, (u8) disp);
    }
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

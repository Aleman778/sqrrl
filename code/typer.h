
struct Type_Context {
    Memory_Arena* arena;
    
    Ast_File* file;
    Ast_Block* block;
    Ast_Type* return_type;
    
    int error_count;
};

void
type_error(Type_Context* tcx, string message, Span span);

// Sample error message:
// 
// error: conversion from `f32` to `int`, possible loss of data
//   |
// 43| int var = foo;
//   |           ~~~

inline void
type_error_lossy_conversion(Type_Context* tcx, Ast_Type* dest, Ast_Type* src, Span span) {
    type_error(tcx, 
               string_print("conversion from `%` to `%`, possible loss of data",
                            f_type(src), f_type(dest)),
               span);
}

inline void
begin_block(Type_Context* tcx, Ast_Block* block) {
    block->parent = tcx->block;
    tcx->block = block;
}

inline void
end_block(Type_Context* tcx) {
    assert(tcx->block);
    tcx->block = tcx->block->parent;
}

Ast_Type* infer_expression(Type_Context* tcx, Ast_Expression* expr);

Ast_Type* infer_binary_expression(Type_Context* tcx, Ast_Binary* binary);

bool infer_block(Type_Context* tcx, Ast_Block* block);

bool infer_function(Type_Context* tcx, Ast_Procedure* proc);

Ast_Type* infer_declaration(Type_Context* tcx, Ast_Declaration* decl);

bool check_expression(Type_Context* tcx, Ast_Expression* expr);

bool check_block(Type_Context* tcx, Ast_Block* block);
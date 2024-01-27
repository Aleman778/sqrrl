
#if 0
struct Preprocessor_Macro {
    u64 integral;
    string source;
    map(string_id, u32)* arg_mapper;
    b32 is_functional;
    b32 is_variadic;
    b32 is_valid;
};

struct Preprocessor_Line {
    string substring;
    umm curr_line_number;
    umm next_line_number;
    umm file_index;
};

struct Replacement_List {
    array(string)* list;
    b32 success;
};

struct Preprocessor_State {
    Tokenizer tokenizer;
    u8* curr_line;
    umm curr_line_number;
    
    bool is_c_or_cpp_file;
    bool is_system_header;
    bool is_valid;
};

struct Preprocessor {
    array(Preprocessor_State)* file_stack;
    
    map(string_id, Preprocessor_Macro)* macros;
    map(string_id, bool)* macro_in_use;
    
    String_Builder output;
    array(Source_Group)* source_groups;
    s64 preprocessed_lines;
    
    array(If_Stk_Status)* if_result_stack;
    b32 curr_branch_taken;
    
    u32 curr_file_index;
    map(u32, u32)* loaded_file_indices;
    
    s32 error_count;
    
    bool abort_curr_file;
    bool is_system_header;
};
#endif


enum If_Stk_Status {
    IfStk_Not_Taken = 0,
    IfStk_Taken,
    IfStk_Prev_Taken,
};

struct Preprocessor {
    String_Builder dest;
    u8* src;
    u64 src_offset;
    u64 src_count;
    
    u32 line_number;
    u32 column_number;
    
    array(If_Stk_Status)* if_result_stack;
    bool curr_branch_taken;
    
    u32 file_index;
    string file;
    
    u32 error_count;
};

inline void
preprocess_error(Preprocessor* p, string message) {
    p->error_count++;
    pln("%:%:%: error: %", 
        f_string(p->file), f_umm(p->line_number + 1), f_umm(p->column_number + 1),
        f_string(message));
    DEBUG_log_backtrace();
}

inline bool
check_if_curr_branch_is_taken(array(If_Stk_Status)* if_stack) {
    bool result = true;
    for_array_v (if_stack, it, _) {
        result = result && (it == IfStk_Taken);
    }
    return result;
}


#if 0

void preprocess_expand_macro(Preprocessor* preprocessor, 
                             String_Builder* sb, 
                             Tokenizer* t, 
                             Preprocessor_Macro macro, 
                             Replacement_List args);
#endif
//void push_file_to_preprocess(Preprocessor* preprocessor, 
//string source, string filepath, string extension, 
//int file_index, bool is_system_header);


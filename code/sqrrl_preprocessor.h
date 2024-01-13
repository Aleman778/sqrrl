
struct Preprocessor_Macro {
    u64 integral;
    string source;
    map(string_id, u32)* arg_mapper;
    b32 is_integral;
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

enum If_Stk_Status {
    IfStk_Not_Taken = 0,
    IfStk_Taken,
    IfStk_Prev_Taken,
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

internal void
preprocess_error(Preprocessor* preprocessor, string message) {
    
    Tokenizer* t = &array_last(preprocessor->file_stack).tokenizer;
    preprocessor->error_count++;
    pln("%:%:%: error: %", 
        f_string(t->file), f_umm(t->line_number + 1), f_umm(t->column_number + 1),
        f_string(message));
    DEBUG_log_backtrace();
}


void preprocess_expand_macro(Preprocessor* preprocessor, 
                             String_Builder* sb, 
                             Tokenizer* t, 
                             Preprocessor_Macro macro, 
                             Replacement_List args);

void push_file_to_preprocess(Preprocessor* preprocessor, 
                             string source, string filepath, string extension, 
                             int file_index, bool is_system_header);

string preprocess_file(Preprocessor* preprocecssor);

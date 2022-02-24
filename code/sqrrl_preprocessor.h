
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
    u32 curr_line_number;
    u32 next_line_number;
    u32 file_index;
};

struct Replacement_List {
    array(string)* list;
    b32 success;
};

struct Source_Group {
    umm offset;
    umm count;
    
    u32 line;
    // TODO(Alexander): we need to record expanded macro spans here, column will just be 0 always
    u32 column;
    u32 file_index;
};

struct Preprocessor {
    Tokenizer* tokenizer;
    
    map(string_id, Preprocessor_Macro)* macros;
    
    String_Builder output;
    array(Source_Group)* source_groups;
    
    array(bool)* if_result_stack;
    b32 curr_branch_taken;
    
    s32 error_count;
};

internal void
preprocess_error(Preprocessor* preprocessor, string message) {
    Tokenizer* t = preprocessor->tokenizer;
    preprocessor->error_count++;
    pln("%:%:%: error: %", 
        f_string(t->file), f_umm(t->line_number), f_umm(t->column_number), 
        f_string(message));
    DEBUG_log_backtrace();
}


void preprocess_expand_macro(Preprocessor* preprocessor, 
                             String_Builder* sb, 
                             Tokenizer* t, 
                             Preprocessor_Macro macro, 
                             Replacement_List args);


string preprocess_file(Preprocessor* preprocecssor, string source, string filename, int file_index);

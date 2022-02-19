
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
};

struct Replacement_List {
    array(string)* list;
    b32 success;
};

struct Preprocessor {
    map(string_id, Preprocessor_Macro)* macros;
    String_Builder output;
    s32 error_count;
};

internal void
preprocess_error(Preprocessor* preprocessor, string message) {
    preprocessor->error_count++;
    pln("preprocess error: %", f_string(message));
    DEBUG_log_backtrace();
}

string preprocess_file(Preprocessor* preprocecssor, string source, string filename);

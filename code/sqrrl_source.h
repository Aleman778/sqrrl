
struct Loaded_Source_File {
    string abspath;
    string filedir;
    string filename;
    string source;
    u32 index;
    b32 is_valid;
    // TODO(Alexander): maybe flag for system header files?
};

global string working_directory = {};
global array(Loaded_Source_File)* loaded_source_files = 0;
global map(string_id, u32)* file_index_table = 0;

internal Loaded_Source_File
push_source_file(Read_File_Result file, string filename, Canonicalized_Path canonicalized_path) {
    
    Loaded_Source_File result = {};
    result.abspath = string_copy(string_lit(canonicalized_path.fullpath));
    result.filedir = string_copy(string_view((u8*) canonicalized_path.fullpath, 
                                             (u8*) canonicalized_path.file_part));
    result.filename = string_copy(filename);
    result.source = create_string(file.contents_size, (u8*) file.contents);
    result.index = (u32) array_count(loaded_source_files);
    result.is_valid = file.contents != 0;
    array_push(loaded_source_files, result);
    
    //string filepath_lc = string_copy(canonicalized_path);
    //string_to_lower_ascii_no_copy(&filepath_lc);
    string_id ident = vars_save_cstring(canonicalized_path.fullpath);
    //string_free(filepath_lc);
    map_put(file_index_table, ident, result.index);
    
    return result;
}

inline Loaded_Source_File*
get_source_file_by_index(u32 index) {
    if (index >= array_count(loaded_source_files)) {
        return 0;
    }
    
    return loaded_source_files + index;
}

inline int 
get_source_file_index_by_ident(string_id ident) {
    return map_get(file_index_table, ident);
}

inline Loaded_Source_File*
get_source_file_by_ident(string_id ident) {
    u32 index = get_source_file_index_by_ident(ident);
    return get_source_file_by_index(index);
}


inline Loaded_Source_File*
get_source_file_by_path(cstring filepath) {
    //string_to_lower_ascii_no_copy(&filepath);
    
    string_id ident = vars_save_cstring(filepath);
    return get_source_file_by_ident(ident);
}

Loaded_Source_File
read_entire_source_file(string filename, Loaded_Source_File* current_file = 0) {
    // TODO(Alexander): hackish way to join two paths!!! Use OS service for this later.
    //string filepath = string_concat(working_directory, filename);
    
    cstring curr_file_path = 0;
    if (current_file) {
        curr_file_path = string_to_cstring(current_file->filedir);
    }
    
    cstring cfilename = string_to_cstring(filename);
    Canonicalized_Path canonicalized_path = DEBUG_get_canonicalized_path(cfilename, curr_file_path);
    cstring_free(cfilename);
    if (curr_file_path) {
        cstring_free(curr_file_path);
    }
    
    Loaded_Source_File* prev_loaded_file = get_source_file_by_path(canonicalized_path.fullpath);
    if (prev_loaded_file && prev_loaded_file->is_valid) {
        DEBUG_free_canonicalized_path(canonicalized_path);
        return *prev_loaded_file;
    } else {
        prev_loaded_file = get_source_file_by_path(canonicalized_path.fullpath);
        if (prev_loaded_file && prev_loaded_file->is_valid) {
            DEBUG_free_canonicalized_path(canonicalized_path);
            return *prev_loaded_file;
        }
    }
    
    Read_File_Result file = DEBUG_read_entire_file(canonicalized_path.fullpath);
    Loaded_Source_File result = push_source_file(file, filename, canonicalized_path);
    DEBUG_free_canonicalized_path(canonicalized_path);
    return result;
}

Loaded_Source_File
read_entire_system_header_file(string filename) {
    cstring cfilename = string_to_cstring(filename);
    Canonicalized_Path canonicalized_path = DEBUG_get_system_canonicalized_path(cfilename);
    cstring_free(cfilename);
    
    Loaded_Source_File* prev_loaded_file = get_source_file_by_path(canonicalized_path.fullpath);
    if (prev_loaded_file && prev_loaded_file->is_valid) {
        DEBUG_free_canonicalized_path(canonicalized_path);
        return *prev_loaded_file;
    }
    
    Read_File_Result file = DEBUG_read_entire_file(canonicalized_path.fullpath);
    Loaded_Source_File result = push_source_file(file, filename, canonicalized_path);
    DEBUG_free_canonicalized_path(canonicalized_path);
    return result;
}

void
free_file_memory(u32 index) {
    Loaded_Source_File* file = get_source_file_by_index(index);
    if (file) {
        DEBUG_free_file_memory(file);
    }
}

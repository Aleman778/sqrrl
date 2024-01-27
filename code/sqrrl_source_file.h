
struct Loaded_Source_File {
    string abspath;
    string filedir;
    string filename;
    string extension;
    string source;
    
    array(smm)* lines;
    
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
    for (smm char_index = result.filename.count - 1; char_index >= 0; char_index--) {
        if (result.filename.data[char_index] == '.' || char_index == 0) {
            result.extension = string_copy(string_view(result.filename.data + char_index + 1, 
                                                       result.filename.data + result.filename.count));
            break;
        }
    }
    
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
    cstring curr_file_path = 0;
    if (current_file) {
        curr_file_path = string_to_cstring(current_file->filedir);
    }
    
    // TODO(Alexander): add temporary allocator for better performance
    cstring cfilename = string_to_cstring(filename);
    cstring cworking_dir = string_to_cstring(working_directory);
    Canonicalized_Path canonicalized_path = DEBUG_get_canonicalized_path(cfilename, cworking_dir, curr_file_path);
    cstring_free(cfilename);
    cstring_free(cworking_dir);
    if (curr_file_path) {
        cstring_free(curr_file_path);
    }
    
    if (!canonicalized_path.success) {
        return *get_source_file_by_index(0);
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
    
    if (!canonicalized_path.success) {
        return *get_source_file_by_index(0);
    }
    
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

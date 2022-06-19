
struct Loaded_Source_File {
    string filepath;
    string source;
    u32 index;
    b32 is_valid;
    // TODO(Alexander): maybe flag for system header files?
};

global string working_directory = {};
global array(Loaded_Source_File)* loaded_source_files = 0;
global map(string_id, u32)* file_index_table = 0;


internal Loaded_Source_File
push_source_file(Read_File_Result file, string filepath) {
    Loaded_Source_File result = {};
    result.filepath = filepath;
    result.source = create_string(file.contents_size, (u8*) file.contents);
    result.index = (u32) array_count(loaded_source_files);
    result.is_valid = file.contents != 0;
    array_push(loaded_source_files, result);
    
    string_id ident = vars_save_string(filepath);
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
read_source_file_by_path(string filepath) {
    string_id ident = vars_save_string(filepath);
    return get_source_file_by_ident(ident);
}

Loaded_Source_File
read_entire_source_file(string filename) {
    // TODO(Alexander): hackish way to join two paths!!! Use OS service for this later.
    string filepath = string_concat(working_directory, filename);
    
    Loaded_Source_File* prev_loaded_file = read_source_file_by_path(filepath);
    if (prev_loaded_file && prev_loaded_file->is_valid) {
        return *prev_loaded_file;
    }
    
    cstring cfilepath = string_to_cstring(filepath);
    
    Read_File_Result file = DEBUG_read_entire_file(cfilepath);
    cstring_free(cfilepath);
    return push_source_file(file, filepath);
}

Loaded_Source_File
read_entire_system_header_file(string filename) {
    Loaded_Source_File* prev_loaded_file = read_source_file_by_path(filename);
    if (prev_loaded_file && prev_loaded_file->is_valid) {
        return *prev_loaded_file;
    }
    
    cstring cfilename = string_to_cstring(filename);
    Read_File_Result file = DEBUG_read_entire_system_header(cfilename);
    cstring_free(cfilename);
    return push_source_file(file, string_copy(filename));
}

void
free_file_memory(u32 index) {
    Loaded_Source_File* file = get_source_file_by_index(index);
    if (file) {
        DEBUG_free_file_memory(file);
    }
}

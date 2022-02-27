
struct Loaded_Source_File {
    string filepath;
    string source;
    u32 index;
    b32 is_valid;
};

global string working_directory = {};
global array(Loaded_Source_File)* loaded_source_files = 0;
global map(string_id, u32)* file_index_table = 0;



Loaded_Source_File
read_entire_file(string filename) {
    Loaded_Source_File result = {};
    
    // TODO(Alexander): hackish way to join two paths!!! Use OS service for this later.
    string filepath = string_concat(working_directory, filename);
    
    Read_File_Result file = DEBUG_read_entire_file(string_to_cstring(filepath));
    result.filepath = filepath;
    result.source = create_string(file.contents_size, (u8*) file.contents);
    result.index = (u32) array_count(loaded_source_files);
    result.is_valid = file.contents != 0;
    
    array_push(loaded_source_files, result);
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

void
free_file_memory(u32 index) {
    Loaded_Source_File* file = get_source_file_by_index(index);
    if (file) {
        DEBUG_free_file_memory(file);
    }
}

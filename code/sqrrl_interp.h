
struct Interp {
    Scope global_scope;
    
    array(Source_File*)* source_files;
    array(Compilation_Unit)* compilation_units;
};

void
interp_push_source_file(Interp* interp, string filename, Source_File* included_from=0) {
    Source_File* source_file = create_source_file(filename, included_from);
    
    bool duplicate = false;
    for_array_v(interp->source_files, existing_file, _) {
        if (source_file->index == existing_file->index) {
            duplicate = true;
        }
    }
    if (!duplicate) {
        array_push(interp->source_files, source_file);
    }
}

inline void*
interp_get_data_pointer(Interp* interp, string_id ident) {
    unimplemented;
    return 0;
}

inline Value
interp_get_value(Interp* interp, string_id ident) {
    return map_get(interp->global_scope.entries, ident).value;
}

inline void
interp_put_global(Interp* interp, string_id ident, Type* type, Value value) {
    // TODO(Alexander): check name collisions!!!
    Type_And_Value tv = { type, value };
    map_put(interp->global_scope.entries, ident, tv);
}

//inline void
//interp_eval_directives() {

//}
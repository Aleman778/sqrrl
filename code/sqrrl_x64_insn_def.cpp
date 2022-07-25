#include "sqrrl_json.h"


#define for_json_array(json_array, it, it_name, it_value) \
json_array_element_s* it = json_array->start; \
json_string_s* it_name = it->name; \
json_value_s* it_value = it->value; \
for (; it; it = it->next, it_name = it ? it->name : 0, it_value = it ? it->value : 0)

#define for_json_object(json_object, it, it_name, it_value) \
json_object_element_s* it = json_object->start; \
json_string_s* it_name = it->name; \
json_value_s* it_value = it->value; \
for (; it; it = it->next, it_name = it ? it->name : 0, it_value = it ? it->value : 0)

#define json_bool_value(val) val->type == json_type_true
#define json_string_value(val) create_string(((json_string_s*) val->payload)->string_size, \
(u8*) ((json_string_s*) val->payload)->string)

inline u8
json_hex_byte_value(json_value_s* val) {
    string hex = json_string_value(val);
    assert(hex.count == 2);
    u8 second = (u8) hex_digit_to_s32(hex.data[0]);
    u8 first = (u8) hex_digit_to_s32(hex.data[1]);
    return (second << 4) | first;
}

inline u8
x64_encode_operand(json_object_s* operand, X64_Operand_Kind_Table* operand_kind_table) {
    if (strcmp(operand->start->name->string,  "type") == 0) {
        json_string_s* type = (json_string_s*) operand->start->value->payload;
        X64_Operand_Kind kind = string_map_get(operand_kind_table, type->string);
        return (u8) kind;
    } else if (strcmp(operand->start->name->string,  "id") == 0) {
        // TODO: used for specific register.
        return 0;
    } else {
        assert(0 && "invalid operand type");
    }
    
    return 0;
}

inline u8
x64_encode_operand_field(json_value_s* value) {
    u8 result = 0;
    string val = json_string_value(value);
    assert(val.count == 1 || val.count == 2);
    if (val.count == 1) {
        result = (u8) hex_digit_to_s32(val.data[0]) << 3 | 0b000000111;
    } else if (val.count == 2 && val.data[0] == '#') {
        result = (u8) hex_digit_to_s32(val.data[1]);
    }
    return result;
}


X64_Instruction_Def_Table*
parse_x86_64_definitions() {
    X64_Instruction_Def_Table* result = 0;
    
    // Create string to opcode table
    string_map(X64_Opcode)* opcode_table = 0;
#define X64_OPCODE(mnemonic, uc_mnemonic) string_map_put(opcode_table, #uc_mnemonic, X64Opcode_##mnemonic);
#define X64_OPCODE_ALIAS(...)
    DEF_X64_OPCODES
#undef X64_OPCODE_ALIAS
#undef X64_OPCODE
    
    X64_Operand_Kind_Table* operand_kind_table = 0;
#define X64_OP(op) string_map_put(operand_kind_table, #op, X64Operand_##op);
    DEF_X64_OPERANDS
#undef X64_OP
    
    assert(string_map_get(opcode_table, "ADD") == X64Opcode_add);
    
    
    string filepath = string_lit("data/x86_64.json");
    Read_File_Result x64_file = DEBUG_read_entire_file(string_to_cstring(filepath));
    assert(x64_file.contents_size > 0);
    
    json_value_s* root = json_parse(x64_file.contents, x64_file.contents_size);
    
    json_object_s* root_object = (struct json_object_s*)root->payload;
    assert(root_object->length == 2);
    
    json_object_element_s* insn_set = root_object->start;
    assert(strcmp(insn_set->name->string, "instruction_set") == 0);
    assert(strcmp(((json_string_s*) insn_set->value->payload)->string, "x86-64") == 0);
    
    json_object_element_s* insns_elem = insn_set->next;
    assert(strcmp(insns_elem->name->string, "instructions") == 0);
    assert(insns_elem->value->type == json_type_object);
    
    json_object_s* insns = (json_object_s*) insns_elem->value->payload;
    //pln("\n\nsqrrl_x64_insn_def.cpp: number of instructions loaded: %", f_umm(insns->length));
    
    
    json_object_element_s* curr_insn = insns->start;
    for (int insn_index = 0; insn_index < insns->length; insn_index++) {
        json_object_s* insn = (json_object_s*) curr_insn->value->payload;
        json_string_s* insn_mnemonic = curr_insn->name;
        X64_Opcode opcode = string_map_get(opcode_table, insn_mnemonic->string);
        if (opcode == X64Opcode_invalid) {
            curr_insn = curr_insn->next;
            continue;
        }
        
        assert(strcmp(insn->start->next->name->string, "forms") == 0);
        json_array_s* insn_forms = (json_array_s*) insn->start->next->value->payload;
        
        json_array_element_s* curr_form_elem = insn_forms->start;
        if (!curr_form_elem) continue;
        
        
        // Build all the different forms this instruction can be in
        while (curr_form_elem) {
            json_object_s* curr_form = (json_object_s*) curr_form_elem->value->payload;
            
            X64_Instruction_Index index = {};
            index.opcode = (u8) opcode;
            
            X64_Encoding encoding = {};
            
            for_json_object(curr_form, curr_form_entry_elem, entry_name, entry_value) {
                if (strcmp(entry_name->string, "operands") == 0) {
                    // Operands
                    json_array_s* operands = (json_array_s*) entry_value->payload;
                    if (operands) {
                        json_array_element_s* op0 = operands->start;
                        if (op0) {
                            index.op0 = x64_encode_operand((json_object_s*) op0->value->payload, operand_kind_table);
                            json_array_element_s* op1 = op0->next;
                            
                            if (op1) {
                                index.op1 = 
                                    x64_encode_operand((json_object_s*) op1->value->payload, operand_kind_table);
                                json_array_element_s* op2 = op1->next;
                                
                                if (op2) {
                                    index.op2 = 
                                        x64_encode_operand((json_object_s*) op2->value->payload, operand_kind_table);
                                }
                            }
                        }
                    }
                } else if (strcmp(entry_name->string, "encodings") == 0) {
                    
                    // Encoding
                    json_array_s* encodings = (json_array_s*) entry_value->payload;
                    encoding.is_valid = true;
                    encoding.modrm_mod = ModRM_not_used;
                    // TODO(Alexander): we will start with a single encoding
                    json_array_element_s* encoding_elem = encodings->start;
                    if (encoding_elem) {
                        json_object_s* encoding_parts = (json_object_s*) encoding_elem->value->payload;
                        
                        json_object_element_s* curr_encoding_part_elem = encoding_parts->start;
                        while (curr_encoding_part_elem) {
                            if (!curr_encoding_part_elem) {
                                break;
                            }
                            json_object_s* curr_encoding_part = (json_object_s*)
                                curr_encoding_part_elem->value->payload;
                            
                            cstring part_name = curr_encoding_part_elem->name->string;
                            json_value_s* part_value = (json_value_s*) curr_encoding_part_elem->value;
                            
                            if (strcmp(part_name, "use_prefix") == 0) {
                                encoding.use_prefix = json_bool_value(part_value);
                            } else if (strcmp(part_name, "use_0f_prefix") == 0) {
                                encoding.use_0f_prefix = json_bool_value(part_value);
                            } else if (strcmp(part_name, "use_rex_prefix") == 0) {
                                encoding.use_rex_prefix = json_bool_value(part_value);
                            } else if (strcmp(part_name, "use_rex_w") == 0) {
                                encoding.use_rex_w = json_bool_value(part_value);
                            } else if (strcmp(part_name, "use_opcode_addend") == 0) {
                                encoding.use_opcode_addend = json_bool_value(part_value);
                            } else if (strcmp(part_name, "prefix") == 0) {
                                encoding.prefix = json_hex_byte_value(part_value);
                            } else if (strcmp(part_name, "primary_opcode") == 0) {
                                encoding.primary_opcode = json_hex_byte_value(part_value);
                            } else if (strcmp(part_name, "secondary_opcode") == 0) {
                                encoding.secondary_opcode = json_hex_byte_value(part_value);
                            } else if (strcmp(part_name, "opcode_addend") == 0) {
                                encoding.opcode_addend = x64_encode_operand_field(part_value);
                            } else if (strcmp(part_name, "modrm_mod_direct") == 0) {
                                bool is_direct = json_bool_value(part_value);
                                encoding.modrm_mod = (u8) (is_direct ? ModRM_direct : ModRM_indirect);
                            } else if (strcmp(part_name, "modrm_reg") == 0) {
                                encoding.modrm_reg = x64_encode_operand_field(part_value);
                            } else if (strcmp(part_name, "modrm_rm") == 0) {
                                encoding.modrm_rm = x64_encode_operand_field(part_value);
                            } else if (strcmp(part_name, "imm_size") == 0) {
                                string size = json_string_value(part_value);
                                assert(size.count == 1);
                                encoding.imm_size = (u8) (size.data[0] - '0');
                            } else if (strcmp(part_name, "imm_op") == 0) {
                                encoding.imm_op = x64_encode_operand_field(part_value);
                            } else {
                                assert(0 && "invalid encoding part");
                            }
#if 0
                            if (strcmp(part_name, "REX") == 0) {
                                
                                for_json_object(part_value, it, name, value) {
                                    if (strcmp(name->string, "mandatory") == 0) {
                                        encoding.is_rex_mandatory = json_bool_value(value);
                                    } else if (strcmp(name->string, "W") == 0) {
                                        encoding.set_rex_w = json_string_value(value).data[0] == '1';
                                    }
                                }
                                
                            } else if (strcmp(part_name, "opcode") == 0) {
                                assert(part_value->start);
                                json_string_s* opcode_kind = part_value->start->name;
                                json_value_s* opcode_data = part_value->start->value;
                                
                                if (strcmp(opcode_kind->string, "byte") == 0) {
                                    string hex = json_string_value(opcode_data);
                                    assert(hex.count == 2);
                                    u8 second = (u8) hex_digit_to_s32(hex.data[0]);
                                    u8 first = (u8) hex_digit_to_s32(hex.data[1]);
                                    encoding.opcode = (second << 4) | first;
                                } else {
                                    assert(0 && "unknown opcode data");
                                }
                                
                                json_object_element_s* addend_elem = part_value->start->next;
                                
                                if (addend_elem) {
                                    json_string_s* addend_kind = addend_elem->name;
                                    json_value_s* addend_data = addend_elem->value;
                                    assert(strcmp(addend_kind->string, "addend") == 0);
                                    
                                    encoding.use_opcode_addend = true;
                                    encoding.opcode_addend = x64_encode_operand_field(addend_data);
                                }
                                
                                
                            } else if (strcmp(part_name, "ModRM") == 0) {
                                assert(part_value->start);
                                json_object_element_s* mod_elem = part_value->start;
                                if (mod_elem) {
                                    assert(strcmp(mod_elem->name->string, "mode") == 0);
                                    
                                    string str = json_string_value(mod_elem->value);
                                    assert(str.count == 2);
                                    if (str.data[0] == '1' && str.data[1] == '1') {
                                        encoding.modrm_mod = 0b11000000;
                                    } else {
                                        encoding.modrm_mod = 0b00000000;
                                    }
                                }
                                
                                json_object_element_s* rm_elem = mod_elem->next;
                                if (rm_elem) {
                                    assert(strcmp(rm_elem->name->string, "rm") == 0);
                                    encoding.modrm_rm = x64_encode_operand_field(rm_elem->value);
                                }
                                
                                json_object_element_s* reg_elem = rm_elem->next;
                                if (reg_elem) {
                                    assert(strcmp(reg_elem->name->string, "reg") == 0);
                                    encoding.modrm_reg = x64_encode_operand_field(reg_elem->value);
                                }
                                
                            } else if (strcmp(part_name, "immediate") == 0) {
                                json_object_element_s* size_elem = part_value->start;
                                json_object_element_s* value_elem = size_elem->next;
                                
                                if (size_elem) {
                                    assert(strcmp(size_elem->name->string, "size") == 0);
                                    string size = json_string_value(size_elem->value);
                                    encoding.immediate_size = (u8) (size.data[0] - '0');
                                }
                                
                                if (value_elem) {
                                    assert(strcmp(value_elem->name->string, "value") == 0);
                                    encoding.immediate_operand = x64_encode_operand_field(value_elem->value);
                                }
                            } else {
                                //assert(0 && "invalid encoding part");
                            }
#endif
                            
                            // Next encoding part
                            curr_encoding_part_elem = curr_encoding_part_elem->next;
                        }
                    }
                }
            }
            // Store the resulting index to instruction encoding
            assert(index.opcode != X64Opcode_invalid);
            encoding.is_valid = true;
            map_put(result, index, encoding);
            
            // Next form
            curr_form_elem = curr_form_elem->next;
        }
        
        // Next instruction
        curr_insn = curr_insn->next;
    }
    
    {
        X64_Instruction_Index index = {};
        
        X64_Encoding encoding = {};
        encoding.is_valid = true;
        encoding.imm_op = 0;
        
        index.opcode = X64Opcode_db;
        index.op0 = X64Operand_imm8;
        encoding.imm_size = 1;
        map_put(result, index, encoding);
        
        index.opcode = X64Opcode_dw;
        index.op0 = X64Operand_imm16;
        encoding.imm_size = 2;
        map_put(result, index, encoding);
        
        index.opcode = X64Opcode_dd;
        index.op0 = X64Operand_imm32;
        encoding.imm_size = 4;
        map_put(result, index, encoding);
        
        index.opcode = X64Opcode_dq;
        index.op0 = X64Operand_imm64;
        encoding.imm_size = 8;
        map_put(result, index, encoding);
    }
    
    //pln("sqrrl_x64_insn_def.cpp: number of encodings built: %", f_umm(map_count(result)));
    //pln("sqrrl_x64_insn_def.cpp: size of x86_64 encoding data: %", f_umm(map_count(result)*sizeof(X64_Encoding)));
    
    free(root);
    DEBUG_free_file_memory(x64_file.contents);
    
    return result;
}

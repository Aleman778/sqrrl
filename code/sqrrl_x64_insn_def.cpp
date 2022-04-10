#include "sqrrl_json.h"

typedef string_map(X64_Operand_Kind) X64_Operand_Kind_Table;

u8
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

X64_Instruction_Def_Table*
parse_x86_64_definitions() {
    X64_Instruction_Def_Table* result = 0;
    
    // Create string to opcode table
    string_map(X64_Opcode)* opcode_table = 0;
#define X64_OPCODE(mnemonic, uc_mnemonic) string_map_put(opcode_table, #uc_mnemonic, X64Opcode_##mnemonic);
    DEF_X64_OPCODES
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
    pln("\n\nnumber of instructions loaded: %", f_umm(insns->length));
    
    
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
            if (curr_form->length != 2) {
                break;
            }
            
            json_array_s* operands = (json_array_s*) curr_form->start->value->payload;
            json_array_s* encodings = (json_array_s*) curr_form->start->next->value->payload;
            
            // Operands
            X64_Instruction_Index index = {};
            index.opcode = (u8) opcode;
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
            
            // Encoding
            X64_Encoding encoding = {};
            // TODO(Alexander): we will start with a single encoding
            json_array_element_s* encoding_elem = operands->start;
            if (encoding_elem) {
                json_object_s* encoding_parts = (json_object_s*) encoding_elem->value->payload;
                
                json_object_element_s* curr_encoding_part_elem = encoding_parts->start;
                if (curr_encoding_part_elem) {
                    json_object_s* curr_encoding_part = (json_object_s*) 
                        curr_encoding_part_elem->value->payload;
                    
                    while (curr_encoding_part) {
                        
                        cstring part_name = curr_encoding_part_elem->name->string;
                        json_object_s* part_value = (json_object_s*) curr_encoding_part_elem->value->payload;
                        
                        if (strcmp(part_name, "REX")) {
                            
                        } else if (strcmp(part_name, "opcode")) {
                            //encoding->opcode = 
                            
                        } else if (strcmp(part_name, "ModRM")) {
                            
                        } else if (strcmp(part_name, "immediate")) {
                            
                        } else {
                            assert(0 && "invalid encoding part");
                        }
                        
                        // Next encoding part
                        curr_encoding_part_elem = curr_encoding_part_elem->next;
                        if (!curr_encoding_part_elem) {
                            break;
                        }
                        curr_encoding_part = (json_object_s*)
                            curr_encoding_part_elem->value->payload;
                    }
                }
            }
            
            // Store the resulting index to instruction encoding
            encoding.is_valid = true;
            map_put(result, index, encoding);
            
            // Next form
            curr_form_elem = curr_form_elem->next;
        }
        
        // Next instruction
        curr_insn = curr_insn->next;
    }
    
    
    free(root);
    DEBUG_free_file_memory(x64_file.contents);
    
    return result;
}

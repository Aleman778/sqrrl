
internal inline u8
utf8_calculate_num_bytes(u8 next) {
    if (next & 0x80) {
        u8 mask = 0xC0;
        u8 num_bytes;
        for (num_bytes = 1; ((u8) (next & mask) != (u8) (mask << 1)); num_bytes++) {
            if (num_bytes > 4) {
                return 0;
            }
            mask = (mask >> 1) | 0x80;
        }
        
        if (num_bytes == 1) {
            return 0;
        }
        
        return num_bytes;
    } else {
        return 1;
    }
}

void
utf8_advance_character(Tokenizer* t) {
    if (t->curr) {
        t->column_number++;
        
        if (*t->curr == '\n') {
            t->curr_line = t->next;
            t->line_number++;
            t->column_number = 0;
        }
    }
    
    t->curr = t->next;
    if (t->next >= t->end) {
        t->curr_utf32_character = 0;
        return;
    }
    
    u8 num_bytes = utf8_calculate_num_bytes(*t->next);
    if (num_bytes == 0) {
        t->curr_utf32_character = 0;
        return;
    }
    
    u8 next = *t->next++;
    t->curr_utf32_character = next & utf8_first_byte_mask[num_bytes - 1];
    for (int i = 1; i < num_bytes; i++) {
        if (t->curr >= t->end) {
            //tokenization_error(t, "invalid utf-8 character");
            t->curr_utf32_character = 0;
            return;
        }
        
        if ((u8) (next & 0xC0) == (u8) 0x80) {
            //tokenization_error(t, "invalid utf-8 character");
            t->curr_utf32_character = 0;
            return;
        }
        
        t->curr_utf32_character <<= 6;
        t->curr_utf32_character |= *t->next++ & 0x3F;
    }
    return;
}



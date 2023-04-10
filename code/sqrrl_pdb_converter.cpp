

struct MSF_SuperBlock {
    u8 file_magic[32];
    u32 block_size;
    u32 free_block_map_block;
    u32 num_blocks;
    u32 num_directory_bytes;
    u32 reserved;
    u32 block_map_address;
};

struct PDB_Directory {
    u32 stream_number;
    u32 offset;
    u32 size;
};

struct PdbStreamHeader {
    u32 Version;
    u32 Signature;
    u32 Age;
    u32 UniqueId[4];
};

void
dump_pdb() {
    Read_File_Result file = DEBUG_read_entire_file("sqrrl.pdb");
    
    u8* curr = (u8*) file.contents;
    
    MSF_SuperBlock* msf = (MSF_SuperBlock*) curr;
    curr += sizeof(MSF_SuperBlock);
    
    string file_magic;
    file_magic.data = msf->file_magic;
    file_magic.count = fixed_array_count(msf->file_magic);
    
    pln("MSF_SuperBlock (size = %)", f_u32(sizeof(MSF_SuperBlock)));
    pln("file_magic = %", f_string(file_magic));
    pln("block_size = %", f_u32(msf->block_size));
    pln("free_block_map_block = %", f_u32(msf->free_block_map_block));
    pln("num_blocks = %", f_u32(msf->num_blocks));
    pln("num_directory_bytes = %", f_u32(msf->num_directory_bytes));
    pln("block_map_address = %", f_u32(msf->block_map_address));
    
    
    int block_index = 3;
    
    PdbStreamHeader* pdb_header = (PdbStreamHeader*) (msf + msf->block_size*3);
    pln("Version: %", f_u32(pdb_header->Version));
    pln("Signature: %", f_u32(pdb_header->Signature));
    pln("Age: %", f_u32(pdb_header->Age));
    
#if 0
    for (;;) {
        PDB_Directory* dir = (PDB_Directory*) (msf + msf->block_size*block_index);
        
        pln("Stream number: %", f_u32(dir->stream_number));
        
        if (dir->stream_number == 0) {
            break;
        }
        pln("Offset: %", f_u32(dir->offset));
        pln("Size: %", f_u32(dir->size));
        
        
        //block_index++;
    } 
    
#endif
}
#include <windows.h>
#include <fstream>
#include <vector>

int main() {
  // Create the DOS header and stub.
  IMAGE_DOS_HEADER dos_header;
  dos_header.e_magic = IMAGE_DOS_SIGNATURE; // The DOS signature, which must be the value 0x5A4D (the ASCII values for "MZ").
  dos_header.e_lfanew = sizeof(IMAGE_DOS_HEADER); // The file offset of the PE header, relative to the beginning of the file.

  // Create the PE header and optional PE+ header.
  IMAGE_NT_HEADERS64 pe_header;
  pe_header.Signature = IMAGE_NT_SIGNATURE; // The PE signature, which must be the value 0x00004550 (the ASCII values for "PE").

  // Create the sections of the PE file.
  IMAGE_SECTION_HEADER code_section;
  code_section.Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ; // The characteristics of the section, such as whether it contains code or data, and whether it is readable, writable, or executable.
  code_section.Name[0] = '.'; // The name of the section.
  code_section.Name[1] = 't';
  code_section.Name[2] = 'e';
  code_section.Name[3] = 'x';
  code_section.Name[4] = 't';
  code_section.Name[5] = 0;

  // Populate the sections with the appropriate data.
  std::vector<uint8_t> code_section_data = {
    0x90, 0x90, 0x90, 0x90, // NOP instructions.
    0xB8, 0x00, 0x00, 0x00, 0x00, // MOV EAX, 0
    0xC3 // RET
  };

  // Create the import and export tables for the exe file.
  IMAGE_IMPORT_DESCRIPTOR import_table[1] = {};
  import_table[0].Name = (DWORD)(sizeof(IMAGE_DOS_HEADER) + sizeof(IMAGE_NT_HEADERS64) + sizeof(IMAGE_SECTION_HEADER)); // The name of the DLL that the exe file imports functions from.
  import_table[0].FirstThunk = (DWORD)(sizeof(IMAGE_DOS_HEADER) + sizeof(IMAGE_NT_HEADERS64) + sizeof(IMAGE_SECTION_HEADER) + sizeof(IMAGE_IMPORT_DESCRIPTOR)); // The file offset of the import lookup table.

  IMAGE_THUNK_DATA64 import_lookup_table[1] = {};
  import_lookup_table[0].u1.

  // The DLL that the exe file imports functions from.
const char* dll_name = "Kernel32.dll";

// The names of the functions that the exe file imports from the DLL.
const char* function_names[] = {
  "GetLastError",
  "GetCurrentProcessId",
  "GetCurrentThreadId",
  "GetTickCount",
  "GetSystemTimeAsFileTime",
  "GetCurrentProcess"
};

// Create the import table for the exe file.
IMAGE_IMPORT_DESCRIPTOR import_table[1] = {};
import_table[0].Name = (DWORD)(sizeof(IMAGE_DOS_HEADER) + sizeof(IMAGE_NT_HEADERS64) + sizeof(IMAGE_SECTION_HEADER)); // The file offset of the DLL name in the exe file.
import_table[0].FirstThunk = (DWORD)(sizeof(IMAGE_DOS_HEADER) + sizeof(IMAGE_NT_HEADERS64) + sizeof(IMAGE_SECTION_HEADER) + sizeof(IMAGE_IMPORT_DESCRIPTOR)); // The file offset of the import lookup table.

// Create the import lookup table for the exe file.
std::vector<IMAGE_THUNK_DATA64> import_lookup_table;
for (const char* function_name : function_names) {
  // Create an IMAGE_IMPORT_BY_NAME structure for the function.
  size_t function_name_length = strlen(function_name);
  std::vector<uint8_t> function_name_data(sizeof(IMAGE_IMPORT_BY_NAME) + function_name_length);
  IMAGE_IMPORT_BY_NAME* function_name_struct = reinterpret_cast<IMAGE_IMPORT_BY_NAME*>(function_name_data.data());
  function_name_struct->Hint = 0; // The hint, which is a 16-bit index into the export table of the DLL.
  memcpy(function_name_struct->Name, function_name, function_name_length + 1); // The name of the imported function.

  // Create an IMAGE_THUNK_DATA64 structure for
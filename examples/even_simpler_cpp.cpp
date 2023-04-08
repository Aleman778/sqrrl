

int global_var = 10;

int* global_ptr = &global_var;


int
main() {
    return *global_ptr + 20;
}

int
mainCRTStartup() {
    
    return main();
}
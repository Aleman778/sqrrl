

int
main() {
    
    
    //__debugbreak();
    debug_break();
    return 0;
}

int
mainCRTStartup() {
    return main();
}
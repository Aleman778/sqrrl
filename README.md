# The Sqrrl Programming Language
The main source code repository for the Sqrrl Programming Language, the core libraries, documentation and examples.
This language is designed to be similar to C with improvements and additional features from C++ and other languages.
If you are familiar with C/C++ this language will feel right at home for you and it will allow you to take advantage
of the [new features](#new-features).

```diff
- This language is a hobby project and can contain bugs and hardcoded stuff, don't use it for any serious projects.
```

`examples/reverse_polish_notation.sq`
```C++
// https://en.wikipedia.org/wiki/Reverse_Polish_notation
// Very simple calculator using RPN

#include <basic.sq>

int
main() {
    string expr = "14 40 + 32 -";
    
    [2]int stack;
    int stack_pointer;
    for (int i = 0; i < expr.count; i++) {
        u8 chr = expr[i];
        switch (chr) {
            case '+': {
                pln("% + %", stack[0], stack[1]);
                assert(stack_pointer >= 2);
                stack[0] += stack[1]; 
                stack[1] = 0;
                stack_pointer--;
            }
            
            case '-': {
                pln("% - %", stack[0], stack[1]);
                assert(stack_pointer >= 2);
                stack[0] -= stack[1];
                stack[1] = 0;
                stack_pointer--;
            }
            
            case: {
                if (chr >= '0' && chr <= '9') {
                    stack[stack_pointer] = stack[stack_pointer]*10 + chr - '0';
                } else if (stack[stack_pointer] > 0) {
                    assert(stack_pointer < stack.count, "stack overflow");
                    stack_pointer++;
                }
            }
        }
    }
    
    assert(stack_pointer >= 1);
    pln("% = %", expr, stack[0]);
}
```

`Output:`
```
14 + 40
54 - 32
14 40 + 32 - = 22

```

<h2 id="new-features">New features</h2>

### Already implemented

- **Zero initialization by default:** very simply all variables you declare will be initialized to zero unless you specify something else.
This works for local and global variables, arrays, structs etc.
- **Saner types:** C/C++ likes to go crazy with types, but in this language we use only sized types and sometimes pointer-wide types (same as address size of target CPU architecture).
  - Signed integers: `s8`, s16`, `s32`, `s64`.
  - Unsigned integers: `u8`, `u16`, `u32`, `u64`.
  - Boolean type: `bool` can have states `true`/ `false` (actually stored as `u8`).
  - Pointer-wide integer: `smm`, `umm`.
  - Floating point: `f32`, `f64`.
  - There is no character type in this language so use `u8` instead.
- **Safety and syntax improvement for fixed sized arrays:** arrays are stored as "wide" pointers, actually they
are stored as a struct containing a data pointer (`T*`) and the count (`smm`). You can either specify a size e.g. `[10]int` or leave it unknown `[]int` both have still a fixed count.
- **Safer `string` type:** like arrays string are also stored with the count. NOTE: `string` type is not null-terminated and you should always rely on the count in e.g. for loops.
But for interacting with C libraries there is a null-terminated `cstring` type that can be converted to using `string_to_cstring` function.
- **Operator and function overloading:** exactly the same as in C++.
- **Nicer struct/ union literals**: check out this example:
```C++
union v4 {
    struct {
        f32 x, y, z, w;
    }; 
    struct {
        f32 r, g, b, a;
    };
    [4]f32 data;
}
v4 a = { 0.0f, 0.5f, 1.0f, 1.0f };
v4 b = { r: 0.0f, g: 0.5f, b: 1.0f, a: 1.0f };
v4 c = { g: 0.5f, b: 1.0f, a: 1.0f };
```
both `a`, `b` and `c` are the same but you can choose to include the individual fields, non included ones are set to zero.
- **Type introspection:** this gives every type in you program some metadata that is stored in the final executable. E.g. `print` or `pln` function uses this
to print out whole data structures without user having to specify anything. In the example above we can print out for example `c` using `pln("c = %", c);`.
This gives us the following output:
```
c = v4 {
    x: 0.000000, 
    y: 0.500000, 
    z: 1.000000, 
    w: 1.000000, 
}
```
For now it will always use the first field in unions.
- **Fast compilation:** maybe not considered a feature for some people. This project strives for fast compilation, which improves productivity.
- **Batteries included:** the default modules provided contains useful facilities to use in your code and they will always be platform agnostic.
However currently Windows is the only supported OS but in the furutre as new OSes are introduced the platform specific code will be added to these facilities provided in the `modules` folder.
You can also opt out of the built-in modules by simply removing `#include <basic.sq>` and any other module imports `#include <...>` which will compile your code 100% as you wrote it with no added external code or metadata.
- **No external linkers allowed:** in this project external linkers and build tools are strictly forbidden. The compiler contains it's own linker and you can currently use it with `extern + @attributes`, e.g.
```C++
@link("kernel32.dll") extern BOOL __stdcall FreeConsole();
```
or use an `extern` block to duplicate this on multiple functions
```C++
@link("kernel32.dll")
extern {
    BOOL __stdcall FreeConsole();
    
    HMODULE __stdcall GetModuleHandleA(LPCSTR lpModuleName);
}
```
An experimental feature was added: `@link_dynamic(var)` which creates an array of structures containing function pointer and function name as global variables of the specified name `var`.
This was used in `modules/opengl_windows.sq` to link functions at runtime which allows you to use regular functions without having to store function pointers.
With a simple loop you can set the function pointers to these functions as seen below:
```C++
for (int i = 0; i < opengl_library.functions.count; i++) {
    Dynamic_Function* fn = &opengl_library.functions[i];
    cstring name = string_to_cstring(fn.name);
    fn.pointer = wglGetProcAddress(name);
}
```

### On the roadmap
- WASM (stated work on this already).
- Metaprogramming: primarily for enabling highly customized builds using the compiler as a library to trigger the compilation.
- TBD...

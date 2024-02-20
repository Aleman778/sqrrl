@echo off

rem MSVC Build
call vcvarsall.bat x64

IF NOT EXIST build mkdir build
pushd build

rem Compilation target file (windows is only support for MSVC)
set main_file=../code/platform_windows.cpp

rem Common Compiler Flags
set compiler_flags=-nologo -Gm- -GR- -Zo -EHa -Oi -FC -Zi -GS- -Gs9999999
set compiler_flags=-WX -W4 -wd4201 -wd4100 -wd4189 -wd4505 -wd4127 %compiler_flags%
set compiler_flags=-DCOMPILER_MSVC=1 -DCOMPILER_GCC=0 -DCOMPILER_LLVM=0 %compiler_flags%

rem Common Linker Flags
set linker_flags=Advapi32.lib -incremental:no -opt:ref -OUT:sqrrl.exe

rem Call the correct configuration
if ["%~1"]==["release"] (call :Release) else (call :Debug)

goto :EOF

:Debug
    echo Compiling sqrrl in debug mode
    set compiler_flags=-Od -DDEBUG=1 -DBUILD_DEBUG=1 -DBUILD_TEST=1 %compiler_flags%
    set compiler_flags=-DBUILD_INTERNAL=1 -DBUILD_MAX_DEBUG=1 %compiler_flags%
    set compiler_flags=-D_CRT_SECURE_NO_WARNINGS %compiler_flags%
    set linker_flags=dbghelp.lib %linker_flags%
    goto :Compile

:Release
    echo Compiling sqrrl in release mode
    set compiler_flags=-DBUILD_DEBUG=0 -DBUILD_TEST=1 %compiler_flags%
    set compiler_flags=-DBUILD_INTERNAL=0 -DBUILD_MAX_DEBUG=0 %compiler_flags%
    set compiler_flags=-D_CRT_SECURE_NO_WARNINGS %compiler_flags%
    set compiler_flags=-O2 -DNDEBUG=1 %compiler_flags%
    goto :Compile

:Compile
    cl %compiler_flags% %main_file% -link %linker_flags%
    popd
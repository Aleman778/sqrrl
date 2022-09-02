#!/bin/bash

project_dir="$PWD"
pushd build > /dev/null

# Common Compiler Options
options=-g

g++ $compiler_flags $project_dir/code/platform_windows.cpp -o sqrrl.exe

popd > /dev/null

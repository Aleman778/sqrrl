#!/bin/bash

code="$PWD"
opts=-g
pushd build > /dev/null
g++ $opts $code/code/main.cpp -o sqrrl.exe
popd > /dev/null

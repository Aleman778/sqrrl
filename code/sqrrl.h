#ifndef _SQRRL_H_
#define _SQRRL_H_

#include "stb_ds.h" // TODO(alexander): implement this on our own!
#include "sqrrl_basic.h"
#include "sqrrl_vars.h"
#include "sqrrl_tokenizer.h"
#include "sqrrl_value.h"
#include "sqrrl_ast.h"
#include "sqrrl_parser.h"

extern "C" int compiler_main_entry(int argc, char* argv[]);

#endif
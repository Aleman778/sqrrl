#ifndef _SQRRL_H_
#define _SQRRL_H_

// TODO(alexander): this will in the future be replaced with our own implementation
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h" // TODO(alexander): implement this on our own!

#include "sqrrl_basic.h"
#include "sqrrl_platform.h"
#include "sqrrl_vars.h"
#include "sqrrl_tokenizer.h"
#include "sqrrl_value.h"
#include "sqrrl_types.h"
#include "sqrrl_ast.h"
#include "sqrrl_parser.h"
#include "sqrrl_preprocessor.h"
#include "sqrrl_interp.h"

extern "C" int compiler_main_entry(int argc, char* argv[]);

#endif

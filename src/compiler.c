#include <stdio.h>

#include "compiler.h"
#include "parser.h"
#include "scanner.h"
#include "common.h"
#include "disassemble.h"

int compile(const char *source) {
  Expr *expr = MALLOC_OR_DIE(Expr, 1);

  if (parse(expr, source)) {
    return -1;
  }

#ifdef DEBUG_PRINT_PARSER
 
  disassembleExpr(expr);
  printf("\n");

#endif
 
  freeExpr(expr);

  return 0;
}

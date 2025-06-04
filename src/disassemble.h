#ifndef bminor_disassemble_h
#define bminor_disassemble_h

#include "scanner.h"
#include "parser.h"

void disassembleExpr(Expr *expr);
void disassembleToken(Token token);

#endif

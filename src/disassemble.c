#include <stdio.h>

#include "disassemble.h"
#include "scanner.h"
#include "parser.h"

static void disassembleLiteralExpr(LiteralExpr expr) {
  printf("%.*s", expr.token.length, expr.token.start);
}

static void disassembleUnaryExpr(UnaryExpr expr) {
  printf("(%.*s ", expr.token.length, expr.token.start);
  disassembleExpr(expr.expr);
  printf(")");
}

static void disassembleBinaryExpr(BinaryExpr expr) {
  printf("(%.*s ", expr.token.length, expr.token.start);
  disassembleExpr(expr.expr1);
  printf(" ");
  disassembleExpr(expr.expr2);
  printf(")");
}

static void disassembleIdxExpr(IdxExpr expr) {
  disassembleExpr(expr.arr);
  printf("[");
  disassembleExpr(expr.idx);
  printf("]");
}

static void disassembleCallExpr(CallExpr expr) {
  disassembleExpr(expr.func);
  printf("((");
  for (int i = 0; i < expr.paramCnt; i++) {
    disassembleExpr(expr.params + i);
    if (i != expr.paramCnt - 1) {
      printf(", ");
    }
  }
  printf("))");
}

void disassembleToken(Token token) {
  printf("Token(line: %d, type: %d, len: %d, lexeme: %.*s)\n", 
      token.line, token.type, token.length, token.length, token.start);
}

void disassembleExpr(Expr *expr) {
  if (expr == NULL) {
    printf("<nullExpr>");
  }

  switch (expr->type) {
    case EXPR_LITERAL: disassembleLiteralExpr(PTR_AS_LITERAL(expr)); break;
    case EXPR_UNARY: disassembleUnaryExpr(PTR_AS_UNARY(expr)); break;
    case EXPR_BINARY: disassembleBinaryExpr(PTR_AS_BINARY(expr)); break;
    case EXPR_IDX: disassembleIdxExpr(PTR_AS_IDX(expr)); break;
    case EXPR_CALL: disassembleCallExpr(PTR_AS_CALL(expr)); break;
  }
}


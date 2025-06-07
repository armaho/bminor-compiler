#include <stdio.h>

#include "compiler.h"
#include "parser.h"
#include "scanner.h"
#include "common.h"
#include "disassemble.h"

void compileIntDeclaration(IntDeclarationStmt stmt) {
  printf("int %.*s; ", stmt.ident.length, stmt.ident.start);
}

void compileAssignment(AssignmentStmt stmt) {
  printf("%.*s = ", stmt.ident.length, stmt.ident.start);
  disassembleExpr(stmt.expr);
  printf(";");
}

int compile(const char *source) {
  Program *program = MALLOC_OR_DIE(Program, 1);

  if (parse(program, source)) {
    printf("err\n");
    return -1;
  }

  for (int i = 0; i < program->len; i++) {
    Stmt s = program->stmts[i];

    switch (s.type) {
      case STMT_INT_DECLARATION: compileIntDeclaration(AS_INT_DECLARATION(s)); break;
      case STMT_ASSIGNMENT: compileAssignment(AS_ASSIGNMENT(s)); break;
    }
  }

  printf("\n");

  return 0;
}

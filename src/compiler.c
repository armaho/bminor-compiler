#include <stdio.h>

#include "compiler.h"
#include "parser.h"
#include "scanner.h"
#include "common.h"
#include "disassemble.h"

static void compileExpr(Expr *expr);
static void compileStmt(Stmt stmt);

static void compileLiteralExpr(LiteralExpr expr) {
  Token t = expr.token;

  switch (t.type) {
    case TOKEN_TRUE: printf("1"); break;
    case TOKEN_FALSE: printf("0"); break;
    default: printf("%.*s", expr.token.length, expr.token.start);
  }
}

static void compileUnaryExpr(UnaryExpr expr) {
  switch (expr.token.type) {
    case TOKEN_BANG: printf("!"); compileExpr(expr.expr); break;
    case TOKEN_MINUS: printf("-"); compileExpr(expr.expr); break;
    case TOKEN_PLUS_PLUS: compileExpr(expr.expr); printf("--"); break;
    case TOKEN_MINUS_MINUS: compileExpr(expr.expr); printf("++"); break;
    default: return; // unreachable
  }
}

static void compileBinaryExpr(BinaryExpr expr) {
  switch (expr.token.type) {
    case TOKEN_MINUS:
    case TOKEN_PLUS:
    case TOKEN_SLASH:
    case TOKEN_STAR:
    case TOKEN_MOD:
    case TOKEN_BANG_EQUAL:
    case TOKEN_EQUAL:
    case TOKEN_EQUAL_EQUAL:
    case TOKEN_GREATER:
    case TOKEN_GREATER_EQUAL:
    case TOKEN_LESS:
    case TOKEN_LESS_EQUAL:
    case TOKEN_AND:
    case TOKEN_OR:
      compileExpr(expr.expr1); 
      printf("%.*s", expr.token.length, expr.token.start);
      compileExpr(expr.expr2);
      break;
    case TOKEN_POW:
      printf("bminorPow(");
      compileExpr(expr.expr1);
      printf(",");
      compileExpr(expr.expr2);
      break;
    default: break; // unreachable
  }
}

static void compileIdxExpr(IdxExpr expr) {
  compileExpr(expr.arr);
  printf("[");
  compileExpr(expr.idx);
  printf("]");
}

static void compileCallExpr(CallExpr expr) {
  compileExpr(expr.func);
  printf("(");
  for (int i = 0; i < expr.paramCnt; i++) {
    compileExpr(expr.params + i);

    if (i != expr.paramCnt - 1) {
      printf(",");
    }
  }
  printf("]");
}

static void compileExpr(Expr *expr) {
  printf("(");

  switch (expr->type) {
    case EXPR_LITERAL: compileLiteralExpr(PTR_AS_LITERAL(expr)); break;
    case EXPR_UNARY: compileUnaryExpr(PTR_AS_UNARY(expr)); break;
    case EXPR_BINARY: compileBinaryExpr(PTR_AS_BINARY(expr)); break;
    case EXPR_IDX: compileIdxExpr(PTR_AS_IDX(expr)); break;
    case EXPR_CALL: compileCallExpr(PTR_AS_CALL(expr)); break;
  }

  printf(")");
}

static void compileExprStmt(ExprStmt stmt) {
  compileExpr(stmt.expr);
  printf(";");
}

static void compileIntDeclaration(IntDeclarationStmt stmt) {
  printf("int %.*s; ", stmt.ident.length, stmt.ident.start);
}

static void compileCharDeclaration(CharDeclarationStmt stmt) {
  printf("char %.*s; ", stmt.ident.length, stmt.ident.start);
}

static void compileStrDeclaration(StrDeclarationStmt stmt) {
  printf("char *%.*s; ", stmt.ident.length, stmt.ident.start);
}

static void compileAssignment(AssignmentStmt stmt) {
  printf("%.*s = ", stmt.ident.length, stmt.ident.start);
  compileExpr(stmt.expr);
  printf(";");
}

static void compileBlock(BlockStmt stmt) {
  printf("{");
  for (int i = 0; i < stmt.len; i++) {
    compileStmt(stmt.stmts[i]); 
  }
  printf("}");
}

static void compileIf(IfStmt stmt) {
  printf("if");
  compileExpr(stmt.condition);
  compileBlock(stmt.then);
  
  printf("else");

  if (stmt.hasElseIf) {
    printf(" ");
    compileIf(*stmt.elze.elzeif);
  } else {
    compileBlock(stmt.elze.elze);
  }
}

static void compileWhile(WhileStmt stmt) {
  printf("while");
  compileExpr(stmt.cond);
  compileBlock(stmt.block);
}

void compileStmt(Stmt stmt) {
  switch (stmt.type) {
    case STMT_EXPR: compileExprStmt(AS_EXPR(stmt)); break;
    case STMT_INT_DECLARATION: compileIntDeclaration(AS_INT_DECLARATION(stmt)); break;
    case STMT_CHAR_DECLARATION: compileCharDeclaration(AS_CHAR_DECLARATION(stmt)); break;
    case STMT_STR_DECLARATION: compileStrDeclaration(AS_STR_DECLARATION(stmt)); break;
    case STMT_ASSIGNMENT: compileAssignment(AS_ASSIGNMENT(stmt)); break;
    case STMT_BLOCK: compileBlock(AS_BLOCK(stmt)); break;
    case STMT_IF: compileIf(AS_IF(stmt)); break;
    case STMT_WHILE: compileWhile(AS_WHILE(stmt)); break;
  }
}

int compile(const char *source) {
  Program *program = MALLOC_OR_DIE(Program, 1);

  if (parse(program, source)) {
    return -1;
  }

  for (int i = 0; i < program->len; i++) {
    Stmt s = program->stmts[i];
    compileStmt(s);
  }

  printf("\n");

  return 0;
}

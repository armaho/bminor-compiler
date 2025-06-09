#ifndef bminor_parser_h
#define bminor_parser_h

#include "scanner.h"

#define MAX_FUNC_PARAM 10

typedef struct _expr Expr;

typedef enum {
  EXPR_UNARY,
  EXPR_BINARY,
  EXPR_LITERAL,
  EXPR_IDX,
  EXPR_CALL,
} ExprType;

typedef struct {
  Token token;
} LiteralExpr;

typedef struct {
  Token token;
  Expr *expr;
} UnaryExpr;

typedef struct {
  Token token;
  Expr *expr1;
  Expr *expr2;
} BinaryExpr;

typedef struct {
  Expr *arr;
  Expr *idx;
} IdxExpr;

typedef struct {
  int paramCnt;
  Expr *func;
  Expr *params;
} CallExpr;

struct _expr {
  ExprType type;
  union {
    UnaryExpr unaryExpr;
    BinaryExpr binaryExpr;
    LiteralExpr literalExpr;
    IdxExpr idxExpr;
    CallExpr callExpr;
  } as;
};

#define LITERAL_EXPR(token)                    \
  ((Expr){                                     \
    .type = EXPR_LITERAL,                      \
    .as = {                                    \
      .literalExpr = {                         \
        .token = (token)                       \
      }                                        \
    }                                          \
  })

#define UNARY_EXPR(token, exprPtr)             \
  ((Expr){                                     \
    .type = EXPR_UNARY,                        \
    .as = {                                    \
      .unaryExpr = {                           \
        .token = (token),                      \
        .expr = (exprPtr)                      \
      }                                        \
    }                                          \
  })

#define BINARY_EXPR(token, exprPtr1, exprPtr2) \
  ((Expr){                                     \
    .type = EXPR_BINARY,                       \
    .as = {                                    \
      .binaryExpr = {                          \
        .token = (token),                      \
        .expr1 = (exprPtr1),                   \
        .expr2 = (exprPtr2)                    \
      }                                        \
    }                                          \
  })

#define IDX_EXPR(arrPtr, idxPtr)               \
  ((Expr) {                                    \
    .type = EXPR_IDX,                          \
    .as = {                                    \
      .idxExpr = {                             \
        .arr = (arrPtr),                       \
        .idx = (idxPtr)                        \
      }                                        \
    }                                          \
  })

#define CALL_EXPR(argCnt, funcPtr, paramsPtr)  \
  ((Expr) {                                    \
  .type = EXPR_CALL,                           \
  .as = {                                      \
    .callExpr = {                              \
      .paramCnt = (argCnt),                    \
      .func = (funcPtr),                       \
      .params = (paramsPtr)                    \
      }                                        \
    }                                          \
  })

#define IS_LITERAL(expr) ((expr).type == EXPR_LITERAL)
#define IS_UNARY(expr) ((expr).type == EXPR_UNARY)
#define IS_BINARY(expr) ((expr).type == EXPR_BINARY)
#define IS_IDX(expr) ((expr).type == EXPR_IDX)
#define IS_CALL(expr) ((expr).type == EXPR_CALL)

#define AS_LITERAL(expr) ((expr).as.literalExpr)
#define AS_UNARY(expr) ((expr).as.unaryExpr)
#define AS_BINARY(expr) ((expr).as.binaryExpr)
#define AS_IDX(expr) ((expr).as.idxExpr)
#define AS_CALL(expr) ((expr).as.callExpr)

#define PTR_AS_LITERAL(expr) ((expr)->as.literalExpr)
#define PTR_AS_UNARY(expr) ((expr)->as.unaryExpr)
#define PTR_AS_BINARY(expr) ((expr)->as.binaryExpr)
#define PTR_AS_IDX(expr) ((expr)->as.idxExpr)
#define PTR_AS_CALL(expr) ((expr)->as.callExpr)

typedef struct _stmt Stmt;
typedef struct _if_stmt IfStmt;

typedef enum {
  STMT_EXPR,
  STMT_INT_DECLARATION,
  STMT_CHAR_DECLARATION,
  STMT_STR_DECLARATION,
  STMT_ASSIGNMENT,
  STMT_BLOCK,
  STMT_IF,
  STMT_WHILE,
  STMT_PRINT,
  STMT_FUNC,
  STMT_RETURN,
} StmtType;

typedef struct {
  Expr *expr;
} ExprStmt;

typedef struct {
  Token ident;
} IntDeclarationStmt;

typedef struct {
  Token ident;
} CharDeclarationStmt;

typedef struct {
  Token ident;
} StrDeclarationStmt;

typedef struct {
  Token ident;
  Token type;
} FuncArg;

typedef struct {
  Token ident;
  Expr *expr;
} AssignmentStmt;

typedef struct {
  int len;
  int cap;
  Stmt *stmts;
} BlockStmt;

struct _if_stmt {
  Expr *condition;
  BlockStmt then;
  int hasElseIf;
  union {
    struct _if_stmt *elzeif;
    BlockStmt elze;
  } elze;
};

typedef struct {
  Expr *cond;
  BlockStmt block;
} WhileStmt;

typedef struct {
  int cnt;
  Expr *args;
} PrintStmt;

typedef struct {
  Token ident;
  Token returnType;
  int cnt;
  FuncArg *args;
  BlockStmt body;
} FuncStmt;

typedef struct {
  Expr *expr;
} ReturnStmt;

struct _stmt {
  StmtType type;
  union {
    ExprStmt exprStmt;
    IntDeclarationStmt intDeclarationStmt;
    CharDeclarationStmt charDeclarationStmt;
    StrDeclarationStmt strDeclarationStmt;
    AssignmentStmt assignmentStmt;
    BlockStmt blockStmt;
    IfStmt ifStmt;
    WhileStmt whileStmt;
    PrintStmt printStmt;
    FuncStmt funcStmt;
    ReturnStmt returnStmt;
  } as;
};

typedef struct {
  int len;
  int cap;
  Stmt *stmts;
} Program;

#define INT_DECLARATION_STMT(identifier) \
  ((Stmt){ \
   .type = STMT_INT_DECLARATION, \
   .as = { \
     .intDeclarationStmt = { \
       .ident = (identifier), \
       }, \
     }, \
   })

#define ASSIGNMENT_STMT(identifier, exprPtr) \
  ((Stmt){ \
    .type = STMT_ASSIGNMENT, \
    .as = { \
      .assignmentStmt = { \
        .ident = (identifier), \
        .expr = (exprPtr), \
      } \
    } \
  })

#define EXPR_STMT(exprPtr) \
  ((Stmt){ \
    .type = STMT_EXPR, \
    .as = { \
      .exprStmt = { \
        .expr = (exprPtr) \
      } \
    } \
  })

#define CHAR_DECLARATION_STMT(identifier) \
  ((Stmt){ \
   .type = STMT_CHAR_DECLARATION, \
   .as = { \
     .charDeclarationStmt = { \
       .ident = (identifier), \
       }, \
     }, \
   })

#define STR_DECLARATION_STMT(identifier) \
  ((Stmt){ \
   .type = STMT_STR_DECLARATION, \
   .as = { \
     .strDeclarationStmt = { \
       .ident = (identifier), \
       }, \
     }, \
   })

#define BLOCK_STMT(block) \
  ((Stmt){ \
    .type = STMT_BLOCK, \
    .as = { \
      .blockStmt = (block), \
    }, \
   })

#define IF_STMT(ifStmtArg) \
  ((Stmt) { \
    .type = STMT_IF, \
    .as = { \
      .ifStmt = (ifStmtArg) \
    }, \
   })

#define WHILE_STMT(condPtr, blockStmt) \
  ((Stmt) { \
    .type = STMT_WHILE, \
    .as = { \
      .whileStmt = { \
        .cond = condPtr, \
        .block = blockStmt, \
      }, \
    }, \
  })

#define PRINT_STMT(argsCnt, argsPtr) \
  ((Stmt) { \
    .type = STMT_PRINT, \
    .as = { \
      .printStmt = { \
        .cnt = argsCnt, \
        .args = argsPtr, \
      }, \
    }, \
  })

#define FUNC_STMT(identToken, returnTypeToken, argsCnt, argsPtr, bodyStmt) \
  ((Stmt) { \
    .type = STMT_FUNC, \
    .as = { \
      .funcStmt = { \
        .ident = (identToken), \
        .returnType = (returnTypeToken), \
        .cnt = (argsCnt), \
        .args = (argsPtr), \
        .body = (bodyStmt), \
      }, \
    }, \
  })

#define RETURN_STMT(exprPtr) \
  ((Stmt){ \
    .type = STMT_RETURN, \
    .as = { \
      .returnStmt = { \
        .expr = (exprPtr) \
      } \
    } \
  })

#define AS_INT_DECLARATION(stmt) ((stmt).as.intDeclarationStmt)
#define AS_ASSIGNMENT(stmt) ((stmt).as.assignmentStmt)
#define AS_EXPR(stmt) ((stmt).as.exprStmt)
#define AS_CHAR_DECLARATION(stmt) ((stmt).as.charDeclarationStmt)
#define AS_STR_DECLARATION(stmt) ((stmt).as.strDeclarationStmt)
#define AS_BLOCK(stmt) ((stmt).as.blockStmt)
#define AS_IF(stmt) ((stmt).as.ifStmt)
#define AS_WHILE(stmt) ((stmt).as.whileStmt)
#define AS_PRINT(stmt) ((stmt).as.printStmt)
#define AS_FUNC(stmt) ((stmt).as.funcStmt)
#define AS_RETURN(stmt) ((stmt).as.returnStmt)

#define PTR_AS_INT_DECLARATION(stmt) ((stmt)->as.intDeclarationStmt)
#define PTR_AS_ASSIGNMENT(stmt) ((stmt)->as.assignmentStmt)
#define PTR_AS_EXPR(stmt) ((stmt)->as.exprStmt)
#define PTR_AS_CHAR(stmt) ((stmt)->as.charDeclarationStmt)
#define PTR_AS_STR_DECLARATION(stmt) ((stmt)->as.strDeclarationStmt)

int initProgram(Program *program);
int parse(Program *program, const char* source);

#endif

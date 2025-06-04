#ifndef bminor_parser_h
#define bminor_parser_h

#include "scanner.h"

typedef struct _expr Expr;

typedef enum {
  EXPR_UNARY,
  EXPR_BINARY,
  EXPR_LITERAL,
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

struct _expr {
  ExprType type;
  union {
    UnaryExpr unaryExpr;
    BinaryExpr binaryExpr;
    LiteralExpr literalExpr;
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

#define IS_LITERAL(expr) ((expr).type == EXPR_LITERAL)
#define IS_UNARY(expr) ((expr).type == EXPR_UNARY)
#define IS_BINARY(expr) ((expr).type == EXPR_BINARY)

#define AS_LITERAL(expr) ((expr).as.literalExpr)
#define AS_UNARY(expr) ((expr).as.unaryExpr)
#define AS_BINARY(expr) ((expr).as.binaryExpr)

#define PTR_AS_LITERAL(expr) ((expr)->as.literalExpr)
#define PTR_AS_UNARY(expr) ((expr)->as.unaryExpr)
#define PTR_AS_BINARY(expr) ((expr)->as.binaryExpr)

int parse(Expr *expr, const char* source);
void freeExpr(Expr *expr);

#endif

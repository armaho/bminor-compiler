#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "common.h"
#include "parser.h"
#include "scanner.h"
#include "disassemble.h"

typedef struct {
  Token current;
  Token previous;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * / %
  PREC_POWER,       // ^
  PREC_UNARY,       // ! -
  PREC_CALL,        // [] ()
  PREC_PRIMARY
} Precedence;

typedef int (*PrefixParseFn)(Expr *expr);
typedef int (*InfixParseFn)(Expr *expr, Expr *leftHandExpr);

typedef struct {
  PrefixParseFn prefix;
  InfixParseFn infix;
  Precedence precedence;
} ParseRule;

Parser parser;

static int expression(Expr *expr);
static ParseRule *getRule(TokenType type);
static int parsePrecedence(Expr *expr, Precedence precedence);

static void errorAt(Token *token, const char *message) {
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type != TOKEN_ERROR) {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
}

static void error(const char *message) {
  errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char *message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();
    
#ifdef DEBUG_PRINT_SCANNER  
    
    disassembleToken(parser.current);

#endif

    if (parser.current.type != TOKEN_ERROR) break;

    switch (parser.current.type) {
      case TOKEN_ERROR: errorAtCurrent(parser.current.start); break;
      case TOKEN_COMMENT: break;
      default: return;
    }
  }
}

static bool consume(TokenType type, const char *message) {
  if (parser.current.type == type) {
    advance();
    return true;
  }

  errorAtCurrent(message);
  return false;
}

static int numberHandler(Expr *expr) {
  Token token = parser.previous;
  *expr = LITERAL_EXPR(token);
  return 0;
}

static int groupingHandler(Expr *expr) {
  if (expression(expr)) {
    return -1;
  }
  if (!consume(TOKEN_RIGHT_PAREN, "Expected ')' at the end of expression.")) {
    return -1;
  }

  return 0;
}

static int unaryHandler(Expr *expr) {
  Token token = parser.previous; 

  Expr *rightHandExpr = MALLOC_OR_DIE(Expr, 1);
  if (parsePrecedence(rightHandExpr, PREC_UNARY)) {
    return -1;
  }

  *expr = UNARY_EXPR(token, rightHandExpr); 

  return 0;
}

static int binaryArithmeticHandler(Expr *expr, Expr *leftHandExpr) {
  Token token = parser.previous;
  ParseRule *rule = getRule(token.type);
  
  Expr *rightHandExpr = MALLOC_OR_DIE(Expr, 1);
  if (parsePrecedence(rightHandExpr, (Precedence)(rule->precedence + 1))) return -1;
  
  *expr = BINARY_EXPR(token, leftHandExpr, rightHandExpr);   

  return 0; 
}

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]     = {groupingHandler, NULL,                      PREC_NONE},
  [TOKEN_RIGHT_PAREN]    = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_LEFT_BRACE]     = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_RIGHT_BRACE]    = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_LEFT_BRACKET]   = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_RIGHT_BRACKET]  = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_COMMA]          = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_DOT]            = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_COLON]          = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_MINUS]          = {unaryHandler,    binaryArithmeticHandler,   PREC_TERM},
  [TOKEN_PLUS]           = {NULL,            binaryArithmeticHandler,   PREC_TERM},
  [TOKEN_SEMICOLON]      = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_SLASH]          = {NULL,            binaryArithmeticHandler,   PREC_FACTOR},
  [TOKEN_STAR]           = {NULL,            binaryArithmeticHandler,   PREC_FACTOR},
  [TOKEN_POW]            = {NULL,            binaryArithmeticHandler,   PREC_POWER},
  [TOKEN_MOD]            = {NULL,            binaryArithmeticHandler,   PREC_FACTOR},
  [TOKEN_BANG]           = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_BANG_EQUAL]     = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_EQUAL]          = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_EQUAL_EQUAL]    = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_GREATER]        = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_GREATER_EQUAL]  = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_LESS]           = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_LESS_EQUAL]     = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_PLUS_PLUS]      = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_MINUS_MINUS]    = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_AND]            = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_OR]             = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_IDENTIFIER]     = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_STRING_LITERAL] = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_CHAR_LITERAL]   = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_NUMBER]         = {numberHandler,   NULL,                      PREC_PRIMARY},
  [TOKEN_ARRAY]          = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_BOOLEAN]        = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_CHAR]           = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_INTEGER]        = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_STRING]         = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_VOID]           = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_IF]             = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_ELSE]           = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_WHILE]          = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_FOR]            = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_TRUE]           = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_FALSE]          = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_FUNCTION]       = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_RETURN]         = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_PRINT]          = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_EOF]            = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_ERROR]          = {NULL,            NULL,                      PREC_NONE},
  [TOKEN_COMMENT]        = {NULL,            NULL,                      PREC_NONE},
};

static ParseRule *getRule(TokenType type) {
  return &rules[type];
}

static int parsePrecedence(Expr *expr, Precedence precedence) {
  advance();

  PrefixParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expected expression.");
    return -1;
  }

  if (prefixRule(expr)) return -1;

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    InfixParseFn infixRule = getRule(parser.previous.type)->infix;

    Expr *newExpr = MALLOC_OR_DIE(Expr, 1);
    *newExpr = *expr;
    if (infixRule(expr, newExpr)) return -1;
  }

  return 0;
}

static int expression(Expr *expr) {
  return parsePrecedence(expr, PREC_ASSIGNMENT);
}

static void freeLiteralExpr(LiteralExpr expr) {
}

static void freeUnaryExpr(UnaryExpr expr) {
  freeExpr(expr.expr);
}

static void freeBinaryExpr(BinaryExpr expr) {
  freeExpr(expr.expr1);
  freeExpr(expr.expr2);
}

int parse(Expr *expr, const char* source) {
  initScanner(source);
  
  advance();

  if (expression(expr)) {
    return -1;
  }

  if (!consume(TOKEN_EOF, "Expected end of expression.")) {
    return -1;
  }

  return 0;
}

void freeExpr(Expr *expr) {
  switch(expr->type) {
    case EXPR_LITERAL: freeLiteralExpr(PTR_AS_LITERAL(expr)); break;
    case EXPR_UNARY: freeUnaryExpr(PTR_AS_UNARY(expr)); break;
    case EXPR_BINARY: freeBinaryExpr(PTR_AS_BINARY(expr)); break;
  }

  free(expr);
}



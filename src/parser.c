#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "common.h"
#include "parser.h"
#include "scanner.h"
#include "disassemble.h"

#define INIT_PROGRAM_SIZE 8

typedef struct {
  Token current;
  Token previous;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // ||
  PREC_AND,         // &&
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * / %
  PREC_POWER,       // ^
  PREC_UNARY,       // ! -
  PREC_INC_DCR,     // -- ++
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
static void freeInsideExpr(Expr *expr);
static void freeExpr(Expr *expr);

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

static bool match(TokenType type) {
  return parser.current.type == type;
}

static int literalHandler(Expr *expr) {
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

static int postfixHandler(Expr *expr, Expr *leftHandExpr) {
  Token token = parser.previous;

  *expr = UNARY_EXPR(token, leftHandExpr);

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

static int idxHandler(Expr *expr, Expr *leftHandExpr) {
  Expr *idxExpr = MALLOC_OR_DIE(Expr, 1);
  if (expression(idxExpr)) {
    return -1;
  } 

  if (!consume(TOKEN_RIGHT_BRACKET, "Expected ']' at the end of expression.")) {
    return -1;
  }

  *expr = IDX_EXPR(leftHandExpr, idxExpr);

  return 0;
}

static int callHandler(Expr *expr, Expr *leftHandExpr) {
  Expr *params = MALLOC_OR_DIE(Expr, MAX_FUNC_PARAM);
  int paramCnt = 0;

  while (!match(TOKEN_EOF) && !match(TOKEN_RIGHT_PAREN) && paramCnt <= MAX_FUNC_PARAM) {
    if (paramCnt != 0 && !consume(TOKEN_COMMA, "Expected ',' between arguments.")) return -1;
    
    if (paramCnt == MAX_FUNC_PARAM) {
      errorAtCurrent("Too many arguments for call.");
      return -1;
    }

    if (expression(params + paramCnt)) return -1;

    paramCnt++;
  }

  if (!consume(TOKEN_RIGHT_PAREN, "Expected ')' after function call")) return -1;

  *expr = CALL_EXPR(paramCnt, leftHandExpr, params);

  return 0;
}

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]     = {groupingHandler,   callHandler,               PREC_CALL},
  [TOKEN_RIGHT_PAREN]    = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_LEFT_BRACE]     = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_RIGHT_BRACE]    = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_LEFT_BRACKET]   = {NULL,              idxHandler,                PREC_CALL},
  [TOKEN_RIGHT_BRACKET]  = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_COMMA]          = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_DOT]            = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_COLON]          = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_MINUS]          = {unaryHandler,      binaryArithmeticHandler,   PREC_TERM},
  [TOKEN_PLUS]           = {NULL,              binaryArithmeticHandler,   PREC_TERM},
  [TOKEN_SEMICOLON]      = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_SLASH]          = {NULL,              binaryArithmeticHandler,   PREC_FACTOR},
  [TOKEN_STAR]           = {NULL,              binaryArithmeticHandler,   PREC_FACTOR},
  [TOKEN_POW]            = {NULL,              binaryArithmeticHandler,   PREC_POWER},
  [TOKEN_MOD]            = {NULL,              binaryArithmeticHandler,   PREC_FACTOR},
  [TOKEN_BANG]           = {unaryHandler,      NULL,                      PREC_NONE},
  [TOKEN_BANG_EQUAL]     = {NULL,              binaryArithmeticHandler,   PREC_EQUALITY},
  [TOKEN_EQUAL]          = {NULL,              binaryArithmeticHandler,   PREC_ASSIGNMENT},
  [TOKEN_EQUAL_EQUAL]    = {NULL,              binaryArithmeticHandler,   PREC_EQUALITY},
  [TOKEN_GREATER]        = {NULL,              binaryArithmeticHandler,   PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL]  = {NULL,              binaryArithmeticHandler,   PREC_COMPARISON},
  [TOKEN_LESS]           = {NULL,              binaryArithmeticHandler,   PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]     = {NULL,              binaryArithmeticHandler,   PREC_COMPARISON},
  [TOKEN_PLUS_PLUS]      = {NULL,              postfixHandler,            PREC_INC_DCR},
  [TOKEN_MINUS_MINUS]    = {NULL,              postfixHandler,            PREC_INC_DCR},
  [TOKEN_AND]            = {NULL,              binaryArithmeticHandler,   PREC_AND},
  [TOKEN_OR]             = {NULL,              binaryArithmeticHandler,   PREC_OR},
  [TOKEN_IDENTIFIER]     = {literalHandler,    NULL,                      PREC_NONE},
  [TOKEN_STRING_LITERAL] = {literalHandler,    NULL,                      PREC_NONE},
  [TOKEN_CHAR_LITERAL]   = {literalHandler,    NULL,                      PREC_NONE},
  [TOKEN_NUMBER]         = {literalHandler,    NULL,                      PREC_PRIMARY},
  [TOKEN_ARRAY]          = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_BOOLEAN]        = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_CHAR]           = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_INTEGER]        = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_STRING]         = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_VOID]           = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_IF]             = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_ELSE]           = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_WHILE]          = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_FOR]            = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_TRUE]           = {literalHandler,    NULL,                      PREC_PRIMARY},
  [TOKEN_FALSE]          = {literalHandler,    NULL,                      PREC_PRIMARY},
  [TOKEN_FUNCTION]       = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_RETURN]         = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_PRINT]          = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_EOF]            = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_ERROR]          = {NULL,              NULL,                      PREC_NONE},
  [TOKEN_COMMENT]        = {NULL,              NULL,                      PREC_NONE},
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

    if (infixRule == NULL) {
      error("Invalid infix operator");
      return -1;
    }

    Expr *newExpr = MALLOC_OR_DIE(Expr, 1);
    *newExpr = *expr;

    if (infixRule(expr, newExpr)) return -1;
  }

  return 0;
}

static int expression(Expr *expr) {
  if (parsePrecedence(expr, PREC_ASSIGNMENT)) return -1;

#ifdef DEBUG_PRINT_PARSER
 
  disassembleExpr(expr);
  printf("\n");

#endif

  return 0;
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

static void freeIdxExpr(IdxExpr expr) {
  freeExpr(expr.arr);
  freeExpr(expr.idx);
}

static void freeCallExpr(CallExpr expr) {
  freeExpr(expr.func);
  for (int i = 0; i < expr.paramCnt; i++) {
    freeInsideExpr(expr.params + i);
  }
  free(expr.params);
}

static void freeInsideExpr(Expr *expr) {
  switch(expr->type) {
    case EXPR_LITERAL: freeLiteralExpr(PTR_AS_LITERAL(expr)); break;
    case EXPR_UNARY: freeUnaryExpr(PTR_AS_UNARY(expr)); break;
    case EXPR_BINARY: freeBinaryExpr(PTR_AS_BINARY(expr)); break;
    case EXPR_IDX: freeIdxExpr(PTR_AS_IDX(expr)); break;
    case EXPR_CALL: freeCallExpr(PTR_AS_CALL(expr)); break;
  }
}

static void freeExpr(Expr *expr) {
  freeInsideExpr(expr);
  free(expr);
}

int initProgram(Program *program) {
  program->len = 0;
  program->cap = 8;
  program->stmts = (Stmt *)MALLOC_OR_DIE(Stmt, INIT_PROGRAM_SIZE);

  return 0;
}

static int addProgram(Program *program, Stmt s) {
  if (program->cap == program->len) {
    int newCap = 2 * program->cap;
    
    program->stmts = REALLOC_OR_DIE(Stmt, program->stmts, newCap);
    program->cap = newCap;
  }

  program->stmts[program->len++] = s;

  return 0;
}

static bool consumeSemicolon() {
  return consume(TOKEN_SEMICOLON, "Expected ';'");
}

static int addAssignmentStmt(Program *program, Token ident) {
  advance();

  Expr *expr = MALLOC_OR_DIE(Expr, 1);
  if (expression(expr)) return -1;

  addProgram(program, ASSIGNMENT_STMT(ident, expr));

  return 0;
}

static int addIntDeclarationStmt(Program *program, Token ident) {
  addProgram(program, INT_DECLARATION_STMT(ident));
 
  advance();

  if (match(TOKEN_EQUAL)) {
    addAssignmentStmt(program, ident);   
    consumeSemicolon();
  } else if (match(TOKEN_SEMICOLON)) {
    advance();
  } else {
    errorAtCurrent("Expected ';'");
    return -1;
  }

  return 0;
}

static int addDeclarationStmt(Program *program, Token ident) {
  advance();

  switch (parser.current.type) {
    case TOKEN_INTEGER: return addIntDeclarationStmt(program, ident);
    default: errorAtCurrent("Expected variable type."); return -1;
  }
}

static int addStmt(Program *program) {
  switch (parser.current.type) {
    case TOKEN_IDENTIFIER: {
      advance();

      switch (parser.current.type) {
        case TOKEN_COLON: return addDeclarationStmt(program, parser.previous);
        default: return expression(NULL); // todo: complete with exprStmt
      }
    }
    case TOKEN_EOF: advance(); return 0;
    default: return -1; // unreachable
  }
}

int parse(Program *program, const char *source) {
    initScanner(source);
    
    advance();

    while (parser.current.type != TOKEN_EOF) {
      if (addStmt(program)) return -1;
    }

    return 0;
}



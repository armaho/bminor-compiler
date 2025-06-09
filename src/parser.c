#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "common.h"
#include "parser.h"
#include "scanner.h"
#include "disassemble.h"

#define INIT_PROGRAM_SIZE 8
#define INIT_BLOCK_SIZE 8

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
static int addStmt(Program *program);
static int readBlockStmt(BlockStmt *block);

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

static Token peek() {
  Token t = peekToken();

  while (t.type == TOKEN_ERROR || t.type == TOKEN_COMMENT) {
    advance();
    t = peekToken();
  }

  return t;
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

static int addCharDeclarationStmt(Program *program, Token ident) {
  addProgram(program, CHAR_DECLARATION_STMT(ident));
 
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

static int addStrDeclarationStmt(Program *program, Token ident) {
  addProgram(program, STR_DECLARATION_STMT(ident));
 
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

static int addFuncStmt(Program *program, Token ident) {
  advance();

  Token returnType;
  if (TOKEN_BOOLEAN <= parser.current.type && parser.current.type <= TOKEN_VOID) {
    returnType = parser.current;
  } else {
    errorAtCurrent("Expected atomic type for return type of function.");
    return -1;
  }
  advance();

  if (!consume(TOKEN_LEFT_PAREN, "Expected '('")) return -1;

  FuncArg *args = MALLOC_OR_DIE(FuncArg, MAX_FUNC_PARAM);
  int argsCnt = 0;

  while (parser.current.type != TOKEN_RIGHT_PAREN && parser.current.type != TOKEN_EOF) {
    if (argsCnt == MAX_FUNC_PARAM) {
      errorAtCurrent("To many arguments.");
      return -1;
    }

    if (!match(TOKEN_IDENTIFIER)) {
      errorAtCurrent("Expected identifier.");
      return -1;
    }
    args[argsCnt].ident = parser.current;
    advance();

    if (!consume(TOKEN_COLON, "Expected ':'.")) return -1;
    
    if (parser.current.type < TOKEN_BOOLEAN || TOKEN_STRING < parser.current.type) {
      errorAtCurrent("Expected type.");
      return -1;
    }
    args[argsCnt].type = parser.current;
    advance();

    if (!match(TOKEN_RIGHT_PAREN) &&
        !consume(TOKEN_COMMA, "Expected ','.")) return -1;
    argsCnt++;
  }
  
  if (!consume(TOKEN_RIGHT_PAREN, "Expected ')'.")) return -1;
  if (!consume(TOKEN_LEFT_BRACE, "Expected '{'.")) return -1;

  BlockStmt block;
  if (readBlockStmt(&block)) return -1;

  addProgram(program, FUNC_STMT(ident, returnType, argsCnt, args, block)); 

  return 0;
}

static int addDeclarationStmt(Program *program, Token ident) {
  advance();
  
  switch (parser.current.type) {
    case TOKEN_INTEGER: return addIntDeclarationStmt(program, ident);
    case TOKEN_CHAR: return addCharDeclarationStmt(program, ident);
    case TOKEN_BOOLEAN: return addIntDeclarationStmt(program, ident);
    case TOKEN_STRING: return addStrDeclarationStmt(program, ident);
    case TOKEN_FUNCTION: return addFuncStmt(program, ident);
    default: errorAtCurrent("Expected variable type."); return -1;
  }
}

static int addExprStmt(Program *program) {
  Expr *expr = MALLOC_OR_DIE(Expr, 1);

  if (expression(expr)) return -1;
  if (!consumeSemicolon()) return -1;

  addProgram(program, EXPR_STMT(expr));
  return 0;
}

static int readBlockStmt(BlockStmt *block) {
  Program *blockProgram = (Program *)block;

  initProgram(blockProgram);

  while (parser.current.type != TOKEN_RIGHT_BRACE && parser.current.type != TOKEN_EOF) {
    if (addStmt(blockProgram)) return -1;
  }

  if (!consume(TOKEN_RIGHT_BRACE, "Expected '}' at the end of block")) return -1;

  return 0;
}

static int addBlockStmt(Program *program) {
  BlockStmt block;

  if (readBlockStmt(&block)) return -1;
  addProgram(program, BLOCK_STMT(block));

  return 0;
}

static int readIfStmt(IfStmt *stmt) {
  if (!consume(TOKEN_IF, "Expected 'if'")) return -1;
  if (!consume(TOKEN_LEFT_PAREN, "Expected '(' after if")) return -1;

  stmt->condition = MALLOC_OR_DIE(Expr, 1);
  if (expression(stmt->condition)) return -1;
  
  if (!consume(TOKEN_RIGHT_PAREN, "Expected ')' after if condition")) return -1;
  if (!consume(TOKEN_LEFT_BRACE, "Expected then block for if")) return -1;
  if (readBlockStmt(&stmt->then)) return -1;

  if (!match(TOKEN_ELSE)) {
    initProgram((Program *)&(stmt->elze.elze));
    stmt->hasElseIf = 0;
  } else {
    advance();

    if (match(TOKEN_IF)) {
      stmt->hasElseIf = 1;
      stmt->elze.elzeif = MALLOC_OR_DIE(IfStmt, 1);
      if (readIfStmt(stmt->elze.elzeif)) return -1;
    } else if (consume(TOKEN_LEFT_BRACE, "Expected '{' after else")) {
      stmt->hasElseIf = 0;
      if (readBlockStmt(&(stmt->elze.elze))) return -1;
    } else {
      return -1;
    }
  }

  return 0;
}

static int addIfStmt(Program *program) {
  IfStmt ifStmt;
  if (readIfStmt(&ifStmt)) return -1;

  addProgram(program, IF_STMT(ifStmt));
  return 0;
}

static int addWhileStmt(Program *program) {
  if (!consume(TOKEN_LEFT_PAREN, "Expected '(' before while condition.")) return -1;

  Expr *cond = MALLOC_OR_DIE(Expr, 1);
  if (expression(cond)) return -1;
  if (!consume(TOKEN_RIGHT_PAREN, "Expected ')' after while condition.")) return -1;

  if (!consume(TOKEN_LEFT_BRACE, "Expected '{' for while block")) return -1;
  BlockStmt block;
  if (readBlockStmt(&block)) return -1;

  addProgram(program, WHILE_STMT(cond, block));

  return 0;
}

static int addForStmt(Program *program) {
  if (!consume(TOKEN_LEFT_PAREN, "Expected '(' after for.")) return -1;

  BlockStmt block;
  Program *programBlock = (Program *)&block;
  initProgram(programBlock);

  if (addExprStmt(programBlock)) return -1;

  Expr *cond = MALLOC_OR_DIE(Expr, 1);
  if (expression(cond)) return -1;

  if (!consumeSemicolon()) return -1;

  Expr *update = MALLOC_OR_DIE(Expr, 1);
  if (expression(update)) return -1;

  if (!consume(TOKEN_RIGHT_PAREN, "Expected ')' after for")) return -1;
  if (!consume(TOKEN_LEFT_BRACE, "Expected '{' at the beginning of for block")) return -1;

  BlockStmt forBlock;
  if (readBlockStmt(&forBlock)) return -1;
  addProgram((Program *)&forBlock, EXPR_STMT(update));

  addProgram(programBlock, WHILE_STMT(cond, forBlock));

  addProgram(program, BLOCK_STMT(block));

  return 0;
}

static int addPrintStmt(Program *program) {
  Expr *args = MALLOC_OR_DIE(Expr, MAX_FUNC_PARAM);
  int argsCnt = 0; 

  while (parser.current.type != TOKEN_SEMICOLON && parser.current.type != TOKEN_EOF) {
    if (argsCnt == MAX_FUNC_PARAM) {
      errorAtCurrent("Too much arguments.");
      return -1;
    }

    if (expression(args + argsCnt)) return -1;
    argsCnt++;
    
    if (!match(TOKEN_SEMICOLON) && 
        !consume(TOKEN_COMMA, "Expected ',' or ';' after expression")) return -1;
  }

  if (!consumeSemicolon()) return -1;

  addProgram(program, PRINT_STMT(argsCnt, args));

  return 0;
}

static int addReturnStmt(Program *program) {
  Expr *expr = MALLOC_OR_DIE(Expr, 1);
  if (expression(expr)) return -1;
  if (!consumeSemicolon()) return -1;

  addProgram(program, RETURN_STMT(expr));

  return 0;
}

static int addStmt(Program *program) {
  switch (parser.current.type) {
    case TOKEN_IDENTIFIER: {
      Token t = peek();

      switch (t.type) {
        case TOKEN_COLON: advance(); return addDeclarationStmt(program, parser.previous);
        default: return addExprStmt(program);
      }
    }
    case TOKEN_EOF: advance(); return 0;
    case TOKEN_LEFT_BRACE: advance(); return addBlockStmt(program);
    case TOKEN_IF: return addIfStmt(program);
    case TOKEN_WHILE: advance(); return addWhileStmt(program);
    case TOKEN_FOR: advance(); return addForStmt(program);
    case TOKEN_PRINT: advance(); return addPrintStmt(program);
    case TOKEN_RETURN: advance(); return addReturnStmt(program);
    default: errorAtCurrent("Unexpected token"); return -1;
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


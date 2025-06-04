#ifndef bminor_scanner_h
#define bminor_scanner_h

typedef enum {
  // single character tokens
  TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
  TOKEN_LEFT_BRACKET, TOKEN_RIGHT_BRACKET,
  TOKEN_COMMA, TOKEN_DOT, TOKEN_COLON, TOKEN_MINUS, TOKEN_PLUS,
  TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR, TOKEN_POW, TOKEN_MOD,

  // one or two character tokens
  TOKEN_BANG, TOKEN_BANG_EQUAL,
  TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER, TOKEN_GREATER_EQUAL,
  TOKEN_LESS, TOKEN_LESS_EQUAL,
  TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS,
  TOKEN_AND, TOKEN_OR,

  // literals
  TOKEN_IDENTIFIER,
  TOKEN_STRING_LITERAL,
  TOKEN_CHAR_LITERAL,
  TOKEN_NUMBER,

  // kewwords
  TOKEN_ARRAY, TOKEN_BOOLEAN, TOKEN_CHAR, TOKEN_INTEGER, TOKEN_STRING, TOKEN_VOID,
  TOKEN_IF, TOKEN_ELSE,
  TOKEN_WHILE, TOKEN_FOR,
  TOKEN_TRUE, TOKEN_FALSE,
  TOKEN_FUNCTION, TOKEN_RETURN,
  TOKEN_PRINT,

  // others
  TOKEN_EOF, 
  TOKEN_ERROR,
  TOKEN_COMMENT,
} TokenType;

typedef struct {
  TokenType type;
  const char *start;
  int length;
  int line;
} Token;

typedef struct {
  const char *start;
  const char *current;
  int line;
} Scanner;

void initScanner(const char *source);
Token scanToken();

#endif

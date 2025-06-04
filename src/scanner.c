#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "scanner.h"

Scanner scn;

static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}

static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static Token makeToken(TokenType type) {
  Token t = {
    .type = type,
    .start = scn.start,
    .length = (int)(scn.current - scn.start),
    .line = scn.line,
  };

  return t;
}

static Token errorToken(char *message) {
  Token t = {
    .type = TOKEN_ERROR,
    .start = message,
    .length = strlen(message),
    .line = scn.line,
  };

  return t;
}

static bool isAtEnd() {
  return *scn.current == '\0';
}

static char advance() {
  return *scn.current++;
}

static char peek() {
  return *scn.current;
}

static char peekNext() {
  if (isAtEnd()) return '\0';
  return scn.current[1];
}

static bool match(char expected) {
  if (isAtEnd()) return false;
  if (*scn.current != expected) return false;
 
  scn.current++;
  return true;
}

static void skipWhitespace() {
  for (;;) {
    char c = peek();

    switch (c) {
      case '\n':
        scn.line++;
      case ' ':
      case '\r':
      case '\t':
        advance();
        break;
      default:
        return;
    }
  }
}

static Token readSingleLineComment() {
  while (peek() != '\n' && !isAtEnd()) advance();
  return makeToken(TOKEN_COMMENT);
}

static Token readMultilineComment() {
  while (!(peek() == '*' && peekNext() == '/') && !isAtEnd()) advance();

  if (isAtEnd()) return errorToken("Unterminated multiline comment");

  advance();
  advance();

  return makeToken(TOKEN_COMMENT);
}

static Token number() {
  while (isDigit(peek())) advance();
  return makeToken(TOKEN_NUMBER);
}

static TokenType checkKeyword(int start, int length, const char *rest, TokenType type) {
  if (scn.current - scn.start == start + length &&
      memcmp(scn.start + start, rest, length) == 0) {
    return type;
  }

  return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
  switch (scn.start[0]) {
    case 'a': return checkKeyword(1, 4, "rray", TOKEN_ARRAY);
    case 'b': return checkKeyword(1, 3, "ool", TOKEN_BOOLEAN);
    case 'c': return checkKeyword(1, 3, "har", TOKEN_CHAR);
    case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE);
    case 'f':
      switch (scn.start[1]) {
        case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
        case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
        case 'u': return checkKeyword(2, 2, "nc", TOKEN_FUNCTION);
      }
    case 'i':
      switch (scn.start[1]) {
        case 'f': return checkKeyword(2, 0, "", TOKEN_IF);
        case 'n': return checkKeyword(2, 1, "t", TOKEN_INTEGER);
      }
    case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT);
    case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
    case 's': return checkKeyword(1, 2, "tr", TOKEN_STRING);
    case 't': return checkKeyword(1, 3, "rue", TOKEN_TRUE);
    case 'v': return checkKeyword(1, 3, "oid", TOKEN_VOID);
    case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE);
  }

  return TOKEN_IDENTIFIER;
}

static Token identifier() {
  while (isAlpha(peek()) || isDigit(peek())) advance();
  return makeToken(identifierType());
}

static Token charLiteral() {
  advance();

  if (!match('\'')) {
    return errorToken("Expected closing single quote after char.");
  }

  return makeToken(TOKEN_CHAR_LITERAL);
}

static Token stringLiteral() {
  while (peek() != '"' && !isAtEnd()) {
    if (peek() == '\n') scn.line++;
    advance();
  }

  if (isAtEnd()) return errorToken("Unterminated string.");

  advance();
  return makeToken(TOKEN_STRING);
}

void initScanner(const char *source) {
  scn.start = source;
  scn.current = source;
  scn.line = 1;
}

Token scanToken() {
  skipWhitespace();
  scn.start = scn.current;

  if (isAtEnd()) return makeToken(TOKEN_EOF);

  char c = advance();
  if (isAlpha(c)) return identifier();
  if (isDigit(c)) return number();

  switch (c) {
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case '[': return makeToken(TOKEN_LEFT_BRACKET);
    case ']': return makeToken(TOKEN_RIGHT_BRACKET);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ':': return makeToken(TOKEN_COLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '*': return makeToken(TOKEN_STAR);
    case '^': return makeToken(TOKEN_POW);
    case '%': return makeToken(TOKEN_MOD);
    case '!': return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
    case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
    case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
    case '>': return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
    case '-': return makeToken(match('-') ? TOKEN_MINUS_MINUS : TOKEN_MINUS); 
    case '+': return makeToken(match('+') ? TOKEN_PLUS_PLUS : TOKEN_PLUS);
    case '/':
      if (match('/')) {
        return readSingleLineComment();
      } else if (match('*')) {
        return readMultilineComment();
      } else {
        return makeToken(TOKEN_SLASH);
      }
    case '&': 
      if (match('&')) {
        return makeToken(TOKEN_AND);
      } else {
        return errorToken("Expected '&'.");
      }
    case '|': 
      if (match('|')) {
        return makeToken(TOKEN_OR);
      } else {
        return errorToken("Expected '|'.");
      }
    case '"': 
      return stringLiteral();
    case '\'':
      return charLiteral();
  }

  return errorToken("Unexpected character.");
}


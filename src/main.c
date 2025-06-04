#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>

#include "parser.h"
#include "compiler.h"

static Expr *allocateExpr() {
  Expr *expr;
  assert((expr = (Expr *)malloc(sizeof(Expr))) != NULL);
  return expr;
}

static void repl() {
#define MAX_REPL_LINE 1024

  char line[MAX_REPL_LINE];
  for (;;) {
    printf("> ");

    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }

    compile(line);
  }

#undef MAX_REPL_LINE
}

static char *readFile(const char *path) {
  FILE *file = fopen(path, "rb");
  if (file == NULL) {
    fprintf(stderr, "Cannot open file \"%s\".\n", path);
    exit(EX_IOERR);
  }

  fseek(file, 0L, SEEK_END);
  size_t fileSize = ftell(file);
  rewind(file);

  char *buf = (char *)malloc(fileSize + 1);
  if (buf == NULL) {
    fprintf(stderr, "Allocating memory failed.\n");
    exit(EX_IOERR);
  }

  size_t bytesRead = fread(buf, sizeof(char), fileSize, file);
  if (bytesRead < fileSize) {
    fprintf(stderr, "cannot read file \"%s\".\n", path);
    exit(EX_IOERR);
  }
  
  buf[bytesRead] = '\0';

  fclose(file);
  return buf;
}

static void runFile(const char *path) {
  char *source = readFile(path);
  
  compile(source);

  free(source); 
}

int main(int argc, char **argv) {
  if (argc == 1) {
    repl();
  } else if (argc == 2) {
    runFile(argv[1]);
  } else {
    fprintf(stderr, "Usage: bminor [path]\n");
    exit(EX_USAGE);
  }

  return 0;
}


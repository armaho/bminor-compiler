#ifndef bminor_common_h
#define bminor_common_h

#include <stdlib.h>
#include <assert.h>

// #define DEBUG_PRINT_PARSER
// #define DEBUG_PRINT_SCANNER

#define MALLOC_OR_DIE(type, count)                             \
  ({                                                           \
    void* _tmp = malloc(sizeof(type) * (count));               \
    assert(_tmp != NULL);                                      \
    (type*)_tmp;                                               \
  })

#define REALLOC_OR_DIE(type, ptr, newCount)                    \
  ({                                                           \
    void* _tmp = realloc(ptr, sizeof(type) * (newCount));      \
    assert(_tmp != NULL);                                      \
    (type*)_tmp;                                               \
  })

#endif

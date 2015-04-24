#ifndef _TEST_UTIL_H
#define _TEST_UTIL_H

#include "util.h"
#include "tree.h"

#include <check.h>

#define parse_and_check(...) ({                                 \
         static const tree_kind_t array[] = { __VA_ARGS__ };    \
         _parse_and_check(array, ARRAY_LEN(array));             \
      })

typedef struct {
   int        line;
   const char *snippet;
} error_t;

void expect_errors(const error_t *lines);
TCase *nvc_unit_test(void);
int nvc_run_test(Suite *s);
tree_t run_elab(void);
tree_t _parse_and_check(const tree_kind_t *array, int num);

#endif  // _TEST_UTIL_H

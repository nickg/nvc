#ifndef _TEST_UTIL_H
#define _TEST_UTIL_H

#include "util.h"
#include "tree.h"

#include <check.h>

typedef struct {
   int        line;
   const char *snippet;
} error_t;

void expect_errors(const error_t *lines);
TCase *nvc_unit_test(void);
int nvc_run_test(Suite *s);
tree_t run_elab(void);

#endif  // _TEST_UTIL_H

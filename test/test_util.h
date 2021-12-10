#ifndef _TEST_UTIL_H
#define _TEST_UTIL_H

#include "util.h"
#include "tree.h"

#include <check.h>

#ifndef ck_assert_ptr_nonnull
#define ck_assert_ptr_nonnull(p) ck_assert_ptr_ne(p, NULL)
#endif

#ifndef ck_assert_double_eq_tol
#include <math.h>
#define ck_assert_double_eq_tol(x, y, t) ck_assert(fabs((x) - (y)) < (t))
#endif

#undef fail
#define fail(...) ck_abort_msg(__VA_ARGS__)

#define parse_and_check(...) ({                                    \
         static const tree_kind_t array[] = { __VA_ARGS__ };       \
         _parse_and_check(array, ARRAY_LEN(array), false, false);  \
      })

#define parse_check_and_simplify(...) ({                           \
         static const tree_kind_t array[] = { __VA_ARGS__ };       \
         _parse_and_check(array, ARRAY_LEN(array), true, false);   \
      })

#define parse_check_simplify_and_lower(...) ({                     \
         static const tree_kind_t array[] = { __VA_ARGS__ };       \
         _parse_and_check(array, ARRAY_LEN(array), true, true);    \
      })

typedef struct {
   int        line;
   const char *snippet;
} error_t;

void expect_errors(const error_t *lines);
void check_expected_errors(void);
void fail_if_errors(void);
TCase *nvc_unit_test(void);
int nvc_run_test(Suite *s);
tree_t run_elab(void);
tree_t _parse_and_check(const tree_kind_t *array, int num,
                        bool simp, bool lower);

#endif  // _TEST_UTIL_H

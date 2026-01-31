//
//  Copyright (C) 2014-2024  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef _TEST_UTIL_H
#define _TEST_UTIL_H

#include "util.h"
#include "prim.h"
#include "tree.h"

#include <check.h>

#ifndef ck_assert_ptr_nonnull
#define ck_assert_ptr_nonnull(p) ck_assert_ptr_ne(p, NULL)
#endif

#ifndef ck_assert_ptr_null
#define ck_assert_ptr_null(p) ck_assert_ptr_eq(p, NULL)
#endif

#ifndef ck_assert_double_eq_tol
#include <math.h>
#define ck_assert_double_eq_tol(x, y, t) ck_assert(fabs((x) - (y)) < (t))
#endif

#ifndef ck_assert_double_eq
#define ck_assert_double_eq(x, y) ck_assert((x) == (y))
#endif

#ifndef ck_assert_mem_eq
#define ck_assert_mem_eq(x, y, l) ck_assert(memcmp((x), (y), (l)) == 0)
#endif

#ifndef ck_assert_mem_ne
#define ck_assert_mem_ne(x, y, l) ck_assert(memcmp((x), (y), (l)) != 0)
#endif

#define ck_assert_ident_eq(x, y) \
   ck_assert_msg(icmp((x), (y)), "'%s' != '%s'", istr((x)), (y))

#define ck_assert_vlog_kind_eq(x, y) do {                               \
      vlog_kind_t __kind = vlog_kind((x));                              \
      ck_assert_msg(__kind == (y), "%s != %s",                          \
                    vlog_kind_str(__kind), vlog_kind_str((y)));         \
   } while (0)

#undef fail
#define fail(...) ck_abort_msg(__VA_ARGS__)

#define parse_and_check(...) ({                                    \
         static const tree_kind_t array[] = { __VA_ARGS__ };       \
         _parse_and_check(array, ARRAY_LEN(array), false);         \
      })

#define parse_check_and_simplify(...) ({                           \
         static const tree_kind_t array[] = { __VA_ARGS__ };       \
         _parse_and_check(array, ARRAY_LEN(array), true);          \
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
unit_registry_t *get_registry(void);
mir_context_t *get_mir(void);
jit_t *get_jit(void);
tree_t run_elab(void);
tree_t _parse_and_check(const tree_kind_t *array, int num, bool simp);
tree_t get_decl(tree_t container, const char *name) RETURNS_NONNULL;
tree_t get_nth_decl(tree_t container, const char *name, int nth);

#endif  // _TEST_UTIL_H

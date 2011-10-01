#include "parse.h"
#include "type.h"
#include "util.h"
#include "phase.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void setup(void)
{
   lib_set_work(lib_tmp());
}

static void teardown(void)
{
   lib_free(lib_work());
}

static bool folded_i(tree_t t, int64_t i)
{
   if (tree_kind(t) != T_LITERAL)
      return false;

   literal_t l = tree_literal(t);
   if (l.kind != L_INT)
      return false;

   return l.i == i;
}

static bool folded_b(tree_t t, bool b)
{
   if (tree_kind(t) != T_REF)
      return false;

   if (type_ident(tree_type(t)) != ident_new("STD.STANDARD.BOOLEAN"))
      return false;

   tree_t lit = tree_ref(t);
   if (tree_kind(lit) != T_ENUM_LIT)
      return false;

   return tree_pos(lit) == (b ? 1 : 0);
}

START_TEST(test_cfold)
{
   tree_t e, a, p, s;
   range_t r;

   fail_unless(input_from_file(TESTDIR "/simp/cfold.vhd"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);
   fail_unless(sem_errors() == 0);

   simplify(a);

   fail_unless(folded_i(tree_value(tree_decl(a, 0)), -10));

   r = type_dim(tree_type(tree_decl(a, 1)), 0);
   fail_unless(folded_i(r.left, -5));
   fail_unless(folded_i(r.right, 8));

   p = tree_stmt(a, 0);

   s = tree_stmt(p, 0);
   fail_unless(folded_i(tree_value(s), 2));
   s = tree_stmt(p, 1);
   fail_unless(folded_i(tree_value(s), 8));
   s = tree_stmt(p, 2);
   fail_unless(folded_i(tree_value(s), -5));
   s = tree_stmt(p, 3);
   fail_unless(folded_b(tree_value(s), true));
   s = tree_stmt(p, 4);
   fail_unless(folded_b(tree_value(s), false));
   s = tree_stmt(p, 5);
   fail_unless(folded_b(tree_value(s), true));
}
END_TEST

int main(void)
{
   register_trace_signal_handlers();

   setenv("NVC_LIBPATH", "../lib/std", 1);

   Suite *s = suite_create("simplify");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_cfold);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}


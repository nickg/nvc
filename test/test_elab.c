#include "test_util.h"
#include "type.h"
#include "util.h"
#include "phase.h"
#include "common.h"

#include <stdio.h>
#include <string.h>

START_TEST(test_elab1)
{
   input_from_file(TESTDIR "/elab/elab1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_elab2)
{
   input_from_file(TESTDIR "/elab/elab2.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_elab3)
{
   input_from_file(TESTDIR "/elab/elab3.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_elab4)
{
   input_from_file(TESTDIR "/elab/elab4.vhd");

   const error_t expect[] = {
      { 21, "actual width 9 does not match formal X width 8" },
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_open)
{
   tree_t top;

   input_from_file(TESTDIR "/elab/open.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   top = run_elab();

   // We used to delete all statements here but the behaviour
   // has changed
   fail_unless(tree_stmts(top) == 2);
}
END_TEST

START_TEST(test_genagg)
{
   input_from_file(TESTDIR "/elab/genagg.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_comp)
{
   input_from_file(TESTDIR "/elab/comp.vhd");

   const error_t expect[] = {
      { 55, "port Y not found in entity WORK.E2" },
      { 62, "type of port X in component declaration E3 is STD.STANDARD.BIT" },
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_issue17)
{
   input_from_file(TESTDIR "/elab/issue17.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_issue19)
{
   input_from_file(TESTDIR "/elab/issue19.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();

   tree_t tmp = NULL;
   const int ndecls = tree_decls(e);
   for (int i = 0; (i < ndecls) && (tmp == NULL); i++) {
      tree_t t = tree_decl(e, i);
      if (icmp(tree_ident(t), ":comp6:c1:tmp"))
         tmp = t;
   }

   fail_if(tmp == NULL);

   tree_t value = tree_value(tmp);
   fail_unless(tree_kind(value) == T_LITERAL);
   fail_unless(tree_ival(value) == 32);

   for (int i = 0; (i < ndecls) && (tmp == NULL); i++) {
      tree_t t = tree_decl(e, i);
      if (icmp(tree_ident(t), ":comp6:c1:tmp3"))
         tmp = t;
   }

   fail_if(tmp == NULL);

   value = tree_value(tmp);
   fail_unless(tree_kind(value) == T_LITERAL);
   fail_unless(tree_ival(value) == 32);
}
END_TEST

START_TEST(test_bounds10)
{
   input_from_file(TESTDIR "/elab/bounds10.vhd");

   const error_t expect[] = {
      { 10, "length of value 1 does not match length of target 101" },
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_copy1)
{
   input_from_file(TESTDIR "/elab/copy1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();

   int nfuncs = 0, nshared = 0;

   const int ndecls = tree_decls(e);
   for (int i = 0; i < ndecls; i++) {
      tree_t t = tree_decl(e, i);
      if (tree_kind(t) == T_FUNC_BODY)
         nfuncs++;
      else if (tree_kind(t) == T_VAR_DECL)
         nshared++;
   }

   fail_unless(nfuncs == 1);
   fail_unless(nshared == 2);
}
END_TEST

START_TEST(test_record)
{
   input_from_file(TESTDIR "/elab/record.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_ifgen)
{
   input_from_file(TESTDIR "/elab/ifgen.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_open2)
{
   input_from_file(TESTDIR "/elab/open2.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_issue93)
{
   input_from_file(TESTDIR "/elab/issue93.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t top = run_elab();
   tree_t c_order = tree_value(tree_decl(top, 2));
   fail_unless(tree_kind(c_order) == T_LITERAL);
   fail_unless(tree_subkind(c_order) == L_INT);
   fail_unless(tree_ival(c_order) == 4);
}
END_TEST

START_TEST(test_const1)
{
   input_from_file(TESTDIR "/elab/const1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t top = run_elab();

   tree_t ctr_r = tree_decl(top, tree_decls(top) - 1);
   fail_unless(tree_ident(ctr_r) == ident_new(":top:pwm_1:ctr_r"));

   range_t r = type_dim(tree_type(ctr_r), 0);
   fail_unless(tree_kind(r.left) == T_LITERAL);
   fail_unless(tree_ival(r.left) == 14);
}
END_TEST

START_TEST(test_libbind)
{
   input_from_file(TESTDIR "/elab/libbind.vhd");

   lib_t work = lib_work();

   lib_t other = lib_tmp("other");
   lib_set_work(other);
   parse_check_and_simplify(T_ENTITY, T_ARCH, -1);
   fail_if(sem_errors() > 0);

   lib_set_work(work);
   fail_if(run_elab() == NULL);
}
END_TEST

START_TEST(test_issue153)
{
   input_from_file(TESTDIR "/elab/issue153.vhd");

   const error_t expect[] = {
      {  9, "array S index -1 out of bounds 7 downto 0" },
      { 13, "array T index -1 out of bounds 7 downto 0" },
      { -1, NULL }
   };
   expect_errors(expect);

   fail_unless(run_elab() == NULL);
}
END_TEST

START_TEST(test_issue157)
{
   input_from_file(TESTDIR "/elab/issue157.vhd");

   fail_if(run_elab() == NULL);
}
END_TEST

START_TEST(test_issue159)
{
   input_from_file(TESTDIR "/elab/issue159.vhd");

   lib_t work = lib_work();

   lib_t other = lib_tmp("dummy");
   lib_set_work(other);
   parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH, -1);
   fail_if(sem_errors() > 0);

   lib_set_work(work);
   fail_if(run_elab() == NULL);
}
END_TEST

START_TEST(test_issue175)
{
   input_from_file(TESTDIR "/elab/issue175.vhd");

   lib_t lib2 = lib_tmp("lib2");
   lib_set_work(lib2);
   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY, -1);
   fail_if(sem_errors() > 0);

   lib_t lib = lib_tmp("lib");
   lib_set_work(lib);
   fail_if(run_elab() == NULL);
}
END_TEST

START_TEST(test_issue184)
{
   input_from_file(TESTDIR "/elab/issue184.vhd");

   tree_t top = run_elab();

   fail_unless(tree_stmts(top) == 1);
   fail_unless(icmp(tree_ident(tree_stmt(top, 0)), ":ent:gen_cfg2:good"));
}
END_TEST

START_TEST(test_libbind2)
{
   input_from_file(TESTDIR "/elab/libbind2.vhd");

   lib_t work = lib_work();

   lib_t other = lib_tmp("other");
   lib_set_work(other);
   parse_check_and_simplify(T_ENTITY, T_ARCH, -1);
   fail_if(sem_errors() > 0);

   lib_set_work(work);
   fail_if(run_elab() == NULL);
}
END_TEST

START_TEST(test_toplevel2)
{
   input_from_file(TESTDIR "/elab/toplevel2.vhd");

   elab_set_generic("I", "4");
   elab_set_generic("S", "hello");
   elab_set_generic("B", "'1'");
   elab_set_generic("V", "101");

   tree_t top = run_elab();
   fail_unless(tree_stmts(top) == 3);
}
END_TEST

START_TEST(test_libbind3)
{
   input_from_file(TESTDIR "/elab/libbind3.vhd");

   lib_set_work(lib_tmp("foo"));
   parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, -1);
   fail_if(sem_errors() > 0);

   lib_set_work(lib_tmp("bar"));
   run_elab();
}
END_TEST

START_TEST(test_issue251)
{
   input_from_file(TESTDIR "/elab/issue251.vhd");

   const error_t expect[] = {
      { 24, "array X index -1 out of bounds 3 downto 0" },
      { 30, "array A index -1 out of bounds 3 downto 0" },
      { -1, NULL }
   };
   expect_errors(expect);

   fail_unless(run_elab() == NULL);
}
END_TEST

START_TEST(test_jcore1)
{
   input_from_file(TESTDIR "/elab/jcore1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t s = tree_decl(top, 3);
   fail_unless(tree_kind(s) == T_SIGNAL_DECL);
   fail_unless(tree_ident(s) == ident_new(":jcore1:sub_i:x"));
   fail_if(tree_attr_int(s, partial_map_i, 0));
}
END_TEST

START_TEST(test_eval1)
{
   input_from_file(TESTDIR "/elab/eval1.vhd");

   const error_t expect[] = {
      { 12, "array index -1 outside bounds 7 downto 0" },
      { 16, "while evaluating call to FUNC" },
      { -1, NULL }
   };
   expect_errors(expect);

   fail_unless(run_elab() == NULL);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("elab");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_elab1);
   tcase_add_test(tc, test_elab2);
   tcase_add_test(tc, test_elab3);
   tcase_add_test(tc, test_elab4);
   tcase_add_test(tc, test_open);
   tcase_add_test(tc, test_genagg);
   tcase_add_test(tc, test_comp);
   tcase_add_test(tc, test_issue17);
   tcase_add_test(tc, test_issue19);
   tcase_add_test(tc, test_bounds10);
   tcase_add_test(tc, test_copy1);
   tcase_add_test(tc, test_record);
   tcase_add_test(tc, test_ifgen);
   tcase_add_test(tc, test_open2);
   tcase_add_test(tc, test_issue93);
   tcase_add_test(tc, test_const1);
   tcase_add_test(tc, test_libbind);
   tcase_add_test(tc, test_issue153);
   tcase_add_test(tc, test_issue157);
   tcase_add_test(tc, test_issue159);
   tcase_add_test(tc, test_issue175);
   tcase_add_test(tc, test_issue184);
   tcase_add_test(tc, test_libbind2);
   tcase_add_test(tc, test_toplevel2);
   tcase_add_test(tc, test_libbind3);
   tcase_add_test(tc, test_issue251);
   tcase_add_test(tc, test_jcore1);
   tcase_add_test(tc, test_eval1);
   suite_add_tcase(s, tc);

   return nvc_run_test(s);
}

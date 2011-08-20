#include "lib.h"
#include "tree.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static lib_t work;

static void setup(void)
{
   work = lib_new("work");
   fail_if(work == NULL);
}

static void teardown(void)
{
   if (work) {
      lib_destroy(work);
      lib_free(work);
      
      work = NULL;
   }
}

START_TEST(test_lib_new)
{
   fail_if(work == NULL);
}
END_TEST

START_TEST(test_lib_fopen)
{
   FILE *f = lib_fopen(work, "_test", "w");
   fprintf(f, "hello world");
   fclose(f);

   lib_free(work);

   work = lib_find("work");
   fail_if(work == NULL);

   f = lib_fopen(work, "_test", "r");
   char buf[12];
   fgets(buf, sizeof(buf), f);

   fail_unless(strcmp(buf, "hello world") == 0);

   fclose(f);
}
END_TEST

START_TEST(test_lib_save)
{
   tree_t ent = tree_new(T_ENTITY);
   tree_set_ident(ent, ident_new("name"));

   tree_t p1 = tree_new(T_PORT_DECL);
   tree_set_ident(p1, ident_new("foo"));
   tree_set_port_mode(p1, PORT_OUT);
   tree_set_type(p1, type_universal_int());
   tree_add_port(ent, p1);

   lib_put(work, ent);

   lib_save(work);
   lib_free(work);

   system("cp -r work /tmp");
   
   work = lib_find("work");
   fail_if(work == NULL);

   ent = lib_get(work, ident_new("name"));
   fail_if(ent == NULL);
   fail_unless(tree_ident(ent) == ident_new("name"));
   fail_unless(tree_ports(ent) == 1);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("lib");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_lib_new);
   tcase_add_test(tc_core, test_lib_fopen);
   tcase_add_test(tc_core, test_lib_save);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}


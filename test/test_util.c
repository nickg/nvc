//
//  Copyright (C) 2014-2025  Nick Gasson
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

#include "test_util.h"
#include "common.h"
#include "diag.h"
#include "diag.h"
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "mir/mir-unit.h"
#include "option.h"
#include "phase.h"
#include "rt/model.h"
#include "scan.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"

#include <assert.h>
#include <stdlib.h>
#include <time.h>

static const error_t   *error_lines = NULL;
static lib_t            test_lib    = NULL;
static unsigned         errors_seen = 0;
static unit_registry_t *registry    = NULL;
static mir_context_t   *mir_context = NULL;
static jit_t           *jit         = NULL;

static void test_error_fn(diag_t *d, void *context)
{
   fail_if(error_lines == NULL);

   const char *msg = diag_get_text(d);
   const loc_t *loc = diag_get_loc(d);

   bool unexpected = error_lines->line == -1
      || error_lines->snippet == NULL
      || error_lines->line != (loc ? loc->first_line : LINE_INVALID)
      || strstr(msg, error_lines->snippet) == NULL;

   if (unexpected) {
      diag_set_consumer(NULL, NULL);
      diag_emit(d);
      printf("expected line %d '%s'\n",
             error_lines->line, error_lines->snippet);
      ck_abort_msg("expected line %d '%s'",
                   error_lines->line, error_lines->snippet);
   }

   error_lines++;
   errors_seen++;

   for (int hint = 0; error_lines->line == 0; hint++, error_lines++) {
      if (error_lines->snippet == NULL)
         ck_assert_int_eq(hint, diag_hints(d));   // Test for absence of hint
      else {
         ck_assert_int_lt(hint, diag_hints(d));

         char *copy LOCAL = xstrdup(diag_get_hint(d, hint));
         if (strstr(copy, error_lines->snippet) == NULL) {
            diag_set_consumer(NULL, NULL);
            diag_emit(d);
            printf("expected hint '%s' got '%s'\n", error_lines->snippet, copy);
            ck_abort_msg("expected hint '%s'", error_lines->snippet);
         }
      }
   }
}

static void setup_per_test(void)
{
   test_lib = lib_tmp("work");
   lib_set_work(test_lib);

   reset_error_count();

   set_standard(STD_93);

   error_lines = NULL;
   errors_seen = 0;
   diag_set_consumer(NULL, NULL);
}

static void teardown_per_test(void)
{
   if (jit != NULL) {
      jit_free(jit);
      jit = NULL;
   }

   if (registry != NULL) {
      unit_registry_free(registry);
      registry = NULL;
   }

   if (mir_context != NULL) {
      mir_context_free(mir_context);
      mir_context = NULL;
   }

   lib_set_work(NULL);
   lib_free(test_lib);
   test_lib = NULL;
}

void expect_errors(const error_t *lines)
{
   diag_set_consumer(test_error_fn, NULL);

   error_lines = lines;
   errors_seen = 0;
}

void check_expected_errors(void)
{
   ck_assert_ptr_nonnull(error_lines);

   if (error_lines->snippet != NULL || error_lines->line != -1)
      ck_abort_msg("missing expected error line %d '%s'",
                   error_lines->line, error_lines->snippet);

   ck_assert_int_eq(errors_seen, diag_count(DIAG_NOTE));
}

TCase *nvc_unit_test(void)
{
   TCase *tc_core = tcase_create("Core");
   tcase_add_checked_fixture(tc_core, setup_per_test, teardown_per_test);
   return tc_core;
}

unit_registry_t *get_registry(void)
{
   if (registry == NULL)
      registry = unit_registry_new(get_mir());

   return registry;
}

mir_context_t *get_mir(void)
{
   if (mir_context == NULL)
      mir_context = mir_context_new();

   return mir_context;
}

jit_t *get_jit(void)
{
   if (jit == NULL)
      jit = jit_new(get_registry(), get_mir());

   return jit;
}

tree_t run_elab(void)
{
   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *j = get_jit();

   object_t *top = NULL;
   switch (source_kind()) {
   case SOURCE_VHDL:
      {
         tree_t t;
         while ((t = parse())) {
            fail_if(error_count() > 0);

            lib_put(lib_work(), t);
            simplify_local(t, j, ur, mc);
            bounds_check(t);
            fail_if(error_count() > 0);

            const tree_kind_t kind = tree_kind(t);
            if (kind == T_ENTITY || kind == T_CONFIGURATION)
               top = tree_to_object(t);
         }
      }
      break;

   case SOURCE_VERILOG:
      {
         vlog_node_t v;
         while ((v = vlog_parse())) {
            fail_if(error_count() > 0);

            lib_put_vlog(lib_work(), v);
            vlog_simp(v);
            fail_if(error_count() > 0);

            if (vlog_kind(v) == V_MODULE)
               top = vlog_to_object(v);
         }
      }
      break;

   default:
      ck_abort_msg("unsupported source kind");
   }

   rt_model_t *m = model_new(j, NULL);

   tree_t e = elab(top, j, ur, mc, NULL, NULL, m);

   model_free(m);

   return e;
}

tree_t _parse_and_check(const tree_kind_t *array, int num, bool simp)
{
   jit_t *jit = NULL;
   tree_t last = NULL;
   for (int i = 0; i < num; i++) {
      if (array[i] == (tree_kind_t)-1)
         goto out;

      last = parse();
      if (last == NULL) {
        ck_abort_msg("expected %s but have NULL", tree_kind_str(array[i]));
        continue;
      }

      const tree_kind_t kind = tree_kind(last);
      ck_assert_msg(tree_kind(last) == array[i],
                    "expected %s have %s", tree_kind_str(array[i]),
                    tree_kind_str(kind));

      lib_put(lib_work(), last);

      if (simp && error_count() == 0) {
         unit_registry_t *ur = get_registry();
         mir_context_t *mc = get_mir();
         if (jit == NULL)
            jit = jit_new(ur, mc);
         else
            unit_registry_purge(ur, tree_ident(last));

         simplify_local(last, jit, ur, mc);
         bounds_check(last);
      }
   }

   fail_unless(parse() == NULL);

 out:
   if (jit != NULL)
      jit_free(jit);

   return last;
}

void fail_if_errors(void)
{
   ck_assert_msg(diag_count(DIAG_WARN) == 0, "have errors");
   ck_assert_msg(errors_seen == 0, "have errors or diagnostics");
}

tree_t get_nth_decl(tree_t container, const char *name, int nth)
{
   const int ndecls = tree_decls(container);
   ident_t id = ident_new(name);

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      if (!tree_has_ident(d))
         continue;
      else if (tree_ident(d) == id && nth-- == 0)
         return d;
   }

   dump(container);
   ck_abort_msg("object %s not found in %s", name, istr(tree_ident(container)));
   should_not_reach_here();
}

tree_t get_decl(tree_t container, const char *name)
{
   return get_nth_decl(container, name, 0);
}

//
//  Copyright (C) 2025-2026  Nick Gasson
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
#include "cov/cov-api.h"
#include "cov/cov-priv.h"
#include "cov/cov-structs.h"
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "option.h"
#include "phase.h"
#include "rt/model.h"
#include "scan.h"
#include "tree.h"

#include <limits.h>

static cover_data_t *run_cover(tree_t top)
{
   cover_data_t *db = cover_data_init(COVER_MASK_ALL, 0, 1);
   mir_context_t *mc = mir_context_new();
   unit_registry_t *ur = unit_registry_new(mc);
   jit_t *j = jit_new(ur, mc, db);

   rt_model_t *m = model_new(j, db);

   elab(tree_to_object(top), j, ur, mc, db, NULL, m);

   model_reset(m);
   model_run(m, UINT64_MAX);

   model_free(m);
   jit_free(j);
   unit_registry_free(ur);
   mir_context_free(mc);

   // TODO: shouldn't need to do this to sync counters
   fbuf_t *tmp = fbuf_open("/dev/null", FBUF_OUT, FBUF_CS_NONE);
   cover_dump_items(db, tmp, COV_DUMP_RUNTIME);
   fbuf_close(tmp, NULL);

   return db;
}

START_TEST(test_perfile1)
{
   input_from_file(TESTDIR "/cover/perfile1.vhd");

   tree_t top = parse_check_and_simplify(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   cover_data_t *db = run_cover(top);

   cover_rpt_t *rpt = cover_report_new(db, INT_MAX);

   cover_scope_t *u1 = cover_get_scope(db, ident_new("WORK.TOP.U1"));
   ck_assert_ptr_nonnull(u1);

   const rpt_hier_t *u1_h = rpt_get_hier(rpt, u1);
   ck_assert_int_eq(u1_h->flat_stats.total[COV_ITEM_STMT], 3);
   ck_assert_int_eq(u1_h->flat_stats.hit[COV_ITEM_STMT], 2);

   const rpt_file_t *f = rpt_get_file(rpt, u1);
   ck_assert_ptr_nonnull(f);

   ck_assert_int_eq(f->stats.total[COV_ITEM_STMT], 3);
   ck_assert_int_eq(f->stats.hit[COV_ITEM_STMT], 3);

   cover_report_free(rpt);
   cover_data_free(db);

   fail_if_errors();
}
END_TEST

START_TEST(test_toggle1)
{
   input_from_file(TESTDIR "/cover/toggle1.vhd");

   elab_set_generic("G_VAL", "2");

   tree_t top = parse_check_and_simplify(T_ENTITY, T_ARCH);

   cover_data_t *db = run_cover(top);

   cover_scope_t *u1 = cover_get_scope(db, ident_new("WORK.TOGGLE1"));
   ck_assert_ptr_nonnull(u1);

   cover_scope_t *vect = u1->children.items[0];
   ck_assert_str_eq(istr(vect->name), "VECT");
   ck_assert_int_eq(vect->items.count, 32);

   cover_item_t *vect15 = vect->items.items[0];
   ck_assert_int_eq(vect15->consecutive, 2);
   ck_assert_ptr_eq(vect->items.items[1], vect15 + 1);
   ck_assert(vect15[0].flags & COV_FLAG_TOGGLE_TO_1);
   ck_assert(vect15[1].flags & COV_FLAG_TOGGLE_TO_0);
   ck_assert_int_eq(vect15[0].data, 0);
   ck_assert_int_eq(vect15[1].data, 0);

   cover_rpt_t *rpt = cover_report_new(db, INT_MAX);

   ck_assert_int_eq(vect15[0].data, 0);   // Should not change
   ck_assert_int_eq(vect15[1].data, 0);

   const rpt_hier_t *u1_h = rpt_get_hier(rpt, u1);
   ck_assert_int_eq(u1_h->flat_stats.total[COV_ITEM_TOGGLE], 32);
   ck_assert_int_eq(u1_h->flat_stats.hit[COV_ITEM_TOGGLE], 2);

   const table_array_t *hits = &(u1_h->detail.hits[COV_ITEM_TOGGLE]);
   ck_assert_int_eq(hits->count, 2);

   const rpt_table_t *hits_t0 = hits->items[0];
   ck_assert_int_eq(hits_t0->count, 1);
   ck_assert_int_eq(hits_t0->items[0]->data, 1);
   ck_assert_str_eq(istr(hits_t0->items[0]->hier),
                    "WORK.TOGGLE1.VECT(3).BIN_1_TO_0");

   const table_array_t *miss = &(u1_h->detail.miss[COV_ITEM_TOGGLE]);
   ck_assert_int_eq(miss->count, 16);

   const rpt_table_t *miss_t0 = miss->items[0];
   ck_assert_int_eq(miss_t0->count, 2);
   ck_assert_int_eq(miss_t0->items[0]->data, 0);
   ck_assert_str_eq(istr(miss_t0->items[0]->hier),
                    "WORK.TOGGLE1.VECT(15).BIN_0_TO_1");

   cover_report_free(rpt);
   cover_data_free(db);

   fail_if_errors();
}
END_TEST

START_TEST(test_merge1)
{
   input_from_file(TESTDIR "/cover/merge1.vhd");

   tree_t top = parse_check_and_simplify(T_ENTITY, T_ARCH);

   elab_set_generic("G_VAL", "0");

   cover_data_t *db1 = run_cover(top);

   elab_set_generic("G_VAL", "1");

   cover_data_t *db2 = run_cover(top);

   cover_merge(db1, db2, MERGE_UNION);

   // TODO: should be safe to call cover_data_free(db2) here

   cover_scope_t *u1 = cover_get_scope(db1, ident_new("WORK.MERGE1"));
   ck_assert_ptr_nonnull(u1);

   cover_rpt_t *rpt = cover_report_new(db1, INT_MAX);

   const rpt_hier_t *u1_h = rpt_get_hier(rpt, u1);
   ck_assert_int_eq(u1_h->flat_stats.total[COV_ITEM_TOGGLE], 8);
   ck_assert_int_eq(u1_h->flat_stats.hit[COV_ITEM_TOGGLE], 4);

   const table_array_t *hits = &(u1_h->detail.hits[COV_ITEM_TOGGLE]);
   ck_assert_int_eq(hits->count, 3);

   const rpt_table_t *hits_t2 = hits->items[2];
   ck_assert_int_eq(hits_t2->count, 2);
   ck_assert_int_eq(hits_t2->items[0]->data, 1);
   ck_assert_str_eq(istr(hits_t2->items[0]->hier),
                    "WORK.MERGE1.TGL(0).BIN_0_TO_1");

   const table_array_t *miss = &(u1_h->detail.miss[COV_ITEM_TOGGLE]);
   ck_assert_int_eq(hits->count, 3);

   const rpt_table_t *miss_t2 = miss->items[2];
   ck_assert_int_eq(miss_t2->count, 2);
   ck_assert_int_eq(miss_t2->items[0]->data, 0);
   ck_assert_str_eq(istr(miss_t2->items[0]->hier),
                    "WORK.MERGE1.TGL(1).BIN_0_TO_1");

   cover_scope_t *gen1 = cover_get_scope(db1, ident_new("WORK.MERGE1.GEN_ONE"));
   ck_assert_ptr_nonnull(gen1);

   cover_report_free(rpt);

   cover_data_free(db1);
   cover_data_free(db2);

   fail_if_errors();
}
END_TEST

Suite *get_cover_tests(void)
{
   Suite *s = suite_create("cover");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_perfile1);
   tcase_add_test(tc, test_toggle1);
   tcase_add_test(tc, test_merge1);
   suite_add_tcase(s, tc);

   return s;
}

//
//  Copyright (C) 2021  Nick Gasson
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
#include "enode.h"
#include "util.h"
#include "phase.h"
#include "ident.h"
#include "type.h"
#include "common.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static int check_eopt(e_node_t e)
{
   fail_if(e == NULL);

   static e_node_t root = NULL;
   int nerrors = 0;

   switch (e_kind(e)) {
   case E_ROOT:
      {
         root = e;

         const int nscopes = e_scopes(e);
         for (int i = 0; i < nscopes; i++)
            nerrors += check_eopt(e_scope(e, i));

         const int nnexus = e_nexuses(e);
         for (int i = 0; i < nnexus; i++)
            nerrors += check_eopt(e_nexus(e, i));

         if (nerrors > 0) {
            e_dump(e);
            ck_abort_msg("%s has %d errors", istr(e_ident(e)), nerrors);
         }
      }
      break;

   case E_SCOPE:
      {
         const int nsignals = e_signals(e);
         for (int i = 0; i < nsignals; i++)
            nerrors += check_eopt(e_signal(e, i));

         const int nscopes = e_scopes(e);
         for (int i = 0; i < nscopes; i++)
            nerrors += check_eopt(e_scope(e, i));

         const int nprocs = e_procs(e);
         for (int i = 0; i < nprocs; i++)
            nerrors += check_eopt(e_proc(e, i));
      }
      break;

   case E_SIGNAL:
      {
         int width = 0;

         const int nnexus = e_nexuses(e);
         for (int i = 0; i < nnexus; i++) {
            e_node_t n = e_nexus(e, i);
            width += e_width(n);

            bool found = false;
            const int nsignals = e_signals(n);
            for (int j = 0; !found && j < nsignals; j++)
               found = (e_signal(n, j) == e);

            if (!found) {
               printf("signal %s (%s) linked to nexus %s with no back link\n",
                      istr(e_ident(e)), istr(e_path(e)), istr(e_ident(n)));
               nerrors++;
            }

            bool found_in_root = false;
            const int nnexus = e_nexuses(root);
            for (int j = 0; !found_in_root && j < nnexus; j++) {
               if (e_nexus(root, j) == n)
                  found_in_root = true;
            }

            if (!found_in_root) {
               printf("floating nexus %s not attached to root\n",
                      istr(e_ident(n)));
               nerrors++;
            }
         }

         if (width != e_width(e)) {
            printf("signal %s (%s) width %u mismatch with total width of "
                   "nexuses %u\n", istr(e_ident(e)), istr(e_path(e)),
                   e_width(e), width);
            nerrors++;
         }
      }
      break;

   case E_NEXUS:
      if (e_width(e) == 0) {
         printf("zero width nexus %s\n", istr(e_ident(e)));
         nerrors++;
      }

      if (e_size(e) == 0) {
         printf("zero size nexus %s\n", istr(e_ident(e)));
         nerrors++;
      }

      if (e_pos(e) == NEXUS_POS_INVALID) {
         printf("nexus with invalid id %s\n", istr(e_ident(e)));
         nerrors++;
      }

      const int nsignals = e_signals(e);
      for (int i = 0; i < nsignals; i++) {
         e_node_t s = e_signal(e, i);
         for (int j = 0; j < i; j++) {
            if (e_signal(e, j) == s) {
               printf("signal %s appears multiple times in nexus %s\n",
                      istr(e_path(s)), istr(e_ident(e)));
               nerrors++;
            }
         }

         const int nnexus = e_nexuses(s);
         bool found_backlink = false;
         for (int j = 0; !found_backlink && j < nnexus; j++) {
            if (e_nexus(s, j) == e)
               found_backlink = true;
         }

         if (!found_backlink) {
            printf("nexus %s connected to signal %s with no backlink\n",
                   istr(e_ident(e)), istr(e_path(s)));
            nerrors++;
         }
      }

      const int nsources = e_sources(e);
      for (int i = 0; i < nsources; i++) {
         e_node_t p = e_source(e, i);
         for (int j = 0; j < i; j++) {
            if (e_source(e, j) == p) {
               printf("source %s appears multiple times in nexus %s\n",
                      istr(e_path(p)), istr(e_ident(e)));
               nerrors++;
            }
         }

         if (e_kind(p) == E_PORT) {
            if (e_nexus(p, 1) != e) {
               printf("port %s not linked to nexus %s\n",
                      istr(e_ident(p)), istr(e_ident(e)));
               nerrors++;
            }

            e_node_t src = e_nexus(p, 0);
            const int noutputs = e_outputs(src);
            bool found_link = false;
            for (int j = 0; j < noutputs && !found_link; j++) {
               if (e_output(src, j) == p)
                  found_link = true;
            }

            if (!found_link) {
               printf("port %s not linked to output of nexus %s\n",
                      istr(e_ident(p)), istr(e_ident(src)));
               nerrors++;
            }
         }
      }

      break;

   default:
      break;
   }

   return nerrors;
}

static e_node_t run_eopt(void)
{
   tree_t top = run_elab();
   fail_if(error_count() > 0);
   lower_unit(top, NULL);

   e_node_t e = eopt_build(top);

   if (error_count() == 0)
      check_eopt(e);

   return e;
}

START_TEST(test_ram1)
{
   input_from_file(TESTDIR "/eopt/ram1.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_scopes(e) == 2);

   e_node_t s0 = e_scope(e, 0);
   fail_unless(e_path(s0) == ident_new(":std:standard"));
   fail_unless(e_signals(s0) == 0);

   e_node_t s1 = e_scope(e, 1);
   fail_unless(e_path(s1) == ident_new(":ram1"));
   fail_unless(e_instance(s1) == ident_new(":ram1(test)"));
   fail_unless(e_signals(s1) == 5);
   fail_unless(e_procs(s1) == 2);

   e_node_t ram = e_signal(s1, 0);
   fail_unless(e_ident(ram) == ident_new("RAM"));
   fail_unless(e_nexuses(ram) == 16);
   fail_unless(e_flags(ram) & E_F_CONTIGUOUS);
   fail_if(e_flags(ram) & E_F_LAST_VALUE);

   for (int i = 0; i < 16; i++) {
      e_node_t n = e_nexus(ram, i);
      fail_unless(e_width(n) == 8);
      fail_unless(e_size(n) == 1);
   }

   e_node_t wr_p = e_proc(s1, 1);
   fail_unless(e_ident(wr_p) == ident_new("WR_P"));
   fail_unless(e_nexuses(wr_p) == 16);
   fail_unless(e_nexus(wr_p, 0) == e_nexus(ram, 0));

   e_node_t dout = e_signal(s1, 2);
   fail_unless(e_ident(dout) == ident_new("DOUT"));
   fail_unless(e_width(dout) == 8);
   fail_unless(e_nexuses(dout) == 1);
   fail_unless(icmp(type_ident(e_type(dout)), "STD.STANDARD.BIT_VECTOR"));
}
END_TEST

START_TEST(test_arrayref1)
{
   input_from_file(TESTDIR "/eopt/arrayref1.vhd");

   e_node_t e = run_eopt();

   // Would have been 15 with old grouping code
   ck_assert_int_eq(e_nexuses(e), 16);
   fail_unless(e_scopes(e) == 2);

   e_node_t s1 = e_scope(e, 1);

   e_node_t x = e_signal(s1, 0);
   fail_unless(e_ident(x) == ident_new("X"));
   fail_unless(e_width(x) == 3);
   fail_unless(e_nexuses(x) == 3);

   e_node_t y = e_signal(s1, 1);
   fail_unless(e_ident(y) == ident_new("Y"));
   fail_unless(e_width(y) == 2);
   fail_unless(e_nexuses(y) == 2);

   e_node_t i = e_signal(s1, 2);
   fail_unless(e_ident(i) == ident_new("I"));
   fail_unless(e_width(i) == 1);
   fail_unless(e_nexuses(i) == 1);

   e_node_t p = e_signal(s1, 3);
   fail_unless(e_ident(p) == ident_new("P"));
   fail_unless(e_width(p) == 4);
   fail_unless(e_nexuses(p) == 4);   // XXX: could be 3

   e_node_t q = e_signal(s1, 4);
   fail_unless(e_ident(q) == ident_new("Q"));
   fail_unless(e_width(q) == 6);
   fail_unless(e_nexuses(q) == 3);
   fail_unless(e_width(e_nexus(q, 0)) == 2);

   e_node_t r = e_signal(s1, 5);
   fail_unless(e_ident(r) == ident_new("R"));
   fail_unless(e_width(r) == 6);
   fail_unless(e_nexuses(r) == 3);
   fail_unless(e_width(e_nexus(r, 0)) == 4);
   fail_unless(e_width(e_nexus(r, 2)) == 1);
}
END_TEST

START_TEST(test_arrayref2)
{
   input_from_file(TESTDIR "/eopt/arrayref2.vhd");

   e_node_t e = run_eopt();

   ck_assert_int_eq(e_nexuses(e), 10);
   fail_unless(e_scopes(e) == 2);

   e_node_t s1 = e_scope(e, 1);

   e_node_t x = e_signal(s1, 0);
   fail_unless(e_ident(x) == ident_new("X"));
   fail_unless(e_width(x) == 9);
   fail_unless(e_nexuses(x) == 5);
   fail_unless(e_width(e_nexus(x, 0)) == 4);
   fail_unless(e_width(e_nexus(x, 1)) == 1);
   fail_unless(e_width(e_nexus(x, 2)) == 2);

   e_node_t y = e_signal(s1, 1);
   fail_unless(e_ident(y) == ident_new("Y"));
   fail_unless(e_width(y) == 18);
   fail_unless(e_nexuses(y) == 5);
   fail_unless(e_width(e_nexus(y, 0)) == 8);
   fail_unless(e_width(e_nexus(y, 1)) == 2);
   fail_unless(e_width(e_nexus(y, 2)) == 4);
}
END_TEST

START_TEST(test_arrayref3)
{
   input_from_file(TESTDIR "/eopt/arrayref3.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 9);
   fail_unless(e_scopes(e) == 2);

   e_node_t ram = e_signal(e_scope(e, 1), 0);
   fail_unless(e_ident(ram) == ident_new("RAM"));
   fail_unless(e_width(ram) == 16);
   fail_unless(e_nexuses(ram) == 8);
   fail_unless(e_width(e_nexus(ram, 0)) == 2);
}
END_TEST

START_TEST(test_recref1)
{
   input_from_file(TESTDIR "/eopt/recref1.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 6);
   fail_unless(e_scopes(e) == 2);

   e_node_t s = e_signal(e_scope(e, 1), 0);
   fail_unless(e_ident(s) == ident_new("S"));
   fail_unless(e_width(s) == 4);
   fail_unless(e_nexuses(s) == 2);
   fail_unless(e_width(e_nexus(s, 0)) == 3);
   fail_unless(e_width(e_nexus(s, 1)) == 1);
}
END_TEST

START_TEST(test_recref2)
{
   input_from_file(TESTDIR "/eopt/recref2.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 6);
   fail_unless(e_scopes(e) == 2);

   e_node_t r = e_signal(e_scope(e, 1), 0);
   fail_unless(e_ident(r) == ident_new("R"));
   fail_unless(e_width(r) == 8);
   fail_unless(e_nexuses(r) == 4);
   fail_unless(e_width(e_nexus(r, 0)) == 2);

   e_node_t s = e_signal(e_scope(e, 1), 1);
   fail_unless(e_ident(s) == ident_new("S"));
   fail_unless(e_width(s) == 2);
   fail_unless(e_nexuses(s) == 2);
   fail_unless(e_width(e_nexus(s, 0)) == 1);
}
END_TEST

START_TEST(test_slice1)
{
   input_from_file(TESTDIR "/eopt/slice1.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 8);
   fail_unless(e_scopes(e) == 2);

   e_node_t x = e_signal(e_scope(e, 1), 0);
   fail_unless(e_ident(x) == ident_new("X"));
   fail_unless(e_width(x) == 8);
   fail_unless(e_nexuses(x) == 4);
   fail_unless(e_width(e_nexus(x, 0)) == 4);
   fail_unless(e_width(e_nexus(x, 1)) == 1);
   fail_unless(e_width(e_nexus(x, 3)) == 2);

   e_node_t y = e_signal(e_scope(e, 1), 1);
   fail_unless(e_ident(y) == ident_new("Y"));
   fail_unless(e_width(y) == 8);
   fail_unless(e_nexuses(y) == 4);
   fail_unless(e_width(e_nexus(y, 0)) == 2);
   fail_unless(e_width(e_nexus(y, 3)) == 4);
}
END_TEST

START_TEST(test_array3)
{
   input_from_file(TESTDIR "/eopt/array3.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 4);
   fail_unless(e_scopes(e) == 2);

   e_node_t m = e_signal(e_scope(e, 1), 0);
   fail_unless(e_ident(m) == ident_new("M"));
   fail_unless(e_width(m) == 8);
   fail_unless(e_nexuses(m) == 4);
   fail_unless(e_width(e_nexus(m, 0)) == 5);
   fail_unless(e_width(e_nexus(m, 1)) == 1);
}
END_TEST

START_TEST(test_jcore4)
{
   input_from_file(TESTDIR "/eopt/jcore4.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_scopes(e) == 2);

   e_node_t a = e_signal(e_scope(e, 1), 0);
   fail_unless(e_kind(a) == E_SIGNAL);
   fail_unless(e_ident(a) == ident_new("A"));
   fail_unless(e_width(a) == 9);
   fail_unless(e_nexuses(a) == 3);

   e_node_t n0 = e_nexus(a, 0);
   fail_unless(e_ident(n0) == ident_new(":jcore4:a[0:2]"));
   fail_unless(e_width(n0) == 3);
   fail_unless(e_size(n0) == 1);
}
END_TEST

START_TEST(test_jcore2)
{
   input_from_file(TESTDIR "/eopt/jcore2.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_scopes(e) == 2);

   e_node_t s = e_signal(e_scope(e, 1), 0);
   fail_unless(e_kind(s) == E_SIGNAL);
   fail_unless(e_ident(s) == ident_new("THIS_C"));
   fail_unless(e_nexuses(s) == 7);

   e_node_t n0 = e_nexus(s, 0);
   fail_unless(e_width(n0) == 32);
   fail_unless(e_size(n0) == 1);

   e_node_t n1 = e_nexus(s, 1);
   fail_unless(e_width(n1) == 1);
   fail_unless(e_size(n1) == 1);
}
END_TEST

START_TEST(test_issue72)
{
   input_from_file(TESTDIR "/eopt/issue72.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 4);
   fail_unless(e_scopes(e) == 3);

   fail_unless(e_path(e_scope(e, 1)) == ident_new(":work:pack1"));

   e_node_t s1 = e_signal(e_scope(e, 2), 0);
   fail_unless(e_ident(s1) == ident_new("S1"));
   fail_unless(e_width(s1) == 4);
   fail_unless(e_nexuses(s1) == 2);
   fail_unless(e_width(e_nexus(s1, 0)) == 2);
}
END_TEST

START_TEST(test_issue73)
{
   input_from_file(TESTDIR "/eopt/issue73.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 8);
   fail_unless(e_scopes(e) == 2);

   e_node_t x = e_signal(e_scope(e, 1), 0);
   fail_unless(e_ident(x) == ident_new("X"));
   fail_unless(e_width(x) == 8);
   fail_unless(e_nexuses(x) == 4);
   fail_unless(e_width(e_nexus(x, 0)) == 3);
   fail_unless(e_width(e_nexus(x, 1)) == 1);

   e_node_t y = e_signal(e_scope(e, 1), 1);
   fail_unless(e_ident(y) == ident_new("Y"));
   fail_unless(e_width(y) == 8);
   fail_unless(e_nexuses(y) == 4);
   fail_unless(e_width(e_nexus(y, 0)) == 2);
   fail_unless(e_width(e_nexus(y, 1)) == 2);
}
END_TEST

START_TEST(test_issue95)
{
   input_from_file(TESTDIR "/eopt/issue95.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 6);
   fail_unless(e_scopes(e) == 2);

   e_node_t points = e_signal(e_scope(e, 1), 0);
   fail_unless(e_ident(points) == ident_new("POINTS"));
   fail_unless(e_width(points) == 6);
   fail_unless(e_nexuses(points) == 6);
   fail_unless(e_width(e_nexus(points, 0)) == 1);
   fail_unless(e_size(e_nexus(points, 0)) == 4);
   fail_unless(e_width(e_nexus(points, 1)) == 1);
   fail_unless(e_size(e_nexus(points, 1)) == 1);
}
END_TEST

START_TEST(test_issue250)
{
   input_from_file(TESTDIR "/eopt/issue250.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 10);
   fail_unless(e_scopes(e) == 2);

   e_node_t cellio = e_signal(e_scope(e, 1), 0);
   fail_unless(e_ident(cellio) == ident_new("CELLIO"));
   fail_unless(e_width(cellio) == 12);
   fail_unless(e_nexuses(cellio) == 8);
   fail_unless(e_width(e_nexus(cellio, 0)) == 2);
   fail_unless(e_width(e_nexus(cellio, 7)) == 1);
}
END_TEST

START_TEST(test_issue371)
{
   input_from_file(TESTDIR "/eopt/issue371.vhd");

   e_node_t e = run_eopt();

   // TODO: this could be a lot better if we didn't split all signal
   // arguments to procedure calls
   fail_unless(e_nexuses(e) == 14);
   fail_unless(e_scopes(e) == 2);

   e_node_t addr = e_signal(e_scope(e, 1), 2);
   fail_unless(e_ident(addr) == ident_new("ADDRESS_SIG"));
   fail_unless(e_nexuses(addr) == 3);
   fail_unless(e_width(e_nexus(addr, 0)) == 1);
}
END_TEST

START_TEST(test_partial1)
{
   input_from_file(TESTDIR "/eopt/partial1.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 4);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":partial1(test)"));

   e_node_t uut = e_scope(top, 0);
   fail_unless(e_instance(uut) == ident_new(":partial1(test):uut@sub(test)"));
   fail_unless(e_path(uut) == ident_new(":partial1:uut"));

   e_node_t uut_in1 = e_signal(uut, 1);
   fail_unless(e_ident(uut_in1) == ident_new("IN1"));
   fail_unless(e_path(uut_in1) == ident_new(":partial1:uut:in1"));
   fail_unless(e_nexuses(uut_in1) == 2);
   fail_unless(e_width(e_nexus(uut_in1, 0)) == 2);
   fail_unless(e_width(e_nexus(uut_in1, 1)) == 2);
   fail_unless(e_sources(e_nexus(uut_in1, 0)) == 1);
   fail_unless(e_sources(e_nexus(uut_in1, 1)) == 0);
   fail_unless(e_signals(e_nexus(uut_in1, 0)) == 2);
   fail_unless(e_signals(e_nexus(uut_in1, 1)) == 1);

   e_node_t top_in1 = e_signal(top, 1);
   fail_unless(e_ident(top_in1) == ident_new("IN1"));
   fail_unless(e_path(top_in1) == ident_new(":partial1:in1"));
   fail_unless(e_nexuses(top_in1) == 1);
   fail_unless(e_nexus(top_in1, 0) == e_nexus(uut_in1, 0));
}
END_TEST

START_TEST(test_nonconst1)
{
   input_from_file(TESTDIR "/eopt/nonconst1.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 18);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":nonconst1(test)"));

   e_node_t s = e_signal(top, 0);
   fail_unless(e_ident(s) == ident_new("S"));
   fail_unless(e_path(s) == ident_new(":nonconst1:s"));
   fail_unless(e_nexuses(s) == 9);
   fail_unless(e_width(e_nexus(s, 0)) == 1);

   e_node_t t = e_signal(top, 1);
   fail_unless(e_ident(t) == ident_new("T"));
   fail_unless(e_path(t) == ident_new(":nonconst1:t"));
   fail_unless(e_nexuses(t) == 9);
   fail_unless(e_width(e_nexus(t, 0)) == 1);
}
END_TEST

START_TEST(test_slice2)
{
   input_from_file(TESTDIR "/eopt/slice2.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 8);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":slice2(test)"));

   e_node_t s = e_signal(top, 0);
   fail_unless(e_ident(s) == ident_new("VEC"));
   fail_unless(e_path(s) == ident_new(":slice2:vec"));
   fail_unless(e_width(s) == 8);
   fail_unless(e_nexuses(s) == 8);
   fail_unless(e_width(e_nexus(s, 0)) == 1);
   fail_unless(e_flags(s) & E_F_CONTIGUOUS);
   fail_unless(e_flags(s) & E_F_LAST_VALUE);
}
END_TEST

START_TEST(test_map1)
{
   input_from_file(TESTDIR "/eopt/map1.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 6);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":map1(test)"));
   fail_unless(e_signals(top) == 4);

   e_node_t a = e_signal(top, 0);
   fail_unless(e_ident(a) == ident_new("A"));
   fail_unless(e_nexuses(a) == 1);

   e_node_t b = e_signal(top, 1);
   fail_unless(e_ident(b) == ident_new("B"));
   fail_unless(e_nexuses(b) == 1);

   e_node_t c = e_signal(top, 2);
   fail_unless(e_ident(c) == ident_new("C"));
   fail_unless(e_nexuses(c) == 1);

   e_node_t d = e_signal(top, 3);
   fail_unless(e_ident(d) == ident_new("D"));
   fail_unless(e_nexuses(d) == 1);

   e_node_t sub = e_scope(top, 0);
   fail_unless(e_instance(sub) == ident_new(":map1(test):sub1_i@sub(test)"));
   fail_unless(e_signals(sub) == 2);

   e_node_t i = e_signal(sub, 0);
   fail_unless(e_ident(i) == ident_new("I"));
   fail_unless(e_width(i) == 8);
   fail_unless(e_nexuses(i) == 2);
   fail_unless(e_nexus(i, 0) == e_nexus(b, 0));
   fail_unless(e_nexus(i, 1) == e_nexus(a, 0));

   e_node_t o = e_signal(sub, 1);
   fail_unless(e_ident(o) == ident_new("O"));
   fail_unless(e_width(o) == 8);
   fail_unless(e_nexuses(o) == 2);
   fail_unless(e_nexus(o, 0) == e_nexus(d, 0));
   fail_unless(e_nexus(o, 1) == e_nexus(c, 0));

   e_node_t sub2 = e_scope(top, 1);
   fail_unless(e_instance(sub2) == ident_new(":map1(test):sub2_i@sub(test)"));
   fail_unless(e_signals(sub2) == 2);

   e_node_t i2 = e_signal(sub2, 0);
   fail_unless(e_ident(i2) == ident_new("I"));
   fail_unless(e_width(i2) == 8);
   fail_unless(e_nexuses(i2) == 2);

   e_node_t i2n0 = e_nexus(i2, 0);
   fail_unless(e_ident(i2n0) == ident_new(":map1:sub2_i:i[0:5]"));
   fail_unless(e_width(i2n0) == 6);
   fail_unless(e_size(i2n0) == 1);
   fail_unless(e_signals(i2n0) == 1);

   fail_unless(e_nexus(i2, 1) == e_nexus(a, 0));
}
END_TEST

START_TEST(test_map2)
{
   input_from_file(TESTDIR "/eopt/map2.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 7);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":map2(test)"));
   fail_unless(e_signals(top) == 4);

   e_node_t a = e_signal(top, 0);
   fail_unless(e_ident(a) == ident_new("A"));
   fail_unless(e_nexuses(a) == 1);
   fail_unless(e_flags(a) & E_F_CONTIGUOUS);

   e_node_t c = e_signal(top, 2);
   fail_unless(e_ident(c) == ident_new("C"));
   fail_unless(e_nexuses(c) == 1);
   fail_unless(e_flags(c) & E_F_CONTIGUOUS);

   e_node_t sub = e_scope(top, 0);
   fail_unless(e_instance(sub) == ident_new(":map2(test):sub1_i@sub(test)"));
   fail_unless(e_signals(sub) == 2);

   e_node_t i = e_signal(sub, 0);
   fail_unless(e_ident(i) == ident_new("I"));
   fail_unless(e_width(i) == 8);
   fail_unless(e_nexuses(i) == 4);
   fail_if(e_flags(i) & E_F_CONTIGUOUS);
   fail_if(e_flags(i) & E_F_LAST_VALUE);

   e_node_t n0 = e_nexus(i, 0);
   fail_unless(n0 == e_nexus(a, 0));
   fail_unless(e_width(n0) == 2);
   fail_unless(e_signals(n0) == 2);

   e_node_t n1 = e_nexus(i, 1);
   fail_unless(e_ident(n1) == ident_new(":map2:sub1_i:i[2]"));
   fail_unless(e_width(n1) == 1);
   fail_unless(e_signals(n1) == 1);

   e_node_t n2 = e_nexus(i, 2);
   fail_unless(n2 == e_nexus(c, 0));
   fail_unless(e_width(n2) == 4);
   fail_unless(e_signals(n2) == 2);

   e_node_t n3 = e_nexus(i, 3);
   fail_unless(e_ident(n3) == ident_new(":map2:sub1_i:i[7]"));
   fail_unless(e_width(n3) == 1);
   fail_unless(e_signals(n3) == 1);
}
END_TEST

START_TEST(test_alias1)
{
   input_from_file(TESTDIR "/eopt/alias1.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 3);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":alias1(test)"));
   fail_unless(e_signals(top) == 1);

   e_node_t s = e_signal(top, 0);
   fail_unless(e_ident(s) == ident_new("X"));
   fail_unless(e_path(s) == ident_new(":alias1:x"));
   fail_unless(e_nexuses(s) == 3);
   fail_unless(e_width(e_nexus(s, 0)) == 1);
   fail_unless(e_width(e_nexus(s, 1)) == 3);
   fail_unless(e_width(e_nexus(s, 2)) == 4);
}
END_TEST

START_TEST(test_pcall1)
{
   input_from_file(TESTDIR "/eopt/pcall1.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 12);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":pcall1(behav)"));
   fail_unless(e_signals(top) == 1);

   e_node_t s = e_signal(top, 0);
   fail_unless(e_ident(s) == ident_new("POINTS"));
   fail_unless(e_path(s) == ident_new(":pcall1:points"));
   fail_unless(e_nexuses(s) == 12);
   fail_unless(e_width(e_nexus(s, 0)) == 1);
   fail_unless(e_width(e_nexus(s, 1)) == 1);

   e_node_t p1 = e_proc(top, 0);
   fail_unless(e_ident(p1) == ident_new("P1"));
   fail_unless(e_nexuses(p1) == 12);
}
END_TEST

START_TEST(test_arrayref4)
{
   input_from_file(TESTDIR "/eopt/arrayref4.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 5);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":arrayref4(test)"));
   fail_unless(e_signals(top) == 2);

   e_node_t s0 = e_signal(top, 0);
   fail_unless(e_ident(s0) == ident_new("STICKY"));
   fail_unless(e_nexuses(s0) == 1);

   e_node_t s1 = e_signal(top, 1);
   fail_unless(e_ident(s1) == ident_new("SHIFTEDFRACY_D1"));
   fail_unless(e_nexuses(s1) == 4);
   fail_unless(e_width(e_nexus(s1, 0)) == 25);
   fail_unless(e_width(e_nexus(s1, 1)) == 1);
   fail_unless(e_width(e_nexus(s1, 2)) == 4);
   fail_unless(e_width(e_nexus(s1, 3)) == 20);

   e_node_t p1 = e_proc(top, 1);
   fail_unless(e_ident(p1) == ident_new("STIM"));
   fail_unless(e_nexuses(p1) == 3);
}
END_TEST

START_TEST(test_source1)
{
   input_from_file(TESTDIR "/eopt/source1.vhd");

   const error_t expect[] = {
      { 18, "unresolved signal X with instance name :source1(test):x "
        "has multiple sources" },
      { 22, "X is driven by a process in instance :source1(test)" },
      { 23, "X is driven by process FOO in instance :source1(test)" },
      { 19, "part of unresolved signal Y with instance name :source1(test):y "
        "has multiple sources" },
      { 25, "Y is driven by a process in instance :source1(test)" },
      { 26, "part of Y is driven by a process in instance :source1(test)" },
      {  4, "part of Y is sourced by port O in instance "
         ":source1(test):sub2_i@sub(test)" },
      { -1, NULL }
   };
   expect_errors(expect);

   run_eopt();

   check_expected_errors();
}
END_TEST

START_TEST(test_issue427)
{
   input_from_file(TESTDIR "/eopt/issue427.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 7);
   fail_unless(e_scopes(e) == 3);

   e_node_t n0 = e_nexus(e, 0);
   fail_unless(e_ident(n0) == ident_new(":test_ng:sync"));
   fail_unless(e_sources(n0) == 2);

   e_node_t n0p0 = e_source(n0, 0);
   fail_unless(e_kind(n0p0) == E_PORT);
   fail_unless(e_ident(e_nexus(n0p0, 0)) ==
               ident_new(":test_ng:plug(1):driver:sync"));

   e_node_t top = e_scope(e, 2);
   fail_unless(e_instance(top) == ident_new(":test_ng(model)"));
   fail_unless(e_signals(top) == 3);

   e_node_t s0 = e_signal(top, 0);
   fail_unless(e_ident(s0) == ident_new("SYNC"));
   fail_unless(e_nexuses(s0) == 1);
   fail_unless(e_flags(s0) & E_F_RESOLVED);

   fail_if_errors();
}
END_TEST

START_TEST(test_pcall2)
{
   input_from_file(TESTDIR "/eopt/pcall2.vhd");

   e_node_t e = run_eopt();

   fail_unless(e_nexuses(e) == 11);
   fail_unless(e_scopes(e) == 2);

   e_node_t top = e_scope(e, 1);
   fail_unless(e_instance(top) == ident_new(":pcall2(test)"));
   fail_unless(e_signals(top) == 4);

   e_node_t x = e_signal(top, 0);
   fail_unless(e_ident(x) == ident_new("X"));
   fail_unless(e_nexuses(x) == 2);
   fail_unless(e_width(x) == 3);
   fail_unless(e_width(e_nexus(x, 0)) == 2);
   fail_unless(e_sources(e_nexus(x, 0)) == 0);
   fail_unless(e_sources(e_nexus(x, 1)) == 1);
   fail_if(e_flags(x) & E_F_LAST_VALUE);

   e_node_t y = e_signal(top, 1);
   fail_unless(e_ident(y) == ident_new("Y"));
   fail_unless(e_nexuses(y) == 3);
   fail_unless(e_width(y) == 3);
   fail_unless(e_width(e_nexus(y, 0)) == 1);
   fail_unless(e_flags(y) & E_F_LAST_VALUE);

   e_node_t z = e_signal(top, 2);
   fail_unless(e_ident(z) == ident_new("Z"));
   fail_unless(e_nexuses(z) == 3);
   fail_unless(e_width(z) == 3);
   fail_unless(e_width(e_nexus(z, 0)) == 1);
   fail_if(e_flags(z) & E_F_LAST_VALUE);

   e_node_t s = e_signal(top, 3);
   fail_unless(e_ident(s) == ident_new("S"));
   fail_unless(e_nexuses(s) == 3);
   fail_unless(e_width(s) == 3);
   fail_unless(e_width(e_nexus(s, 0)) == 1);
   fail_unless(e_flags(s) & E_F_LAST_VALUE);

   fail_if_errors();
}
END_TEST

Suite *get_eopt_tests(void)
{
   Suite *s = suite_create("eopt");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_ram1);
   tcase_add_test(tc, test_arrayref1);
   tcase_add_test(tc, test_arrayref2);
   tcase_add_test(tc, test_arrayref3);
   tcase_add_test(tc, test_recref1);
   tcase_add_test(tc, test_recref2);
   tcase_add_test(tc, test_slice1);
   tcase_add_test(tc, test_array3);
   tcase_add_test(tc, test_jcore4);
   tcase_add_test(tc, test_jcore2);
   tcase_add_test(tc, test_issue72);
   tcase_add_test(tc, test_issue73);
   tcase_add_test(tc, test_issue95);
   tcase_add_test(tc, test_issue371);
   tcase_add_test(tc, test_issue250);
   tcase_add_test(tc, test_partial1);
   tcase_add_test(tc, test_nonconst1);
   tcase_add_test(tc, test_slice2);
   tcase_add_test(tc, test_map1);
   tcase_add_test(tc, test_map2);
   tcase_add_test(tc, test_alias1);
   tcase_add_test(tc, test_pcall1);
   tcase_add_test(tc, test_arrayref4);
   tcase_add_test(tc, test_source1);
   tcase_add_test(tc, test_issue427);
   tcase_add_test(tc, test_pcall2);
   suite_add_tcase(s, tc);

   return s;
}

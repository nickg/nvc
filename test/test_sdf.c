//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "lib.h"
#include "option.h"
#include "phase.h"
#include "sdf/sdf-node.h"
#include "sdf/sdf-phase.h"
#include "scan.h"
#include "type.h"

#include <math.h>

#define fail_unless_floats_equal(a, b) fail_unless(fabs(a - b) < 0.00001);

START_TEST(test_parse1)
{
   input_from_file(TESTDIR "/sdf/parse1.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   fail_unless(sdf_kind(s) == S_DELAY_FILE);
   fail_unless(sdf_subkind(s) == SDF_STD_4_0);

   sdf_node_t ver = sdf_decl(s, 0);
   fail_unless(sdf_kind(ver) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(ver) == S_HEADER_SDF_VERSION);
}
END_TEST

START_TEST(test_parse2)
{
   input_from_file(TESTDIR "/sdf/parse2.sdf");

   const error_t expect[] = {
      { 2, "Invalid SDF version: \"FOR_SURE_INVALID_SDF_VERSION\". "
           "SDF version shall contain one of: \"1.0\", \"2.0\", \"2.1\", \"3.0\" or \"4.0\"" },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
}
END_TEST

START_TEST(test_parse3)
{
   input_from_file(TESTDIR "/sdf/parse3.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   fail_unless(sdf_kind(s) == S_DELAY_FILE);
   fail_unless(sdf_subkind(s) == SDF_STD_1_0);

   fail_unless(sdf_decls(s) == 6);

   sdf_node_t sdf_version = sdf_decl(s, 0);
   fail_unless(sdf_kind(sdf_version) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(sdf_version) == S_HEADER_SDF_VERSION);
   fail_unless(sdf_ident(sdf_version) == ident_new("\"Fancy SDF super version 1.0 followed by 2.1 that shall be ignored\""));

   sdf_node_t design = sdf_decl(s, 1);
   fail_unless(sdf_kind(design) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(design) == S_HEADER_DESIGN);
   fail_unless(sdf_ident(design) == ident_new("\"testchip\""));

   sdf_node_t date = sdf_decl(s, 2);
   fail_unless(sdf_kind(date) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(date) == S_HEADER_DATE);
   fail_unless(sdf_ident(date) == ident_new("\"Dec 17, 1991 14:49:48\""));

   sdf_node_t vendor = sdf_decl(s, 3);
   fail_unless(sdf_kind(vendor) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(vendor) == S_HEADER_VENDOR);
   fail_unless(sdf_ident(vendor) == ident_new("\"Big Chips Inc.\""));

   sdf_node_t program = sdf_decl(s, 4);
   fail_unless(sdf_kind(program) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(program) == S_HEADER_PROGRAM);
   fail_unless(sdf_ident(program) == ident_new("\"New VHDL Compiler\""));

   sdf_node_t version = sdf_decl(s, 5);
   fail_unless(sdf_kind(version) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(version) == S_HEADER_VERSION);
   fail_unless(sdf_ident(version) == ident_new("\"1.10.2\""));
}
END_TEST

START_TEST(test_parse4)
{
   input_from_file(TESTDIR "/sdf/parse4.sdf");

   const error_t expect[] = {
      { 6, "Used hierarchy separator: / but hierarchy separator defined in SDF header is: ." },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   fail_unless(sdf_kind(s) == S_DELAY_FILE);
   fail_unless(sdf_subkind(s) == SDF_STD_2_1);

   fail_unless(sdf_decls(s) == 2);

   sdf_node_t sdf_version = sdf_decl(s, 0);
   fail_unless(sdf_kind(sdf_version) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(sdf_version) == S_HEADER_SDF_VERSION);

   sdf_node_t design = sdf_decl(s, 1);
   fail_unless(sdf_kind(design) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(design) == S_HEADER_DIVIDER);
   fail_unless(sdf_ident(design) == ident_new("."));
}
END_TEST

START_TEST(test_parse5)
{
   input_from_file(TESTDIR "/sdf/parse5.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   fail_unless(sdf_kind(s) == S_DELAY_FILE);
   fail_unless(sdf_subkind(s) == SDF_STD_3_0);

   fail_unless(sdf_decls(s) == 4);

   sdf_node_t voltage = sdf_decl(s, 1);
   fail_unless(sdf_kind(voltage) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(voltage) == S_HEADER_VOLTAGE);
   sdf_node_t voltage_val = sdf_number(voltage);
   fail_unless(sdf_kind(voltage_val) == S_TRIPPLE);
   sdf_node_t voltage_min = sdf_min(voltage_val);
   sdf_node_t voltage_typ = sdf_typ(voltage_val);
   sdf_node_t voltage_max = sdf_max(voltage_val);
   fail_unless(sdf_subkind(voltage_min) == S_NUMBER_DOUBLE);
   fail_unless(sdf_subkind(voltage_typ) == S_NUMBER_DOUBLE);
   fail_unless(sdf_subkind(voltage_max) == S_NUMBER_DOUBLE);
   fail_unless_floats_equal(sdf_dval(voltage_min), 1.08);
   fail_unless_floats_equal(sdf_dval(voltage_typ), 1.20);
   fail_unless_floats_equal(sdf_dval(voltage_max), 1.32);

   sdf_node_t temp = sdf_decl(s, 3);
   fail_unless(sdf_kind(temp) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(temp) == S_HEADER_TEMPERATURE);
   sdf_node_t temp_val = sdf_number(temp);
   fail_unless(sdf_kind(temp_val) == S_TRIPPLE);
   sdf_node_t temp_min = sdf_min(temp_val);
   sdf_node_t temp_typ = sdf_typ(temp_val);
   sdf_node_t temp_max = sdf_max(temp_val);
   fail_unless(sdf_subkind(temp_min) == S_NUMBER_DOUBLE);
   fail_unless(sdf_subkind(temp_typ) == S_NUMBER_DOUBLE);
   fail_unless(sdf_subkind(temp_max) == S_NUMBER_DOUBLE);
   fail_unless_floats_equal(sdf_dval(temp_min), -40.0);
   fail_unless_floats_equal(sdf_dval(temp_typ), 25);
   fail_unless_floats_equal(sdf_dval(temp_max), 125);
}
END_TEST

START_TEST(test_parse6)
{
   input_from_file(TESTDIR "/sdf/parse6.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   fail_unless(sdf_kind(s) == S_DELAY_FILE);
   fail_unless(sdf_subkind(s) == SDF_STD_3_0);

   fail_unless(sdf_decls(s) == 2);

   sdf_node_t timescale = sdf_decl(s, 1);
   fail_unless(sdf_kind(timescale) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(timescale) == S_HEADER_TIMESCALE);
   fail_unless(sdf_ident(timescale) == ident_new("ns"));
   double scale_num = sdf_dval(sdf_number(timescale));
   fail_unless_floats_equal(scale_num, 10.0);
}
END_TEST

START_TEST(test_parse7)
{
   input_from_file(TESTDIR "/sdf/parse7.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   fail_unless(sdf_decls(s) == 1);
   fail_unless(sdf_cells(s) == 1);

   sdf_node_t cell = sdf_cell(s, 0);
   fail_unless(sdf_ident(cell) == ident_new("\"AND2\""));
   fail_unless(sdf_ident2(cell) == ident_new("*"));
}
END_TEST

START_TEST(test_parse8)
{
   input_from_file(TESTDIR "/sdf/parse8.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   fail_unless(sdf_decls(s) == 1);
   fail_unless(sdf_cells(s) == 1);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 6);


   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_kind(d_0)     == S_DELAY);
   fail_unless(sdf_subkind(d_0)  == S_DELAY_KIND_IOPATH);
   fail_unless(sdf_signals(d_0)  == 2);
   fail_unless(sdf_values(d_0)   == 1);
   fail_unless((sdf_flags(d_0) & S_F_VALUE_ABSOLUTE) > 0);

   sdf_node_t d_0_v = sdf_value(d_0, 0);
   fail_unless(sdf_kind(d_0_v)   == S_DELVAL);


   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_kind(d_1)     == S_DELAY);
   fail_unless(sdf_subkind(d_1)  == S_DELAY_KIND_PORT);
   fail_unless(sdf_signals(d_1)  == 1);
   fail_unless(sdf_values(d_1)   == 1);
   fail_unless((sdf_flags(d_1) & S_F_VALUE_ABSOLUTE) > 0);

   sdf_node_t d_1_v = sdf_value(d_1, 0);
   fail_unless(sdf_kind(d_1_v)   == S_DELVAL);


   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_kind(d_2)     == S_DELAY);
   fail_unless(sdf_subkind(d_2)  == S_DELAY_KIND_INTERCONNECT);
   fail_unless(sdf_signals(d_2)  == 2);
   fail_unless(sdf_values(d_2)   == 1);
   fail_unless((sdf_flags(d_2) & S_F_VALUE_ABSOLUTE) > 0);

   sdf_node_t d_2_v = sdf_value(d_2, 0);
   fail_unless(sdf_kind(d_2_v)   == S_DELVAL);


   sdf_node_t d_3 = sdf_delay(c, 3);
   fail_unless(sdf_kind(d_3)     == S_DELAY);
   fail_unless(sdf_subkind(d_3)  == S_DELAY_KIND_NETDELAY);
   fail_unless(sdf_signals(d_3)  == 1);
   fail_unless(sdf_values(d_3)   == 1);
   fail_unless((sdf_flags(d_3) & S_F_VALUE_INCREMENT) > 0);

   sdf_node_t d_3_v = sdf_value(d_3, 0);
   fail_unless(sdf_kind(d_3_v)   == S_DELVAL);


   sdf_node_t d_4 = sdf_delay(c, 4);
   fail_unless(sdf_kind(d_4)     == S_DELAY);
   fail_unless(sdf_subkind(d_4)  == S_DELAY_KIND_DEVICE);
   fail_unless(sdf_signals(d_4)  == 1);
   fail_unless(sdf_values(d_4)   == 1);
   fail_unless((sdf_flags(d_4) & S_F_VALUE_INCREMENT) > 0);

   sdf_node_t d_4_v = sdf_value(d_4, 0);
   fail_unless(sdf_kind(d_4_v)   == S_DELVAL);


   sdf_node_t d_5 = sdf_delay(c, 5);
   fail_unless(sdf_kind(d_5)     == S_DELAY);
   fail_unless(sdf_subkind(d_5)  == S_DELAY_KIND_DEVICE);
   fail_unless(sdf_signals(d_5)    == 0);
   fail_unless(sdf_values(d_5)   == 1);
   fail_unless((sdf_flags(d_5) & S_F_VALUE_INCREMENT) > 0);

   sdf_node_t d_5_v = sdf_value(d_5, 0);
   fail_unless(sdf_kind(d_5_v)   == S_DELVAL);
}
END_TEST

START_TEST(test_parse9)
{
   input_from_file(TESTDIR "/sdf/parse9.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 3);


   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_signals(d_0) == 2);
   sdf_node_t d_0_p_0 = sdf_signal(d_0, 0);
   sdf_node_t d_0_p_1 = sdf_signal(d_0, 1);
   fail_unless(sdf_kind(d_0_p_0)       == S_SIGNAL);
   fail_unless(sdf_kind(d_0_p_1)       == S_SIGNAL);
   fail_unless(sdf_subkind(d_0_p_0)    == S_SIGNAL_SCALAR);
   fail_unless(sdf_subkind(d_0_p_1)    == S_SIGNAL_SCALAR);
   fail_unless(sdf_ident(d_0_p_0)      == ident_new("a"));
   fail_unless(sdf_ident(d_0_p_1)      == ident_new("x"));

   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_signals(d_1) == 2);
   sdf_node_t d_1_p_0 = sdf_signal(d_1, 0);
   sdf_node_t d_1_p_1 = sdf_signal(d_1, 1);
   fail_unless(sdf_kind(d_1_p_0)       == S_SIGNAL);
   fail_unless(sdf_kind(d_1_p_1)       == S_SIGNAL);
   fail_unless(sdf_subkind(d_1_p_0)    == S_SIGNAL_SCALAR);
   fail_unless(sdf_subkind(d_1_p_1)    == S_SIGNAL_SCALAR);
   fail_unless(sdf_ident(d_1_p_0)      == ident_new("top.dut.b"));
   fail_unless(sdf_ident(d_1_p_1)      == ident_new("top.dut.x"));
   fail_unless(sdf_dims(d_1_p_0)       == 1);
   fail_unless(sdf_dims(d_1_p_1)       == 1);
   sdf_node_t d_1_min_p_0 = sdf_dim(d_1_p_0, 0);
   sdf_node_t d_1_min_p_1 = sdf_dim(d_1_p_1, 0);
   fail_unless(sdf_ival(d_1_min_p_0)   == 897);
   fail_unless(sdf_ival(d_1_min_p_1)   == 921);

   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_signals(d_2) == 2);
   sdf_node_t d_2_p_0 = sdf_signal(d_2, 0);
   sdf_node_t d_2_p_1 = sdf_signal(d_2, 1);
   fail_unless(sdf_kind(d_2_p_0)       == S_SIGNAL);
   fail_unless(sdf_kind(d_2_p_1)       == S_SIGNAL);
   fail_unless(sdf_subkind(d_2_p_0)    == S_SIGNAL_BUS);
   fail_unless(sdf_subkind(d_2_p_1)    == S_SIGNAL_BUS);
   fail_unless(sdf_ident(d_2_p_0)      == ident_new("b"));
   fail_unless(sdf_ident(d_2_p_1)      == ident_new("x"));
   fail_unless(sdf_dims(d_2_p_0)       == 2);
   fail_unless(sdf_dims(d_2_p_1)       == 2);
   sdf_node_t d_2_min_p_0              = sdf_dim(d_2_p_0, 0);
   sdf_node_t d_2_min_p_1              = sdf_dim(d_2_p_1, 0);
   sdf_node_t d_2_max_p_0              = sdf_dim(d_2_p_0, 1);
   sdf_node_t d_2_max_p_1              = sdf_dim(d_2_p_1, 1);
   fail_unless(sdf_ival(d_2_min_p_0)   == 5);
   fail_unless(sdf_ival(d_2_min_p_1)   == 9);
   fail_unless(sdf_ival(d_2_max_p_0)   == 2);
   fail_unless(sdf_ival(d_2_max_p_1)   == 6);

}
END_TEST

START_TEST(test_parse10)
{
   input_from_file(TESTDIR "/sdf/parse10.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 3);

   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_signals(d_0) == 2);
   sdf_node_t d_0_p_0 = sdf_signal(d_0, 0);
   sdf_node_t d_0_p_1 = sdf_signal(d_0, 1);
   fail_unless(sdf_kind(d_0_p_0)       == S_SIGNAL);
   fail_unless(sdf_kind(d_0_p_1)       == S_SIGNAL);
   fail_unless(sdf_subkind(d_0_p_0)    == S_SIGNAL_SCALAR);
   fail_unless(sdf_subkind(d_0_p_1)    == S_SIGNAL_SCALAR);
   fail_unless(sdf_ident(d_0_p_0)      == ident_new("clk_a"));
   fail_unless(sdf_ident(d_0_p_1)      == ident_new("x_q"));
   fail_unless(sdf_dims(d_0_p_0)       == 0);
   fail_unless(sdf_dims(d_0_p_1)       == 0);

   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_signals(d_1) == 2);
   sdf_node_t d_1_p_0 = sdf_signal(d_1, 0);
   sdf_node_t d_1_p_1 = sdf_signal(d_1, 1);
   fail_unless(sdf_kind(d_1_p_0)       == S_SIGNAL);
   fail_unless(sdf_kind(d_1_p_1)       == S_SIGNAL);
   fail_unless(sdf_subkind(d_1_p_0)    == S_SIGNAL_SCALAR);
   fail_unless(sdf_subkind(d_1_p_1)    == S_SIGNAL_SCALAR);
   fail_unless(sdf_ident(d_1_p_0)      == ident_new("clk_b"));
   fail_unless(sdf_ident(d_1_p_1)      == ident_new("y_q"));
   fail_unless(sdf_dims(d_1_p_0)       == 1);
   fail_unless(sdf_dims(d_1_p_1)       == 0);
   sdf_node_t d_1_min_p_0 = sdf_dim(d_1_p_0, 0);
   fail_unless(sdf_ival(d_1_min_p_0)   == 17);

   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_signals(d_2) == 2);
   sdf_node_t d_2_p_0 = sdf_signal(d_2, 0);
   sdf_node_t d_2_p_1 = sdf_signal(d_2, 1);
   fail_unless(sdf_kind(d_2_p_0)       == S_SIGNAL);
   fail_unless(sdf_kind(d_2_p_1)       == S_SIGNAL);
   fail_unless(sdf_subkind(d_2_p_0)    == S_SIGNAL_BUS);
   fail_unless(sdf_subkind(d_2_p_1)    == S_SIGNAL_BUS);
   fail_unless(sdf_ident(d_2_p_0)      == ident_new("clk_c"));
   fail_unless(sdf_ident(d_2_p_1)      == ident_new("z_q"));
   fail_unless(sdf_dims(d_2_p_0)       == 2);
   fail_unless(sdf_dims(d_2_p_1)       == 2);
   sdf_node_t d_2_min_p_0              = sdf_dim(d_2_p_0, 0);
   sdf_node_t d_2_min_p_1              = sdf_dim(d_2_p_1, 0);
   sdf_node_t d_2_max_p_0              = sdf_dim(d_2_p_0, 1);
   sdf_node_t d_2_max_p_1              = sdf_dim(d_2_p_1, 1);
   fail_unless(sdf_ival(d_2_min_p_0)   == 2);
   fail_unless(sdf_ival(d_2_min_p_1)   == 1);
   fail_unless(sdf_ival(d_2_max_p_0)   == 3);
   fail_unless(sdf_ival(d_2_max_p_1)   == 2);
}
END_TEST

START_TEST(test_parse11)
{
   input_from_file(TESTDIR "/sdf/parse11.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 3);

   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_values(d_0) == 1);
   sdf_node_t d_0_v_0 = sdf_value(d_0, 0);
   fail_unless(sdf_kind(d_0_v_0) == S_DELVAL);
   double d_0_v_0_f = sdf_dval(sdf_number(sdf_value(d_0_v_0, 0)));
   fail_unless_floats_equal(d_0_v_0_f, 17);

   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_values(d_1) == 1);
   sdf_node_t d_1_v_0 = sdf_value(d_1, 0);
   fail_unless(sdf_kind(d_1_v_0) == S_DELVAL);
   sdf_node_t d_1_v_0_0 = sdf_value(d_1_v_0, 0);
   sdf_node_t d_1_v_0_1 = sdf_value(d_1_v_0, 1);
   double d_1_v_0_0_f = sdf_dval(sdf_number(d_1_v_0_0));
   double d_1_v_0_1_f = sdf_dval(sdf_number(d_1_v_0_1));
   fail_unless_floats_equal(d_1_v_0_0_f, 2.3);
   fail_unless_floats_equal(d_1_v_0_1_f, 1.2);

   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_values(d_2) == 1);
   sdf_node_t d_2_v_0 = sdf_value(d_2, 0);
   fail_unless(sdf_kind(d_2_v_0) == S_DELVAL);
   sdf_node_t d_2_v_0_0 = sdf_value(d_2_v_0, 0);
   sdf_node_t d_2_v_0_1 = sdf_value(d_2_v_0, 1);
   sdf_node_t d_2_v_0_2 = sdf_value(d_2_v_0, 2);
   double d_2_v_0_0_f = sdf_dval(sdf_number(d_2_v_0_0));
   double d_2_v_0_1_f = sdf_dval(sdf_number(d_2_v_0_1));
   double d_2_v_0_2_f = sdf_dval(sdf_number(d_2_v_0_2));
   fail_unless_floats_equal(d_2_v_0_0_f, 0.0);
   fail_unless_floats_equal(d_2_v_0_1_f, 1.0);
   fail_unless_floats_equal(d_2_v_0_2_f, 2.5);
}
END_TEST

START_TEST(test_parse12)
{
   input_from_file(TESTDIR "/sdf/parse12.sdf");

   const error_t expect[] = {
      { 9, "'delval_list' shall have at most 12 'delval' entries" },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 2);

   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_values(d_0) == 3);
   sdf_node_t d_0_v_0 = sdf_value(d_0, 0);
   sdf_node_t d_0_v_1 = sdf_value(d_0, 1);
   sdf_node_t d_0_v_2 = sdf_value(d_0, 2);
   fail_unless(sdf_kind(d_0_v_0) == S_DELVAL);
   fail_unless(sdf_kind(d_0_v_1) == S_DELVAL);
   fail_unless(sdf_kind(d_0_v_2) == S_DELVAL);

   sdf_node_t d_0_v_0_0 = sdf_value(d_0_v_0, 0);
   fail_unless(sdf_kind(d_0_v_0_0) == S_VALUE);
   sdf_node_t d_0_v_0_0_t = sdf_number(d_0_v_0_0);
   fail_unless_floats_equal(sdf_dval(sdf_min(d_0_v_0_0_t)), 1.0);
   fail_unless_floats_equal(sdf_dval(sdf_typ(d_0_v_0_0_t)), 2.4);
   fail_unless_floats_equal(sdf_dval(sdf_max(d_0_v_0_0_t)), 3.0);


   sdf_node_t d_0_v_1_0 = sdf_value(d_0_v_1, 0);
   sdf_node_t d_0_v_1_1 = sdf_value(d_0_v_1, 1);
   sdf_node_t d_0_v_1_2 = sdf_value(d_0_v_1, 2);
   sdf_node_t d_0_v_1_0_t = sdf_number(d_0_v_1_0);
   sdf_node_t d_0_v_1_1_t = sdf_number(d_0_v_1_1);
   sdf_node_t d_0_v_1_2_t = sdf_number(d_0_v_1_2);

   fail_unless(!sdf_has_min(d_0_v_1_0_t));
   fail_unless_floats_equal(sdf_dval(sdf_typ(d_0_v_1_0_t)), 4.7);
   fail_unless_floats_equal(sdf_dval(sdf_max(d_0_v_1_0_t)), 5.0);

   fail_unless_floats_equal(sdf_dval(sdf_min(d_0_v_1_1_t)), 6.1);
   fail_unless(!sdf_has_typ(d_0_v_1_1_t));
   fail_unless_floats_equal(sdf_dval(sdf_max(d_0_v_1_1_t)), 7.0);

   fail_unless_floats_equal(sdf_dval(sdf_min(d_0_v_1_2_t)), 8.9);
   fail_unless_floats_equal(sdf_dval(sdf_typ(d_0_v_1_2_t)), 9.0);
   fail_unless(!sdf_has_max(d_0_v_1_2_t));


   sdf_node_t d_0_v_2_0 = sdf_value(d_0_v_2, 0);
   sdf_node_t d_0_v_2_1 = sdf_value(d_0_v_2, 1);
   sdf_node_t d_0_v_2_2 = sdf_value(d_0_v_2, 2);
   sdf_node_t d_0_v_2_0_t = sdf_number(d_0_v_2_0);
   sdf_node_t d_0_v_2_1_t = sdf_number(d_0_v_2_1);
   sdf_node_t d_0_v_2_2_t = sdf_number(d_0_v_2_2);

   fail_unless_floats_equal(sdf_dval(sdf_min(d_0_v_2_0_t)), 10.00007);
   fail_unless(!sdf_has_typ(d_0_v_2_0_t));
   fail_unless(!sdf_has_max(d_0_v_2_0_t));

   fail_unless(!sdf_has_min(d_0_v_2_1_t));
   fail_unless_floats_equal(sdf_dval(sdf_typ(d_0_v_2_1_t)), 11.5);
   fail_unless(!sdf_has_max(d_0_v_2_1_t));

   fail_unless(!sdf_has_min(d_0_v_2_2_t));
   fail_unless(!sdf_has_typ(d_0_v_2_2_t));
   fail_unless_floats_equal(sdf_dval(sdf_max(d_0_v_2_2_t)), 12.99);
}
END_TEST

START_TEST(test_parse13)
{
   input_from_file(TESTDIR "/sdf/parse13.sdf");

   const error_t expect[] = {
      { 4, "unexpected ( while parsing delay file, expecting )" },
      { 7, "Duplicit header item" },
      { -1, NULL }
   };

   expect_errors(expect);

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);
}
END_TEST

START_TEST(test_parse14)
{
   input_from_file(TESTDIR "/sdf/parse14.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_labels(c) == 4);

   sdf_node_t l_0 = sdf_label(c, 0);
   fail_unless(sdf_ident(l_0) == ident_new("TCLK_Q"));
   fail_unless(sdf_values(l_0) == 2);
   fail_unless((sdf_flags(l_0) & S_F_VALUE_ABSOLUTE) > 0);

   sdf_node_t l_1 = sdf_label(c, 1);
   fail_unless(sdf_ident(l_1) == ident_new("TSETUP_D_CLK"));
   fail_unless(sdf_values(l_1) == 1);
   fail_unless((sdf_flags(l_1) & S_F_VALUE_ABSOLUTE) > 0);

   sdf_node_t l_2 = sdf_label(c, 2);
   fail_unless(sdf_ident(l_2) == ident_new("TCLK_QB"));
   fail_unless(sdf_values(l_2) == 2);
   fail_unless((sdf_flags(l_2) & S_F_VALUE_INCREMENT) > 0);

   sdf_node_t l_3 = sdf_label(c, 3);
   fail_unless(sdf_ident(l_3) == ident_new("THOLD_D_CLK"));
   fail_unless(sdf_values(l_3) == 1);
   fail_unless((sdf_flags(l_3) & S_F_VALUE_INCREMENT) > 0);
}
END_TEST

START_TEST(test_parse15)
{
   input_from_file(TESTDIR "/sdf/parse15.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 4);

   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_subkind(d_0) == S_DELAY_KIND_PATHPULSE);
   fail_unless(sdf_values(d_0) == 1);

   sdf_node_t d_0_v_0 = sdf_value(d_0, 0);
   fail_unless(sdf_kind(d_0_v_0) == S_VALUE);

   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_subkind(d_1) == S_DELAY_KIND_PATHPULSE);
   fail_unless(sdf_values(d_1) == 2);

   sdf_node_t d_1_v_0 = sdf_value(d_1, 0);
   fail_unless(sdf_kind(d_1_v_0) == S_VALUE);
   fail_unless_floats_equal(sdf_dval(sdf_number(d_1_v_0)), 0.1);

   sdf_node_t d_1_v_1 = sdf_value(d_1, 1);
   fail_unless(sdf_kind(d_1_v_1) == S_VALUE);
   fail_unless_floats_equal(sdf_dval(sdf_number(d_1_v_1)), 0.5);

   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_subkind(d_2) == S_DELAY_KIND_PATHPULSE);
   fail_unless(sdf_values(d_2) == 1);

   sdf_node_t d_2_p_1 = sdf_signal(d_2, 0);
   sdf_node_t d_2_p_2 = sdf_signal(d_2, 1);

   fail_unless(icmp(sdf_ident(d_2_p_1), "my_inst.my_sub_inst.a"));
   fail_unless(icmp(sdf_ident(d_2_p_2), "my_inst_my_sub_inst.b"));

   sdf_node_t d_2_v_0 = sdf_value(d_2, 0);
   fail_unless(sdf_kind(d_2_v_0) == S_VALUE);

   sdf_node_t d_3 = sdf_delay(c, 3);
   fail_unless(sdf_subkind(d_3) == S_DELAY_KIND_PATHPULSEP);
   fail_unless(sdf_values(d_3) == 2);

   sdf_node_t d_3_p_1 = sdf_signal(d_3, 0);
   sdf_node_t d_3_p_2 = sdf_signal(d_3, 1);

   fail_unless(icmp(sdf_ident(d_3_p_1), "top.in"));
   fail_unless(icmp(sdf_ident(d_3_p_2), "top.out"));

   fail_unless(sdf_values(d_3) == 2);

   sdf_node_t d_3_v_0 = sdf_value(d_3, 0);
   fail_unless(sdf_kind(d_3_v_0) == S_VALUE);
   fail_unless_floats_equal(sdf_dval(sdf_number(d_3_v_0)), 10.2);

   sdf_node_t d_3_v_1 = sdf_value(d_3, 1);
   fail_unless(sdf_kind(d_3_v_1) == S_VALUE);
   fail_unless_floats_equal(sdf_dval(sdf_number(d_3_v_1)), 11.9);
}
END_TEST

START_TEST(test_parse16)
{
   input_from_file(TESTDIR "/sdf/parse16.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 2);

   // Delay 0
   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_subkind(d_0) == S_DELAY_KIND_IOPATH);
   fail_unless(icmp(sdf_ident(sdf_signal(d_0, 0)), "i_and.a"));
   fail_unless(icmp(sdf_ident(sdf_signal(d_0, 1)), "i_and.y"));

   sdf_node_t d_0_c_0 = sdf_cond(d_0, 0);
   sdf_node_t d_0_c_0_e_0 = sdf_expr(d_0_c_0);
   fail_unless(sdf_kind(d_0_c_0_e_0) == S_BINARY);
   fail_unless(sdf_subkind(d_0_c_0_e_0) == S_BINARY_EXPR_LOGEQ);

   sdf_node_t d_0_c_0_e_0_l = sdf_value(d_0_c_0_e_0, 0);
   sdf_node_t d_0_c_0_e_0_r = sdf_value(d_0_c_0_e_0, 1);

   fail_unless(sdf_kind(d_0_c_0_e_0_l) == S_SIGNAL);
   fail_unless(sdf_kind(d_0_c_0_e_0_r) == S_NUMBER);
   fail_unless(sdf_ival(d_0_c_0_e_0_r) == 0);

   // Delay 1
   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_subkind(d_1) == S_DELAY_KIND_IOPATH);
   fail_unless(icmp(sdf_ident(sdf_signal(d_1, 0)), "i_and.a"));
   fail_unless(icmp(sdf_ident(sdf_signal(d_1, 1)), "i_and.y"));

   sdf_node_t d_1_c_0 = sdf_cond(d_1, 0);
   fail_unless(icmp(sdf_ident(d_1_c_0), "\"My condition name\""));

   sdf_node_t d_1_c_0_e_0 = sdf_expr(d_1_c_0);
   fail_unless(sdf_kind(d_1_c_0_e_0) == S_BINARY);
   fail_unless(sdf_subkind(d_1_c_0_e_0) == S_BINARY_EXPR_LOGEQ);

   sdf_node_t d_1_c_0_e_0_l = sdf_value(d_1_c_0_e_0, 0);
   sdf_node_t d_1_c_0_e_0_r = sdf_value(d_1_c_0_e_0, 1);

   fail_unless(sdf_kind(d_1_c_0_e_0_l) == S_SIGNAL);
   fail_unless(icmp(sdf_ident(d_1_c_0_e_0_l), "i_and.b"));
   fail_unless(sdf_kind(d_1_c_0_e_0_r) == S_NUMBER);
   fail_unless(sdf_ival(d_1_c_0_e_0_r) == 1);
}
END_TEST

START_TEST(test_parse17)
{
   input_from_file(TESTDIR "/sdf/parse17.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 14);

   // Delay 0
   sdf_node_t d_0 = sdf_delay(c, 0);
   sdf_node_t d_0_ce = sdf_expr(sdf_cond(d_0, 0));
   fail_unless(sdf_kind(d_0_ce) == S_SIGNAL);
   fail_unless(icmp(sdf_ident(d_0_ce), "a"));

   // Delay 1 - Tests that "/" hchar is normalized to "."
   sdf_node_t d_1 = sdf_delay(c, 1);
   sdf_node_t d_1_ce = sdf_expr(sdf_cond(d_1, 0));
   fail_unless(sdf_kind(d_1_ce) == S_SIGNAL);
   fail_unless(icmp(sdf_ident(d_1_ce), "a.b.c.d"));

   // Delay 2 - Tests that "/" hchar is normalized to "."
   sdf_node_t d_2 = sdf_delay(c, 2);
   sdf_node_t d_2_ce = sdf_expr(sdf_cond(d_2, 0));
   fail_unless(sdf_kind(d_2_ce) == S_SIGNAL);
   fail_unless(icmp(sdf_ident(d_2_ce), "e.f.g.h"));

   // Delays 3 - 13
   sdf_unary_expr_kind_t unary_kind[11] = {
      S_UNARY_EXPR_PLUS,
      S_UNARY_EXPR_MINUS,
      S_UNARY_EXPR_LOGNOT,
      S_UNARY_EXPR_BITNOT,
      S_UNARY_EXPR_AND,
      S_UNARY_EXPR_NAND,
      S_UNARY_EXPR_OR,
      S_UNARY_EXPR_NOR,
      S_UNARY_EXPR_XOR,
      S_UNARY_EXPR_XNOR,
      S_UNARY_EXPR_XNOR
   };

   for (int i = 0; i < ARRAY_LEN(unary_kind); i++) {
      sdf_node_t d_i = sdf_delay(c, i + 3);
      sdf_node_t d_i_ce = sdf_expr(sdf_cond(d_i, 0));
      fail_unless(sdf_kind(d_i_ce) == S_UNARY);
      fail_unless(sdf_subkind(d_i_ce) == unary_kind[i]);
      fail_unless(icmp(sdf_ident(sdf_value(d_i_ce, 0)), "x.y.z"));
   }
}
END_TEST

START_TEST(test_parse18)
{
   input_from_file(TESTDIR "/sdf/parse18.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);

   sdf_binary_expr_kind_t binary_kind[] = {
      S_BINARY_EXPR_PLUS,
      S_BINARY_EXPR_MINUS,
      S_BINARY_EXPR_MULT,
      S_BINARY_EXPR_DIV,
      S_BINARY_EXPR_MOD,

      S_BINARY_EXPR_LOGEQ,
      S_BINARY_EXPR_LOGNEQ,
      S_BINARY_EXPR_CASEEQ,
      S_BINARY_EXPR_CASENEQ,
      S_BINARY_EXPR_LOGAND,

      S_BINARY_EXPR_LOGOR,
      S_BINARY_EXPR_LT,
      S_BINARY_EXPR_LTEQ,
      S_BINARY_EXPR_GT,
      S_BINARY_EXPR_GTEQ ,

      S_BINARY_EXPR_BITAND,
      S_BINARY_EXPR_BITOR,
      S_BINARY_EXPR_BITXOR,
      S_BINARY_EXPR_BITXNOR,
      S_BINARY_EXPR_BITXNOR,


      S_BINARY_EXPR_SHRIGHT,
      S_BINARY_EXPR_SHLEFT
   };

   for (int i = 0; i < 20; i++) {
      sdf_node_t d_i = sdf_delay(c, i);
      sdf_node_t d_i_ce = sdf_expr(sdf_cond(d_i, 0));
      fail_unless(sdf_kind(d_i_ce) == S_BINARY);
      fail_unless(sdf_subkind(d_i_ce) == binary_kind[i]);
      fail_unless(icmp(sdf_ident(sdf_value(d_i_ce, 0)), "a.b.c"));
      fail_unless(icmp(sdf_ident(sdf_value(d_i_ce, 1)), "x.y.z"));
   }
}
END_TEST

START_TEST(test_parse19)
{
   input_from_file(TESTDIR "/sdf/parse19.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);

   // Delay 0
   sdf_node_t d_0 = sdf_delay(c, 0);
   sdf_node_t d_0_c = sdf_cond(d_0, 0);
   fail_unless(sdf_kind(d_0_c) == S_COND);
   fail_unless(icmp(sdf_ident(d_0_c), "\"my_cond_description\""));

   // Delay 1
   sdf_node_t d_1 = sdf_delay(c, 1);
   sdf_node_t d_1_c = sdf_cond(d_1, 0);
   sdf_node_t d_1_v0 = sdf_value(d_1, 0);
   fail_unless(sdf_kind(d_1_c) == S_COND);
   fail_unless(sdf_kind(d_1_v0) == S_RETAIN);

   // Retain values
   sdf_node_t rv_0 = sdf_value(d_1_v0, 0);
   fail_unless(sdf_kind(rv_0) == S_DELVAL);

   sdf_node_t rv_1 = sdf_value(d_1_v0, 1);
   fail_unless(sdf_kind(rv_1) == S_DELVAL);

   sdf_node_t rv_2 = sdf_value(d_1_v0, 2);
   fail_unless(sdf_kind(rv_2) == S_DELVAL);

   // Delay 2 - CONDELSE
   sdf_node_t d_3 = sdf_delay(c, 2);
   sdf_node_t d_3_c = sdf_cond(d_3, 0);
   fail_unless(sdf_has_expr(d_3_c) == false);
}
END_TEST

START_TEST(test_parse20)
{
   input_from_file(TESTDIR "/sdf/parse20.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);

   // Delay 0
   sdf_node_t d_0 = sdf_delay(c, 0);
   sdf_node_t d_0_p = sdf_signal(d_0, 0);

   fail_unless(sdf_subkind(d_0) == S_DELAY_KIND_PORT);
   fail_unless(sdf_kind(d_0_p) == S_SIGNAL);
   fail_unless(icmp(sdf_ident(d_0_p), "my_inst.x"));
   fail_unless(sdf_values(d_0) == 3);

   // Delay 1
   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_subkind(d_1) == S_DELAY_KIND_INTERCONNECT);
   fail_unless(sdf_signals(d_1) == 2);

   // Delay 2
   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_subkind(d_2) == S_DELAY_KIND_NETDELAY);
   fail_unless(sdf_signals(d_2) == 1);

   // Delay 3
   sdf_node_t d_3 = sdf_delay(c, 3);
   fail_unless(sdf_subkind(d_3) == S_DELAY_KIND_DEVICE);
   fail_unless(sdf_signals(d_3) == 0);

   // Delay 4
   sdf_node_t d_4 = sdf_delay(c, 4);
   fail_unless(sdf_subkind(d_4) == S_DELAY_KIND_DEVICE);
   fail_unless(sdf_signals(d_4) == 1);
}
END_TEST

START_TEST(test_parse21)
{
   input_from_file(TESTDIR "/sdf/parse21.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);

   // Timing check 0
   sdf_node_t tc_0 = sdf_tcheck(c, 0);
   sdf_node_t tc_0_p_0 = sdf_signal(tc_0, 0);
   sdf_node_t tc_0_p_1 = sdf_signal(tc_0, 1);
   fail_unless(sdf_kind(tc_0) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_0) == S_TCHECK_SETUP);
   fail_unless(sdf_kind(tc_0_p_0) == S_SIGNAL);
   fail_unless(sdf_kind(tc_0_p_1) == S_SIGNAL);
   fail_unless(icmp(sdf_ident(tc_0_p_0), "my_inst.x"));
   fail_unless(icmp(sdf_ident(tc_0_p_1), "my_inst.y"));
   fail_unless(sdf_values(tc_0) == 1);

   // Timing check 1
   sdf_node_t tc_1 = sdf_tcheck(c, 1);
   sdf_node_t tc_1_p_0 = sdf_signal(tc_1, 0);
   sdf_node_t tc_1_p_1 = sdf_signal(tc_1, 1);
   fail_unless(sdf_kind(tc_1) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_1) == S_TCHECK_HOLD);
   fail_unless(icmp(sdf_ident(tc_1_p_0), "my_inst.a"));
   fail_unless(icmp(sdf_ident(tc_1_p_1), "my_inst.b"));

   // Timing check 2
   sdf_node_t tc_2 = sdf_tcheck(c, 2);
   fail_unless(sdf_kind(tc_2) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_2) == S_TCHECK_SETUPHOLD);

   // Timing check 3
   sdf_node_t tc_3 = sdf_tcheck(c, 3);
   sdf_node_t tc_3_sc = sdf_cond(tc_3, 0);
   sdf_node_t tc_3_sc_e = sdf_expr(tc_3_sc);
   fail_unless(sdf_conds(tc_3) == 1);
   fail_unless(sdf_kind(tc_3_sc) == S_COND);
   fail_unless(sdf_subkind(tc_3_sc) == S_COND_SCOND);
   fail_unless(sdf_kind(tc_3_sc_e) == S_BINARY);
   fail_unless(sdf_subkind(tc_3_sc_e) == S_BINARY_EXPR_LOGEQ);

   // Timing check 4
   sdf_node_t tc_4 = sdf_tcheck(c, 4);
   sdf_node_t tc_4_sc = sdf_cond(tc_4, 0);
   sdf_node_t tc_4_sc_e = sdf_expr(tc_4_sc);
   sdf_node_t tc_4_cc = sdf_cond(tc_4, 1);
   sdf_node_t tc_4_cc_e = sdf_expr(tc_4_cc);
   fail_unless(sdf_conds(tc_4) == 2);
   fail_unless(sdf_kind(tc_4_sc) == S_COND);
   fail_unless(sdf_kind(tc_4_cc) == S_COND);
   fail_unless(sdf_subkind(tc_4_sc) == S_COND_SCOND);
   fail_unless(sdf_subkind(tc_4_cc) == S_COND_CCOND);
   fail_unless(sdf_kind(tc_4_sc_e) == S_BINARY);
   fail_unless(sdf_subkind(tc_4_sc_e) == S_BINARY_EXPR_LOGNEQ);
   fail_unless(icmp(sdf_ident(tc_4_sc), "\"AAA\""));
   fail_unless(sdf_kind(tc_4_cc_e) == S_BINARY);
   fail_unless(sdf_subkind(tc_4_cc_e) == S_BINARY_EXPR_CASEEQ);

   // Timing check 5
   sdf_node_t tc_5 = sdf_tcheck(c, 5);
   sdf_node_t tc_5_cc = sdf_cond(tc_5, 0);
   sdf_node_t tc_5_cc_e = sdf_expr(tc_5_cc);
   fail_unless(sdf_conds(tc_5) == 1);
   fail_unless(sdf_kind(tc_5_cc) == S_COND);
   fail_unless(sdf_subkind(tc_5_cc) == S_COND_CCOND);
   fail_unless(sdf_kind(tc_5_cc_e) == S_BINARY);
   fail_unless(sdf_subkind(tc_5_cc_e) == S_BINARY_EXPR_CASENEQ);
   fail_unless(icmp(sdf_ident(tc_5_cc), "\"BBB\""));

   // Timing check 6
   sdf_node_t tc_6 = sdf_tcheck(c, 6);
   sdf_node_t tc_6_sc = sdf_cond(tc_6, 0);
   sdf_node_t tc_6_sc_e = sdf_expr(tc_6_sc);
   sdf_node_t tc_6_cc = sdf_cond(tc_6, 1);
   sdf_node_t tc_6_cc_e = sdf_expr(tc_6_cc);
   fail_unless(sdf_conds(tc_6) == 2);
   fail_unless(sdf_kind(tc_6_sc) == S_COND);
   fail_unless(sdf_kind(tc_6_cc) == S_COND);
   fail_unless(sdf_subkind(tc_6_sc) == S_COND_SCOND);
   fail_unless(sdf_subkind(tc_6_cc) == S_COND_CCOND);
   fail_unless(sdf_kind(tc_6_sc_e) == S_UNARY);
   fail_unless(sdf_subkind(tc_6_sc_e) == S_UNARY_EXPR_LOGNOT);
   fail_unless(sdf_kind(tc_6_cc_e) == S_UNARY);
   fail_unless(sdf_subkind(tc_6_cc_e) == S_UNARY_EXPR_BITNOT);

   // Timing check 7
   sdf_node_t tc_7 = sdf_tcheck(c, 7);
   fail_unless(sdf_kind(tc_7) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_7) == S_TCHECK_RECOVERY);

   // Timing check 8
   sdf_node_t tc_8 = sdf_tcheck(c, 8);
   fail_unless(sdf_kind(tc_8) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_8) == S_TCHECK_REMOVAL);

   // Timing check 9
   sdf_node_t tc_9 = sdf_tcheck(c, 9);
   fail_unless(sdf_kind(tc_9) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_9) == S_TCHECK_RECREM);
}
END_TEST

START_TEST(test_parse22)
{
   input_from_file(TESTDIR "/sdf/parse22.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);

   // Timing check 0
   sdf_node_t tc_0 = sdf_tcheck(c, 0);
   sdf_node_t tc_0_p_0 = sdf_signal(tc_0, 0);
   sdf_node_t tc_0_p_1 = sdf_signal(tc_0, 1);
   sdf_node_t tc_0_p_0_c = sdf_cond(tc_0_p_0, 0);
   sdf_node_t tc_0_p_0_c_e = sdf_expr(tc_0_p_0_c);
   fail_unless(sdf_kind(tc_0) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_0) == S_TCHECK_RECREM);
   fail_unless(icmp(sdf_ident(tc_0_p_0_c), "\"COMMENT\""));
   fail_unless(sdf_kind(tc_0_p_0_c_e) == S_BINARY);
   fail_unless(sdf_subkind(tc_0_p_0_c_e) == S_BINARY_EXPR_LOGEQ);
   fail_unless(sdf_kind(tc_0_p_1) == S_SIGNAL);

   // Timing check 1
   sdf_node_t tc_1 = sdf_tcheck(c, 1);
   sdf_node_t tc_1_p_0 = sdf_signal(tc_1, 0);
   sdf_node_t tc_1_p_1 = sdf_signal(tc_1, 1);
   fail_unless(sdf_kind(tc_1) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_1) == S_TCHECK_SKEW);
   fail_unless(icmp(sdf_ident(tc_1_p_0), "y"));
   fail_unless(icmp(sdf_ident(tc_1_p_1), "z"));

   // Timing check 2
   sdf_node_t tc_2 = sdf_tcheck(c, 2);
   sdf_node_t tc_2_p_0 = sdf_signal(tc_2, 0);
   sdf_node_t tc_2_p_1 = sdf_signal(tc_2, 1);
   fail_unless(sdf_kind(tc_2) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_2) == S_TCHECK_BIDIRSKEW);
   fail_unless(icmp(sdf_ident(tc_2_p_0), "z"));
   fail_unless(icmp(sdf_ident(tc_2_p_1), "x"));

   // Timing check 3
   sdf_node_t tc_3 = sdf_tcheck(c, 3);
   sdf_node_t tc_3_p_0 = sdf_signal(tc_3, 0);
   sdf_node_t tc_3_p_0_c = sdf_cond(tc_3_p_0, 0);
   sdf_node_t tc_3_p_0_c_e = sdf_expr(tc_3_p_0_c);
   fail_unless(sdf_kind(tc_3) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_3) == S_TCHECK_WIDTH);
   fail_unless(icmp(sdf_ident(tc_3_p_0), "port_in"));
   fail_unless(icmp(sdf_ident(tc_3_p_0_c), "\"WTF\""));
   fail_unless(sdf_kind(tc_3_p_0_c_e) == S_UNARY);
   fail_unless(sdf_subkind(tc_3_p_0_c_e) == S_UNARY_EXPR_LOGNOT);

   // Timing check 4
   sdf_node_t tc_4 = sdf_tcheck(c, 4);
   sdf_node_t tc_4_p_0 = sdf_signal(tc_4, 0);
   sdf_node_t tc_4_p_0_c = sdf_cond(tc_4_p_0, 0);
   sdf_node_t tc_4_p_0_c_e = sdf_expr(tc_4_p_0_c);
   fail_unless(sdf_kind(tc_4) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_4) == S_TCHECK_PERIOD);
   fail_unless(icmp(sdf_ident(tc_4_p_0), "port_out"));
   fail_unless(icmp(sdf_ident(tc_4_p_0_c), "\"DEADBEEF\""));
   fail_unless(sdf_kind(tc_4_p_0_c_e) == S_UNARY);
   fail_unless(sdf_subkind(tc_4_p_0_c_e) == S_UNARY_EXPR_BITNOT);

   // Timing check 5
   sdf_node_t tc_5 = sdf_tcheck(c, 5);
   sdf_node_t tc_5_p_0 = sdf_signal(tc_5, 0);
   sdf_node_t tc_5_p_1 = sdf_signal(tc_5, 1);
   sdf_node_t tc_5_v0 = sdf_value(tc_5, 0);
   sdf_node_t tc_5_v1 = sdf_value(tc_5, 1);
   fail_unless(sdf_kind(tc_5) == S_TIMING_CHECK);
   fail_unless(sdf_subkind(tc_5) == S_TCHECK_NOCHANGE);
   fail_unless(icmp(sdf_ident(tc_5_p_0), "p_one"));
   fail_unless(icmp(sdf_ident(tc_5_p_1), "p_two"));
   fail_unless_floats_equal(sdf_dval(sdf_number(tc_5_v0)), -10);
   fail_unless_floats_equal(sdf_dval(sdf_number(tc_5_v1)), -15);
}
END_TEST

START_TEST(test_parse23)
{
   input_from_file(TESTDIR "/sdf/parse23.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   sdf_node_t s = file->root;

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);

   // Timing env 0 - Path constraint
   sdf_node_t te_0 = sdf_tenv(c, 0);
   fail_unless(sdf_kind(te_0) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_0) == S_CONSTR_KIND_PATH);
   fail_unless(icmp(sdf_ident(te_0), "\"Fancy constraint\""));
   fail_unless(sdf_signals(te_0) == 2);
   fail_unless(sdf_values(te_0) == 2);

   // Timing env 1 - Path constraint
   sdf_node_t te_1 = sdf_tenv(c, 1);
   fail_unless(sdf_kind(te_1) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_1) == S_CONSTR_KIND_PATH);
   fail_unless(sdf_signals(te_1) == 6);
   fail_unless(sdf_values(te_1) == 2);

   // Timing env 2 - Period constraint
   sdf_node_t te_2 = sdf_tenv(c, 2);
   fail_unless(sdf_kind(te_2) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_2) == S_CONSTR_KIND_PERIOD);
   fail_unless(sdf_signals(te_2) == 1);
   fail_unless(sdf_values(te_2) == 1);
   fail_unless(sdf_exceptions(te_2) == 0);

   // Timing env 3 - Period constraint
   sdf_node_t te_3 = sdf_tenv(c, 3);
   fail_unless(sdf_kind(te_3) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_3) == S_CONSTR_KIND_PERIOD);
   fail_unless(sdf_signals(te_3) == 1);
   fail_unless(sdf_values(te_3) == 1);
   fail_unless(sdf_exceptions(te_3) == 3);
   fail_unless(icmp(sdf_ident(sdf_exception(te_3, 0)), "top.my_inst"));
   fail_unless(icmp(sdf_ident(sdf_exception(te_3, 1)), "top.your_inst"));
   fail_unless(icmp(sdf_ident(sdf_exception(te_3, 2)), "*"));

   // Timing env 4 - Sum constraint
   sdf_node_t te_4 = sdf_tenv(c, 4);
   sdf_node_t te_4_s_0 = sdf_signal(te_4, 0);
   sdf_node_t te_4_s_1 = sdf_signal(te_4, 1);
   sdf_node_t te_4_s_0_p_0 = sdf_signal(te_4_s_0, 0);
   sdf_node_t te_4_s_0_p_1 = sdf_signal(te_4_s_0, 1);
   sdf_node_t te_4_s_1_p_0 = sdf_signal(te_4_s_1, 0);
   sdf_node_t te_4_s_1_p_1 = sdf_signal(te_4_s_1, 1);
   fail_unless(sdf_kind(te_4) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_4) == S_CONSTR_KIND_SUM);
   fail_unless(sdf_signals(te_4) == 2);
   fail_unless(sdf_values(te_4) == 1);
   fail_unless(sdf_kind(te_4_s_0) == S_CONSTR_PATH);
   fail_unless(icmp(sdf_ident(te_4_s_0_p_0), "top.gate_x1.a"));
   fail_unless(icmp(sdf_ident(te_4_s_0_p_1), "top.gate_x1.z"));
   fail_unless(sdf_kind(te_4_s_1) == S_CONSTR_PATH);
   fail_unless(icmp(sdf_ident(te_4_s_1_p_0), "top.gate_x2.b"));
   fail_unless(icmp(sdf_ident(te_4_s_1_p_1), "top.gate_x2.y"));

   // Timing env 5 - Sum constraint
   sdf_node_t te_5 = sdf_tenv(c, 5);
   sdf_node_t te_5_s_0 = sdf_signal(te_5, 0);
   sdf_node_t te_5_s_1 = sdf_signal(te_5, 1);
   sdf_node_t te_5_s_2 = sdf_signal(te_5, 1);
   sdf_node_t te_5_s_3 = sdf_signal(te_5, 1);
   fail_unless(sdf_kind(te_5) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_5) == S_CONSTR_KIND_SUM);
   fail_unless(sdf_signals(te_5) == 4);
   fail_unless(sdf_values(te_5) == 2);
   fail_unless(sdf_kind(te_5_s_0) == S_CONSTR_PATH);
   fail_unless(sdf_kind(te_5_s_1) == S_CONSTR_PATH);
   fail_unless(sdf_kind(te_5_s_2) == S_CONSTR_PATH);
   fail_unless(sdf_kind(te_5_s_3) == S_CONSTR_PATH);

   // Timing env 6 - Diff constraint
   sdf_node_t te_6 = sdf_tenv(c, 6);
   sdf_node_t te_6_s_0 = sdf_signal(te_6, 0);
   sdf_node_t te_6_s_1 = sdf_signal(te_6, 1);
   fail_unless(sdf_kind(te_6) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_6) == S_CONSTR_KIND_DIFF);
   fail_unless(sdf_signals(te_6) == 2);
   fail_unless(sdf_values(te_6) == 1);
   fail_unless(sdf_kind(te_6_s_0) == S_CONSTR_PATH);
   fail_unless(sdf_kind(te_6_s_1) == S_CONSTR_PATH);

   // Timing env 7 - Diff constraint
   sdf_node_t te_7 = sdf_tenv(c, 7);
   fail_unless(sdf_kind(te_7) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_7) == S_CONSTR_KIND_DIFF);
   fail_unless(sdf_signals(te_7) == 2);
   fail_unless(sdf_values(te_7) == 2);

   // Timing env 8 - Skew constraint
   sdf_node_t te_8 = sdf_tenv(c, 8);
   sdf_node_t te_8_s_0 = sdf_signal(te_8, 0);
   fail_unless(sdf_kind(te_8) == S_CONSTRAINT);
   fail_unless(sdf_subkind(te_8) == S_CONSTR_KIND_SKEW);
   fail_unless(sdf_signals(te_8) == 1);
   fail_unless(sdf_values(te_8) == 1);
   fail_unless(sdf_kind(te_8_s_0) ==  S_SIGNAL);
   fail_unless(sdf_flags(te_8_s_0) & S_F_POSEDGE);
}
END_TEST

START_TEST(test_parse24)
{
   input_from_file(TESTDIR "/sdf/parse24.sdf");

   const error_t expect[] = {
      { 8, "'rtripple' shall have at least one number specified"},
      { 12, "'tripple' shall have at least one number specified"},
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);

}
END_TEST

Suite *get_sdf_tests(void)
{
   Suite *s = suite_create("sdf");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_parse1);
   tcase_add_test(tc_core, test_parse2);
   tcase_add_test(tc_core, test_parse3);
   tcase_add_test(tc_core, test_parse4);
   tcase_add_test(tc_core, test_parse5);
   tcase_add_test(tc_core, test_parse6);
   tcase_add_test(tc_core, test_parse7);
   tcase_add_test(tc_core, test_parse8);
   tcase_add_test(tc_core, test_parse9);
   tcase_add_test(tc_core, test_parse10);
   tcase_add_test(tc_core, test_parse11);
   tcase_add_test(tc_core, test_parse12);
   tcase_add_test(tc_core, test_parse13);
   tcase_add_test(tc_core, test_parse14);
   tcase_add_test(tc_core, test_parse15);
   tcase_add_test(tc_core, test_parse16);
   tcase_add_test(tc_core, test_parse17);
   tcase_add_test(tc_core, test_parse18);
   tcase_add_test(tc_core, test_parse19);
   tcase_add_test(tc_core, test_parse20);
   tcase_add_test(tc_core, test_parse21);
   tcase_add_test(tc_core, test_parse22);
   tcase_add_test(tc_core, test_parse23);
   tcase_add_test(tc_core, test_parse24);
   suite_add_tcase(s, tc_core);

   return s;
}

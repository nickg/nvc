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

START_TEST(test_sdf1)
{
   input_from_file(TESTDIR "/sdf/parse1.sdf");

   sdf_node_t s = sdf_parse();

   fail_if(s == NULL);

   fail_unless(sdf_kind(s) == S_DELAY_FILE);
   fail_unless(sdf_subkind(s) == SDF_STD_4_0);

   sdf_node_t ver = sdf_decl(s, 0);
   fail_unless(sdf_kind(ver) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(ver) == S_HEADER_SDF_VESION);
}
END_TEST

START_TEST(test_sdf2)
{
   input_from_file(TESTDIR "/sdf/parse2.sdf");

   const error_t expect[] = {
      { 2, "Invalid SDF version: \"FOR_SURE_INVALID_SDF_VERSION\". "
           "SDF version shall contain one of: \"1.0\", \"2.0\", \"2.1\", \"3.0\" or \"4.0\"" },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_parse();
}
END_TEST

START_TEST(test_sdf3)
{
   input_from_file(TESTDIR "/sdf/parse3.sdf");

   sdf_node_t s = sdf_parse();

   fail_if(s == NULL);

   fail_unless(sdf_kind(s) == S_DELAY_FILE);
   fail_unless(sdf_subkind(s) == SDF_STD_1_0);

   fail_unless(sdf_decls(s) == 6);

   sdf_node_t sdf_version = sdf_decl(s, 0);
   fail_unless(sdf_kind(sdf_version) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(sdf_version) == S_HEADER_SDF_VESION);
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

START_TEST(test_sdf4)
{
   input_from_file(TESTDIR "/sdf/parse4.sdf");

   const error_t expect[] = {
      { 6, "Hierarchy separator: / different than hierarchy separator in SDF header: ." },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_node_t s = sdf_parse();

   fail_if(s == NULL);

   fail_unless(sdf_kind(s) == S_DELAY_FILE);
   fail_unless(sdf_subkind(s) == SDF_STD_2_1);

   fail_unless(sdf_decls(s) == 2);

   sdf_node_t sdf_version = sdf_decl(s, 0);
   fail_unless(sdf_kind(sdf_version) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(sdf_version) == S_HEADER_SDF_VESION);

   sdf_node_t design = sdf_decl(s, 1);
   fail_unless(sdf_kind(design) == S_HEADER_ITEM);
   fail_unless(sdf_subkind(design) == S_HEADER_DIVIDER);
   fail_unless(sdf_ident(design) == ident_new("."));
}
END_TEST

START_TEST(test_sdf5)
{
   input_from_file(TESTDIR "/sdf/parse5.sdf");

   sdf_node_t s = sdf_parse();

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

START_TEST(test_sdf6)
{
   input_from_file(TESTDIR "/sdf/parse6.sdf");

   sdf_node_t s = sdf_parse();

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

START_TEST(test_sdf7)
{
   input_from_file(TESTDIR "/sdf/parse7.sdf");

   sdf_node_t s = sdf_parse();

   fail_if(s == NULL);

   fail_unless(sdf_decls(s) == 1);
   fail_unless(sdf_cells(s) == 1);

   sdf_node_t cell = sdf_cell(s, 0);
   fail_unless(sdf_ident(cell) == ident_new("\"AND2\""));
   fail_unless(sdf_ident2(cell) == ident_new("*"));
}
END_TEST

START_TEST(test_sdf8)
{
   input_from_file(TESTDIR "/sdf/parse8.sdf");

   sdf_node_t s = sdf_parse();

   fail_if(s == NULL);

   fail_unless(sdf_decls(s) == 1);
   fail_unless(sdf_cells(s) == 1);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 6);


   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_kind(d_0)     == S_DELAY);
   fail_unless(sdf_subkind(d_0)  == S_DELAY_KIND_IOPATH);
   fail_unless(sdf_ports(d_0)    == 2);
   fail_unless(sdf_values(d_0)   == 1);
   fail_unless((sdf_flags(d_0) & S_F_VALUE_ABSOLUTE) > 0);

   sdf_node_t d_0_v = sdf_value(d_0, 0);
   fail_unless(sdf_kind(d_0_v)   == S_DELVAL);


   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_kind(d_1)     == S_DELAY);
   fail_unless(sdf_subkind(d_1)  == S_DELAY_KIND_PORT);
   fail_unless(sdf_ports(d_1)    == 1);
   fail_unless(sdf_values(d_1)   == 1);
   fail_unless((sdf_flags(d_1) & S_F_VALUE_ABSOLUTE) > 0);

   sdf_node_t d_1_v = sdf_value(d_1, 0);
   fail_unless(sdf_kind(d_1_v)   == S_DELVAL);


   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_kind(d_2)     == S_DELAY);
   fail_unless(sdf_subkind(d_2)  == S_DELAY_KIND_INTERCONNECT);
   fail_unless(sdf_ports(d_2)    == 2);
   fail_unless(sdf_values(d_2)   == 1);
   fail_unless((sdf_flags(d_2) & S_F_VALUE_ABSOLUTE) > 0);

   sdf_node_t d_2_v = sdf_value(d_2, 0);
   fail_unless(sdf_kind(d_2_v)   == S_DELVAL);


   sdf_node_t d_3 = sdf_delay(c, 3);
   fail_unless(sdf_kind(d_3)     == S_DELAY);
   fail_unless(sdf_subkind(d_3)  == S_DELAY_KIND_NETDELAY);
   fail_unless(sdf_ports(d_3)    == 1);
   fail_unless(sdf_values(d_3)   == 1);
   fail_unless((sdf_flags(d_3) & S_F_VALUE_INCREMENT) > 0);

   sdf_node_t d_3_v = sdf_value(d_3, 0);
   fail_unless(sdf_kind(d_3_v)   == S_DELVAL);


   sdf_node_t d_4 = sdf_delay(c, 4);
   fail_unless(sdf_kind(d_4)     == S_DELAY);
   fail_unless(sdf_subkind(d_4)  == S_DELAY_KIND_DEVICE);
   fail_unless(sdf_ports(d_4)    == 1);
   fail_unless(sdf_values(d_4)   == 1);
   fail_unless((sdf_flags(d_4) & S_F_VALUE_INCREMENT) > 0);

   sdf_node_t d_4_v = sdf_value(d_4, 0);
   fail_unless(sdf_kind(d_4_v)   == S_DELVAL);


   sdf_node_t d_5 = sdf_delay(c, 5);
   fail_unless(sdf_kind(d_5)     == S_DELAY);
   fail_unless(sdf_subkind(d_5)  == S_DELAY_KIND_DEVICE);
   fail_unless(sdf_ports(d_5)    == 0);
   fail_unless(sdf_values(d_5)   == 1);
   fail_unless((sdf_flags(d_5) & S_F_VALUE_INCREMENT) > 0);

   sdf_node_t d_5_v = sdf_value(d_5, 0);
   fail_unless(sdf_kind(d_5_v)   == S_DELVAL);
}
END_TEST

START_TEST(test_sdf9)
{
   input_from_file(TESTDIR "/sdf/parse9.sdf");

   sdf_node_t s = sdf_parse();

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 3);


   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_ports(d_0) == 2);
   sdf_node_t d_0_p_0 = sdf_port(d_0, 0);
   sdf_node_t d_0_p_1 = sdf_port(d_0, 1);
   fail_unless(sdf_kind(d_0_p_0)       == S_PORT);
   fail_unless(sdf_kind(d_0_p_1)       == S_PORT);
   fail_unless(sdf_subkind(d_0_p_0)    == S_PORT_SCALAR);
   fail_unless(sdf_subkind(d_0_p_1)    == S_PORT_SCALAR);
   fail_unless(sdf_ident(d_0_p_0)      == ident_new("a"));
   fail_unless(sdf_ident(d_0_p_1)      == ident_new("x"));

   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_ports(d_1) == 2);
   sdf_node_t d_1_p_0 = sdf_port(d_1, 0);
   sdf_node_t d_1_p_1 = sdf_port(d_1, 1);
   fail_unless(sdf_kind(d_1_p_0)       == S_PORT);
   fail_unless(sdf_kind(d_1_p_1)       == S_PORT);
   fail_unless(sdf_subkind(d_1_p_0)    == S_PORT_SCALAR);
   fail_unless(sdf_subkind(d_1_p_1)    == S_PORT_SCALAR);
   fail_unless(sdf_ident(d_1_p_0)      == ident_new("top.dut.b"));
   fail_unless(sdf_ident(d_1_p_1)      == ident_new("top.dut.x"));
   fail_unless(sdf_dims(d_1_p_0)       == 1);
   fail_unless(sdf_dims(d_1_p_1)       == 1);
   sdf_node_t d_1_min_p_0 = sdf_dim(d_1_p_0, 0);
   sdf_node_t d_1_min_p_1 = sdf_dim(d_1_p_1, 0);
   fail_unless(sdf_ival(d_1_min_p_0)   == 897);
   fail_unless(sdf_ival(d_1_min_p_1)   == 921);

   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_ports(d_2) == 2);
   sdf_node_t d_2_p_0 = sdf_port(d_2, 0);
   sdf_node_t d_2_p_1 = sdf_port(d_2, 1);
   fail_unless(sdf_kind(d_2_p_0)       == S_PORT);
   fail_unless(sdf_kind(d_2_p_1)       == S_PORT);
   fail_unless(sdf_subkind(d_2_p_0)    == S_PORT_BUS);
   fail_unless(sdf_subkind(d_2_p_1)    == S_PORT_BUS);
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

START_TEST(test_sdf10)
{
   input_from_file(TESTDIR "/sdf/parse10.sdf");

   sdf_node_t s = sdf_parse();

   fail_if(s == NULL);

   sdf_node_t c = sdf_cell(s, 0);
   fail_unless(sdf_delays(c) == 3);

   sdf_node_t d_0 = sdf_delay(c, 0);
   fail_unless(sdf_ports(d_0) == 2);
   sdf_node_t d_0_p_0 = sdf_port(d_0, 0);
   sdf_node_t d_0_p_1 = sdf_port(d_0, 1);
   fail_unless(sdf_kind(d_0_p_0)       == S_PORT);
   fail_unless(sdf_kind(d_0_p_1)       == S_PORT);
   fail_unless(sdf_subkind(d_0_p_0)    == S_PORT_SCALAR);
   fail_unless(sdf_subkind(d_0_p_1)    == S_PORT_SCALAR);
   fail_unless(sdf_ident(d_0_p_0)      == ident_new("clk_a"));
   fail_unless(sdf_ident(d_0_p_1)      == ident_new("x_q"));
   fail_unless(sdf_dims(d_0_p_0)       == 0);
   fail_unless(sdf_dims(d_0_p_1)       == 0);

   sdf_node_t d_1 = sdf_delay(c, 1);
   fail_unless(sdf_ports(d_1) == 2);
   sdf_node_t d_1_p_0 = sdf_port(d_1, 0);
   sdf_node_t d_1_p_1 = sdf_port(d_1, 1);
   fail_unless(sdf_kind(d_1_p_0)       == S_PORT);
   fail_unless(sdf_kind(d_1_p_1)       == S_PORT);
   fail_unless(sdf_subkind(d_1_p_0)    == S_PORT_SCALAR);
   fail_unless(sdf_subkind(d_1_p_1)    == S_PORT_SCALAR);
   fail_unless(sdf_ident(d_1_p_0)      == ident_new("clk_b"));
   fail_unless(sdf_ident(d_1_p_1)      == ident_new("y_q"));
   fail_unless(sdf_dims(d_1_p_0)       == 1);
   fail_unless(sdf_dims(d_1_p_1)       == 0);
   sdf_node_t d_1_min_p_0 = sdf_dim(d_1_p_0, 0);
   fail_unless(sdf_ival(d_1_min_p_0)   == 17);

   sdf_node_t d_2 = sdf_delay(c, 2);
   fail_unless(sdf_ports(d_2) == 2);
   sdf_node_t d_2_p_0 = sdf_port(d_2, 0);
   sdf_node_t d_2_p_1 = sdf_port(d_2, 1);
   fail_unless(sdf_kind(d_2_p_0)       == S_PORT);
   fail_unless(sdf_kind(d_2_p_1)       == S_PORT);
   fail_unless(sdf_subkind(d_2_p_0)    == S_PORT_BUS);
   fail_unless(sdf_subkind(d_2_p_1)    == S_PORT_BUS);
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

START_TEST(test_sdf11)
{
   input_from_file(TESTDIR "/sdf/parse11.sdf");

   sdf_node_t s = sdf_parse();

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

START_TEST(test_sdf12)
{
   input_from_file(TESTDIR "/sdf/parse12.sdf");

   const error_t expect[] = {
      { 9, "'delval_list' shall have at most 12 'delval' entries" },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_node_t s = sdf_parse();

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

START_TEST(test_sdf13)
{
   input_from_file(TESTDIR "/sdf/parse13.sdf");

   const error_t expect[] = {
      { 4, "Invalid header item order. SDF header items shall have strict order:\n"
	        "\tSDFVERSION, DESIGN, DATE, VENDOR, PROGRAM, VERSION, DIVIDER, VOLTAGE, PROCESS, TEMPERATURE, TIMESCALE" },
      { 7, "Duplicit header item" },
      { -1, NULL }
   };

   expect_errors(expect);

   sdf_node_t s = sdf_parse();

   fail_if(s == NULL);
}
END_TEST

START_TEST(test_sdf14)
{
   input_from_file(TESTDIR "/sdf/parse14.sdf");

   sdf_node_t s = sdf_parse();

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

Suite *get_sdf_tests(void)
{
   Suite *s = suite_create("sdf");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_sdf1);
   tcase_add_test(tc_core, test_sdf2);
   tcase_add_test(tc_core, test_sdf3);
   tcase_add_test(tc_core, test_sdf4);
   tcase_add_test(tc_core, test_sdf5);
   tcase_add_test(tc_core, test_sdf6);
   tcase_add_test(tc_core, test_sdf7);
   tcase_add_test(tc_core, test_sdf8);
   tcase_add_test(tc_core, test_sdf9);
   tcase_add_test(tc_core, test_sdf10);
   tcase_add_test(tc_core, test_sdf11);
   tcase_add_test(tc_core, test_sdf12);
   tcase_add_test(tc_core, test_sdf13);
   tcase_add_test(tc_core, test_sdf14);
   suite_add_tcase(s, tc_core);

   return s;
}

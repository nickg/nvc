//
//  Copyright (C) 2024  Nick Gasson
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

#include "vhpi_test.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static void add2(const vhpiCbDataT *cb_data_p)
{
   vhpiHandleT p0 = vhpi_handle_by_index(vhpiParamDecls, cb_data_p->obj, 0);
   check_error();
   fail_if(p0 == NULL);
   fail_unless(vhpi_get(vhpiKindP, p0) == vhpiConstParamDeclK);
   fail_unless(strcmp((char *)vhpi_get_str(vhpiNameP, p0), "X") == 0);

   vhpiValueT p0val = { .format = vhpiIntVal };
   fail_unless(vhpi_get_value(p0, &p0val) == 0);
   check_error();

   printf("add2 called p0=%p[%d]\n", p0, p0val.value.intg);

   vhpiValueT result = {
      .format = vhpiIntVal,
      .value = { .intg = p0val.value.intg + 2 }
   };
   vhpi_put_value(cb_data_p->obj, &result, vhpiDeposit);
   check_error();

   vhpi_release_handle(p0);
}

static void popcount(const vhpiCbDataT *cb_data_p)
{
   vhpiHandleT p0 = vhpi_handle_by_index(vhpiParamDecls, cb_data_p->obj, 0);
   check_error();
   fail_if(p0 == NULL);
   fail_unless(vhpi_get(vhpiKindP, p0) == vhpiConstParamDeclK);

   vhpiValueT arg = {
      .format    = vhpiBinStrVal,
      .bufSize   = 0,
      .value.str = NULL
   };
   int need = vhpi_get_value(p0, &arg);
   check_error();

   vhpi_printf("need %d bytes for p0 string", need);

   arg.value.str = malloc(need);
   arg.bufSize   = need;
   fail_unless(vhpi_get_value(p0, &arg) == 0);
   check_error();

   vhpi_printf("argument %s", arg.value.str);

   vhpiValueT result = {
      .format = vhpiIntVal,
      .value = { .intg = 0 }
   };

   for (int i = 0; i < need; i++) {
      if (arg.value.str[i] == '1')
         result.value.intg++;
   }

   vhpi_put_value(cb_data_p->obj, &result, vhpiDeposit);
   check_error();

   free(arg.value.str);
   vhpi_release_handle(p0);
}

static void test1(const vhpiCbDataT *cb_data_p)
{
   vhpiHandleT p0 = vhpi_handle_by_index(vhpiParamDecls, cb_data_p->obj, 0);
   check_error();
   fail_if(p0 == NULL);
   fail_unless(vhpi_get(vhpiKindP, p0) == vhpiVarParamDeclK);

   vhpiHandleT p1 = vhpi_handle_by_index(vhpiParamDecls, cb_data_p->obj, 1);
   check_error();
   fail_if(p1 == NULL);
   fail_unless(vhpi_get(vhpiKindP, p1) == vhpiConstParamDeclK);

   vhpiValueT p1_value = { .format = vhpiIntVal };
   fail_unless(vhpi_get_value(p1, &p1_value) == 0);
   check_error();

   vhpiValueT p0_value = {
      .format      = vhpiIntVecVal,
      .bufSize     = 0,
      .value.intgs = NULL
   };
   int need = vhpi_get_value(p0, &p0_value);
   check_error();

   vhpi_printf("need %d bytes for p0 array", need);

   p0_value.value.intgs = malloc(need * 2);
   p0_value.bufSize = need;
   fail_if(vhpi_get_value(p0, &p0_value) == -1);
   check_error();

   fail_unless(p0_value.numElems == 4);

   for (int i = 0; i < p0_value.numElems; i++) {
      vhpi_printf("[%d] = %d + %d\n", i, p0_value.value.intgs[i],
                  p1_value.value.intg);
      p0_value.value.intgs[i] += p1_value.value.intg;
   }

   vhpi_put_value(p0, &p0_value, vhpiDeposit);
   check_error();

   fail_unless(vhpi_put_value(cb_data_p->obj, &p0_value,
                              vhpiSizeConstraint) == 1);

   p0_value.bufSize = need * 2;   // Out of bounds
   fail_if(vhpi_put_value(p0, &p0_value, vhpiDeposit) == 0);

   free(p0_value.value.intgs);
   vhpi_release_handle(p0);
   vhpi_release_handle(p1);
}

static void iota(const vhpiCbDataT *cb_data_p)
{
   vhpiHandleT p0 = vhpi_handle_by_index(vhpiParamDecls, cb_data_p->obj, 0);
   check_error();
   fail_if(p0 == NULL);
   fail_unless(vhpi_get(vhpiKindP, p0) == vhpiConstParamDeclK);
   fail_unless(strcmp((char *)vhpi_get_str(vhpiNameP, p0), "N") == 0);

   vhpiValueT p0_value = { .format = vhpiIntVal };
   fail_unless(vhpi_get_value(p0, &p0_value) == 0);
   check_error();

   vhpiValueT result = {
      .format      = vhpiIntVecVal,
      .bufSize     = p0_value.value.intg * sizeof(vhpiIntT),
      .numElems    = p0_value.value.intg,
   };
   vhpi_put_value(cb_data_p->obj, &result, vhpiSizeConstraint);
   check_error();

   result.value.intgs = malloc(result.bufSize);
   for (int i = 0; i < p0_value.value.intg; i++)
      result.value.intgs[i] = i;

   vhpi_put_value(cb_data_p->obj, &result, vhpiDeposit);
   check_error();

   free(result.value.intgs);
   vhpi_release_handle(p0);
}

static void no_args(const vhpiCbDataT *cb_data_p)
{
   vhpi_printf("no_args called\n");

   vhpiHandleT it = vhpi_iterator(vhpiParamDecls, cb_data_p->obj);
   check_error();
   fail_if(it == NULL);
   fail_unless(vhpi_scan(it) == NULL);

   vhpi_release_handle(it);
}

static void set_logic(const vhpiCbDataT *cb_data_p)
{
   vhpiHandleT iter, var_param, dummy_param;
   iter = vhpi_iterator(vhpiParamDecls, cb_data_p->obj);
   var_param = vhpi_scan(iter); // -> first out argument "var"
   dummy_param = vhpi_scan(iter);
   vhpi_release_handle(iter);

   vhpiValueT value;
   value.format = vhpiLogicVal;
   value.value.enumv = vhpi1;
   vhpi_put_value(var_param, &value, vhpiDepositPropagate);
   check_error();

   vhpiErrorInfoT info;
   vhpi_put_value(dummy_param, &value, vhpiDepositPropagate);
   fail_unless(vhpi_check_error(&info));

   vhpi_release_handle(var_param);
   vhpi_release_handle(dummy_param);
}

void vhpi14_startup(void)
{
   vhpiForeignDataT add2_data = {
      .kind = vhpiFuncF,
      .libraryName = "lib",
      .modelName = "add2",
      .execf = add2,
   };
   vhpiHandleT h = vhpi_register_foreignf(&add2_data);
   check_error();

   vhpiForeignDataT popcount_data = {
      .kind = vhpiFuncF,
      .libraryName = "lib",
      .modelName = "popcount",
      .execf = popcount,
   };
   vhpi_register_foreignf(&popcount_data);
   check_error();

   vhpiForeignDataT test1_data = {
      .kind = vhpiFuncF,
      .libraryName = "lib",
      .modelName = "test1",
      .execf = test1,
   };
   vhpi_register_foreignf(&test1_data);
   check_error();

   vhpiForeignDataT test2_data = {
      .kind = vhpiFuncF,
      .libraryName = "lib",
      .modelName = "test2",
      .execf = test1,   // Can reuse same code
   };
   vhpi_register_foreignf(&test2_data);
   check_error();

   vhpiForeignDataT iota_data = {
      .kind = vhpiFuncF,
      .libraryName = "lib",
      .modelName = "iota",
      .execf = iota,
   };
   vhpi_register_foreignf(&iota_data);
   check_error();

   vhpiForeignDataT no_args_data = {
      .kind = vhpiProcF,
      .libraryName = "lib",
      .modelName = "no_args",
      .execf = no_args,
   };
   vhpi_register_foreignf(&no_args_data);
   check_error();

   vhpiForeignDataT set_logic_data = {
      .kind = vhpiProcF,
      .libraryName = "lib",
      .modelName = "set_logic",
      .execf = set_logic,
   };
   vhpi_register_foreignf(&set_logic_data);
   check_error();

   vhpiForeignDataT check;
   fail_if(vhpi_get_foreignf_info(h, &check));
   fail_unless(check.kind == vhpiFuncF);
   fail_unless(strcmp(check.modelName, "add2") == 0);
}

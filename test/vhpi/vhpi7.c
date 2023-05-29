#include "vhpi_test.h"

#include <stdio.h>
#include <string.h>

static vhpiHandleT m_outer[3];
static vhpiHandleT m_inner[3][2];

static vhpiHandleT n_outer[7][5];
static vhpiHandleT n_inner[7][5][3][2];

static vhpiHandleT o_outer[3];
static vhpiHandleT o_inner[3][2];

static void last_delta_2d(char *sig, vhpiHandleT inner[3][2])
{
   vhpiValueT val = {
      .format = vhpiIntVal,
   };

   for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 2; j++) {
         vhpi_get_value(inner[i][j], &val);
         check_error();
         vhpi_printf("%s(%d)(%d) = %.02x", sig, i, j, val.value.intg);
         fail_unless(val.value.intg == j * 16 + i);
      }
   }
}

static void last_delta(const vhpiCbDataT *cb_data)
{
   last_delta_2d("m", m_inner);

   vhpiValueT val = {
      .format = vhpiIntVal,
   };

   for (int i = 0; i < 7; i++) {
      for (int j = 0; j < 5; j++) {
         for (int k = 0; k < 3; k++) {
            for (int l = 0; l < 2; l++) {
               vhpi_get_value(n_inner[i][j][k][l], &val);
               check_error();
               vhpi_printf("n(%d,%d)(%d,%d) = %.04x", i, j, k, l, val.value.intg);
               fail_unless(val.value.intg == l * 4096 + k * 256 + j * 16 + i);
            }
         }
      }
   }

   last_delta_2d("o", o_inner);
}

static void start_of_sim_2d(char *sig, vhpiHandleT inner[3][2])
{
   vhpiValueT val = {
      .format = vhpiIntVal,
   };

   for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 2; j++) {
         vhpi_get_value(inner[i][j], &val);
         check_error();
         vhpi_printf("%s(%d)(%d) = %.02x", sig, i, j, val.value.intg);
         fail_unless(val.value.intg == i * 16 + j);

         val.value.intg = j * 16 + i;
         vhpi_put_value(inner[i][j], &val, vhpiDepositPropagate);
         check_error();
      }
   }
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiValueT val = {
      .format = vhpiIntVal,
   };

   start_of_sim_2d("m", m_inner);

   for (int i = 0; i < 7; i++) {
      for (int j = 0; j < 5; j++) {
         for (int k = 0; k < 3; k++) {
            for (int l = 0; l < 2; l++) {
               vhpi_get_value(n_inner[i][j][k][l], &val);
               check_error();

               vhpi_printf("n(%d,%d)(%d,%d) = %.04x", i, j, k, l, val.value.intg);
               fail_unless(val.value.intg == i * 4096 + j * 256 + k * 16 + l);

               val.value.intg = l * 4096 + k * 256 + j * 16 + i;
               vhpi_put_value(n_inner[i][j][k][l], &val, vhpiDepositPropagate);
               check_error();
            }
         }
      }
   }

   start_of_sim_2d("o", o_inner);
}

static void startup_2d(vhpiHandleT root, char *sig, vhpiHandleT outer[3],
                       vhpiHandleT inner[3][2])
{
   vhpiHandleT handle = vhpi_handle_by_name(sig, root);
   check_error();
   fail_if(handle == NULL);

   char name[64];
   for (int i = 0; i < 3; i++) {
      outer[i] = vhpi_handle_by_index(vhpiIndexedNames, handle, i);
      check_error();
      fail_if(outer[i] == NULL);

      fail_unless(vhpi_get(vhpiBaseIndexP, outer[i]) == i);

      snprintf(name, sizeof(name), "%s(%d)", sig, i);
      fail_if(strcmp(name, (char *)vhpi_get_str(vhpiNameP, outer[i])));

      for (int j = 0; j < 2; j++) {
         inner[i][j] = vhpi_handle_by_index(vhpiIndexedNames, outer[i], j);
         check_error();
         fail_if(inner[i][j] == NULL);

         fail_unless(vhpi_get(vhpiBaseIndexP, inner[i][j]) == j);

         snprintf(name, sizeof(name), "%s(%d)(%d)", sig, i, j);
         fail_if(strcmp(name, (char *)vhpi_get_str(vhpiNameP, inner[i][j])));
      }
   }

   vhpi_release_handle(handle);
}

void vhpi7_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   vhpi_register_cb(&cb_data, 0);
   check_error();

   cb_data.reason = vhpiCbLastKnownDeltaCycle;
   cb_data.cb_rtn = last_delta;
   vhpi_register_cb(&cb_data, 0);
   check_error();

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);

   startup_2d(root, "M", m_outer, m_inner);

   vhpiHandleT n = vhpi_handle_by_name("n", root);
   check_error();
   fail_if(n == NULL);

   char name[64];
   for (int i = 0; i < 7; i++) {
      for (int j = 0; j < 5; j++) {
         n_outer[i][j] = vhpi_handle_by_index(vhpiIndexedNames, n, i * 5 + j);
         check_error();
         fail_if(n_outer[i][j] == NULL);

         fail_unless(vhpi_get(vhpiBaseIndexP, n_outer[i][j]) == i * 5 + j);

         snprintf(name, sizeof(name), "N(%d,%d)", i, j);
         fail_if(strcmp(name, (char *)vhpi_get_str(vhpiNameP, n_outer[i][j])));

         for (int k = 0; k < 3; k++) {
            for (int l = 0; l < 2; l++) {
               n_inner[i][j][k][l] =
                  vhpi_handle_by_index(vhpiIndexedNames, n_outer[i][j], k * 2 + l);
               check_error();
               fail_if(n_inner[i][j][k][l] == NULL);

               fail_unless(vhpi_get(vhpiBaseIndexP, n_inner[i][j][k][l]) == k * 2 + l);

               snprintf(name, sizeof(name), "N(%d,%d)(%d,%d)", i, j, k, l);
               fail_if(strcmp(name, (char *)vhpi_get_str(vhpiNameP, n_inner[i][j][k][l])));
            }
         }
      }
   }

   vhpi_release_handle(n);

   startup_2d(root, "O", o_outer, o_inner);

   vhpi_release_handle(root);
}

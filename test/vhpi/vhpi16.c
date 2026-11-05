#include "vhpi_test.h"

#include <string.h>
#include <stdbool.h>

static void x_value_change(const vhpiCbDataT *cb_data)
{
   const vhpiCharT *name =
      VHPI_CHECK(vhpi_get_str(vhpiFullNameP, cb_data->obj));
   vhpi_printf("%s value change", name);

   // VHPI spec says releasing the passed-in handle is erroneous
   vhpi_release_handle(cb_data->obj);

   vhpiErrorInfoT info;
   fail_unless(vhpi_check_error(&info));
}

// x is 1, 2 and 3 in turn: the tool fills in its own copy of the value
// structure, as the one passed to vhpi_register_cb was on the stack
static int expect = 1;
static int expect_str = 1;   // Index into expected values of v

static void x_value_change_with_value(const vhpiCbDataT *cb_data)
{
   fail_if(cb_data->value == NULL);
   fail_unless(cb_data->value->format == vhpiIntVal);
   fail_unless(cb_data->value->value.intg == expect);
   fail_if(cb_data->time == NULL);
   fail_unless(cb_data->time->low == expect * 1000000);
   vhpi_printf("x is %d", cb_data->value->value.intg);
   expect++;
}

static void v_value_change_with_str(const vhpiCbDataT *cb_data)
{
   // The tool must allocate the buffer for a string value itself
   static const char *expect_v[] = { "0001", "0010", "0011" };

   fail_if(cb_data->value == NULL);
   fail_unless(cb_data->value->format == vhpiStrVal);
   fail_if(cb_data->value->value.str == NULL);
   fail_unless(expect_str >= 1 && expect_str <= 3);
   fail_unless(strcmp((char *)cb_data->value->value.str,
                      expect_v[expect_str - 1]) == 0);
   vhpi_printf("v is %s", cb_data->value->value.str);
   expect_str++;
}

static void end_of_sim(const vhpiCbDataT *cb_data)
{
   fail_unless(expect == 4);
   fail_unless(expect_str == 4);
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   vhpiHandleT x = VHPI_CHECK(vhpi_handle_by_name("x", root));
   vhpiHandleT v = VHPI_CHECK(vhpi_handle_by_name("v", root));

   vhpiCbDataT x_cb_data = {
      .obj = x,
      .cb_rtn = x_value_change,
      .reason = vhpiCbValueChange,
   };
   VHPI_CHECK(vhpi_register_cb(&x_cb_data, 0));

   // LRM 08 section 23.29: the tool does not retain references to
   // the callback data structure or storage pointed to from within it
   {
      vhpiValueT value = { .format = vhpiObjTypeVal };
      vhpiTimeT time = { 0, 0 };
      vhpiCbDataT x_value_cb_data = {
         .obj = x,
         .cb_rtn = x_value_change_with_value,
         .reason = vhpiCbValueChange,
         .value = &value,
         .time = &time,
      };
      VHPI_CHECK(vhpi_register_cb(&x_value_cb_data, 0));

      memset(&value, 0xff, sizeof(value));
      memset(&time, 0xff, sizeof(time));
      memset(&x_value_cb_data, 0xff, sizeof(x_value_cb_data));
   }

   {
      vhpiValueT value = { .format = vhpiStrVal };
      vhpiCbDataT v_str_cb_data = {
         .obj = v,
         .cb_rtn = v_value_change_with_str,
         .reason = vhpiCbValueChange,
         .value = &value,
      };
      VHPI_CHECK(vhpi_register_cb(&v_str_cb_data, 0));

      memset(&value, 0xff, sizeof(value));
      memset(&v_str_cb_data, 0xff, sizeof(v_str_cb_data));
   }

   vhpiCbDataT eos_cb_data = {
      .cb_rtn = end_of_sim,
      .reason = vhpiCbEndOfSimulation,
   };
   VHPI_CHECK(vhpi_register_cb(&eos_cb_data, 0));

   vhpi_release_handle(root);

   // This should not invalidate the handle passed to the callback
   vhpi_release_handle(x);
   vhpi_release_handle(v);
}

void vhpi16_startup(void)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   fail_unless(root == NULL);

   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}

#include "vhpi_test.h"

#include <stdlib.h>

static int phase = 0;

static void start_of_analysis(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start of analysis");
   fail_unless(phase++ == 0);
}

static void end_of_analysis(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end of analysis");
   fail_unless(phase++ == 1);
}

static void end_of_tool(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end of tool");
   fail_unless(phase++ == 2);
}

static void exit_handler(void)
{
   fail_unless(phase == 3);
}

void vhpi17_startup(void)
{
   vhpiHandleT tool = VHPI_CHECK(vhpi_handle(vhpiTool, NULL));
   fail_if(tool == NULL);

   vhpiCapabibilityT caps = VHPI_CHECK(vhpi_get(vhpiCapabilitiesP, tool));
   fail_unless(caps & vhpiProvidesPostAnalysis);

   VHPI_CHECK(vhpi_release_handle(tool));

   vhpiCbDataT cb_data = {
      .reason = vhpiCbEndOfAnalysis,
      .cb_rtn = end_of_analysis,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));

   cb_data.reason = vhpiCbStartOfAnalysis;
   cb_data.cb_rtn = start_of_analysis;
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));

   cb_data.reason = vhpiCbEndOfTool;
   cb_data.cb_rtn = end_of_tool;
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));

   atexit(exit_handler);
}

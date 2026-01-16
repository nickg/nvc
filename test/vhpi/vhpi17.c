#include "vhpi_test.h"

#include <stdlib.h>

static int phase = 0;

static void start_of_analysis(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start of analysis");
   fail_unless(phase++ == 0);

   vhpiPhaseT phase = VHPI_CHECK(vhpi_get(vhpiPhaseP, NULL));
   fail_unless(phase == vhpiAnalysisPhase);
}

static void end_of_analysis(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end of analysis");
   fail_unless(phase++ == 1);

   vhpiPhaseT phase = VHPI_CHECK(vhpi_get(vhpiPhaseP, NULL));
   fail_unless(phase == vhpiAnalysisPhase);
}

static void start_of_elaboration(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start of elaboration");
   fail_unless(phase++ == 2);

   vhpiPhaseT phase = VHPI_CHECK(vhpi_get(vhpiPhaseP, NULL));
   fail_unless(phase == vhpiElaborationPhase);
}

static void end_of_elaboration(const vhpiCbDataT *cb_data)
{
   vhpi_printf("enf of analysis");
   fail_unless(phase++ == 3);

   vhpiPhaseT phase = VHPI_CHECK(vhpi_get(vhpiPhaseP, NULL));
   fail_unless(phase == vhpiElaborationPhase);
}

static void start_of_simulation(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start of simulation");
   fail_unless(phase++ == 4);

   vhpiPhaseT phase = VHPI_CHECK(vhpi_get(vhpiPhaseP, NULL));
   fail_unless(phase == vhpiSimulationPhase);
}

static void end_of_simulation(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end of simulation");
   fail_unless(phase++ == 5);

   vhpiPhaseT phase = VHPI_CHECK(vhpi_get(vhpiPhaseP, NULL));
   fail_unless(phase == vhpiSimulationPhase);
}

static void end_of_tool(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end of tool");
   fail_unless(phase++ == 6);

   vhpiPhaseT phase = VHPI_CHECK(vhpi_get(vhpiPhaseP, NULL));
   fail_unless(phase == vhpiTerminationPhase);
}

static void exit_handler(void)
{
   fail_unless(phase == 7);
}

void vhpi17_startup(void)
{
   vhpiPhaseT phase = VHPI_CHECK(vhpi_get(vhpiPhaseP, NULL));
   fail_unless(phase == vhpiRegistrationPhase);

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

   cb_data.reason = vhpiCbStartOfElaboration;
   cb_data.cb_rtn = start_of_elaboration;
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));

   cb_data.reason = vhpiCbEndOfElaboration;
   cb_data.cb_rtn = end_of_elaboration;
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));

   cb_data.reason = vhpiCbStartOfSimulation;
   cb_data.cb_rtn = start_of_simulation;
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));

   cb_data.reason = vhpiCbEndOfSimulation;
   cb_data.cb_rtn = end_of_simulation;
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));

   cb_data.reason = vhpiCbEndOfTool;
   cb_data.cb_rtn = end_of_tool;
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));

   atexit(exit_handler);
}

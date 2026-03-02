#include "vhpi_test.h"

#include <string.h>
#include <stdlib.h>

static int64_t phys_to_i64(vhpiPhysT phys)
{
   return ((uint64_t)phys.high) << 32 | phys.low;
}

static void end_of_analysis(const vhpiCbDataT *cb_data)
{
   // TODO: use @work.types in post analysis callback
   vhpiHandleT pack = VHPI_CHECK(vhpi_handle_by_name(":types", NULL));
   fail_unless(vhpi_get(vhpiKindP, pack) == vhpiPackInstK);

   vhpiHandleT freq = VHPI_CHECK(vhpi_handle_by_name("t_freq", pack));
   fail_unless(vhpi_get(vhpiKindP, freq) == vhpiPhysTypeDeclK);

   vhpiHandleT units = VHPI_CHECK(vhpi_iterator(vhpiUnitDecls, freq));

   vhpiHandleT hz = VHPI_CHECK(vhpi_scan(units));
   fail_if(hz == NULL);
   fail_unless(vhpi_get(vhpiKindP, hz) == vhpiUnitDeclK);
   fail_unless(vhpi_compare_handles(freq, vhpi_handle(vhpiType, hz)));

   vhpiPhysT hz_pos = VHPI_CHECK(vhpi_get_phys(vhpiPhysPositionP, hz));
   fail_unless(phys_to_i64(hz_pos) == 1);

   vhpi_release_handle(hz);

   vhpiHandleT khz = VHPI_CHECK(vhpi_scan(units));
   fail_if(khz == NULL);
   fail_unless(vhpi_get(vhpiKindP, khz) == vhpiUnitDeclK);
   fail_unless(vhpi_compare_handles(freq, vhpi_handle(vhpiType, khz)));

   vhpiPhysT khz_pos = VHPI_CHECK(vhpi_get_phys(vhpiPhysPositionP, khz));
   fail_unless(phys_to_i64(khz_pos) == 1000);

   vhpi_release_handle(khz);

   vhpi_release_handle(freq);
   vhpi_release_handle(pack);
}

void vhpi19_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,  // XXX: use EndOfAnalysis
      .cb_rtn = end_of_analysis,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}

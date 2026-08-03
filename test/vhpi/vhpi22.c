#include "vhpi_test.h"

#include <stdarg.h>

// A port or signal whose index constraint is not statically foldable, for
// example because it is derived from an array generic, must still report
// itself as constrained through vhpiConstraints.  The bounds are only known
// once the object has been elaborated but the subtype is not unconstrained.

static void check_range(vhpiHandleT scope, const char *name, int ndims, ...)
{
   vhpiHandleT obj = VHPI_CHECK(vhpi_handle_by_name(name, scope));
   check_handle(obj);

   vhpiHandleT type = VHPI_CHECK(vhpi_handle(vhpiType, obj));
   check_handle(type);
   fail_unless(vhpi_get(vhpiKindP, type) == vhpiSubtypeDeclK);
   fail_if(vhpi_get(vhpiIsUnconstrainedP, type));

   vhpiHandleT constraints = VHPI_CHECK(vhpi_iterator(vhpiConstraints, type));
   check_handle(constraints);

   va_list ap;
   va_start(ap, ndims);

   for (int i = 0; i < ndims; i++) {
      const int left = va_arg(ap, int), right = va_arg(ap, int);

      vhpiHandleT range = VHPI_CHECK(vhpi_scan(constraints));
      check_handle(range);
      fail_unless(vhpi_get(vhpiKindP, range) == vhpiIntRangeK);

      // This is the property cocotb consults before reading the bounds
      fail_if(vhpi_get(vhpiIsUnconstrainedP, range));

      fail_unless(vhpi_get(vhpiLeftBoundP, range) == left);
      fail_unless(vhpi_get(vhpiRightBoundP, range) == right);
      fail_if(vhpi_get(vhpiIsUpP, range));
      fail_if(vhpi_get(vhpiIsNullP, range));
      fail_unless(vhpi_get(vhpiIsDiscreteP, range));
      check_error();

      vhpi_printf("%s dimension %d range %d downto %d", name, i, left, right);

      vhpi_release_handle(range);
   }

   va_end(ap);

   fail_unless(vhpi_scan(constraints) == NULL);

   vhpi_release_handle(type);
   vhpi_release_handle(obj);
}

// The converse must still hold: a subtype that really has no index constraint
// reports itself, and its constraints, as unconstrained even though the
// bounds can be read back from the elaborated object.
static void check_unconstrained(vhpiHandleT scope, const char *name,
                                int left, int right)
{
   vhpiHandleT obj = VHPI_CHECK(vhpi_handle_by_name(name, scope));
   check_handle(obj);

   vhpiHandleT type = VHPI_CHECK(vhpi_handle(vhpiType, obj));
   check_handle(type);
   fail_unless(vhpi_get(vhpiKindP, type) == vhpiSubtypeDeclK);
   fail_unless(vhpi_get(vhpiIsUnconstrainedP, type));

   vhpiHandleT constraints = VHPI_CHECK(vhpi_iterator(vhpiConstraints, type));
   check_handle(constraints);

   vhpiHandleT range = VHPI_CHECK(vhpi_scan(constraints));
   check_handle(range);
   fail_unless(vhpi_get(vhpiIsUnconstrainedP, range));

   fail_unless(vhpi_get(vhpiLeftBoundP, range) == left);
   fail_unless(vhpi_get(vhpiRightBoundP, range) == right);
   check_error();

   vhpi_printf("%s unconstrained range %d to %d", name, left, right);

   fail_unless(vhpi_scan(constraints) == NULL);

   vhpi_release_handle(range);
   vhpi_release_handle(type);
   vhpi_release_handle(obj);
}

static void start_of_simulation(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));
   check_handle(root);

   // Signals in the top-level architecture
   check_range(root, "p_func", 1, 4, 0);
   check_range(root, "p_len", 1, 3, 0);
   check_range(root, "p_index", 1, 0, 0);
   check_range(root, "p_scalar", 1, 3, 0);
   check_range(root, "p_lit", 1, 3, 0);
   check_range(root, "p_2d", 2, 4, 0, 3, 0);

   // Ports of the instantiated entity, where the bounds come from the
   // actual generic values
   vhpiHandleT sub = VHPI_CHECK(vhpi_handle_by_name("u", root));
   check_handle(sub);

   check_range(sub, "p_func", 1, 4, 0);
   check_range(sub, "p_len", 1, 3, 0);
   check_range(sub, "p_index", 1, 0, 0);
   check_range(sub, "p_scalar", 1, 3, 0);
   check_range(sub, "p_lit", 1, 3, 0);
   check_range(sub, "p_2d", 2, 4, 0, 3, 0);

   // The array generic itself has an unconstrained subtype
   check_unconstrained(sub, "WIDTHS", 0, 3);

   vhpi_release_handle(sub);
   vhpi_release_handle(root);

   VHPI_CHECK(vhpi_control(vhpiFinish));
}

void vhpi22_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_simulation,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}

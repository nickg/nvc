set -xe

pwd
which nvc

# VPI vpi_handle_by_name round-trip test.
#
# Write a small Verilog design and a C VPI module.  The VPI module
# registers a system task ($vlog85_check) that calls vpi_handle_by_name
# with the canonical dotted path for a leaf instance, then verifies
# vpi_get_str(vpiFullName, ...) returns the same string.
#
# This locks the resolver/VPI/%m name encoding together: whatever
# string %m produces must be the same string vpi_handle_by_name accepts
# and vpiFullName returns.
#
# NOTE: nvc has no existing VPI .sh test infrastructure in test/regress/.
# The --load=PLUGIN global option loads a VHPI/VPI shared library.
# The C file is compiled against nvc's vpi_user.h.  If the build
# environment does not support this (missing headers/flags), the test
# will fail at the compile step — that is intentional, as it surfaces
# the missing VPI test infrastructure.
#
# TODO: The exact compiler flags for building a VPI plugin against nvc
# may need adjustment.  Check `nvc --install` or the nvc manual for
# the canonical way to compile VPI modules.  The include path below
# assumes the nvc source tree layout; a packaged install would use
# $(pkg-config --cflags nvc) or similar.

NVC_SRCDIR=${NVC_SRCDIR:-$TESTDIR/..}

cat >vlog85.v <<'EOF'
module leaf;
    reg [3:0] val = 4'b1100;
endmodule

module mid;
    leaf inner ();
endmodule

module vlog85;
    mid child ();

    initial begin
        $vlog85_check;
        $finish;
    end
endmodule
EOF

cat >vlog85_vpi.c <<'EOF'
#include "vpi_user.h"
#include <string.h>
#include <stdio.h>

static PLI_INT32 vlog85_check_calltf(PLI_BYTE8 *user_data)
{
    vpiHandle h;
    const char *name;

    /* Look up the leaf instance by its canonical dotted path */
    h = vpi_handle_by_name("vlog85.child.inner", NULL);
    if (h == NULL) {
        vpi_printf("FAILED: vpi_handle_by_name returned NULL\n");
        vpi_control(vpiFinish, 1);
        return 0;
    }

    name = vpi_get_str(vpiFullName, h);
    if (name == NULL) {
        vpi_printf("FAILED: vpiFullName returned NULL\n");
        vpi_control(vpiFinish, 1);
        return 0;
    }

    if (strcmp(name, "vlog85.child.inner") != 0) {
        vpi_printf("FAILED: vpiFullName='%s' expected 'vlog85.child.inner'\n",
                   name);
        vpi_control(vpiFinish, 1);
        return 0;
    }

    vpi_printf("PASSED\n");
    return 0;
}

static void vlog85_register(void)
{
    s_vpi_systf_data tf_data;
    memset(&tf_data, 0, sizeof(tf_data));
    tf_data.type      = vpiSysTask;
    tf_data.tfname    = "$vlog85_check";
    tf_data.calltf    = vlog85_check_calltf;
    vpi_register_systf(&tf_data);
}

void (*vlog_startup_routines[])(void) = {
    vlog85_register,
    NULL
};
EOF

# Compile the VPI module.
# TODO: Adjust include path and link flags if nvc installs headers
# elsewhere or requires specific link flags for VPI plugins.
VPI_INCLUDE=${VPI_INCLUDE:-$NVC_SRCDIR/src/vpi}
cc -shared -fPIC -I"$VPI_INCLUDE" -o vlog85_vpi.so vlog85_vpi.c

nvc -a vlog85.v
nvc --load=./vlog85_vpi.so -e vlog85 --no-save -r >stdout 2>stderr

grep PASSED stdout

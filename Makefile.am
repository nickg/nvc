EXTRA_DIST = autogen.sh nvc.1 README.md NEWS.md THANKS.md

ACLOCAL_AMFLAGS = -I m4

man_MANS = nvc.1

AM_CPPFLAGS = -I$(top_srcdir)/thirdparty -I$(top_srcdir)/src
AM_CFLAGS   = -Wall $(EXTRA_CFLAGS) $(LTO_FLAG)
AM_LDFLAGS  = $(EXTRA_LDFLAGS) $(LLVM_LDFLAGS)

AM_CC       = $(PTHREAD_CC)
AM_CFLAGS  += $(PTHREAD_CFLAGS)
AM_LDFLAGS += $(PTHREAD_LIBS)

AM_CFLAGS += $(check_CFLAGS) $(libdw_CFLAGS)

bin_PROGRAMS =
noinst_LIBRARIES =
include_HEADERS =
check_PROGRAMS =
check_LIBRARIES =
noinst_PROGRAMS =
pkgdata_DATA =
CLEANFILES =
DISTCLEANFILES =
BOOTSTRAPLIBS =
LIBDIRS =
lib_LIBRARIES =

include thirdparty/Makemodule.am
include src/Makemodule.am
include src/rt/Makemodule.am
include src/vhpi/Makemodule.am
include test/Makemodule.am
include test/vhpi/Makemodule.am
include lib/Makemodule.am
include lib/std/Makemodule.am
include lib/std.08/Makemodule.am
include lib/nvc/Makemodule.am
include lib/nvc.08/Makemodule.am
include lib/ieee/Makemodule.am
include lib/ieee.08/Makemodule.am
include lib/vital/Makemodule.am
include lib/synopsys/Makemodule.am
include tools/Makemodule.am

compile-commands:
	$(MAKE) -C $(top_builddir) clean
	bear -- $(MAKE) -j -C $(top_builddir) $(bin_PROGRAMS) $(check_PROGRAMS)

clean-local: clean-test clean-libs

.PHONY: bootstrap cov-reset cov-report clean-libs clean-test compile-commands

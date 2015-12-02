EXTRA_DIST = autogen.sh data/coverage.css nvc.1

ACLOCAL_AMFLAGS = -I m4

pkgdata_DATA = data/coverage.css

man_MANS = nvc.1

AM_CPPFLAGS = -I$(top_srcdir)/thirdparty -I$(top_srcdir)/src
AM_CFLAGS   = -Wall $(WERROR_CFLAGS) $(COV_CFLAGS) $(CHECK_CFLAGS)
AM_LDFLAGS  = $(RDYNAMIC_FLAG) $(COV_LDFLAGS)

if HAVE_LLVM
AM_LDFLAGS += $(LLVM_LDFLAGS)
endif

if FST_WRITER_PARALLEL
AM_CC       = $(PTHREAD_CC)
AM_CFLAGS  += $(PTHREAD_CFLAFGS)
AM_LDFLAGS += $(PTHREAD_LIBS)
endif

bin_PROGRAMS =
noinst_LIBRARIES =
include_HEADERS =
check_PROGRAMS =
check_LIBRARIES =
CLEANFILES =
LIBDIRS =

include thirdparty/Makemodule.am
include src/Makemodule.am
if HAVE_LLVM
include src/llvm/Makemodule.am
endif
include src/rt/Makemodule.am
include src/vhpi/Makemodule.am
include test/Makemodule.am
include test/vhpi/Makemodule.am
include lib/Makemodule.am
include lib/std/Makemodule.am
include lib/ieee/Makemodule.am
include lib/nvc/Makemodule.am
include lib/synopsys/Makemodule.am

if HAVE_RONN
nvc.1: nvc.1.md
	$(AM_V_GEN)ronn --roff $(top_srcdir)/nvc.1.md --manual="NVC Manual"
endif

clean-local: clean-libs clean-test

.PHONY: bootstrap cov-reset cov-report clean-libs clean-test

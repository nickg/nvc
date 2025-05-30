EXTRA_DIST = autogen.sh nvc.1 README.md NEWS.md THANKS.md

ACLOCAL_AMFLAGS = -I m4

man_MANS = nvc.1

AM_CPPFLAGS = -I$(top_srcdir)/thirdparty -I$(top_srcdir)/src
AM_CFLAGS   = -Wall $(EXTRA_CFLAGS) $(LTO_FLAG)
AM_CXXFLAGS = -Wall $(EXTRA_CFLAGS) $(LTO_FLAG)
AM_LDFLAGS  = $(EXTRA_LDFLAGS) $(LLVM_LDFLAGS)
AM_YFLAGS   = --locations $(EXTRA_YFLAGS)
AM_LFLAGS   = -o lex.yy.c

AM_CC       = $(PTHREAD_CC)
AM_CFLAGS   += $(PTHREAD_CFLAGS)
AM_LDFLAGS  += $(PTHREAD_LIBS)

AM_CFLAGS += \
	$(check_CFLAGS) \
	$(libdw_CFLAGS) \
	$(libdwarf_CFLAGS) \
	$(libffi_CFLAGS) \
	$(zlib_CFLAGS) \
	$(capstone_CFLAGS) \
	$(LLVM_CFLAGS) \
	$(libzstd_CFLAGS) \
	$(TCL_CFLAGS)

bin_PROGRAMS =
noinst_LIBRARIES =
include_HEADERS =
check_PROGRAMS =
check_LIBRARIES =
noinst_PROGRAMS =
pkgdata_DATA =
pkglibexec_PROGRAMS =
pkglibexec_SCRIPTS =
EXTRA_PROGRAMS =
BUILT_SOURCES =
CLEANFILES =
DISTCLEANFILES =
BOOTSTRAPLIBS =
LIBDIRS =
lib_LIBRARIES =
pkglib_LIBRARIES =

include thirdparty/Makemodule.am
include src/Makemodule.am
include src/cov/Makemodule.am
include src/jit/Makemodule.am
include src/mir/Makemodule.am
include src/psl/Makemodule.am
include src/rt/Makemodule.am
include src/sdf/Makemodule.am
include src/vhdl/Makemodule.am
include src/vhpi/Makemodule.am
include src/vlog/Makemodule.am
include src/vpi/Makemodule.am
include test/Makemodule.am
include test/vhpi/Makemodule.am
include lib/Makemodule.am
include lib/std/Makemodule.am
include lib/std.08/Makemodule.am
include lib/std.19/Makemodule.am
include lib/nvc/Makemodule.am
include lib/nvc.08/Makemodule.am
include lib/nvc.19/Makemodule.am
include lib/ieee/Makemodule.am
include lib/ieee.08/Makemodule.am
include lib/ieee.19/Makemodule.am
include lib/vital/Makemodule.am
include lib/synopsys/Makemodule.am
include contrib/Makemodule.am
include contrib/gui/Makemodule.am
include contrib/msi/Makemodule.am

clean-local: clean-libs
	rm -f src/vlog/vlog-parse.c
	-test ! -d logs || rm -r logs

everything: $(BUILT_SOURCES) config.h
	$(MAKE) $(AM_MAKEFLAGS) all-am $(check_PROGRAMS) $(EXTRA_PROGRAMS)

if MAINTAINER_MODE

compile-commands:
	$(MAKE) -C $(top_builddir) clean
	bear -- $(MAKE) -k -j$$(nproc) -C $(top_builddir) everything

release: update-test-dist
	echo $(PACKAGE_VERSION) | grep -E "^[[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+$$"
	$(AM_V_GEN)( \
	  v=$(PACKAGE_VERSION); d=$$(date '+%Y-%m-%d'); \
	  b=$$(echo $(PACKAGE_VERSION) | sed 's/.[0-9]$$//'); \
	  n=$$(date '+%-d'); l=$$(date '+%B %Y'); \
	  case $$n in \
	    1?) n=$${n}th ;; \
	    *1) n=$${n}st ;; \
	    *2) n=$${n}nd ;; \
	    *3) n=$${n}rd ;; \
	    *)  n=$${n}th ;; \
	  esac; \
	  last=$$(awk '/## Version/ { print($$3); exit }' $(top_srcdir)/NEWS.md); \
	  sed -i "s/\(Unreleased changes\)/\1\n\n## Version $$v - $$d/" $(top_srcdir)/NEWS.md; \
	  sed -i "s/\bmaster\b/$$b/" $(top_srcdir)/www/features.html.in; \
	  sed -i -e "/define(__version,/cdefine(__version, \`$$v')dnl" \
	    -e "/define(__release_date,/cdefine(__release_date, \`$$n $$l')dnl" \
	    -e "/define(__release_date_short,/cdefine(__release_date_short, \`$$d')dnl" \
	    $(top_srcdir)/www/defs.m4; \
	  sed -i 's/'$$last'/'$$v'/g' $(top_srcdir)/README.md; \
	  $(MAKE) -C $(top_builddir) distcheck; \
	  gpg --yes -b $(top_builddir)/$(PACKAGE)-$(VERSION).tar.gz \
	)

tag:
	echo "Tagging $(PACKAGE_VERSION)"
	git tag -a -m "Release $(PACKAGE_VERSION)" r$(PACKAGE_VERSION) -s

sync-branch:
	head -n1 $(top_srcdir)/configure.ac | grep -q \\-devel || exit 1; \
	last=$$(awk '/## Version/ { print($$3); exit }' $(top_srcdir)/NEWS.md); \
	branch=origin/v$$(echo $$last | sed 's/.[0-9]$$//')-branch; \
	echo "last version was $$last, branch $$branch"; \
	git show $$branch:NEWS.md >/tmp/NEWS.md; \
	new=$$(awk '/## Version/ { print($$3); exit }' /tmp/NEWS.md); \
	echo "new version was $$new"; \
	awk '/Version '$$last'/ { exit; } /Version/ { x=1 } x==1,/./ { print($$0); }' \
	  /tmp/NEWS.md > /tmp/trimmed.md; \
	sed -i -e '/Version/,$$ b;' -e '/^$$/ r /tmp/trimmed.md' $(top_srcdir)/NEWS.md; \
	git checkout $$branch $(top_srcdir)/www/defs.m4; \
	sed -i 's/'$$last'/'$$new'/g' $(top_srcdir)/README.md

upload-artifacts:
	tag=$$(git describe --tags $$(git rev-list --tags --max-count=1)) && \
	gh release upload $$tag $(top_builddir)/$(PACKAGE)-$${tag/r/}.tar.gz{,.sig} && \
	scp $(top_builddir)/$(PACKAGE)-$${tag/r/}.tar.gz{,.sig} vds:/sites/nick/files && \
	run_id=$$(gh run list -w "Build release packages" \
		  -L 1 --json headBranch,databaseId \
		  --jq '.[0] | select(.headBranch == "'$$tag'").databaseId') && \
	echo "Run ID : $$run_id" && \
	rm -rf /tmp/artifacts && \
	mkdir -p /tmp/artifacts && \
	gh run download $$run_id --dir /tmp/artifacts && \
	find /tmp/artifacts -type f -print -exec gh release upload $$tag \{\} \;

endif  # MAINTAINER_MODE

.PHONY: cov-reset cov-report compile-commands everything
.PHONY: release sync-branch upload-artifacts

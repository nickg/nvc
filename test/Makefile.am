check_PROGRAMS = test_lib test_ident test_parse test_sem test_simp \
	test_elab test_heap test_fbuf
TESTS_ENVIRONMENT = BUILD_DIR=$(top_builddir)
TESTS = $(check_PROGRAMS) run_regr.rb

src = $(top_srcdir)/src
build = $(top_builddir)/src
shared = $(src)/util.c
covdir = $(top_builddir)/coverage

AM_CFLAGS = -I $(src) -I $(build) -DTESTDIR=\"$(top_srcdir)/test\" \
	$(CHECK_CFLAGS)
LDADD = -lcheck $(build)/rt/libnvc-rt.a $(build)/libnvc.a $(CHECK_LIBS) \
	$(CURSES_LIB)
AM_LDFLAGS = -rdynamic $(COV_LDFLAGS)

clean-local:
	-test -d logs && $(RM) -r logs

if ENABLE_COVERAGE

cov-reset:
	rm -rf $(covdir)
	find $(top_builddir) -name '*.gcda' -exec rm {} \;
	lcov --directory $(build) --zerocounters

cov-report:
	mkdir -p $(covdir)
	lcov --directory $(build) --capture --output-file $(covdir)/nvc.info
	genhtml -o $(covdir) $(covdir)/nvc.info
	-x-www-browser $(covdir)/index.html

endif

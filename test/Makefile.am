check_PROGRAMS = test_lib test_ident test_parse test_sem test_simp \
	test_elab test_heap
TESTS_ENVIRONMENT = BUILD_DIR=$(top_builddir)
TESTS = $(check_PROGRAMS) run_regr.rb

src = $(top_srcdir)/src
build = $(top_builddir)/src
shared = $(src)/util.c

AM_CFLAGS = -I $(src) -I $(build) -DTESTDIR=\"$(top_srcdir)/test\" \
	$(CHECK_CFLAGS)
LDADD = -lcheck $(build)/rt/libnvc-rt.a $(build)/libnvc.a $(CHECK_LIBS) \
	$(CURSES_LIB)
AM_LDFLAGS = -rdynamic

test_lib_SOURCES = test_lib.c
test_ident_SOURCES = test_ident.c
test_parse_SOURCES = test_parse.c
test_sem_SOURCES = test_sem.c
test_simp_SOURCES = test_simp.c

clean-local:
	-test -d logs && $(RM) -r logs

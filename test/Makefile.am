check_PROGRAMS = test_lib test_ident test_parse test_sem
TESTS = $(check_PROGRAMS)

src = $(top_srcdir)/src
build = $(top_builddir)/src
shared = $(src)/util.c

AM_CFLAGS = -I $(src) -I $(build) -DTESTDIR=\"$(top_srcdir)/test\"
LDADD = -lcheck $(build)/libnhdl.a
AM_LDFLAGS = -rdynamic

test_lib_SOURCES = test_lib.c
test_ident_SOURCES = test_ident.c
test_parse_SOURCES = test_parse.c
test_sem_SOURCES = test_sem.c

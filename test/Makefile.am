check_PROGRAMS = test_lib test_ident test_parse
TESTS = $(check_PROGRAMS)

src = $(top_srcdir)/src
build = $(top_builddir)/src
shared = $(src)/util.c

AM_CFLAGS = -I $(src) -I $(build) -DTESTDIR=\"$(top_srcdir)/test\"
LDADD = -lcheck

test_lib_SOURCES = test_lib.c $(src)/lib.c $(shared)
test_ident_SOURCES = test_ident.c $(src)/ident.c $(shared)
test_parse_SOURCES = test_parse.c $(src)/ident.c $(build)/parse.c \
	$(build)/lexer.c $(src)/tree.c $(shared)

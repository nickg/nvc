check_PROGRAMS = test_lib test_ident
TESTS = $(check_PROGRAMS)

src = $(top_srcdir)/src
shared = $(src)/util.c

AM_CFLAGS = -I $(src)
LDADD = -lcheck

test_lib_SOURCES = test_lib.c $(src)/lib.c $(shared)
test_ident_SOURCES = test_ident.c $(src)/ident.c $(shared)

check_PROGRAMS = test_lib
TESTS = $(check_PROGRAMS)

src = $(top_srcdir)/src

AM_CFLAGS = -I $(src)
LDADD = -lcheck

test_lib_SOURCES = test_lib.c $(src)/lib.c

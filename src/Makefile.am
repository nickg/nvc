bin_PROGRAMS = nhdl
lib_LIBRARIES = libnhdl.a
noinst_LIBRARIES = libcgen.a

AM_CFLAGS = -Wall -Werror
AM_YFLAGS = -d --locations
AM_LDFLAGS = -rdynamic $(LLVM_LDFLAGS)

BUILT_SOURCES = parse.h

libnhdl_a_SOURCES = lib.c util.c ident.c parse.y lexer.l tree.c type.c \
	sem.c elab.c

libcgen_a_SOURCES = cgen.c
libcgen_a_CFLAGS = $(AM_CFLAGS) $(LLVM_CFLAGS)

nhdl_SOURCES = nhdl.c

# Force C++ linking
nodist_EXTRA_nhdl_SOURCES = dummy.cxx

nhdl_LDADD = libnhdl.a libcgen.a $(LLVM_LIBS)


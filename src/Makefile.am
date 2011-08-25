bin_PROGRAMS = nhdl
lib_LIBRARIES = libnhdl.a

AM_CFLAGS = -Wall -Werror
AM_YFLAGS = -d --locations
AM_LDFLAGS = -rdynamic

BUILT_SOURCES = parse.h

libnhdl_a_SOURCES = lib.c util.c ident.c parse.y lexer.l tree.c type.c \
	sem.c elab.c

nhdl_SOURCES = nhdl.c

nhdl_LDADD = libnhdl.a

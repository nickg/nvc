SUBDIRS = rt

bin_PROGRAMS = nvc
noinst_LIBRARIES = libcgen.a libnvc.a

AM_CFLAGS = -Wall $(WERROR_CFLAGS) $(COV_CFLAGS)
AM_YFLAGS = -d --locations --report=all
AM_LDFLAGS = -rdynamic $(LLVM_LDFLAGS) $(COV_LDFLAGS)

BUILT_SOURCES = parse.h

libnvc_a_SOURCES = lib.c util.c ident.c parse.y lexer.l tree.c type.c \
	sem.c elab.c simp.c dump.c opt.c link.c eval.c common.c fbuf.c \
	hash.c group.c bounds.c make.c

libcgen_a_SOURCES = cgen.c
libcgen_a_CFLAGS = $(AM_CFLAGS) $(LLVM_CFLAGS)

if FST_WRITER_PARALLEL
# FIXME: Consider FORCE_CXX_LINK?
AM_CC       = $(PTHREAD_CC)
AM_CFLAGS  += $(PTHREAD_CFLAGS)
AM_LDFLAGS += $(PTHREAD_LIBS)
endif

nvc_SOURCES = nvc.c

if FORCE_CXX_LINK
nodist_EXTRA_nvc_SOURCES = dummy.cxx
endif

nvc_LDADD = libnvc.a libcgen.a rt/libnvc-rt.a rt/libjit.a ../lxt/liblxt.a \
	../fst/libfst.a $(LLVM_LIBS) $(CURSES_LIB)

if IMPLIB_REQUIRED

AM_LDFLAGS += -Wl,--export-all-symbols -Wl,--out-implib=libnvcimp.a

pkgdata_DATA = libnvcimp.a

libnvcimp.a: nvc$(EXEEXT)

.PHONY: libnvcimp.a

endif

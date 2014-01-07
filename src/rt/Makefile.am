noinst_LIBRARIES = libnvc-rt.a libjit.a

AM_CFLAGS = -Wall $(COV_CFLAGS) -I$(srcdir)/.. -I$(top_srcdir)/lxt \
	-I$(top_srcdir)/fst

libnvc_rt_a_SOURCES = rtkern.c slave.c shell.c alloc.c vcd.c heap.c \
	pprint.c netdb.c cover.c lxt.c fst.c wave.c

libjit_a_SOURCES = jit.c
libjit_a_CFLAGS = $(AM_CFLAGS) $(LLVM_CFLAGS)

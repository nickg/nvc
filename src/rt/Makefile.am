noinst_LIBRARIES = libnhdl-rt.a libjit.a

AM_CFLAGS = -Wall -Werror -I$(srcdir)/..

libnhdl_rt_a_SOURCES = rtkern.c

libjit_a_SOURCES = jit.c
libjit_a_CFLAGS = $(AM_CFLAGS) $(LLVM_CFLAGS)

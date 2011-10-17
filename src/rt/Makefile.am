noinst_LIBRARIES = libnvc-rt.a libjit.a

AM_CFLAGS = -Wall -Werror -I$(srcdir)/..

libnvc_rt_a_SOURCES = rtkern.c slave.c shell.c

libjit_a_SOURCES = jit.c
libjit_a_CFLAGS = $(AM_CFLAGS) $(LLVM_CFLAGS)

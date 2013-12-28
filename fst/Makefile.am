noinst_LIBRARIES = libfst.a

if FST_WRITER_PARALLEL
AM_CC      = $(PTHREAD_CC)
AM_CFLAGS  = $(PTHREAD_CFLAFGS)
AM_LDFLAGS = $(PTHREAD_LIBS)
endif

libfst_a_SOURCES = fstapi.c fastlz.c

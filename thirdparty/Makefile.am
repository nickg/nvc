noinst_LIBRARIES = libfst.a liblxt.a libfastlz.a

liblxt_a_SOURCES = lxt_write.c

if FST_WRITER_PARALLEL
AM_CC      = $(PTHREAD_CC)
AM_CFLAGS  = $(PTHREAD_CFLAFGS)
AM_LDFLAGS = $(PTHREAD_LIBS)
endif

libfst_a_SOURCES = fstapi.c

libfastlz_a_SOURCES = fastlz.c

nvc = $(top_builddir)/src/nvc

stddir = $(pkgdatadir)/std
std_DATA = std/_NVC_LIB std/STD.STANDARD

bootstrap: $(std_DATA)

std/_NVC_LIB: std/STD.STANDARD

std/STD.STANDARD: $(nvc) $(srcdir)/standard.vhd
	$(nvc) --work=std -a --bootstrap $(srcdir)/standard.vhd

clean-local:
	-test -d std && $(RM) -r std

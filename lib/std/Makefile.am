nvc = $(top_builddir)/src/nvc

bootstrap: $(nvc)
	$(RM) -r std
	$(nvc) --work=std -a --bootstrap $(srcdir)/standard.vhd

stddir = $(pkgdatadir)/std
std_DATA = std/_NHDL_LIB std/STD.STANDARD

$(std_DATA): bootstrap

clean-local:
	-test -d std && $(RM) -r std

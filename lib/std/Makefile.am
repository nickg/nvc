nvc = $(nvc_verbose)$(top_builddir)/src/nvc

stddir = $(pkgdatadir)/std
std_DATA = std/_NVC_LIB std/STD.STANDARD std/STD.TEXTIO std/_STD.TEXTIO.bc \
	std/STD.TEXTIO-body std/_STD.TEXTIO-body.bc

nvc_verbose = $(nvc_verbose_@AM_V@)
nvc_verbose_ = $(nvc_verbose_@AM_DEFAULT_V@)
nvc_verbose_0 = @echo "  NVC   " $@;

bootstrap: $(std_DATA)

std/_NVC_LIB: std/STD.STANDARD

std/STD.STANDARD: $(srcdir)/standard.vhd
	$(nvc) --work=std -a --bootstrap $(srcdir)/standard.vhd

std/STD.TEXTIO: $(srcdir)/textio.vhd
	$(nvc) --work=std -a $(srcdir)/textio.vhd

clean-local:
	-test -d std && $(RM) -r std

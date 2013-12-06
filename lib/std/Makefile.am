stddir = $(pkgdatadir)/std
std_DATA = std/_NVC_LIB std/STD.STANDARD std/STD.TEXTIO std/_STD.TEXTIO.bc \
	std/STD.TEXTIO-body std/_STD.TEXTIO-body.bc std/_STD.TEXTIO.$(SO_EXT) \
	std/_STD.TEXTIO-body.$(SO_EXT)

bootstrap: $(std_DATA)

std/_NVC_LIB: std/STD.STANDARD

std/STD.STANDARD: $(srcdir)/standard.vhd
	$(nvc) --work=std -a --bootstrap $(srcdir)/standard.vhd

std/STD.TEXTIO: $(srcdir)/textio.vhd
	$(nvc) --work=std -a $(srcdir)/textio.vhd
	$(codegen) --work=std --codegen textio

clean-local:
	-test -d std && $(RM) -r std

include ../nvc.mk

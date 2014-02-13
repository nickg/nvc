stddir = $(pkgdatadir)/std
std_DATA = std/_NVC_LIB std/STD.STANDARD std/STD.TEXTIO std/_STD.TEXTIO.bc \
	std/STD.TEXTIO-body std/_STD.TEXTIO-body.bc

std_08dir = $(pkgdatadir)/std.08
std_08_DATA = std.08/_NVC_LIB \
	std.08/STD.STANDARD std.08/STD.TEXTIO std.08/_STD.TEXTIO.bc \
	std.08/STD.TEXTIO-body std.08/_STD.TEXTIO-body.bc \
	std.08/STD.ENV std.08/STD.ENV-body std.08/_STD.ENV-body.bc

if ENABLE_NATIVE
std_so = std/_STD.TEXTIO.so std/_STD.TEXTIO-body.so
std_08_so = std.08/_STD.TEXTIO.so std.08/_STD.TEXTIO-body.so \
	std.08/_STD.ENV-body.so

if IMPLIB_REQUIRED
std_DATA += std/_STD.TEXTIO.a std/_STD.TEXTIO-body.a
std_08_DATA += std.08/_STD.TEXTIO.a std.08/_STD.TEXTIO-body.a \
	std.08/_STD.ENV-body.a

# Using SCRIPTS rather than data ensures execute bit gets set on Cygwin
std_SCRIPTS = $(std_so)
std_08_SCRIPTS = $(std_08_so)

$(std_SCRIPTS) : $(std_DATA)
$(std_08_SCRIPTS) : $(std_08_DATA)
else

std_DATA += $(std_so)
std_08_DATA += $(std_08_so)

endif

endif

bootstrap: $(std_DATA) $(std_08_DATA)

std/_NVC_LIB: std/STD.STANDARD

std.08/_NVC_LIB: std.08/STD.STANDARD

std/STD.STANDARD: $(srcdir)/standard.vhd
	$(nvc) --work=std -a --bootstrap $(srcdir)/standard.vhd

std.08/STD.STANDARD: $(srcdir)/standard.vhd
	$(nvc) --std=2008 --work=std.08 -a --bootstrap $(srcdir)/standard.vhd

std/STD.TEXTIO: std/STD.STANDARD $(srcdir)/textio.vhd
	$(nvc) --work=std -a $(srcdir)/textio.vhd
	$(codegen) --work=std --codegen textio

std.08/STD.TEXTIO: std.08/STD.STANDARD $(srcdir)/textio.vhd
	$(nvc) --std=2008 --work=std.08 -a $(srcdir)/textio.vhd
	$(codegen) --std=2008 --work=std.08 --codegen textio

std.08/STD.ENV: std.08/STD.STANDARD $(srcdir)/env.vhd
	$(nvc) --std=2008 --work=std.08 -a $(srcdir)/env.vhd
	$(codegen) --std=2008 --work=std.08 --codegen env

clean-local:
	-test ! -d std || rm -r std
	-test ! -d std.08 || rm -r std.08

include ../nvc.mk

gen-deps:
	$(nvc) --work=std --make --posix --deps-only --native | \
		$(deps_pp) > $(srcdir)/deps.mk
	$(nvc) --std=2008 --work=std.08 --make --posix --deps-only --native | \
		$(deps_pp) >> $(srcdir)/deps.mk

include deps.mk

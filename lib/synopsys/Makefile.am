# The Synopsys packages are not part of the IEEE standard but are
# traditionally compiled into the IEEE library
synopsys_ieeedir = $(pkgdatadir)/ieee
synopsysdir = $(pkgdatadir)/synopsys
ieee = ../ieee/ieee
synopsys_ieee_DATA = $(ieee)/IEEE.STD_LOGIC_ARITH $(ieee)/IEEE.STD_LOGIC_ARITH-body \
	$(ieee)/_IEEE.STD_LOGIC_ARITH-body.bc $(ieee)/IEEE.STD_LOGIC_SIGNED \
	$(ieee)/IEEE.STD_LOGIC_SIGNED-body $(ieee)/_IEEE.STD_LOGIC_SIGNED-body.bc \
	$(ieee)/IEEE.STD_LOGIC_UNSIGNED $(ieee)/IEEE.STD_LOGIC_UNSIGNED-body \
	$(ieee)/_IEEE.STD_LOGIC_UNSIGNED-body.bc $(ieee)/IEEE.STD_LOGIC_TEXTIO \
	$(ieee)/IEEE.STD_LOGIC_TEXTIO-body $(ieee)/_IEEE.STD_LOGIC_TEXTIO-body.bc \
	$(ieee)/IEEE.STD_LOGIC_MISC $(ieee)/IEEE.STD_LOGIC_MISC-body \
	$(ieee)/_IEEE.STD_LOGIC_MISC-body.bc
synopsys_DATA = synopsys/_NVC_LIB synopsys/SYNOPSYS.ATTRIBUTES

if ENABLE_NATIVE
synopsys_ieee_so = \
	$(ieee)/_IEEE.STD_LOGIC_ARITH-body.so \
	$(ieee)/_IEEE.STD_LOGIC_SIGNED-body.so \
	$(ieee)/_IEEE.STD_LOGIC_UNSIGNED-body.so \
	$(ieee)/_IEEE.STD_LOGIC_TEXTIO-body.so \
	$(ieee)/_IEEE.STD_LOGIC_MISC-body.so

if IMPLIB_REQUIRED
synopsys_ieee_DATA += \
	$(ieee)/_IEEE.STD_LOGIC_ARITH-body.a \
	$(ieee)/_IEEE.STD_LOGIC_SIGNED-body.a \
	$(ieee)/_IEEE.STD_LOGIC_UNSIGNED-body.a \
	$(ieee)/_IEEE.STD_LOGIC_TEXTIO-body.a \
	$(ieee)/_IEEE.STD_LOGIC_MISC-body.a

# Using SCRIPTS rather than data ensures execute bit gets set on Cygwin
synopsys_ieee_SCRIPTS = $(synopsys_ieee_so)

$(synopsys_ieee_SCRIPTS) : $(synopsys_ieee_DATA)
else

synopsys_ieee_DATA += $(synopsys_ieee_so)

endif

endif

bootstrap: $(synopsys_ieee_DATA) $(synopsys_DATA)

synopsys/_NVC_LIB:

$(ieee)/IEEE.STD_LOGIC_ARITH: $(srcdir)/std_logic_arith.vhd
	$(nvc) -L ../std --work=$(ieee) -a $(srcdir)/std_logic_arith.vhd
	$(codegen) -L ../std --work=$(ieee) --codegen std_logic_arith

$(ieee)/IEEE.STD_LOGIC_UNSIGNED: $(srcdir)/std_logic_unsigned.vhd
	$(nvc) -L ../std --work=$(ieee) -a $(srcdir)/std_logic_unsigned.vhd
	$(codegen) -L ../std --work=$(ieee) --codegen std_logic_unsigned

$(ieee)/IEEE.STD_LOGIC_SIGNED: $(srcdir)/std_logic_signed.vhd
	$(nvc) -L ../std --work=$(ieee) -a $(srcdir)/std_logic_signed.vhd
	$(codegen) -L ../std --work=$(ieee) --codegen std_logic_signed

$(ieee)/IEEE.STD_LOGIC_TEXTIO: $(srcdir)/std_logic_textio.vhd
	$(nvc) -L ../std --work=$(ieee) -a $(srcdir)/std_logic_textio.vhd
	$(codegen) -L ../std --work=$(ieee) --codegen std_logic_textio

$(ieee)/IEEE.STD_LOGIC_MISC: $(srcdir)/std_logic_misc.vhd
	$(nvc) -L ../std -L ../synopsys --work=$(ieee) -a $(srcdir)/std_logic_misc.vhd
	$(codegen) -L ../std -L ../synopsys --work=$(ieee) --codegen std_logic_misc

synopsys/SYNOPSYS.ATTRIBUTES: $(srcdir)/attributes.vhd
	$(nvc) -L ../std --work=synopsys -a $(srcdir)/attributes.vhd

clean-local:
	-test ! -d synopsys || rm -r synopsys

include ../nvc.mk

gen-deps:
	$(nvc) -L ../std --work=../ieee/ieee --make --deps-only --native | \
		$(deps_pp) > $(srcdir)/deps.mk
	$(nvc) -L ../std --work=synopsys --make --deps-only --native | \
		$(deps_pp) >> $(srcdir)/deps.mk

include deps.mk

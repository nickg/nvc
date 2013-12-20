# The Synopsys packages are not part of the IEEE standard but are
# traditionally compiled into the IEEE library
synopsysdir = $(pkgdatadir)/ieee
ieee = ../ieee/ieee
synopsys_DATA = $(ieee)/IEEE.STD_LOGIC_ARITH $(ieee)/IEEE.STD_LOGIC_ARITH-body \
	$(ieee)/_IEEE.STD_LOGIC_ARITH-body.bc $(ieee)/IEEE.STD_LOGIC_SIGNED \
	$(ieee)/IEEE.STD_LOGIC_SIGNED-body $(ieee)/_IEEE.STD_LOGIC_SIGNED-body.bc \
	$(ieee)/IEEE.STD_LOGIC_UNSIGNED $(ieee)/IEEE.STD_LOGIC_UNSIGNED-body \
	$(ieee)/_IEEE.STD_LOGIC_UNSIGNED-body.bc $(ieee)/IEEE.STD_LOGIC_TEXTIO \
	$(ieee)/IEEE.STD_LOGIC_TEXTIO-body $(ieee)/_IEEE.STD_LOGIC_TEXTIO-body.bc

if ENABLE_NATIVE
synopsys_so = \
	$(ieee)/_IEEE.STD_LOGIC_ARITH-body.so \
	$(ieee)/_IEEE.STD_LOGIC_SIGNED-body.so \
	$(ieee)/_IEEE.STD_LOGIC_UNSIGNED-body.so \
	$(ieee)/_IEEE.STD_LOGIC_TEXTIO-body.so

if IMPLIB_REQUIRED
synopsys_DATA += \
	$(ieee)/_IEEE.STD_LOGIC_ARITH-body.a \
	$(ieee)/_IEEE.STD_LOGIC_SIGNED-body.a \
	$(ieee)/_IEEE.STD_LOGIC_UNSIGNED-body.a \
	$(ieee)/_IEEE.STD_LOGIC_TEXTIO-body.a

# Using SCRIPTS rather than data ensures execute bit gets set on Cygwin
synopsys_SCRIPTS = $(synopsys_so)

$(synopsys_SCRIPTS) : $(synopsys_DATA)
else

synopsys_DATA += $(synopsys_so)

endif

endif

bootstrap: $(synopsys_DATA)

$(ieee)/IEEE.STD_LOGIC_ARITH: $(srcdir)/std_logic_arith.vhd
	$(nvc) -L ../std --work=$(ieee) -a $<
	$(codegen) -L ../std --work=$(ieee) --codegen std_logic_arith

$(ieee)/IEEE.STD_LOGIC_UNSIGNED: $(srcdir)/std_logic_unsigned.vhd
	$(nvc) -L ../std --work=$(ieee) -a $<
	$(codegen) -L ../std --work=$(ieee) --codegen std_logic_unsigned

$(ieee)/IEEE.STD_LOGIC_SIGNED: $(srcdir)/std_logic_signed.vhd
	$(nvc) -L ../std --work=$(ieee) -a $<
	$(codegen) -L ../std --work=$(ieee) --codegen std_logic_signed

$(ieee)/IEEE.STD_LOGIC_TEXTIO: $(srcdir)/std_logic_textio.vhd
	$(nvc) -L ../std --work=$(ieee) -a $<
	$(codegen) -L ../std --work=$(ieee) --codegen std_logic_textio

include ../nvc.mk

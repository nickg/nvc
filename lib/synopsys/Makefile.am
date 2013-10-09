# The Synopsys packages are not part of the IEEE standard but are
# traditionally compiled into the IEEE library
synopsysdir = $(pkgdatadir)/ieee
ieee = ../ieee/ieee
synopsys_DATA = $(ieee)/IEEE.STD_LOGIC_ARITH $(ieee)/IEEE.STD_LOGIC_ARITH-body \
	$(ieee)/_IEEE.STD_LOGIC_ARITH-body.bc $(ieee)/IEEE.STD_LOGIC_SIGNED \
	$(ieee)/IEEE.STD_LOGIC_SIGNED-body $(ieee)/_IEEE.STD_LOGIC_SIGNED-body.bc \
	$(ieee)/IEEE.STD_LOGIC_UNSIGNED $(ieee)/IEEE.STD_LOGIC_UNSIGNED-body \
	$(ieee)/_IEEE.STD_LOGIC_UNSIGNED-body.bc $(ieee)/IEEE.STD_LOGIC_TEXTIO \
	$(ieee)/IEEE.STD_LOGIC_TEXTIO-body $(ieee)/_IEEE.STD_LOGIC_TEXTIO-body.bc \
	$(ieee)/_IEEE.STD_LOGIC_ARITH-body.so $(ieee)/_IEEE.STD_LOGIC_SIGNED-body.so \
	$(ieee)/_IEEE.STD_LOGIC_UNSIGNED-body.so $(ieee)/_IEEE.STD_LOGIC_TEXTIO-body.so

bootstrap: $(synopsys_DATA)

$(ieee)/IEEE.STD_LOGIC_ARITH: $(srcdir)/std_logic_arith.vhd
	$(nvc) -L ../std --work=$(ieee) -a $<
	$(codegen) --work=$(ieee) --codegen std_logic_arith

$(ieee)/IEEE.STD_LOGIC_UNSIGNED: $(srcdir)/std_logic_unsigned.vhd
	$(nvc) -L ../std --work=$(ieee) -a $<
	$(codegen) --work=$(ieee) --codegen std_logic_unsigned

$(ieee)/IEEE.STD_LOGIC_SIGNED: $(srcdir)/std_logic_signed.vhd
	$(nvc) -L ../std --work=$(ieee) -a $<
	$(codegen) --work=$(ieee) --codegen std_logic_signed

$(ieee)/IEEE.STD_LOGIC_TEXTIO: $(srcdir)/std_logic_textio.vhd
	$(nvc) -L ../std --work=$(ieee) -a $<
	$(codegen) --work=$(ieee) --codegen std_logic_textio

include ../nvc.mk

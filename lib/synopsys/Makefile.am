nvc = $(top_builddir)/src/nvc

# The Synopsys packages are not part of the IEEE standard but are
# traditionally compiled into the IEEE library
synopsysdir = $(pkgdatadir)/ieee
ieee = ../ieee/ieee
synopsys_DATA = $(ieee)/IEEE.STD_LOGIC_ARITH $(ieee)/IEEE.STD_LOGIC_ARITH-body \
	$(ieee)/IEEE.STD_LOGIC_SIGNED $(ieee)/IEEE.STD_LOGIC_UNSIGNED

lib = NVC_LIBPATH=../std

bootstrap: $(synopsys_DATA)

$(ieee)/IEEE.STD_LOGIC_ARITH: $(srcdir)/std_logic_arith.vhd
	$(lib) $(nvc) --work=$(ieee) -a $<

$(ieee)/IEEE.STD_LOGIC_UNSIGNED: $(srcdir)/std_logic_unsigned.vhd
	$(lib) $(nvc) --work=$(ieee) -a $<

$(ieee)/IEEE.STD_LOGIC_SIGNED: $(srcdir)/std_logic_signed.vhd
	$(lib) $(nvc) --work=$(ieee) -a $<

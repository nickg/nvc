nvc = $(top_builddir)/src/nvc

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

# XXX: add -L argument and move $(nvc_verbose) to $(nvc)
lib = $(nvc_verbose) NVC_LIBPATH=../std

nvc_verbose = $(nvc_verbose_@AM_V@)
nvc_verbose_ = $(nvc_verbose_@AM_DEFAULT_V@)
nvc_verbose_0 = @echo "  NVC   " $@;

bootstrap: $(synopsys_DATA)

$(ieee)/IEEE.STD_LOGIC_ARITH: $(srcdir)/std_logic_arith.vhd
	$(lib) $(nvc) --work=$(ieee) -a $<

$(ieee)/IEEE.STD_LOGIC_UNSIGNED: $(srcdir)/std_logic_unsigned.vhd
	$(lib) $(nvc) --work=$(ieee) -a $<

$(ieee)/IEEE.STD_LOGIC_SIGNED: $(srcdir)/std_logic_signed.vhd
	$(lib) $(nvc) --work=$(ieee) -a $<

$(ieee)/IEEE.STD_LOGIC_TEXTIO: $(srcdir)/std_logic_textio.vhd
	$(lib) $(nvc) --work=$(ieee) -a $<

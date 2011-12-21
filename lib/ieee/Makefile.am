nvc = $(top_builddir)/src/nvc

ieeedir = $(pkgdatadir)/ieee
ieee_DATA = ieee/_NVC_LIB ieee/IEEE.STD_LOGIC_1164 ieee/IEEE.NUMERIC_BIT \
	ieee/IEEE.NUMERIC_STD ieee/IEEE.STD_LOGIC_1164-body

lib = NVC_LIBPATH=../std

bootstrap: $(ieee_DATA)

ieee/_NVC_LIB:

ieee/IEEE.STD_LOGIC_1164: $(srcdir)/std_logic_1164.vhdl
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.STD_LOGIC_1164-body: $(srcdir)/std_logic_1164-body.vhdl
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.NUMERIC_BIT: $(srcdir)/numeric_bit.vhdl
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.NUMERIC_STD: $(srcdir)/numeric_std.vhdl
	$(lib) $(nvc) --work=ieee -a $<

clean-local:
	-test -d ieee && $(RM) -r ieee

$(srcdir)/std_logic_1164.vhdl: 
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/std_logic_1164-body.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/numeric_bit.vhdl: 
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/numeric_std.vhdl: 
	@$(MAKE) missing-ieee LIB=$@

missing-ieee:
	@echo
	@echo "Missing IEEE library source ${LIB}"
	@echo
	@echo "Due to copyright restrictions IEEE sources cannot be freely"
	@echo "redistributed. See lib/ieee/README for details."
	@echo
	@false

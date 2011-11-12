nvc = $(top_builddir)/src/nvc

ieeedir = $(pkgdatadir)/ieee
ieee_DATA = ieee/_NVC_LIB ieee/IEEE.STD_LOGIC_1164

lib = NVC_LIBPATH=../lib/std

bootstrap: $(ieee_DATA)

ieee/_NVC_LIB: ieee/IEEE.STD_LOGIC_1164

ieee/IEEE.STD_LOGIC_1164: $(srcdir)/std_logic_1164.vhdl
	$(lib) $(nvc) --work=ieee -a $(srcdir)/std_logic_1164.vhdl

clean-local:
	-test -d ieee && $(RM) -r ieee

SUFFIXES = .vhdl

$(srcdir)/std_logic_1164.vhdl: 
	@$(MAKE) missing-ieee LIB=$@

missing-ieee:
	@echo
	@echo "Missing IEEE library source ${LIB}"
	@echo
	@echo "Due to copyright restrictions IEEE sources cannot be freely"
	@echo "redistributed. See lib/ieee/README for details."
	@echo
	@false

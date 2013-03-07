nvc = $(top_builddir)/src/nvc

ieeedir = $(pkgdatadir)/ieee
ieee_DATA = ieee/_NVC_LIB ieee/IEEE.STD_LOGIC_1164 ieee/IEEE.NUMERIC_BIT \
	ieee/IEEE.NUMERIC_STD ieee/IEEE.STD_LOGIC_1164-body \
	ieee/_IEEE.STD_LOGIC_1164-body.bc ieee/IEEE.NUMERIC_STD-body \
	ieee/_IEEE.NUMERIC_STD-body.bc ieee/IEEE.MATH_REAL \
	ieee/IEEE.MATH_REAL-body ieee/_IEEE.MATH_REAL-body.bc \
	ieee/IEEE.VITAL_TIMING ieee/IEEE.VITAL_PRIMITIVES \
	ieee/IEEE.VITAL_MEMORY

# XXX: add -L argument and move $(nvc_verbose) to $(nvc)
lib = $(nvc_verbose) NVC_LIBPATH=../std

nvc_verbose = $(nvc_verbose_@AM_V@)
nvc_verbose_ = $(nvc_verbose_@AM_DEFAULT_V@)
nvc_verbose_0 = @echo "  NVC   " $@;

bootstrap: $(ieee_DATA)

ieee/_NVC_LIB:

ieee/IEEE.STD_LOGIC_1164: $(srcdir)/std_logic_1164.vhdl
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.STD_LOGIC_1164-body: $(srcdir)/std_logic_1164-body.vhdl \
	ieee/IEEE.STD_LOGIC_1164
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.NUMERIC_BIT: $(srcdir)/numeric_bit.vhdl ieee/IEEE.STD_LOGIC_1164
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.NUMERIC_STD: $(srcdir)/numeric_std.vhdl ieee/IEEE.STD_LOGIC_1164
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.NUMERIC_STD-body: $(srcdir)/numeric_std-body.vhdl ieee/IEEE.NUMERIC_STD
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.MATH_REAL: $(srcdir)/math_real.vhdl
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.MATH_REAL-body: $(srcdir)/math_real-body.vhdl ieee/IEEE.MATH_REAL
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.VITAL_TIMING: $(srcdir)/timing_p.vhdl
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.VITAL_TIMING-body: $(srcdir)/timing_b.vhdl ieee/IEEE.VITAL_TIMING
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.VITAL_PRIMITIVES: $(srcdir)/prmtvs_p.vhdl
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.VITAL_PRIMITIVES-body: $(srcdir)/prmtvs_b.vhdl \
	ieee/IEEE.VITAL_PRIMITIVES
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.VITAL_MEMORY: $(srcdir)/memory_p.vhdl
	$(lib) $(nvc) --work=ieee -a $<

ieee/IEEE.VITAL_MEMORY-body: $(srcdir)/memory_b.vhdl ieee/IEEE.VITAL_MEMORY
	$(lib) $(nvc) --work=ieee -a $<

clean-local:
	-test -d ieee && $(RM) -r ieee

$(srcdir)/std_logic_1164.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/std_logic_1164-body.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/numeric_bit.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/numeric_bit-body.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/numeric_std.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/numeric_std-body.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/math_real.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/math_real-body.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/timing_p.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/timing_b.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/prmtvs_p.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/prmtvs_b.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/memory_p.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/memory_b.vhdl:
	@$(MAKE) missing-ieee LIB=$@

missing-ieee:
	@echo
	@echo "Missing IEEE library source ${LIB}"
	@echo
	@echo "Due to copyright restrictions IEEE sources cannot be freely"
	@echo "redistributed. See lib/ieee/README for details."
	@echo
	@false

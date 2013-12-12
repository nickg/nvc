ieeedir = $(pkgdatadir)/ieee
ieee_DATA = ieee/_NVC_LIB \
	ieee/IEEE.STD_LOGIC_1164 ieee/IEEE.NUMERIC_BIT \
	ieee/IEEE.NUMERIC_STD ieee/_IEEE.NUMERIC_STD.bc \
	ieee/IEEE.STD_LOGIC_1164-body ieee/_IEEE.STD_LOGIC_1164-body.bc \
	ieee/IEEE.NUMERIC_STD-body ieee/_IEEE.NUMERIC_STD-body.bc \
	ieee/IEEE.NUMERIC_BIT-body ieee/_IEEE.NUMERIC_BIT-body.bc \
	ieee/IEEE.MATH_REAL ieee/_IEEE.MATH_REAL.bc \
	ieee/IEEE.MATH_REAL-body ieee/_IEEE.MATH_REAL-body.bc \
	ieee/IEEE.VITAL_TIMING ieee/_IEEE.VITAL_TIMING.bc \
	ieee/IEEE.VITAL_TIMING-body ieee/_IEEE.VITAL_TIMING-body.bc \
	ieee/IEEE.VITAL_PRIMITIVES ieee/_IEEE.VITAL_PRIMITIVES.bc \
	ieee/IEEE.VITAL_PRIMITIVES-body ieee/_IEEE.VITAL_PRIMITIVES-body.bc

if ENABLE_NATIVE
ieee_DATA += \
	ieee/IEEE.VITAL_MEMORY ieee/_IEEE.NUMERIC_STD.so \
	ieee/_IEEE.STD_LOGIC_1164-body.so ieee/_IEEE.NUMERIC_STD-body.so \
	ieee/_IEEE.NUMERIC_BIT-body.so ieee/_IEEE.MATH_REAL.so \
	ieee/_IEEE.MATH_REAL-body.so ieee/_IEEE.VITAL_TIMING.so \
	ieee/_IEEE.VITAL_TIMING-body.so ieee/_IEEE.VITAL_PRIMITIVES.so \
	ieee/_IEEE.VITAL_PRIMITIVES-body.so
endif

bootstrap: $(ieee_DATA)

ieee/_NVC_LIB:

ieee/IEEE.STD_LOGIC_1164: $(srcdir)/std_logic_1164.vhdl
	$(nvc) -L ../std --work=ieee -a $<

ieee/IEEE.STD_LOGIC_1164-body: $(srcdir)/std_logic_1164-body.vhdl \
	ieee/IEEE.STD_LOGIC_1164
	$(nvc) -L ../std --work=ieee -a $<
	$(codegen) --work=ieee --codegen std_logic_1164

ieee/IEEE.NUMERIC_BIT: $(srcdir)/numeric_bit.vhdl ieee/IEEE.STD_LOGIC_1164
	$(nvc) -L ../std --work=ieee -a $<

ieee/IEEE.NUMERIC_STD: $(srcdir)/numeric_std.vhdl ieee/IEEE.STD_LOGIC_1164
	$(nvc) -L ../std --work=ieee -a $<

ieee/IEEE.NUMERIC_STD-body: $(srcdir)/numeric_std-body.vhdl ieee/IEEE.NUMERIC_STD
	$(nvc) -L ../std --work=ieee -a $<
	$(codegen) --work=ieee --codegen numeric_std

ieee/IEEE.NUMERIC_BIT-body: $(srcdir)/numeric_bit-body.vhdl ieee/IEEE.NUMERIC_STD
	$(nvc) -L ../std --work=ieee -a $<
	$(codegen) --work=ieee --codegen numeric_bit

ieee/IEEE.MATH_REAL: $(srcdir)/math_real.vhdl
	$(nvc) -L ../std --work=ieee -a $<

ieee/IEEE.MATH_REAL-body: $(srcdir)/math_real-body.vhdl ieee/IEEE.MATH_REAL
	$(nvc) -L ../std --work=ieee -a $<
	$(codegen) --work=ieee --codegen math_real

ieee/IEEE.VITAL_TIMING: $(srcdir)/timing_p.vhdl
	$(nvc) -L ../std --work=ieee -a $<

ieee/IEEE.VITAL_TIMING-body: $(srcdir)/timing_b.vhdl ieee/IEEE.VITAL_TIMING
	$(nvc) -L ../std --work=ieee -a $<
	$(codegen) --work=ieee --codegen vital_timing

ieee/IEEE.VITAL_PRIMITIVES: $(srcdir)/prmtvs_p.vhdl
	$(nvc) -L ../std --work=ieee -a $<

ieee/IEEE.VITAL_PRIMITIVES-body: $(srcdir)/prmtvs_b.vhdl \
	ieee/IEEE.VITAL_PRIMITIVES
	$(nvc) -L ../std --work=ieee -a $<
	$(codegen) --work=ieee --codegen vital_primitives

ieee/IEEE.VITAL_MEMORY: $(srcdir)/memory_p.vhdl
	$(nvc) -L ../std --work=ieee -a $<

ieee/IEEE.VITAL_MEMORY-body: $(srcdir)/memory_b.vhdl ieee/IEEE.VITAL_MEMORY
	$(nvc) -L ../std --work=ieee -a $<
	$(codegen) --work=ieee --codegen vital_memory

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

include ../nvc.mk

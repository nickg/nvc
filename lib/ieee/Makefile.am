ieeedir = $(pkgdatadir)/ieee
ieee_DATA = ieee/_NVC_LIB \
	ieee/IEEE.STD_LOGIC_1164 ieee/IEEE.NUMERIC_BIT \
	ieee/IEEE.NUMERIC_STD ieee/_IEEE.NUMERIC_STD.bc \
	ieee/IEEE.STD_LOGIC_1164-body ieee/_IEEE.STD_LOGIC_1164-body.bc \
	ieee/IEEE.NUMERIC_STD-body ieee/_IEEE.NUMERIC_STD-body.bc \
	ieee/IEEE.NUMERIC_BIT-body ieee/_IEEE.NUMERIC_BIT-body.bc \
	ieee/IEEE.MATH_REAL ieee/_IEEE.MATH_REAL.bc \
	ieee/IEEE.MATH_REAL-body ieee/_IEEE.MATH_REAL-body.bc \
	ieee/IEEE.MATH_COMPLEX ieee/_IEEE.MATH_COMPLEX.bc \
	ieee/IEEE.MATH_COMPLEX-body ieee/_IEEE.MATH_COMPLEX-body.bc \
	ieee/IEEE.VITAL_TIMING ieee/_IEEE.VITAL_TIMING.bc \
	ieee/IEEE.VITAL_TIMING-body ieee/_IEEE.VITAL_TIMING-body.bc \
	ieee/IEEE.VITAL_PRIMITIVES ieee/_IEEE.VITAL_PRIMITIVES.bc \
	ieee/IEEE.VITAL_PRIMITIVES-body ieee/_IEEE.VITAL_PRIMITIVES-body.bc \
	ieee/IEEE.VITAL_MEMORY

if ENABLE_NATIVE
ieee_so = ieee/_IEEE.NUMERIC_STD.so \
	ieee/_IEEE.STD_LOGIC_1164-body.so ieee/_IEEE.NUMERIC_STD-body.so \
	ieee/_IEEE.NUMERIC_BIT-body.so ieee/_IEEE.MATH_REAL.so \
	ieee/_IEEE.MATH_REAL-body.so ieee/_IEEE.MATH_COMPLEX.so \
	ieee/_IEEE.MATH_COMPLEX-body.so ieee/_IEEE.VITAL_TIMING.so \
	ieee/_IEEE.VITAL_TIMING-body.so ieee/_IEEE.VITAL_PRIMITIVES.so \
	ieee/_IEEE.VITAL_PRIMITIVES-body.so

if IMPLIB_REQUIRED
ieee_DATA += \
	ieee/_IEEE.NUMERIC_STD.a \
	ieee/_IEEE.STD_LOGIC_1164-body.a ieee/_IEEE.NUMERIC_STD-body.a \
	ieee/_IEEE.NUMERIC_BIT-body.a ieee/_IEEE.MATH_REAL.a \
	ieee/_IEEE.MATH_REAL-body.a ieee/_IEEE.MATH_COMPLEX.a \
	ieee/_IEEE.MATH_COMPLEX-body.a ieee/_IEEE.VITAL_TIMING.a \
	ieee/_IEEE.VITAL_TIMING-body.a ieee/_IEEE.VITAL_PRIMITIVES.a \
	ieee/_IEEE.VITAL_PRIMITIVES-body.a

# Using SCRIPTS rather than data ensures execute bit gets set on Cygwin
ieee_SCRIPTS = $(ieee_so)

$(ieee_SCRIPTS) : $(ieee_DATA)
else

ieee_DATA += $(ieee_so)

endif

endif

bootstrap: $(ieee_DATA)

ieee/_NVC_LIB:

ieee/IEEE.STD_LOGIC_1164: $(srcdir)/std_logic_1164.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/std_logic_1164.vhdl

ieee/IEEE.STD_LOGIC_1164-body: $(srcdir)/std_logic_1164-body.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/std_logic_1164-body.vhdl
	$(codegen) -L ../std --work=ieee --codegen std_logic_1164

ieee/IEEE.NUMERIC_BIT: $(srcdir)/numeric_bit.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/numeric_bit.vhdl

ieee/IEEE.NUMERIC_STD: $(srcdir)/numeric_std.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/numeric_std.vhdl

ieee/IEEE.NUMERIC_STD-body: $(srcdir)/numeric_std-body.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/numeric_std-body.vhdl
	$(codegen) -L ../std --work=ieee --codegen numeric_std

ieee/IEEE.NUMERIC_BIT-body: $(srcdir)/numeric_bit-body.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/numeric_bit-body.vhdl
	$(codegen) -L ../std --work=ieee --codegen numeric_bit

ieee/IEEE.MATH_REAL: $(srcdir)/math_real.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/math_real.vhdl

ieee/IEEE.MATH_REAL-body: $(srcdir)/math_real-body.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/math_real-body.vhdl
	$(codegen) -L ../std --work=ieee --codegen math_real

ieee/IEEE.MATH_COMPLEX: $(srcdir)/math_complex.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/math_complex.vhdl

ieee/IEEE.MATH_COMPLEX-body: $(srcdir)/math_complex-body.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/math_complex-body.vhdl
	$(codegen) -L ../std --work=ieee --codegen math_complex

ieee/IEEE.VITAL_TIMING: $(srcdir)/timing_p.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/timing_p.vhdl

ieee/IEEE.VITAL_TIMING-body: $(srcdir)/timing_b.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/timing_b.vhdl
	$(codegen) -L ../std --work=ieee --codegen vital_timing

ieee/IEEE.VITAL_PRIMITIVES: $(srcdir)/prmtvs_p.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/prmtvs_p.vhdl

ieee/IEEE.VITAL_PRIMITIVES-body: $(srcdir)/prmtvs_b.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/prmtvs_b.vhdl
	$(codegen) -L ../std --work=ieee --codegen vital_primitives

ieee/IEEE.VITAL_MEMORY: $(srcdir)/memory_p.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/memory_p.vhdl

ieee/IEEE.VITAL_MEMORY-body: $(srcdir)/memory_b.vhdl
	$(nvc) -L ../std --work=ieee -a $(srcdir)/memory_b.vhdl
	$(codegen) -L ../std --work=ieee --codegen vital_memory

clean-local:
	-test ! -d ieee || rm -r ieee

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

$(srcdir)/math_complex.vhdl:
	@$(MAKE) missing-ieee LIB=$@

$(srcdir)/math_complex-body.vhdl:
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

gen-deps:
	$(nvc) -L ../std -L ../synopsys --work=ieee --make --posix \
	--deps-only --native | $(deps_pp) > $(srcdir)/deps.mk

include deps.mk

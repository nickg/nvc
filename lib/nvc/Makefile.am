nvcdir = $(pkgdatadir)/nvc
nvc_DATA = nvc/_NVC_LIB nvc/NVC.SIM nvc/NVC.SIM-body nvc/_NVC.SIM-body.bc

if ENABLE_NATIVE
nvc_so = nvc/_NVC.SIM-body.so

if IMPLIB_REQUIRED
nvc_DATA += nvc/_NVC.SIM.a

# Using SCRIPTS rather than data ensures execute bit gets set on Cygwin
nvc_SCRIPTS = $(nvc_so)

$(nvc_SCRIPTS) : $(nvc_DATA)
else

nvc_DATA += $(nvc_so)

endif

endif

bootstrap: $(nvc_DATA)

nvc/_NVC_LIB: nvc/NVC.SIM

nvc/NVC.SIM: $(srcdir)/sim.vhd
	$(nvc) --work=nvc -a $(srcdir)/sim.vhd
	$(codegen) --work=nvc --codegen sim

clean-local:
	-test ! -d nvc || rm -r nvc

include ../nvc.mk

gen-deps:
	$(nvc) --work=nvc --make --posix --deps-only --native | \
		$(deps_pp) > $(srcdir)/deps.mk

include deps.mk

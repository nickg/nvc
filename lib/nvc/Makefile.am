# VHDL-2008 has std.env package.
nvcdir = $(pkgdatadir)/nvc
std = ../std/std
nvc_DATA = nvc/_NVC_LIB nvc/NVC.ENV nvc/NVC.ENV-body nvc/_NVC.ENV-body.bc

if ENABLE_NATIVE
nvc_so = nvc/_NVC.ENV-body.so

if IMPLIB_REQUIRED
nvc_DATA += nvc/_NVC.ENV-body.a

# Using SCRIPTS rather than data ensures execute bit gets set on Cygwin
nvc_SCRIPTS = $(nvc_so)

$(nvc_SCRIPTS) : $(nvc_DATA)
else

nvc_DATA += $(nvc_so)

endif

endif

bootstrap: $(nvc_DATA)

nvc/_NVC_LIB: nvc/NVC.ENV

nvc/NVC.ENV: $(top_srcdir)/lib/std/env.vhd
	$(nvc) -L ../std --work=nvc -a $(top_srcdir)/lib/std/env.vhd
	$(codegen) -L ../std --work=nvc --codegen env

clean-local:
	-test ! -d nvc || rm -r nvc

include ../nvc.mk

gen-deps:
	$(nvc) -L ../std --work=nvc --make --posix --deps-only --native | \
		$(deps_pp) > $(srcdir)/deps.mk

include deps.mk

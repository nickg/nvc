# VHDL-2008 has std.env package.
if ENABLE_VHDL_2008
nvc_stddir = $(pkgdatadir)/std
endif
nvcdir = $(pkgdatadir)/nvc
std = ../std/std
nvc_DATA = nvc/_NVC_LIB nvc/NVC.ENV nvc/NVC.ENV-body nvc/_NVC.ENV-body.bc
if ENABLE_VHDL_2008
nvc_std_DATA = $(std)/_NVC_LIB $(std)/STD.ENV $(std)/STD.ENV-body $(std)/_STD.ENV-body.bc
endif

if ENABLE_NATIVE
nvc_so = nvc/_NVC.ENV-body.so
if ENABLE_VHDL_2008
nvc_std_so = $(std)/_STD.ENV-body.so
endif

if IMPLIB_REQUIRED
nvc_DATA += nvc/_NVC.ENV-body.a
if ENABLE_VHDL_2008
nvc_std_DATA += $(std)/_STD.ENV-body.a
endif

# Using SCRIPTS rather than data ensures execute bit gets set on Cygwin
nvc_SCRIPTS = $(nvc_so)
if ENABLE_VHDL_2008
nvc_std_SCRIPTS = $(nvc_std_so)
endif

$(nvc_SCRIPTS) : $(nvc_DATA)
if ENABLE_VHDL_2008
$(nvc_std_SCRIPTS) : $(nvc_std_DATA)
endif
else

nvc_DATA += $(nvc_so)
if ENABLE_VHDL_2008
nvc_std_DATA += $(nvc_std_so)
endif

endif

endif

bootstrap: $(nvc_DATA)

nvc/_NVC_LIB: nvc/NVC.ENV

nvc/NVC.ENV: $(srcdir)/env.vhd
	$(nvc) -L ../std --work=nvc -a $(srcdir)/env.vhd
	$(codegen) -L ../std --work=nvc --codegen env

if ENABLE_VHDL_2008
$(std)/_NVC_LIB: $(std)/STD.ENV

$(std)/STD.ENV: $(srcdir)/env.vhd
	$(nvc) -L ../std --work=$(std) -a $(srcdir)/env.vhd
	$(codegen) -L ../std --work=$(std) --codegen env
endif

clean-local:
	-test ! -d nvc || rm -r nvc

include ../nvc.mk

gen-deps:
	$(nvc) -L ../std --work=nvc --make --posix --deps-only --native | \
		$(deps_pp) > $(srcdir)/deps.mk
if ENABLE_VHDL_2008
	$(nvc) -L ../std --work=$(std) --make --posix --deps-only --native | \
		$(deps_pp) >> $(srcdir)/deps.mk
endif

include deps.mk

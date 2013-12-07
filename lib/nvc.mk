# -*- mode: makefile-automake -*-

nvc = $(nvc_verbose)$(top_builddir)/src/nvc

if ENABLE_NATIVE
codegen = $(codegen_verbose) $(top_builddir)/src/nvc
else
codegen = @true
endif

nvc_verbose = $(nvc_verbose_@AM_V@)
nvc_verbose_ = $(nvc_verbose_@AM_DEFAULT_V@)
nvc_verbose_0 = @echo "  NVC     " $@;

codegen_verbose = $(codegen_verbose_@AM_V@)
codegen_verbose_ = $(codegen_verbose_@AM_DEFAULT_V@)

if ENABLE_NATIVE
codegen_verbose_0 = @echo "  NATIVE  " $@; NVC_LINK_QUIET=1
else
codegen_verbose_0 =
endif

SUBDIRS = thirdparty src test #lib test

EXTRA_DIST = autogen.sh data/coverage.css nvc.1

ACLOCAL_AMFLAGS = -I m4

pkgdata_DATA = data/coverage.css

man_MANS = nvc.1

if HAVE_RONN
nvc.1: nvc.1.md
	$(AM_V_GEN)ronn --roff $(top_srcdir)/nvc.1.md --manual="NVC Manual"
endif

bootstrap: all
	@cd lib && $(MAKE) $(AM_MAKEFLAGS) clean bootstrap

gen-deps: all
	@cd lib && $(MAKE) $(AM_MAKEFLAGS) $@

cov-reset cov-report:
	cd test && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: bootstrap cov-reset cov-report

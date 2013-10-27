SUBDIRS = lxt fst src lib test

EXTRA_DIST = autogen.sh

ACLOCAL_AMFLAGS = -I m4

pkgdata_DATA = data/coverage.css

bootstrap: all
	@cd lib && $(MAKE) $(AM_MAKEFLAGS) clean bootstrap

cov-reset cov-report:
	cd test && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: bootstrap cov-reset cov-report

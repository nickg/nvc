SUBDIRS = lxt fst src lib test

EXTRA_DIST = autogen.sh data/coverage.css nvc.1

ACLOCAL_AMFLAGS = -I m4

pkgdata_DATA = data/coverage.css

man_MANS = nvc.1

bootstrap: all
	@cd lib && $(MAKE) $(AM_MAKEFLAGS) clean bootstrap

cov-reset cov-report:
	cd test && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: bootstrap cov-reset cov-report

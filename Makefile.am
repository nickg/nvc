SUBDIRS = lxt src lib test

EXTRA_DIST = autogen.sh

ACLOCAL_AMFLAGS = -I m4

bootstrap: all
	@cd lib && $(MAKE) $(AM_MAKEFLAGS) clean bootstrap

cov-reset cov-report:
	cd test && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: bootstrap cov-reset cov-report

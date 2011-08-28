SUBDIRS = src lib test

EXTRA_DIST = autogen.sh

ACLOCAL_AMFLAGS = -I m4

bootstrap: all
	@cd lib && $(MAKE) $(AM_MAKEFLAGS) bootstrap

.PHONY: bootstrap

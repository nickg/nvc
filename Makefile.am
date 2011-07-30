SUBDIRS = src lib test

EXTRA_DIST = autogen.sh

bootstrap: all
	@cd lib && $(MAKE) $(AM_MAKEFLAGS) bootstrap

.PHONY: bootstrap

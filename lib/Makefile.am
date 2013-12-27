SUBDIRS = std ieee synopsys

bootstrap:
	@cd std && $(MAKE) $(AM_MAKEFLAGS) bootstrap
	@cd ieee && $(MAKE) $(AM_MAKEFLAGS) bootstrap
	@cd synopsys && $(MAKE) $(AM_MAKEFLAGS) bootstrap

gen-deps:
	@cd std && $(MAKE) $(AM_MAKEFLAGS) $@
	@cd ieee && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: bootstrap

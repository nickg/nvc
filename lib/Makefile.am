SUBDIRS = std ieee synopsys

gen-deps bootstrap:
	@cd std && $(MAKE) $(AM_MAKEFLAGS) $@
	@cd ieee && $(MAKE) $(AM_MAKEFLAGS) $@
	@cd synopsys && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: bootstrap

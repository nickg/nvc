nvc = $(top_builddir)/src/nvc

bootstrap: $(nvc)
	$(RM) -r std
	$(nvc) --work=std -a --bootstrap $(srcdir)/standard.vhd

clean-local:
	-test -d std && $(RM) -r std

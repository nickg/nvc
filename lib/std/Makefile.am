nhdl = $(top_builddir)/src/nhdl

bootstrap: $(nhdl)
	$(RM) -r std
	$(nhdl) --work=std -a --bootstrap $(srcdir)/standard.vhd

clean-local:
	-test -d std && $(RM) -r std

nhdl = $(top_builddir)/src/nhdl

bootstrap: $(nhdl)
	$(RM) -r std
	$(nhdl) -a --work=std $(srcdir)/standard.vhd

clean-local:
	-test -d std && $(RM) -r std

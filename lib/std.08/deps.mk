# Generated by nvc 1.13-devel

lib/std.08/STD.ENV-body: lib/std.08/STD.STANDARD lib/std.08/STD.ENV $(top_srcdir)/lib/std.08/env-body.vhd

lib/std.08/STD.ENV: lib/std.08/STD.STANDARD $(top_srcdir)/lib/std.08/env.vhd

lib/std.08/STD.STANDARD: $(top_srcdir)/lib/std.08/standard.vhd

lib/std.08/STD.TEXTIO: lib/std.08/STD.STANDARD $(top_srcdir)/lib/std.08/textio.vhd

lib/std.08/STD.STANDARD-body: lib/std.08/STD.STANDARD $(top_srcdir)/lib/std/standard-body.vhd

lib/std.08/STD.TEXTIO-body: lib/std.08/STD.TEXTIO lib/std.08/STD.STANDARD lib/nvc.08/NVC.POLYFILL $(top_srcdir)/lib/std/textio-body.vhd


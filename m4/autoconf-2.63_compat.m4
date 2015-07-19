##rhinton: stolen from http://hg.savannah.gnu.org/hgweb/octave/rev/691545147aca
dnl
dnl If needed, define the m4_ifblank and m4_ifnblank macros from autoconf 2.64
dnl This allows us to run with earlier Autoconfs as well.
ifdef([m4_ifblank],[],[
m4_define([m4_ifblank],
[m4_if(m4_translit([[$1]],  [ ][        ][
]), [], [$2], [$3])])])
dnl
ifdef([m4_ifnblank],[],[
m4_define([m4_ifnblank],
[m4_if(m4_translit([[$1]],  [ ][        ][
]), [], [$3], [$2])])])
dnl

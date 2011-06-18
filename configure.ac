AC_INIT([nhdl], [0.1],
        [Nick Gasson <nick@nick.me.uk>],
        [nhdl])
AM_INIT_AUTOMAKE([-Wall color-tests])

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MKDIR_P
     
AC_FUNC_MALLOC
AC_TYPE_SIZE_T
AC_CHECK_FUNCS([strdup mkdir realpath rmdir strerror])
AC_CHECK_HEADERS([limits.h stdlib.h stddef.h])

DX_HTML_FEATURE(ON)
DX_CHM_FEATURE(OFF)
DX_CHI_FEATURE(OFF)
DX_MAN_FEATURE(OFF)
DX_RTF_FEATURE(OFF)
DX_XML_FEATURE(OFF)
DX_PDF_FEATURE(OFF)
DX_PS_FEATURE(OFF)
DX_INIT_DOXYGEN(nhdl, doxygen.cfg, doc)

AC_CONFIG_HEADERS([config.h])
AC_OUTPUT([Makefile src/Makefile test/Makefile])

AC_INIT(gnus-ja.texi)
AC_SET_MAKE
AC_PROG_INSTALL

dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
dnl environment variable to 't'.  Lets undo the damage.
if test "${EMACS}" = "t"; then
   EMACS=""
fi

AC_ARG_WITH(emacs,[  --with-emacs            Use Emacs to build], 
 [ if test "${withval}" = "yes"; then EMACS=emacs; else EMACS=${withval}; fi ])
AC_CHECK_PROG(EMACS, emacs, emacs, xemacs)

AC_CHECK_PROG(DVIPDFMX, dvipdfmx, dvipdfmx, false)
AC_CHECK_PROG(JBIBTEX, jbibtex, jbibtex, false)
AC_CHECK_PROG(PDVIPS, pdvips, pdvips, false)
AC_CHECK_PROG(PTEX, ptex, ptex, false)
AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi, false)

AC_OUTPUT(Makefile)

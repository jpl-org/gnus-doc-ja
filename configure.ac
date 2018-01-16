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

AC_DEFUN(AC_CHECK_TEXINFO,
 dnl Check the existence and the version of a texinfo command.
 [command=$1
  name=`echo $command| tr a-z A-Z`
  if test -n "${name}"; then
    AC_MSG_CHECKING([if ${command} version >= 6.3])
    version=`${command} --version\
      | awk 'BEGIN {zero=0}\
        tolower($zero)~/gnu +texinfo/&&match($zero,/[[0-9]]+\.[[0-9]]+/)\
        {print substr($zero,RSTART,RLENGTH); exit}' 2>/dev/null`
    case "${version}" in
    6\.[[3-9]]|[[7-9]]\.[[0-9]])
      AC_MSG_RESULT([ok (${version})])
      eval ${name}=${command};;
    *)
      if test -z "${version}"; then
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT([no (${version})])
      fi
      eval ${name}=;;
    esac
  fi])

AC_CHECK_PROG(MAKEINFO, makeinfo, makeinfo)
AC_CHECK_TEXINFO($MAKEINFO)
AC_SUBST(MAKEINFO)
AC_CHECK_PROG(TEXI2PDF, texi2pdf, texi2pdf)
AC_CHECK_TEXINFO($TEXI2PDF)
AC_SUBST(TEXI2PDF)

AC_PATH_PROG(GZIP_PROG, gzip)
AC_ARG_WITH(compress-install,
	[AS_HELP_STRING([[--without-compress-install]],
		[do not compress .el and .info files when installing.])],
	[ if test "${withval}" = no; then
		COMPRESS_INSTALL=no;
	  else
		if test -n "${GZIP_PROG}"; then
			COMPRESS_INSTALL=yes;
		else
			COMPRESS_INSTALL=no;
		fi;
	  fi ],
	[ if test -n "${GZIP_PROG}"; then
		COMPRESS_INSTALL=yes;
	  else
		COMPRESS_INSTALL=no;
	  fi ])
AC_SUBST(COMPRESS_INSTALL)

AC_OUTPUT(Makefile)

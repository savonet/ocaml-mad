# $Id$

# check for one particular file of the sources 
AC_INIT([ocaml-mad],[0.4.4],[savonet-users@lists.sourceforge.net])

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT(configuring $PACKAGE_STRING)

AC_BASE_CHECKS()

# Use pkg-config
PKG_PROG_PKG_CONFIG()
if ! test -n "$PKG_CONFIG"; then
  PKG_CONFIG="/bin/false"
fi

# PKG_CHECK_MODULES loses when you need --libs-only-[lL]
#PKG_CHECK_MODULES([MAD], [mad])
if ! $PKG_CONFIG --exists mad; then
       # PKG_config check failed, trying header detection..
       AC_CHECK_HEADERS([mad.h], [ac_have_mad="yes"])
       if ! test -n "$ac_have_mad"; then
         AC_MSG_ERROR([libmad not found])
       fi
       MAD_CFLAGS=""
       MAD_LIBS="-lmad"
       MAD_LDFLAGS=""
else
  MAD_CFLAGS=`$PKG_CONFIG --cflags mad`
  MAD_LIBS=`$PKG_CONFIG --libs-only-l mad`
  MAD_LDFLAGS=`$PKG_CONFIG --libs-only-L mad`
fi
AC_SUBST([MAD_CFLAGS])
AC_SUBST([MAD_LIBS])
AC_SUBST([MAD_LDFLAGS])

# substitutions to perform
AC_SUBST(VERSION)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_CONFIG_FILES([src/META])
AC_OUTPUT

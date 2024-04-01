AC_INIT([lj_mc_moldyn_interactive], [1.0], [florent.calvayrac@univ-lemans.fr])
AM_INIT_AUTOMAKE([1.16 foreign])
AC_CONFIG_SRCDIR([./view.c])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])
AC_CHECK_LIB([glut], [glutMainLoop], [], [AC_MSG_ERROR([FreeGLUT library not found.])])
PKG_CHECK_MODULES([GLUT], [glut], [], [AC_MSG_ERROR([GLUT required but not found.])])


# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_F77
AC_PROG_FC

# Checks for libraries.
# Replace 'function_to_check' with a function from each library to test if the library is present.
AC_CHECK_LIB([m], [sin], [], [AC_MSG_ERROR([*** Math library not found.])])
AC_CHECK_LIB([glut], [glutInit], [], [AC_MSG_ERROR([*** GLUT not found.])])
AC_CHECK_LIB([GLU], [gluLookAt], [], [AC_MSG_ERROR([*** GLU not found.])])
AC_CHECK_LIB([GL], [glBegin], [], [AC_MSG_ERROR([*** OpenGL not found.])])
AC_CHECK_LIB([Xmu], [XmuClientWindow], [], [AC_MSG_ERROR([*** Xmu not found.])])

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h stdio.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([memset])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

dnl
dnl AC_GET_MH
dnl
dnl Load machine specific compiler options
dnl
define(AC_GET_MH,
[AC_MSG_CHECKING([for machine dependent configuration])
changequote(,)
cat > confsed <<EOF
s/ *= */="/
/="/s/$/"/
/^#/{
    d
}
/[^LIBS].*=/{
    p
    s/=.*$//
    s/^/export /
    p
    d
}
/LIBS.*\\\/ {
    s/\\\/ /g
    s/"$//
    h
    D
    bnext
}
/^ *\-L.*\\\/{
    s/\\\/ /g
    s/^ *//g
    s/"$//
    H
    D
    bnext
}
/^ *\-L/{
    s/^ *//g
    s/"$//
    H
    x
    s/\n//g
    s/$/"/
    p
    a\\
export LIBS
    d
}

:next
EOF
changequote([,])
sed -f confsed $1 > conftest 
. ./conftest
/bin/rm -f confsed conftest
if test "$1" != 0 ; then
    AC_MSG_RESULT($1)
else
    AC_MSG_RESULT(unavailable)
fi
])dnl



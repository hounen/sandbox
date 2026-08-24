dnl ---------------------------------------------------- 
dnl a little function to check for a specific version of 
dnl gnu make and quit if it isn't the one we need

define(MAJVRS, 3)
define(MINVRS, 76)

AC_DEFUN(AC_CHECK_MAKE_VERSION,
[  which_make=
   dnl first look for gmake - if not found, look for make
   dnl search order is users PATH variable:/usr/local/bin:/usr/bin:/bin
   AC_PATH_PROG(first_make,gmake,PROBLEM,$PATH:/usr/local/bin:/usr/bin:/bin)
   dnl check which_make to see if problem occurred (no value or PROBLEM)
   if test "$first_make" = PROBLEM ; then
      dnl gmake not found - search again with make
      AC_PATH_PROG(second_make,make,PROBLEM,$PATH:/usr/local/bin:/usr/bin:/bin)
      dnl check which_make to see if problem occurred (no value or PROBLEM)
      if test "$second_make" = PROBLEM ; then
         dnl make was not found - exit with error
         AC_MSG_ERROR("configure did not find a GNU make utility on this machine")
      else
         which_make="$second_make"
      fi
   else
      which_make="$first_make"
   fi

   dnl which_make refers to a valid make - query it for version
   AC_QUERY_MAKE
   dnl use of autoconf setting of MAKE variable occurs in aclocal.m4
])


AC_DEFUN(AC_QUERY_MAKE,
[  dnl either gmake or make will accept a version query - other makes won't
   if AC_TRY_COMMAND(${which_make} --version) | egrep GNU >/dev/null 2>&1; then
      make_text=`$which_make --version`
      dnl we have GNU - but is it the right version?
      dnl in order to work with regexp we have to use m4 quotes [ ] so change m4 quotes to << >>
      changequote(<<,>> )dnl
      dnl peel out the first part of the string up to version number, i.e. GNU Make version 3.78.1
      vers_hdr=`expr "$make_text" : "\(.*[0-9]*[\.][0-9]*\)\,"`
      echo "version header is $vers_hdr"
      dnl peel out the last part of the string that is just the number and only 2 places
      nums=`expr "$vers_hdr" : "GNU Make version \([0-9]*[\.][0-9]*\).*"`
      dnl echo "nums is $nums"
      fndmaj=`expr "$nums" : "\([0-9]*\)[\.].*"`
      fndmin=`expr "$nums" : ".*[\.]\([0-9]*\)"`

      dnl revert back to [ ] for m4 quotes
      changequote([,])dnl
      if test "$fndmaj" -lt MAJVRS ; then
         AC_MSG_ERROR("This configure requires version MAJVRS.MINVRS of gnu make - you have $fndmaj.$fndmin")
      fi
      if test "$fndmin" -lt MINVRS ; then
         AC_MSG_ERROR("This configure requires version MAJVRS.MINVRS of gnu make - you have $fndmaj.$fndmin")
      fi
      dnl otherwise the version is OK and we proceed
   else
      AC_MSG_ERROR("found $which_make first and it is NOT gnu - check your PATH environment variable")
   fi
])

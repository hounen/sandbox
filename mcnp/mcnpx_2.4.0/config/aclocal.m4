dnl ========================================================================
dnl 
dnl 	Author:	Teri Roberts
dnl 		Los Alamos   
dnl 	Date:	April 13, 2000
dnl 
dnl 	Copyright (c) 1997 U. S. Department of Energy. All rights reserved.
dnl 
dnl ========================================================================
dnl 
dnl ********* WARNING! ************  ********* WARNING! ************
dnl Placing dnl comments on the end of lines sometimes causes errors.
dnl ********* WARNING! ************  ********* WARNING! ************
dnl
dnl - - - - - - File Handling - - - - - -
dnl Numbers in the define statements below are file descriptors
dnl fortran compiler messages will be saved in config.log
define(AC_FD_F77, 5)
[  ] 
dnl c compiler messages will be saved in config.log
define(AC_FD_CC, 5)
[  ]
dnl checking for ... messages and results go to stdout
define(AC_FD_MSG, 6)
[  ]
dnl
dnl ----------------------------------------------------
dnl Insert a little routine that checks the version of gnu
dnl make before investing in a full blown configure that 
dnl would be wrong.
dnl ----------------------------------------------------
builtin(include, checks.m4)
dnl
dnl - - - - - - Compiler Specification or Use of Default Compiler - - - - - -
AC_DEFUN(AC_DFLT_FC,
[  dnl if the environment varialble $FC was set externally, use that setting
   if test  -n "${FC}" ; then
       dnl set autoconf internal variable F77
       F77=${FC} 
   else
      dnl find a fortran compiler (sets F77 variable in the process)
      AC_PROG_F77 
      dnl FC gets passed as a variable through substitution
      FC=${F77}  
   fi
   dnl set our FCOMP Makefile variable for Fortran Compiler 
   FCOMP=${F77}     
 
   AC_PROG_F77_WORKS 
   FMODS=
])
dnl
AC_DEFUN(AC_DFLT_CC,
[  dnl if the environment varialble $CC is set, use that setting   
   if test  -z "${CC}" ; then
      AC_PROG_CC   
   fi
   dnl set our Makefile variable 
   CCOMP=$CC 
   AC_PROG_CC_WORKS   
   FMODS=
])
dnl
AC_DEFUN(AC_SET_FC,
[  dnl fprtran compiler was given as an option
   F77="${withval}"  

   dnl set our FCOMP Makefile variable for Fortran Compiler 
   FCOMP=${F77}                 

   dnl set FC, it gets passed as a variable through substitution
   FC=${F77}  
   AC_PROG_F77_WORKS 
   FMODS=
])
dnl
AC_DEFUN(AC_SET_CC,
[  dnl c compiler was given as an option
   CC="${withval}" 
   CCOMP=${CC}
   AC_PROG_CC_WORKS 
   FMODS=
])
dnl these COPT/FOPT settings override the default or calculated settings
AC_DEFUN(AC_SET_COPT,
[
   COPT="${withval}"
])
AC_DEFUN(AC_SET_FOPT,
[
   FOPT="${withval}"
])
dnl these are COPT/FOPT default or calculated settings
AC_DEFUN(AC_DFLT_COPT,
[
   COPT="${DFLT_COPT}"
])
AC_DEFUN(AC_DFLT_FOPT,
[
   FOPT="${DFLT_FOPT}"
])
dnl these FFLAGS/CFLAGS settings will override the default or calculated settings
AC_DEFUN(AC_SET_FFLAGS,
[  AC_MSG_WARN(Overriding FFLAGS setting: ${FFLAGS} with: ${withval} at your request)
   FFLAGS="${withval}"
])
AC_DEFUN(AC_SET_CFLAGS,
[  AC_MSG_WARN(Overriding CFLAGS setting: ${CFLAGS} with: ${withval} at your request)
   CFLAGS="${withval}"
])
dnl
dnl - - - - - - Debug option  - - - - - -
dnl
AC_DEFUN(AC_SET_DEBUG,
[  dnl debug was given as an option
   if test "${withval}" = yes; then
         ac_debug=yes
   fi
])

dnl - - - - - - PVM  library option  - - - - - -
dnl
AC_DEFUN(AC_SET_PVMLIB,
[  dnl PVM is just one of many parallel library options - so set PARALIB variable 
   AC_DEFINE(MULTP)
   AC_DEFINE(PVM)
   dnl add these 2 to CDEFS - FDEFS will get all DEFS later, CDEFS doesn't
   if test "${withval}" = yes; then
	dnl user must have valid settings for $PVM_ROOT and $PVM_ARCH
	if ( test -n "${PVM_ROOT}" ) && ( test -n "${PVM_ARCH}" ) ; then
           PARALIB="-L ${PVM_ROOT}/lib/${PVM_ARCH} -lfpvm3 -lpvm3"
           dnl for SUN only - there are some things in /usr/lib that have to be linked too
           tempstr="${PVM_ARCH}"
           tempsubstr="SUN"
           case "${tempstr}" in
              *"${tempsubstr}"* ) 
                      PARALIB="${PARALIB} -L /usr/lib -lsocket -lnsl"
                      ;;
              * ) 
                      ;;
           esac
        else
           AC_MSG_WARN(*** You requested PVM but you have not set \$PVM_ROOT and \$PVM_ARCH!)
           AC_MSG_WARN(*** PVM will not be linked)
	fi
   fi
])

dnl - - - - - - Check to see how many TASKS the user wants with PVM
dnl
AC_DEFUN(AC_SET_TASKS,
[  dnl Take what the user gives and use it in the Test Makefile.h 
   if ( test -n "${withval}" ) ; then
      TASKS="${withval}"
   else
      TASKS=
   fi
])

dnl
dnl - - - - - - SHARED or STATIC linking of compiled code - - - - - -
AC_DEFUN(AC_SET_SHARED,
[  dnl shared was given as an option
   ac_lnkopt=true
   dnl  only set things ONCE
   if test  -z "${LINKMETH}" ; then 
      LINKMETH='shared'
      TARGETS='sharedtargets'
      suffix="${withval}"
      ac_mutex=`expr ${ac_mutex} '+' 1`
      dnl if suffix is yes, use a default suffix, else use the given suffix
      if test "${suffix}" = yes; then
         AC_SHARE_SUFFIX
      else 
         SHARSUF="${suffix}"
      fi
   fi
   dnl make sure static suffix is always set - shared object building uses it
   if test  -z "${STATSUF}" ; then
      STATSUF="a"
   fi
])

dnl
AC_DEFUN(AC_SET_STATIC,
[  dnl static was given as an option
   ac_lnkopt=true
   dnl  only set things ONCE  
   if test  -z "${LINKMETH}" ; then 
      LINKMETH='static'
      TARGETS='statictargets'
      suffix="${withval}"
      ac_mutex=`expr ${ac_mutex} '+' 1`
      dnl if suffix is yes, use a default suffix, else use the given suffix
      if test "${suffix}" = yes; then
         STATSUF="a"
      else 
         STATSUF="${suffix}"
      fi
   fi
])
dnl
dnl --------- What are shared object libraries called? -------
dnl So far xxx.so is the norm, HP is different xxx.sl
dnl ----------------------------------------------
AC_DEFUN(AC_SHARE_SUFFIX,
[  dnl set SHARSUF according to arch and os
   case "${ARCH}" in 
   hppa*)  case "${SYSTEM}" in 
	hpux* | HPUX*) SHARSUF="sl"                                                   ;; 
	*) echo " for hppa* unrecognized OS: ${SYSTEM}"; exit -99   ;; 
	esac 
   ;; 
   i*86*|alpha*|sparc*|mips*)    
	case "${SYSTEM}" in 
	linux*| Linux*|LINUX*|aix*|AIX*|osf1*|OSF1*|osf*|OSF*|Sun*|sun*|solaris*|Solaris*|SOLARIS*|IRIX64*|irix64*|Irix64*|IRIX*|irix*|Irix*)   SHARSUF="so"      ;; 
	*) echo " for ${ARCH} unrecognized OS: ${SYSTEM}"; exit -99  ;; 
	esac 
   ;; 
   *) echo "In shared suffix setting - unrecognized host cpu ${ARCH}"; exit -89  ;; 
  esac 
])

dnl
dnl - - - - - - MCNPX  FEATURES TO INCLUDE AS MODULES - - - - - 
AC_DEFUN(AC_FMOD,
[  fmod='$1' 
   AC_DEFINE_UNQUOTED( ${fmod} ) 
   dnl translate 
   lcfmod=`echo ${fmod}  | tr '[A-Z]' '[a-z]'`  
   FMODS="${FMODS} $lcfmod" 
   echo "fmod requested: $fmod"
])
dnl
dnl --------- What is the canonical form of the computer hardware? ----
AC_DEFUN(AC_SET_ARCH,
[ dnl use available macros to allow configure to 
  dnl detect the execution platform when it runs
  dnl 
  AC_CANONICAL_SYSTEM
  dnl a few of the Makefiles still refer to SYSTEM & ARCH
  SYSTEM="$host_os"
  ARCH="$host_cpu"
])

dnl
dnl - - - - - Default values for initializing variables - - - - -
AC_DEFUN(AC_CLL_DEFAULTS,
[  dnl ----- all compile/static link/dynamic load flags start out with no value
   dnl except  for FFLAGS and CFLAGS - we have to create a symbol to 
   dnl bootstrap finding the data that mcnpx needs to run
   dnl compilers
   FCOMP=
   CCOMP=
   dnl compiler optimization flags
   COPT=
   FOPT=
   dnl debug flags
   CDEBUG=
   FDEBUG=
   LDDEBUG=
   dnl make flags
   MFLAGS=
   FDEFS=
   CDEFS= 
   FFLAGS=
   CFLAGS=

   FPPFLAGS=
   LIBS=
   FLIBS=
   XLIBS=
   PARALIB=
   TASKS=
   FLAGS=
   LDFLAGS=
   dnl the SOFFLAGS & SOCFLAGS  variables get PREFIX symbol via 
   dnl a later merging of FFLAGS & CFLAGS
   SOFFLAGS=
   SOCFLAGS=
   SOLIBS=
   SOFLAGS=
   SOLDFLAGS= 
   dnl Targets in Makefiles are either static targets or shared targets
   dnl in order to let the Makefiles serve double duty
   TARGETS= 

   dnl --- set default debug indicator ---
   ac_debug=no
   dnl --- set default old cross section indicator ---
   ac_oldxs=no

   dnl ----- set static/shared mutual exclusion var to 0 and link method to nil 
   ac_mutex=0
   unset LINKMETH
   ac_linkopt=false
   dnl ----- final value for ac_mutex better be 0 (use default)  or 1 (use option given)
   dnl ----- final value of 2 would indicate both static and shared were requested

   paw_done=
   dnl set a few variables for easy Test case identification
   TARCH=
   TSYS=
   TFC=
   TCC=
   dnl if the environment variable EDITOR is set, use that, if not, default it to view
   if test  -z "${EDITOR}" ; then
      EDITOR=view
   fi
])
dnl -----------------------
dnl Check on validity of mutex var
dnl -----------------------
AC_DEFUN(AC_CHK_MUTEX,
[  dnl were static and shared both given as options?
   if test "$ac_lnkopt" = true; then
   dnl echo "mutex is $ac_mutex"
   dnl mutex = 0 -> no options were encountered - use default link method & suffixes
   dnl mutex = 1 -> one option was encountered - use the setting and suffixes given
      if test "$ac_mutex" != 0 && test "$ac_mutex" != 1; then
          dnl error - more than one option was encountered, fall back on default link method & suffixes
          AC_MSG_WARN('--with-STATIC and --with-SHARED are mutually exclusive')
          AC_MSG_WARN('default of STATIC will be used')
          LINKMETH='static'
          TARGETS='statictargets'
          STATSUF='a'
      fi
   else 
      dnl no link option was encountered    
      if test "$ac_mutex" = 0; then
          LINKMETH='static'
          TARGETS='statictargets'
          STATSUF='a'
      fi
   fi 
])

dnl ---------------------------------------------
dnl Compile/Link Flags and Output Variables for use in Makefiles
dnl ---------------------------------------------
AC_DEFUN(AC_ENV_FLAGS_VARS,
[  dnl set compiler flags by arch & system & compiler
   dnl also defines symbols by arch & system

   dnl We don't call AC_FLAGS_BY_ARCH_SYS_COMP (in flags.m4) anymore.
   dnl We call all the feature macros here instead.
   AC_SET_TARCH
   AC_SET_TSYS
   AC_SET_TFC_TCC
   AC_ECHO_CONFIG
   AC_PREPROC_SYMBOLS
   AC_FORT_CPP_PARM_WIERDNESS
dnl no longer need pointers gwm 5-20-02
dnl AC_F77_CRAY_PTRS
   AC_BIT_MODE
   AC_COMP_SPECIFIC
   AC_OPTIM_DEBUG
   AC_FCOMP_FLAGS
   AC_CCOMP_FLAGS
   AC_SHARED_LIBRARY_FLAGS

   dnl define additional miscellaneous symbols
   AC_EXTRA_DEFINES

   dnl find the commands that the generated Makefiles need
   AC_FIND_CMDS
])



dnl
dnl call these macros from somewhere appropriate (AC_ENV_FLAGS_VARS) 
dnl
dnl ----------------------------------------------------
dnl Set the TARCH variable to a short version of ARCH
dnl ----------------------------------------------------
AC_DEFUN(AC_SET_TARCH,
[  dnl The ARCH variables can be quite long. To accomodate
   dnl the Test directory name formation, TARCH is set to a shorter name
   dnl
   AC_MSG_CHECKING([Architecture])
   case "${ARCH}" in
	i*86)
		TARCH=intel
		;;
	alpha*)
		TARCH=dec
		;;
	rs6000)
		TARCH=ibm
		;;
	sparc*)
		TARCH=sun
		;;
	hppa*)
		TARCH=hp
		;;
	mips*)
		TARCH=sgi
		;;
	*)
		TARCH=unknown
		;;
   esac
   AC_MSG_RESULT($TARCH)
   if  test "${TARCH}" = unknown; then
	echo unknown
	echo; echo ">>>>>>>>>>>>>>>>>>>>>>>>>"; echo
	echo "${ARCH} is an unrecognized configuration Architecture"
	echo "Be aware that errors may occur due to incorrect compiler flag settings"
	echo "or other unknown configuration issues."
	echo; echo ">>>>>>>>>>>>>>>>>>>>>>>>>"; echo
   fi
])
dnl ----------------------------------------------------
dnl Set the TARCH variable to a short version of ARCH
dnl ----------------------------------------------------
AC_DEFUN(AC_SET_TSYS,
[  dnl The SYSTEM variable can be quite long. To accomodate
   dnl the Test directory name formation, TSYS is set to a shorter name
   dnl
dnl
dnl Set System preprocessor flags
dnl
   AC_MSG_CHECKING([System type])
   case "${SYSTEM}" in
	linux*|Linux*)
		TSYS=linux
		;;
	osf*|OSF*)
		TSYS=osf
		;;
	aix*|AIX*)
		TSYS=aix
		;;
	solaris*|Solaris*|sun*|Sun*|SOLARIS*|SUN*)
		TSYS=solaris
		;;
	hpux*|HPUX*)
		TSYS=hpux
		;;
	irix*|Irix*|IRIX*)
		TSYS=irix
		;;
	*)
		TSYS=unknown
		;;
   esac
   AC_MSG_RESULT($TSYS)
   if  test "${TSYS}" = unknown; then
	echo unknown
	echo; echo ">>>>>>>>>>>>>>>>>>>>>>>>>"; echo
	echo "${SYSTEM} is an unrecognized system configuration"
	echo "Be aware that errors may occur due to incorrect compiler flag settings"
	echo "or other unknown configuration issues."
	echo; echo ">>>>>>>>>>>>>>>>>>>>>>>>>"; echo
   fi
])
dnl ----------------------------------------------------
dnl Set the TFC and TCC variables that are used in making
dnl names for the test problem directories
dnl ----------------------------------------------------
AC_DEFUN(AC_SET_TFC_TCC,
[  dnl In various places in these macros the FCOMP variable 
   dnl and CCOMP variables have extra compiler options or switches  
   dnl appended after the name of the compiler. Grab the naked values of 
   dnl FCOMP and CCOMP before they get any compiler options appended.
   TFC="${FCOMP}"
   TCC="${CCOMP}"
])
dnl ----------------------------------------------------
dnl Echo the most verbose known configuration (but no switches or flags) 
dnl ----------------------------------------------------
AC_DEFUN(AC_ECHO_CONFIG,
[  
   echo "configuration is ${ARCH}:${SYSTEM}:${TFC}:${TCC}"
])
dnl ----------------------------------------------------
dnl Check to see what Pre-Processor symbols should be defined
dnl ----------------------------------------------------
AC_DEFUN(AC_PREPROC_SYMBOLS,
[ dnl use canonical system value ${SYSTEM}
   AC_MSG_CHECKING([System type])
   case "${SYSTEM}" in
	linux*|Linux*)
		AC_DEFINE(LINUX)
		AC_DEFINE(UNIX)
		;;
	osf*|OSF*)
		AC_DEFINE(DEC)
		AC_DEFINE(UNIX)
		;;
	aix*|AIX*)
		AC_DEFINE(AIX)
		AC_DEFINE(UNIX)
		;;
	solaris*|Solaris*|sun*|Sun*|SOLARIS*|SUN*)
		AC_DEFINE(SUN)
		AC_DEFINE(UNIX)
		;;
	hpux*|HPUX*)
		AC_DEFINE(HPUX)
		AC_DEFINE(UNIX)
		;;
	irix*|Irix*|IRIX*)
		AC_DEFINE(SGI)
		AC_DEFINE(UNIX)
		;;
	*) 	
	echo; echo ">>>>>>>>>>>>>>>>>>>>>>>>>"; echo
	echo "${SYSTEM} is an unrecognized system"
	echo "Be aware that errors may occur due to incorrect preprocessor symbol settings."
	echo; echo ">>>>>>>>>>>>>>>>>>>>>>>>>"; echo
		;;
   esac
])
dnl 
dnl The next few routines set what are effectively CPPFLAGS for the 
dnl Fortran compiler. These are a combination of our feature definitions
dnl DEFS followed by the definitions for $PREFIX in PATH_DEFINES 
dnl followed by the -I include directories.
dnl ----------------------------------------------------
dnl Adjust feature DEFS and INCLUDES for different platforms
dnl ----------------------------------------------------
AC_DEFUN(AC_FORT_CPP_PARM_WIERDNESS,
[  dnl Most systems just pass along the -Ds and -Is. However, the
   dnl Fortran compiler on AIX uses -D to turn on 'D's in col 1 as a
   dnl debugging statement. So, C pre-processor -Dsymbol args need to
   dnl be wrapped in a comma-seperated -WF,... construct to get the
   dnl proper stuff to the C pre-processor. This also applies to -I which
   dnl only sets the Fortran "INCLUDE" path on AIX. Cpp needs the -I
   dnl passed explicitly into it in the 'WF,...' from Fortran.
   dnl
   AC_MSG_CHECKING([Fortran CPP parameter weirdness])
   case "${SYSTEM}" in
	aix*|AIX*)
		FDEFS='-WF,$(subst ${SPACE},${COMMA},${DEFS}${PATH_DEFINES}${INCLUDE})'
		CDEFS='${DEFS} ${PATH_DEFINES} ${INCLUDE}'
		AC_MSG_RESULT("Set for AIX")
		;;
	*)
		FDEFS='${DEFS} ${PATH_DEFINES} ${INCLUDE}'
		CDEFS='${DEFS} ${PATH_DEFINES} ${INCLUDE}'
		AC_MSG_RESULT("None")
		;;
   esac
])
dnl
dnl ----------------------------------------------------
dnl Does our Fortran-77 support Cray POINTERs?
dnl ----------------------------------------------------
AC_DEFUN(AC_F77_CRAY_PTRS,
[  dnl use value of FCOMP to set things up
   AC_MSG_CHECKING([Cray Pointers])
   case "${FCOMP}" in
	g77)
		AC_MSG_RESULT(no)
		;;
	*)
		AC_DEFINE(POINTERS)
		AC_MSG_RESULT(yes)
		;;
   esac
])
dnl
dnl ----------------------------------------------------
dnl Need to determine if 32-bit or 64-bit mode should be used
dnl ----------------------------------------------------
AC_DEFUN(AC_BIT_MODE,
[  dnl
   dnl Does our Fortran work in 32-bit or 64-bit mode?
   dnl
   dnl NOT FULLY IMPLEMENTED - FOR NOW JUST DEFINE CHEAP
   dnl   case "${FCOMP}" in
   dnl	*)
   dnl		AC_DEFINE(CHEAP)
   dnl		;;
   dnl   esac
   AC_MSG_CHECKING([Double Precision defaults])
   AC_DEFINE(CHEAP)
   AC_MSG_RESULT([Yes, CHEAP defined])
])
dnl
dnl ----------------------------------------------------
dnl Some compilers need special symbols defined in order
dnl for the compile to work (language usage kludge, et. al).
dnl ----------------------------------------------------
AC_DEFUN(AC_COMP_SPECIFIC,
[  dnl
   dnl Compiler specific defines?
   dnl
   AC_MSG_CHECKING([fortran compiler-specific defines])
   case "${FCOMP}" in
	g77)
		AC_DEFINE(G77)
		FFLAGS="${FFLAGS} -fno-globals"
		;;
	pgf77*)
		AC_DEFINE(PGF77)
		;;
	pgf90*)
		AC_DEFINE(PGF90)
		;;
        f90*)
		AC_DEFINE(F90)
		;;
	*)
                temp=`expr ${FCOMP} | tr [a-z] [A-Z]`
		AC_MSG_WARN('Warning - Symbol definition -D${temp} may be missing')
		;;
   esac
   AC_MSG_RESULT()
])
dnl
dnl ----------------------------------------------------
dnl Determine default settings for Optimization and Debug
dnl compiler flags.
dnl ----------------------------------------------------
AC_DEFUN(AC_OPTIM_DEBUG,
[  dnl
   dnl Set FFLAGS and other compiler flags depending on architecture and compiler.
   dnl
   dnl First set Optimization and Debug flags. These are generally (but not always)
   dnl related in some way.
   dnl The structure of the logic is:
   dnl    Set empty defaults
   dnl    if debugging
   dnl        set optimizations for debug mode
   dnl    else
   dnl        set optimizations for non-debugging mode
   dnl
   AC_MSG_CHECKING([Optimization and Debug flags])
   DFLT_FOPT=
   DFLT_COPT=
   if test "$ac_debug" = yes; then
	FDEBUG="-g "
	CDEBUG="-g "
	LDDEBUG="-g "
	dnl these settings must be compatible with debugging mode each platform
	case "${ARCH}:${SYSTEM}:${TFC}" in
		hppa*:HPUX*:f77 | hppa*:hpux*:f77 |   \
		hppa*:HPUX*:fort77 | hppa*:hpux*:fort77 | \
		hppa*:HPUX*:f90 | hppa*:hpux*:f90)
dnl			DFLT_FOPT=' +O0'
			DFLT_FOPT=' +O0'
			;;
	esac
   else
	dnl these settings are the usual (non-debugging) optimization settings on each platform
	case "${ARCH}:${SYSTEM}:${TFC}" in
		*:*:g77*)
			DFLT_FOPT='-O0'
			;;
		i*86*:linux*:pgf77 | i*86*:Linux*:pgf77 |  \
		i*86*:linux*:pgf90 | i*86*:Linux*:pgf90)
			DFLT_FOPT=' -O0 '
			;;
		hppa*:HPUX*:f77 | hppa*:hpux*:f77 |   \
		hppa*:HPUX*:fort77 | hppa*:hpux*:fort77)
			DFLT_FOPT=' +O0 '
			;;
		hppa*:HPUX*:f90 | hppa*:hpux*:f90)
			dnl DFLT_FOPT=' +O0'
			dnl Developers want O3 as the default optimization level for HP/f90.
dnl			DFLT_FOPT=' +O3'
			DFLT_FOPT=' +O0'
			;;
		mips*:IRIX*:f77 | mips*:irix*:f77 | mips*:Irix*:f77)
			DFLT_FOPT='-O -OPT:Olimit=0'
			DFLT_COPT='-O -OPT:Olimit=0'
			;;
		mips*:IRIX64*:f90 | mips*:irix64*:f90 | mips*:Irix64*:f90)
			DFLT_FOPT='-O -OPT:Olimit=0'
			DFLT_COPT='-O -OPT:Olimit=0'
			;;
		mips*:IRIX*:f90 | mips*:irix*:f90 | mips*:Irix*:f90)
			DFLT_FOPT=
			DFLT_COPT=
			;;
		rs6000:AIX*:xlf* | rs6000:aix*:xlf*)
			dnl Developers want O2 as the default optimization level for AIX/f90.
			DFLT_FOPT='-O2'			
			;;
		sparc*:sun*:f90 | sparc*:SUN*:f90 | sparc*:Sun*:f90 |   \
		sparc*:Solaris*:f90 | sparc*:solaris*:f90 | sparc*:SOLARIS*:f90)
			dnl Developers want O3 as the default optimization level for Sun/f90.
			DFLT_FOPT='-O3'
			;;
		alpha*:OSF*:f90 | alpha*:osf*:f90)
			dnl Developers want O3 as the default optimization level for Alpha/f90.
			dnl Only seems to work if -O0 is specified.
			DFLT_FOPT='-O0'
			;;
	esac
   fi
   AC_MSG_RESULT("$FDEBUG $DFLT_FOPT")
])
dnl
dnl ----------------------------------------------------
dnl Fortran Compiler Flags
dnl ----------------------------------------------------
AC_DEFUN(AC_FCOMP_FLAGS,
[  dnl
   dnl Now switch on the architecture and compiler type and set any other needed
   dnl compiler flags. Fortran compiler flags...
   dnl 
   dnl    BY THE WAY ... The Portland group Fortran compiler (pgf77) does 
   dnl    not require any special default FFLAG settings.
   AC_MSG_CHECKING([Fortran Flags for ${ARCH} ${SYSTEM} ${TFC}])
   case "${ARCH}:${SYSTEM}:${TFC}" in
	i*86*:linux*:ifc | i*86*:Linux*:ifc)
		FFLAGS="${FFLAGS} -w -Vaxlib"
		LDFLAGS="${LDFLAGS} -Vaxlib"
		if test "x${XLIBS}" != "x"; then
			COMMA_DEFS=`echo ${XLIBS} | sed -e "s/ /,/g"`
		else
			COMMA_DEFS=""
		fi
		XLIBS='-Qoption,link,-call_shared,'${COMMA_DEFS}
		;;
	rs6000:AIX*:f77 | rs6000:aix*:f77)
		FFLAGS="${FFLAGS} -NQ20000 -NA16384"
		;;
	alpha*:OSF*:f77* | alpha*:osf*:f77*)
		AC_DEFINE(LP64)
		FCOMP="${FCOMP} -old_f77"
		;;
	alpha*:OSF*:f90* | alpha*:osf*:f90*)
		AC_DEFINE(LP64)
		;;
	alpha*:linux*:fort | alpha*:Linux*:fort)
		FFLAGS="${FFLAGS} -Wp,-P"
		;;
	sparc*:sun*:f77* | sparc*:SUN*:f77* | sparc*:Sun*:f77* |   \
	sparc*:Solaris*:f77* | sparc*:solaris*:f77* | sparc*:SOLARIS*:f77*)
		INCLUDE="${INCLUDE} -I/usr/openwin/include"
		FFLAGS="${FFLAGS} -Nn6000 -Nq6000 -Ns6000 -Nx2000 -dalign"
		;;
	sparc*:sun*:f90 | sparc*:SUN*:f90 | sparc*:Sun*:f90 |   \
	sparc*:Solaris*:f90 | sparc*:solaris*:f90 | sparc*:SOLARIS*:f90)
		INCLUDE="${INCLUDE} -I/usr/openwin/include"
		FFLAGS="${FFLAGS} -dalign"
		;;
	hppa*:HPUX*:f77 | hppa*:hpux*:f77)
		FFLAGS="+T +E1 ${FFLAGS}"
		# Transmogrophy XLIBS for the non-posix HP f77 compiler
		if test "x${XLIBS}" != "x"; then
			COMMA_DEFS=`echo ${XLIBS} | sed -e "s/ /,/g"`
			XLIBS='-Wl,'${COMMA_DEFS}
		fi
		;;
	hppa*:HPUX*:fort77 | hppa*:hpux*:fort77)
		FFLAGS="+T +E1 ${FFLAGS}"
		;;
	hppa*:HPUX*:f90* | hppa*:hpux*:f90)
		dnl The COINCD, ALPHA, PHTVAR symbols are required
		dnl right now to get around a cpp bug on the HP platform.
		FFLAGS="${FFLAGS} -DCOINCD=0 -DALPHA=0 -DPHTVAR=0"
		;;
	mips*:IRIX64*:f77 | mips*:irix64*:f77 | mips*:Irix64*:f77)
		AC_DEFINE(LP64)
		FFLAGS="${FFLAGS} -mips4 -n32 "
		;;
	mips*:IRIX*:f77 | mips*:irix*:f77 | mips*:Irix*:f77)
		FFLAGS="${FFLAGS} -mips3 -n32 "
		;;
	mips*:IRIX64*:f90 | mips*:irix64*:f90 | mips*:Irix64*:f90)
		dnl The DEC symbol will be going away some day.
		AC_DEFINE(LP64)
		FFLAGS="-DDEC=1 ${FFLAGS} -mips4 -n32 "
		;;
	mips*:IRIX*:f90 | mips*:irix*:f90 | mips*:Irix*:f90)
		dnl The DEC symbol will be going away some day.
		FFLAGS="-DDEC=1 ${FFLAGS} -mips3 -n32 "
		;;
	dnl If your configuration doesn't appear above,
	dnl then no special symbols need to be defined.
	dnl Should you discover some symbols you need,
	dnl make a new case block. 
   esac
   AC_MSG_RESULT($FFLAGS)
])
dnl
dnl ----------------------------------------------------
dnl C Compiler Flags
dnl ----------------------------------------------------
AC_DEFUN(AC_CCOMP_FLAGS,
[  dnl
   dnl Now switch on the architecture and compiler type and set any other needed
   dnl compiler flags. C compiler flags...
   dnl
   dnl    BY THE WAY ... We use gcc with the Portland Group Fortran compiler pgf77.
   dnl    No special flag settings required here for gcc use. Check the AC_COMP_SPECIFIC.
   AC_MSG_CHECKING([C Flags for ${ARCH} ${SYSTEM} ${TCC}])
   case "${ARCH}:${SYSTEM}:${TCC}" in
	sparc*:sun*:cc | sparc*:SUN*:cc | sparc*:Sun*:cc |   \
	sparc*:Solaris*:cc | sparc*:solaris*:cc | sparc*:SOLARIS*:cc)
		CFLAGS="${CFLAGS} -xsb -dalign"
		;;
	hppa*:HPUX*:cc | hppa*:hpux*:cc)
		CFLAGS="-Ae ${CFLAGS}"
		LDFLAGS="+FPD ${LDFLAGS}"
		;;
	mips*:IRIX64*:cc | mips*:irix64*:cc | mips*:Irix64*:cc)
		dnl The DEC symbol will be going away some day.
		CFLAGS="-DDEC=1 ${CFLAGS} -mips4 -n32 "
		;;
	mips*:IRIX*:cc | mips*:irix*:cc | mips*:Irix*:cc)
		dnl The DEC symbol will be going away some day.
		CFLAGS="-DDEC=1 ${CFLAGS} -mips3 -n32 "
		;;
   esac
   AC_MSG_RESULT($CFLAGS)
])
dnl
dnl ----------------------------------------------------
dnl Flags that are needed to build shared, relocatable executables.
dnl Depends on all 4 configuration variable values.
dnl ----------------------------------------------------
AC_DEFUN(AC_SHARED_LIBRARY_FLAGS,
[  dnl
   dnl Set flags needed for shared library generation
   dnl
   AC_MSG_CHECKING([Shared Object Module Support])
   if test "${LINKMETH}" = shared; then
	AC_MSG_RESULT(Yes)
	AC_MSG_CHECKING([Flags for shared object library generation])
	case "${ARCH}:${SYSTEM}:${TFC}:${TCC}" in
		i*86*:linux*:g77:gcc | i*86*:Linux*:g77:gcc)
			# Set shared object flags - these get merged later with regular flags.
			SOLIBS='-ldl' 
			SOFLAGS='-shared -rdynamic' 
			SOFFLAGS='-fPIC -fno-second-underscore' 
			SOCFLAGS='-fPIC -DHAVE_DLFCN_H'
			SOLDFLAGS='-rdynamic'
			;;
		i*86*:linux*:pgf77:gcc | i*86*:Linux*:pgf77:gcc)
			SOLIBS='-ldl' 
			SOFLAGS='-shared'
			SOFFLAGS='-Kieee' 
			SOCFLAGS='-DHAVE_DLFCN_H' 
			SOLDFLAGS='-rdynamic'
			;;
		i*86*:linux*:pgf90:gcc | i*86*:Linux*:pgf90:gcc)
			SOLIBS='-ldl' 
			SOFLAGS='-shared'
			SOFFLAGS='-Kieee' 
			SOCFLAGS='-DHAVE_DLFCN_H' 
			SOLDFLAGS='-rdynamic' 
			;;
		i*86*:linux*:pgf90:pgcc | i*86*:Linux*:pgf90:pgcc)
			SOLIBS='-ldl' 
			SOFLAGS='-shared'
			SOFFLAGS='-Kieee' 
			SOCFLAGS='-fpic -DHAVE_DLFCN_H' 
			SOLDFLAGS=
			;;
		rs6000:AIX*:f77:cc | rs6000:aix*:f77:cc)
			echo "no flags set yet for shared object on ibm f77"
			echo "errors may occur until these are correctly set"
			;;
		alpha*:linux*:g77:gcc | alpha*:Linux*:g77:gcc)
			SOLIBS='-ldl'
			SOFLAGS='-shared -rdynamic'
			SOFFLAGS='-fPIC -fno-second-underscore' 
			SOCFLAGS='-fPIC -DHAVE_DLFCN_H'
			SOLDFLAGS='-rdynamic' 
			;;
		alpha*:linux*:fort:cc | alpha*:Linux*:fort:cc | alpha*:linux*:fort:gcc | alpha*:Linux*:fort:gcc)
			SOLIBS=-ldl
			SOFLAGS=-shared
			SOFFLAGS=-DGENERIC_ENV
			SOCFLAGS='-DGENERIC_ENV -DHAVE_DLFCN_H'		 
			SOLDFLAGS=
			;;
		alpha*:OSF*:f77*:cc | alpha*:osf*:f77*:cc)
			SOLIBS=
			SOFLAGS=-shared
			SOFFLAGS=-DGENERIC_ENV
			SOCFLAGS='-DHAVE_DLFCN_H -DGENERIC_ENV'
			;;
		sparc*:sun*:f77-5.0:cc-5.0 | sparc*:SUN*:f77-5.0:cc-5.0 | sparc*:Sun*:f77-5.0:cc-5.0 |   \
		sparc*:Solaris*:f77-5.0:cc-5.0 | sparc*:solaris*:f77-5.0:cc-5.0 | sparc*:SOLARIS*:f77-5.0:cc-5.0)
			SOLIBS='-ldl'
			SOFLAGS='-G'
			SOFFLAGS='-PIC -DGENERIC_ENV'
			SOCFLAGS='-KPIC -DHAVE_DLFCN_H -DGENERIC_ENV'
			SOLDFLAGS=
			;;	                              
		sparc*:sun*:f77:cc | sparc*:SUN*:f77:cc | sparc*:Sun*:f77:cc |   \
		sparc*:Solaris*:f77:cc | sparc*:solaris*:f77:cc | sparc*:SOLARIS*:f77:cc)
			SOLIBS='-ldl'
			SOFLAGS='-G'
			SOFFLAGS='-PIC -DGENERIC_ENV'
			SOCFLAGS='-KPIC -DHAVE_DLFCN_H -DGENERIC_ENV'
			SOLDFLAGS=
			;;                              
		sparc*:sun*:f90:cc | sparc*:SUN*:f90:cc | sparc*:Sun*:f90:cc |   \
		sparc*:Solaris*:f90:cc | sparc*:solaris*:f90:cc | sparc*:SOLARIS*:f90:cc)
			SOLIBS='-ldl'
			SOFLAGS='-G'
			SOFFLAGS='-PIC -DGENERIC_ENV'
			SOCFLAGS='-KPIC -DHAVE_DLFCN_H -DGENERIC_ENV'
			SOLDFLAGS=
			;;                              
		sparc*:sun*:g77:gcc | sparc*:SUN*:g77:gcc | sparc*:Sun*:g77:gcc |   \
		sparc*:Solaris*:g77:gcc | sparc*:solaris*:g77:gcc | sparc*:SOLARIS*:g77:gcc)
			SOLIBS='-ldl' 
			SOFLAGS='-G' 
			SOFFLAGS='-fPIC -fno-second-underscore' 
			SOCFLAGS='-fPIC -DHAVE_DLFCN_H' 
			SOLDFLAGS=
			;;
		sparc*:linux*:g77:gcc | sparc*:Linux*:g77:gcc | sparc*:LINUX*:g77:gcc)
			SOLIBS='-ldl' 
			SOFLAGS='-shared -rdynamic' 
			SOFFLAGS='-fPIC -fno-second-underscore' 
			SOCFLAGS='-fPIC -DHAVE_DLFCN_H'
			SOLDFLAGS='-rdynamic'
			;;
		hppa*:HPUX*:f77:cc | hppa*:hpux*:f77:cc)
			SOLIBS='-ldld'
			SOFLAGS='-Wl,-b,-a,shared,-B,symbolic -ts,/opt/langtools/lib/scrt0.o'
			SOFFLAGS='+Z -DGENERIC_ENV'
			SOCFLAGS='-DHAVE_DL_H -DGENERIC_ENV +Z'
			SOLDFLAGS='-Wl,-E'
			;;
		hppa*:HPUX*:fort77:cc | hppa*:hpux*:fort77:cc)
			SOLIBS='-ldld'
			SOFLAGS='-Wl,-b,-a,shared,-B,symbolic -ts,/opt/langtools/lib/scrt0.o'
			SOFFLAGS='+Z -DGENERIC_ENV'
			SOCFLAGS='-DHAVE_DL_H -DGENERIC_ENV +Z'
			SOLDFLAGS='-Wl,-E'
			;;
		hppa*:HPUX*:f90:cc | hppa*:hpux*:f90:cc)
			SOLIBS='-ldld'
			SOFLAGS='-Wl,-b,-a,shared,-B,symbolic -ts,/opt/langtools/lib/scrt0.o'
			SOFFLAGS='+Z -DGENERIC_ENV'
			SOCFLAGS='-DHAVE_DL_H -DGENERIC_ENV +Z'
			SOLDFLAGS='-Wl,-E'
			;;
		hppa*:HPUX*:g77:gcc | hppa*:hpux*:g77:gcc)
			SOLIBS='-ldl' 
			SOFLAGS='-shared -rdynamic' 
			SOFFLAGS='-fPIC -fno-second-underscore' 
			SOCFLAGS='-fPIC -DHAVE_DLFCN_H'
			SOLDFLAGS='-rdynamic'
			;;
		mips*:IRIX64*:f77:cc | mips*:irix64*:f77:cc | mips*:Irix64*:f77:cc)
			SOLIBS=
			SOFLAGS='-shared'
			SOFFLAGS='-DGENERIC_ENV'
			SOCFLAGS='-DGENERIC_ENV -DHAVE_DLFCN_H'
			SOLDFLAGS=
			;;
		mips*:IRIX*:f77:cc | mips*:irix*:f77:cc | mips*:Irix*:f77:cc)
			SOLIBS='-ldl'
			SOFLAGS='-shared'
			SOFFLAGS='-DGENERIC_ENV'
			SOCFLAGS='-DGENERIC_ENV -DHAVE_DLFCN_H'
			SOLDFLAGS=
			;;
		dnl We need to add the f90 compiler options for shared building.
	esac
	AC_MSG_RESULT("$SOFFLAGS $SOFLAGS")
   else
	AC_MSG_RESULT(No)
   fi
])
dnl

AC_DEFUN(AC_FIND_CMDS,
[  dnl use autoconf macro to set MAKE variable
   dnl make version has already been checked earlier
   AC_PROG_MAKE_SET
   dnl path to the make utility was checked and set early in configure
   dnl FIND the path to the commands we like to use
   dnl We want to find system defined ones first before user defined ones
   dnl so search standard places first before resorting to users PATH var
   AC_PATH_PROG(SHELL,sh,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(CP,cp,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(MV,mv,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(RM,rm,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(LS,ls,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(ECHO,echo,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(AR_CMD,ar,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(CHMOD,chmod,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(SED,sed,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(LD,ld,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
   AC_PATH_PROG(LN,ln,PROBLEM,/bin:/usr/bin:/usr/local/bin:$PATH) 
])

dnl Here's how to default the NO_PAW based on system architecture
dnl See calling routines below this one.
AC_DEFUN(AC_DEFAULT_NO_PAW,
[ dnl For some systems we cannot do the PAW capability, so define NO_PAW.
  dnl For everything else, we can do PAW - no action needed to remove it.
  case "${ARCH}:${SYSTEM}" in
	hppa*:HPUX* | hppa*:hpux*)
		if test "${FC}" = g77; then
			if test "${paw_done}" = 1; then
          		 	echo "NO_PAW already defined"
        		else
           			AC_DEFINE(NO_PAW)
                                paw_done=1
        		fi
                fi
	;;
	i*86*:linux* | i*86*:Linux* | alpha*:linux* | alpha*:Linux* | sparc*:linux* | sparc*:Linux* | sparc*:LINUX*)
		if test "${paw_done}" = 1; then
          		echo "NO_PAW already defined"
        	else
           		AC_DEFINE(NO_PAW)
                        paw_done=1
        	fi		
	;; 

	* ) echo "${ARCH}:${SYSTEM} handles PAW."
	;;
  esac
])

dnl
AC_DEFUN(AC_EXTRA_DEFINES,
[  dnl This is the miscellaneous catch-all place where extra things
   dnl that are not dependent on arch & system & compiler get set
   dnl after all the other arch & system & compiler stuff is done
   if test "$ac_oldxs" = yes; then
      AC_DEFINE(OLDM)
   fi
   if test "${LINKMETH}" = shared; then
      AC_DEFINE(SHARED)
   fi
])
dnl

AC_DEFUN(AC_ADJUST_PROBS,
[  dnl This might be used to omit test problem 14 if g77 was the compiler in use.
   dnl The g77 compiler core dumps when problem 14 is executed.
   dnl Currently the test problems are being set in the Makefile.in
])



dnl
dnl ------------- hunt for the x libraries path
AC_DEFUN(SEARCH_FOR_X,
[ dnl if user gave the --x-libraries= option, then x_libraries will already be set
  dnl calling autoconf's AC_PATH_X shouldn't destroy them
  AC_PATH_X
  if test "$no_x" != "yes"; then
     if test -n "$x_libraries"; then
        XLIBS="-L$x_libraries -lX11"
     else
        XLIBS="-lX11"
     fi
  fi
])

dnl ---- standard LIBS related ----
AC_DEFUN(AC_SET_LIBS,
[  dnl now set up libraries used in linking - provided they aren't already set.
   if test -z "${XLIBS}"; then
      SEARCH_FOR_X
      if test "$no_x" = "yes"; then
         echo "**** Difficulty finding path to X libraries! ****"
         echo "Without this information, building of mcnpx will fail."
         echo "Please run configure again and provide the '--x-libraries=dir' option."
         exit -33
      fi
   else 
      dnl something already set the XLIBS - use it
      echo "XLIBS is already set to ${XLIBS}"
      echo "No further attempt will be made to find path to x-libraries."
      echo "If building of mcnpx fails, please run configure again and provide the '--x-libraries=dir' option."
   fi
   dnl set up fortran libs if not already set
   dnl NOTE: seems to work OK when f90 is in use.
   dnl NOTE: doesn't work quite right for 64 bit IRIX libraries
   if test  -z "${FLIBS}" ; then
      dnl get FLIBS variable set 
      AC_F77_LIBRARY_LDFLAGS 
   else 
      temp="${FLIBS}"
      AC_F77_LIBRARY_LDFLAGS 
      FLIBS="${temp} ${FLIBS}"
   fi
])

dnl
AC_DEFUN(AC_MERGE_FLAGS,
[  dnl merge shared vars/flags settings into one set of vars/flags
   if test "${LINKMETH}" = shared; then
      FFLAGS="${FFLAGS} "'${SOFFLAGS}'
      CFLAGS="${CFLAGS} "'${SOCFLAGS}'
      dnl LDFLAGS="${LDFLAGS} "'${SOLDFLAGS}'
   fi
   dnl merge in optimization settings
   FFLAGS="${FFLAGS} "'${FOPT}'
   CFLAGS="${CFLAGS} "'${COPT}'
])

dnl
AC_DEFUN(AC_APPEND_DEBUGS,
[  if test "$ac_debug" = yes; then
      dnl stick the debug settings on the end of existing flags
      dnl set of flags for static operations
      FFLAGS="${FFLAGS} "'${FDEBUG}'
      CFLAGS="${CFLAGS} "'${CDEBUG}'
      LDFLAGS="${LDFLAGS} "'${LDDEBUG}'
      dnl set of flags for shared operations
      dnl SOFFLAGS="${SOFFLAGS} "'${FDEBUG}'
      dnl SOCFLAGS="${SOCFLAGS} "'${CDEBUG}'
      dnl SOLDFLAGS="${SOLDFLAGS} "'${LDDEBUG}'
   fi
])

dnl 
dnl -------- PLATFORM INDEPENDENT DEFINES ---------
AC_DEFUN(AC_DEFINE_OPTIONS,
[ dnl these symbols MUST be defined for mcnpx_2.2.0 
  dnl some may become options for later modifications.
  AC_DEFINE(MESHTAL)   
  AC_DEFINE(RADIOG)
  AC_DEFINE(CEM)
  AC_DEFINE(HISTP)
  AC_DEFINE(SPABI)
  AC_DEFINE(DFACT)
  AC_DEFINE(XS64)

  dnl Graphics options
  AC_DEFINE(PLOT)
  AC_DEFINE(MCPLOT)
  AC_DEFINE(GKSSIM)
  AC_DEFINE(XLIB)
 
])

dnl -------------------- Mapping -------------------
AC_DEFUN(AC_MAP_OUT_TO_ENV,
[  dnl Finally, map needed output variables to environment variables 
   dnl These will be available for the Makefiles to use
   dnl DEFS is usually a list of -Dxxx args for the #ifdef's in the code
   dnl          for use by the C preprocessor, or FORTRAN equivalent.
   dnl CFLAGS and FFLAGS (initialized to DEFS) add to DEFS whatever 
   dnl          other system flags are needed for compilation of the standard code. 
   dnl COPT and FOPT are compiler optimization switches or flags
   dnl          for compilation of the standard code.
   dnl FPPFLAGS provides the information in DEFS, with possible additions,
   dnl          for the preprocessing phase when compiling the debuggable code.
   dnl          This is now used only for CRAY UNICOS.
   dnl CDEBUG and FDEBUG, and LDDEBUG if present, control the actual compilation
   dnl          of the debuggable code.
   AC_SUBST(SYSTEM) dnl
   AC_SUBST(ARCH) dnl
   AC_SUBST(DEFS) dnl
   AC_SUBST(CDEBUG) dnl
   AC_SUBST(FDEBUG) dnl
   AC_SUBST(LDDEBUG) dnl
   AC_SUBST(COPT) dnl
   AC_SUBST(FOPT) dnl
   AC_SUBST(MFLAGS) dnl
   AC_SUBST(FDEFS) dnl
   AC_SUBST(CDEFS) dnl
   AC_SUBST(FFLAGS) dnl
   AC_SUBST(CFLAGS) dnl
   AC_SUBST(FPPFLAGS) dnl
   AC_SUBST(LDFLAGS) dnl
   AC_SUBST(XLIBS) dnl
   AC_SUBST(FLIBS) dnl
   AC_SUBST(FCOMP) dnl our Makefile uses this one
   AC_SUBST(CCOMP) dnl our Makefile uses this one
   AC_SUBST(FC) dnl env var may be externally set
   AC_SUBST(CC) dnl env var may be externally set
   AC_SUBST(SOLDFLAGS) dnl
   AC_SUBST(SOFLAGS) dnl
   AC_SUBST(SOFFLAGS) dnl
   AC_SUBST(SOCFLAGS) dnl
   AC_SUBST(SOLIBS) dnl
   AC_SUBST(LIBS) dnl   
   AC_SUBST(FLAGS) dnl
   AC_SUBST(FMODS) dnl
   AC_SUBST(SHARSUF) dnl
   AC_SUBST(STATSUF) dnl
   AC_SUBST(LINKMETH) dnl
   AC_SUBST(TARGETS) dnl
   AC_SUBST(TARCH) dnl translated for simplicity
   AC_SUBST(TSYS) dnl translated for simplicity
   AC_SUBST(TFC) dnl could be same as FCOMP
   AC_SUBST(TCC) dnl could be same as CCOMP
   AC_SUBST(EDITOR) dnl for test use

   AC_SUBST(SHELL)
   AC_SUBST(CP)
   AC_SUBST(MV)
   AC_SUBST(RM)
   AC_SUBST(LS)
   AC_SUBST(ECHO)
   AC_SUBST(AR_CMD)
   AC_SUBST(CHMOD)
   AC_SUBST(SED)
   AC_SUBST(LD)
   AC_SUBST(LN)
   AC_SUBST(PARALIB)
   AC_SUBST(TASKS)
])
dnl
dnl -------------------------------------
dnl utility command to evaluate commands
AC_DEFUN(AC_TRY_COMMAND,
[   { ac_try='$1'; AC_TRY_EVAL($ac_try); }
])
dnl -------------------------------------

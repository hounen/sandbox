ECHO OFF
SET BLDROOT=%CD%
SET CMDPATH=%0%
SET SRCROOT=%CMDPATH:\configure=%
SET SRCPATH=%SRCROOT%
COPY /Y %SRCPATH%\Makefile.w32 Makefile
ECHO # Windows include Makefile created by configure script.> Makefile.h
ECHO srcdir = %SRCPATH%>> Makefile.h
ECHO VPATH = %SRCPATH%>> Makefile.h
ECHO incdir = %SRCROOT%\src\include>> Makefile.h
ECHO libdir = %SRCROOT%\lib>> Makefile.h
TYPE %SRCROOT%\config\Makefile.w3h >> Makefile.h
SET SRCPATH=..\%SRCROOT%
FOR %%I IN (src) DO (
IF NOT EXIST %%I MKDIR %%I
CD %%I
COPY /Y %SRCPATH%\%%I\Makefile.w32 Makefile
ECHO # Windows include Makefile created by configure script.> Makefile.h
ECHO srcdir = %SRCPATH%\%%I>> Makefile.h
ECHO VPATH = %SRCPATH%\%%I>> Makefile.h
ECHO incdir = ..\%SRCROOT%\src\include>> Makefile.h
ECHO libdir = ..\%SRCROOT%\lib>> Makefile.h
TYPE ..\%SRCROOT%\config\Makefile.w3h >> Makefile.h
CD ..
)
CD src
SET SRCPATH=..\..\%SRCROOT%\src
FOR %%I IN (bertin gridconv htape3x makxs phtlib Test xsex3 mcnpx) DO (
IF NOT EXIST %%I MKDIR %%I
CD %%I
COPY /Y %SRCPATH%\%%I\Makefile.w32 Makefile
ECHO # Windows include Makefile created by configure script.> Makefile.h
ECHO srcdir = %SRCPATH%\%%I>> Makefile.h
ECHO VPATH = %SRCPATH%\%%I>> Makefile.h
ECHO incdir = ..\..\%SRCROOT%\src\include>> Makefile.h
ECHO libdir = ..\..\%SRCROOT%\lib>> Makefile.h
TYPE ..\..\%SRCROOT%\config\Makefile.w3h >> Makefile.h
CD ..
)
cd mcnpx
SET SRCPATH=..\..\..\%SRCROOT%\src\mcnpx
FOR %%I IN (cem dedx dl-mod f77main fluka89 gvaviv hexs histp lcs mcnpc mcnpf mcnpx-main meshtal mod-fortran spabi utils) DO (
IF NOT EXIST %%I MKDIR %%I
CD %%I
COPY /Y %SRCPATH%\%%I\Makefile.w32 Makefile
ECHO # Windows include Makefile created by configure script.> Makefile.h
ECHO srcdir = %SRCPATH%\%%I>> Makefile.h
ECHO VPATH = %SRCPATH%\%%I>> Makefile.h
ECHO incdir = ..\..\..\%SRCROOT%\src\include>> Makefile.h
ECHO libdir = ..\..\..\%SRCROOT%\lib>> Makefile.h
TYPE ..\..\..\%SRCROOT%\config\Makefile.w3h >> Makefile.h
CD ..
)
CD %BLDROOT%
CD %SRCROOT%
SET PATH=%PATH%;%CD%\bin
ECHO ***************************************
ECHO *                                     *
ECHO *   UNZIPPING AUXILIARY WIN32 FILES   *
ECHO *           CLICK ON UNZIP            *
ECHO *               THEN OK               *
ECHO *              THEN CLOSE             *
ECHO *                                     *
ECHO ***************************************
Win32.exe
CD %BLDROOT%
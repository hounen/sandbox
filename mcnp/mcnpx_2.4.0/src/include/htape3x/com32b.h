!     $Id: com32b.h,v 1.6 2002/03/27 18:14:11 gwm Exp $                       !/*co          1*/
#ifdef CHEAP                                                                    /*!co          2*/
      implicit double precision (a-h,o-z)                                       !/*co          3*/
      parameter (huge_float = 1.0d+37)                                          !/*co          4*/
#else                                                                           /*!co          5*/
      parameter (huge_float = 1.0e123)                                          !/*co          6*/
#endif                                                                          /*!co          7*/
!     Comment formerly required to avoid a DEC ALPHA preprocessor bug.          !/*co          8*/
      parameter (dp0=0.d0, dp1=1.d0, dp2=2.d0, dp3=3.d0, dp4=4.d0, dph=.        !/*co          9*/
     & 5d0, dp5=5.d0, dp10=1.d1, dpth=dp1/dp3, dppi=3.1415926535898d0,          !/*co         10*/
     & dp2th=dp2/dp3)                                                           !/*co         11*/
      save                                                                      !/*co         12*/

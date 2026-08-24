!     $Id: double.h,v 1.8 2002/03/27 18:14:14 gwm Exp $                       !/*do          1*/
#ifdef CHEAP                                                                    /*!do          2*/
      implicit double precision (a-h,o-z)                                       !/*do          3*/
      parameter (huge_float = 1.0d+37, tiny_float = 1.0d-37)                    !/*do          4*/
#else                                                                           /*!do          5*/
      parameter (huge_float = 1.0e123, tiny_float = 1.0e-123)                   !/*do          6*/
#endif                                                                          /*!do          7*/
!     Comment formerly required to avoid a DEC ALPHA preprocessor bug.          !/*do          8*/
      parameter (dp0=0.d0, dp1=1.d0, dp2=2.d0, dp3=3.d0, dp4=4.d0,              !/*do          9*/
     & dp5=5.d0, dp6=6.d0, dp10=10.d0, dp12=12.d0, dp24=24.d0,                  !/*do         10*/
     & dp30=30.d0, dp41=41.d0, dp120=120.d0, dp130=130.d0,                      !/*do         11*/
     & dp200=200.d0, dp650=650.d0, dp720=720.d0,                                !/*do         12*/
     & dp1p30=1.d30)                                                            !/*do         13*/
      parameter (dpth=dp1/dp3, dph=0.5d0, dp2th=dp2/dp3,                        !/*do         14*/
     & dppi=3.1415926535898d0)                                                  !/*do         15*/
      parameter (dp1m2=1.d-02, dp1m4=1.d-04, dp1m5=1.d-05,                      !/*do         16*/
     & dp1m6=1.d-06, dp1m10=1.d-10, dp1m20=1.d-20)                              !/*do         17*/
      parameter (dp4m3=4.d-03, dp2m2=2.d-02, dp9m1=0.9d0, dp11m1=1.1d0,         !/*do         18*/
     & dp7095m6=7.095d-03,                                                      !/*do         19*/
     & dp6906m2=69.06d0)                                                        !/*do         20*/
      save                                                                      !/*do         21*/

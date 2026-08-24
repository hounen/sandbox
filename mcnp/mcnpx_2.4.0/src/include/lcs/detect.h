!     $Id: detect.h,v 1.4 2002/03/27 18:14:14 gwm Exp $                       !/*de          1*/
!=== detect ===========================================================*        !/*de          2*/
!                                                                      *        !/*de          3*/
!----------------------------------------------------------------------*        !/*de          4*/
!                                                                      *        !/*de          5*/
!     detect                         created 20-sep-1989 by a. ferrari *        !/*de          6*/
!                                                                      *        !/*de          7*/
!            included in:                                              *        !/*de          8*/
!                         geoden                                       *        !/*de          9*/
!                         detect                                       *        !/*de         10*/
!                         bdnopt                                       *        !/*de         11*/
!                                                                      *        !/*de         12*/
!            w a r n i n g any change of the ndtcmx parameter must be  *        !/*de         13*/
!            done also on the ndtcm2 parameter of the detloc module!   *        !/*de         14*/
!----------------------------------------------------------------------*        !/*de         15*/
!                                                                      *        !/*de         16*/
      parameter (nrgnmx=10)                                                     !/*de         17*/
      parameter (ndtcmx=10)                                                     !/*de         18*/
      parameter (nscrmx=10)                                                     !/*de         19*/
      parameter (ndtbin=1024)                                                   !/*de         20*/
      character*10 titdet, titsco                                               !/*de         21*/
      logical ldtctr                                                            !/*de         22*/
      common /detct/ edtmin(ndtcmx), edtbin(ndtcmx), edtcut(ndtcmx),            !/*de         23*/
     & kdtreg(nrgnmx,ndtcmx), kdtdet(ndtcmx,nscrmx), ndtsco, ndtdet,            !/*de         24*/
     & ldtctr, idtreg(mxxrgn), kdtscd(nscrmx)                                   !/*de         25*/
      common /detch/ titdet(ndtcmx), titsco(nscrmx)                             !/*de         26*/

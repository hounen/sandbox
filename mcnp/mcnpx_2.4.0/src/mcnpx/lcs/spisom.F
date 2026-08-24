!     $Id: spisom.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                       !/*sp          1*/
      subroutine spisom                                                         !/*sp          2*/
#include <lcs/double.h>                                                         /*!sp          3*/
#include <lcs/btcom.h>                                                          /*!sp          4*/
   10 continue                                                                  !/*sp          5*/
! ISOM coded inline                                                             !/*sp          6*/
   20 continue                                                                  !/*sp          7*/
      z=rang()                                                                  !/*sp          8*/
      z=dp2*z-dp1                                                               !/*sp          9*/
      x=rang()                                                                  !/*sp         10*/
      x=dp2*x-dp1                                                               !/*sp         11*/
      y=rang()                                                                  !/*sp         12*/
      y=dp2*y-dp1                                                               !/*sp         13*/
      dd=x*x+y*y+z*z                                                            !/*sp         14*/
      if (dd.gt.dp1) go to 20                                                   !/*sp         15*/
      m=clsm+0.05d0                                                             !/*sp         16*/
      if (strkp.le.-dp2) m=m+3                                                  !/*sp         17*/
      fermn=fmpn(m)                                                             !/*sp         18*/
! p2 redefined as square of original here and in crjab                          !/*sp         19*/
      p2=fermn*fermn*dd                                                         !/*sp         20*/
!     frmic selects largest of 3 random numbers                                 !/*sp         21*/
!     p2=momentum of particle selected from proper                              !/*sp         22*/
!     fermi distribution                                                        !/*sp         23*/
!     p2 sin theta                                                              !/*sp         24*/
      pxyz(2)=fermn*x                                                           !/*sp         25*/
!     p2 sin theta cos phi                                                      !/*sp         26*/
      pxyz(6)=fermn*y                                                           !/*sp         27*/
!     p2 sin phi                                                                !/*sp         28*/
      pxyz(10)=fermn*z                                                          !/*sp         29*/
!     p2 cos theta                                                              !/*sp         30*/
      e(2)=sqrt(p2+sqnm)                                                        !/*sp         31*/
!     sq. rt. momentum struck part. sqd. +nucleon mass sqd.                     !/*sp         32*/
      rlke=(((e(1)*e(2)-pxyz(1)*pxyz(2)-pxyz(5)*pxyz(6)-pxyz(9)*pxyz(10)        !/*sp         33*/
     & )/dncms)-pm(1))/rcpmv                                                    !/*sp         34*/
!     relative k.e.(mev)--constant=nucleon mass         /cm,                    !/*sp         35*/
!     second=mev/cm.                                                            !/*sp         36*/
      if (i1) 30,30,40                                                          !/*sp         37*/
   30 space(176)=pxyz(2)                                                        !/*sp         38*/
      space(177)=pxyz(6)                                                        !/*sp         39*/
      space(178)=pxyz(10)                                                       !/*sp         40*/
      if (i1) 60,50,50                                                          !/*sp         41*/
   40 pxyz(2)=pxyz(2)+space(176)                                                !/*sp         42*/
      pxyz(6)=pxyz(6)+space(177)                                                !/*sp         43*/
      pxyz(10)=pxyz(10)+space(178)                                              !/*sp         44*/
      e(2)=(pxyz(10)*pxyz(10)+pxyz(6)*pxyz(6)+pxyz(2)*pxyz(2))/1.9032e14        !/*sp         45*/
   50 return                                                                    !/*sp         46*/
   60 i1=1                                                                      !/*sp         47*/
      go to 10                                                                  !/*sp         48*/
      end                                                                       !/*sp         49*/

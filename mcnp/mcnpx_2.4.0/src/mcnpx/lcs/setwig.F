!     $Id: setwig.F,v 1.5 2002/03/27 18:15:23 gwm Exp $                       !/*se          1*/
      subroutine setwig (kk,a,p,r1,rhoq)                                        !/*se          2*/
#include <lcs/double.h>                                                         /*!se          3*/
!     ==================================================================        !/*se          4*/
!     setwig renormalizes the distribution by its volume                        !/*se          5*/
!     i.e. if getwig used rhog, such that the volume-integral                   !/*se          6*/
!     of rhog was 1.0, then r1 = rhoq*(4*pi*kk**3/3.)                           !/*se          7*/
!     hence, for a square-well distribution, r1(0) =dp1  .                      !/*se          8*/
!                                                                               !/*se          9*/
! SETWIG calls   EXP                                                            !/*se         10*/
! called by GAMMA                                                               !/*se         11*/
!     ==================================================================        !/*se         12*/
!                                                                               !/*se         13*/
#ifdef CHEAP                                                                    /*!se         14*/
      double precision kk                                                       !/*se         15*/
#else                                                                           /*!se         16*/
      real kk                                                                   !/*se         17*/
#endif                                                                          /*!se         18*/
      data fourpi /12.56637062d0/                                               !/*se         19*/
!                                                                               !/*se         20*/
      if (a.le.0) go to 40                                                      !/*se         21*/
      if (p.le.dp0) go to 30                                                    !/*se         22*/
      xx=kk/a                                                                   !/*se         23*/
      x=p/a                                                                     !/*se         24*/
!                                                                               !/*se         25*/
      r1=dp0                                                                    !/*se         26*/
      rhoq=dp0                                                                  !/*se         27*/
      if (x.le.dp0) return                                                      !/*se         28*/
!                                                                               !/*se         29*/
!     (-670.,742.) is the cdc argument range                                    !/*se         30*/
      xxpx=xx+x                                                                 !/*se         31*/
      xxmx=xx-x                                                                 !/*se         32*/
      xxmx=min(xxmx,dp650)                                                      !/*se         33*/
      xxpx=min(xxpx,dp650)                                                      !/*se         34*/
!                                                                               !/*se         35*/
      if (x-xx) 10,10,20                                                        !/*se         36*/
!                                                                               !/*se         37*/
!      p < kk                                                                   !/*se         38*/
!                                                                               !/*se         39*/
   10 continue                                                                  !/*se         40*/
!     r1 = 1. - (xx+1.)*exp(-xx) * sinh(x)/x                                    !/*se         41*/
      r1=dp1-dph*(xx+dp1)*(exp(-xxmx)-exp(-xxpx))/x                             !/*se         42*/
      rhoq=r1*dp3/kk**3/fourpi                                                  !/*se         43*/
      return                                                                    !/*se         44*/
!                                                                               !/*se         45*/
!      p > kk                                                                   !/*se         46*/
!                                                                               !/*se         47*/
   20 continue                                                                  !/*se         48*/
!     r1 = (xx*cosh(xx)-sinh(xx)) * exp(-x)/x                                   !/*se         49*/
      r1=dph*((xx-dp1)*exp(xxmx)+(xx+dp1)*exp(-xxpx))/x                         !/*se         50*/
      rhoq=r1*dp3/kk**3/fourpi                                                  !/*se         51*/
      return                                                                    !/*se         52*/
!                                                                               !/*se         53*/
!       input momentum = 0.00                                                   !/*se         54*/
!                                                                               !/*se         55*/
   30 continue                                                                  !/*se         56*/
      r1=dp0                                                                    !/*se         57*/
      rhoq=dp0                                                                  !/*se         58*/
      return                                                                    !/*se         59*/
!                                                                               !/*se         60*/
!        sharp-cutoff case                                                      !/*se         61*/
!                                                                               !/*se         62*/
   40 r1=dp1                                                                    !/*se         63*/
      if (p.gt.kk) r1=dp0                                                       !/*se         64*/
      rhoq=r1*dp3/kk**3/fourpi                                                  !/*se         65*/
      return                                                                    !/*se         66*/
!                                                                               !/*se         67*/
      end                                                                       !/*se         68*/

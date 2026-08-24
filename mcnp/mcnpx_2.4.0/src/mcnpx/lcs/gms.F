!     $Id: gms.F,v 1.4 2002/03/27 18:15:06 gwm Exp $                          !/*gm          1*/
      subroutine gms                                                            !/*gm          2*/
#include <lcs/double.h>                                                         /*!gm          3*/
!                                                                               !/*gm          4*/
!  GMS computes the interaction radius RH and interaction volume                !/*gm          5*/
!     V=(4PI/3)*RH**3 for masses A=1,...,22.                                    !/*gm          6*/
!  It also calculates the statistical factor                                    !/*gm          7*/
!     GAM(N)=(2PI)**(-X)/GAMMA(X), X=(3N-3)/2                                   !/*gm          8*/
!  for N=2,...,LCHAN.                                                           !/*gm          9*/
!                                                                               !/*gm         10*/
!    REFERENCES.......  MASEDT                                                  !/*gm         11*/
!    EXTERNALS........  ALOG      EXP       GAMMAR                              !/*gm         12*/
!                                                                               !/*gm         13*/
#include <lcs/parms.h>                                                          /*!gm         14*/
#include <lcs/fbrcom.h>                                                         /*!gm         15*/
      data con /4.18879d0/, pi2 /6.2831852d0/                                   !/*gm         16*/
      f=log(pi2)                                                                !/*gm         17*/
      do 10 n=2,lchan                                                           !/*gm         18*/
      x=n                                                                       !/*gm         19*/
      tem=-dp3*(x-1)/dp2                                                        !/*gm         20*/
      w3=exp(f*tem)                                                             !/*gm         21*/
      w5=gammar(dp3*(x-1)/dp2)                                                  !/*gm         22*/
      gam(n)=w3/w5                                                              !/*gm         23*/
   10 continue                                                                  !/*gm         24*/
      rm=1.08d0*12.d0**dpth+0.73d0                                              !/*gm         25*/
      do 20 i=1,22                                                              !/*gm         26*/
      am=i                                                                      !/*gm         27*/
      if (i.le.4) then                                                          !/*gm         28*/
        r=2.08d0                                                                !/*gm         29*/
      elseif (i.le.11) then                                                     !/*gm         30*/
        r=1.85d0*am**dpth                                                       !/*gm         31*/
        r=max(r,rm)                                                             !/*gm         32*/
      else                                                                      !/*gm         33*/
        r=1.08d0*am**dpth+0.73d0                                                !/*gm         34*/
      endif                                                                     !/*gm         35*/
      rh(i)=r                                                                   !/*gm         36*/
   20 continue                                                                  !/*gm         37*/
      do 30 i=1,22                                                              !/*gm         38*/
      v(i)=con*rh(i)**3                                                         !/*gm         39*/
   30 continue                                                                  !/*gm         40*/
      return                                                                    !/*gm         41*/
      end                                                                       !/*gm         42*/

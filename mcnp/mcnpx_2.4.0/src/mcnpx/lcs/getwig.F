!     $Id: getwig.F,v 1.5 2002/03/27 18:15:06 gwm Exp $                       !/*ge          1*/
      subroutine getwig (kk,a,p,r1)                                             !/*ge          2*/
#include <lcs/double.h>                                                         /*!ge          3*/
!     ==================================================================        !/*ge          4*/
!     getwig solves (numerically) for the momentum (p) given                    !/*ge          5*/
!            by a folded-yukawa distribution fcn.                               !/*ge          6*/
!            r1 is the value of the c.d.f. at that momentum,                    !/*ge          7*/
!            whose parameters are kk(radius) and a(width)                       !/*ge          8*/
!                                                                               !/*ge          9*/
! GETWIG calls   EXP  COSH  SINH                                                !/*ge         10*/
! called by GAMMA  POENEW  PSEL                                                 !/*ge         11*/
!     ==================================================================        !/*ge         12*/
!                                                                               !/*ge         13*/
!      wigner fcns may not be properly normalized, check this                   !/*ge         14*/
!                                                                               !/*ge         15*/
#ifdef CHEAP                                                                    /*!ge         16*/
      double precision kk, k0, k1                                               !/*ge         17*/
#else                                                                           /*!ge         18*/
      real kk, k0, k1                                                           !/*ge         19*/
#endif                                                                          /*!ge         20*/
      data oth /0.33333333333d0/                                                !/*ge         21*/
      data fourpi /12.56637062d0/                                               !/*ge         22*/
!                                                                               !/*ge         23*/
      if (a.le.dp0) go to 60                                                    !/*ge         24*/
      xx=kk/a                                                                   !/*ge         25*/
!           742. is the limit of the cdc argument range                         !/*ge         26*/
      xx=min(xx,dp200)                                                          !/*ge         27*/
!                                                                               !/*ge         28*/
      t1=dp1-dp3*(xx+dp1)*(xx*cosh(xx)-sinh(xx))*exp(-xx)/xx**3                 !/*ge         29*/
      if (r1-t1) 50,10,20                                                       !/*ge         30*/
   10 p=kk                                                                      !/*ge         31*/
      return                                                                    !/*ge         32*/
!                                                                               !/*ge         33*/
!         r1 > t1 ==> p > kk                                                    !/*ge         34*/
!                                                                               !/*ge         35*/
   20 continue                                                                  !/*ge         36*/
      t2=(dp1-r1)*xx**3/(xx*cosh(xx)-sinh(xx))/dp3                              !/*ge         37*/
!                                                                               !/*ge         38*/
!     solve by iteration using newton method                                    !/*ge         39*/
!                                                                               !/*ge         40*/
      k0=xx                                                                     !/*ge         41*/
   30 k1=k0+(k0+dp1-t2*exp(k0))/k0                                              !/*ge         42*/
      tol=abs(k1-k0)/k0                                                         !/*ge         43*/
      if (tol.lt.1.d-02) go to 40                                               !/*ge         44*/
      k0=k1                                                                     !/*ge         45*/
      go to 30                                                                  !/*ge         46*/
   40 p=a*k1                                                                    !/*ge         47*/
      return                                                                    !/*ge         48*/
!                                                                               !/*ge         49*/
!         r1 < t1  ==< p < kk                                                   !/*ge         50*/
!                                                                               !/*ge         51*/
   50 continue                                                                  !/*ge         52*/
      t3=dp3*(xx+dp1)*exp(-xx)                                                  !/*ge         53*/
      t2=r1*xx**3                                                               !/*ge         54*/
      k0=xx*r1**oth                                                             !/*ge         55*/
!                                                                               !/*ge         56*/
!    the approximation of k0 = xx * r1 ** 1/3 is so good that                   !/*ge         57*/
!    one pass with newton method is very good, so no looping here               !/*ge         58*/
!                                                                               !/*ge         59*/
      k1=(dp2*k0**3+t3*(k0*cosh(k0)-(dp1+k0*k0)*sinh(k0))+t2)/(dp3*k0*k0        !/*ge         60*/
     & -t3*k0*sinh(k0))                                                         !/*ge         61*/
!                                                                               !/*ge         62*/
!     tol = abs(k1-k0)/k0                                                       !/*ge         63*/
!     if (tol .lt. 1.d-02) go to 35                                             !/*ge         64*/
!     k0 = k1                                                                   !/*ge         65*/
!     go to 32                                                                  !/*ge         66*/
!                                                                               !/*ge         67*/
      p=a*k1                                                                    !/*ge         68*/
      return                                                                    !/*ge         69*/
!                                                                               !/*ge         70*/
!           when a=0, a sharp distribution                                      !/*ge         71*/
!                                                                               !/*ge         72*/
   60 k1=r1**oth                                                                !/*ge         73*/
      p=kk*k1                                                                   !/*ge         74*/
      return                                                                    !/*ge         75*/
      end                                                                       !/*ge         76*/

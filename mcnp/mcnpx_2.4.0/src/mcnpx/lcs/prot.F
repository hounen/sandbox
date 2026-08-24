!     $Id: prot.F,v 1.4 2002/03/27 18:15:17 gwm Exp $                         !/*pr          1*/
      subroutine prot (p0,e,p,x,y,z)                                            !/*pr          2*/
! PROT       calls...  SQRT  FLTRNF  EXP  ALOG  COS  AZIRN                      !/*pr          3*/
!     PROT   called by...   NUCNUC  STOR                                        !/*pr          4*/
#include <lcs/double.h>                                                         /*!pr          5*/
!                                                                               !/*pr          6*/
!           selects from the ranft distribution over all momenta up to p        !/*pr          7*/
!            over angles from 0 to 45-deg.                                      !/*pr          8*/
!           to be used for p+p and n+p inelastic events                         !/*pr          9*/
!                                                                               !/*pr         10*/
!           p0 = momentum of incident particle (gev/c)                          !/*pr         11*/
!           e  = energy of secondary nucleon (gev)                              !/*pr         12*/
!           p  = momentum of secondary nucleon (gev/c)                          !/*pr         13*/
!           x  = x-direction cosine                                             !/*pr         14*/
!           y  = y-direction cosine                                             !/*pr         15*/
!           z  = z-direction cosine                                             !/*pr         16*/
!                                                                               !/*pr         17*/
#ifdef CHEAP                                                                    /*!pr         18*/
      double precision mp, mp2                                                  !/*pr         19*/
#else                                                                           /*!pr         20*/
      real mp, mp2                                                              !/*pr         21*/
#endif                                                                          /*!pr         22*/
      data a1 /0.885d0/, a2 /0.101d0/, a3 /4.256d0/, mp /0.940075d0/,           !/*pr         23*/
     & mp2 /0.883741005625d0/, a4 /0.616849233d0/, pi /3.14159d0/, a5 /0        !/*pr         24*/
     & .216470d0/, a6 /0.672377d0/, a7 /1.1004872d0/                            !/*pr         25*/
      data done /dp1/, dfifty /5.d1/                                            !/*pr         26*/
!       a4 is 45-deg. in rads squared                                           !/*pr         27*/
!       a1, a2, and a3 are the ranft coefficients                               !/*pr         28*/
!       mp is mass of proton                                                    !/*pr         29*/
!       mp2 is mp*mp                                                            !/*pr         30*/
!       the maximum of dndp = a5 + a6/(p0**a7) = dndpm                          !/*pr         31*/
!                                                                               !/*pr         32*/
      if (e.ne.dp0) go to 50                                                    !/*pr         33*/
      r1=p0                                                                     !/*pr         34*/
      b1=r1*r1                                                                  !/*pr         35*/
      b2=sqrt(done+b1/mp2)                                                      !/*pr         36*/
      dndpm=a5+a6/(r1**a7)                                                      !/*pr         37*/
   10 continue                                                                  !/*pr         38*/
      r2=rang()                                                                 !/*pr         39*/
      r2=r1*r2                                                                  !/*pr         40*/
!       r2 =  chosen p --- rejection follows                                    !/*pr         41*/
      b3=r2*r2                                                                  !/*pr         42*/
      b4=sqrt(done+b3/mp2)                                                      !/*pr         43*/
      b5=abs(r2+r2*b2-r1*b4)                                                    !/*pr         44*/
      b6=done+b2-r1*r2/(mp2*b4)                                                 !/*pr         45*/
      b7=-a3*b3*a4                                                              !/*pr         46*/
      if (b7.lt.-dfifty) go to 20                                               !/*pr         47*/
      b7=exp(b7)                                                                !/*pr         48*/
      b7=done-b7                                                                !/*pr         49*/
      go to 30                                                                  !/*pr         50*/
   20 b7=done                                                                   !/*pr         51*/
   30 dndp=(pi/a3)*(a1/r1+(a2/b1)*b5)*b6*b7                                     !/*pr         52*/
!       dndp = number/gev/c                                                     !/*pr         53*/
      r3=dndp/dndpm                                                             !/*pr         54*/
      r4=rang()                                                                 !/*pr         55*/
      if (r4.gt.r3) go to 10                                                    !/*pr         56*/
!     rejection completed---accept p = r2                                       !/*pr         57*/
!     next follows z                                                            !/*pr         58*/
   40 continue                                                                  !/*pr         59*/
      r4=rang()                                                                 !/*pr         60*/
      r5=(-done/(a3*b3))*log(done-r4*b7)                                        !/*pr         61*/
!       r5 = theta**2                                                           !/*pr         62*/
      z=cos(sqrt(r5))                                                           !/*pr         63*/
      call azirn (csp,snp)                                                      !/*pr         64*/
      r6=sqrt(done-z*z)                                                         !/*pr         65*/
      x=r6*csp                                                                  !/*pr         66*/
      y=r6*snp                                                                  !/*pr         67*/
      p=r2                                                                      !/*pr         68*/
      e=sqrt(b3+mp2)-mp                                                         !/*pr         69*/
      return                                                                    !/*pr         70*/
   50 r2=p                                                                      !/*pr         71*/
      b3=r2*r2                                                                  !/*pr         72*/
      b7=-a3*b3*a4                                                              !/*pr         73*/
      if (b7.lt.-dfifty) go to 60                                               !/*pr         74*/
      b7=exp(b7)                                                                !/*pr         75*/
      b7=done-b7                                                                !/*pr         76*/
      go to 40                                                                  !/*pr         77*/
   60 b7=done                                                                   !/*pr         78*/
      go to 40                                                                  !/*pr         79*/
      end                                                                       !/*pr         80*/

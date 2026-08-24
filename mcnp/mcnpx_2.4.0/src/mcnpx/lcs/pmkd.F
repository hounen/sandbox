!     $Id: pmkd.F,v 1.4 2002/03/27 18:15:16 gwm Exp $                         !/*pm          1*/
      subroutine pmkd (mult,npm,wsp3,ws3,psel1)                                 !/*pm          2*/
#include <lcs/double.h>                                                         /*!pm          3*/
!     ==================================================================        !/*pm          4*/
!     pmkd calculates the multiplicity and charge                               !/*pm          5*/
!     distribution of pi/ka in ap-p and ap-n annihilation.                      !/*pm          6*/
!     both multiplicity distributions were taken equal to ghesquire             !/*pm          7*/
!     experimental data.                                                        !/*pm          8*/
!     the charge distributions are calculated from the statistical              !/*pm          9*/
!     "fireball" model  of s.j. orfanidis and v. rittenberg                     !/*pm         10*/
!     nucl. phys. b59 (1973) 570                                                !/*pm         11*/
!                                                                               !/*pm         12*/
! PMKD   calls   FLTRNF  ALOG  SQRT  SIN                                        !/*pm         13*/
! called by GAMMA                                                               !/*pm         14*/
!     ==================================================================        !/*pm         15*/
!                                                                               !/*pm         16*/
      dimension amult(6), anpm(49)                                              !/*pm         17*/
      data amult /0.0038d0,0.0818d0,0.2568d0,0.7148d0,0.9358d0,0.9967d0/        !/*pm         18*/
      data anpm /0.31579d0,dp1,dp1,dp1,0.12949d0,dp1,dp1,dp1,0.05086d0,0        !/*pm         19*/
     & .74114d0,dp1,dp1,0.02052d0,0.48144d0,dp1,dp1,0.00814d0,0.28462d0,        !/*pm         20*/
     & 0.90679d0,dp1,0.00328d0,0.15764d0,0.73892d0,dp1,dp0,0.06667d0,0.5        !/*pm         21*/
     & 333d0,0.96667d0,dp1,dp1,dp1,0.12949d0,dp1,dp1,0.30743d0,dp1,dp1,0        !/*pm         22*/
     & .15349d0,0.84454d0,dp1,0.07376d0,0.6267d0,dp1,0.0345d0,0.422d0,0.        !/*pm         23*/
     & 9458d0,dp0,0.23333d0,0.8d0/                                              !/*pm         24*/
!                                                                               !/*pm         25*/
!      centroid and width from fits to multiplicity data                        !/*pm         26*/
!       at 1.6, 3.0, and 5.7 gev/c, from data in cern-hera 79-03:               !/*pm         27*/
!       compilation of cross-sections, p and pbar induced rxns                  !/*pm         28*/
!                                                                               !/*pm         29*/
!     sig=0.77d0+1.d-04*psel1                                                   !/*pm         30*/
!     rn=2.21+0.00037*psel1                                                     !/*pm         31*/
!     using CERN-ps-183 preliminary data, and older values                      !/*pm         32*/
!     from above, ignoring data at 5.7 GeV/c, we get a much                     !/*pm         33*/
!     improved fit for the lower energy data (ignoring k,k* branch)             !/*pm         34*/
!                                                                               !/*pm         35*/
      sig=0.593d0+0.000183d0*psel1                                              !/*pm         36*/
      rn=1.05d0+0.000791d0*psel1                                                !/*pm         37*/
   10 continue                                                                  !/*pm         38*/
      rand=rang()                                                               !/*pm         39*/
      r2=rang()                                                                 !/*pm         40*/
      xn=sqrt(-dp2*log(rand))*sin(6.2832d0*r2)                                  !/*pm         41*/
!        add 2 in addition to 0.5 to avoid round-offs at 0                      !/*pm         42*/
      mxn=rn+xn*sig+2.5d0                                                       !/*pm         43*/
      mult=mxn-2                                                                !/*pm         44*/
      if (mult.lt.0) go to 10                                                   !/*pm         45*/
      if (mult.gt.8) go to 10                                                   !/*pm         46*/
!     add on the two kaons                                                      !/*pm         47*/
      mult=mult+2                                                               !/*pm         48*/
      rand=rang()                                                               !/*pm         49*/
      if (wsp3+ws3) 40,20,40                                                    !/*pm         50*/
!                            pbar-p or nbar-n charge multiplicities             !/*pm         51*/
   20 lmin=(mult-2)*4+1                                                         !/*pm         52*/
      lmax=lmin+3                                                               !/*pm         53*/
      npm=0                                                                     !/*pm         54*/
      do 30 i=lmin,lmax                                                         !/*pm         55*/
      if (rand.le.anpm(i)) go to 60                                             !/*pm         56*/
   30 npm=npm+1                                                                 !/*pm         57*/
      go to 60                                                                  !/*pm         58*/
!                          pbar-n or nbar-p charge multiplicities               !/*pm         59*/
!                          subr. gamma turns #(pi-) into #(pi+)                 !/*pm         60*/
   40 lmin=(mult-2)*3+29                                                        !/*pm         61*/
      lmax=lmin+2                                                               !/*pm         62*/
      npm=1                                                                     !/*pm         63*/
      do 50 i=lmin,lmax                                                         !/*pm         64*/
      if (rand.le.anpm(i)) go to 60                                             !/*pm         65*/
   50 npm=npm+1                                                                 !/*pm         66*/
   60 return                                                                    !/*pm         67*/
      end                                                                       !/*pm         68*/

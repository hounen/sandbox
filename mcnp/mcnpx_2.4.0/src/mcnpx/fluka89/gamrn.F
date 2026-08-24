!     $Id: gamrn.F,v 1.4 2002/03/27 18:14:44 gwm Exp $                        !/*ga          1*/
      double precision function gamrn (alam,eta)                                !/*ga          2*/
!                                                                     *         !/*ga          3*/
!---------------------------------------------------------------------*         !/*ga          4*/
!                                                                     *         !/*ga          5*/
#include <lcs/double.h>                                                         /*!ga          6*/
#include <fluka89/flprm1.h>                                                     /*!ga          7*/
#include <fluka89/flprm2.h>                                                     /*!ga          8*/
#include <lcs/inout.h>                                                          /*!ga          9*/
!                                                                               !/*ga         10*/
!********************************************************************           !/*ga         11*/
!                                                                               !/*ga         12*/
!     random number selection from gamma distribution                           !/*ga         13*/
!     f(x) = alam**eta*x**(eta-1)*exp(-alam*x) / gam(eta)                       !/*ga         14*/
!                                                                               !/*ga         15*/
!********************************************************************           !/*ga         16*/
!                                                                               !/*ga         17*/
      ncou=0                                                                    !/*ga         18*/
      n=eta                                                                     !/*ga         19*/
      f=eta-n                                                                   !/*ga         20*/
      if (f.eq.0.d0) go to 20                                                   !/*ga         21*/
   10 r=rang()                                                                  !/*ga         22*/
      ncou=ncou+1                                                               !/*ga         23*/
      if (ncou.ge.11) go to 20                                                  !/*ga         24*/
      if (r.lt.f/(f+2.718281828459045d0)) go to 30                              !/*ga         25*/
      yyy=log(rang()+1.0d-38)/f                                                 !/*ga         26*/
      if (abs(yyy).gt.50.d0) go to 20                                           !/*ga         27*/
      y=exp(yyy)                                                                !/*ga         28*/
      if (log(rang()+1.0d-38).gt.-y) go to 10                                   !/*ga         29*/
      go to 40                                                                  !/*ga         30*/
   20 y=0.d0                                                                    !/*ga         31*/
      go to 50                                                                  !/*ga         32*/
   30 y=1.d0-log(rang()+1.0d-38)                                                !/*ga         33*/
      if (rang().gt.y**(f-1.d0)) go to 10                                       !/*ga         34*/
   40 if (n.eq.0) go to 70                                                      !/*ga         35*/
   50 z=1.d0                                                                    !/*ga         36*/
      if (n.le.0) n=1                                                           !/*ga         37*/
      do 60 i=1,n                                                               !/*ga         38*/
   60 z=z*rang()                                                                !/*ga         39*/
      y=y-log(z+1.0d-38)                                                        !/*ga         40*/
   70 gamrn=y/alam                                                              !/*ga         41*/
      return                                                                    !/*ga         42*/
      end                                                                       !/*ga         43*/

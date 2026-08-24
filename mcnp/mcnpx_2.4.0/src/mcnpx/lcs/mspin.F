!     $Id: mspin.F,v 1.4 2002/03/27 18:15:11 gwm Exp $                        !/*ms          1*/
      integer function mspin (ia,e)                                             !/*ms          2*/
#include <lcs/double.h>                                                         /*!ms          3*/
#include <lcs/phtcom.h>                                                         /*!ms          4*/
      dimension fms(lms), arg(lms), aux(lms)                                    !/*ms          5*/
      psjsum=dp0                                                                !/*ms          6*/
      msp0=mod(ia,2)-1                                                          !/*ms          7*/
      u=e-de                                                                    !/*ms          8*/
      if (u.le.dp0) go to 130                                                   !/*ms          9*/
      con=0.7104*sqrt(aa*u)*cra(ia)**2                                          !/*ms         10*/
      con1=log(4.0/con)                                                         !/*ms         11*/
      fm=msp0                                                                   !/*ms         12*/
      nup=dph*(sqrt(con*(con1+200.0))-fm)+dp1                                   !/*ms         13*/
      nup=min(lms,nup)                                                          !/*ms         14*/
      do 10 i=1,nup                                                             !/*ms         15*/
   10 fms(i)=fm+2*i                                                             !/*ms         16*/
      do 20 i=1,nup                                                             !/*ms         17*/
   20 arg(i)=con1-fms(i)**2/con                                                 !/*ms         18*/
      do 30 i=1,nup                                                             !/*ms         19*/
   30 psj(i)=exp(arg(i))                                                        !/*ms         20*/
      do 40 i=1,nup                                                             !/*ms         21*/
   40 aux(i)=psj(i)*fms(i)                                                      !/*ms         22*/
      psmax=aux(1)                                                              !/*ms         23*/
      do 50 i=2,nup                                                             !/*ms         24*/
      psmax=max(psmax,aux(i))                                                   !/*ms         25*/
      if (aux(i).le.0.001*psmax) go to 60                                       !/*ms         26*/
   50 continue                                                                  !/*ms         27*/
      go to 70                                                                  !/*ms         28*/
   60 nup=i                                                                     !/*ms         29*/
   70 continue                                                                  !/*ms         30*/
      psjsum=dp0                                                                !/*ms         31*/
      do 80 i=1,nup                                                             !/*ms         32*/
      psjsum=psjsum+aux(i)                                                      !/*ms         33*/
   80 psj(i)=psjsum                                                             !/*ms         34*/
      if (nup.ge.lms) go to 100                                                 !/*ms         35*/
      do 90 i=nup,lms                                                           !/*ms         36*/
   90 psj(i)=psjsum                                                             !/*ms         37*/
  100 continue                                                                  !/*ms         38*/
      if (psjsum.le.dp0) go to 130                                              !/*ms         39*/
      pp=psjsum*rang()                                                          !/*ms         40*/
      do 110 i=1,nup                                                            !/*ms         41*/
      if (pp.le.psj(i)) go to 120                                               !/*ms         42*/
  110 continue                                                                  !/*ms         43*/
      i=nup                                                                     !/*ms         44*/
  120 continue                                                                  !/*ms         45*/
      msp=msp0+2*i                                                              !/*ms         46*/
      mspin=msp                                                                 !/*ms         47*/
      return                                                                    !/*ms         48*/
  130 continue                                                                  !/*ms         49*/
      psjsum=dp0                                                                !/*ms         50*/
      i=1                                                                       !/*ms         51*/
      go to 120                                                                 !/*ms         52*/
      end                                                                       !/*ms         53*/

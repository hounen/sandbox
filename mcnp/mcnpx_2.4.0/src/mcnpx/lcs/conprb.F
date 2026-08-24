!     $Id: conprb.F,v 1.4 2002/03/27 18:14:58 gwm Exp $                       !/*co          1*/
      subroutine conprb (e,e1,ia)                                               !/*co          2*/
#include <lcs/double.h>                                                         /*!co          3*/
#include <lcs/phtcom.h>                                                         /*!co          4*/
      dimension fms(lms), arg(lms)                                              !/*co          5*/
      dimension j0(lms), aux(lms)                                               !/*co          6*/
      psjsum=dp0                                                                !/*co          7*/
      pnorm=dp0                                                                 !/*co          8*/
      msp0=mod(ia,2)-1                                                          !/*co          9*/
      u=e1-de                                                                   !/*co         10*/
      if (u.le.dp0) return                                                      !/*co         11*/
      con=0.7104*sqrt(aa*u)*cra(ia)**2                                          !/*co         12*/
      con1=log(4.0/con)                                                         !/*co         13*/
      fm=msp0                                                                   !/*co         14*/
      dele=e-e1                                                                 !/*co         15*/
      nup=dph*(sqrt(con*(con1+200.0))-fm)+dp1                                   !/*co         16*/
      nup=min(lms,nup)                                                          !/*co         17*/
      do 10 i=1,nup                                                             !/*co         18*/
   10 fms(i)=fm+2*i                                                             !/*co         19*/
      do 20 i=1,nup                                                             !/*co         20*/
   20 arg(i)=con1-fms(i)**2/con                                                 !/*co         21*/
      do 30 i=1,nup                                                             !/*co         22*/
   30 psj(i)=exp(arg(i))                                                        !/*co         23*/
      do 40 i=1,nup                                                             !/*co         24*/
   40 psj(i)=psj(i)*fms(i)                                                      !/*co         25*/
      do 50 i=1,nup                                                             !/*co         26*/
   50 pnorm=pnorm+psj(i)                                                        !/*co         27*/
      do 60 i=1,nup                                                             !/*co         28*/
   60 j0(i)=max(abs(msp0+i+i-isp0)/2,1)                                         !/*co         29*/
      if (dele.le.dp0) go to 80                                                 !/*co         30*/
      do 70 i=1,nup                                                             !/*co         31*/
      aux(i)=psj(i)*fj(j0(i))*(0.1*c1*cra(ia)*dele)**(2*j0(i)-2)                !/*co         32*/
   70 continue                                                                  !/*co         33*/
      go to 100                                                                 !/*co         34*/
   80 continue                                                                  !/*co         35*/
      do 90 i=1,nup                                                             !/*co         36*/
      aux(i)=dp0                                                                !/*co         37*/
      if (j0(i).ne.1) go to 90                                                  !/*co         38*/
      aux(i)=psj(i)                                                             !/*co         39*/
   90 continue                                                                  !/*co         40*/
  100 continue                                                                  !/*co         41*/
      if (isp0.eq.1) aux(1)=dp0                                                 !/*co         42*/
      psmax=aux(1)                                                              !/*co         43*/
      do 110 i=2,nup                                                            !/*co         44*/
      psmax=max(psmax,aux(i))                                                   !/*co         45*/
      if (aux(i).lt.0.001*psmax) go to 120                                      !/*co         46*/
  110 continue                                                                  !/*co         47*/
      go to 130                                                                 !/*co         48*/
  120 nup=i                                                                     !/*co         49*/
  130 continue                                                                  !/*co         50*/
      nstat=nup                                                                 !/*co         51*/
      do 140 i=1,nup                                                            !/*co         52*/
      psjsum=psjsum+aux(i)                                                      !/*co         53*/
  140 psj(i)=psjsum                                                             !/*co         54*/
      if (nup.ge.lms) go to 160                                                 !/*co         55*/
      do 150 i=nup,lms                                                          !/*co         56*/
  150 psj(i)=psjsum                                                             !/*co         57*/
  160 continue                                                                  !/*co         58*/
      if (pnorm.le.dp0) return                                                  !/*co         59*/
      do 170 i=1,lms                                                            !/*co         60*/
  170 psj(i)=psj(i)/pnorm                                                       !/*co         61*/
      psjsum=psj(lms)                                                           !/*co         62*/
      return                                                                    !/*co         63*/
      end                                                                       !/*co         64*/

!     $Id: scattl.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                       !/*sc          1*/
      subroutine scattl (inc,eke1,itfas,efas,alpfas,betfas,gamfas)              !/*sc          2*/
! SCATT      calls...  ERRORF  SQRT  FLTRNF  EXP  ALOG  AZIRN                   !/*sc          3*/
!     SCATT  called by...   HYD                                                 !/*sc          4*/
#include <lcs/double.h>                                                         /*!sc          5*/
!                                                                               !/*sc          6*/
!      inc = type of incident particle                                          !/*sc          7*/
!               = 0  proton                                                     !/*sc          8*/
!               = 1  neutron                                                    !/*sc          9*/
!               = 2  pi +                                                       !/*sc         10*/
!               = 3  not used                                                   !/*sc         11*/
!               = 4  pi -                                                       !/*sc         12*/
!      eke1 = kinetic energy of incident particle(mev)                          !/*sc         13*/
!      itfas = type of particle (same as above)                                 !/*sc         14*/
!      efas = kinetic energy of particles (mev)                                 !/*sc         15*/
!      alpfas = x-direction cosine                                              !/*sc         16*/
!      betfas = y-direction cosine                                              !/*sc         17*/
!      gamfas = z-direction cosine                                              !/*sc         18*/
!      rands = location of random number sequence                               !/*sc         19*/
!                                                                               !/*sc         20*/
!               subroutines call ed  flran  and  azio.     both are used        !/*sc         21*/
!                                                                               !/*sc         22*/
      dimension itfas(2), efas(2), alpfas(2), betfas(2), gamfas(2)              !/*sc         23*/
#ifdef CHEAP                                                                    /*!sc         24*/
      double precision m1, m2                                                   !/*sc         25*/
#else                                                                           /*!sc         26*/
      real m1, m2                                                               !/*sc         27*/
#endif                                                                          /*!sc         28*/
      data m2 /940.075d0/                                                       !/*sc         29*/
      t2=m2*m2                                                                  !/*sc         30*/
      inc1=inc+1                                                                !/*sc         31*/
      go to (10,10,20,30,40), inc1                                              !/*sc         32*/
   10 m1=940.075d0                                                              !/*sc         33*/
      go to 60                                                                  !/*sc         34*/
   20 b=7.040d-6                                                                !/*sc         35*/
      go to 50                                                                  !/*sc         36*/
   30 call errorf ('scattl')                                                    !/*sc         37*/
   40 b=7.575d-6                                                                !/*sc         38*/
   50 m1=139.89d0                                                               !/*sc         39*/
   60 e1=eke1+m1                                                                !/*sc         40*/
      t1=m1*m1                                                                  !/*sc         41*/
      p1=sqrt(e1*e1-t1)                                                         !/*sc         42*/
      if (inc1.gt.2) go to 70                                                   !/*sc         43*/
      b=7.26d-6+(3.13d-11)*p1                                                   !/*sc         44*/
   70 etlab=e1+m2                                                               !/*sc         45*/
      beta=p1/etlab                                                             !/*sc         46*/
      gamma=dp1/sqrt(dp1-beta*beta)                                             !/*sc         47*/
      etcm=etlab/gamma                                                          !/*sc         48*/
      t3=t1+t2                                                                  !/*sc         49*/
      t4=t1-t2                                                                  !/*sc         50*/
      t4=t4*t4                                                                  !/*sc         51*/
      t5=etcm*etcm                                                              !/*sc         52*/
      t6=t5*t5                                                                  !/*sc         53*/
      t7=b*(t6-2.d0*t5*t3+t4)/(2.d0*t5)                                         !/*sc         54*/
      t8=rang()                                                                 !/*sc         55*/
      tb=t7                                                                     !/*sc         56*/
      if (t7.gt.5.d1) tb=5.d1                                                   !/*sc         57*/
      cst=dp1+log(dp1-t8*(dp1-exp(-tb)))/t7                                     !/*sc         58*/
      snt=sqrt(dp1-cst*cst)                                                     !/*sc         59*/
      call azirn (t9,t10)                                                       !/*sc         60*/
      ecm1=(t5+t1-t2)/(2.d0*etcm)                                               !/*sc         61*/
      pcm1=sqrt(ecm1*ecm1-t1)                                                   !/*sc         62*/
      itfas(1)=inc                                                              !/*sc         63*/
      itfas(2)=dp0                                                              !/*sc         64*/
      t11=gamma*(ecm1+pcm1*cst*beta)-m1                                         !/*sc         65*/
      t12=etlab-t11-m1-m2                                                       !/*sc         66*/
      plab1=sqrt(dp2*m1*t11+t11*t11)                                            !/*sc         67*/
      plab2=sqrt(dp2*m2*t12+t12*t12)                                            !/*sc         68*/
      efas(1)=t11                                                               !/*sc         69*/
      efas(2)=t12                                                               !/*sc         70*/
      alpfas(1)=pcm1*snt*t9                                                     !/*sc         71*/
      alpfas(2)=-alpfas(1)                                                      !/*sc         72*/
      alpfas(1)=alpfas(1)/plab1                                                 !/*sc         73*/
      alpfas(2)=alpfas(2)/plab2                                                 !/*sc         74*/
      betfas(1)=pcm1*snt*t10                                                    !/*sc         75*/
      betfas(2)=-betfas(1)                                                      !/*sc         76*/
      betfas(1)=betfas(1)/plab1                                                 !/*sc         77*/
      betfas(2)=betfas(2)/plab2                                                 !/*sc         78*/
      gamfas(1)=gamma*(pcm1*cst+beta*ecm1)                                      !/*sc         79*/
      gamfas(2)=-gamfas(1)+p1                                                   !/*sc         80*/
      gamfas(1)=gamfas(1)/plab1                                                 !/*sc         81*/
      gamfas(2)=gamfas(2)/plab2                                                 !/*sc         82*/
      return                                                                    !/*sc         83*/
      end                                                                       !/*sc         84*/

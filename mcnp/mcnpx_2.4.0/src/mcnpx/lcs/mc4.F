!     $Id: mc4.F,v 1.4 2002/03/27 18:15:10 gwm Exp $                          !/*mc          1*/
      subroutine mc4                                                            !/*mc          2*/
#include <lcs/double.h>                                                         /*!mc          3*/
#include <lcs/mcm.h>                                                            /*!mc          4*/
#include <lcs/inout.h>                                                          /*!mc          5*/
      data iprnt /0/                                                            !/*mc          6*/
      if (zkz.eq.dp0) return                                                    !/*mc          7*/
      do 10 ihk=1,10                                                            !/*mc          8*/
      anumbe(ihk)=anumbe(ihk)/zkz                                               !/*mc          9*/
      einct(ihk)=einct(ihk)/zkz                                                 !/*mc         10*/
      yield(ihk)=yield(ihk)/zkz                                                 !/*mc         11*/
   10 avere(ihk)=avere(ihk)/zkz                                                 !/*mc         12*/
      estart=estart/zkz                                                         !/*mc         13*/
      tarmat=tarmat/zkz                                                         !/*mc         14*/
      btotpr=btotpr/zkz                                                         !/*mc         15*/
      amornt=amornt/zkz                                                         !/*mc         16*/
      amrnst=amrnst/zkz                                                         !/*mc         17*/
      ernt=ernt/zkz                                                             !/*mc         18*/
      ernprt=ernprt/zkz                                                         !/*mc         19*/
      btot=btot/zkz                                                             !/*mc         20*/
      etot=dp0                                                                  !/*mc         21*/
      etotpr=dp0                                                                !/*mc         22*/
      do 20 i=1,5                                                               !/*mc         23*/
      etot=etot+einct(i)+resmas(i)*anumbe(i)                                    !/*mc         24*/
   20 etotpr=etotpr+einct(i+5)+resmas(i)*anumbe(i+5)                            !/*mc         25*/
      esum=dp0                                                                  !/*mc         26*/
      esumpr=dp0                                                                !/*mc         27*/
      do 30 i=1,5                                                               !/*mc         28*/
      esum=esum+avere(i)                                                        !/*mc         29*/
   30 esumpr=esumpr+avere(i+5)                                                  !/*mc         30*/
      if (iprnt.eq.0) return                                                    !/*mc         31*/
      write (io,60) zkz                                                         !/*mc         32*/
      write (io,70) (einct(i),i=1,10)                                           !/*mc         33*/
      write (io,80) (anumbe(i),i=1,10)                                          !/*mc         34*/
      do 40 i=1,5                                                               !/*mc         35*/
      avere(i)=avere(i)+(cutl(i)-resmas(i))*yield(i)                            !/*mc         36*/
   40 avere(i+5)=avere(i+5)+(cutl(i)-resmas(i))*yield(i+5)                      !/*mc         37*/
      write (io,90) (avere(ihk),ihk=1,10)                                       !/*mc         38*/
      write (io,100) (yield(i),i=1,10)                                          !/*mc         39*/
      bprlab=etotpr-esumpr-resmas(1)*anumbe(6)-resmas(2)*anumbe(7)              !/*mc         40*/
      btolab=etot-esum-resmas(1)*anumbe(1)-resmas(2)*anumbe(2)                  !/*mc         41*/
      write (io,110) estart                                                     !/*mc         42*/
      aesum=dp0                                                                 !/*mc         43*/
      do 50 i=1,5                                                               !/*mc         44*/
   50 aesum=aesum+resmas(i)*yield(i+5)+avere(i+5)                               !/*mc         45*/
      estarp=etotpr+tarmat-amrnst-ernprt-aesum                                  !/*mc         46*/
      write (io,120) estarp                                                     !/*mc         47*/
      write (io,130) btolab                                                     !/*mc         48*/
      write (io,140) btot                                                       !/*mc         49*/
      write (io,150) btotpr                                                     !/*mc         50*/
      write (io,160) bprlab                                                     !/*mc         51*/
      write (io,170) ernt                                                       !/*mc         52*/
      write (io,180) ernprt                                                     !/*mc         53*/
      write (io,190) tarmat                                                     !/*mc         54*/
      write (io,200) amornt,amrnst                                              !/*mc         55*/
      write (io,210) nmlteo,count4                                              !/*mc         56*/
      write (io,220) count5,check7                                              !/*mc         57*/
      write (io,230) nttme                                                      !/*mc         58*/
      write (io,240) check8                                                     !/*mc         59*/
      write (io,250) count0,count1,count2,count3,(fug(i),i=1,5)                 !/*mc         60*/
      return                                                                    !/*mc         61*/
!                                                                               !/*mc         62*/
   60 format (1h1,'number of successful collisions=',f15.2)                     !/*mc         63*/
   70 format ('0',15x,'average ke.(mev) of incident particles'/('0',15x,        !/*mc         64*/
     & 5e16.7))                                                                 !/*mc         65*/
   80 format ('0',15x,'average type of incident particles'/('0',15x,5e16        !/*mc         66*/
     & .7))                                                                     !/*mc         67*/
   90 format (1h0,52haverage energy received by unscaled particles in la        !/*mc         68*/
     &b/5(3x,d16.9)/1x,50haverage energy received by scaled particles in        !/*mc         69*/
     & lab/5(3x,d16.9))                                                         !/*mc         70*/
  100 format (1h0,36haverage yield for unscaled particles,/5(3x,d16.9),/        !/*mc         71*/
     & 1x,34haverage yield for scaled particles,/5(3x,d16.9))                   !/*mc         72*/
  110 format (1h0,52haverage residual excitation energy before scaling =        !/*mc         73*/
     & ,f12.3,4h mev)                                                           !/*mc         74*/
  120 format (1h0,51haverage residual excitation energy after scaling =         !/*mc         75*/
     & ,f12.3,4h mev)                                                           !/*mc         76*/
  130 format (1h0,41hresidual unscaled energy in lab system = ,f12.3,           !/*mc         77*/
     & 4h mev)                                                                  !/*mc         78*/
  140 format (1h0,40hresidual unscaled energy in cm system = ,f12.3,4h m        !/*mc         79*/
     &ev)                                                                       !/*mc         80*/
  150 format (1h0,38hresidual scaled energy in cm system = ,f12.3,4h mev        !/*mc         81*/
     & )                                                                        !/*mc         82*/
  160 format (1h0,39hresidual scaled energy in lab system = ,f12.3,4h me        !/*mc         83*/
     &v)                                                                        !/*mc         84*/
  170 format (1h0,41haverage unscaled nuclear recoil energy = ,f12.3,           !/*mc         85*/
     & 4h mev)                                                                  !/*mc         86*/
  180 format (1h0,42haverage scaled-up nuclear recoil energy = ,f12.3,4h        !/*mc         87*/
     & mev)                                                                     !/*mc         88*/
  190 format (1h0,14htarget mass = ,f16.3,1x,3hmev)                             !/*mc         89*/
  200 format (1h0,50haverage mass of residual nucleus before scaling =          !/*mc         90*/
     & ,f12.3,4h mev/1x,50haverage mass of residual nucleus after scalin        !/*mc         91*/
     &g  = ,f12.3,4h mev)                                                       !/*mc         92*/
  210 format (1h0,72hnumber of times mass of recoil nucleus was less tha        !/*mc         93*/
     &n or equal to zero   ,/,1x,8hunscaled,i10,5x,6hscaled,f10.0)              !/*mc         94*/
  220 format (1h0,60hnumber of times recoil unscaled energy is < 0 when         !/*mc         95*/
     &estar < 0,f10.0,/,1x,31hnumber of times estar lt. 0. = ,f10.0)            !/*mc         96*/
  230 format (1h0,87hnumber of times energy equations for weight and rec        !/*mc         97*/
     &oiling nucleus were not satisfied = ,i5)                                  !/*mc         98*/
  240 format (1h0,83hnumber of times expression used in conservation of         !/*mc         99*/
     &transverse momentum is le. 0. = ,f10.0)                                   !/*mc        100*/
  250 format (1h0,34hnumber of times ecm not maximum = ,f10.1,//,35h num        !/*mc        101*/
     &ber of times a11 equals zero = ,f10.1,//,61h number of times solut        !/*mc        102*/
     &ion to quadratic is zero or negative = ,f10.1,//,90h number of tim        !/*mc        103*/
     &es energy and momentum equation is satisfied to better than .001 p        !/*mc        104*/
     &er cent = ,f10.1,//,1h ,52hmultiplicity factors for the five emerg        !/*mc        105*/
     &ent particles,/,5f16.3)                                                   !/*mc        106*/
      end                                                                       !/*mc        107*/

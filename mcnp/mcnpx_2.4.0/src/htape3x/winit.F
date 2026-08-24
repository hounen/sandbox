!     $Id: winit.F,v 1.4 2002/03/27 18:14:04 gwm Exp $                        !/*wi          1*/
      subroutine winit                                                          !/*wi          2*/
!------ initialize weight parameters                                            !/*wi          3*/
#include <htape3x/com32b.h>                                                     /*!wi          4*/
#include <htape3x/wtpars.h>                                                     /*!wi          5*/
      dimension orms(2)                                                         !/*wi          6*/
      pi=2.0*acos(dp0)                                                          !/*wi          7*/
      do 50 i=1,2                                                               !/*wi          8*/
      si(i)=sqrt(1.0-co(i)**2)                                                  !/*wi          9*/
      rap(i)=1.0/ap(i)                                                          !/*wi         10*/
      rbp(i)=1.0/bp(i)                                                          !/*wi         11*/
      isopt(i)=isopt(i)+1                                                       !/*wi         12*/
      go to (10,20,30,10,40), isopt(i)                                          !/*wi         13*/
   10 orms(i)=pi*ap(i)*bp(i)                                                    !/*wi         14*/
      go to 50                                                                  !/*wi         15*/
   20 orms(i)=pi*ap(i)*bp(i)*cp(i)*(1.0-0.5*cp(i))                              !/*wi         16*/
      go to 50                                                                  !/*wi         17*/
   30 cps(i)=cp(i)**2                                                           !/*wi         18*/
      orms(i)=2.0*pi*ap(i)*bp(i)*(1.0-exp(-0.5*cps(i)))                         !/*wi         19*/
      go to 50                                                                  !/*wi         20*/
   40 orms(i)=4.0*ap(i)*bp(i)                                                   !/*wi         21*/
   50 continue                                                                  !/*wi         22*/
      orm=orms(1)/orms(2)                                                       !/*wi         23*/
      return                                                                    !/*wi         24*/
      end                                                                       !/*wi         25*/

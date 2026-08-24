!     $Id: weight.F,v 1.4 2002/03/27 18:14:04 gwm Exp $                       !/*we          1*/
      subroutine weight                                                         !/*we          2*/
#include <htape3x/com32b.h>                                                     /*!we          3*/
#include <htape3x/srcwt.h>                                                      /*!we          4*/
#include <htape3x/label.h>                                                      /*!we          5*/
#include <htape3x/wtpars.h>                                                     /*!we          6*/
      dimension ww(2)                                                           !/*we          7*/
      do 60 i=2,1,-1                                                            !/*we          8*/
      ww(i)=1.0                                                                 !/*we          9*/
      if (i.eq.1.and.(ww(2).le.dp0.or.isopt(1).eq.1.or.isopt(1).eq.5))          !/*we         10*/
     & go to 70                                                                 !/*we         11*/
      ww(i)=0                                                                   !/*we         12*/
      dx=x-x0p(i)                                                               !/*we         13*/
      dy=y-y0p(i)                                                               !/*we         14*/
      xp=si(i)*dx+co(i)*dy                                                      !/*we         15*/
      yp=si(i)*dy-co(i)*dx                                                      !/*we         16*/
      if (isopt(i).eq.5) go to 50                                               !/*we         17*/
      test=(rap(i)*xp)**2+(rbp(i)*yp)**2                                        !/*we         18*/
      go to (10,20,30,40), isopt(i)                                             !/*we         19*/
   10 if (test.le.1.0) ww(i)=1.0                                                !/*we         20*/
      go to 60                                                                  !/*we         21*/
   20 if (test.le.cp(i)) ww(i)=1.0-test                                         !/*we         22*/
      go to 60                                                                  !/*we         23*/
   30 if (test.le.cps(i)) ww(i)=exp(-0.5*test)                                  !/*we         24*/
      go to 60                                                                  !/*we         25*/
   40 ww(i)=wght4(test)                                                         !/*we         26*/
      go to 60                                                                  !/*we         27*/
   50 if (abs(xp).le.ap(i).and.abs(yp).le.bp(i)) ww(i)=1.0                      !/*we         28*/
   60 continue                                                                  !/*we         29*/
   70 fwti=orm*ww(2)                                                            !/*we         30*/
      if (ww(1).gt.dp0) fwti=fwti/ww(1)                                         !/*we         31*/
      fwtcum=fwtcum+fwti                                                        !/*we         32*/
      fwscum=fwscum+fwti**2                                                     !/*we         33*/
      return                                                                    !/*we         34*/
      end                                                                       !/*we         35*/

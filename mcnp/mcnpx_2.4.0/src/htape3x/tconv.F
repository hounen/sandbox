!     $Id: tconv.F,v 1.4 2002/03/27 18:14:03 gwm Exp $                        !/*tc          1*/
      subroutine tconv (tc,ntim)                                                !/*tc          2*/
#include <htape3x/com32b.h>                                                     /*!tc          3*/
      real tc                                                                   !/*tc          4*/
#include <htape3x/lpt.h>                                                        /*!tc          5*/
!------ compute time convolution tally contributions                            !/*tc          6*/
#include <htape3x/contrl.h>                                                     /*!tc          7*/
!------ contrl only needed to supply parameters for bndy                        !/*tc          8*/
#include <htape3x/bndy.h>                                                       /*!tc          9*/
#include <htape3x/ctconv.h>                                                     /*!tc         10*/
#include <htape3x/ctcon.h>                                                      /*!tc         11*/
!------ as a precaution, define local equivalents of sub arguments              !/*tc         12*/
      t=tc                                                                      !/*tc         13*/
      n=ntim                                                                    !/*tc         14*/
      if (itopt2.eq.1) then                                                     !/*tc         15*/
        test=t+tpeak                                                            !/*tc         16*/
        tim1=dp0                                                                !/*tc         17*/
      else                                                                      !/*tc         18*/
        tmin=min(max(aa,dp0-t),bb)                                              !/*tc         19*/
      endif                                                                     !/*tc         20*/
      do 70 i=1,n                                                               !/*tc         21*/
      ta(i)=dp0                                                                 !/*tc         22*/
      if (itopt2.eq.1) then                                                     !/*tc         23*/
        if (tim1.le.test.and.test.lt.timb(i)) ta(i)=1.0                         !/*tc         24*/
        tim1=timb(i)                                                            !/*tc         25*/
        go to 70                                                                !/*tc         26*/
      endif                                                                     !/*tc         27*/
      tmax=max(min(bb,timb(i)-t),aa)                                            !/*tc         28*/
      go to (70,10,20,30,40,55), itopt2                                         !/*tc         29*/
   10 ta(i)=twit1*(tmax-tmin)                                                   !/*tc         30*/
      go to 60                                                                  !/*tc         31*/
   20 xmin=twit1*(tmin-tpeak)                                                   !/*tc         32*/
      xmax=twit1*(tmax-tpeak)                                                   !/*tc         33*/
      ta(i)=0.25*(xmax*(3.0-xmax**2)-xmin*(3.0-xmin**2))                        !/*tc         34*/
      go to 60                                                                  !/*tc         35*/
   30 xmin=twit1*abs(tmin-tpeak)                                                !/*tc         36*/
      xmax=twit1*abs(tmax-tpeak)                                                !/*tc         37*/
      if (tmin.lt.tpeak.and.tpeak.lt.tmax) then                                 !/*tc         38*/
        ta(i)=twit2*(erf_rep(xmax)+erf_rep(xmin))                               !/*tc         39*/
      else                                                                      !/*tc         40*/
        ta(i)=twit2*abs(erf_rep(xmax)-erf_rep(xmin))                            !/*tc         41*/
      endif                                                                     !/*tc         42*/
      go to 60                                                                  !/*tc         43*/
   40 do 50 j=1,ntprb                                                           !/*tc         44*/
      if (tsrc(j).le.tmin) go to 50                                             !/*tc         45*/
      if (tmax.le.tsrc(j)) then                                                 !/*tc         46*/
        ta(i)=ta(i)+tprb(j)*(tmax-tmin)                                         !/*tc         47*/
        go to 60                                                                !/*tc         48*/
      else                                                                      !/*tc         49*/
        ta(i)=ta(i)+tprb(j)*(tsrc(j)-tmin)                                      !/*tc         50*/
        tmin=tsrc(j)                                                            !/*tc         51*/
      endif                                                                     !/*tc         52*/
   50 continue                                                                  !/*tc         53*/
      go to 60                                                                  !/*tc         54*/
   55 continue                                                                  !/*tc         55*/
      xmin=twit1*(tmin-tpeak)                                                   !/*tc         56*/
      xmax=twit1*(tmax-tpeak)                                                   !/*tc         57*/
      if (xmax.le.0.0) then                                                     !/*tc         58*/
        ta(i)=(xmax-xmin)*(1.0+0.5*(xmax+xmin))                                 !/*tc         59*/
      elseif (xmin.ge.0.0) then                                                 !/*tc         60*/
        ta(i)=(xmax-xmin)*(1.0-0.5*(xmax+xmin))                                 !/*tc         61*/
      else                                                                      !/*tc         62*/
        ta(i)=xmax-xmin-0.5*(xmax**2+xmin**2)                                   !/*tc         63*/
      endif                                                                     !/*tc         64*/
   60 tmin=min(max(aa,timb(i)-t),bb)                                            !/*tc         65*/
   70 continue                                                                  !/*tc         66*/
      return                                                                    !/*tc         67*/
      end                                                                       !/*tc         68*/

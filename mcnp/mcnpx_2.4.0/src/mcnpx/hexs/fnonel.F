!     $Id: fnonel.F,v 1.4 2002/03/27 18:14:49 gwm Exp $                       !/*fn          1*/
      function fnonel (it,a,e)                                                  !/*fn          2*/
#include <lcs/double.h>                                                         /*!fn          3*/
#include <hexs/dgmcom.h>                                                        /*!fn          4*/
      external nebd                                                             !/*fn          5*/
!***********************************************************************        !/*fn          6*/
      if (e.lt.3.d0) go to 120                                                  !/*fn          7*/
      e0=e                                                                      !/*fn          8*/
      a0=a                                                                      !/*fn          9*/
      if (it.eq.1) then                                                         !/*fn         10*/
        go to 10                                                                !/*fn         11*/
      elseif (it.eq.2) then                                                     !/*fn         12*/
        go to 60                                                                !/*fn         13*/
      else                                                                      !/*fn         14*/
        stop                                                                    !/*fn         15*/
      endif                                                                     !/*fn         16*/
   10 continue                                                                  !/*fn         17*/
      do 20 i=1,iedgmp                                                          !/*fn         18*/
      if (e0.le.edgmp(i)) go to 30                                              !/*fn         19*/
   20 continue                                                                  !/*fn         20*/
      go to 120                                                                 !/*fn         21*/
   30 continue                                                                  !/*fn         22*/
      ie=i                                                                      !/*fn         23*/
      if (ie.eq.1) then                                                         !/*fn         24*/
        if (e0.gt.0.99d0*edgmp(1)) then                                         !/*fn         25*/
          ie=2                                                                  !/*fn         26*/
        else                                                                    !/*fn         27*/
          go to 120                                                             !/*fn         28*/
        endif                                                                   !/*fn         29*/
      endif                                                                     !/*fn         30*/
      do 40 i=1,iadgmp                                                          !/*fn         31*/
      if (a0.lt.adgmp(i)) go to 50                                              !/*fn         32*/
   40 continue                                                                  !/*fn         33*/
      i=iadgmp                                                                  !/*fn         34*/
   50 continue                                                                  !/*fn         35*/
      ia=i                                                                      !/*fn         36*/
      if (ia.eq.1) go to 120                                                    !/*fn         37*/
      r=(e0-edgmp(ie-1))/(edgmp(ie)-edgmp(ie-1))                                !/*fn         38*/
      x1=xdgmp(ie-1,ia-1)+r*(xdgmp(ie,ia-1)-xdgmp(ie-1,ia-1))                   !/*fn         39*/
      x2=xdgmp(ie-1,ia)+r*(xdgmp(ie,ia)-xdgmp(ie-1,ia))                         !/*fn         40*/
      p=log(a/adgmp(ia-1))/log(adgmp(ia)/adgmp(ia-1))                           !/*fn         41*/
      go to 110                                                                 !/*fn         42*/
   60 continue                                                                  !/*fn         43*/
      do 70 i=1,iedgmn                                                          !/*fn         44*/
      if (e0.le.edgmn(i)) go to 80                                              !/*fn         45*/
   70 continue                                                                  !/*fn         46*/
      go to 120                                                                 !/*fn         47*/
   80 continue                                                                  !/*fn         48*/
      ie=i                                                                      !/*fn         49*/
      if (ie.eq.1) then                                                         !/*fn         50*/
        if (e0.gt.0.99d0*edgmn(1)) then                                         !/*fn         51*/
          ie=2                                                                  !/*fn         52*/
        else                                                                    !/*fn         53*/
          go to 120                                                             !/*fn         54*/
        endif                                                                   !/*fn         55*/
      endif                                                                     !/*fn         56*/
      do 90 i=1,iadgmn                                                          !/*fn         57*/
      if (a0.lt.adgmn(i)) go to 100                                             !/*fn         58*/
   90 continue                                                                  !/*fn         59*/
      i=iadgmn                                                                  !/*fn         60*/
  100 continue                                                                  !/*fn         61*/
      ia=i                                                                      !/*fn         62*/
      if (ia.eq.1) go to 120                                                    !/*fn         63*/
      r=(e0-edgmn(ie-1))/(edgmn(ie)-edgmn(ie-1))                                !/*fn         64*/
      x1=xdgmn(ie-1,ia-1)+r*(xdgmn(ie,ia-1)-xdgmn(ie-1,ia-1))                   !/*fn         65*/
      x2=xdgmn(ie-1,ia)+r*(xdgmn(ie,ia)-xdgmn(ie-1,ia))                         !/*fn         66*/
      p=log(a/adgmn(ia-1))/log(adgmn(ia)/adgmn(ia-1))                           !/*fn         67*/
  110 continue                                                                  !/*fn         68*/
      x=x1*(x2/x1)**p                                                           !/*fn         69*/
      go to 130                                                                 !/*fn         70*/
  120 continue                                                                  !/*fn         71*/
      x=dp0                                                                     !/*fn         72*/
  130 continue                                                                  !/*fn         73*/
      fnonel=x                                                                  !/*fn         74*/
      return                                                                    !/*fn         75*/
      end                                                                       !/*fn         76*/

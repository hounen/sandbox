!     $Id: edx1.F,v 1.6 2002/03/27 18:16:24 gwm Exp $                         !/*ed          1*/
      subroutine edx1                                                           !/*ed          2*/
! editing routine for program XSEX                                              !/*ed          3*/
#include <htape3x/com32b.h>                                                     /*!ed          4*/
#include <htape3x/label.h>                                                      /*!ed          5*/
#include <htape3x/contrl.h>                                                     /*!ed          6*/
#include <xsex3/charxs.h>                                                       /*!ed          7*/
      ncas=maxbch*maxcas                                                        !/*ed          8*/
      fcas=ncas                                                                 !/*ed          9*/
      cnorm=fnorm/fcas                                                          !/*ed         10*/
      dnorm=dp0                                                                 !/*ed         11*/
      if (ncas.gt.1) dnorm=1.0/(fcas-1.0)                                       !/*ed         12*/
      enorm=dnorm*fcas                                                          !/*ed         13*/
      b=tsels                                                                   !/*ed         14*/
      if (b.gt.0) then                                                          !/*ed         15*/
        c=abs(tlels)                                                            !/*ed         16*/
        b=enorm*b-dnorm*c**2                                                    !/*ed         17*/
        if (b.lt.dp0) b=dp0                                                     !/*ed         18*/
        b=sqrt(b)/c                                                             !/*ed         19*/
        tsels=b                                                                 !/*ed         20*/
        tlels=cnorm*tlels                                                       !/*ed         21*/
      endif                                                                     !/*ed         22*/
      b=tstot                                                                   !/*ed         23*/
      if (b.gt.0) then                                                          !/*ed         24*/
        c=abs(tltot)                                                            !/*ed         25*/
        b=enorm*b-dnorm*c**2                                                    !/*ed         26*/
        if (b.lt.dp0) b=dp0                                                     !/*ed         27*/
        b=sqrt(b)/c                                                             !/*ed         28*/
        tstot=b                                                                 !/*ed         29*/
        tltot=cnorm*tltot                                                       !/*ed         30*/
      endif                                                                     !/*ed         31*/
      b=tsnon                                                                   !/*ed         32*/
      if (b.gt.0) then                                                          !/*ed         33*/
        c=abs(tlnon)                                                            !/*ed         34*/
        b=enorm*b-dnorm*c**2                                                    !/*ed         35*/
        if (b.lt.dp0) b=dp0                                                     !/*ed         36*/
        b=sqrt(b)/c                                                             !/*ed         37*/
        tsnon=b                                                                 !/*ed         38*/
        tlnon=cnorm*tlnon                                                       !/*ed         39*/
      endif                                                                     !/*ed         40*/
      b=tsfis                                                                   !/*ed         41*/
      if (b.gt.0) then                                                          !/*ed         42*/
        c=abs(tlfis)                                                            !/*ed         43*/
        b=enorm*b-dnorm*c**2                                                    !/*ed         44*/
        if (b.lt.dp0) b=dp0                                                     !/*ed         45*/
        b=sqrt(b)/c                                                             !/*ed         46*/
        tsfis=b                                                                 !/*ed         47*/
        tlfis=cnorm*tlfis                                                       !/*ed         48*/
      endif                                                                     !/*ed         49*/
      write (io,10) tltot,tstot                                                 !/*ed         50*/
      write (io,20) tlnon,tsnon                                                 !/*ed         51*/
      if (tlnon.gt.dp0) then                                                    !/*ed         52*/
         temp=tlfis/tlnon                                                       !/*ed         53*/
      else                                                                      !/*ed         54*/
         temp=dp0                                                               !/*ed         55*/
      endif                                                                     !/*ed         56*/
      write (io,30) tlfis,tsfis,temp                                            !/*ed         57*/
      write (io,40) tlels,tsels                                                 !/*ed         58*/
      return                                                                    !/*ed         59*/
!                                                                               !/*ed         60*/
   10 format (////20x,'total cross section  =',1pd13.5,0pf6.3)                  !/*ed         61*/
   20 format (/20x,'nonelastic cross section =',1pd13.5,0pf6.3)                 !/*ed         62*/
   30 format (/25x,'fission cross section =',1pd13.5,0pf6.3/                    !/*ed         63*/
     & 25x,'fission yield =',1pd13.5/)                                          !/*ed         64*/
   40 format (/20x,'elastic cross section  =',1pd13.5,0pf6.3)                   !/*ed         65*/
      end                                                                       !/*ed         66*/

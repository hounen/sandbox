!     $Id: punp.F,v 1.4 2002/03/27 18:15:17 gwm Exp $                         !/*pu          1*/
      subroutine punp                                                           !/*pu          2*/
#include <lcs/double.h>                                                         /*!pu          3*/
#include <lcs/btcom.h>                                                          /*!pu          4*/
      if (pgvc(1)) 10,60,30                                                     !/*pu          5*/
   10 i1=-1                                                                     !/*pu          6*/
   20 return                                                                    !/*pu          7*/
   30 pgvc(1)=pgvc(1)-11.d0                                                     !/*pu          8*/
      k=pgvc(1)+2.005d0                                                         !/*pu          9*/
      do 40 i=1,11                                                              !/*pu         10*/
      curr(i)=pgvc(k)                                                           !/*pu         11*/
      pgvc(k)=dp0                                                               !/*pu         12*/
   40 k=k+1                                                                     !/*pu         13*/
   50 i1=1                                                                      !/*pu         14*/
      go to 20                                                                  !/*pu         15*/
   60 if (plvc(1)) 10,130,70                                                    !/*pu         16*/
   70 univ=dp0                                                                  !/*pu         17*/
      l=plvc(1)                                                                 !/*pu         18*/
      k=-10                                                                     !/*pu         19*/
      do 110 i=1,l                                                              !/*pu         20*/
   80 k=k+12                                                                    !/*pu         21*/
      if (plvc(k)) 10,80,90                                                     !/*pu         22*/
   90 if (plvc(k)-univ) 110,100,100                                             !/*pu         23*/
  100 univ=plvc(k)                                                              !/*pu         24*/
      m=k                                                                       !/*pu         25*/
  110 continue                                                                  !/*pu         26*/
      plvc(m)=dp0                                                               !/*pu         27*/
      do 120 i=1,11                                                             !/*pu         28*/
      m=m+1                                                                     !/*pu         29*/
      curr(i)=plvc(m)                                                           !/*pu         30*/
  120 plvc(m)=dp0                                                               !/*pu         31*/
      plvc(1)=plvc(1)-dp1                                                       !/*pu         32*/
      go to 50                                                                  !/*pu         33*/
  130 i1=0                                                                      !/*pu         34*/
      ac=amasno                                                                 !/*pu         35*/
      if (no.gt.2) go to 140                                                    !/*pu         36*/
      zc=zee                                                                    !/*pu         37*/
      ac=amasno+dp1                                                             !/*pu         38*/
! ac-compound nuc,zc=chg.compound,ar=mass cascade resid.nucleus                 !/*pu         39*/
      if (no.eq.1) go to 150                                                    !/*pu         40*/
      go to 160                                                                 !/*pu         41*/
  140 if (no.lt.5) go to 150                                                    !/*pu         42*/
      zc=zee-dp1                                                                !/*pu         43*/
      go to 160                                                                 !/*pu         44*/
  150 zc=zee+dp1                                                                !/*pu         45*/
  160 ar=ac-count(1)-count(2)                                                   !/*pu         46*/
      ze=count(1)+count(3)-count(5)                                             !/*pu         47*/
      if (ar) 170,180,190                                                       !/*pu         48*/
  170 count(7)=count(7)+dp1                                                     !/*pu         49*/
      go to 210                                                                 !/*pu         50*/
  180 if (zc.eq.ze) go to 20                                                    !/*pu         51*/
      count(8)=count(8)+dp1                                                     !/*pu         52*/
      go to 210                                                                 !/*pu         53*/
  190 if (ar.ge.(zc-ze)) go to 200                                              !/*pu         54*/
      count(9)=count(9)+dp1                                                     !/*pu         55*/
      go to 210                                                                 !/*pu         56*/
  200 if (zc.ge.ze) go to 20                                                    !/*pu         57*/
      count(10)=count(10)+dp1                                                   !/*pu         58*/
  210 if (begru.eq.dp1) go to 250                                               !/*pu         59*/
      begru=begru-dp1                                                           !/*pu         60*/
  220 do 230 i=1,12                                                             !/*pu         61*/
      cc(i)=pcc(i)                                                              !/*pu         62*/
  230 ipec(i)=nip(i)                                                            !/*pu         63*/
      do 240 i=1,5                                                              !/*pu         64*/
  240 pnbc(i)=ppnb(i)                                                           !/*pu         65*/
      nor=nor-1                                                                 !/*pu         66*/
      count(6)=-dp1                                                             !/*pu         67*/
      go to 20                                                                  !/*pu         68*/
  250 begru=-dp1                                                                !/*pu         69*/
      go to 220                                                                 !/*pu         70*/
! 1. picks up last 11 items in pgvc and stores                                  !/*pu         71*/
! them in curr.  stores zero in those 11 pgvc cells.                            !/*pu         72*/
!     group is greater than 1st in all other groups.  stores                    !/*pu         73*/
!     2-12th items in curr--zeroes items 1-12 in plvc group.                    !/*pu         74*/
      end                                                                       !/*pu         75*/

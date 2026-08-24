!     $Id: spcn.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                         !/*sp          1*/
      subroutine spcn                                                           !/*sp          2*/
#include <lcs/double.h>                                                         /*!sp          3*/
#include <lcs/btcom.h>                                                          /*!sp          4*/
      fmax(4)=dp0                                                               !/*sp          5*/
      fmax(5)=dp0                                                               !/*sp          6*/
      i6=curr(1)-1.95d0                                                         !/*sp          7*/
      wk=wkrpn(i2)                                                              !/*sp          8*/
      if (i6.eq.2) go to 10                                                     !/*sp          9*/
      univ=pnms                                                                 !/*sp         10*/
      go to 20                                                                  !/*sp         11*/
   10 univ=poms                                                                 !/*sp         12*/
   20 call bovera (wk,univ,unive)                                               !/*sp         13*/
      value1=dp0                                                                !/*sp         14*/
      if (wk-22.d1) 30,30,80                                                    !/*sp         15*/
   30 if (i6.eq.2) go to 60                                                     !/*sp         16*/
      fmax(5)=0.35d-27*unive                                                    !/*sp         17*/
!     (pip+p)s.p.                                                               !/*sp         18*/
      fmax(6)=2.3d-27*unive                                                     !/*sp         19*/
!     (pim+p)s.p.                                                               !/*sp         20*/
      fmax(1)=0.20d-24*unive                                                    !/*sp         21*/
!     (pip+p)sc                                                                 !/*sp         22*/
      fmax(3)=.0451d-24*unive                                                   !/*sp         23*/
!     (pim+p)ex                                                                 !/*sp         24*/
   40 fmax(2)=0.023d-24*unive                                                   !/*sp         25*/
!     (pim-p)sc                                                                 !/*sp         26*/
   50 return                                                                    !/*sp         27*/
   60 fmax(6)=1.35d-27*unive                                                    !/*sp         28*/
!     (pi0+p)s.p.                                                               !/*sp         29*/
      fmax(1)=89.2d-27*unive                                                    !/*sp         30*/
!     (pi0+p)sc                                                                 !/*sp         31*/
      fmax(2)=.0451d-24*unive                                                   !/*sp         32*/
!     (pi0+p)ex                                                                 !/*sp         33*/
   70 fmax(3)=fmax(1)                                                           !/*sp         34*/
!     (pi0+n)sc                                                                 !/*sp         35*/
      fmax(4)=fmax(2)                                                           !/*sp         36*/
!     (pi0+n)ex                                                                 !/*sp         37*/
      fmax(7)=fmax(6)                                                           !/*sp         38*/
!     (pi0+n)s.p.                                                               !/*sp         39*/
      go to 50                                                                  !/*sp         40*/
   80 if (wk-4.d+02) 90,90,110                                                  !/*sp         41*/
   90 if (i6.eq.2) go to 100                                                    !/*sp         42*/
      fmax(5)=4.5d-27*unive                                                     !/*sp         43*/
      fmax(6)=20.6d-27*unive                                                    !/*sp         44*/
      fmax(1)=0.20d-24                                                          !/*sp         45*/
      fmax(3)=.0451d-24                                                         !/*sp         46*/
      go to 40                                                                  !/*sp         47*/
  100 fmax(6)=12.5d-27*unive                                                    !/*sp         48*/
      fmax(1)=89.2d-27                                                          !/*sp         49*/
      fmax(2)=.0451d-24                                                         !/*sp         50*/
      go to 70                                                                  !/*sp         51*/
  110 if (wk-5.d+02) 120,120,140                                                !/*sp         52*/
  120 if (i6.eq.2) go to 130                                                    !/*sp         53*/
      fmax(1)=.113d-24                                                          !/*sp         54*/
      fmax(2)=20.5d-27*unive                                                    !/*sp         55*/
      fmax(3)=27.7d-27                                                          !/*sp         56*/
      fmax(5)=11.7d-27*unive                                                    !/*sp         57*/
      fmax(6)=21.7d-27*unive                                                    !/*sp         58*/
      go to 50                                                                  !/*sp         59*/
  130 fmax(1)=50.8d-27                                                          !/*sp         60*/
      fmax(2)=27.7d-27                                                          !/*sp         61*/
      fmax(6)=14.6d-27*unive                                                    !/*sp         62*/
      go to 70                                                                  !/*sp         63*/
  140 value1=dp1                                                                !/*sp         64*/
      if (wk-6.d+02) 150,150,170                                                !/*sp         65*/
  150 if (i6.eq.2) go to 160                                                    !/*sp         66*/
      fmax(1)=51.0d-27                                                          !/*sp         67*/
      fmax(2)=24.7d-27*unive                                                    !/*sp         68*/
      fmax(3)=15.5d-27*unive                                                    !/*sp         69*/
      fmax(5)=15.1d-27*unive                                                    !/*sp         70*/
      fmax(6)=30.4d-27*unive                                                    !/*sp         71*/
      go to 50                                                                  !/*sp         72*/
  160 fmax(1)=23.0d-27*unive                                                    !/*sp         73*/
      fmax(2)=15.5d-27*unive                                                    !/*sp         74*/
      fmax(6)=22.6d-27*unive                                                    !/*sp         75*/
      go to 70                                                                  !/*sp         76*/
  170 if (wk-8.d+02) 180,180,200                                                !/*sp         77*/
  180 if (i6.eq.2) go to 190                                                    !/*sp         78*/
      fmax(1)=33.0d-27                                                          !/*sp         79*/
      fmax(2)=26.3d-27*unive                                                    !/*sp         80*/
      fmax(3)=12.0d-27*unive                                                    !/*sp         81*/
      fmax(5)=20.1d-27*unive                                                    !/*sp         82*/
      fmax(6)=30.4d-27*unive                                                    !/*sp         83*/
      go to 50                                                                  !/*sp         84*/
  190 fmax(1)=16.0d-27*unive                                                    !/*sp         85*/
      fmax(2)=12.0d-27*unive                                                    !/*sp         86*/
      fmax(6)=22.6d-27*unive                                                    !/*sp         87*/
      go to 70                                                                  !/*sp         88*/
  200 if (i6.eq.2) go to 210                                                    !/*sp         89*/
      fmax(1)=19.3d-27*unive                                                    !/*sp         90*/
      fmax(2)=26.3d-27*unive                                                    !/*sp         91*/
      fmax(3)=8.2d-27*unive                                                     !/*sp         92*/
      fmax(5)=23.3d-27*unive                                                    !/*sp         93*/
      fmax(6)=30.4d-27*unive                                                    !/*sp         94*/
      go to 50                                                                  !/*sp         95*/
  210 fmax(1)=16.0d-27*unive                                                    !/*sp         96*/
      fmax(2)=8.2d-27*unive                                                     !/*sp         97*/
      fmax(6)=24.9d-27*unive                                                    !/*sp         98*/
      go to 70                                                                  !/*sp         99*/
      end                                                                       !/*sp        100*/

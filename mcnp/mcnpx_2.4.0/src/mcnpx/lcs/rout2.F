!     $Id: rout2.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                        !/*ro          1*/
      subroutine rout2 (t)                                                      !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension t(19)                                                           !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      i1=1                                                                      !/*ro          6*/
      value2=einc+space(12)                                                     !/*ro          7*/
      call bovera (value2,pnms,ans)                                             !/*ro          8*/
      space(14)=0.20d-24*ans                                                    !/*ro          9*/
!     pip+p                                                                     !/*ro         10*/
      space(15)=0.023d-24*ans                                                   !/*ro         11*/
!     (pim+p)el                                                                 !/*ro         12*/
      space(16)=.0451d-24*ans                                                   !/*ro         13*/
!     (pim+p)ex                                                                 !/*ro         14*/
!     180                                                                       !/*ro         15*/
      if (value1-1.d+02) 10,10,50                                               !/*ro         16*/
   10 fmax(1)=space(14)                                                         !/*ro         17*/
      fmax(2)=space(15)                                                         !/*ro         18*/
      fmax(3)=space(16)                                                         !/*ro         19*/
      s(1)=dp0                                                                  !/*ro         20*/
      s(2)=dp0                                                                  !/*ro         21*/
   20 call crdet (1,t(1),einc)                                                  !/*ro         22*/
!     (pip-p) absorption cross section                                          !/*ro         23*/
      space(17)=crdt(1)                                                         !/*ro         24*/
      if (i1) 40,40,30                                                          !/*ro         25*/
   30 fmax(4)=space(17)                                                         !/*ro         26*/
   40 return                                                                    !/*ro         27*/
   50 if (value2-26.d+02) 70,70,60                                              !/*ro         28*/
!     value2 is greater than 2600                                               !/*ro         29*/
   60 i1=0                                                                      !/*ro         30*/
      go to 40                                                                  !/*ro         31*/
   70 space(17)=dp0                                                             !/*ro         32*/
      if (value2-22.d1) 80,80,90                                                !/*ro         33*/
   80 s(1)=0.75d-27*ans                                                         !/*ro         34*/
!     (pip-p)s.p. 400mev                                                        !/*ro         35*/
      s(2)=4.7d-27*ans                                                          !/*ro         36*/
!     (pim-p)s.p. 400mev                                                        !/*ro         37*/
      go to 110                                                                 !/*ro         38*/
   90 if (value2-4.d+02) 100,100,120                                            !/*ro         39*/
  100 space(14)=0.20d-24                                                        !/*ro         40*/
      space(16)=.0451d-24                                                       !/*ro         41*/
      s(1)=7.8d-27*ans                                                          !/*ro         42*/
      s(2)=21.8d-27*ans                                                         !/*ro         43*/
!     660 mev                                                                   !/*ro         44*/
  110 if (einc-36.d1) 20,40,40                                                  !/*ro         45*/
  120 if (value2-5.d+02) 130,130,140                                            !/*ro         46*/
  130 space(14)=.113d-24                                                        !/*ro         47*/
!     250 mev                                                                   !/*ro         48*/
      space(15)=20.5d-27*ans                                                    !/*ro         49*/
!     620 mev                                                                   !/*ro         50*/
      space(16)=27.7d-27                                                        !/*ro         51*/
!     250 mev                                                                   !/*ro         52*/
      s(1)=13.8d-27*ans                                                         !/*ro         53*/
      s(2)=24.4d-27*ans                                                         !/*ro         54*/
!     800 mev                                                                   !/*ro         55*/
      i1=-1                                                                     !/*ro         56*/
      go to 110                                                                 !/*ro         57*/
  140 s(2)=30.4d-27*ans                                                         !/*ro         58*/
      space(15)=26.3d-27*ans                                                    !/*ro         59*/
!     900                                                                       !/*ro         60*/
      if (value2-6.d+02) 150,150,160                                            !/*ro         61*/
!     940,325                                                                   !/*ro         62*/
  150 space(14)=53.0d-27                                                        !/*ro         63*/
!     325                                                                       !/*ro         64*/
!     value2 lte 600                                                            !/*ro         65*/
!     325                                                                       !/*ro         66*/
      space(16)=16.2d-27                                                        !/*ro         67*/
      s(1)=15.2d-27*ans                                                         !/*ro         68*/
!     940                                                                       !/*ro         69*/
      go to 40                                                                  !/*ro         70*/
  160 if (value2-8.d+02) 170,170,180                                            !/*ro         71*/
!     1200,400                                                                  !/*ro         72*/
  170 space(14)=33.0d-27                                                        !/*ro         73*/
!     400                                                                       !/*ro         74*/
      space(16)=12.0d-27*ans                                                    !/*ro         75*/
!     400                                                                       !/*ro         76*/
      s(1)=20.9d-27*ans                                                         !/*ro         77*/
!     1200                                                                      !/*ro         78*/
      go to 40                                                                  !/*ro         79*/
  180 space(14)=19.3d-27*ans                                                    !/*ro         80*/
!     1300                                                                      !/*ro         81*/
      space(16)=8.2d-27*ans                                                     !/*ro         82*/
!     540                                                                       !/*ro         83*/
      s(1)=23.3d-27*ans                                                         !/*ro         84*/
!     1400                                                                      !/*ro         85*/
      s(2)=30.4d-27*ans                                                         !/*ro         86*/
!     sigma(a) 900+2mb for future correction                                    !/*ro         87*/
      go to 40                                                                  !/*ro         88*/
!     values of fi for both incident and cascade particles for                  !/*ro         89*/
!     charged pions(pi +or-) ,single production.  s(1)=n)s.p., s(2)=p)          !/*ro         90*/
!     s.p., space(14)=n)s, space(15)=p)d.s., space(16)=p)abs, space(17)         !/*ro         91*/
!     =p)abs  (ppac)                                                            !/*ro         92*/
      end                                                                       !/*ro         93*/

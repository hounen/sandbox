!     $Id: pfmax.F,v 1.4 2002/03/27 18:15:13 gwm Exp $                        !/*pf          1*/
      subroutine pfmax                                                          !/*pf          2*/
#include <lcs/double.h>                                                         /*!pf          3*/
#include <lcs/btcom.h>                                                          /*!pf          4*/
      i=i2                                                                      !/*pf          5*/
      if (curr(1)-dp2) 20,10,10                                                 !/*pf          6*/
   10 i=i+3                                                                     !/*pf          7*/
   20 wk=wkrpn(i)                                                               !/*pf          8*/
      if (wk-16.d1) 30,30,50                                                    !/*pf          9*/
   30 fmax(1)=0.176d-24                                                         !/*pf         10*/
      fmax(2)=0.52d-24                                                          !/*pf         11*/
      fmax(3)=dp0                                                               !/*pf         12*/
      fmax(4)=dp0                                                               !/*pf         13*/
   40 return                                                                    !/*pf         14*/
   50 call bovera (wk,dncms,univ)                                               !/*pf         15*/
      if (wk-4.d+02) 60,110,110                                                 !/*pf         16*/
   60 if (wk-3.d+02) 70,100,100                                                 !/*pf         17*/
   70 if (wk-2.d+02) 80,90,90                                                   !/*pf         18*/
   80 fmax(3)=1.95d-27*univ                                                     !/*pf         19*/
!     3 and 4 at 465 mev                                                        !/*pf         20*/
      fmax(4)=1.7d-27*univ                                                      !/*pf         21*/
      fmax(1)=0.103d-24                                                         !/*pf         22*/
      fmax(2)=0.313d-24                                                         !/*pf         23*/
      go to 40                                                                  !/*pf         24*/
   90 fmax(1)=0.09d-24                                                          !/*pf         25*/
!     1 and 2 at 35 mev                                                         !/*pf         26*/
      fmax(2)=0.26d-24                                                          !/*pf         27*/
      fmax(3)=11.4d-27*univ                                                     !/*pf         28*/
!     3 and 4 at 630 mev                                                        !/*pf         29*/
      fmax(4)=11.2d-27*univ                                                     !/*pf         30*/
      go to 40                                                                  !/*pf         31*/
  100 fmax(1)=28.0d-27*univ                                                     !/*pf         32*/
!     1 and 2 at 100 mev                                                        !/*pf         33*/
      fmax(2)=0.073d-24                                                         !/*pf         34*/
      fmax(3)=20.0d-27*univ                                                     !/*pf         35*/
!     3 and 4 at 780 mev                                                        !/*pf         36*/
      fmax(4)=14.0d-27*univ                                                     !/*pf         37*/
      go to 40                                                                  !/*pf         38*/
  110 fmax(1)=27.2d-27*univ                                                     !/*pf         39*/
!     1 and 2 at 155 mev                                                        !/*pf         40*/
      fmax(2)=48.0d-27*univ                                                     !/*pf         41*/
      fmax(3)=22.6d-27*univ                                                     !/*pf         42*/
!     3 and 4 at 1020 mev                                                       !/*pf         43*/
      fmax(4)=14.0d-27*univ                                                     !/*pf         44*/
      go to 40                                                                  !/*pf         45*/
!     fmax(1)=(p-p)s---(2)=(p-n)s---(3)=(p-p)s.p.---(4)=(p-n)s.p.               !/*pf         46*/
      end                                                                       !/*pf         47*/

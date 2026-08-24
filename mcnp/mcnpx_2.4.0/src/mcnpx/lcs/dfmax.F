!     $Id: dfmax.F,v 1.4 2002/03/27 18:15:00 gwm Exp $                        !/*df          1*/
      subroutine dfmax                                                          !/*df          2*/
#include <lcs/double.h>                                                         /*!df          3*/
#include <lcs/btcom.h>                                                          /*!df          4*/
      i=i2                                                                      !/*df          5*/
      if (curr(1)-dp2) 20,10,10                                                 !/*df          6*/
   10 i=i+3                                                                     !/*df          7*/
   20 wk=wkrpn(i)                                                               !/*df          8*/
      call bovera (wk,dncms,univ)                                               !/*df          9*/
      if (wk-56.d1) 30,30,50                                                    !/*df         10*/
   30 fmax(1)=27.2d-27*univ                                                     !/*df         11*/
!     820 mev                                                                   !/*df         12*/
!     (p-p)s                                                                    !/*df         13*/
      fmax(2)=38.0d-27*univ                                                     !/*df         14*/
!     230 mev                                                                   !/*df         15*/
!     (p-n)s                                                                    !/*df         16*/
      fmax(3)=22.6d-27*univ                                                     !/*df         17*/
!     1020 mev                                                                  !/*df         18*/
!     (p-p)s.p.                                                                 !/*df         19*/
      fmax(4)=14.0d-27*univ                                                     !/*df         20*/
!     750 mev                                                                   !/*df         21*/
!     (p-n)s.p.                                                                 !/*df         22*/
      fmax(5)=dp0                                                               !/*df         23*/
!     (p-p)d.p.                                                                 !/*df         24*/
      fmax(6)=dp0                                                               !/*df         25*/
!     (p-n)d.p.)                                                                !/*df         26*/
   40 return                                                                    !/*df         27*/
   50 if (wk-8.d+02) 60,80,80                                                   !/*df         28*/
   60 fmax(2)=37.0d-27*univ                                                     !/*df         29*/
!     250 mev                                                                   !/*df         30*/
      fmax(5)=1.9d-27*univ                                                      !/*df         31*/
!     5 an6 at 1380 mev                                                         !/*df         32*/
      fmax(6)=9.0d-27*univ                                                      !/*df         33*/
   70 fmax(1)=27.2d-27*univ                                                     !/*df         34*/
!     820 mev                                                                   !/*df         35*/
      fmax(3)=22.6d-27*univ                                                     !/*df         36*/
!     1020                                                                      !/*df         37*/
      fmax(4)=14.0d-27*univ                                                     !/*df         38*/
!     750                                                                       !/*df         39*/
      go to 40                                                                  !/*df         40*/
   80 if (wk-168.d1) 90,100,100                                                 !/*df         41*/
   90 fmax(2)=33.0d-27*univ                                                     !/*df         42*/
!     400                                                                       !/*df         43*/
      fmax(5)=10.8d-27*univ                                                     !/*df         44*/
!     5 and 6 at 2600                                                           !/*df         45*/
      fmax(6)=17.4d-27*univ                                                     !/*df         46*/
      go to 70                                                                  !/*df         47*/
  100 fmax(1)=26.3d-27*univ                                                     !/*df         48*/
!     1000                                                                      !/*df         49*/
      fmax(2)=24.7d-27*univ                                                     !/*df         50*/
!     1000                                                                      !/*df         51*/
      fmax(3)=22.6d-27*univ                                                     !/*df         52*/
!     1020                                                                      !/*df         53*/
      fmax(4)=13.5d-27*univ                                                     !/*df         54*/
!     1000                                                                      !/*df         55*/
      fmax(5)=18.0d-27*univ                                                     !/*df         56*/
      fmax(6)=23.6d-27*univ                                                     !/*df         57*/
!     3500                                                                      !/*df         58*/
      go to 40                                                                  !/*df         59*/
      end                                                                       !/*df         60*/

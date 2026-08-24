!     $Id: look.F,v 1.4 2002/03/27 18:15:10 gwm Exp $                         !/*lo          1*/
      subroutine look (arg,x,istep)                                             !/*lo          2*/
#include <lcs/double.h>                                                         /*!lo          3*/
!     ==================================================================        !/*lo          4*/
!     look is to find which region the particle is in.  it is                   !/*lo          5*/
!     the step number.                                                          !/*lo          6*/
!                                                                               !/*lo          7*/
! LOOK   called by REGB  REFL  SELPT                                            !/*lo          8*/
!     ==================================================================        !/*lo          9*/
!                                                                               !/*lo         10*/
      dimension x(17)                                                           !/*lo         11*/
!                                                                               !/*lo         12*/
!     do 1 i=1,9                                                                !/*lo         13*/
!     if(arg.gt.x(i)) go to 2                                                   !/*lo         14*/
!   1 continue                                                                  !/*lo         15*/
!     i=9                                                                       !/*lo         16*/
!   2 istep=i-1                                                                 !/*lo         17*/
!                   binary-chop method (3-4 ifs/search)                         !/*lo         18*/
      if (arg-x(9)) 10,150,160                                                  !/*lo         19*/
   10 if (arg-x(13)) 20,70,90                                                   !/*lo         20*/
   20 if (arg-x(15)) 30,50,60                                                   !/*lo         21*/
   30 if (arg-x(16)) 40,40,50                                                   !/*lo         22*/
   40 istep=16                                                                  !/*lo         23*/
      return                                                                    !/*lo         24*/
   50 istep=15                                                                  !/*lo         25*/
      return                                                                    !/*lo         26*/
   60 if (arg-x(14)) 80,80,70                                                   !/*lo         27*/
   70 istep=13                                                                  !/*lo         28*/
      return                                                                    !/*lo         29*/
   80 istep=14                                                                  !/*lo         30*/
      return                                                                    !/*lo         31*/
   90 if (arg-x(11)) 100,110,130                                                !/*lo         32*/
  100 if (arg-x(12)) 120,120,110                                                !/*lo         33*/
  110 istep=11                                                                  !/*lo         34*/
      return                                                                    !/*lo         35*/
  120 istep=12                                                                  !/*lo         36*/
      return                                                                    !/*lo         37*/
  130 if (arg-x(10)) 140,140,150                                                !/*lo         38*/
  140 istep=10                                                                  !/*lo         39*/
      return                                                                    !/*lo         40*/
  150 istep=9                                                                   !/*lo         41*/
      return                                                                    !/*lo         42*/
!          "old" b.c. region for 8 steps in rho(r)                              !/*lo         43*/
  160 if (arg-x(5)) 170,220,240                                                 !/*lo         44*/
  170 if (arg-x(7)) 180,200,210                                                 !/*lo         45*/
  180 if (arg-x(8)) 190,190,200                                                 !/*lo         46*/
  190 istep=8                                                                   !/*lo         47*/
      return                                                                    !/*lo         48*/
  200 istep=7                                                                   !/*lo         49*/
      return                                                                    !/*lo         50*/
  210 if (arg-x(6)) 230,230,220                                                 !/*lo         51*/
  220 istep=5                                                                   !/*lo         52*/
      return                                                                    !/*lo         53*/
  230 istep=6                                                                   !/*lo         54*/
      return                                                                    !/*lo         55*/
  240 if (arg-x(3)) 250,260,280                                                 !/*lo         56*/
  250 if (arg-x(4)) 270,270,260                                                 !/*lo         57*/
  260 istep=3                                                                   !/*lo         58*/
      return                                                                    !/*lo         59*/
  270 istep=4                                                                   !/*lo         60*/
      return                                                                    !/*lo         61*/
  280 if (arg-x(2)) 290,290,300                                                 !/*lo         62*/
  290 istep=2                                                                   !/*lo         63*/
      return                                                                    !/*lo         64*/
  300 if (arg-x(1)) 310,310,320                                                 !/*lo         65*/
  310 istep=1                                                                   !/*lo         66*/
      return                                                                    !/*lo         67*/
  320 istep=0                                                                   !/*lo         68*/
      return                                                                    !/*lo         69*/
      end                                                                       !/*lo         70*/

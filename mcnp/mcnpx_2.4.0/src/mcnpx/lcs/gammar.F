!     $Id: gammar.F,v 1.4 2002/03/27 18:15:04 gwm Exp $                       !/*ga          1*/
      function gammar (x)                                                       !/*ga          2*/
#include <lcs/double.h>                                                         /*!ga          3*/
!                                                                               !/*ga          4*/
!  GAMMAR computes the gamma funtion of x.                                      !/*ga          5*/
!                                                                               !/*ga          6*/
!    REFERENCES.......  GMS                                                     !/*ga          7*/
!    EXTERNALS........  EXP                                                     !/*ga          8*/
!                                                                               !/*ga          9*/
      if (x.gt.1) then                                                          !/*ga         10*/
        z=x-dp1                                                                 !/*ga         11*/
      else                                                                      !/*ga         12*/
        z=x                                                                     !/*ga         13*/
   10   continue                                                                !/*ga         14*/
        if (z.le.dp0) then                                                      !/*ga         15*/
          z=z+dp1                                                               !/*ga         16*/
          go to 10                                                              !/*ga         17*/
        endif                                                                   !/*ga         18*/
      endif                                                                     !/*ga         19*/
      t1=z+dph                                                                  !/*ga         20*/
      tzg=t1+dp5                                                                !/*ga         21*/
      t1=tzg**t1                                                                !/*ga         22*/
      t1=exp(-tzg)*t1*2.50662827465d0                                           !/*ga         23*/
      gamz=t1*(dp1+76.18009173d0/(z+dp1)-86.50532033d0/(z+dp2)+24.014098        !/*ga         24*/
     & 22d0/(z+dp3)-1.231739516d0/(z+dp4)+.120858003d-2/(z+dp5)-.536382d        !/*ga         25*/
     & -5/(z+6.d0))                                                             !/*ga         26*/
      if (x.le.1) then                                                          !/*ga         27*/
   20   continue                                                                !/*ga         28*/
        gamz=gamz/z                                                             !/*ga         29*/
        adz=abs(z-x)                                                            !/*ga         30*/
        if (adz.ge.dph) then                                                    !/*ga         31*/
          z=z-dp1                                                               !/*ga         32*/
          go to 20                                                              !/*ga         33*/
        endif                                                                   !/*ga         34*/
      endif                                                                     !/*ga         35*/
      gammar=gamz                                                               !/*ga         36*/
      return                                                                    !/*ga         37*/
      end                                                                       !/*ga         38*/

!     $Id: rou10.F,v 1.4 2002/03/27 18:15:20 gwm Exp $                        !/*ro          1*/
      subroutine rou10                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      i3=0                                                                      !/*ro          5*/
      if (ex-d(4)) 40,40,10                                                     !/*ro          6*/
   10 call spac32 (31)                                                          !/*ro          7*/
      go to 20                                                                  !/*ro          8*/
   20 i3=-1                                                                     !/*ro          9*/
   30 return                                                                    !/*ro         10*/
   40 curr(2)=out(15)                                                           !/*ro         11*/
      wkrpn(1)=out(15)                                                          !/*ro         12*/
      wkrpn(4)=out(18)                                                          !/*ro         13*/
!     k.e. for protons and neutrons region 1                                    !/*ro         14*/
      go to 30                                                                  !/*ro         15*/
      end                                                                       !/*ro         16*/

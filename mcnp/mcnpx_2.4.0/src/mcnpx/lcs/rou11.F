!     $Id: rou11.F,v 1.4 2002/03/27 18:15:20 gwm Exp $                        !/*ro          1*/
      subroutine rou11 (t)                                                      !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension t(6426)                                                         !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      go to (10,30,40,20), i3                                                   !/*ro          6*/
   10 pt(2)=dp3                                                                 !/*ro          7*/
   20 ik=it                                                                     !/*ro          8*/
      pt(14)=dp1                                                                !/*ro          9*/
   30 pm(3)=pnms                                                                !/*ro         10*/
   40 if (34.d1-rlke) 50,70,70                                                  !/*ro         11*/
   50 i3=1                                                                      !/*ro         12*/
   60 return                                                                    !/*ro         13*/
   70 call mud (snt,inpt)                                                       !/*ro         14*/
      if (ik-3) 100,80,90                                                       !/*ro         15*/
   80 i3=2                                                                      !/*ro         16*/
      go to 60                                                                  !/*ro         17*/
   90 i3=3                                                                      !/*ro         18*/
      go to 60                                                                  !/*ro         19*/
  100 call crdet (51,t(1),rlke)                                                 !/*ro         20*/
      i3=4                                                                      !/*ro         21*/
      go to 60                                                                  !/*ro         22*/
      end                                                                       !/*ro         23*/

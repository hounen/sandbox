!     $Id: rou13.F,v 1.4 2002/03/27 18:15:20 gwm Exp $                        !/*ro          1*/
      subroutine rou13                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      i3=0                                                                      !/*ro          5*/
      if (iv) 10,50,50                                                          !/*ro          6*/
   10 if (aabs) 20,50,20                                                        !/*ro          7*/
   20 if (ifca-2) 30,70,60                                                      !/*ro          8*/
   30 in1=0                                                                     !/*ro          9*/
   40 i3=1                                                                      !/*ro         10*/
      go to 80                                                                  !/*ro         11*/
   50 call signex                                                               !/*ro         12*/
      if (ifc-12) 110,110,120                                                   !/*ro         13*/
   60 if (ifca-6) 70,30,90                                                      !/*ro         14*/
   70 in1=0                                                                     !/*ro         15*/
      i3=-1                                                                     !/*ro         16*/
   80 return                                                                    !/*ro         17*/
   90 if (ifca-8) 30,100,100                                                    !/*ro         18*/
  100 in1=1                                                                     !/*ro         19*/
      go to 40                                                                  !/*ro         20*/
  110 in1=0                                                                     !/*ro         21*/
      go to 80                                                                  !/*ro         22*/
  120 if (ifc-18) 130,130,140                                                   !/*ro         23*/
  130 in1=-1                                                                    !/*ro         24*/
      go to 80                                                                  !/*ro         25*/
  140 in1=1                                                                     !/*ro         26*/
      go to 80                                                                  !/*ro         27*/
      end                                                                       !/*ro         28*/

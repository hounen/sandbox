!     $Id: rout8.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                        !/*ro          1*/
      subroutine rout8                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      i3=1                                                                      !/*ro          5*/
      if (iv) 20,10,10                                                          !/*ro          6*/
   10 if (value1-value2) 20,20,110                                              !/*ro          7*/
   20 if (isw(3)) 80,30,80                                                      !/*ro          8*/
   30 ifc=7+ifcc                                                                !/*ro          9*/
!     7=bg6e(2461)  8=bg6ia(4026)  ntnt(21626)  bg48x(12762)=19                 !/*ro         10*/
      if (in1) 40,60,40                                                         !/*ro         11*/
   40 i3=2                                                                      !/*ro         12*/
   50 return                                                                    !/*ro         13*/
   60 c(3)=d(2)                                                                 !/*ro         14*/
      go to 70                                                                  !/*ro         15*/
   70 i3=3                                                                      !/*ro         16*/
      go to 50                                                                  !/*ro         17*/
   80 ifc=8+ifcc                                                                !/*ro         18*/
      if (in1) 90,100,90                                                        !/*ro         19*/
   90 i3=4                                                                      !/*ro         20*/
      go to 50                                                                  !/*ro         21*/
  100 c(3)=d(2)+d(3)+d(4)                                                       !/*ro         22*/
      go to 70                                                                  !/*ro         23*/
  110 call signex                                                               !/*ro         24*/
      go to 50                                                                  !/*ro         25*/
      end                                                                       !/*ro         26*/

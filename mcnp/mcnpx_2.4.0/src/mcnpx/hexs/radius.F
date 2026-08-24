!     $Id: radius.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                       !/*ra          1*/
      function radius (a)                                                       !/*ra          2*/
!  NASA subroutine                                                              !/*ra          3*/
#include <lcs/double.h>                                                         /*!ra          4*/
      dimension na(23), rms(23)                                                 !/*ra          5*/
      data na /1,2,3,4,6,7,9,10,11,12,13,14,15,16,17,18,19,20,22,23,24,2        !/*ra          6*/
     & 5,26/                                                                    !/*ra          7*/
      data rms /0.85d0,2.095d0,1.976d0,1.671d0,2.57d0,2.41d0,2.519d0,2.4        !/*ra          8*/
     & 5d0,2.42d0,2.471d0,2.440d0,2.58d0,2.611d0,2.730d0,2.662d0,2.727d0        !/*ra          9*/
     & ,2.900d0,3.040d0,2.969d0,2.94d0,3.075d0,3.11d0,3.06/                     !/*ra         10*/
      fact=sqrt(dp5/dp3)                                                        !/*ra         11*/
      ia=a+0.4d0                                                                !/*ra         12*/
      radius=fact*(0.84d0*a**dpth+0.55d0)                                       !/*ra         13*/
      do 20 i=1,23                                                              !/*ra         14*/
      if (ia.eq.na(i)) go to 10                                                 !/*ra         15*/
      go to 20                                                                  !/*ra         16*/
   10 radius=fact*rms(i)                                                        !/*ra         17*/
   20 continue                                                                  !/*ra         18*/
      return                                                                    !/*ra         19*/
      end                                                                       !/*ra         20*/

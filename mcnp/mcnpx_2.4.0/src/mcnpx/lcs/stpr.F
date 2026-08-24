!     $Id: stpr.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                         !/*st          1*/
      subroutine stpr                                                           !/*st          2*/
#include <lcs/double.h>                                                         /*!st          3*/
#include <lcs/btcom.h>                                                          /*!st          4*/
      i1=0                                                                      !/*st          5*/
      med=clsm                                                                  !/*st          6*/
      do 90 i=3,39,12                                                           !/*st          7*/
      k=i                                                                       !/*st          8*/
      if (pt(i)) 10,90,10                                                       !/*st          9*/
   10 if (pt(k-1)-dp2) 20,30,20                                                 !/*st         10*/
   20 pt(k-2)=pt(k)-space(med+9)                                                !/*st         11*/
      go to 60                                                                  !/*st         12*/
   30 pt(k-2)=pt(k)-space(med+3)                                                !/*st         13*/
   40 if (pt(k-2)-5.d+02) 50,50,80                                              !/*st         14*/
   50 call stpl (pt(k-2))                                                       !/*st         15*/
!     velocity less than criterion                                              !/*st         16*/
      if (i1) 100,90,100                                                        !/*st         17*/
   60 if (pt(k-1)-dp1) 70,40,70                                                 !/*st         18*/
   70 pt(k-2)=(dncms*pt(k-2))/pt(k+1)                                           !/*st         19*/
      go to 40                                                                  !/*st         20*/
   80 call stph (pt(k-1))                                                       !/*st         21*/
!     velocity greater than criterion                                           !/*st         22*/
      if (i1) 100,90,100                                                        !/*st         23*/
   90 continue                                                                  !/*st         24*/
  100 return                                                                    !/*st         25*/
      end                                                                       !/*st         26*/

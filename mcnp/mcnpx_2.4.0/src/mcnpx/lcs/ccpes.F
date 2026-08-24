!     $Id: ccpes.F,v 1.4 2002/03/27 18:14:57 gwm Exp $                        !/*cc          1*/
      subroutine ccpes                                                          !/*cc          2*/
#include <lcs/double.h>                                                         /*!cc          3*/
#include <lcs/btcom.h>                                                          /*!cc          4*/
      i1=0                                                                      !/*cc          5*/
      i=int(curr(1)+0.05d0)                                                     !/*cc          6*/
      count(i)=count(i)+dp1                                                     !/*cc          7*/
!*** count no. of times each type of particle escapes                           !/*cc          8*/
      if (curr(1)-dp2) 10,20,10                                                 !/*cc          9*/
   10 k=curr(10)+9.05d0                                                         !/*cc         10*/
      go to 30                                                                  !/*cc         11*/
   20 k=curr(10)+3.05d0                                                         !/*cc         12*/
   30 if (esps(1)-6.d1) 60,40,40                                                !/*cc         13*/
   40 i1=1                                                                      !/*cc         14*/
!     storage already filled                                                    !/*cc         15*/
   50 return                                                                    !/*cc         16*/
   60 l=esps(1)*8.d0+2.05d0                                                     !/*cc         17*/
      esps(l)=curr(1)                                                           !/*cc         18*/
      esps(l+1)=curr(2)-space(k)                                                !/*cc         19*/
      m=13.05d0-curr(11)                                                        !/*cc         20*/
      cc(m)=cc(m)+dp1                                                           !/*cc         21*/
      m=4                                                                       !/*cc         22*/
      l=l+2                                                                     !/*cc         23*/
      n=l+2                                                                     !/*cc         24*/
      do 70 i=l,n                                                               !/*cc         25*/
      esps(i)=curr(m+3)                                                         !/*cc         26*/
      esps(i+3)=curr(m)                                                         !/*cc         27*/
   70 m=m+1                                                                     !/*cc         28*/
      esps(1)=esps(1)+dp1                                                       !/*cc         29*/
      go to 50                                                                  !/*cc         30*/
      end                                                                       !/*cc         31*/

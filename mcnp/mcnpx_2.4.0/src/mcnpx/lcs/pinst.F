!     $Id: pinst.F,v 1.4 2002/03/27 18:15:15 gwm Exp $                        !/*pi          1*/
      subroutine pinst                                                          !/*pi          2*/
#include <lcs/double.h>                                                         /*!pi          3*/
#include <lcs/btcom.h>                                                          /*!pi          4*/
      i1=0                                                                      !/*pi          5*/
      med=clsm                                                                  !/*pi          6*/
      if (inc) 10,140,10                                                        !/*pi          7*/
   10 inc=0                                                                     !/*pi          8*/
      if (med-1) 20,50,40                                                       !/*pi          9*/
   20 i1=1                                                                      !/*pi         10*/
   30 return                                                                    !/*pi         11*/
   40 if (med-3) 60,90,20                                                       !/*pi         12*/
   50 ipec(12)=ipec(12)+1                                                       !/*pi         13*/
!     incident particle on reg.1 collision in reg.1                             !/*pi         14*/
      go to 30                                                                  !/*pi         15*/
   60 if (d(4)) 80,70,80                                                        !/*pi         16*/
   70 ipec(8)=ipec(8)+1                                                         !/*pi         17*/
      go to 30                                                                  !/*pi         18*/
!     inc. particle on reg.2 collision in reg.2                                 !/*pi         19*/
   80 ipec(9)=ipec(9)+1                                                         !/*pi         20*/
!     inc. particle on reg.1 collision in reg.2                                 !/*pi         21*/
      go to 30                                                                  !/*pi         22*/
   90 if (d(3)) 100,120,100                                                     !/*pi         23*/
  100 if (d(4)) 110,130,110                                                     !/*pi         24*/
  110 ipec(5)=ipec(5)+1                                                         !/*pi         25*/
!     inc. particle on reg.1 collision in reg.3                                 !/*pi         26*/
      go to 30                                                                  !/*pi         27*/
  120 ipec(3)=ipec(3)+1                                                         !/*pi         28*/
!     inc. particle on reg.3 collision in reg.3                                 !/*pi         29*/
      go to 30                                                                  !/*pi         30*/
  130 ipec(4)=ipec(4)+1                                                         !/*pi         31*/
!     inc. particle on reg.2 collision in reg.3                                 !/*pi         32*/
      go to 30                                                                  !/*pi         33*/
  140 k=curr(11)                                                                !/*pi         34*/
      k=3*(med-1)+k                                                             !/*pi         35*/
      cc(k)=cc(k)+dp1                                                           !/*pi         36*/
      go to 30                                                                  !/*pi         37*/
!     collision reg.1 particle origin k                                         !/*pi         38*/
      end                                                                       !/*pi         39*/

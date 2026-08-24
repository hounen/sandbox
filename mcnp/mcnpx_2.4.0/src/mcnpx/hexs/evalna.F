!     $Id: evalna.F,v 1.4 2002/03/27 18:14:49 gwm Exp $                       !/*ev          1*/
      subroutine evalna (a,e,sig)                                               !/*ev          2*/
!--------------------------------------------------------                       !/*ev          3*/
!  EVALNA calls  IAHN  IEHN  ALOG%  RTOR%                                       !/*ev          4*/
!--------------------------------------------------------                       !/*ev          5*/
#include <lcs/double.h>                                                         /*!ev          6*/
#include <lcs/evalc.h>                                                          /*!ev          7*/
      dimension tsig(2)                                                         !/*ev          8*/
      ia=iaen(a)                                                                !/*ev          9*/
      ie=ieen(e)                                                                !/*ev         10*/
      if (ia) 80,50,10                                                          !/*ev         11*/
   10 if (ie) 110,90,20                                                         !/*ev         12*/
   20 temp1=a/aeval(ia)                                                         !/*ev         13*/
      temp2=log(aeval(ia+1)/aeval(ia))                                          !/*ev         14*/
      ke=ie                                                                     !/*ev         15*/
      do 30 i=1,2                                                               !/*ev         16*/
      power=log(xaeval(ke,ia+1)/xaeval(ke,ia))/temp2                            !/*ev         17*/
      tsig(i)=xaeval(ke,ia)*temp1**power                                        !/*ev         18*/
   30 ke=ke+1                                                                   !/*ev         19*/
   40 sig=((e-eeval(ie))*(tsig(2)-tsig(1)))/(eeval(ie+1)-eeval(ie))             !/*ev         20*/
     & +tsig(1)                                                                 !/*ev         21*/
      return                                                                    !/*ev         22*/
!      a is less than a-min                                                     !/*ev         23*/
   50 ja=1                                                                      !/*ev         24*/
      temp1=dp1/(aeval(ja)/a)**dp2th                                            !/*ev         25*/
   60 if (ie) 140,120,70                                                        !/*ev         26*/
   70 tsig(1)=xaeval(ie,ja)*temp1                                               !/*ev         27*/
      tsig(2)=xaeval(ie+1,ja)*temp1                                             !/*ev         28*/
      go to 40                                                                  !/*ev         29*/
!        a is greater than a-max                                                !/*ev         30*/
   80 ja=iaeval                                                                 !/*ev         31*/
      temp1=(a/aeval(ja))**dp2th                                                !/*ev         32*/
      go to 60                                                                  !/*ev         33*/
   90 je=1                                                                      !/*ev         34*/
  100 temp1=a/aeval(ia)                                                         !/*ev         35*/
      temp2=log(aeval(ia+1)/aeval(ia))                                          !/*ev         36*/
      power=log(xaeval(je,ia+1)/xaeval(je,ia))/temp2                            !/*ev         37*/
      sig=xaeval(je,ia)*temp1**power                                            !/*ev         38*/
      return                                                                    !/*ev         39*/
  110 je=ieeval                                                                 !/*ev         40*/
      go to 100                                                                 !/*ev         41*/
  120 je=1                                                                      !/*ev         42*/
  130 sig=xaeval(je,ja)*temp1                                                   !/*ev         43*/
      return                                                                    !/*ev         44*/
  140 je=ieeval                                                                 !/*ev         45*/
      go to 130                                                                 !/*ev         46*/
      end                                                                       !/*ev         47*/

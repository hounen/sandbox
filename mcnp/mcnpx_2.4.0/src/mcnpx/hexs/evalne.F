!     $Id: evalne.F,v 1.4 2002/03/27 18:14:49 gwm Exp $                       !/*ev          1*/
      subroutine evalne (a,e,sig)                                               !/*ev          2*/
!--------------------------------------------------------                       !/*ev          3*/
!  EVALNE calls  IAHN  IEHN  ALOG%  RTOR%                                       !/*ev          4*/
!  EVALNE called by...   ESNEUT                                                 !/*ev          5*/
!--------------------------------------------------------                       !/*ev          6*/
#include <lcs/double.h>                                                         /*!ev          7*/
#include <lcs/evalc.h>                                                          /*!ev          8*/
      dimension tsig(2)                                                         !/*ev          9*/
      ia=iaen(a)                                                                !/*ev         10*/
      ie=ieen(e)                                                                !/*ev         11*/
      if (ia) 80,50,10                                                          !/*ev         12*/
   10 if (ie) 110,90,20                                                         !/*ev         13*/
   20 temp1=a/aeval(ia)                                                         !/*ev         14*/
      temp2=log(aeval(ia+1)/aeval(ia))                                          !/*ev         15*/
      ke=ie                                                                     !/*ev         16*/
      do 30 i=1,2                                                               !/*ev         17*/
      power=log(xseval(ke,ia+1)/xseval(ke,ia))/temp2                            !/*ev         18*/
      tsig(i)=xseval(ke,ia)*temp1**power                                        !/*ev         19*/
   30 ke=ke+1                                                                   !/*ev         20*/
   40 sig=((e-eeval(ie))*(tsig(2)-tsig(1)))/(eeval(ie+1)-eeval(ie))             !/*ev         21*/
     & +tsig(1)                                                                 !/*ev         22*/
      return                                                                    !/*ev         23*/
!      a is less than a-min                                                     !/*ev         24*/
   50 ja=1                                                                      !/*ev         25*/
      temp1=dp1/(aeval(ja)/a)**dp2th                                            !/*ev         26*/
   60 if (ie) 140,120,70                                                        !/*ev         27*/
   70 tsig(1)=xseval(ie,ja)*temp1                                               !/*ev         28*/
      tsig(2)=xseval(ie+1,ja)*temp1                                             !/*ev         29*/
      go to 40                                                                  !/*ev         30*/
!        a is greater than a-max                                                !/*ev         31*/
   80 ja=iaeval                                                                 !/*ev         32*/
      temp1=(a/aeval(ja))**dp2th                                                !/*ev         33*/
      go to 60                                                                  !/*ev         34*/
   90 je=1                                                                      !/*ev         35*/
  100 temp1=a/aeval(ia)                                                         !/*ev         36*/
      temp2=log(aeval(ia+1)/aeval(ia))                                          !/*ev         37*/
      power=log(xseval(je,ia+1)/xseval(je,ia))/temp2                            !/*ev         38*/
      sig=xseval(je,ia)*temp1**power                                            !/*ev         39*/
      return                                                                    !/*ev         40*/
  110 je=ieeval                                                                 !/*ev         41*/
      go to 100                                                                 !/*ev         42*/
  120 je=1                                                                      !/*ev         43*/
  130 sig=xseval(je,ja)*temp1                                                   !/*ev         44*/
      return                                                                    !/*ev         45*/
  140 je=ieeval                                                                 !/*ev         46*/
      go to 130                                                                 !/*ev         47*/
      end                                                                       !/*ev         48*/

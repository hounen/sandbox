!     $Id: hilonx.F,v 1.4 2002/03/27 18:15:06 gwm Exp $                       !/*hi          1*/
      subroutine hilonx (a,e,sig)                                               !/*hi          2*/
!--------------------------------------------------------                       !/*hi          3*/
! subname= hilonx                       b.colborn                               !/*hi          4*/
!  HILONX calls  IAHN  IEHN  ALOG%  RTOR%                                       !/*hi          5*/
!  HILONX called by...   ESNEUT                                                 !/*hi          6*/
!--------------------------------------------------------                       !/*hi          7*/
#include <lcs/double.h>                                                         /*!hi          8*/
#include <lcs/hilonc.h>                                                         /*!hi          9*/
      dimension tsig(2)                                                         !/*hi         10*/
      ia=iahn(a)                                                                !/*hi         11*/
      ie=iehn(e)                                                                !/*hi         12*/
      if (ia) 80,50,10                                                          !/*hi         13*/
   10 if (ie) 110,90,20                                                         !/*hi         14*/
   20 temp1=a/avalhn(ia)                                                        !/*hi         15*/
      temp2=log(avalhn(ia+1)/avalhn(ia))                                        !/*hi         16*/
      ke=ie                                                                     !/*hi         17*/
      do 30 i=1,2                                                               !/*hi         18*/
      power=log(hilon(ke,ia+1)/hilon(ke,ia))/temp2                              !/*hi         19*/
      tsig(i)=hilon(ke,ia)*temp1**power                                         !/*hi         20*/
   30 ke=ke+1                                                                   !/*hi         21*/
   40 sig=((e-evalhn(ie))*(tsig(2)-tsig(1)))/(evalhn(ie+1)-evalhn(ie))          !/*hi         22*/
     & +tsig(1)                                                                 !/*hi         23*/
      return                                                                    !/*hi         24*/
!      a is less than a-min                                                     !/*hi         25*/
   50 ja=1                                                                      !/*hi         26*/
      temp1=dp1/(avalhn(ja)/a)**dp2th                                           !/*hi         27*/
   60 if (ie) 140,120,70                                                        !/*hi         28*/
   70 tsig(1)=hilon(ie,ja)*temp1                                                !/*hi         29*/
      tsig(2)=hilon(ie+1,ja)*temp1                                              !/*hi         30*/
      go to 40                                                                  !/*hi         31*/
!        a is greater than a-max                                                !/*hi         32*/
   80 ja=iamxhn                                                                 !/*hi         33*/
      temp1=(a/avalhn(ja))**dp2th                                               !/*hi         34*/
      go to 60                                                                  !/*hi         35*/
   90 je=1                                                                      !/*hi         36*/
  100 temp1=a/avalhn(ia)                                                        !/*hi         37*/
      temp2=log(avalhn(ia+1)/avalhn(ia))                                        !/*hi         38*/
      power=log(hilon(je,ia+1)/hilon(je,ia))/temp2                              !/*hi         39*/
      sig=hilon(je,ia)*temp1**power                                             !/*hi         40*/
      return                                                                    !/*hi         41*/
  110 je=iemxhn                                                                 !/*hi         42*/
      go to 100                                                                 !/*hi         43*/
  120 je=1                                                                      !/*hi         44*/
  130 sig=hilon(je,ja)*temp1                                                    !/*hi         45*/
      return                                                                    !/*hi         46*/
  140 je=iemxhn                                                                 !/*hi         47*/
      go to 130                                                                 !/*hi         48*/
      end                                                                       !/*hi         49*/

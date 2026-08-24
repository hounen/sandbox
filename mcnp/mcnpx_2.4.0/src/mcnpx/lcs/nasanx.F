!     $Id: nasanx.F,v 1.4 2002/03/27 18:15:12 gwm Exp $                       !/*na          1*/
      subroutine nasanx (a,e,sig)                                               !/*na          2*/
!--------------------------------------------------------                       !/*na          3*/
! subname= nasanx                       b.colborn                               !/*na          4*/
!  NASANX calls  IANN  IENN  ALOG%  RTOR%                                       !/*na          5*/
!  NASANX called by...   ESNEUT                                                 !/*na          6*/
!--------------------------------------------------------                       !/*na          7*/
#include <lcs/double.h>                                                         /*!na          8*/
#include <lcs/nasanc.h>                                                         /*!na          9*/
      dimension tsig(2)                                                         !/*na         10*/
      ia=iann(a)                                                                !/*na         11*/
      ie=ienn(e)                                                                !/*na         12*/
      if (ia) 80,50,10                                                          !/*na         13*/
   10 if (ie) 110,90,20                                                         !/*na         14*/
   20 temp1=a/avalnn(ia)                                                        !/*na         15*/
      temp2=log(avalnn(ia+1)/avalnn(ia))                                        !/*na         16*/
      ke=ie                                                                     !/*na         17*/
      do 30 i=1,2                                                               !/*na         18*/
      power=log(nasan(ke,ia+1)/nasan(ke,ia))/temp2                              !/*na         19*/
      tsig(i)=nasan(ke,ia)*temp1**power                                         !/*na         20*/
   30 ke=ke+1                                                                   !/*na         21*/
   40 sig=((e-evalnn(ie))*(tsig(2)-tsig(1)))/(evalnn(ie+1)-evalnn(ie))          !/*na         22*/
     & +tsig(1)                                                                 !/*na         23*/
      return                                                                    !/*na         24*/
!      a is less than a-min                                                     !/*na         25*/
   50 ja=1                                                                      !/*na         26*/
      temp1=dp1/(avalnn(ja)/a)**dp2th                                           !/*na         27*/
   60 if (ie) 140,120,70                                                        !/*na         28*/
   70 tsig(1)=nasan(ie,ja)*temp1                                                !/*na         29*/
      tsig(2)=nasan(ie+1,ja)*temp1                                              !/*na         30*/
      go to 40                                                                  !/*na         31*/
!        a is greater than a-max                                                !/*na         32*/
   80 ja=iamxnn                                                                 !/*na         33*/
      temp1=(a/avalnn(ja))**dp2th                                               !/*na         34*/
      go to 60                                                                  !/*na         35*/
   90 je=1                                                                      !/*na         36*/
  100 temp1=a/avalnn(ia)                                                        !/*na         37*/
      temp2=log(avalnn(ia+1)/avalnn(ia))                                        !/*na         38*/
      power=log(nasan(je,ia+1)/nasan(je,ia))/temp2                              !/*na         39*/
      sig=nasan(je,ia)*temp1**power                                             !/*na         40*/
      return                                                                    !/*na         41*/
  110 je=iemxnn                                                                 !/*na         42*/
      go to 100                                                                 !/*na         43*/
  120 je=1                                                                      !/*na         44*/
  130 sig=nasan(je,ja)*temp1                                                    !/*na         45*/
      return                                                                    !/*na         46*/
  140 je=iemxnn                                                                 !/*na         47*/
      go to 130                                                                 !/*na         48*/
      end                                                                       !/*na         49*/

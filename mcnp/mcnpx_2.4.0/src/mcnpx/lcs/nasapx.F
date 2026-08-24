!     $Id: nasapx.F,v 1.4 2002/03/27 18:15:12 gwm Exp $                       !/*na          1*/
      subroutine nasapx (a,e,sig)                                               !/*na          2*/
!--------------------------------------------------------                       !/*na          3*/
! subname= nasapx                       b.colborn                               !/*na          4*/
!  NASAPX calls  IANP  IENP  ALOG%  RTOR%                                       !/*na          5*/
!  NASAPX called by...   ESPROT                                                 !/*na          6*/
!--------------------------------------------------------                       !/*na          7*/
#include <lcs/double.h>                                                         /*!na          8*/
#include <lcs/nasapc.h>                                                         /*!na          9*/
      dimension tsig(2)                                                         !/*na         10*/
      if (e.lt.evalnp(1)) then                                                  !/*na         11*/
        sig=dp0                                                                 !/*na         12*/
        return                                                                  !/*na         13*/
      endif                                                                     !/*na         14*/
      ia=ianp(a)                                                                !/*na         15*/
      ie=ienp(e)                                                                !/*na         16*/
      if (ia) 80,50,10                                                          !/*na         17*/
   10 if (ie) 110,90,20                                                         !/*na         18*/
   20 temp1=a/avalnp(ia)                                                        !/*na         19*/
      temp2=log(avalnp(ia+1)/avalnp(ia))                                        !/*na         20*/
      ke=ie                                                                     !/*na         21*/
      do 30 i=1,2                                                               !/*na         22*/
      power=log(nasap(ke,ia+1)/nasap(ke,ia))/temp2                              !/*na         23*/
      tsig(i)=nasap(ke,ia)*temp1**power                                         !/*na         24*/
   30 ke=ke+1                                                                   !/*na         25*/
   40 sig=((e-evalnp(ie))*(tsig(2)-tsig(1)))/(evalnp(ie+1)-evalnp(ie))          !/*na         26*/
     & +tsig(1)                                                                 !/*na         27*/
      return                                                                    !/*na         28*/
!      a is less than a-min                                                     !/*na         29*/
   50 ja=1                                                                      !/*na         30*/
      temp1=dp1/(avalnp(ja)/a)**dp2th                                           !/*na         31*/
   60 if (ie) 140,120,70                                                        !/*na         32*/
   70 tsig(1)=nasap(ie,ja)*temp1                                                !/*na         33*/
      tsig(2)=nasap(ie+1,ja)*temp1                                              !/*na         34*/
      go to 40                                                                  !/*na         35*/
!        a is greater than a-max                                                !/*na         36*/
   80 ja=iamxnp                                                                 !/*na         37*/
      temp1=(a/avalnp(ja))**dp2th                                               !/*na         38*/
      go to 60                                                                  !/*na         39*/
   90 je=1                                                                      !/*na         40*/
  100 temp1=a/avalnp(ia)                                                        !/*na         41*/
      temp2=log(avalnp(ia+1)/avalnp(ia))                                        !/*na         42*/
      power=log(nasap(je,ia+1)/nasap(je,ia))/temp2                              !/*na         43*/
      sig=nasap(je,ia)*temp1**power                                             !/*na         44*/
      return                                                                    !/*na         45*/
  110 je=iemxnp                                                                 !/*na         46*/
      go to 100                                                                 !/*na         47*/
  120 je=1                                                                      !/*na         48*/
  130 sig=nasap(je,ja)*temp1                                                    !/*na         49*/
      return                                                                    !/*na         50*/
  140 je=iemxnp                                                                 !/*na         51*/
      go to 130                                                                 !/*na         52*/
      end                                                                       !/*na         53*/

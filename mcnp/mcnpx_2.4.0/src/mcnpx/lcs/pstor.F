!     $Id: pstor.F,v 1.4 2002/03/27 18:15:17 gwm Exp $                        !/*ps          1*/
      subroutine pstor                                                          !/*ps          2*/
#include <lcs/double.h>                                                         /*!ps          3*/
#include <lcs/btcom.h>                                                          /*!ps          4*/
      l=(i1*12)-28                                                              !/*ps          5*/
      if (i2) 10,60,70                                                          !/*ps          6*/
   10 jj=0                                                                      !/*ps          7*/
      if (pm(3)-dncms) 30,30,20                                                 !/*ps          8*/
   20 i1=i1+1                                                                   !/*ps          9*/
      jj=1                                                                      !/*ps         10*/
!     x-y-z-coordinates of collision point                                      !/*ps         11*/
   30 univ=sqrt(pxyz(i1)*pxyz(i1)+pxyz(i1+4)*pxyz(i1+4)+pxyz(i1+8)*pxyz         !/*ps         12*/
     & (i1+8))                                                                  !/*ps         13*/
      k=i1+8                                                                    !/*ps         14*/
      do 40 i=i1,k,4                                                            !/*ps         15*/
      pt(l)=pxyz(i)/univ                                                        !/*ps         16*/
   40 l=l+1                                                                     !/*ps         17*/
      i1=i1-jj                                                                  !/*ps         18*/
   50 pt(l)=clsm                                                                !/*ps         19*/
      pt(l+1)=curr(11)                                                          !/*ps         20*/
      pt(l-6)=c(1)                                                              !/*ps         21*/
      pt(l-5)=c(2)                                                              !/*ps         22*/
      pt(l-4)=c(3)                                                              !/*ps         23*/
      return                                                                    !/*ps         24*/
   60 k=14                                                                      !/*ps         25*/
      go to 90                                                                  !/*ps         26*/
   70 if (i2-2) 80,110,110                                                      !/*ps         27*/
   80 k=17                                                                      !/*ps         28*/
   90 univ=sqrt(pnidk(k)*pnidk(k)+pnidk(k+1)*pnidk(k+1)+pnidk(k+2)*pnidk        !/*ps         29*/
     & (k+2))                                                                   !/*ps         30*/
      j=k+2                                                                     !/*ps         31*/
      do 100 i=k,j                                                              !/*ps         32*/
      pt(l)=pnidk(i)/univ                                                       !/*ps         33*/
  100 l=l+1                                                                     !/*ps         34*/
      go to 50                                                                  !/*ps         35*/
  110 univ=sqrt(pt(l-3)*pt(l-3)+pt(l-2)*pt(l-2)+pt(l-1)*pt(l-1))                !/*ps         36*/
      k=l-1                                                                     !/*ps         37*/
      m=l-3                                                                     !/*ps         38*/
      do 120 i=m,k                                                              !/*ps         39*/
      pt(l)=pt(i)/univ                                                          !/*ps         40*/
  120 l=l+1                                                                     !/*ps         41*/
      go to 50                                                                  !/*ps         42*/
      end                                                                       !/*ps         43*/

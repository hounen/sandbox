!     $Id: qrdet.F,v 1.4 2002/03/27 18:15:19 gwm Exp $                        !/*qr          1*/
      subroutine qrdet (nodata,data,ener)                                       !/*qr          2*/
! QRDET                                                                         !/*qr          3*/
!     QRDET  called by...   ISOB  QOU17  QOU21                                  !/*qr          4*/
#include <lcs/double.h>                                                         /*!qr          5*/
      dimension data(380)                                                       !/*qr          6*/
#include <lcs/isobar.h>                                                         /*!qr          7*/
      ie=abs(ener/2.d1)                                                         !/*qr          8*/
      univ_=(ener-real(ie)*2.d1)/2.d1                                           !/*qr          9*/
      do 10 i=1,25                                                              !/*qr         10*/
   10 crdt_(i)=dp0                                                              !/*qr         11*/
      k=(nodata*ie)+1                                                           !/*qr         12*/
      if (inpt_) 50,20,50                                                       !/*qr         13*/
   20 n=nodata                                                                  !/*qr         14*/
   30 l=k+nodata                                                                !/*qr         15*/
      do 40 i=1,n                                                               !/*qr         16*/
      crdt_(i)=(data(l)-data(k))*univ_+data(k)                                  !/*qr         17*/
      k=k+1                                                                     !/*qr         18*/
   40 l=l+1                                                                     !/*qr         19*/
      inpt_=0                                                                   !/*qr         20*/
      return                                                                    !/*qr         21*/
   50 k=inpt_-1+k                                                               !/*qr         22*/
      n=2                                                                       !/*qr         23*/
      go to 30                                                                  !/*qr         24*/
      end                                                                       !/*qr         25*/

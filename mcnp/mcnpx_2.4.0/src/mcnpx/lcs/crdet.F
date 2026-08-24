!     $Id: crdet.F,v 1.4 2002/03/27 18:14:59 gwm Exp $                        !/*cr          1*/
      subroutine crdet (nodata,data,ener)                                       !/*cr          2*/
#include <lcs/double.h>                                                         /*!cr          3*/
      dimension data(6426)                                                      !/*cr          4*/
#include <lcs/btcom.h>                                                          /*!cr          5*/
      ie=abs(ener/2.d1)                                                         !/*cr          6*/
!     energy interval                                                           !/*cr          7*/
      univ=(ener-real(ie)*2.d1)/2.d1                                            !/*cr          8*/
!     inpt=0 if whole interval considered                                       !/*cr          9*/
!     nodata=data per energy interval                                           !/*cr         10*/
      do 10 i=1,25                                                              !/*cr         11*/
   10 crdt(i)=dp0                                                               !/*cr         12*/
!     answers stored in crdt                                                    !/*cr         13*/
      k=(nodata*ie)+1                                                           !/*cr         14*/
      if (inpt) 20,30,60                                                        !/*cr         15*/
   20 call errorf ('crdet')                                                     !/*cr         16*/
   30 n=nodata                                                                  !/*cr         17*/
   40 l=k+nodata                                                                !/*cr         18*/
      do 50 i=1,n                                                               !/*cr         19*/
      crdt(i)=(data(l)-data(k))*univ+data(k)                                    !/*cr         20*/
      k=k+1                                                                     !/*cr         21*/
   50 l=l+1                                                                     !/*cr         22*/
      inpt=0                                                                    !/*cr         23*/
      return                                                                    !/*cr         24*/
   60 k=inpt-1+k                                                                !/*cr         25*/
      n=2                                                                       !/*cr         26*/
      go to 40                                                                  !/*cr         27*/
!     not all parts evaluated                                                   !/*cr         28*/
      end                                                                       !/*cr         29*/

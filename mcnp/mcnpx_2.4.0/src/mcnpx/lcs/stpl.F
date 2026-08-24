!     $Id: stpl.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                         !/*st          1*/
      subroutine stpl (w)                                                       !/*st          2*/
#include <lcs/double.h>                                                         /*!st          3*/
      dimension w(12)                                                           !/*st          4*/
#include <lcs/btcom.h>                                                          /*!st          5*/
      do 10 k=2,950,12                                                          !/*st          6*/
      if (plvc(k)) 10,30,10                                                     !/*st          7*/
   10 continue                                                                  !/*st          8*/
      i1=1                                                                      !/*st          9*/
   20 return                                                                    !/*st         10*/
   30 i=k                                                                       !/*st         11*/
      j=i+11                                                                    !/*st         12*/
!     plvc(1)=no. of times entered for storage of velocity                      !/*st         13*/
!     less than criterion                                                       !/*st         14*/
      l=1                                                                       !/*st         15*/
      do 40 k=i,j                                                               !/*st         16*/
      plvc(k)=w(l)                                                              !/*st         17*/
   40 l=l+1                                                                     !/*st         18*/
      plvc(1)=plvc(1)+dp1                                                       !/*st         19*/
      go to 20                                                                  !/*st         20*/
      end                                                                       !/*st         21*/

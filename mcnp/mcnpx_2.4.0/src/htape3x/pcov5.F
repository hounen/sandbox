!     $Id: pcov5.F,v 1.4 2002/03/27 18:14:02 gwm Exp $                        !/*pc          1*/
      subroutine pcov5 (tlly,tlfsq,cov,nerg1,nup,enorm,cnorm)                   !/*pc          2*/
#include <htape3x/com32b.h>                                                     /*!pc          3*/
#include <htape3x/srcwt.h>                                                      /*!pc          4*/
#include <htape3x/lpt.h>                                                        /*!pc          5*/
#include <htape3x/contrl.h>                                                     /*!pc          6*/
      dimension tlly(4,nerg1,1), tlfsq(4,nerg1,1), cov(2,nerg1,1)               !/*pc          7*/
      cn=enorm*cnorm**2                                                         !/*pc          8*/
      do 10 k=1,nup                                                             !/*pc          9*/
      do 10 j=1,nerg1                                                           !/*pc         10*/
      do 10 i=1,2                                                               !/*pc         11*/
      b=fwscum                                                                  !/*pc         12*/
      d=tlly(2*i-1,j,k)*tlly(2*i,j,k)                                           !/*pc         13*/
      if (d.gt.dp0) b=b+fnorm**2*cov(i,j,k)/d                                   !/*pc         14*/
      if (tlly(2*i-1,j,k).gt.dp0) b=b-fnorm*tlfsq(2*i-1,j,k)/tlly(2*i-1         !/*pc         15*/
     & ,j,k)                                                                    !/*pc         16*/
      if (tlly(2*i,j,k).gt.dp0) b=b-fnorm*tlfsq(2*i,j,k)/tlly(2*i,j,k)          !/*pc         17*/
   10 cov(i,j,k)=cn*max(b,dp0)                                                  !/*pc         18*/
      return                                                                    !/*pc         19*/
      end                                                                       !/*pc         20*/

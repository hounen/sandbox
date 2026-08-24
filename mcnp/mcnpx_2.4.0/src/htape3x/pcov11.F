!     $Id: pcov11.F,v 1.4 2002/03/27 18:14:02 gwm Exp $                       !/*pc          1*/
      subroutine pcov11 (tlly,cov,tlfsq,nerg1,nup,enorm,cnorm)                  !/*pc          2*/
#include <htape3x/com32b.h>                                                     /*!pc          3*/
#include <htape3x/srcwt.h>                                                      /*!pc          4*/
#include <htape3x/lpt.h>                                                        /*!pc          5*/
#include <htape3x/contrl.h>                                                     /*!pc          6*/
      dimension tlly(3,nerg1,1), tlfsq(3,nerg1,1), cov(3,nerg1,1)               !/*pc          7*/
      cn=enorm*cnorm**2                                                         !/*pc          8*/
      do 10 m=1,nup                                                             !/*pc          9*/
      do 10 j=1,nerg1                                                           !/*pc         10*/
      do 10 i=1,3                                                               !/*pc         11*/
      ii=mod(i,3)+1                                                             !/*pc         12*/
      b=fwscum                                                                  !/*pc         13*/
      dem=tlly(i,j,m)*tlly(ii,j,m)                                              !/*pc         14*/
      if (dem.gt.dp0) b=b+fnorm**2*cov(i,j,m)/dem                               !/*pc         15*/
      if (tlly(i,j,m).gt.dp0) b=b-fnorm*tlfsq(i,j,m)/tlly(i,j,m)                !/*pc         16*/
      if (tlly(ii,j,m).gt.dp0) b=b-fnorm*tlfsq(ii,j,m)/tlly(ii,j,m)             !/*pc         17*/
      cov(i,j,m)=cn*max(b,dp0)                                                  !/*pc         18*/
   10 continue                                                                  !/*pc         19*/
      return                                                                    !/*pc         20*/
      end                                                                       !/*pc         21*/

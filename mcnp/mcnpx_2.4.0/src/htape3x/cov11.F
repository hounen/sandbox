!     $Id: cov11.F,v 1.4 2002/03/27 18:13:56 gwm Exp $                        !/*co          1*/
      subroutine cov11 (cov,hold1,nup1)                                         !/*co          2*/
#include <htape3x/com32b.h>                                                     /*!co          3*/
#include <htape3x/srcwt.h>                                                      /*!co          4*/
      dimension cov(1), hold1(1)                                                !/*co          5*/
#include <htape3x/lpt.h>                                                        /*!co          6*/
#include <htape3x/contrl.h>                                                     /*!co          7*/
      nup2=nup1-2                                                               !/*co          8*/
      fwsq=fwti**2                                                              !/*co          9*/
      do 10 k=1,nup2,3                                                          !/*co         10*/
      cov(k)=fwsq*hold1(k)*hold1(k+1)+cov(k)                                    !/*co         11*/
   10 continue                                                                  !/*co         12*/
      do 20 k=1,nup2,3                                                          !/*co         13*/
      cov(k+1)=fwsq*hold1(k+1)*hold1(k+2)+cov(k+1)                              !/*co         14*/
   20 continue                                                                  !/*co         15*/
      do 30 k=1,nup2,3                                                          !/*co         16*/
      cov(k+2)=fwsq*hold1(k+2)*hold1(k)+cov(k+2)                                !/*co         17*/
   30 continue                                                                  !/*co         18*/
      return                                                                    !/*co         19*/
      end                                                                       !/*co         20*/

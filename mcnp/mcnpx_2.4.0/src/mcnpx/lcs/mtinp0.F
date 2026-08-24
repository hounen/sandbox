!     $Id: mtinp0.F,v 1.4 2002/03/27 18:15:11 gwm Exp $                       !/*mt          1*/
      subroutine mtinp0 (jaz,nw,mx)                                             !/*mt          2*/
#include <lcs/double.h>                                                         /*!mt          3*/
#include <lcs/lptS.h>                                                           /*!mt          4*/
#include <lcs/inout.h>                                                          /*!mt          5*/
#include <lcs/concom.h>                                                         /*!mt          6*/
! ..  save nel1                                                                 !/*mt          7*/
      nerr=0                                                                    !/*mt          8*/
      if (nw.eq.1) nel1=0                                                       !/*mt          9*/
      iz=jaz/1000                                                               !/*mt         10*/
      zzx=iz                                                                    !/*mt         11*/
      ia=mod(jaz,1000)                                                          !/*mt         12*/
      ax=ia                                                                     !/*mt         13*/
!        TEMPORARY expedient: special treatment for 1004.id and 1005.id         !/*mt         14*/
!        for mixed ortho and para S(alpha,beta) sets for H-1 and H-2.           !/*mt         15*/
      if (iz.eq.1) then                                                         !/*mt         16*/
         if(ia.eq.4)ax=dp1                                                      !/*mt         17*/
         if(ia.eq.5)ax=dp2                                                      !/*mt         18*/
      endif                                                                     !/*mt         19*/
!                                                                               !/*mt         20*/
      nelm=0                                                                    !/*mt         21*/
      call afill0 (nelm,zzx,ax,nerr)                                            !/*mt         22*/
      mxmel=mxmel+nelm                                                          !/*mt         23*/
      nel1=nel1+nelm                                                            !/*mt         24*/
      mxelm=max(mxelm,nel1)                                                     !/*mt         25*/
!                                                                               !/*mt         26*/
      if (nerr.gt.0) call erprnt(1,1,0,0,0,0,0,0,                               !/*mt         27*/
     &   '18hMTINP0.  nerr.gt.0')                                               !/*mt         28*/
!                                                                               !/*mt         29*/
      mx=mxmel                                                                  !/*mt         30*/
      return                                                                    !/*mt         31*/
      end                                                                       !/*mt         32*/

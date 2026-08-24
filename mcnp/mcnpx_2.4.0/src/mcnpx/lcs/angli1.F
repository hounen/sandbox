!     $Id: angli1.F,v 1.4 2002/03/27 18:14:54 gwm Exp $                       !/*an          1*/
      subroutine angli1 (coscm)                                                 !/*an          2*/
#include <lcs/double.h>                                                         /*!an          3*/
!     ==================================================================        !/*an          4*/
!     angli1 selects the cos of the angle in the c.m.system of the              !/*an          5*/
!     isobar created in a nucleon-nucleon collision which produces              !/*an          6*/
!     only one pion  or,equivalenty in the c.m.system it creates an isob        !/*an          7*/
!     and a nucleon.                                                            !/*an          8*/
!     this routine is subject to change because the actual distribution         !/*an          9*/
!     is unknown except that it is expected to be peaked forward and            !/*an         10*/
!     backward.  We start with p(coscm)=.25+.75*coscm*coscm                     !/*an         11*/
!     note.pmax=dp1                                                             !/*an         12*/
!                                                                               !/*an         13*/
! ANGLI1 calls   FLTRNF                                                         !/*an         14*/
! called by DECAY  GAMMA                                                        !/*an         15*/
!     ==================================================================        !/*an         16*/
!                                                                               !/*an         17*/
   10 continue                                                                  !/*an         18*/
      rant1=rang()                                                              !/*an         19*/
      eta2=rang()                                                               !/*an         20*/
      coscm=dp2*rant1-dp1                                                       !/*an         21*/
      p=.25d0+.75d0*coscm*coscm                                                 !/*an         22*/
      if (eta2.gt.p) go to 10                                                   !/*an         23*/
!                                                                               !/*an         24*/
!       due to inefficiency of rejection (on cdc7600), use a                    !/*an         25*/
!       pure cos**2 distribution that is faster (but not on cray)               !/*an         26*/
!                                                                               !/*an         27*/
!         this was made a statement-fcn 27/11/81 1500                           !/*an         28*/
!     coscm=max(ranf(r1),ranf(r2))*sign(dp1,ranf(r3)-dph)                       !/*an         29*/
!                                                                               !/*an         30*/
      return                                                                    !/*an         31*/
      end                                                                       !/*an         32*/

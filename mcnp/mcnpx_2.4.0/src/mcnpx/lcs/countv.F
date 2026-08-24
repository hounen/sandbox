!     $Id: countv.F,v 1.4 2002/03/27 18:14:59 gwm Exp $                       !/*co          1*/
      subroutine countv (ws3,inuc,in_out)                                       !/*co          2*/
#include <lcs/double.h>                                                         /*!co          3*/
!.......................................................................        !/*co          4*/
! ws3 identifies the particle type                                              !/*co          5*/
! inuc=1 for target, inuc=2 for projectile                                      !/*co          6*/
! in_out=1 for particle taken from a nucleus, in_out=2 for a particle           !/*co          7*/
! put into the nucleus.                                                         !/*co          8*/
!                                                                               !/*co          9*/
! COUNT  calls   ERRORF                                                         !/*co         10*/
! called by RLAR  WWSST  GAMMA  PSEL                                            !/*co         11*/
!.......................................................................        !/*co         12*/
#include <lcs/vegas.h>                                                          /*!co         13*/
      dimension ia(12), iq(12)                                                  !/*co         14*/
      data ia /-1,-1,0,1,1,7*0/, iq /0,-1,0,1,0,1,0,-1,0,1,0,-1/                !/*co         15*/
      itype=dp3+ws3                                                             !/*co         16*/
      if (itype.lt.1.or.itype.gt.12) go to 20                                   !/*co         17*/
      if (itype.eq.3.or.itype.eq.9) go to 20                                    !/*co         18*/
      if (in_out.eq.2) go to 10                                                 !/*co         19*/
      ixa(inuc)=ixa(inuc)-ia(itype)                                             !/*co         20*/
      ixq(inuc)=ixq(inuc)-iq(itype)                                             !/*co         21*/
      go to 30                                                                  !/*co         22*/
   10 continue                                                                  !/*co         23*/
      ixa(inuc)=ixa(inuc)+ia(itype)                                             !/*co         24*/
      ixq(inuc)=ixq(inuc)+iq(itype)                                             !/*co         25*/
      go to 30                                                                  !/*co         26*/
   20 continue                                                                  !/*co         27*/
      call expirx(1,'COUNT','itype<1 or >12 or =3 or =9')                       !/*co         28*/
   30 continue                                                                  !/*co         29*/
      return                                                                    !/*co         30*/
      end                                                                       !/*co         31*/

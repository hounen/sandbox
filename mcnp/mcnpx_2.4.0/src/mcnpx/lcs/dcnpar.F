!     $Id: dcnpar.F,v 1.4 2002/03/27 18:14:59 gwm Exp $                       !/*dc          1*/
      subroutine dcnpar (anpar,n,itype,isc)                                     !/*dc          2*/
#include <lcs/double.h>                                                         /*!dc          3*/
!     ==================================================================        !/*dc          4*/
!     dcnpar decrements the no. of particles in tnpar or pnpar                  !/*dc          5*/
!     (here anpar of dim. n) by deleteng the first particle of type             !/*dc          6*/
!     itype and copying the last one in its place. the dimension n              !/*dc          7*/
!     is decremented by 1.                                                      !/*dc          8*/
!     isc=1 if the search was successful, 0 otherwise.                          !/*dc          9*/
!     dcnpar is used in gamma in case of a particle allowed to fall             !/*dc         10*/
!     in the fermi sea.                                                         !/*dc         11*/
!                                                                               !/*dc         12*/
! DCNPAR called by WWSST  GAMMA                                                 !/*dc         13*/
!     ==================================================================        !/*dc         14*/
!                                                                               !/*dc         15*/
      dimension anpar(4,250)                                                    !/*dc         16*/
      isc=0                                                                     !/*dc         17*/
      type=itype                                                                !/*dc         18*/
      do 10 i=1,n                                                               !/*dc         19*/
      if (type.ne.anpar(4,i)) go to 10                                          !/*dc         20*/
      ii=i                                                                      !/*dc         21*/
      go to 20                                                                  !/*dc         22*/
   10 continue                                                                  !/*dc         23*/
      return                                                                    !/*dc         24*/
   20 ics=1                                                                     !/*dc         25*/
      do 30 i=1,4                                                               !/*dc         26*/
      anpar(i,ii)=anpar(i,n)                                                    !/*dc         27*/
   30 continue                                                                  !/*dc         28*/
      n=n-1                                                                     !/*dc         29*/
      return                                                                    !/*dc         30*/
      end                                                                       !/*dc         31*/

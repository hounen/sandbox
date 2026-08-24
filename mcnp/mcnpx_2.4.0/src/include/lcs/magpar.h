!     $Id: magpar.h,v 1.4 2002/03/27 18:14:16 gwm Exp $                       !/*ma          1*/
!----------------------------------------------------------------------*        !/*ma          2*/
!     include file: magpar copy                   created 26/11/86 by p*        !/*ma          3*/
!     changes: none                                                    *        !/*ma          4*/
!     included in the following subroutines or functions: not updated  *        !/*ma          5*/
!                                                                      *        !/*ma          6*/
!     description of the common block(s) and variable(s)               *        !/*ma          7*/
!                                                                      *        !/*ma          8*/
!     /magpar/ contains information about the magnetic field.          *        !/*ma          9*/
!         angle = biggest angle that particle is aloud to travel (deg) *        !/*ma         10*/
!         delta = error of the boundary iteration (cm)                 *        !/*ma         11*/
!         dstep = guarantees that step is not forced to be too small.  *        !/*ma         12*/
!                 (if suggested angle is too big)                      *        !/*ma         13*/
!         bcom  = strength of the homogenous magnetic field (tesla)    *        !/*ma         14*/
!        btxcom = direction cosines                                    *        !/*ma         15*/
!        btycom =        =                                             *        !/*ma         16*/
!        btzcom =        =                                             *        !/*ma         17*/
!     magnon(j) = true if region j has a magnetic field                *        !/*ma         18*/
!                                                                      *        !/*ma         19*/
!                        mxxrgn = maximum number of regions            *        !/*ma         20*/
!                                                                      *        !/*ma         21*/
!----------------------------------------------------------------------*        !/*ma         22*/
!                                                                               !/*ma         23*/
      logical magnon                                                            !/*ma         24*/
      common /magpar/ angle, delta, dstep, bcom, btxcom, btycom, btzcom,        !/*ma         25*/
     & magnon(mxxrgn)                                                           !/*ma         26*/

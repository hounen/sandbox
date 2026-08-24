!     $Id: user.h,v 1.4 2002/03/27 18:14:19 gwm Exp $                         !/*us          1*/
!----------------------------------------------------------------------*        !/*us          2*/
!                                                                      *        !/*us          3*/
!     common user   for egs4                                           *        !/*us          4*/
!                                                                      *        !/*us          5*/
!                                     last change: a. ferrari 8-21-89  *        !/*us          6*/
!                                                                      *        !/*us          7*/
!        lbias  = flag for leading particle biasing                    *        !/*us          8*/
!        lemagn = flag for magnetic field (set by howfar)              *        !/*us          9*/
!                                                                      *        !/*us         10*/
!        xemf   = x-coordinate of the end point in the magnetic field  *        !/*us         11*/
!        yemf   = y-coordinate of the end point in the magnetic field  *        !/*us         12*/
!        zemf   = z-coordinate of the end point in the magnetic field  *        !/*us         13*/
!                                                                      *        !/*us         14*/
!        uemf   = u-cosine of the end point in the magnetic field      *        !/*us         15*/
!        vemf   = v-cosine of the end point in the magnetic field      *        !/*us         16*/
!        wemf   = w-cosine of the end point in the magnetic field      *        !/*us         17*/
!                                                                      *        !/*us         18*/
!----------------------------------------------------------------------*        !/*us         19*/
!                                                                               !/*us         20*/
      common /user/ xemf, yemf, zemf, uemf, vemf, wemf, lbias(mxxrgn),          !/*us         21*/
     & lemagn                                                                   !/*us         22*/
      logical lbias, lemagn                                                     !/*us         23*/

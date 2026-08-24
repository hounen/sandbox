!     $Id: stepsz.h,v 1.4 2002/03/27 18:14:18 gwm Exp $                       !/*st          1*/
!=== stepsz ===========================================================*        !/*st          2*/
!                                                                               !/*st          3*/
!----------------------------------------------------------------------*        !/*st          4*/
!                                                                      *        !/*st          5*/
!   common stepsz for setting the minimum and maximum step sizes on a  *        !/*st          6*/
!                 a region by region basis: very useful for vacuum re- *        !/*st          7*/
!                 gions with magnetic filed and for saving time ( and  *        !/*st          8*/
!                 accuracy ) with the new plc and lca algorithm in     *        !/*st          9*/
!                 egs and fluka                                        *        !/*st         10*/
!                                                                      *        !/*st         11*/
!          w a r n i n g !!!!! at the moment implemented only for      *        !/*st         12*/
!          electron and positron transport in egs and for charged      *        !/*st         13*/
!          particles transport in fluka with the new multiple scat-    *        !/*st         14*/
!          tering module!!!!!!                                         *        !/*st         15*/
!                                                                      *        !/*st         16*/
!                  created by a. ferrari & p. sala on 14-jan-1990      *        !/*st         17*/
!                                                                      *        !/*st         18*/
!          included in:                                                *        !/*st         19*/
!                        bdnopt                                        *        !/*st         20*/
!                        fluka (main)                                  *        !/*st         21*/
!                        prolog                                        *        !/*st         22*/
!                        electr (new version)                          *        !/*st         23*/
!                                                                      *        !/*st         24*/
!                        stepmn  = minimum step size (cm)              *        !/*st         25*/
!                        stepmx  = maximum step size (cm)              *        !/*st         26*/
!                        mxxrgn = maximum number of regions            *        !/*st         27*/
!                                                                      *        !/*st         28*/
!----------------------------------------------------------------------*        !/*st         29*/
!                                                                               !/*st         30*/
      common /stepsz/ stepmn(mxxrgn), stepmx(mxxrgn)                            !/*st         31*/

!     $Id: bamjcm.h,v 1.4 2002/03/27 18:14:06 gwm Exp $                       !/*ba          1*/
!=== bamjcm ===========================================================*        !/*ba          2*/
!                                                                               !/*ba          3*/
!                                                                      *        !/*ba          4*/
!----------------------------------------------------------------------*        !/*ba          5*/
!                                                                      *        !/*ba          6*/
!     bamjcm by a. ferrari                                             *        !/*ba          7*/
!                           included in:                               *        !/*ba          8*/
!                                        bamjet                        *        !/*ba          9*/
!     when changing kmxjcm dimension look also at verein!!!!!          *        !/*ba         10*/
!----------------------------------------------------------------------*        !/*ba         11*/
!                                                                      *        !/*ba         12*/
      parameter (kmxjcm=100)                                                    !/*ba         13*/
      common /bamjcm/ rpx(kmxjcm), rpy(kmxjcm), re(kmxjcm), kfr1(kmxjcm)        !/*ba         14*/
     & , kfr2(kmxjcm), iv(kmxjcm)                                               !/*ba         15*/

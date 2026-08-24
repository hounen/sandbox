!     $Id: trackr.h,v 1.4 2002/03/27 18:14:19 gwm Exp $                       !/*tr          1*/
!=== trackr ==========================================================*         !/*tr          2*/
!                                                                     *         !/*tr          3*/
!---------------------------------------------------------------------*         !/*tr          4*/
!                                                                     *         !/*tr          5*/
!     tracks recording       by  alfredo ferrari, infn - milan        *         !/*tr          6*/
!            included in :                                            *         !/*tr          7*/
!                          ausgab                                     *         !/*tr          8*/
!                          electr                                     *         !/*tr          9*/
!                          kaskad (new version)                       *         !/*tr         10*/
!                          photon                                     *         !/*tr         11*/
!                          magmov                                     *         !/*tr         12*/
!                          move                                       *         !/*tr         13*/
!                                                                     *         !/*tr         14*/
!---------------------------------------------------------------------*         !/*tr         15*/
!                                                                     *         !/*tr         16*/
      parameter (mxtrck=150)                                                    !/*tr         17*/
      common /trackr/ xtrack(0:mxtrck), ytrack(0:mxtrck), ztrack(0              !/*tr         18*/
     & :mxtrck), ttrack(mxtrck), dtrack(mxtrck), etrack, wtrack, ntrack,        !/*tr         19*/
     & mtrack, jtrack                                                           !/*tr         20*/

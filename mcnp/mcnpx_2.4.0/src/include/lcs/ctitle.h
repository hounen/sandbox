!     $Id: ctitle.h,v 1.4 2002/03/27 18:14:13 gwm Exp $                       !/*ct          1*/
!=== ctitle ===========================================================*        !/*ct          2*/
!                                                                               !/*ct          3*/
!----------------------------------------------------------------------*        !/*ct          4*/
!                                                                      *        !/*ct          5*/
!   common ctitle: it allows the identification of the run             *        !/*ct          6*/
!                                created by a. ferrari on 10-feb-1990  *        !/*ct          7*/
!                                                                      *        !/*ct          8*/
!          included in:                                                *        !/*ct          9*/
!                        fluka (main)                                  *        !/*ct         10*/
!                        bdnopt                                        *        !/*ct         11*/
!                        detect                                        *        !/*ct         12*/
!                        usrbin                                        *        !/*ct         13*/
!                                                                      *        !/*ct         14*/
!----------------------------------------------------------------------*        !/*ct         15*/
!                                                                               !/*ct         16*/
      character runtit*80, runtim*32                                            !/*ct         17*/
      common /ctitle/ runtit, runtim                                            !/*ct         18*/

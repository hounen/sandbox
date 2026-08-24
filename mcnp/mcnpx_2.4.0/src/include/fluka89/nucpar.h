!     $Id: nucpar.h,v 1.4 2002/03/27 18:14:08 gwm Exp $                       !/*nu          1*/
!=== nucpar ===========================================================*        !/*nu          2*/
!                                                                               !/*nu          3*/
!----------------------------------------------------------------------*        !/*nu          4*/
!     created on 6-9-89 by a. ferrari                                  *        !/*nu          5*/
!                       included in:                                   *        !/*nu          6*/
!                                    nucevt                            *        !/*nu          7*/
!                                    evxtes                            *        !/*nu          8*/
!                                    distnu                            *        !/*nu          9*/
!                                    eventq                            *        !/*nu         10*/
!----------------------------------------------------------------------*        !/*nu         11*/
      parameter (mxpnuc=600)                                                    !/*nu         12*/
      character*8 annu                                                          !/*nu         13*/
      common /nucpar/ pxnu(mxpnuc), pynu(mxpnuc), pznu(mxpnuc), hepnu           !/*nu         14*/
     & (mxpnuc), amnu(mxpnuc), annu(mxpnuc), ichnu(mxpnuc), ibarnu              !/*nu         15*/
     & (mxpnuc), nrenu(mxpnuc)                                                  !/*nu         16*/

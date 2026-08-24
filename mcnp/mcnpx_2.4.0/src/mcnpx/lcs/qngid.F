!     $Id: qngid.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*qn          1*/
      subroutine qngid                                                          !/*qn          2*/
! QNGID      calls...  ERRORF  FLTRNF  POLRN  AZIRN                             !/*qn          3*/
!     QNGID  called by...   QOU17  QOU21                                        !/*qn          4*/
#include <lcs/double.h>                                                         /*!qn          5*/
#include <lcs/isobar.h>                                                         /*!qn          6*/
!.....                                                                          !/*qn          7*/
!     IT is undefined in original.                                              !/*qn          8*/
      data it /0/                                                               !/*qn          9*/
!.....                                                                          !/*qn         10*/
!     ******************************************************************        !/*qn         11*/
!****    calculates  cos and sin theta,sin and cos phi    **************        !/*qn         12*/
!     ******************************************************************        !/*qn         13*/
!****  incident particle - nucleon                                              !/*qn         14*/
!****  single  production                                                       !/*qn         15*/
      if (rlke_.gt.35.d+02) call errorf ('qngid.1')                             !/*qn         16*/
      if (rlke_.lt.5.d+02) go to 30                                             !/*qn         17*/
      tesiso=0.75d0                                                             !/*qn         18*/
      if (rlke_.lt.1.d+03) go to 10                                             !/*qn         19*/
      tesiso=0.5d0                                                              !/*qn         20*/
      if (rlke_.lt.13.d+02) go to 10                                            !/*qn         21*/
      tesiso=0.25d0                                                             !/*qn         22*/
      if (rlke_.lt.25.d+02) go to 10                                            !/*qn         23*/
      go to 20                                                                  !/*qn         24*/
   10 continue                                                                  !/*qn         25*/
      r=rang()                                                                  !/*qn         26*/
      if (r.le.tesiso) go to 30                                                 !/*qn         27*/
!**** backward/forward                                                          !/*qn         28*/
   20 continue                                                                  !/*qn         29*/
      r=rang()                                                                  !/*qn         30*/
!****  test for direction                                                       !/*qn         31*/
      cst_=0.9999995d0                                                          !/*qn         32*/
      snt_=0.003162d0                                                           !/*qn         33*/
      if (r.le.dph) go to 40                                                    !/*qn         34*/
      cst_=-0.9999995d0                                                         !/*qn         35*/
      go to 40                                                                  !/*qn         36*/
!****  isotropic                                                                !/*qn         37*/
   30 call polrn (cst_,snt_)                                                    !/*qn         38*/
!****  calculates  cos,sin phi                                                  !/*qn         39*/
   40 call azirn (sopc_,sops_)                                                  !/*qn         40*/
      return                                                                    !/*qn         41*/
      end                                                                       !/*qn         42*/

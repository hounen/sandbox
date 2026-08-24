      block data  bd3                                                           !/*bd          1*/
#ifdef CEM                                                                      /*!bd          2*/
! ======================================================================        !/*bd          3*/
!                                                                               !/*bd          4*/
!     This subroutine sets up the masses of the common blocks mnuc and          !/*bd          5*/
!     mpi and defines pi of common block pi                                     !/*bd          6*/
!     all within subroutine initial.                                            !/*bd          7*/
!     It also initializes the  arrays eb and egs of common block ajsbar         !/*bd          8*/
!     for function bf.                                                          !/*bd          9*/
!     It also initializes icount and ifil of the common block outsid            !/*bd         10*/
!     for subroutine exceed.                                                    !/*bd         11*/
!     It also initializes the arrays w and fiks  of the common block            !/*bd         12*/
!     cohelp for subroutine gu8.                                                !/*bd         13*/
!     It also initializes the array gb of the common block gambet               !/*bd         14*/
!     for subroutine precof.                                                    !/*bd         15*/
!                                                                               !/*bd         16*/
!   CEM95 written by S. G. Mashnik                                              !/*bd         17*/
!                                                                               !/*bd         18*/
!   Edited by A. J. Sierk,  LANL T-2, February, 1996.                           !/*bd         19*/
!   Edited by AJS, July-August, 1997.                                           !/*bd         20*/
!   Edited by F.Gallmeier,  ORNL CPED  March 1998                               !/*bd         21*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*bd         22*/
!                                                                               !/*bd         23*/
! ======================================================================        !/*bd         24*/
#include <cem/cdouble.h>                                                        /*!bd         25*/
      common /pii/    pi                                                        !/*bd         26*/
      common /mnuc/   emnucm, emnucg, emnucb, emneut, emprot                    !/*bd         27*/
      common /mpi/    empich, empi0                                             !/*bd         28*/
      common /ajsbar/ ainit, zinit, eb(20,50,90), egs(20,50,90)                 !/*bd         29*/
      common /outsid/ icount,ifil,nout(1000),izout(1000),inout(1000)            !/*bd         30*/
      common /cohelp/ w(8), fiks(8)                                             !/*bd         31*/
      common /gambet/ gb(6)                                                     !/*bd         32*/
      common /cascnt/ ncaa                                                      !/*bd         33*/
      common /initiz/ inifst                                                    !/*bd         34*/
      data emnucm,emnucg,emnucb / 938.919d+0,0.938919d+0,0.9315014d+0/          !/*bd         35*/
      data emneut,emprot,empich,empi0 / 0.9395656d+0, 0.9382723d+0,             !/*bd         36*/
     &                                  0.139568d+0, 0.134973d+0/               !/*bd         37*/
      data pi /3.1415926536d0/                                                  !/*bd         38*/
      data eb, egs /90000*0.0d+0, 90000*0.0d+0/                                 !/*bd         39*/
      data icount, ifil /0, 0/                                                  !/*bd         40*/
!  Weights                                                                      !/*bd         41*/
      data w /0.1012285363d+0, 0.2223810345d+0,                                 !/*bd         42*/
     &        0.3137066459d+0, 0.3626837834d+0,                                 !/*bd         43*/
     &        0.3626837834d+0, 0.3137066459d+0,                                 !/*bd         44*/
     &        0.2223810345d+0, 0.1012285363d+0/                                 !/*bd         45*/
!  Abscissas                                                                    !/*bd         46*/
      data fiks /0.9602898565d+0, 0.7966664774d+0,                              !/*bd         47*/
     &           0.5255324099d+0, 0.1834346425d+0,                              !/*bd         48*/
     &          -0.1834346425d+0,-0.5255324099d+0,                              !/*bd         49*/
     &          -0.7966664774d+0,-0.9602898565d+0/                              !/*bd         50*/
      data gb /6*1.0d+0/                                                        !/*bd         51*/
      data ncaa /0/                                                             !/*bd         52*/
      data inifst/0/                                                            !/*bd         53*/
! ======================================================================        !/*bd         54*/
#endif                                                                          /*!bd         55*/
      end                                                                       !/*bd         56*/

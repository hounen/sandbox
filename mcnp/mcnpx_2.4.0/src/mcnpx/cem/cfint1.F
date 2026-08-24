      function cfint1 (fname, flim1, flim2, bs, rs)                             !/*cf          1*/
! ======================================================================        !/*cf          2*/
!                                                                               !/*cf          3*/
!   Gaussian quadrature; integral from flim2 to flim1 of fname.                 !/*cf          4*/
!   Identical to fint; Needed to prevent a recursion when fint (fis...)         !/*cf          5*/
!   is called, since fis uses fint(1).                                          !/*cf          6*/
!                                                                               !/*cf          7*/
!   CEM95 written by S. G. Mashnik                                              !/*cf          8*/
!                                                                               !/*cf          9*/
!   Edited by A. J. Sierk  LANL  T-2  February-March, 1996.                     !/*cf         10*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*cf         11*/
!                                                                               !/*cf         12*/
! ======================================================================        !/*cf         13*/
#include <cem/cdouble.h>                                                        /*!cf         14*/
#ifndef CEM                                                                     /*!cf         15*/
      cfint1=dp0                                                                !/*cf         16*/
#else                                                                           /*!cf         17*/
      common /cohelp/ w(8), fiks(8)                                             !/*cf         18*/
! ======================================================================        !/*cf         19*/
      temp = dp0                                                                !/*cf         20*/
      t1 = dph*(flim1 - flim2)                                                  !/*cf         21*/
        do 30 it = 1,8                                                          !/*cf         22*/
        fy = t1*fiks(it) + (flim1 + flim2)/dp2                                  !/*cf         23*/
        temp = temp + w(it)*fname (fy, bs, rs)                                  !/*cf         24*/
   30   continue                                                                !/*cf         25*/
      cfint1 = temp*t1                                                          !/*cf         26*/
#endif                                                                          /*!cf         27*/
      return                                                                    !/*cf         28*/
      end                                                                       !/*cf         29*/

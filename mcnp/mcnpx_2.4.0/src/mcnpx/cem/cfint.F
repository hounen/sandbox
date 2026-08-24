      function cfint (fname, flim1, flim2, bs, rs)                              !/*cf          1*/
! ======================================================================        !/*cf          2*/
!                                                                               !/*cf          3*/
!   Gaussian quadrature; integral from flim2 to flim1 of fname.                 !/*cf          4*/
!                                                                               !/*cf          5*/
!   CEM95 written by S. G. Mashnik                                              !/*cf          6*/
!                                                                               !/*cf          7*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*cf          8*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*cf          9*/
!                                                                               !/*cf         10*/
! ======================================================================        !/*cf         11*/
#include <cem/cdouble.h>                                                        /*!cf         12*/
#ifndef CEM                                                                     /*!cf         13*/
      cfint=dp0                                                                 !/*cf         14*/
#else                                                                           /*!cf         15*/
      common /cohelp/ w(8), fiks(8)                                             !/*cf         16*/
! ======================================================================        !/*cf         17*/
      temp = dp0                                                                !/*cf         18*/
      t1 = dph*(flim1 - flim2)                                                  !/*cf         19*/
        do 30 it = 1,8                                                          !/*cf         20*/
        fy = t1*fiks(it) + (flim1 + flim2)/dp2                                  !/*cf         21*/
        temp = temp + w(it)*fname (fy, bs, rs)                                  !/*cf         22*/
   30   continue                                                                !/*cf         23*/
      cfint = temp*t1                                                           !/*cf         24*/
#endif                                                                          /*!cf         25*/
      return                                                                    !/*cf         26*/
      end                                                                       !/*cf         27*/

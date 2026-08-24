!     $Id: calcu.F,v 1.4 2002/03/27 18:14:56 gwm Exp $                        !/*ca          1*/
      subroutine calcu (u3,ws5,ws4,u,v)                                         !/*ca          2*/
#include <lcs/double.h>                                                         /*!ca          3*/
!     ==================================================================        !/*ca          4*/
!     calcu calculates the total energy in the center of mass                   !/*ca          5*/
!     in unit of bev between an isobar and a stationary proton or neutro        !/*ca          6*/
!     this routine is called only by amfp.                                      !/*ca          7*/
!                                                                               !/*ca          8*/
! CALCU  calls   SQRT                                                           !/*ca          9*/
! called by AMFP                                                                !/*ca         10*/
!     ==================================================================        !/*ca         11*/
!                                                                               !/*ca         12*/
#include <lcs/vegas.h>                                                          /*!ca         13*/
! u3 contains the mass of the proton or neutron.                                !/*ca         14*/
! ws5 and ws4 are the total energy and mass of the isobar.                      !/*ca         15*/
! v is the mass of the isobar in unit of bev.                                   !/*ca         16*/
      u1=ws4*ws4+u3*u3                                                          !/*ca         17*/
      u=1.d-03*sqrt(dp2*u3*ws5+u1)                                              !/*ca         18*/
      if (u-2.1d0) 30,10,10                                                     !/*ca         19*/
   10 v=ws4*1.d-03                                                              !/*ca         20*/
      if (v-1.11d0) 40,20,20                                                    !/*ca         21*/
   20 return                                                                    !/*ca         22*/
! tables for isobar scattering have a minimum u and v of 2.1 and 1.11           !/*ca         23*/
!  and if the calculated u and v are less than these, then u and v are          !/*ca         24*/
!  set to 2.1 and 1.11.                                                         !/*ca         25*/
   30 u=2.1d0                                                                   !/*ca         26*/
      go to 10                                                                  !/*ca         27*/
   40 v=1.11d0                                                                  !/*ca         28*/
      go to 20                                                                  !/*ca         29*/
      end                                                                       !/*ca         30*/

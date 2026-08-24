!     $Id: crsec.F,v 1.5 2002/03/27 18:14:59 gwm Exp $                        !/*cr          1*/
      subroutine crsec (e,inc,sigt,sigel,signel)                                !/*cr          2*/
! CRSEC      calls...  ERRORF  SQRT                                             !/*cr          3*/
!     CRSEC  called by...   HYD                                                 !/*cr          4*/
#include <lcs/double.h>                                                         /*!cr          5*/
! calculates proton and neutron cross sections for energies greater than        !/*cr          6*/
! calculates pi+    and pi-     cross sections for energies greater than        !/*cr          7*/
!       the target particle is a proton                                         !/*cr          8*/
!       e ------- kinetic energy of incident particle(mev)                      !/*cr          9*/
!       inc ----- type of incident particle                                     !/*cr         10*/
!                   0 -- proton                                                 !/*cr         11*/
!                   1    neutron                                                !/*cr         12*/
!                   2    pi+                                                    !/*cr         13*/
!                   3    not used                                               !/*cr         14*/
!                   4    pi-                                                    !/*cr         15*/
!       sigt ---- total cross section(cm**2)                                    !/*cr         16*/
!       sigel --- elastic cross section(cm**2)                                  !/*cr         17*/
!       signel -- nonelastic cross section(cm**2)                               !/*cr         18*/
!                                                                               !/*cr         19*/
#include <lcs/inout.h>                                                          /*!cr         20*/
      inc1=inc+1                                                                !/*cr         21*/
      e1=e*1.d-3                                                                !/*cr         22*/
      go to (20,30,50,10,80), inc1                                              !/*cr         23*/
   10 call errorf ('crsec')                                                     !/*cr         24*/
!                                                                               !/*cr         25*/
   20 sigt=37.5d0+7.d0/sqrt(e1)                                                 !/*cr         26*/
      sigel=7.d0+21.03d0/(e1**0.873d0)                                          !/*cr         27*/
      go to 110                                                                 !/*cr         28*/
!                                                                               !/*cr         29*/
   30 if (e1.gt.8.d0) go to 40                                                  !/*cr         30*/
      sigt=42.d0                                                                !/*cr         31*/
      sigel=-0.222d0*e1+12.48d0                                                 !/*cr         32*/
      go to 110                                                                 !/*cr         33*/
   40 sigt=37.5d0+26.45d0/(e1**0.852d0)                                         !/*cr         34*/
      sigel=6.d0+73.144d0/(e1**1.32d0)                                          !/*cr         35*/
      go to 110                                                                 !/*cr         36*/
!                                                                               !/*cr         37*/
   50 if (e1.gt.dp3) go to 60                                                   !/*cr         38*/
      sigt=-4.66d0*e1+42.95d0                                                   !/*cr         39*/
      sigel=-1.4d0*e1+10.d0                                                     !/*cr         40*/
      go to 110                                                                 !/*cr         41*/
   60 if (e1.gt.6.d0) go to 70                                                  !/*cr         42*/
      sigt=-0.7567d0*e1+31.24d0                                                 !/*cr         43*/
      sigel=-0.166d0*e1+6.298d0                                                 !/*cr         44*/
      go to 110                                                                 !/*cr         45*/
   70 sigt=21.5d0+18.914d0/(e1**.7155d0)                                        !/*cr         46*/
      sigel=3.5d0+9.93d0/(e1**.953d0)                                           !/*cr         47*/
      go to 110                                                                 !/*cr         48*/
!                                                                               !/*cr         49*/
   80 if (e1.gt.dp5) go to 90                                                   !/*cr         50*/
      sigt=-1.66d0*e1+37.34d0                                                   !/*cr         51*/
      go to 100                                                                 !/*cr         52*/
   90 sigt=23.6d0+25.51d0/(e1**0.959d0)                                         !/*cr         53*/
  100 sigel=3.8d0+7.273d0/(e1**0.896d0)                                         !/*cr         54*/
!                                                                               !/*cr         55*/
  110 sigt=sigt*1.d-27                                                          !/*cr         56*/
      sigel=sigel*1.d-27                                                        !/*cr         57*/
      signel=sigt-sigel                                                         !/*cr         58*/
!                                                                               !/*cr         59*/
      if ((inc1.le.2).and.(e1.lt.3.4949d0)) write (io,120)                      !/*cr         60*/
      if ((inc1.gt.2).and.(e1.lt.2.4949d0)) write (io,130)                      !/*cr         61*/
      return                                                                    !/*cr         62*/
!                                                                               !/*cr         63*/
  120 format (1h1,'  proton or neutron cross-sections are being ',              !/*cr         64*/
     & 'requested for an energy less than 3.5-gev ')                            !/*cr         65*/
  130 format (1h1,'  pi+    or pi-     cross-sections are being ',              !/*cr         66*/
     & 'requested for an energy less than 2.5-gev ')                            !/*cr         67*/
      end                                                                       !/*cr         68*/

!     $Id: akeka.F,v 1.4 2002/03/27 18:14:38 gwm Exp $                        !/*ak          1*/
      double precision function akeka (it,to,amss)                              !/*ak          2*/
!                                                                     *         !/*ak          3*/
!---------------------------------------------------------------------*         !/*ak          4*/
!                                                                     *         !/*ak          5*/
#include <lcs/double.h>                                                         /*!ak          6*/
#include <fluka89/flprm1.h>                                                     /*!ak          7*/
#include <fluka89/flprm2.h>                                                     /*!ak          8*/
#include <lcs/inout.h>                                                          /*!ak          9*/
!********************************************************************           !/*ak         10*/
!     version by                     j. ranft                                   !/*ak         11*/
!                                    leipzig                                    !/*ak         12*/
!     last change 05. december 83    pertti aarnio                              !/*ak         13*/
!                                    helsinki university of                     !/*ak         14*/
!                                    technology, finland                        !/*ak         15*/
!                                                                               !/*ak         16*/
!     to be called from high energy production                                  !/*ak         17*/
!                                                                               !/*ak         18*/
!     slope of the energy distribution of the higher energy particles           !/*ak         19*/
!     of the intranuclear cascade protons and neutrons (alfa-2p                 !/*ak         20*/
!     and alfa-2n). note that it is assumed in ekeka and rakeka                 !/*ak         21*/
!     that alfa-2n=6*alfa-1n and alfa-2p=6*alfa-1p.                             !/*ak         22*/
!                                                                               !/*ak         23*/
!     note!!!!!!! non-standard particle numbering                               !/*ak         24*/
!                                                                               !/*ak         25*/
!     input variables:                                                          !/*ak         26*/
!        it   = type of the secondary; 1=proton, 2=neutron                      !/*ak         27*/
!        to   = kinetic energy of the primary hadron in gev                     !/*ak         28*/
!        amss = atomic weight of the nucleus                                    !/*ak         29*/
!                                                                               !/*ak         30*/
!     see ranft/routti particle acc. vol 4 p 105                                !/*ak         31*/
!********************************************************************           !/*ak         32*/
!                                                                               !/*ak         33*/
      dimension a(2), b(2)                                                      !/*ak         34*/
      data a /0.11d0,0.1d0/                                                     !/*ak         35*/
      data b /0.21d0,0.20d0/                                                    !/*ak         36*/
      if (to.ge.10.d0) go to 10                                                 !/*ak         37*/
      akeka=(1.d0-0.001d0*amss)*(a(it)+0.01d0*to)                               !/*ak         38*/
      return                                                                    !/*ak         39*/
   10 continue                                                                  !/*ak         40*/
      akeka=b(it)*(1.d0-0.001d0*amss)                                           !/*ak         41*/
      return                                                                    !/*ak         42*/
      end                                                                       !/*ak         43*/

!     $Id: save.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                         !/*sa          1*/
      subroutine save                                                           !/*sa          2*/
#include <lcs/double.h>                                                         /*!sa          3*/
!     ==================================================================        !/*sa          4*/
!     save saves some quantities from ws into wws.  in case the                 !/*sa          5*/
!     isobar makes a forbidden collision, such as forbidden charge              !/*sa          6*/
!     exchange scattering, capture, or decay.  also, when a pion                !/*sa          7*/
!     tries to interact, save ws into wws, so that if an isobar is              !/*sa          8*/
!     formed, wws is stored into st via subr wwsst.                             !/*sa          9*/
!                                                                               !/*sa         10*/
! SAVE   called by GAMMA  PSEL                                                  !/*sa         11*/
!     ==================================================================        !/*sa         12*/
!                                                                               !/*sa         13*/
#include <lcs/vegas.h>                                                          /*!sa         14*/
      do 10 i=2,9                                                               !/*sa         15*/
   10 wws(i)=ws(i)                                                              !/*sa         16*/
      wws(24)=ws(24)                                                            !/*sa         17*/
      wws(31)=ws(31)                                                            !/*sa         18*/
      return                                                                    !/*sa         19*/
      end                                                                       !/*sa         20*/

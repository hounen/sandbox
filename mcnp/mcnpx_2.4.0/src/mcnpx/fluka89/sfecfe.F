!     $Id: sfecfe.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                       !/*sf          1*/
      subroutine sfecfe (sfe,cfe)                                               !/*sf          2*/
!                                                                     *         !/*sf          3*/
!---------------------------------------------------------------------*         !/*sf          4*/
!                                                                     *         !/*sf          5*/
#include <lcs/double.h>                                                         /*!sf          6*/
#include <fluka89/flprm1.h>                                                     /*!sf          7*/
#include <fluka89/flprm2.h>                                                     /*!sf          8*/
#include <lcs/inout.h>                                                          /*!sf          9*/
      entry cosi(sfe,cfe)                                                       !/*sf         10*/
!********************************************************************           !/*sf         11*/
!     version june 81 by             pertti aarnio                              !/*sf         12*/
!     last change 11. december 85 by pertti aarnio                              !/*sf         13*/
!                                    helsinki university of                     !/*sf         14*/
!                                    technology, finland                        !/*sf         15*/
!                                                                               !/*sf         16*/
!     subroutine of fluka to give sine and cosine of an                         !/*sf         17*/
!     random angle uniformly distributed between 0 and 2*pi                     !/*sf         18*/
!********************************************************************           !/*sf         19*/
!                                                                               !/*sf         20*/
   10 x=2.d0*rang()-1.d0                                                        !/*sf         21*/
      y=rang()                                                                  !/*sf         22*/
      x2=x*x                                                                    !/*sf         23*/
      y2=y*y                                                                    !/*sf         24*/
      if (x2+y2.gt.1.d0) go to 10                                               !/*sf         25*/
      cfe=(x2-y2)/(x2+y2)                                                       !/*sf         26*/
      sfe=2.d0*x*y/(x2+y2)                                                      !/*sf         27*/
      return                                                                    !/*sf         28*/
      end                                                                       !/*sf         29*/

!     $Id: resto.F,v 1.4 2002/03/27 18:15:20 gwm Exp $                        !/*re          1*/
      subroutine resto                                                          !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
!     ==================================================================        !/*re          4*/
!     resto restores some quantities that had been stored in ww                 !/*re          5*/
!     into ws.  for example, if an isobar has made a forbidden charge           !/*re          6*/
!     exchange scattering, capture, or decay, then the characteristics          !/*re          7*/
!     of the original isobar has to be stored back into ws.                     !/*re          8*/
!                                                                               !/*re          9*/
! RESTO  called by GAMMA                                                        !/*re         10*/
!     ==================================================================        !/*re         11*/
!                                                                               !/*re         12*/
#include <lcs/vegas.h>                                                          /*!re         13*/
      do 10 i=3,9                                                               !/*re         14*/
      ws(i)=wws(i)                                                              !/*re         15*/
   10 continue                                                                  !/*re         16*/
      ws(24)=wws(24)                                                            !/*re         17*/
      ws(31)=wws(31)                                                            !/*re         18*/
      return                                                                    !/*re         19*/
      end                                                                       !/*re         20*/

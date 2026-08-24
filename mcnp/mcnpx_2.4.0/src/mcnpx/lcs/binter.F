!     $Id: binter.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                       !/*bi          1*/
      subroutine binter (e,ener,i,aint3,aint4,aint5)                            !/*bi          2*/
#include <lcs/double.h>                                                         /*!bi          3*/
!     ==================================================================        !/*bi          4*/
!     binter calculates the coefficients for triple interpolation               !/*bi          5*/
!                                                                               !/*bi          6*/
! BINTER called by AIANGL  AITOTA                                               !/*bi          7*/
!     ==================================================================        !/*bi          8*/
!                                                                               !/*bi          9*/
      dimension ener(40)                                                        !/*bi         10*/
      aint=ener(i-1)-ener(i)                                                    !/*bi         11*/
      aint=(e-ener(i+1))/aint                                                   !/*bi         12*/
      aint1=ener(i-1)-ener(i+1)                                                 !/*bi         13*/
      aint1=(e-ener(i))/aint1                                                   !/*bi         14*/
      aint2=ener(i)-ener(i+1)                                                   !/*bi         15*/
      aint2=(e-ener(i-1))/aint2                                                 !/*bi         16*/
      aint5=aint2*aint1                                                         !/*bi         17*/
      aint3=aint*aint1                                                          !/*bi         18*/
      aint4=-aint2*aint                                                         !/*bi         19*/
      return                                                                    !/*bi         20*/
      end                                                                       !/*bi         21*/

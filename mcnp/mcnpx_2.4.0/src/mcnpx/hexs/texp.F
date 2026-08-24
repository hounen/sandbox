!     $Id: texp.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                         !/*te          1*/
      function texp (x)                                                         !/*te          2*/
!  NASA subroutine                                                              !/*te          3*/
#include <lcs/double.h>                                                         /*!te          4*/
!                                                                               !/*te          5*/
!   purpose                                                                     !/*te          6*/
!   to eliminate over/under flow of cpu if exp is used                          !/*te          7*/
!                                                                               !/*te          8*/
!   result = texp(x)                                                            !/*te          9*/
      if (x.lt.-80.d0) x=-80.d0                                                 !/*te         10*/
      if (x.gt.80.d0) x=80.d0                                                   !/*te         11*/
      texp=exp(x)                                                               !/*te         12*/
      end                                                                       !/*te         13*/

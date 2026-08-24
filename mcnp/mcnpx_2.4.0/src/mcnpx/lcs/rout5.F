!     $Id: rout5.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                        !/*ro          1*/
      subroutine rout5 (t,b,r)                                                  !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension t(126), b(126), r(126)                                          !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      if (not-2) 10,20,30                                                       !/*ro          6*/
   10 call crjab (1,t(1))                                                       !/*ro          7*/
      go to 40                                                                  !/*ro          8*/
!     (pip-p)elastic scattering crs.                                            !/*ro          9*/
   20 call crjab (1,b(1))                                                       !/*ro         10*/
      go to 40                                                                  !/*ro         11*/
!     (pim-p)direct scattering crs.                                             !/*ro         12*/
   30 call crjab (1,r(1))                                                       !/*ro         13*/
   40 return                                                                    !/*ro         14*/
      end                                                                       !/*ro         15*/

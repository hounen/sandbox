!     $Id: rout4.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                        !/*ro          1*/
      subroutine rout4                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      call geo                                                                  !/*ro          5*/
      if (i1) 20,10,10                                                          !/*ro          6*/
   10 curr(3)=pnms                                                              !/*ro          7*/
!     pi+ or -mass/cm                                                           !/*ro          8*/
      curr(1)=no                                                                !/*ro          9*/
      call partin                                                               !/*ro         10*/
      call spac32 (32)                                                          !/*ro         11*/
   20 return                                                                    !/*ro         12*/
      end                                                                       !/*ro         13*/

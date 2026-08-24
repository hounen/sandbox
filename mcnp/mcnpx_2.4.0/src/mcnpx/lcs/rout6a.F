!     $Id: rout6a.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                       !/*ro          1*/
      subroutine rout6a                                                         !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
   10 i1=0                                                                      !/*ro          5*/
      call spisom                                                               !/*ro          6*/
      strkp=-dp2                                                                !/*ro          7*/
      i1=1                                                                      !/*ro          8*/
      call spisom                                                               !/*ro          9*/
      strkp=-dp1                                                                !/*ro         10*/
      com=(awd(med)-7.d0)*dp2*rcpmv                                             !/*ro         11*/
      if (com-e(2)) 10,10,20                                                    !/*ro         12*/
   20 pm(2)=dp2*dncms                                                           !/*ro         13*/
      pm(3)=dncms                                                               !/*ro         14*/
      e(2)=pm(2)+e(2)                                                           !/*ro         15*/
      return                                                                    !/*ro         16*/
      end                                                                       !/*ro         17*/

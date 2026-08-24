!     $Id: bg6b.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                         !/*bg          1*/
      subroutine bg6b                                                           !/*bg          2*/
#include <lcs/double.h>                                                         /*!bg          3*/
#include <lcs/btcom.h>                                                          /*!bg          4*/
      call zero                                                                 !/*bg          5*/
      j=i2                                                                      !/*bg          6*/
      do 10 i=2,i4                                                              !/*bg          7*/
      ce(i)=space(j)                                                            !/*bg          8*/
   10 j=j+1                                                                     !/*bg          9*/
      j=i4+1                                                                    !/*bg         10*/
      do 20 i=j,6                                                               !/*bg         11*/
      ce(i)=s(i3)                                                               !/*bg         12*/
   20 i3=i3+1                                                                   !/*bg         13*/
      return                                                                    !/*bg         14*/
      end                                                                       !/*bg         15*/

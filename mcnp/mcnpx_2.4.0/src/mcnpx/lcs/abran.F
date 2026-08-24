!     $Id: abran.F,v 1.4 2002/03/27 18:14:52 gwm Exp $                        !/*ab          1*/
      subroutine abran (k1)                                                     !/*ab          2*/
#include <lcs/double.h>                                                         /*!ab          3*/
#include <lcs/btcom.h>                                                          /*!ab          4*/
      value1=rang()                                                             !/*ab          5*/
      value1=value1*sign                                                        !/*ab          6*/
      value2=dp0                                                                !/*ab          7*/
      not=1                                                                     !/*ab          8*/
      do 10 i=2,k1                                                              !/*ab          9*/
      value2=ce(i)+value2                                                       !/*ab         10*/
      if (value2-value1) 10,20,20                                               !/*ab         11*/
   10 not=not+1                                                                 !/*ab         12*/
   20 return                                                                    !/*ab         13*/
!     value2=sum of ce for a particular region--sum f(i1)mass                   !/*ab         14*/
      end                                                                       !/*ab         15*/

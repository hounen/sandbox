!     $Id: big7.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                         !/*bi          1*/
      subroutine big7 (c,great,ix)                                              !/*bi          2*/
#include <lcs/double.h>                                                         /*!bi          3*/
      great=c                                                                   !/*bi          4*/
      i=ix+1                                                                    !/*bi          5*/
      do 10 k=1,i                                                               !/*bi          6*/
      c=rang()                                                                  !/*bi          7*/
      if (c.lt.great) go to 10                                                  !/*bi          8*/
      great=c                                                                   !/*bi          9*/
   10 continue                                                                  !/*bi         10*/
!     great is the largest of i random nos.                                     !/*bi         11*/
      return                                                                    !/*bi         12*/
      end                                                                       !/*bi         13*/

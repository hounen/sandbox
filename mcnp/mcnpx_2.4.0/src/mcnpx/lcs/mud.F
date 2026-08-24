!     $Id: mud.F,v 1.4 2002/03/27 18:15:11 gwm Exp $                          !/*mu          1*/
      subroutine mud (sine,inp)                                                 !/*mu          2*/
#include <lcs/double.h>                                                         /*!mu          3*/
      sine=rang()                                                               !/*mu          4*/
      sine=5.d1*sine                                                            !/*mu          5*/
      inp=int(sine+dp1)                                                         !/*mu          6*/
      sine=real(inp)-sine                                                       !/*mu          7*/
!     sine=(.02n-r)/.02=n-r/.02   n=inpt   r/.02=(n-1)+x                        !/*mu          8*/
      return                                                                    !/*mu          9*/
      end                                                                       !/*mu         10*/

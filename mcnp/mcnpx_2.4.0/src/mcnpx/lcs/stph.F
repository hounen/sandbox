!     $Id: stph.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                         !/*st          1*/
      subroutine stph (w)                                                       !/*st          2*/
#include <lcs/double.h>                                                         /*!st          3*/
      dimension w(11)                                                           !/*st          4*/
#include <lcs/btcom.h>                                                          /*!st          5*/
      if (pgvc(1)-44.d1) 10,40,40                                               !/*st          6*/
   10 i=pgvc(1)+2.005d0                                                         !/*st          7*/
      j=i+10                                                                    !/*st          8*/
!     pgvc(1)=no.of times velocity greater than criterion entered               !/*st          9*/
      l=1                                                                       !/*st         10*/
      do 20 k=i,j                                                               !/*st         11*/
      pgvc(k)=w(l)                                                              !/*st         12*/
   20 l=l+1                                                                     !/*st         13*/
      pgvc(1)=pgvc(1)+11.d0                                                     !/*st         14*/
   30 return                                                                    !/*st         15*/
   40 i1=1                                                                      !/*st         16*/
      go to 30                                                                  !/*st         17*/
      end                                                                       !/*st         18*/

!     $Id: nxtsym.F,v 1.4 2002/03/27 18:16:23 gwm Exp $                       !/*nx          1*/
      subroutine nxtsym (hs,hd,ir,it,iu)                                        !/*nx          2*/
!        find the next symbol in hs, starting at position ir.  the              !/*nx          3*/
!        found symbol starts at position it and ends at position iu.            !/*nx          4*/
!        iu=0 means no symbol was found, including ir > length of hs.           !/*nx          5*/
!        hd contains the delimiters.  delimiters other than blanks are          !/*nx          6*/
!        themselves symbols.                                                    !/*nx          7*/
#include <htape3x/com32b.h>                                                     /*!nx          8*/
      character hs*(*), hd*(*)                                                  !/*nx          9*/
!                                                                               !/*nx         10*/
      iu=0                                                                      !/*nx         11*/
      it=ir                                                                     !/*nx         12*/
      l=len(hs)                                                                 !/*nx         13*/
      if (ir.gt.l) return                                                       !/*nx         14*/
      if (index(hd,' ').eq.0) go to 20                                          !/*nx         15*/
      do 10 it=ir,l                                                             !/*nx         16*/
   10 if (hs(it:it).ne.' ') go to 20                                            !/*nx         17*/
      return                                                                    !/*nx         18*/
   20 do 30 iu=it,l                                                             !/*nx         19*/
   30 if (index(hd,hs(iu:iu)).ne.0) go to 40                                    !/*nx         20*/
   40 iu=max(it,iu-1)                                                           !/*nx         21*/
      return                                                                    !/*nx         22*/
      end                                                                       !/*nx         23*/

!     $Id: nxtsym.F,v 1.4 2002/03/27 18:14:22 gwm Exp $                       !/*nx          1*/
      subroutine nxtsym(hs,hd,ir,it,iu)                                         !/*nx          2*/
!        find the next symbol in hs, starting at position ir.  the              !/*nx          3*/
!        found symbol starts at position it and ends at position iu.            !/*nx          4*/
!        iu=0 means no symbol was found, including ir > length of hs.           !/*nx          5*/
!        hd contains the delimiters.  delimiters other than blanks are          !/*nx          6*/
!        themselves symbols.                                                    !/*nx          7*/
      character hs*(*),hd*(*)                                                   !/*nx          8*/
!                                                                               !/*nx          9*/
      iu=0                                                                      !/*nx         10*/
      it=ir                                                                     !/*nx         11*/
      l=len(hs)                                                                 !/*nx         12*/
      if(ir.gt.l)return                                                         !/*nx         13*/
      if(index(hd,' ').eq.0)go to 20                                            !/*nx         14*/
      do 10 it=ir,l                                                             !/*nx         15*/
   10 if(hs(it:it).ne.' ')go to 20                                              !/*nx         16*/
      return                                                                    !/*nx         17*/
   20 do 30 iu=it,l                                                             !/*nx         18*/
   30 if(index(hd,hs(iu:iu)).ne.0)go to 40                                      !/*nx         19*/
   40 iu=max(it,iu-1)                                                           !/*nx         20*/
      return                                                                    !/*nx         21*/
      end                                                                       !/*nx         22*/

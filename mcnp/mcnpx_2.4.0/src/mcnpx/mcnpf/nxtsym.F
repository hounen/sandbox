!#ident nxtsym.F from deck ns                                                   !/*ns          1*/
      subroutine nxtsym(hs,hd,ir,it,iu,ic)                                      !/*ns4a        1*/
!        find the next symbol in hs, starting at position ir.  the              !/*ns          3*/
!        found symbol starts at position it and ends at position iu.            !/*ns          4*/
!        iu=0 means no symbol was found, including ir > length of hs.           !/*ns          5*/
!        hd contains the delimiters.  delimiters other than blanks are          !/*ns          6*/
!        themselves symbols.  If ic=0 the case is not changed; if ic=1          !/*ns4a        2*/
!        the next symbol is reduced to lowercase; and if ic=2 a symbol          !/*ns4a        3*/
!        is not searched for and the entire hs string becomes lowercase.        !/*ns4a        4*/
      character hs*(*),hd*(*),hu*26,hl*26                                       !/*ns4a        5*/
!ccc     hu alphabet must be upper case.                                        !/*ns4a        6*/
      data hu/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/,                                    !/*ns4a        7*/
     &     hl/'abcdefghijklmnopqrstuvwxyz'/                                     !/*ns4a        8*/
!                                                                               !/*ns          9*/
      iu=0                                                                      !/*ns         10*/
      it=ir                                                                     !/*ns         11*/
      l=len(hs)                                                                 !/*ns         12*/
      if(ic.eq.2)go to 50                                                       !/*ns4a        9*/
      if(ir.gt.l)return                                                         !/*ns         13*/
      if(index(hd,' ').eq.0)go to 20                                            !/*ns         14*/
      do 10 it=ir,l                                                             !/*ns         15*/
   10 if(hs(it:it).ne.' ')go to 20                                              !/*ns         16*/
      return                                                                    !/*ns         17*/
   20 do 30 iu=it,l                                                             !/*ns         18*/
   30 if(index(hd,hs(iu:iu)).ne.0)go to 40                                      !/*ns         19*/
   40 iu=max(it,iu-1)                                                           !/*ns         20*/
      go to(80,60,50)ic+1                                                       !/*ns4a       10*/
   50 it=1                                                                      !/*ns4a       11*/
      iu=l                                                                      !/*ns4a       12*/
   60 do 70 i=it,iu                                                             !/*ns4a       13*/
      j=index(hu,hs(i:i))                                                       !/*ns4a       14*/
   70 if(j.ne.0)hs(i:i)=hl(j:j)                                                 !/*ns4a       15*/
   80 return                                                                    !/*ns4a       16*/
      end                                                                       !/*ns         22*/

!     $Id: addsym.F,v 1.5 2002/03/27 18:14:21 gwm Exp $                       !/*ad          1*/
      subroutine addsym(ha,hx,nl,ix,ie)                                         !/*ad          2*/
!        add the symbol in ha to the array of nl lines in hx(nl).               !/*ad          3*/
!        use a blank as the delimiter and do not cross line boundaries.         !/*ad          4*/
!        put a '+' at the end of the line when a new line is started.           !/*ad          5*/
!        return ix = index of the last non-blank line in hx.                    !/*ad          6*/
!        return with ie=0 if successful, ie=1 otherwise.                        !/*ad          7*/
      character ha*(*),hx(nl)*(*)                                               !/*ad          8*/
!                                                                               !/*ad          9*/
      ie=1                                                                      !/*ad         10*/
      if(ha.eq.' ')return                                                       !/*ad         11*/
      ll=len(hx(1))                                                             !/*ad         12*/
      do 30 ix=1,nl                                                             !/*ad         13*/
      do 10 ip=ll+1,2,-1                                                        !/*ad         14*/
   10 if(hx(ix)(ip-1:ip-1).ne.' ')go to 20                                      !/*ad         15*/
   20 if(ix.eq.nl)go to 40                                                      !/*ad         16*/
   30 if(hx(ix+1).eq.' ')go to 40                                               !/*ad         17*/
   40 call nxtsym(ha,' ',1,it,iu)                                               !/*ad         18*/
      jp=iu-it+1                                                                !/*ad         19*/
      if(jp.gt.ll)return                                                        !/*ad         20*/
      if(ip+jp.le.ll-2)go to 50                                                 !/*ad         21*/
      hx(ix)(ip+1:ip+1)='+'                                                     !/*ad         22*/
      ip=1                                                                      !/*ad         23*/
      ix=ix+1                                                                   !/*ad         24*/
      if(ix.gt.nl)return                                                        !/*ad         25*/
   50 hx(ix)(ip+1:ip+jp)=ha(it:iu)                                              !/*ad         26*/
      ie=0                                                                      !/*ad         27*/
      return                                                                    !/*ad         28*/
      end                                                                       !/*ad         29*/

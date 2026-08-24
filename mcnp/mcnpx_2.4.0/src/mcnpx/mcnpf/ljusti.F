!#ident ljusti.F from deck lj                                                   !/*lj          1*/
      subroutine ljusti(ha)                                                     !/*lj          2*/
!        left justify ha.  len(ha)<81 is required.                              !/*lj          3*/
      character ha*(*),hb*80                                                    !/*lj          4*/
!                                                                               !/*lj          5*/
      hb=ha                                                                     !/*lj          6*/
      if(hb.eq.' ')return                                                       !/*lj          7*/
      do 10 i=1,len(hb)                                                         !/*lj          8*/
   10 if(hb(i:i).ne.' ')go to 20                                                !/*lj          9*/
   20 ha=hb(i:80)                                                               !/*lj         10*/
      return                                                                    !/*lj         11*/
      end                                                                       !/*lj         12*/

!     $Id: leng.F,v 1.4 2002/03/27 18:13:50 gwm Exp $                         !/*le          1*/
      function leng(ha)                                                         !/*le          2*/
!        return the length of ha, excluding trailing blanks.                    !/*le          3*/
      character ha*(*)                                                          !/*le          4*/
!                                                                               !/*le          5*/
      leng=0                                                                    !/*le          6*/
      do 10 j=1,len(ha)                                                         !/*le          7*/
   10 if(ha(j:j).ne.' ')leng=j                                                  !/*le          8*/
      return                                                                    !/*le          9*/
      end                                                                       !/*le         10*/

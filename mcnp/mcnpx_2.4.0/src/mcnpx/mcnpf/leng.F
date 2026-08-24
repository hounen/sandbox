!#ident leng.F from deck ln                                                     !/*ln          1*/
      function leng(ha)                                                         !/*ln          2*/
!        return the length of ha, excluding trailing blanks.                    !/*ln          3*/
      character ha*(*)                                                          !/*ln          4*/
!                                                                               !/*ln          5*/
      leng=0                                                                    !/*ln          6*/
      do 10 j=1,len(ha)                                                         !/*ln          7*/
   10 if(ha(j:j).ne.' ')leng=j                                                  !/*ln          8*/
      return                                                                    !/*ln          9*/
      end                                                                       !/*ln         10*/

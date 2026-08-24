!#ident ckchar.F from deck kk                                                   !/*kk          1*/
      subroutine ckchar(hl,jt,jp)                                               !/*kk          2*/
!        check that all the characters in hl are printable.                     !/*kk          3*/
!        it does not catch bad characters beyond tilde (>126).                  !/*kk          4*/
!        jt = tty output unit number.   jp = print file unit number.            !/*kk          5*/
      character hl*80,hc*3                                                      !/*kk          6*/
!                                                                               !/*kk          7*/
      do 10 i=1,80                                                              !/*kk          8*/
   10 if(llt(hl(i:i),' '))go to 20                                              !/*kk          9*/
      return                                                                    !/*kk         10*/
   20 write(jt,'(1x,a69)')hl                                                    !/*kk         11*/
      if(jp.ne.0)write(jp,'(1x,a80)')hl                                         !/*kk         12*/
      write(hc,'(i3)')i                                                         !/*kk         13*/
      call expire(0,'ckchar',                                                   !/*kk         14*/
     & 'bad character, probably a control character, in column'//hc)            !/*kk         15*/
      end                                                                       !/*kk         16*/

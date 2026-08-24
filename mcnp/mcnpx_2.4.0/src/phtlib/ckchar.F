!     $Id: ckchar.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                       !/*ck          1*/
      subroutine ckchar (hl,jt,jp)                                              !/*ck          2*/
!        check that all the characters in hl are printable.                     !/*ck          3*/
!        it does not catch bad characters beyond tilde (>126).                  !/*ck          4*/
!        jt = tty output unit number.   jp = print file unit number.            !/*ck          5*/
#include <htape3x/com32b.h>                                                     /*!ck          6*/
      character hl*80, hc*3                                                     !/*ck          7*/
!                                                                               !/*ck          8*/
      do 10 i=1,80                                                              !/*ck          9*/
   10 if (llt(hl(i:i),' ')) go to 20                                            !/*ck         10*/
      return                                                                    !/*ck         11*/
   20 write (jt,'(1x,a69)') hl                                                  !/*ck         12*/
      if (jp.ne.0) write (jp,'(1x,a80)') hl                                     !/*ck         13*/
      write (hc,'(i3)') i                                                       !/*ck         14*/
      call expire (0,'ckchar',                                                  !/*ck         15*/
     & 'bad character, probably a control character, in column'//hc)            !/*ck         16*/
      end                                                                       !/*ck         17*/

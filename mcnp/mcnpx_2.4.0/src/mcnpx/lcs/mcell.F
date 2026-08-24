!     $Id: mcell.F,v 1.4 2002/03/27 18:15:10 gwm Exp $                        !/*mc          1*/
      function mcell (m)                                                        !/*mc          2*/
      use global_data                                                           !/*mcxc        1*/
!                                                                               !/*mcxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*mcxc        3*/
      implicit integer(kindi) (i-n)                                             !/*mcxc        4*/
!                                                                               !/*mcxc        5*/
      do 10 i=1,mxa                                                             !/*mc          4*/
      if (mat(lmat+i).eq.m) then                                                !/*mc          5*/
        mcell=i                                                                 !/*mc          6*/
        return                                                                  !/*mc          7*/
      endif                                                                     !/*mc          8*/
   10 continue                                                                  !/*mc          9*/
      mcell=0                                                                   !/*mc         10*/
      return                                                                    !/*mc         11*/
      end                                                                       !/*mc         12*/

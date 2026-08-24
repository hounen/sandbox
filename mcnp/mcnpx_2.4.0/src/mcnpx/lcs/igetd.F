!     $Id: igetd.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                        !/*ig          1*/
      function igetd (k)                                                        !/*ig          2*/
      use global_data                                                           !/*igxc        1*/
!                                                                               !/*igxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*igxc        3*/
      implicit integer(kindi) (i-n)                                             !/*igxc        4*/
!                                                                               !/*igxc        5*/
      if (k.eq.0) then                                                          !/*ig          4*/
         igetd=-777777                                                          !/*ig          5*/
      else                                                                      !/*ig          6*/
         igetd=npstc                                                            !/*ig          7*/
      endif                                                                     !/*ig          8*/
      return                                                                    !/*ig          9*/
      end                                                                       !/*ig         10*/

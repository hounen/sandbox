!#ident begone.F from deck be                                                   !/*be          1*/
      subroutine begone(ht,l)                                                   !/*be          2*/
!        flush the one-line print buffer ht and reset l to 1.                   !/*be          3*/
      use global_data                                                           !/*bex         1*/
!                                                                               !/*bex         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*bex         3*/
      implicit integer(kindi) (i-n)                                             !/*bex         4*/
!                                                                               !/*bex         5*/
      character ht*119                                                          !/*be          5*/
!                                                                               !/*be          6*/
      write(iuo,'(a119)')ht                                                     !/*be          7*/
      ht=' '                                                                    !/*be          8*/
      l=1                                                                       !/*be          9*/
      return                                                                    !/*be         10*/
      end                                                                       !/*be         11*/

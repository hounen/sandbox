!#ident tallyx.F from deck tx                                                   !/*tx          1*/
      subroutine tallyx(t,ib)                                                   !/*tx          2*/
!        dummy for user-supplied tallyx subroutine.                             !/*tx          3*/
!        t is the input and output tally score value.                           !/*tx          4*/
!        ib controls scoring.  see the user's manual.                           !/*tx4c        1*/
      use global_data                                                           !/*txx         1*/
!                                                                               !/*txx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*txx         3*/
      implicit integer(kindi) (i-n)                                             !/*txx         4*/
!                                                                               !/*txx         5*/
!                                                                               !/*tx          7*/
!        print a warning the first time this dummy tallyx is called.            !/*tx          8*/
      if(jtlx.eq.0)call errprn(1,jtlx,0,zero,zero,' ',' ',                      !/*tx4b        1*/
     & 'a tallyx subroutine is ordinarily needed with fu cards.')               !/*tx4a        2*/
      return                                                                    !/*tx         20*/
      end                                                                       !/*tx         21*/

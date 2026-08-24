!#ident pttyin.F from deck qi4a                                                 !/*qi4a        1*/
      subroutine pttyin                                                         !/*qi4a        2*/
!        intercept tty interrupts.  i/o and integer divides forbidden.          !/*qi4a        3*/
      use global_data                                                           !/*qix         1*/
      use message_passing                                                       !/*qixk        1*/
!                                                                               !/*qix         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*qix         3*/
      implicit integer(kindi) (i-n)                                             !/*qix         4*/
!                                                                               !/*qix         5*/
      irup=irup+1                                                               !/*qi4a        5*/
      if(irup.eq.3)write(jtty,'(30h next interrupt will kill job.)')            !/*qi4a        6*/
      if(irup.gt.3)go to 10                                                     !/*qi4b        1*/
#ifdef MULTT                                                                    /*!qi4a       10*/
      kbp=1                                                                     !/*qi4a       11*/
#endif                                                                          /*!cpp        83*/
      call ttyint                                                               !/*qi4a       12*/
      return                                                                    !/*qi4a       15*/
   10 continue                                                                  !/*qi4b        2*/
#ifdef MULTP                                                                    /*!qi4b        3*/
      if(ltasks.gt.1.and.iovr.eq.4)call msgcon(3)                               !/*qi4c        1*/
      if(mynum.eq.0)call mterm                                                  !/*qix         6*/
#endif                                                                          /*!cpp        84*/
      stop                                                                      !/*qi4b        6*/
      end                                                                       !/*qi4a       16*/

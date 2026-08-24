!#ident secnd.F from deck se                                                    !/*se          1*/
      subroutine secnd(t)                                                       !/*se          2*/
!        return the time consumed so far in this run, in seconds.               !/*se          3*/
      use global_parameters                                                     !/*sex         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*sex         2*/
      implicit integer(kindi) (i-n)                                             !/*sex         3*/
      real etime,a2(2)                                                          !/*se          7*/
#ifdef PCDOS                                                                    /*!se4a        4*/
      save t0,tl                                                                !/*se4a        5*/
      data t0,tl/100000.,0./                                                    !/*se4a        6*/
#endif                                                                          /*!cpp       563*/
!                                                                               !/*se          8*/
      t=0.                                                                      !/*se          9*/
#ifdef UNICOS                                                                   /*!se4c        1*/
      call second(t)                                                            !/*se4c        2*/
#endif                                                                          /*!cpp       564*/
#if defined(UNIX) || defined(DEC)                                               /*!se4c        3*/
      t=etime(a2)                                                               !/*se         23*/
#endif                                                                          /*!cpp       566*/
#ifdef PCDOS                                                                    /*!se4a        8*/
!        only good if this routine is called at least once a day.               !/*sex         4*/
      call timer(i)                                                             !/*se4a        9*/
      t=.01*i                                                                   !/*se4a       10*/
      if(t0.eq.100000.)t0=t                                                     !/*se4a       11*/
      if(t.lt.tl)t0=t0-86400.                                                   !/*se4a       12*/
      tl=t                                                                      !/*se4a       13*/
      t=t-t0                                                                    !/*se4a       14*/
#endif                                                                          /*!cpp       567*/
      return                                                                    !/*se         24*/
      end                                                                       !/*se         25*/

!#ident ttyint.F from deck tt4a                                                 !/*tt4a        1*/
      subroutine ttyint                                                         !/*tt4a        2*/
!        set up the tty interrupt.                                              !/*tt4a        3*/
#ifdef PCDOS                                                                    /*!ttx         1*/
      use global_data                                                           !/*ttx         2*/
!                                                                               !/*ttx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ttx         4*/
      implicit integer(kindi) (i-n)                                             !/*ttx         5*/
!                                                                               !/*ttx         6*/
#endif                                                                          /*!ttx         7*/
      external pttyin                                                           !/*tt4a        6*/
#ifdef UNICOS                                                                   /*!tt4a       15*/
!ccc     REGISTER and SIGINT must be upper case.                                !/*tt4a       16*/
      call fsigctl('REGISTER','SIGINT',pttyin)                                  !/*tt4a       17*/
#endif                                                                          /*!cpp        76*/
#if defined(UNIX) || defined(DEC)                                               /*!tt4c        1*/
#if !defined(AIX) && !defined(HPUX) && !defined(G77)                            /*!ttx         8*/
      integer*4 l                                                               !/*tt4c        3*/
      integer signal                                                            !/*tt4c        4*/
      l=2                                                                       !/*tt4c        5*/
      k=signal(l,pttyin,-1)                                                     !/*tt4c        6*/
#endif                                                                          /*!cpp        78*/
#ifdef AIX                                                                      /*!tt4a       22*/
      call signal(2,pttyin)                                                     !/*tt4a       23*/
#endif                                                                          /*!cpp        79*/
#ifdef HPUX                                                                     /*!tt4a       24*/
#ifndef G77                                                                     /*!ttxa        1*/
      on controlc call pttyin                                                   !/*ttxa        2*/
#endif                                                                          /*!ttxa        3*/
#endif                                                                          /*!cpp        81*/
#if defined(LINUX) && defined(G77)                                              /*!ttx         9*/
      integer lsignal                                                           !/*ttx        10*/
      k=lsignal(2,pttyin)                                                       !/*ttx        11*/
#endif                                                                          /*!ttx        12*/
#endif                                                                          /*!cpp        77*/
#ifdef PCDOS                                                                    /*!tt4c2       1*/
      call break(lockl)                                                         !/*tt4c        8*/
#endif                                                                          /*!cpp        82*/
      return                                                                    !/*tt4a       29*/
      end                                                                       !/*tt4a       30*/

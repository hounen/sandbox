!#ident nsf.F from deck nq4c                                                    !/*nq4c        1*/
      function nsf(n)                                                           !/*nq4c        2*/
!        return problem surface number of lnsf+j=n and facet number kfq.        !/*nq4c        3*/
!        if program surface j is a facet of a macrobody, then return            !/*nq4c        4*/
!        problem surface number of main macrobody and facet number.             !/*nq4c        5*/
!        if j is an ordinary surface, kfq is set to zero.                       !/*nq4c        6*/
      use global_data                                                           !/*nqx         1*/
!                                                                               !/*nqx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*nqx         3*/
      implicit integer(kindi) (i-n)                                             !/*nqx         4*/
!                                                                               !/*nqx         5*/
      kfq=0                                                                     !/*nqx         6*/
      nsf=0                                                                     !/*nqx         7*/
      if(n.le.lnsf)return                                                       !/*nqx         8*/
      nsf=nsfm(n)                                                               !/*nq4c        9*/
      if(nsf.ne.0)return                                                        !/*nq4c       11*/
      j=n-lnsf                                                                  !/*nq4c       12*/
      ka=ksm(lksm+j)                                                            !/*nq4c       13*/
      if(ka.eq.0)return                                                         !/*nqx         9*/
      kfq=j-ka                                                                  !/*nq4c       14*/
      nsf=nsfm(lnsf+ka)                                                         !/*nq4c       15*/
      return                                                                    !/*nq4c       16*/
      end                                                                       !/*nq4c       17*/

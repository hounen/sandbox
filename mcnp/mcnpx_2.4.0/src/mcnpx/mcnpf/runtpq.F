!#ident runtpq.F from deck rq                                                   !/*rq          1*/
      subroutine runtpq                                                         !/*rq          2*/
!        limit runtpe size by overwriting every ndmp dumps.                     !/*rq          3*/
      use global_data                                                           !/*rqx         1*/
!                                                                               !/*rqx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rqx         3*/
      implicit integer(kindi) (i-n)                                             !/*rqx         4*/
!                                                                               !/*rqx         5*/
      if(knod.le.ndmp)return                                                    !/*rq          5*/
!                                                                               !/*rq          6*/
!        reposition runtpe.                                                     !/*rq          7*/
      rewind iur                                                                !/*rq          8*/
      do 10 i=1,mxe+4                                                           !/*rq          9*/
   10 read(iur)id                                                               !/*rq4a        1*/
      if(ndmp.eq.1)return                                                       !/*rq         11*/
      kn=knod                                                                   !/*rq         12*/
      nd=ndmp                                                                   !/*rq         13*/
!                                                                               !/*rq         14*/
!        open scratch files.                                                    !/*rq         15*/
      open(iu1,status='scratch',form='unformatted')                             !/*rq         16*/
      open(iu2,status='scratch',form='unformatted')                             !/*rq         17*/
!                                                                               !/*rq         18*/
!        write current dump to scratch file iu1.                                !/*rq         19*/
      call runtpw(iu1)                                                          !/*rq         20*/
!                                                                               !/*rq         21*/
!        write last ndmp-1 dumps to scratch file iu2                            !/*rq         22*/
   20 call runtpr(iur,ie)                                                       !/*rq         23*/
      if(ie.ne.0)go to 30                                                       !/*rq         24*/
      if(kn-knod.lt.nd)call runtpw(iu2)                                         !/*rq         25*/
      if(knod.lt.kn-1)go to 20                                                  !/*rq         26*/
!                                                                               !/*rq         27*/
!        reposition runtpe and scratch files.                                   !/*rq         28*/
   30 rewind(iur)                                                               !/*rq         29*/
      do 40 i=1,mxe+4                                                           !/*rq         30*/
   40 read(iur)id                                                               !/*rq4a        2*/
      rewind iu2                                                                !/*rq         32*/
      rewind iu1                                                                !/*rq         33*/
!                                                                               !/*rq         34*/
!        copy scratch file dumps to runtpe.                                     !/*rq         35*/
   50 call runtpr(iu2,ie)                                                       !/*rq         36*/
      if(ie.ne.0)go to 60                                                       !/*rq         37*/
      call runtpw(iur)                                                          !/*rq         38*/
      if(knod.lt.kn-1)go to 50                                                  !/*rq         39*/
!                                                                               !/*rq         40*/
!        reload present dump to common and close scratch files.                 !/*rq         41*/
   60 call runtpr(iu1,ie)                                                       !/*rq         42*/
      close(iu1)                                                                !/*rq         43*/
      close(iu2)                                                                !/*rq         44*/
      return                                                                    !/*rq         45*/
      end                                                                       !/*rq         46*/

!#ident timint.F from deck ti                                                   !/*ti          1*/
      subroutine timint(m)                                                      !/*ti          3*/
#ifndef MULTT                                                                   /*!tix         1*/
!        turn on (m=1) or turn off (m=2) the time interrupt.                    !/*ti          4*/
      use global_data                                                           !/*tix         2*/
!                                                                               !/*tix         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*tix         4*/
      implicit integer(kindi) (i-n)                                             !/*tix         5*/
!                                                                               !/*tix         6*/
!                                                                               !/*ti          6*/
      end                                                                       !/*ti         24*/
      subroutine ptimin                                                         !/*ti         27*/
!        process time interrupts.                                               !/*ti         28*/
      use global_data                                                           !/*tix         7*/
!                                                                               !/*tix         8*/
      implicit real(kindr) (a-h,o-z)                                            !/*tix         9*/
      implicit integer(kindi) (i-n)                                             !/*tix        10*/
!                                                                               !/*tix        11*/
!                                                                               !/*ti         30*/
!                                                                               !/*ti         35*/
!        quit if a fourth interrupt occurs in the current history.              !/*ti         36*/
      if(kdb.eq.11)call expirx(0,'timint',                                      !/*ti         39*/
     & 'the code appears to be in an infinite loop.')                           !/*ti         40*/
!                                                                               !/*ti         41*/
!        set kdb for run termination if a third interrupt occurs.               !/*ti         42*/
      if(ntii.eq.-nps)kdb=11                                                    !/*ti         43*/
!                                                                               !/*ti         44*/
!        set ntii to mark the first and second interrupts.                      !/*ti         45*/
      if(ntii.eq.nps)ntii=-nps                                                  !/*ti         46*/
      if(ntii.ne.-nps)ntii=nps                                                  !/*ti         47*/
!                                                                               !/*ti         48*/
!        reset the interrupt and return.                                        !/*ti         49*/
   10 call timint(1)                                                            !/*ti         50*/
#else                                                                           /*!tix        12*/
      return                                                                    !/*ti         54*/
#endif                                                                          /*!cpp       245*/
      end                                                                       !/*ti         55*/

!#ident keypro.F from deck vf4c                                                 !/*vf4c        1*/
      integer*4 function keypro(nc,mp,lp)                                       !/*vf4c        3*/
#ifdef QWIN                                                                     /*!vfx         1*/
!        allow processing of keyboard input for interrupts.                     !/*vf4c        4*/
!DEC$ ATTRIBUTES STDCALL, ALIAS: '_keypro@12' :: keypro                         !/*vf4c        5*/
      use dfwin                                                                 !/*vf4c        6*/
      use dflib                                                                 !/*vf4c        7*/
#endif                                                                          /*!vfx         2*/
      keypro=0                                                                  !/*vfx         3*/
#ifdef QWIN                                                                     /*!vfx         4*/
!                                                                               !/*vf4c        9*/
!        check mp for character code 67 (letter c).                             !/*vf4c       10*/
!        check if cntl key was depressed (16th bit nonzero).                    !/*vf4c       11*/
!        only send signal when key is released (lp 32nd bit nonzero).           !/*vf4c       12*/
      if(nc.ge.0.and.mp.eq.67.and.getkeystate(vk_control).lt.0)then             !/*vf4c       13*/
         if(lp.lt.0)i=raiseqq(sig$int)                                          !/*vf4c       14*/
         keypro=1                                                               !/*vf4c       15*/
      endif                                                                     !/*vf4c       16*/
      return                                                                    !/*vf4c       17*/
      end                                                                       !/*vf4c       18*/
      logical*4 function initialsettings()                                      !/*vf4c       19*/
!        initialize the frame window and set its parameters.                    !/*vf4c       20*/
!        this function is called before main execution.                         !/*vf4c       21*/
      use dfwin                                                                 !/*vf4c       22*/
      use dflib                                                                 !/*vf4c       23*/
      external keypro                                                           !/*vf4c       24*/
!DEC$ ATTRIBUTES STDCALL, ALIAS: '_keypro@12' :: keypro                         !/*vf4c       25*/
      type (qwinfo) wi                                                          !/*vf4c       26*/
      k=setexitqq(qwin$exitpersist)                                             !/*vf4c       27*/
!                                                                               !/*vf4c       28*/
!        enable keyboard interrupts on the main execute thread.                 !/*vf4c       29*/
      k=setwindowshookex(wh_keyboard,loc(keypro),0,                             !/*vf4c       30*/
     & getcurrentthreadid())                                                    !/*vf4c       31*/
!                                                                               !/*vf4c       32*/
!        set the frame window to its maximum size.                              !/*vf4c       33*/
      k=getwsizeqq(qwin$framewindow,qwin$sizemax,wi)                            !/*vf4c       34*/
      k=setwsizeqq(qwin$framewindow,wi)                                         !/*vf4c       35*/
      initialsettings=.true.                                                    !/*vf4c       36*/
#endif                                                                          /*!vfx         5*/
      return                                                                    !/*vf4c       37*/
      end                                                                       !/*vf4c       38*/

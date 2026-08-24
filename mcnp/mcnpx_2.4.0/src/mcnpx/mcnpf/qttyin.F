!#ident qttyin.F from deck qj4a                                                 !/*qj4a        1*/
      subroutine qttyin(ia,hm)                                                  !/*qj4a        2*/
!        process tty interrupts if irup=1 flag on.  hm=message.                 !/*qj4a        3*/
!        the allowed interrupts are                                             !/*qj4a        4*/
!          (ctrl-c) status (s) (or nothing) -- return program status.           !/*qj4a        5*/
!          (ctrl-c) mcplot (m) -- call mcplot.  mcrun only.                     !/*qj4a        6*/
!          (ctrl-c) quit (q) -- terminate at history end.  mcrun only.          !/*qj4a        7*/
!          (ctrl-c) kill (k) -- unconditional kill.                             !/*qj4a        8*/
      use global_data                                                           !/*qjx         1*/
      use message_passing                                                       !/*qjxk        1*/
!                                                                               !/*qjx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*qjx         3*/
      implicit integer(kindi) (i-n)                                             !/*qjx         4*/
!                                                                               !/*qjx         5*/
      character ha*60,hm*(*),hg*8,is(4)*6,js(4)*2                               !/*qj4a       10*/
      data is/'status','mcplot','quit','kill'/,js/'is','im','iq','ik'/          !/*qj4a       11*/
!                                                                               !/*qj4a       12*/
!        get the message from the tty.                                          !/*qj4a       13*/
      irup=0                                                                    !/*qj4a       14*/
   10 write(jtty,'(48h **** interrupt.  enter s (status), m (mcplot), ,'        !/*qj4a       20*/
     & //'18hq (quit), k (kill))')                                              !/*qj4a       21*/
      read(itty,'(a8)')hg                                                       !/*qj4a       22*/
   20 call nxtsym(hg,' ',1,i,j,2)                                               !/*qj4a       23*/
      do 30 mi=1,4                                                              !/*qj4a       24*/
   30 if(hg.eq.is(mi)(1:1).or.hg.eq.is(mi).or.hg.eq.js(mi))go to 40             !/*qj4a       25*/
   40 if((mi.eq.2.or.mi.eq.3).and.iovr.ne.4)mi=1                                !/*qj4a       26*/
      go to(50,60,70,80)mi                                                      !/*qj4a       27*/
!                                                                               !/*qj4a       28*/
! >>>>>  mi=1 -- return the status.                                             !/*qj4a       29*/
   50 call secnd(t)                                                             !/*qj4a       30*/
      ha='(7h time =,f9.2,5x,'//hm//')'                                         !/*qj4a       31*/
!     QUESTION: Is the nch(1)+nch(2) left over from only three particle         !/*qjx         6*/
!               types?  Should the sum be over all particle types ???           !/*qjx         7*/
      if(ia.lt.0)write(jtty,ha)t/60.,nps,nch(1)+nch(2)+nch(3)                   !/*qjx         8*/
      if(ia.eq.0)write(jtty,ha)t/60.                                            !/*qjx         9*/
      if(ia.gt.0)write(jtty,ha)t/60.,ia                                         !/*qjx        10*/
      return                                                                    !/*qj4a       35*/
!                                                                               !/*qj4a       36*/
! >>>>>  mi=2 -- call mcplot.                                                   !/*qj4a       37*/
   60 kmplot=1                                                                  !/*qj4a       38*/
      write(jtty,'(32h will call plotter after history,i10)')nps                !/*qj4a       39*/
      return                                                                    !/*qj4a       40*/
!                                                                               !/*qj4a       41*/
! >>>>>  mi=3 -- terminate run after this history.                              !/*qj4a       42*/
   70 nst=nst+16                                                                !/*qj4a       43*/
      write(jtty,'(24h will quit after history,i10)')nps                        !/*qj4a       44*/
      return                                                                    !/*qj4a       45*/
!                                                                               !/*qj4a       46*/
! >>>>>  mi=4 -- unconditional kill.                                            !/*qj4a       47*/
   80 continue                                                                  !/*qj4a       48*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!qj4a       49*/
!        terminate graphics.                                                    !/*qjx        11*/
      call gxquit                                                               !/*qj4a       50*/
#endif                                                                          /*!cpp        85*/
#ifdef MULTP                                                                    /*!qj4b        1*/
      if(ltasks.gt.1.and.iovr.eq.4)call msgcon(3)                               !/*qj4c        1*/
      if(mynum.eq.0)call mterm                                                  !/*qjx        12*/
#endif                                                                          /*!cpp        86*/
      stop                                                                      !/*qj4a       53*/
      end                                                                       !/*qj4a       54*/

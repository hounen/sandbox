!#ident expire.F from deck ex                                                   !/*ex          1*/
      subroutine expire(mm,hs,hm)                                               !/*ex          2*/
!        print a fatal error message and quit.                                  !/*ex          3*/
!        mm:  1=print history identification if in mcrun   0=don't              !/*ex          4*/
!        hs = the name of the calling subroutine.                               !/*ex          5*/
!        hm = error message.                                                    !/*ex          6*/
!                                                                               !/*ex          7*/
!        tty messages on some systems are limited to 70 characters.             !/*ex          8*/
!        so expire error messages should have only 69 characters.               !/*ex          9*/
!                                                                               !/*ex         10*/
      use global_data                                                           !/*exx         1*/
      use message_passing                                                       !/*exxk        1*/
!                                                                               !/*exx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*exx         3*/
      implicit integer(kindi) (i-n)                                             !/*exx         4*/
!                                                                               !/*exx         5*/
      character hs*(*),hm*(*),hd(4)*70                                          !/*ex         12*/
      data hd/' bad trouble in          in routine',                            !/*exx         6*/
     & ' source particle no.',' starting random number =',' '/                  !/*ex         14*/
!                                                                               !/*ex         15*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!ex4a        1*/
!        terminate graphics.                                                    !/*exx         7*/
      call gxquit                                                               !/*ex         19*/
#endif                                                                          /*!cpp        99*/
!                                                                               !/*ex         20*/
!        set up the names of the section and routine.                           !/*exx         8*/
      hd(1)(17:24)=hovr                                                         !/*exx         9*/
      hd(1)(37:67)=hs                                                           !/*exx        10*/
!                                                                               !/*ex         24*/
!        set up history identification if in mcrun.                             !/*ex         25*/
      kd=3                                                                      !/*ex         26*/
      if(mm.eq.0)go to 10                                                       !/*ex         27*/
      kd=1                                                                      !/*ex         28*/
      if(kdbnps.eq.0)write(hd(2)(21:30),'(i10)')nps                             !/*ex         29*/
      if(kdbnps.ne.0)write(hd(2)(21:30),'(i10)')kdbnps                          !/*ex         30*/
      if(kdbnps.eq.0)write(hd(3)(26:42),'(f17.0)')rijk                          !/*ex         31*/
      hd(3)(42:42)=' '                                                          !/*ex         32*/
!                                                                               !/*ex         33*/
!        set up the specific error message, hm.                                 !/*ex         34*/
   10 hd(4)(2:70)=hm                                                            !/*ex         35*/
!                                                                               !/*ex         36*/
!        print the error story.                                                 !/*ex         37*/
      write(jtty,20)(hd(j),j=1,4,kd)                                            !/*ex         38*/
      if(iuou.ne.0)write(iuo,20)(hd(j),j=1,4,kd)                                !/*ex         39*/
   20 format(/a70/a70/a70/a70)                                                  !/*ex         40*/
!                                                                               !/*ex         41*/
!        if required, back up to history beginning.                             !/*ex         42*/
      nst=nst+512                                                               !/*ex         43*/
      if(ntasks.gt.1.or.ltasks.gt.1)go to 30                                    !/*exx        11*/
      if(mm.eq.0.or.nps.lt.50)go to 30                                          !/*ex         45*/
      call backup                                                               !/*ex         46*/
      nps=nps-1                                                                 !/*ex         47*/
      inif=1                                                                    !/*ex         48*/
      call vtask                                                                !/*ex         49*/
      call output                                                               !/*ex         50*/
   30 continue                                                                  !/*ex         51*/
!                                                                               !/*ex         52*/
#ifdef MULTP                                                                    /*!ex4a        2*/
!        stop master task.                                                      !/*exx        12*/
      if(mynum.eq.0)call mterm                                                  !/*exx        13*/
#endif                                                                          /*!cpp       100*/
      stop                                                                      !/*ex         57*/
      end                                                                       !/*ex         58*/

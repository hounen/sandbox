!#ident errprn.F from deck en4a                                                 !/*en4a        1*/
      subroutine errprn(kl,n1,kf,v1,v2,h1,h2,hf)                                !/*en4b        1*/
!        print messages about errors during transport part of code.             !/*en4a        3*/
!        kl=1 set multitasking locks.                                           !/*en4a        4*/
!        n1   increment how many times this error encountered.                  !/*en4a        5*/
!        kf   format flag for variables v1,v2.                                  !/*en4a        7*/
!        v1,v2 variables to print.                                              !/*en4a        8*/
!        h1,h2 names of v1, v2.                                                 !/*en4a        9*/
!        hf   error message (60 characters max).                                !/*en4a       10*/
      use global_data                                                           !/*enx         1*/
!                                                                               !/*enx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*enx         3*/
      implicit integer(kindi) (i-n)                                             !/*enx         4*/
!                                                                               !/*enx         5*/
      character h1*(*),h2*(*),ha*16,hb*26,hc*37,hd*60,hf*(*)                    !/*en4a       12*/
!                                                                               !/*en4a       13*/
!        set multitasking locks.                                                !/*en4a       14*/
      if(kl.eq.0)go to 10                                                       !/*en4a       15*/
#ifdef MULTT                                                                    /*!enxk        1*/
      call thread_lockon(jlock)                                                 !/*enxk        2*/
#endif                                                                          /*!enxk        3*/
!                                                                               !/*en4a       17*/
!        print warning if this is the first time encountered.                   !/*en4a       18*/
   10 if(n1.gt.0)go to 20                                                       !/*en4a       19*/
      hd=hf                                                                     !/*en4a       20*/
      write(ha,'(6h nps =,i10)')npstc                                           !/*en4a       21*/
      write(hb,'(5hnrn =,f17.0,tl1,1h ,4x)')rnr+rnrtc                           !/*en4c2       1*/
      hc=' '                                                                    !/*en4a       23*/
      if(kf.eq.1)write(hc,'(a3,2h =,i5)')h1,nint(v1)                            !/*en4a       24*/
      if(kf.eq.2)write(hc,'(a3,2h =,i5,5x,a3,2h =,i5)')h1,nint(v1),             !/*en4a       25*/
     & h2,nint(v2)                                                              !/*en4a       26*/
      if(kf.eq.3)write(hc,'(a3,2h =,i5,5x,a3,2h =,1pe11.4)')h1,                 !/*en4a       27*/
     & nint(v1),h2,v2                                                           !/*en4a       28*/
      if(kf.eq.4)write(hc,'(a3,2h =,1pe11.4)')h1,v1                             !/*en4a       29*/
      if(kf.eq.5)write(hc,'(a3,2h =,1pe11.4,5x,a3,2h =,e11.4)')h1,v1,           !/*en4a       30*/
     & h2,v2                                                                    !/*en4a       31*/
      write(iuo,'(11h warning.  ,a60/a16,5x,a26,a37)')hd,ha,hb,hc               !/*en4a       32*/
      if(ntasks.le.1.and.ltasks.le.1)                                           !/*en4c        1*/
     & write(jtty,'(10h warning. ,a60/a16,5x,a37)')hd,ha,hc                     !/*en4c        2*/
      if(npp.ge.0)nwer=nwer+1                                                   !/*en4a       34*/
!                                                                               !/*en4a       35*/
!        increment counts and unlock multitasking as required.                  !/*en4a       36*/
   20 if(n1.ge.0)n1=n1+1                                                        !/*en4c2       2*/
      if(kl.eq.0)return                                                         !/*en4a       39*/
#ifdef MULTT                                                                    /*!enxk        4*/
      call thread_lockoff(jlock)                                                !/*enxk        5*/
#endif                                                                          /*!enxk        6*/
      return                                                                    !/*en4a       41*/
      end                                                                       !/*en4a       42*/

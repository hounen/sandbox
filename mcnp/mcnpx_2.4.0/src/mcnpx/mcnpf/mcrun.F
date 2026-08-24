!#ident mcrun.F from deck mr                                                    !/*mr          1*/
      subroutine mcrun                                                          !/*mr          2*/
!        transport the particles and print the results.                         !/*mr          3*/
      use global_data                                                           !/*mrx         1*/
!                                                                               !/*mrx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*mrx         3*/
      implicit integer(kindi) (i-n)                                             !/*mrx         4*/
!                                                                               !/*mrx         5*/
#if defined(MULTT) || defined(MULTP)                                            /*!mrxk        1*/
       real, external :: wclock                                                 !/*mrxk        2*/
#endif                                                                          /*!mrxk        3*/
#ifdef MULTT                                                                    /*!mr          5*/
      external trnspt                                                           !/*mr          6*/
      if(ntasks.gt.1)call thread_number(ntasks)                                 !/*mrxk        4*/
#endif                                                                          /*!cpp       223*/
!                                                                               !/*mr         14*/
!        get the time already consumed so far in this run, in seconds.          !/*mr         15*/
      hovr='mcrun'                                                              !/*mr         16*/
      call secnd(t1)                                                            !/*mrx         7*/
#if defined(MULTT) || defined(MULTP)                                            /*!mr4c        7*/
      w4=wclock()                                                               !/*mr4c        8*/
#endif                                                                          /*!cpp       226*/
!                                                                               !/*mr         19*/
!        print and quit if that is all that is wanted.                          !/*mr         20*/
      if(npp.ge.0)go to 10                                                      !/*mr         21*/
      mpc=1                                                                     !/*mr         22*/
      call output                                                               !/*mr         23*/
      return                                                                    !/*mr         24*/
!                                                                               !/*mr         25*/
!        print mcrun startup messages.                                          !/*mr         26*/
   10 write(jtty,20)lfll,lfll*8,t1/60.                                          !/*mrx         8*/
   20 format(18h dynamic storage =,i10,7h words,,i11,7h bytes.,6x,              !/*mr4b        3*/
     & 5hcp0 =,f6.2)                                                            !/*mr4b        4*/
      if(ink(110).ne.0.and.mct.ge.0)write(iuo,30)lfll,lfll*8,t1/60.,aid         !/*mrx         9*/
      if(ink(110).ne.0.and.mct.lt.0)write(iuo,30)0,0,0.,aid                     !/*mr4b        7*/
   30 format(1h1,3x,15hstarting mcrun.,6x,18h dynamic storage =,i10,            !/*mr4b        8*/
     & 7h words,,i11,7h bytes.,6x,5hcp0 =,f6.2,9x,15hprint table 110//          !/*mr4b        9*/
     & 6x,a80)                                                                  !/*mr4b       10*/
      if(konrun*ink(110).ne.0.and.aid1(1:8).eq.'continue')                      !/*mr         33*/
     & write(iuo,'(6x,a80)')aid1                                                !/*mr         34*/
!                                                                               !/*mr         35*/
!        print a warning if this run is in spite of fatal errors.               !/*mr         36*/
      if(nfer.ne.0)write(jtty,40)nfer                                           !/*mr         37*/
      if(nfer.ne.0)write(iuo,40)nfer                                            !/*mr         38*/
   40 format(/1x,69(1h*)//4x,26hsince the fatal option was,                     !/*mr4a        5*/
     & 25h set on the execute line,/4x,25hthis run is proceeding in,            !/*mr         40*/
     & 9h spite of,i4,24h otherwise fatal errors.//1x,69(1h*)/)                 !/*mr         41*/
!                                                                               !/*mr         42*/
!        mark any dr tables that have identical energy group structures.        !/*mr         43*/
      do 50 ie=1,mxe                                                            !/*mr         44*/
   50 nxs(lnxs+16,ie)=ie                                                        !/*mr         45*/
      do 80 ie=1,mxe                                                            !/*mr         46*/
      if(nty(lnty+ie).ne.2.or.nxs(lnxs+16,ie).gt.1000)go to 80                  !/*mr         47*/
      nxs(lnxs+16,ie)=1000+ie                                                   !/*mr         48*/
      do 70 i=ie+1,mxe                                                          !/*mr         49*/
      if(nty(lnty+i).ne.2.or.nxs(lnxs+16,i).gt.1000.or.                         !/*mr         50*/
     & nxs(lnxs+3,i).ne.nxs(lnxs+3,ie))go to 70                                 !/*mr         51*/
      do 60 j=1,nxs(lnxs+3,ie)                                                  !/*mr         52*/
   60 if(xss(jxs(ljxs+1,i)+j-1).ne.xss(jxs(ljxs+1,ie)+j-1))go to 70             !/*mr         53*/
      nxs(lnxs+16,i)=nxs(lnxs+16,ie)                                            !/*mr         54*/
   70 continue                                                                  !/*mr         55*/
   80 continue                                                                  !/*mr         56*/
!                                                                               !/*mr         57*/
!        initialize cross-section indexes.                                      !/*mr         61*/
      do 90 k=1,mtasks                                                          !/*mr         62*/
      do 90 i=1,mxe                                                             !/*mr         63*/
      ktc(lktc+2*mxe1*(k-1)+1,i)=1                                              !/*mr         64*/
      do 90 j=1,15                                                              !/*mr4c        9*/
   90 rtc(lrtc+15*mxe1*(k-1)+j,i)=0.                                            !/*mr4c       10*/
      allocate (gbnk(mbnk*mtasks), stat=is)                                     !/*mrx        10*/
      if(is.ne.0)call expire(0,'mcrun','error getting memory')                  !/*mrx        11*/
      gbnk=0.                                                                   !/*mrx        12*/
!        variable iunr may be changed in getxst before runtpe written.          !/*mrx        13*/
      allocate (ibnk(0:(nbmx*(lpblcm+2*abs(iunr))+1)*mtasks), stat=is)          !/*mrx        14*/
      if(is.ne.0)call expire(0,'mcrun','error getting memory')                  !/*mrx        15*/
      ibnk=0                                                                    !/*mrx        16*/
#ifdef MULTP                                                                    /*!mr4c       11*/
      if(ltasks.gt.1)call msgcon(1)                                             !/*mr4c       12*/
#endif                                                                          /*!cpp       227*/
!                                                                               !/*mr         70*/
!        run the particle histories and produce output.                         !/*mr         71*/
  100 itask=0                                                                   !/*mr         72*/
      kbp=0                                                                     !/*mr         73*/
      call secnd(t1)                                                            !/*mrx        17*/
      cp3=cp3+t1                                                                !/*mrx        18*/
#if defined(MULTT) || defined(MULTP)                                            /*!mr4c       13*/
      w2=wclock()                                                               !/*mr4c       14*/
#endif                                                                          /*!cpp       228*/
      if(ltasks.le.1) then                                                      !/*mrxk        5*/
        if(ntasks.le.1) then                                                    !/*mrxk        6*/
          call trnspt                                                           !/*mrxk        7*/
        else                                                                    !/*mrxk        8*/
#ifdef MULTT                                                                    /*!mrxk        9*/
          call thread_routine(trnspt)                                           !/*mrxk       10*/
          pptme(1)=pptme(1)+thread_ltime()                                      !/*mrxk       11*/
          pptme(2)=pptme(2)+wclock()-w2                                         !/*mrxk       12*/
          jlock=0                                                               !/*mrxk       13*/
#endif                                                                          /*!mrxk       14*/
        endif                                                                   !/*mrxk       15*/
      endif                                                                     !/*mrxk       16*/
#ifdef MULTP                                                                    /*!mr4c       22*/
!        determine subtask workload                                             !/*mrx        19*/
      if(ltasks.gt.1)call msgcon(2)                                             !/*mr4c       23*/
#endif                                                                          /*!cpp       232*/
#if defined(MULTT) || defined(MULTP)                                            /*!mr4c       24*/
      pptme(3)=pptme(3)+wclock()-w2                                             !/*mr4c       25*/
      pptme(4)=wclock()-w4                                                      !/*mr4c       26*/
#endif                                                                          /*!cpp       233*/
      call secnd(t1)                                                            !/*mrx        20*/
      cp3=cp3-t1                                                                !/*mrx        21*/
      call output                                                               !/*mr        108*/
      if(nst.eq.0)go to 100                                                     !/*mr        109*/
#ifdef HISTP                                                                    /*!mrx        22*/
      if(ihistp.ne.0) then                                                      !/*mrx        23*/
        if(mcal.eq.0)call end_histp(nps)                                        !/*mrx        24*/
      endif                                                                     !/*mrx        25*/
#endif                                                                          /*!mrx        26*/
      close(iur)                                                                !/*mr        110*/
!                                                                               !/*mr        111*/
!        print the number of restarts, if any.                                  !/*mr        112*/
      if(nrc.ne.0)write(iuo,150)nrc                                             !/*mr        113*/
  150 format(/i10,35h restarts occurred during this run.)                       !/*mr        114*/
!                                                                               !/*mr        115*/
#ifdef MULTP                                                                    /*!mr4a       14*/
!        kill subtasks                                                          !/*mrx        27*/
      if(ltasks.gt.1)call msgcon(3)                                             !/*mr4c       27*/
#endif                                                                          /*!cpp       234*/
#ifdef CEM                                                                      /*!mrx        28*/
!        print CEM reaction statistics if required.                             !/*mrx        29*/
!        (LCAOPT(9) is the ICEM flag.)                                          !/*mrx        30*/
      if(lcaopt(9).ne.0.and.ink(46).ne.0)call cemstat(iuo)                      !/*mrx        31*/
#endif                                                                          /*!mrx        32*/
      deallocate (gbnk)                                                         !/*mrx        33*/
      deallocate (ibnk)                                                         !/*mrx        34*/
      return                                                                    !/*mr        118*/
      end                                                                       !/*mr        119*/

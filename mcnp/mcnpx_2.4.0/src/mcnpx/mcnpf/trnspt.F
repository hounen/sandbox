!#ident trnspt.F from deck tn                                                   !/*tn          1*/
      subroutine trnspt                                                         !/*tn          3*/
!        run particle histories until output is necessary.                      !/*tn          6*/
      use global_data                                                           !/*tnx         1*/
!                                                                               !/*tnx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tnx         3*/
      implicit integer(kindi) (i-n)                                             !/*tnx         4*/
!                                                                               !/*tnx         5*/
!                                                                               !/*tn         12*/
      call utask                                                                !/*tn         13*/
      call secnd(t1)                                                            !/*tnx         6*/
      cp2(ktask+1)=t1                                                           !/*tnx         7*/
#ifdef MULTT                                                                    /*!tnxk        1*/
      call thread_lockon(jlock)                                                 !/*tnxk        2*/
#endif                                                                          /*!tnxk        3*/
      if(kbp.gt.0)go to 120                                                     !/*tn4b        1*/
!                                                                               !/*tn4a        1*/
!        update to next kcode cycle.                                            !/*tn4a        2*/
      if(nsr.eq.71.and.kcy.eq.kcz)kcy=kcy+1                                     !/*tn4a        3*/
!                                                                               !/*tn         17*/
!        quit if enough particles have been started from the source.            !/*tn         18*/
   10 if(nsr.eq.6.and.nrrs.ge.nrss)nst=nst+1024                                 !/*tn         19*/
      if(nps.ge.npp.and.npp.ne.0)nst=nst+4                                      !/*tn         20*/
!                                                                               !/*tn         21*/
!        quit if too many particles are lost.                                   !/*tn         22*/
      if(nerr.ge.lost(1))nst=nst+8                                              !/*tn         23*/
!                                                                               !/*tn         24*/
!        set indicators if any output is required.                              !/*tn         25*/
      jtfc=0                                                                    !/*tn         26*/
      mpc=0                                                                     !/*tn         27*/
      mdc=0                                                                     !/*tn         28*/
      mrm=0                                                                     !/*tn         29*/
      n=nps                                                                     !/*tn         30*/
#ifdef MULTP                                                                    /*!tn4c        2*/
      if(mynum.gt.0.and.kbp.lt.0)go to 30                                       !/*tnx         9*/
#endif                                                                          /*!cpp       235*/
      if(n.eq.npsout)go to 30                                                   !/*tn         31*/
      if(ntal.gt.0.and.mod(n,npd).eq.0)jtfc=1                                   !/*tnx        12*/
      if(nsr.ne.71)go to 20                                                     !/*tn         33*/
!                                                                               !/*tn4c        5*/
!        set flags if kcode cycle done.                                         !/*tn4c        6*/
      if(nsa.ne.0)go to 30                                                      !/*tn         34*/
      i=0                                                                       !/*tn4c        7*/
      if(kcy.ge.kct.and.kct.ne.0)i=1                                            !/*tn4c        8*/
      if(i.ne.0)nst=nst+128                                                     !/*tn4c       10*/
      if(prn.lt.0..and.cp1-tlc.ge.-prn*60.)mpc=1                                !/*tn         36*/
      if(dmp.lt.0..and.cp1-tdc.ge.-dmp*60.)mdc=1                                !/*tn         37*/
      if(freq.lt.0..and.cp1-trm.ge.-freq*60.)mrm=1                              !/*tn         38*/
      n=kcy                                                                     !/*tn         39*/
!                                                                               !/*tn4c       15*/
!        set output flags at specified histories.                               !/*tn4c       16*/
   20 if(prn.gt.0..and.mod(n,max(1,int(prn))).eq.0)mpc=1                        !/*tn         40*/
      if(dmp.gt.0..and.mod(n,max(1,int(dmp))).eq.0)mdc=1                        !/*tn         41*/
      if(kmplot.ne.0.or.krtm.ne.0.and.freq.gt.0..and.                           !/*tn         42*/
     & mod(n,max(1,int(freq))).eq.0)mrm=1                                       !/*tn         43*/
!                                                                               !/*tn         44*/
!        check the time once a second and for output.                           !/*tn         45*/
   30 call secnd(t2)                                                            !/*tn4c       17*/
      if(nps.lt.ntc1.and.nst+jtfc+mpc+mdc+mrm.eq.0.and.                         !/*tnx        18*/
     & (nsr.ne.71.or.nsa.ne.0).and.                                             !/*tnx        19*/
#ifdef RADIOG                                                                   /*!tnx        20*/
     & (nps.ne.npsmg.or.npsmg.eq.0.or.npsout.eq.npsmg).and.                     !/*tnx        21*/
#endif                                                                          /*!tnx        22*/
     & (nps.ne.200.or.npsout.eq.200))go to 80                                   !/*tnx        23*/
      if(t2.ge.cp2(ktask+1)-1.)go to 50                                         !/*tn4c       20*/
      write(jtty,40)kod,cp2(ktask+1)/60.,t2/60.                                 !/*tn4c       21*/
      write(iuo,40)kod,cp2(ktask+1)/60.,t2/60.                                  !/*tn4c       22*/
   40 format(11h restarted ,a8,8x,10hold time =,f7.2,4x,                        !/*tn4c       23*/
     & 10hnew time =,f7.2)                                                      !/*tn4c       24*/
      nrc=nrc+1                                                                 !/*tn4c       25*/
      go to 60                                                                  !/*tn4c       26*/
   50 cp1=cp1+(t2-cp2(ktask+1))/ntasks                                          !/*tn4c       27*/
      cp3=cp3+t2-cp2(ktask+1)                                                   !/*tn4c       28*/
      cts=cts+t2-cp2(ktask+1)                                                   !/*tn4c       29*/
   60 cp2(ktask+1)=t2                                                           !/*tn4c       30*/
      cpv=t2                                                                    !/*tn         62*/
      ntc=max(one+.8*ntc,min(zero+1.2*ntc,nps/max(one,cts)))                    !/*tn4a       10*/
      ntc1=nps+ntc                                                              !/*tn4a       11*/
#ifdef MULTP                                                                    /*!tnx        24*/
      if(mynum.gt.0.and.kbp.lt.0)go to 75                                       !/*tnx        25*/
#endif                                                                          /*!tnx        26*/
      if(nsa.ne.0)go to 70                                                      !/*tn         66*/
      if(prn.lt.0..and.cp1-tlc.ge.-prn*60.)mpc=1                                !/*tn         67*/
      if(dmp.lt.0..and.cp1-tdc.ge.-dmp*60.)mdc=1                                !/*tn         68*/
      if(krtm.ne.0.and.freq.lt.0..and.cp1-trm.ge.-freq*60.)mrm=1                !/*tn         69*/
!                                                                               !/*tn         70*/
!        quit if enough time has been used in this run.                         !/*tn         71*/
   70 if(ctme.ne.0..and.cp3.ge.ctme*60.)nst=nst+32                              !/*tn4c       31*/
!                                                                               !/*tn4a       13*/
#ifndef MULTP                                                                   /*!tn4b        6*/
!        provide this in subroutine msgcon                                      !/*tnx        27*/
      if(krtm.ne.0.and.nst.ne.0)mrm=1                                           !/*tn4a       14*/
#endif                                                                          /*!cpp       236*/
!                                                                               !/*tn         89*/
!        run the next history if output is not required.                        !/*tn         90*/
   75 if(nsr.eq.71.and.nsa.eq.0.or.jtfc+mpc+mdc+mrm+nst.ne.0.or.                !/*tn4a       15*/
     & nps.eq.200.and.npsout.ne.200)go to 120                                   !/*tn         92*/
#ifdef RADIOG                                                                   /*!tnx        28*/
      if(nps.eq.npsmg.and.npsmg.ne.0.and.npsout.ne.npsmg)go to 120              !/*tnx        29*/
#endif                                                                          /*!tnx        30*/
!                                                                               !/*tn         95*/
!        advance the starting random number.                                    !/*tn         96*/
   80 if(inif.ne.0)go to 90                                                     !/*tn         97*/
      call advijk                                                               !/*tn         98*/
   90 inif=0                                                                    !/*tn         99*/
      ranjtc=ranj                                                               !/*tn        100*/
      ranitc=rani                                                               !/*tn        101*/
      rijktc=rijk                                                               !/*tn        102*/
!                                                                               !/*tn        103*/
!        run the next history.                                                  !/*tn        104*/
  100 nps=nps+1                                                                 !/*tn        105*/
      npstc=nps                                                                 !/*tn        106*/
      if(ltasks.le.1)nbal(ktask+1)=nbal(ktask+1)+1                              !/*tn4c       32*/
      if(ltasks.gt.1)nbal(mynum+1)=nbal(mynum+1)+1                              !/*tn4c       33*/
      if(nsr.eq.6.or.nsr.eq.71)go to 110                                        !/*tn        108*/
!        lock is left on for surface source or kcode source.                    !/*tnx        31*/
#ifdef MULTT                                                                    /*!tnxk        4*/
      call thread_lockoff(jlock)                                                !/*tnxk        5*/
#endif                                                                          /*!tnxk        6*/
  110 call hstory                                                               !/*tn        110*/
#ifdef MULTT                                                                    /*!tnxk        7*/
      call thread_lockon(jlock)                                                 !/*tnxk        8*/
#endif                                                                          /*!tnxk        9*/
      if(kbp.le.0.and.kdb.ge.0)go to 10                                         !/*tn4b        8*/
!                                                                               !/*tn        115*/
!        return to do output.                                                   !/*tn        116*/
  120 kbp=1                                                                     !/*tn        117*/
      call secnd(t2)                                                            !/*tnx        34*/
      cp1=cp1+(t2-cp2(ktask+1))/ntasks                                          !/*tnx        35*/
      cts=cts+t2-cp2(ktask+1)                                                   !/*tnx        36*/
      call vtask                                                                !/*tn        118*/
      call secnd(t2)                                                            !/*tnx        37*/
      cp3=cp3+t2-cp2(ktask+1)                                                   !/*tnx        38*/
      cp2(ktask+1)=t2                                                           !/*tnx        39*/
#ifdef MULTT                                                                    /*!tnxk       10*/
      call thread_lockoff(jlock)                                                !/*tnxk       11*/
#endif                                                                          /*!tnxk       12*/
      return                                                                    !/*tn        120*/
      end                                                                       !/*tn        121*/

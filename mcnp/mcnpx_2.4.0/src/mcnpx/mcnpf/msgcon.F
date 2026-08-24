!#ident msgcon.F from deck me4c                                                 !/*me4c        1*/
      subroutine msgcon(mm)                                                     !/*me4c        3*/
#ifdef MULTP                                                                    /*!mex         1*/
!        handle all distributed memory multiprocessing communication.           !/*me4c        4*/
!        called only by master task.                                            !/*me4c        5*/
      use global_data                                                           !/*mex         2*/
      use message_passing                                                       !/*mexk        1*/
      use histp                                                                 !/*mexk        2*/
!                                                                               !/*mex         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*mex         4*/
      implicit integer(kindi) (i-n)                                             !/*mex         5*/
!                                                                               !/*mex         6*/
      parameter (dt=120.d0,mf=5,mn=30*mcpu)                                     !/*me4c        7*/
      save nd,np,ns                                                             !/*me4c        8*/
      dimension ib(0:mcpu),np(2,6),rg(11),nc(0:mn,4)                            !/*me4c        9*/
      character hs*31,hm*69,hf*130,hd*19                                        !/*mex         7*/
      data nd/11/,np/12*0/,lf/1/                                                !/*me4c2       1*/
!                                                                               !/*me4c       12*/
      go to(10,150,500)mm                                                       !/*me4c       13*/
!                                                                               !/*me4c       14*/
!***********************************************************************        !/*me4c       15*/
! >>>>>  mm = 1 -- initialize multiprocessing.                                  !/*me4c       16*/
!                                                                               !/*me4c       17*/
!        master task -- initiate ltasks subtasks if ltasks < 10;                !/*me4c       18*/
!        otherwise initiate ltasks-1 subtasks.                                  !/*me4c       19*/
   10 k=1                                                                       !/*me4c       20*/
      if(ltasks.lt.10.or.jtasks.lt.0)k=0                                        !/*me4c       21*/
      ltasks=ltasks-k                                                           !/*me4c       22*/
      call getidt(hd)                                                           !/*me4c       23*/
      write(jtty,'(15hmaster starting,i8,3h by,i8,10h subtasks ,a19)')          !/*me4c       24*/
     & ltasks,ntasks,hd                                                         !/*me4c       25*/
      if(mct.ge.0)write(iuo,'(/16h master starting,i8,3h by,i8,                 !/*mex         8*/
     & 10h subtasks ,a19)')ltasks,ntasks,hd                                     !/*mex         9*/
      call minit(ltasks,k)                                                      !/*me4c       28*/
      if(k.lt.ltasks)call erprnt(1,2,2,max(1,k),ltasks,0,0,0,                   !/*me4c       29*/
     & '23h could only initialize ,i3,4h of ,i3,17h requested tasks.')          !/*me4c       30*/
      ltasks=max(1,k)                                                           !/*me4c       31*/
      if(ltasks.eq.1)return                                                     !/*me4c       32*/
      do 20 i=1,ltasks+1                                                        !/*me4c       33*/
   20 nbal(i)=0                                                                 !/*me4c       34*/
!                                                                               !/*me4c       35*/
!        initialize all possible rendezvous points.                             !/*me4c       36*/
      if(nsr.eq.71)go to 30                                                     !/*me4c       37*/
      if(prn.gt.0.)np(1,1)=int(prn)                                             !/*me4c       38*/
      if(dmp.gt.0.)np(1,2)=int(dmp)                                             !/*me4c       39*/
      if(krtm.ne.0.and.freq.gt.0.)np(1,3)=int(freq)                             !/*me4c       40*/
   30 np(1,5)=200                                                               !/*me4c       41*/
      np(1,6)=npp                                                               !/*me4c       42*/
      if(nsr.eq.6)np(1,6)=nps+nint(min(one,1.1*snit)*(niss-nqss))               !/*me4c       43*/
      if(nsr.eq.71)np(1,6)=nps+nsa                                              !/*me4c       44*/
      do 40 i=1,6                                                               !/*me4c       45*/
   40 np(2,i)=np(1,i)                                                           !/*me4c       46*/
!                                                                               !/*me4c       47*/
!        initialize npd:                                                        !/*me4c       48*/
!        - if prdmp(5)<0  use default value (1000);                             !/*me4c       49*/
!        - if prdmp(5)=0  1/10th of job unless ddg(1,*)>0;                      !/*me4c       50*/
!        - if prdmp(5)>0  set to prdmp(5).                                      !/*me4c       51*/
      ns=0                                                                      !/*me4c       52*/
      if(inpd.lt.0.or.(konrun.ne.0.and.inpd.eq.0))go to 80                      !/*me4c       53*/
      do 50 i=1,ndtt                                                            !/*me4c       54*/
   50 if(ddg(1,i).gt.0)go to 70                                                 !/*me4c       55*/
      do 60 i=1,nipt                                                            !/*mex        10*/
      do 60 j=1,ndx(i)                                                          !/*me4c       57*/
   60 if(ddx(i,1,j).gt.0)go to 70                                               !/*me4c       58*/
      if(inpd.gt.0)go to 80                                                     !/*me4c       59*/
      n=max(0,npp-nps)                                                          !/*me4c       60*/
      if(nsr.eq.6)n=nint(min(one,snit)*(niss-nqss))                             !/*me4c       61*/
      if(nsr.eq.71)n=nsrck*(kct-kcy)                                            !/*me4c       62*/
      if(npp.eq.0.and.nsr.ne.6.and.nsr.ne.71)ns=1                               !/*me4c       63*/
      npd=1000*max(1,nint(one*n/10000.))                                        !/*me4c       64*/
   70 if(inpd.gt.0)call erprnt(1,2,1,npd,0,0,0,0,                               !/*me4c       65*/
     & '36hdetector roulette data updated every,i10,11h particles.')            !/*me4c       66*/
!                                                                               !/*me4c       67*/
!        send common block data to subtasks.                                    !/*me4c       68*/
   80 write(jtty,'(32hmaster sending static commons...)')                       !/*me4c       69*/
      call msndi                                                                !/*me4c       70*/
      call mputd(gfixcm,nfixcm,i)                                               !/*me4c       71*/
      call mputi(jfixcm,lfixcm,i)                                               !/*me4c       72*/
      call mputd(gvarcm,nvarcm,i)                                               !/*me4c       73*/
      call mputi(jvarcm,lvarcm,i)                                               !/*me4c       74*/
      call mputd(gephcm,nephcm,i)                                               !/*me4c       75*/
      call mputi(jephcm,lephcm,i)                                               !/*me4c       76*/
      call msend(-1,4,i)                                                        !/*me4c       77*/
!                                                                               !/*me4c       80*/
!        send dynamic common data to subtasks.                                  !/*me4c       81*/
      write(jtty,'(33hmaster sending dynamic commons...)')                      !/*me4c       82*/
      call msndi                                                                !/*me4c       83*/
      call fdac_pack                                                            !/*mex        11*/
      call vdac_pack                                                            !/*mex        12*/
      call msend(-1,4,i)                                                        !/*mex        13*/
#ifdef MESHTAL                                                                  /*!mexk        3*/
!                                                                               !/*mexk        4*/
!        send mesh tally data.                                                  !/*mexk        5*/
      if(nmesh.gt.0) call gdtal(16,zero,zc,jz)                                  !/*mexk        6*/
#endif                                                                          /*!mexk        7*/
!                                                                               !/*me4c      107*/
!        send cross section data.                                               !/*me4c      108*/
      if(mxe.eq.0)go to 90                                                      !/*mexk        8*/
      write(jtty,'(36hmaster sending cross section data...)')                   !/*me4c      110*/
      call msndi                                                                !/*me4c      111*/
#ifndef XS64                                                                    /*!me4c      113*/
      call mputr(xss(lxss+1),lxs,i)                                             !/*me4c      114*/
#else                                                                           /*!mex        14*/
      call mputd(xss(lxss+1),lxs,i)                                             !/*me4c      116*/
#endif                                                                          /*!mex        15*/
      call msend(-1,4,i)                                                        !/*me4c      117*/
!                                                                               !/*me4c      118*/
!        set dimension of ssw data (nd), default = 11.                          !/*me4c2       2*/
   90 if(nsph.eq.1)nd=6                                                         !/*me4c      120*/
!                                                                               !/*me4c      121*/
!        send rssa file to subtasks.  send in packets of nr records.            !/*me4c      122*/
      if(nsr.ne.6)go to 110                                                     !/*me4c      123*/
      write(jtty,'(27hmaster sending rssa file...)')                            !/*me4c      124*/
      m=0                                                                       !/*me4c      125*/
      call msndi                                                                !/*me4c      126*/
      mr=(lchnk+nrcd-1)/nrcd                                                    !/*me4c      127*/
      call mputd(ssb,nrcd,j)                                                    !/*me4c      128*/
      do 100 n=2,nrss-nrrs                                                      !/*me4c      129*/
      read(iusr,end=600)(ssb(i),i=1,nrcd)                                       !/*me4c      130*/
      call mputd(ssb,nrcd,j)                                                    !/*me4c      131*/
      if(mod(n,mr).ne.0.or.n.ge.nrss-nrrs)go to 100                             !/*me4c      132*/
      call msend(-1,50+m,i)                                                     !/*me4c      133*/
      call msndi                                                                !/*me4c      134*/
      m=m+1                                                                     !/*me4c      135*/
  100 continue                                                                  !/*me4c      136*/
!        send last partial packet.                                              !/*me4c      137*/
      call msend(-1,50+m,i)                                                     !/*me4c      138*/
!                                                                               !/*me4c      139*/
  110 write(jtty,'(43hmaster completed initialization broadcasts.)')            !/*me4c      140*/
      return                                                                    !/*me4c      141*/
!                                                                               !/*me4c      142*/
!***********************************************************************        !/*me4c      143*/
! >>>>>  mm = 2 -- assign and execute the microtasks.                           !/*me4c      144*/
!                                                                               !/*me4c      145*/
!        master task -- find the next rendezvous point.                         !/*me4c      146*/
  150 if(nst.ne.0)return                                                        !/*me4c      147*/
      if(ns.gt.0)npd=1000*max(1,nint(ctme*60.*nps/max(one,cts)/10000.))         !/*me4c      148*/
      if(ntal.gt.0)np(1,4)=npd                                                  !/*me4c      149*/
      if(ns.gt.0.and.ntal.eq.0)np(2,6)=nps+npd                                  !/*me4c      150*/
      if(nsr.eq.71)np(2,6)=nps+nsa                                              !/*me4c      151*/
      n=max(100000,np(1,4),np(1,6)-nps)                                         !/*me4c      152*/
      do 160 i=1,6                                                              !/*me4c      153*/
      if(i.le.4.and.np(1,i).gt.0.and.nps.ge.np(2,i))                            !/*me4c      154*/
     & np(2,i)=np(2,i)+np(1,i)*((nps-np(2,i))/np(1,i)+1)                        !/*me4c      155*/
  160 if(nps.lt.np(2,i))n=min(n,np(2,i)-nps)                                    !/*me4c      156*/
!                                                                               !/*me4c      157*/
!        determine the number of microtasks.                                    !/*me4c      158*/
      nm=ltasks                                                                 !/*me4c      159*/
      if(n.lt.mf*ltasks.or.jtasks.lt.0)go to 170                                !/*me4c      160*/
      if(ntasks.gt.1)go to 170                                                  !/*me4c      161*/
      nm=min(mf*ltasks,mn)                                                      !/*me4c      162*/
      if(n.lt.lf*ltasks)go to 170                                               !/*me4c      163*/
      nm=min(lf*ltasks,mn)                                                      !/*me4c      164*/
!                                                                               !/*me4c      165*/
!        set nps ranges for each microtask.                                     !/*me4c      166*/
  170 do 180 i=1,nm                                                             !/*me4c      167*/
      nc(i,1)=nps+int(float(n*i)/nm)                                            !/*me4c      168*/
      nc(i,2)=0                                                                 !/*me4c      169*/
  180 nc(i,3)=0                                                                 !/*me4c      170*/
      nc(0,1)=nps                                                               !/*me4c      171*/
      nc(nm,1)=nps+n                                                            !/*me4c      172*/
      call getidt(hd)                                                           !/*me4c      173*/
      write(jtty,'(27hmaster set rendezvous nps =,i10,5x,a19)')nps+n,hd         !/*me4c      174*/
      if(mct.ge.0)write(iuo,'(27hmaster set rendezvous nps =,i10,5x,            !/*mex        16*/
     & a19)')nps+n,hd                                                           !/*mex        17*/
!                                                                               !/*me4c      176*/
!        send nps ranges for all microtasks.                                    !/*me4c      177*/
      call msndi                                                                !/*me4c      178*/
      call mputi(nm,1,i)                                                        !/*me4c      179*/
      call mputi(nc(0,1),nm+1,i)                                                !/*me4c      180*/
      call msend(-1,20,i)                                                       !/*me4c      181*/
!                                                                               !/*me4c      182*/
!        update variables to the current history.                               !/*me4c      183*/
      call msndi                                                                !/*me4c      184*/
      call mputd(cp1,1,i)                                                       !/*me4c      185*/
      call mputd(cp3,1,i)                                                       !/*me4c      186*/
      call mputd(tdc,1,i)                                                       !/*me4c      187*/
      call mputd(tlc,1,i)                                                       !/*me4c      188*/
      call mputd(trm,1,i)                                                       !/*me4c      189*/
      call mputd(ddn(1,1),24*ndnd1,i)                                           !/*mexk        9*/
      call mputd(dxd(1,1,1),mipt*24*mxdx*min(1,nxnx1),i)                        !/*mexk       10*/
      call mputd(gvarcm,nvarcm,i)                                               !/*me4c      192*/
      call mputi(jvarcm,lvarcm,i)                                               !/*me4c      193*/
      if(nsr.eq.71.and.nss.eq.0)call mputd(fso(lfso+1),6*msrk1,i)               !/*mexk       11*/
      call msend(-1,4,i)                                                        !/*me4c      195*/
!                                                                               !/*me4c      196*/
!        make the initial microtask assignment.                                 !/*me4c      197*/
      do 190 mt=1,ltasks                                                        !/*me4c      198*/
      call msndi                                                                !/*me4c      199*/
      call mputi(mt,1,i)                                                        !/*me4c      200*/
      call msend(mt,31,i)                                                       !/*me4c      201*/
  190 nc(mt,2)=-mt                                                              !/*me4c      202*/
!                                                                               !/*me4c      203*/
!        initialize variables for microtask assignment.                         !/*me4c      204*/
      npstc=nps                                                                 !/*me4c      205*/
      rijktc=rijk                                                               !/*me4c      206*/
      nk=0                                                                      !/*me4c      207*/
      nf=1                                                                      !/*me4c      208*/
      call secnd(t0)                                                            !/*me4c      209*/
!                                                                               !/*me4c      210*/
!        clear the interrupt and check for an available host.                   !/*me4c      211*/
  200 if(irup.ne.0)call qttyin                                                  !/*me4c      212*/
     & (max(1,nc(nk,1)),'31hpvm microtask assignment, nps =,i9')                !/*me4c      213*/
      if(jtasks.lt.0)go to 260                                                  !/*me4c      214*/
      call mprob(-1,30,i)                                                       !/*me4c      215*/
      if(i.ne.0)go to 260                                                       !/*me4c      216*/
!                                                                               !/*me4c      217*/
!        periodically check the status of the slowest processor.                !/*me4c      218*/
      call secnd(t1)                                                            !/*me4c      219*/
      if(t1-t0.lt.dt)go to 200                                                  !/*me4c      220*/
      t0=t1                                                                     !/*me4c      221*/
      do 210 nf=nf,nm                                                           !/*me4c      222*/
      n=nc(nf,2)                                                                !/*me4c      223*/
      if(n.eq.0)go to 200                                                       !/*me4c      224*/
      if(n.gt.0)go to 210                                                       !/*me4c      225*/
      call mstat(-n,j)                                                          !/*me4c      226*/
      if(j.eq.0)go to 200                                                       !/*me4c      227*/
      ltasks=ltasks-1                                                           !/*me4c      228*/
      go to 220                                                                 !/*me4c      229*/
  210 continue                                                                  !/*me4c      230*/
      nf=nm                                                                     !/*me4c      231*/
      go to 200                                                                 !/*me4c      232*/
!                                                                               !/*me4c      233*/
!        check remaining subtasks and send the fault signal.                    !/*me4c      234*/
  220 do 230 nt=1,ltasks                                                        !/*me4c      235*/
      call mstat(nt,j)                                                          !/*me4c      236*/
  230 if(j.ne.0)ltasks=ltasks-1                                                 !/*me4c      237*/
      call errprn(0,-1,1,max(one,one*ltasks),zero,'ntk',' ',                    !/*me4c2       3*/
     & 'one or more pvm processes have terminated prematurely.')                !/*me4c      240*/
      if(ltasks.eq.1)go to 500                                                  !/*me4c      241*/
      ltasks=max(1,ltasks)                                                      !/*me4c      242*/
      if(ltasks.eq.1)return                                                     !/*me4c      243*/
      call msndi                                                                !/*me4c      244*/
      call mputi(-ltasks,1,i)                                                   !/*me4c      245*/
      call msend(-1,31,i)                                                       !/*me4c      246*/
!        reset runtpe-time values.                                              !/*me4c      247*/
      jtfc=0                                                                    !/*me4c      248*/
      mpc=0                                                                     !/*me4c      249*/
      mdc=0                                                                     !/*me4c      250*/
      mrm=0                                                                     !/*me4c      251*/
!        wait for each subtask response to fault signal.                        !/*me4c      252*/
      do 240 nt=1,ltasks                                                        !/*me4c      253*/
  240 call mrecv(-1,32,i)                                                       !/*me4c      254*/
!        clear the microtask-complete message buffers.                          !/*me4c      255*/
  250 call mprob(-1,30,i)                                                       !/*me4c      256*/
!        redo entire set of microtasks if necessary.                            !/*me4c      257*/
      if(i.eq.0)go to 150                                                       !/*me4c      258*/
      call mrecv(-1,30,i)                                                       !/*me4c      259*/
      go to 250                                                                 !/*me4c      260*/
!                                                                               !/*me4c      261*/
!        flag this task as completed and assign the next microtask.             !/*me4c      262*/
  260 call mrecv(-1,30,i)                                                       !/*me4c      263*/
      call mrcvi(nh)                                                            !/*me4c      264*/
      call mgeti(mt,1,i)                                                        !/*me4c      265*/
      nc(mt,2)=nh                                                               !/*me4c      266*/
      nk=nk+1                                                                   !/*me4c      267*/
      mt=nk+ltasks                                                              !/*me4c      268*/
      call msndi                                                                !/*me4c      269*/
      call mputi(mt,1,i)                                                        !/*me4c      270*/
      call msend(nh,31,i)                                                       !/*me4c      271*/
      if(mt.le.nm)nc(mt,2)=-nh                                                  !/*me4c      272*/
      if(nk.lt.nm)go to 200                                                     !/*me4c      273*/
!                                                                               !/*me4c      274*/
!        all microtasks complete, send a completion signal to subtasks.         !/*me4c      275*/
      call msndi                                                                !/*me4c      277*/
      call mputi(-ltasks,1,i)                                                   !/*me4c      278*/
      call msend(-1,31,i)                                                       !/*me4c      279*/
!                                                                               !/*mex        21*/
!        save all variables used to count warning messages.                     !/*mex        22*/
      n0=nerr                                                                   !/*mex        23*/
      n1=netb(1)                                                                !/*mex        24*/
      n2=netb(2)                                                                !/*mex        25*/
      n3=nwse                                                                   !/*mex        26*/
      n4=nwst                                                                   !/*mex        27*/
      n5=nwsg(1)                                                                !/*mex        28*/
      n6=nwsg(2)                                                                !/*mex        29*/
      n7=nwsg(3)                                                                !/*mex        30*/
      n8=nwsb                                                                   !/*mex        31*/
      n9=jrad                                                                   !/*mex        32*/
      m1=nesm                                                                   !/*mex        33*/
      m2=nsom                                                                   !/*mex        34*/
      m3=nppm                                                                   !/*mex        35*/
      m4=npnm                                                                   !/*mex        36*/
      m5=nkrp                                                                   !/*mex        37*/
      m6=jtlx                                                                   !/*mex        38*/
!                                                                               !/*me4c      286*/
!        collect data from subtasks, first come first serve.                    !/*me4c      287*/
      do 360 nt=1,ltasks                                                        !/*me4c      288*/
!                                                                               !/*me4c      289*/
!        receive task common.                                                   !/*me4c      290*/
      call mrecv(-1,4,i)                                                        !/*me4c      291*/
!        mrcvi(nh) identifies host for further receives.                        !/*me4c      292*/
      call mrcvi(nh)                                                            !/*me4c      293*/
      call mgetd(gtskcm,nvarsw,i)                                               !/*me4c      294*/
      call mgeti(jtskcm,lvarsw,i)                                               !/*me4c      295*/
      call mgeti(ktskpt,ltskpt-1,i)                                             !/*me4c      296*/
!                                                                               !/*me4c      297*/
!        receive tallies in chunks.                                             !/*me4c      298*/
      j=0                                                                       !/*me4c      299*/
  270 k=min(lchnk,(nmxf-1)*mxf-j)                                               !/*me4c      300*/
      call mrecv(nh,4,i)                                                        !/*me4c      301*/
      call mgetd(tal(ktal+mxf+j+1),k,i)                                         !/*me4c      302*/
      j=j+k                                                                     !/*me4c      303*/
      if(j.lt.(nmxf-1)*mxf)go to 270                                            !/*me4c      304*/
#ifdef MESHTAL                                                                  /*!mexk       12*/
!                                                                               !/*mexk       13*/
!        receive mesh tally array.                                              !/*mexk       14*/
      if(nmesh.gt.0) call gdtal(19,zero,zc,nh)                                  !/*mexk       15*/
#endif                                                                          /*!mexk       16*/
#ifdef HISTP                                                                    /*!mexk       17*/
!                                                                               !/*mexk       18*/
!        receive histp information.                                             !/*mexk       19*/
      if(ihistp.ne.0) call unpack_histp(nh,ic,id)                               !/*mexk       20*/
#endif                                                                          /*!mexk       21*/
!                                                                               !/*me4c      305*/
!        receive task arrays.                                                   !/*me4c      306*/
      call mrecv(nh,5,i)                                                        !/*me4c      307*/
      call mgetd(ddm(kddm+1,1),2*ntal1*(npert1+1),i)                            !/*mexk       22*/
      call mgetd(ddn(kddn+1,1),24*ndnd1,i)                                      !/*mexk       23*/
      call mgetd(dec(kdec+1,1),3*mxa1*ndnd1,i)                                  !/*mexk       24*/
      call mgetd(dxc(kdxc+1,1),3*mxa1*nxnx1,i)                                  !/*mexk       25*/
      call mgetd(dxd(kdxd+1,1,1),mipt*24*mxdx*min(1,nxnx1),i)                   !/*mexk       26*/
      call mgetd(febl(kfeb+1,1),2*max(16,igm1),i)                               !/*mexk       33*/
      call mgetd(flx(kflx+1),min(1,icw1)*mxa1*igm1,i)                           !/*mexk       34*/
      call mgetd(wwfa(kwfa+1),(mgww1+mipt)*nwwma1,i)                            !/*mexk       35*/
      call mgetd(swwfa(ksww+1),(mgww1+mipt)*nwwma1,i)                           !/*mexk       36*/
      call mgetd(pac(kpac+1,1,1),mipt*10*mxa1,i)                                !/*mexk       37*/
      call mgetd(pan(kpan+1,1,1),3*8*npn1,i)                                    !/*mexk       38*/
      call mgetd(pwb(kpwb+1,1,1),mipt*items_pwb_total*mxa1,i)                   !/*mexk       39*/
      call mgetd(pcc(kpcc+1,1),3*mxa1*kpt1(2),i)                                !/*mexk       40*/
      if(nsr.eq.71)call mgetd(sump(ksum+1),npert1,i)                            !/*mexk       41*/
      call mgetd(wns(kwns+1,1),2*mxxs1/4,i)                                     !/*mexk       42*/
      call mgeti(isef(kise+1,1),2*mxa1,i)                                       !/*mexk       43*/
      call mgeti(maze(kmaz+1),3*nmaz1*(kpt1(1)+kpt1(2)+kpt1(3)),i)              !/*mexk       44*/
      call mgeti(ndpf(kndp+1,1),6*ndnd1,i)                                      !/*mexk       45*/
      call mgeti(ndr(kndr+1),mxe1,i)                                            !/*mex        57*/
!                                                                               !/*me4c      310*/
!        receive tally arrays - shsd,stt,nhsd,tal.                              !/*me4c      311*/
      if(ntal.eq.0)go to 280                                                    !/*me4c      312*/
      call mgetd(shsd(kshs+1,1),nspt*ntal1*(npert1+1),i)                        !/*mexk       46*/
      call mgetd(stt(kstt+1,1),ntp*ntal1*(npert1+1),i)                          !/*mexk       47*/
      call mgeti(nhsd(knhs+1,1),nsp12*ntal1*(npert1+1),i)                       !/*mexk       48*/
!                                                                               !/*me4c      316*/
  280 call vtask                                                                !/*me4c      317*/
!                                                                               !/*me4c      318*/
!        receive output variables not updated in vtask.                         !/*me4c      319*/
      call mgeti(n,1,i)                                                         !/*me4c      320*/
      if(n.eq.0)go to 290                                                       !/*me4c      321*/
      call mgeti(m,1,i)                                                         !/*me4c      322*/
      call mgetc(hs,6,i)                                                        !/*me4c      323*/
      call mgetc(hm,69,i)                                                       !/*me4c      324*/
      if(kdbnps.ne.0)go to 290                                                  !/*me4c      325*/
      kdbnps=n                                                                  !/*me4c      326*/
      mmkdb=m                                                                   !/*me4c      327*/
      hsub=hs                                                                   !/*me4c      328*/
      hmes=hm                                                                   !/*me4c      329*/
  290 call mgeti(n,1,i)                                                         !/*me4c      330*/
      if(kcsf.eq.0)kcsf=n                                                       !/*me4c      331*/
      call mgeti(n,1,i)                                                         !/*me4c      332*/
      call mgeti(nbal(n+1),1,i)                                                 !/*me4c      333*/
      call mgeti(n,1,i)                                                         !/*me4c      334*/
      nbov=nbov+n                                                               !/*me4c      335*/
      call mgeti(n,1,i)                                                         !/*mex        58*/
      nerr=nerr+n-n0                                                            !/*mex        59*/
      call mgeti(n,1,i)                                                         !/*mex        60*/
      netb(1)=netb(1)+n-n1                                                      !/*mex        61*/
      call mgeti(n,1,i)                                                         !/*mex        62*/
      netb(2)=netb(2)+n-n2                                                      !/*mex        63*/
      call mgeti(n,1,i)                                                         !/*mex        64*/
      nwse=nwse+n-n3                                                            !/*mex        65*/
      call mgeti(n,1,i)                                                         !/*mex        66*/
      nwst=nwst+n-n4                                                            !/*mex        67*/
      call mgeti(n,1,i)                                                         !/*mex        68*/
      nwsg(1)=nwsg(1)+n-n5                                                      !/*mex        69*/
      call mgeti(n,1,i)                                                         !/*mex        70*/
      nwsg(2)=nwsg(2)+n-n6                                                      !/*mex        71*/
      call mgeti(n,1,i)                                                         !/*mex        72*/
      nwsg(3)=nwsg(3)+n-n7                                                      !/*mex        73*/
      call mgeti(n,1,i)                                                         !/*mex        74*/
      nwsb=nwsb+n-n8                                                            !/*mex        75*/
      call mgeti(n,1,i)                                                         !/*mex        76*/
      jrad=jrad+n-n9                                                            !/*mex        77*/
      call mgeti(n,1,i)                                                         !/*mex        78*/
      nesm=nesm+n-m1                                                            !/*mex        79*/
      call mgeti(n,1,i)                                                         !/*mex        80*/
      nsom=nsom+n-m2                                                            !/*mex        81*/
      call mgeti(n,1,i)                                                         !/*mex        82*/
      nppm=nppm+n-m3                                                            !/*mex        83*/
      call mgeti(n,1,i)                                                         !/*mex        84*/
      npxm=npxm+n                                                               !/*mex        85*/
      call mgeti(n,1,i)                                                         !/*mex        86*/
      npnm=npnm+n-m4                                                            !/*mex        87*/
      call mgeti(n,1,i)                                                         !/*mex        88*/
      nkrp=nkrp+n-m5                                                            !/*mex        89*/
      call mgeti(n,1,i)                                                         !/*mex        90*/
      jtlx=jtlx+n-m6                                                            !/*mex        91*/
      call mgeti(n,1,i)                                                         !/*me4c      350*/
      nqsw=nqsw+n                                                               !/*me4c      351*/
      call mgeti(n,1,i)                                                         !/*me4c      352*/
      nrsw=nrsw+n                                                               !/*me4c      353*/
      do 300 k=1,njss1+nilw1                                                    !/*mexk       49*/
      do 300 j=1,2+4*mipt                                                       !/*me4c      355*/
      call mgeti(n,1,i)                                                         !/*me4c      356*/
  300 nsl(lnsl+j,k)=nsl(lnsl+j,k)+n                                             !/*me4c      357*/
      call mgeti(n,1,i)                                                         !/*me4c      358*/
      nss=nss+n                                                                 !/*me4c      359*/
      do 310 k=1,ntal1                                                          !/*mexk       50*/
      do 310 j=1,5                                                              !/*me4c      361*/
      call mgeti(n,1,i)                                                         !/*me4c      362*/
  310 ntbb(lntb+j,k)=ntbb(lntb+j,k)+max(0,n-1)                                  !/*me4c      363*/
      do 320 k=1,nww(nips)                                                      !/*me4c      374*/
      do 320 j=1,2                                                              !/*me4c      375*/
      call mgeti(n,1,i)                                                         !/*me4c      376*/
  320 nwws(j,k)=nwws(j,k)+n                                                     !/*me4c      377*/
      do 330 j =1,ndnd1                                                         !/*mexk       51*/
      call mgetd(a,1,i)                                                         !/*me4c      379*/
  330 ddn(lddn+22,j)=ddn(lddn+22,j)+dble(nint(a))                               !/*mex        92*/
      do 340 k=1,mxdx*min(1,nxnx1)                                              !/*mexk       52*/
      do 340 j=1,mipt                                                           !/*me4c      382*/
      call mgetd(a,1,i)                                                         !/*me4c      383*/
  340 dxd(ldxd+j,22,k)=dxd(ldxd+j,22,k)+dble(nint(a))                           !/*mex        93*/
!                                                                               !/*me4c      385*/
!        receive certain variables only from the last microtask.                !/*me4c      386*/
      call mgeti(n,1,i)                                                         !/*me4c      387*/
!                                                                               !/*me4c      388*/
!        receive variables from static variable common.                         !/*me4c      389*/
      if(n.eq.0)go to 360                                                       !/*me4c      390*/
      call mgeti(ixak,1,i)                                                      !/*me4c      391*/
      call mgeti(jtfc,1,i)                                                      !/*me4c      392*/
      call mgeti(kcy,1,i)                                                       !/*me4c      393*/
      call mgeti(laj(llaj+1),(mlaj1+mxa1)*mtasks,i)                             !/*mexk       53*/
      call mgeti(lcaj(llcj+1),(mlja1+1)*mtasks,i)                               !/*mexk       54*/
      call mgeti(mdc,1,i)                                                       !/*me4c      394*/
      call mgeti(mpc,1,i)                                                       !/*me4c      395*/
      call mgeti(mrm,1,i)                                                       !/*me4c      396*/
      mrm=max(mrm,kmplot)                                                       !/*me4c      397*/
      call mgeti(nps,1,i)                                                       !/*me4c      398*/
      call mgeti(npsr,1,i)                                                      !/*me4c      399*/
      call mgeti(nqss,1,i)                                                      !/*me4c      400*/
      call mgeti(nrrs,1,i)                                                      !/*me4c      401*/
      call mgeti(nsa,1,i)                                                       !/*me4c      402*/
      call mgeti(nssi,10,i)                                                     !/*me4c      403*/
      call mgeti(ntc,1,i)                                                       !/*mex        96*/
      call mgeti(ntc1,1,i)                                                      !/*mex        97*/
      call mgeti(n,1,i)                                                         !/*me4c      404*/
      if(n.eq.0)go to 350                                                       !/*me4c      405*/
      if(mod(nst/n,2).eq.0)nst=nst+n                                            !/*me4c      406*/
  350 call mgeti(ntss,1,i)                                                      !/*me4c      407*/
      call mgetd(twac,1,i)                                                      !/*me4c      408*/
      call mgetd(twss,1,i)                                                      !/*me4c      409*/
      call mgetd(wssi,10,i)                                                     !/*me4c      410*/
      call mgetd(rani,1,i)                                                      !/*me4c      411*/
      call mgetd(ranj,1,i)                                                      !/*me4c      412*/
      call mgetd(rijk,1,i)                                                      !/*me4c      413*/
      call mgeti(nijk,1,i)                                                      !/*me4c      414*/
      call mgeti(inif,1,i)                                                      !/*me4c      415*/
  360 continue                                                                  !/*me4c      416*/
!                                                                               !/*me4c      417*/
!        obtain the buffer identifier for each subtask.                         !/*me4c      418*/
      do 370 nt=1,ltasks                                                        !/*me4c      419*/
      call mrecv(-1,34,i)                                                       !/*me4c      420*/
      call mrcvi(nh)                                                            !/*me4c      421*/
  370 call mrecb(0,ib(nh))                                                      !/*me4c      422*/
!                                                                               !/*me4c      423*/
!        step through the microtasks in order - to preserve tracking.           !/*me4c      424*/
      ko=0                                                                      !/*me4c      425*/
      nc(0,2)=0                                                                 !/*me4c      426*/
      do 430 mt=1,nm                                                            !/*me4c      427*/
      nh=nc(mt,2)                                                               !/*me4c      428*/
      nc(nh,3)=nc(nh,3)+1                                                       !/*me4c      429*/
      call mrecb(ib(nh),ib(nc(mt-1,2)))                                         !/*me4c      430*/
!                                                                               !/*me4c      431*/
!        receive output file messages.                                          !/*me4c      432*/
  380 call mgetc(hf,130,j)                                                      !/*me4c      433*/
      if(hf(1:5).eq.'$end$')go to 390                                           !/*me4c      434*/
      write(iuo,'(a130)')hf                                                     !/*me4c      435*/
      go to 380                                                                 !/*me4c      436*/
!                                                                               !/*me4c      437*/
!        receive ssw data.                                                      !/*me4c      438*/
  390 if(issw.eq.0)go to 410                                                    !/*me4c      439*/
      call mgeti(n,1,i)                                                         !/*me4c      440*/
      do 400 k=1,n                                                              !/*me4c      441*/
      call mgetd(rg,nd,j)                                                       !/*me4c      442*/
  400 write(iusc)(rg(i),i=1,nd)                                                 !/*me4c      443*/
!                                                                               !/*me4c      444*/
!        receive kcode source data.                                             !/*me4c      445*/
  410 if(nsr.ne.71)go to 430                                                    !/*me4c      446*/
      call mgeti(n,1,i)                                                         !/*me4c      447*/
      do 420 k=1,n                                                              !/*me4c      448*/
      call mgetd(fso(lfso+6*(ist-1)+1),6,i)                                     !/*me4c      449*/
      if(ist.eq.msrk)ist=0                                                      !/*me4c      450*/
  420 ist=ist+1                                                                 !/*me4c      451*/
!                                                                               !/*me4c      452*/
  430 continue                                                                  !/*me4c      453*/
!                                                                               !/*me4c      454*/
!        free the receive buffers.                                              !/*me4c      455*/
      do 440 nt=1,ltasks                                                        !/*me4c      456*/
  440 call mfbuf(ib(nt),i)                                                      !/*me4c      457*/
!                                                                               !/*me4c      458*/
!        receive timing data from each subtask.                                 !/*me4c      459*/
      c1=cp1                                                                    !/*me4c      460*/
      c3=cp3                                                                    !/*me4c      461*/
      cs=cts                                                                    !/*mex        98*/
      do 450 nt=1,ltasks                                                        !/*me4c      462*/
      call mrecv(-1,35,i)                                                       !/*me4c      463*/
      call mgetd(a,1,i)                                                         !/*me4c      464*/
      cp1=cp1+(a-c1)/ltasks                                                     !/*me4c      465*/
      call mgetd(a,1,i)                                                         !/*me4c      466*/
      cp3=cp3+a-c3                                                              !/*me4c      467*/
      call mgetd(a,1,i)                                                         !/*mex        99*/
      cts=cts+a-cs                                                              !/*mex       100*/
      do 450 j=1,2                                                              !/*me4c      469*/
      call mgetd(a,1,i)                                                         !/*me4c      470*/
  450 pptme(j)=pptme(j)+a                                                       !/*me4c      471*/
!                                                                               !/*me4c      472*/
!        find the ratio of load balance and set lf => 1.                        !/*me4c      473*/
      if(jtasks.lt.0.or.ntasks.gt.1)go to 470                                   !/*me4c      474*/
      lp=lf                                                                     !/*me4c      475*/
      nn=nm                                                                     !/*me4c      476*/
      nx=1                                                                      !/*me4c      477*/
      do 460 nt=1,ltasks                                                        !/*me4c      478*/
      nn=min(nn,nc(nt,3))                                                       !/*me4c      479*/
  460 nx=max(nx,nc(nt,3))                                                       !/*me4c      480*/
      lf=min(int(float(nx)/max(1,nn)),3)**3                                     !/*me4c      481*/
      if(lp.eq.1.and.lf.eq.1)lf=mf                                              !/*me4c      482*/
  470 continue                                                                  !/*mex       101*/
!                                                                               !/*me4c      492*/
!        undo nst=4 and reset appropriately.                                    !/*me4c      493*/
      if((nps.lt.npp.or.npp.eq.0).and.mod(nst/4,2).ne.0)nst=nst-4               !/*me4c      494*/
      if(krtm.ne.0.and.nst.ne.0)mrm=1                                           !/*me4c      495*/
      return                                                                    !/*me4c      496*/
!                                                                               !/*me4c      497*/
!***********************************************************************        !/*me4c      498*/
! >>>>>  mm = 3 -- kill the subtasks in an organized manner.                    !/*me4c      499*/
!                                                                               !/*me4c      500*/
!        master task -- send message types 20 and 31.                           !/*me4c      501*/
  500 call msndi                                                                !/*me4c      502*/
      call mputi(0,1,i)                                                         !/*me4c      503*/
      call msend(-1,20,i)                                                       !/*me4c      504*/
      call msndi                                                                !/*me4c      505*/
      call mputi(0,1,i)                                                         !/*me4c      506*/
      call msend(-1,31,i)                                                       !/*me4c      507*/
!                                                                               !/*me4c      508*/
!        wait for a response from the subtasks.                                 !/*me4c      509*/
      do 510 nt=1,ltasks                                                        !/*me4c      510*/
  510 call mrecv(-1,99,i)                                                       !/*me4c      511*/
      return                                                                    !/*me4c      512*/
!                                                                               !/*me4c      513*/
! >>>>>  handle all errors.                                                     !/*me4c      514*/
  600 call expirx(0,'msgcon','unexpected eof on '//rssa//' file.')              !/*me4c      515*/
#endif                                                                          /*!mex       102*/
      return                                                                    !/*me4c      516*/
      end                                                                       !/*me4c      517*/

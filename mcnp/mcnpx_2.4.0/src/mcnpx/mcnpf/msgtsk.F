!#ident msgtsk.F from deck mh4c                                                 !/*mh4c        1*/
      subroutine msgtsk                                                         !/*mh4c        3*/
#ifdef MULTP                                                                    /*!mhx         1*/
!        called by distributed memory subtasks.                                 !/*mh4c        4*/
!        they never return.                                                     !/*mh4c        5*/
      use allocate_memory                                                       !/*mhx         2*/
      use global_data                                                           !/*mhx         3*/
      use message_passing                                                       !/*mhxk        1*/
      use histp                                                                 !/*mhxk        2*/
!                                                                               !/*mhx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*mhx         5*/
      implicit integer(kindi) (i-n)                                             !/*mhx         6*/
!                                                                               !/*mhx         7*/
      parameter (mn=30*mcpu)                                                    !/*mh4c        7*/
       real, external :: wclock                                                 !/*mhxk        3*/
#ifdef MULTT                                                                    /*!mh4c        8*/
      integer*4 mz                                                              !/*mh4c        9*/
#endif                                                                          /*!cpp       254*/
      dimension rg(11),nc(0:mn,4)                                               !/*mh4c       10*/
      dimension jf(ltskpt)                                                      !/*mh4c       11*/
      character hf*130                                                          !/*mh4c       12*/
      save nd                                                                   !/*mh4c       13*/
      data nd/11/                                                               !/*mh4c2       1*/
!                                                                               !/*mh4c       15*/
!        subtasks -- receive common block data from master task.                !/*mh4c       16*/
      call mrecv(-1,4,i)                                                        !/*mh4c       17*/
      call mgetd(gfixcm,nfixcm,i)                                               !/*mh4c       18*/
      call mgeti(jfixcm,lfixcm,i)                                               !/*mh4c       19*/
      call mgetd(gvarcm,nvarcm,i)                                               !/*mh4c       20*/
      call mgeti(jvarcm,lvarcm,i)                                               !/*mh4c       21*/
      call mgetd(gephcm,nephcm,i)                                               !/*mh4c       22*/
      call mgeti(jephcm,lephcm,i)                                               !/*mh4c       23*/
      call secnd(cp2(1))                                                        !/*mh4c       24*/
#ifdef MULTT                                                                    /*!mhxk        4*/
      if(ntasks.gt.1)call thread_number(ntasks)                                 !/*mhxk        5*/
#endif                                                                          /*!mhxk        6*/
!                                                                               !/*mhx         8*/
!        initialize dynamic variables.                                          !/*mhx         9*/
      call dynamic_allocate                                                     !/*mhx        10*/
      tal => chg_mem (tal,(nmxf*mxf+ktls)*(mtasks+1),is)                        !/*mhx        11*/
      if(is.ne.0)go to 980                                                      !/*mhx        12*/
      tal=0.                                                                    !/*mhx        13*/
      allocate (gbnk(mbnk*mtasks), stat=is)                                     !/*mhx        14*/
      if(is.ne.0)go to 980                                                      !/*mhx        15*/
      gbnk=0.                                                                   !/*mhx        16*/
!        variable iunr may be changed in getxst before runtpe written.          !/*mhx        17*/
      allocate (ibnk(0:(nbmx*(lpblcm+2*abs(iunr))+1)*mtasks), stat=is)          !/*mhx        18*/
      if(is.ne.0)go to 980                                                      !/*mhx        19*/
      ibnk=0                                                                    !/*mhx        20*/
!                                                                               !/*mh4c       31*/
!        receive dynamic common.                                                !/*mh4c       32*/
      call mrecv(-1,4,i)                                                        !/*mh4c       33*/
      call fdac_unpack                                                          !/*mhx        23*/
      call vdac_unpack                                                          !/*mhx        24*/
#ifdef MESHTAL                                                                  /*!mhxk        7*/
!                                                                               !/*mhxk        8*/
!        receive mesh tally initialization data.                                !/*mhxk        9*/
      if(nmesh.gt.0) call gdtal(17,zero,zc,jz)                                  !/*mhxk       10*/
#endif                                                                          /*!mhxk       11*/
!                                                                               !/*mh4c       51*/
!        receive cross section data.                                            !/*mh4c       52*/
      if(mxe.eq.0)go to 10                                                      !/*mhxk       12*/
      xss => chg_mem (xss,lxs+4,is)                                             !/*mhxk       13*/
      if(is.ne.0)go to 980                                                      !/*mhxk       14*/
      call mrecv(-1,4,i)                                                        !/*mh4c       54*/
#ifndef XS64                                                                    /*!mh4c       56*/
      call mgetr(xss(lxss+1),lxs,i)                                             !/*mh4c       57*/
#else                                                                           /*!mhx        25*/
      call mgetd(xss(lxss+1),lxs,i)                                             !/*mh4c       59*/
#endif                                                                          /*!mhx        26*/
!                                                                               !/*mh4c       80*/
!        initialize other variables.                                            !/*mh4c       81*/
!        set dimension of ssw data (nd), default = 11.                          !/*mh4c2       2*/
   10 if(nsph.eq.1)nd=6                                                         !/*mhx        27*/
!        initialize workload counters.                                          !/*mh4c       84*/
      nc(0,3)=0                                                                 !/*mh4c       85*/
      nc(0,4)=0                                                                 !/*mh4c       86*/
!                                                                               !/*mh4c       87*/
!        open the outp scratch file.                                            !/*mh4c       88*/
      open(iuo,status='scratch')                                                !/*mh4c       89*/
!                                                                               !/*mh4c       90*/
!        receive rssa file from master task.                                    !/*mh4c       91*/
      if(nsr.ne.6)go to 100                                                     !/*mh4c       92*/
      open(iusr,status='scratch',form='unformatted')                            !/*mh4c       93*/
      rewind iusr                                                               !/*mh4c       94*/
      m=0                                                                       !/*mh4c       95*/
      call mrecv(-1,50,i)                                                       !/*mh4c       96*/
!        mr = max records to send at once.                                      !/*mh4c       97*/
      mr=(lchnk+nrcd-1)/nrcd                                                    !/*mh4c       98*/
      do 90 n=1,nrss-nrrs                                                       !/*mh4c       99*/
      call mgetd(ssb,nrcd,j)                                                    !/*mh4c      100*/
      write(iusr)(ssb(i),i=1,nrcd)                                              !/*mh4c      101*/
      if(mod(n,mr).ne.0.or.n.ge.nrss-nrrs)go to 90                              !/*mh4c      102*/
      m=m+1                                                                     !/*mh4c      103*/
      call mrecv(-1,50+m,i)                                                     !/*mh4c      104*/
   90 continue                                                                  !/*mh4c      105*/
      endfile iusr                                                              !/*mh4c      106*/
      rewind iusr                                                               !/*mh4c      107*/
      read(iusr,end=930)(ssb(i),i=1,nrcd)                                       !/*mh4c      108*/
!                                                                               !/*mh4c      111*/
!        prepare surface-source write file.                                     !/*mh4c      112*/
  100 if (issw.ne.0) then                                                       !/*mhxk       15*/
        open(iusc,status='scratch',form='unformatted')                          !/*mh4c      114*/
        rewind iusc                                                             !/*mh4c      115*/
        nrsw=0                                                                  !/*mh4c      116*/
        nqsw=0                                                                  !/*mh4c      117*/
      endif                                                                     !/*mhxk       16*/
!                                                                               !/*mhxk       17*/
!        initialize high energy physics models.                                 !/*mhxk       18*/
      if(mcal.eq.0) then                                                        !/*mhxk       19*/
         call setlcs(jtty,iuo,ink(43),nmat,kpt,mipt,                            !/*mhxk       20*/
     &    lcaopt,m_lca,optlcb,m_lcb,leaopt,m_lea,optleb,m_leb)                  !/*mhxk       21*/
         call mtinp1 (mxa,nel(lnel+1),mel(lmel+1),aa9(laa9+1),                  !/*mhxk       22*/
     &    zz9(lzz9+1),fmf(lfmf+1),iza(liza+1),fme(lfme+1),jmd(ljmd+1))          !/*mhxk       23*/
         call matinp (mxa,nel(lnel+1),mel(lmel+1),aa9(laa9+1),                  !/*mhxk       24*/
     &    zz9(lzz9+1),fmf(lfmf+1),arg(larg+1),rho(lrho+1))                      !/*mhxk       25*/
         call xin (nel(lnel+1),mel(lmel+1),iazdex(liax+1),fmf(lfmf+1),          !/*mhxk       26*/
     &    zz9(lzz9+1),aa9(laa9+1),rho(lrho+1),sigg(lsgg+1,1),                   !/*mhxk       27*/
     &    hsigg(lhsg+1,1),sigmx(lsgx+1,1),pcap(lpcp+1),esxln(lexn+1,1),         !/*mhxk       28*/
     &    esxlp(lexp+1,1),esxmp(lesp+1))                                        !/*mhxk       29*/
      endif                                                                     !/*mhxk       30*/
      call thetag                                                               !/*mhxk       31*/
#ifdef HISTP                                                                    /*!mhxk       32*/
      if(ihistp.ne.0) call ins_histp_d(mynum)                                   !/*mhxk       33*/
#endif                                                                          /*!mhxk       34*/
!                                                                               !/*mh4c      120*/
!        subtasks -- execute the microtasks.                                    !/*mh4c      121*/
!                                                                               !/*mh4c      122*/
!        receive task assignments.                                              !/*mh4c      123*/
  110 call mrecv(-1,20,i)                                                       !/*mh4c      124*/
      call mgeti(nm,1,i)                                                        !/*mh4c      125*/
      if(nm.eq.0)go to 910                                                      !/*mh4c      126*/
      call mgeti(nc(0,1),nm+1,i)                                                !/*mh4c      127*/
!                                                                               !/*mh4c      128*/
!        update variables to the current history.                               !/*mh4c      129*/
      n1=nrrs                                                                   !/*mh4c      130*/
      call mrecv(-1,4,i)                                                        !/*mh4c      131*/
      call mgetd(cp1,1,i)                                                       !/*mh4c      132*/
      call mgetd(cp3,1,i)                                                       !/*mh4c      133*/
      call mgetd(tdc,1,i)                                                       !/*mh4c      134*/
      call mgetd(tlc,1,i)                                                       !/*mh4c      135*/
      call mgetd(trm,1,i)                                                       !/*mh4c      136*/
      call mgetd(ddn(1,1),24*ndnd1,i)                                           !/*mhxk       35*/
      call mgetd(dxd(1,1,1),mipt*24*mxdx*min(1,nxnx1),i)                        !/*mhxk       36*/
      call mgetd(gvarcm,nvarcm,i)                                               !/*mh4c      139*/
      call mgeti(jvarcm,lvarcm,i)                                               !/*mh4c      140*/
      if(nsr.eq.71.and.nss.eq.0)call mgetd(fso(lfso+1),6*msrk1,i)               !/*mhxk       37*/
!                                                                               !/*mh4c      142*/
!        initialize various variables for this rendezvous.                      !/*mh4c      143*/
      do 120 i=1,nm                                                             !/*mh4c      144*/
  120 nc(i,2)=0                                                                 !/*mh4c      145*/
      is=ist                                                                    !/*mh4c      146*/
      irt=50                                                                    !/*mh4c      147*/
      itask=0                                                                   !/*mh4c      148*/
      nbov=0                                                                    !/*mh4c      149*/
      nqsw=0                                                                    !/*mh4c      152*/
      nrsw=0                                                                    !/*mh4c      153*/
      do 130 k=1,njss1+nilw1                                                    !/*mhxk       38*/
      do 130 j=1,2+4*nipt                                                       !/*mhx        31*/
  130 nsl(lnsl+j,k)=0                                                           !/*mh4c      156*/
      nss=0                                                                     !/*mh4c      157*/
      do 140 k=1,ntal1                                                          !/*mhxk       39*/
      do 140 j=1,5                                                              !/*mh4c      159*/
  140 ntbb(lntb+j,k)=1                                                          !/*mh4c      160*/
      do 150 k=1,nww(nips)                                                      !/*mh4c      166*/
      do 150 j=1,2                                                              !/*mh4c      167*/
  150 nwws(j,k)=0                                                               !/*mh4c      168*/
      pptme(1)=0.                                                               !/*mh4c      169*/
      pptme(2)=0.                                                               !/*mh4c      170*/
!                                                                               !/*mh4c      171*/
!        prepare various input and output files.                                !/*mh4c      172*/
      rewind iuo                                                                !/*mh4c      173*/
      if(issw.ne.0)rewind iusc                                                  !/*mh4c      174*/
      if(nsr.ne.6)go to 170                                                     !/*mh4c      175*/
      do 160 i=1,nrrs-n1-1                                                      !/*mh4c      176*/
  160 read(iusr,end=930)                                                        !/*mh4c      177*/
      if(n1.lt.nrrs.and.nrrs.lt.nrss)                                           !/*mh4c      178*/
     & read(iusr,end=930)(ssb(i),i=1,nrcd)                                      !/*mh4c      179*/
!                                                                               !/*mh4c      180*/
!        clear the swept part of varcom.                                        !/*mh4c      181*/
  170 do 180 i=1,nvarsw                                                         !/*mhx        32*/
  180 gvarcm(i)=0.                                                              !/*mhx        33*/
      wgts(1)=huge                                                              !/*mhx        34*/
      do 190 i=1,lvarsw                                                         !/*mh4c      186*/
  190 jvarcm(i)=0                                                               !/*mh4c      187*/
!                                                                               !/*mh4c      188*/
!        receive the next microtask assignment.                                 !/*mh4c      189*/
  200 call mrecv(-1,31,i)                                                       !/*mh4c      190*/
      call mgeti(mt,1,i)                                                        !/*mh4c      191*/
!        no available tasks, wait for signal.                                   !/*mh4c      192*/
      if(mt.gt.nm)go to 200                                                     !/*mh4c      193*/
!        all tasks complete, report results.                                    !/*mh4c      194*/
      if(mt.lt.0)go to 300                                                      !/*mh4c      195*/
!        no more work, terminate.                                               !/*mh4c      196*/
      if(mt.eq.0)go to 910                                                      !/*mh4c      197*/
!        default, receive assignment and fall through.                          !/*mh4c      198*/
!                                                                               !/*mh4c      199*/
!        set nps, npp, random number seed, rssa file, and kcode source.         !/*mh4c      200*/
      nst=0                                                                     !/*mh4c      201*/
#ifdef MULTT                                                                    /*!mh4c      202*/
      kbp=0                                                                     !/*mh4c      203*/
#else                                                                           /*!mhxk       40*/
      kbp=-1                                                                    !/*mh4c      205*/
      if(mt.eq.nm)kbp=0                                                         !/*mh4c      206*/
#endif                                                                          /*!cpp       261*/
      r=rnrtc                                                                   !/*mh4c      207*/
      do 220 i=nps+1,nc(mt-1,1)                                                 !/*mh4c      208*/
      if(inif.eq.0)call advijk                                                  !/*mh4c      209*/
      inif=0                                                                    !/*mh4c      210*/
      nps=nps+1                                                                 !/*mh4c      211*/
      ranb=rani                                                                 !/*mh4c      212*/
      rans=ranj                                                                 !/*mh4c      213*/
      if(nsr.ne.6)go to 210                                                     !/*mh4c      214*/
      call sursrc(1)                                                            !/*mh4c      215*/
      if(nrrs.ge.nrss)go to 230                                                 !/*mh4c      216*/
      go to 220                                                                 !/*mh4c      217*/
  210 if(nsr.ne.71)go to 220                                                    !/*mh4c      218*/
      nsa=nsa-1                                                                 !/*mh4c      219*/
      if(ksdef.gt.0)go to 220                                                   !/*mh4c      220*/
      if(ixak.eq.msrk)ixak=0                                                    !/*mh4c      221*/
      ixak=ixak+1                                                               !/*mh4c      222*/
  220 continue                                                                  !/*mh4c      223*/
!                                                                               !/*mh4c      224*/
  230 rnrtc=r                                                                   !/*mh4c      225*/
      npp=nc(mt,1)                                                              !/*mh4c      226*/
!                                                                               !/*mh4c      227*/
!        transport assigned histories.                                          !/*mh4c      228*/
      w2=wclock()                                                               !/*mh4c      229*/
      if(ntasks.le.1) then                                                      !/*mhxk       41*/
        call trnspt                                                             !/*mhxk       42*/
      else                                                                      !/*mhxk       43*/
#ifdef MULTT                                                                    /*!mhxk       44*/
!                                                                               !/*mhxk       45*/
!        master thread saves, restores l pointers since used in vtask.          !/*mhxk       46*/
        jlock=-1                                                                !/*mhxk       47*/
        jf=lpntcm                                                               !/*mhxk       48*/
        call thread_routine(trnspt)                                             !/*mhxk       49*/
        lpntcm=jf                                                               !/*mhxk       50*/
        jlock=0                                                                 !/*mhxk       51*/
#endif                                                                          /*!mhxk       52*/
      endif                                                                     !/*mhxk       53*/
!                                                                               !/*mh4c      251*/
!        record wallclock time spent in trnspt.                                 !/*mh4c      252*/
  280 pptme(2)=pptme(2)+wclock()-w2                                             !/*mh4c      253*/
!                                                                               !/*mh4c      254*/
!        microtask complete, notify master and get next microtask.              !/*mh4c      255*/
      call msndi                                                                !/*mh4c      256*/
      call mputi(mt,1,i)                                                        !/*mh4c      257*/
      call msend(0,30,i)                                                        !/*mh4c      258*/
      write(iuo,'(5h$end$)')                                                    !/*mh4c      259*/
      nc(mt,2)=mynum                                                            !/*mh4c      260*/
      nc(mt,3)=nrsw                                                             !/*mh4c      261*/
      nc(mt,4)=nss                                                              !/*mh4c      262*/
      itask=-1                                                                  !/*mh4c      263*/
      go to 200                                                                 !/*mh4c      264*/
!                                                                               !/*mh4c      265*/
!        all microtasks complete, check for fault signal.                       !/*mh4c      266*/
  300 itask=-2                                                                  !/*mh4c      267*/
!                                                                               !/*mh4c      268*/
!        fault signal detected. reset ltasks and acknowledge signal.            !/*mh4c      269*/
      if(-mt.eq.ltasks)go to 310                                                !/*mh4c      270*/
      ltasks=-mt                                                                !/*mh4c      271*/
      call msndi                                                                !/*mh4c      272*/
      call msend(0,32,i)                                                        !/*mh4c      273*/
!        reset variables and rewind files.                                      !/*mh4c      274*/
      call vtask                                                                !/*mh4c      275*/
      n1=-1                                                                     !/*mh4c      276*/
      if(nsr.eq.6)rewind iusr                                                   !/*mh4c      277*/
!        loop back to restart this set of microtasks.                           !/*mh4c      278*/
      go to 110                                                                 !/*mh4c      279*/
!                                                                               !/*mh4c      280*/
!        send task common data stored in varcom.                                !/*mh4c      281*/
  310 call msndi                                                                !/*mh4c      282*/
      call mputd(gvarcm,nvarsw,i)                                               !/*mh4c      283*/
      call mputi(jvarcm,lvarsw,i)                                               !/*mh4c      284*/
      call mputi(ktskpt,ltskpt-1,i)                                             !/*mh4c      285*/
      call msend(0,4,i)                                                         !/*mh4c      286*/
!                                                                               !/*mh4c      287*/
!        send tallies in chunks.                                                !/*mh4c      288*/
      j=0                                                                       !/*mh4c      289*/
  320 k=min(lchnk,(nmxf-1)*mxf-j)                                               !/*mh4c      290*/
      call msndi                                                                !/*mh4c      291*/
      call mputd(tal(ktal+mxf+j+1),k,i)                                         !/*mh4c      292*/
      call msend(0,4,i)                                                         !/*mh4c      293*/
      j=j+k                                                                     !/*mh4c      294*/
      if(j.lt.(nmxf-1)*mxf)go to 320                                            !/*mh4c      295*/
#ifdef MESHTAL                                                                  /*!mhxk       54*/
!                                                                               !/*mhxk       55*/
!        send mesh tally data.                                                  !/*mhxk       56*/
      if(nmesh.gt.0) call gdtal(18,zero,zc,jz)                                  !/*mhxk       57*/
#endif                                                                          /*!mhxk       58*/
#ifdef HISTP                                                                    /*!mhxk       59*/
!                                                                               !/*mhxk       60*/
!        send histp information.                                                !/*mhxk       61*/
      if(ihistp.ne.0) call pack_histp(ic)                                       !/*mhxk       62*/
#endif                                                                          /*!mhxk       63*/
!                                                                               !/*mh4c      296*/
!        send task arrays.                                                      !/*mh4c      297*/
      call msndi                                                                !/*mh4c      298*/
      call mputd(ddm(kddm+1,1),2*ntal1*(npert1+1),i)                            !/*mhxk       64*/
      call mputd(ddn(kddn+1,1),24*ndnd1,i)                                      !/*mhxk       65*/
      call mputd(dec(kdec+1,1),3*mxa1*ndnd1,i)                                  !/*mhxk       66*/
      call mputd(dxc(kdxc+1,1),3*mxa1*nxnx1,i)                                  !/*mhxk       67*/
      call mputd(dxd(kdxd+1,1,1),mipt*24*mxdx*min(1,nxnx1),i)                   !/*mhxk       68*/
      call mputd(febl(kfeb+1,1),2*max(16,igm1),i)                               !/*mhxk       75*/
      call mputd(flx(kflx+1),min(1,icw1)*mxa1*igm1,i)                           !/*mhxk       76*/
      call mputd(wwfa(kwfa+1),(mgww1+mipt)*nwwma1,i)                            !/*mhxk       77*/
      call mputd(swwfa(ksww+1),(mgww1+mipt)*nwwma1,i)                           !/*mhxk       78*/
      call mputd(pac(kpac+1,1,1),mipt*10*mxa1,i)                                !/*mhxk       79*/
      call mputd(pan(kpan+1,1,1),3*8*npn1,i)                                    !/*mhxk       80*/
      call mputd(pwb(kpwb+1,1,1),mipt*items_pwb_total*mxa1,i)                   !/*mhxk       81*/
      call mputd(pcc(kpcc+1,1),3*mxa1*kpt1(2),i)                                !/*mhxk       82*/
      if(nsr.eq.71)call mputd(sump(ksum+1),npert1,i)                            !/*mhxk       83*/
      call mputd(wns(kwns+1,1),2*mxxs1/4,i)                                     !/*mhxk       84*/
      call mputi(isef(kise+1,1),2*mxa1,i)                                       !/*mhxk       85*/
      call mputi(maze(kmaz+1),3*nmaz1*(kpt1(1)+kpt1(2)+kpt1(3)),i)              !/*mhxk       86*/
      call mputi(ndpf(kndp+1,1),6*ndnd1,i)                                      !/*mhxk       87*/
      call mputi(ndr(kndr+1),mxe1,i)                                            !/*mhx        53*/
!                                                                               !/*mh4c      301*/
!        send tally arrays - shsd,stt,nhsd,tal                                  !/*mh4c      302*/
      if(ntal.eq.0)go to 330                                                    !/*mh4c      303*/
      call mputd(shsd(kshs+1,1),nspt*ntal1*(npert1+1),i)                        !/*mhxk       88*/
      call mputd(stt(kstt+1,1),ntp*ntal1*(npert1+1),i)                          !/*mhxk       89*/
      call mputi(nhsd(knhs+1,1),nsp12*ntal1*(npert1+1),i)                       !/*mhxk       90*/
!                                                                               !/*mh4c      307*/
!        send output variables not updated in vtask - a bit tricky.             !/*mh4c      308*/
  330 call vtask                                                                !/*mh4c      309*/
      call mputi(kdbnps,1,i)                                                    !/*mh4c      310*/
      if(kdbnps.eq.0)go to 340                                                  !/*mh4c      311*/
      call mputi(mmkdb,1,i)                                                     !/*mh4c      312*/
      call mputc(hsub,6,i)                                                      !/*mh4c      313*/
      call mputc(hmes,69,i)                                                     !/*mh4c      314*/
  340 call mputi(kcsf,1,i)                                                      !/*mh4c      315*/
      call mputi(mynum,1,i)                                                     !/*mh4c      316*/
      call mputi(nbal(mynum+1),1,i)                                             !/*mh4c      317*/
      call mputi(nbov,1,i)                                                      !/*mh4c      318*/
      call mputi(nerr,1,i)                                                      !/*mhx        54*/
      call mputi(netb,2,i)                                                      !/*mhx        55*/
      call mputi(nwse,1,i)                                                      !/*mhx        56*/
      call mputi(nwst,1,i)                                                      !/*mhx        57*/
      call mputi(nwsg,3,i)                                                      !/*mhx        58*/
      call mputi(nwsb,1,i)                                                      !/*mhx        59*/
      call mputi(jrad,1,i)                                                      !/*mhx        60*/
      call mputi(nesm,1,i)                                                      !/*mhx        61*/
      call mputi(nsom,1,i)                                                      !/*mhx        62*/
      call mputi(nppm,1,i)                                                      !/*mhx        63*/
      call mputi(npxm,1,i)                                                      !/*mhx        64*/
      call mputi(npnm,1,i)                                                      !/*mhx        65*/
      call mputi(nkrp,1,i)                                                      !/*mhx        66*/
      call mputi(jtlx,1,i)                                                      !/*mhx        67*/
      call mputi(nqsw,1,i)                                                      !/*mh4c      325*/
      call mputi(nrsw,1,i)                                                      !/*mh4c      326*/
      call mputi(nsl(lnsl+1,1),(2+4*mipt)*(njss1+nilw1),i)                      !/*mhxk       91*/
      call mputi(nss,1,i)                                                       !/*mh4c      328*/
      call mputi(ntbb(lntb+1,1),5*ntal1,i)                                      !/*mhxk       92*/
      call mputi(nwws,2*nww(nips),i)                                            !/*mh4c      334*/
      do 350 j=1,ndnd1                                                          !/*mhxk       93*/
  350 call mputd(ddn(lddn+22,j)-ddn(kddn+22,j),1,i)                             !/*mhx        68*/
      do 360 k=1,mxdx*min(1,nxnx1)                                              !/*mhxk       94*/
      do 360 j=1,mipt                                                           !/*mh4c      338*/
  360 call mputd(dxd(ldxd+j,22,k)-dxd(kdxd+j,22,k),1,i)                         !/*mhx        69*/
!                                                                               !/*mh4c      340*/
!        send variables depending on if the last microtask.                     !/*mh4c      341*/
      if(mynum.eq.nc(nm,2))go to 370                                            !/*mh4c      342*/
      call mputi(0,1,i)                                                         !/*mh4c      343*/
      go to 380                                                                 !/*mh4c      344*/
!                                                                               !/*mh4c      345*/
!        send variables from static variable common.                            !/*mh4c      346*/
  370 call mputi(1,1,i)                                                         !/*mh4c      347*/
      call mputi(ixak,1,i)                                                      !/*mh4c      348*/
      call mputi(jtfc,1,i)                                                      !/*mh4c      349*/
      call mputi(kcy,1,i)                                                       !/*mh4c      350*/
      call mputi(laj(llaj+1),(mlaj1+mxa1)*mtasks,i)                             !/*mhxk       95*/
      call mputi(lcaj(llcj+1),(mlja1+1)*mtasks,i)                               !/*mhxk       96*/
      call mputi(mdc,1,i)                                                       !/*mh4c      351*/
      call mputi(mpc,1,i)                                                       !/*mh4c      352*/
      call mputi(mrm,1,i)                                                       !/*mh4c      353*/
      call mputi(nps,1,i)                                                       !/*mh4c      354*/
      call mputi(npsr,1,i)                                                      !/*mh4c      355*/
      call mputi(nqss,1,i)                                                      !/*mh4c      356*/
      call mputi(nrrs,1,i)                                                      !/*mh4c      357*/
      call mputi(nsa,1,i)                                                       !/*mh4c      358*/
      call mputi(nssi,10,i)                                                     !/*mh4c      359*/
      call mputi(ntc,1,i)                                                       !/*mhx        72*/
      call mputi(ntc1,1,i)                                                      !/*mhx        73*/
      call mputi(nst,1,i)                                                       !/*mh4c      360*/
      call mputi(ntss,1,i)                                                      !/*mh4c      361*/
      call mputd(twac,1,i)                                                      !/*mh4c      362*/
      call mputd(twss,1,i)                                                      !/*mh4c      363*/
      call mputd(wssi,10,i)                                                     !/*mh4c      364*/
      call mputd(rani,1,i)                                                      !/*mh4c      365*/
      call mputd(ranj,1,i)                                                      !/*mh4c      366*/
      call mputd(rijk,1,i)                                                      !/*mh4c      367*/
      call mputi(nijk,1,i)                                                      !/*mh4c      368*/
      call mputi(inif,1,i)                                                      !/*mh4c      369*/
  380 call msend(0,5,i)                                                         !/*mh4c      370*/
!                                                                               !/*mh4c      371*/
!        prepare scratch files and sort multi-file data.                        !/*mh4c      372*/
      rewind iuo                                                                !/*mh4c      373*/
      if(issw.ne.0)rewind iusc                                                  !/*mh4c      374*/
!                                                                               !/*mh4c      375*/
!        send some data in order of performed microtasks.                       !/*mh4c      376*/
      ist=is                                                                    !/*mh4c      377*/
      mp=0                                                                      !/*mh4c      378*/
      call msndi                                                                !/*mh4c      379*/
!        do submicrotasks.                                                      !/*mh4c      380*/
      do 450 mt=1,nm                                                            !/*mh4c      381*/
      if(nc(mt,2).eq.0)go to 450                                                !/*mh4c      382*/
!                                                                               !/*mh4c      383*/
!        send text written to outp file.                                        !/*mh4c      384*/
  390 read(iuo,'(a130)',end=400)hf                                              !/*mh4c      385*/
      call mputc(hf,130,i)                                                      !/*mh4c      386*/
      if(hf.ne.'$end$')go to 390                                                !/*mh4c      387*/
!                                                                               !/*mh4c      388*/
!        send ssw data.                                                         !/*mh4c      389*/
  400 if(issw.eq.0)go to 420                                                    !/*mh4c      390*/
      n=nc(mt,3)-nc(mp,3)                                                       !/*mh4c      391*/
      call mputi(n,1,i)                                                         !/*mh4c      392*/
      do 410 k=1,n                                                              !/*mh4c      393*/
      read(iusc,end=920)(rg(i),i=1,nd)                                          !/*mh4c      394*/
  410 call mputd(rg,nd,j)                                                       !/*mh4c      395*/
!                                                                               !/*mh4c      396*/
!        send kcode source data.                                                !/*mh4c      397*/
  420 if(nsr.ne.71)go to 440                                                    !/*mh4c      398*/
      n=nc(mt,4)-nc(mp,4)                                                       !/*mh4c      399*/
      call mputi(n,1,i)                                                         !/*mh4c      400*/
      do 430 k=1,n                                                              !/*mh4c      401*/
      call mputd(fso(lfso+6*(ist-1)+1),6,i)                                     !/*mh4c      402*/
      if(ist.eq.msrk)ist=0                                                      !/*mh4c      403*/
  430 ist=ist+1                                                                 !/*mh4c      404*/
!                                                                               !/*mh4c      405*/
!        end of submicrotasks do loop.                                          !/*mh4c      406*/
  440 mp=mt                                                                     !/*mh4c      407*/
  450 continue                                                                  !/*mh4c      408*/
      call msend(0,34,i)                                                        !/*mh4c      409*/
!                                                                               !/*mh4c      410*/
!        send timing data to master.                                            !/*mh4c      411*/
!        cp1 = total cpu time per processor for this run.                       !/*mh4c      412*/
!        cp3 = total cpu time from all processors for this run.                 !/*mh4c      413*/
!        pptme(1) = wall-time wasted to locks.                                  !/*mh4c      414*/
!        pptme(2) = wall-time spent on tracking particles.                      !/*mh4c      415*/
      call secnd(t2)                                                            !/*mh4c      416*/
      cp3=cp3+t2-cp2(1)                                                         !/*mh4c      418*/
      cp2(1)=t2                                                                 !/*mhx        74*/
      call msndi                                                                !/*mh4c      419*/
      call mputd(cp1,1,i)                                                       !/*mh4c      420*/
      call mputd(cp3,1,i)                                                       !/*mh4c      421*/
      call mputd(cts,1,i)                                                       !/*mhx        75*/
      call mputd(pptme,2,i)                                                     !/*mh4c      422*/
      call msend(0,35,i)                                                        !/*mh4c      423*/
      go to 110                                                                 !/*mh4c      424*/
!                                                                               !/*mh4c      425*/
!        subtasks -- respond to kill signal, exit, and stop.                    !/*mh4c      426*/
  910 call msndi                                                                !/*mh4c      427*/
      call msend(0,99,i)                                                        !/*mh4c      428*/
#ifdef HISTP                                                                    /*!mhxk       97*/
      if(ihistp.ne.0) call del_histp_d                                          !/*mhxk       98*/
#endif                                                                          /*!mhxk       99*/
      call mterm                                                                !/*mh4c      429*/
      stop                                                                      !/*mh4c      430*/
!                                                                               !/*mh4c      431*/
!        error handling.                                                        !/*mh4c      432*/
  920 write(jtty,'(44hmsgtsk: unexpected eof on subtask wxxa file.)')           !/*mh4c      433*/
      go to 990                                                                 !/*mh4c      434*/
  930 write(jtty,'(44hmsgtsk: unexpected eof on subtask rssa file.)')           !/*mh4c      435*/
      go to 990                                                                 !/*mh4c      436*/
  980 write(jtty,'(39hmsgtsk: subtask dynamic allocate error.)')                !/*mhxk      100*/
  990 call mterm                                                                !/*mh4c      439*/
      stop                                                                      !/*mh4c      440*/
#else                                                                           /*!mhx        76*/
      return                                                                    !/*mhx        77*/
#endif                                                                          /*!mhx        78*/
      end                                                                       !/*mh4c      441*/

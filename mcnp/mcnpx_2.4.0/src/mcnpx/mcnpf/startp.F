!#ident startp.F from deck sp                                                   !/*sp          1*/
      subroutine startp                                                         !/*sp          2*/
!        start a particle from the source.                                      !/*sp          3*/
      use global_data                                                           !/*spx         1*/
!                                                                               !/*spx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*spx         3*/
      implicit integer(kindi) (i-n)                                             !/*spx         4*/
!                                                                               !/*spx         5*/
#ifdef MESHTAL                                                                  /*!spx         6*/
      dimension zd(10),jz(5)                                                    !/*spx         7*/
#endif                                                                          /*!spx         8*/
      character ha*130,hs*7                                                     !/*sp4a        1*/
!                                                                               !/*sp          7*/
!        initialize variables for the new history.                              !/*sp          8*/
      ranb=ranitc                                                               !/*sp          9*/
      rans=ranjtc                                                               !/*sp         10*/
      rnrtc0=rnrtc                                                              !/*sp         11*/
      if(kfl.eq.0)go to 40                                                      !/*sp         12*/
      if(kfl.eq.1)go to 20                                                      !/*sp         13*/
      do 10 i=1,mxa                                                             !/*sp         14*/
   10 ifl(kifl+i)=0                                                             !/*sp         15*/
      if(kfl.eq.2)go to 40                                                      !/*sp         16*/
   20 do 30 j=1,mxj                                                             !/*sp         17*/
   30 jfl(kjfl+j)=0                                                             !/*sp         18*/
   40 node=1                                                                    !/*sp         19*/
      cbwf=1.                                                                   !/*sp         20*/
      iexp=0                                                                    !/*sp4b        1*/
      mtp=0                                                                     !/*sp4b        2*/
      totmp=one                                                                 !/*sp4b        3*/
      do 45 j=1,nptb(lnpt+npert+1)                                              !/*sp4b        4*/
      do 45 k=1,3                                                               !/*sp4b        5*/
   45 ptb(kptb+k,j)=0.                                                          !/*sp4c        1*/
      alfap(1)=0.                                                               !/*sp4c        2*/
      alfap(2)=0.                                                               !/*sp4c        3*/
      irt=50                                                                    !/*sp         21*/
      jtls=0                                                                    !/*sp         22*/
      do 50 i=1,maxv                                                            !/*spx         9*/
   50 isic(i)=0                                                                 !/*sp         24*/
      kdb=0                                                                     !/*sp         25*/
      nbnk=0                                                                    !/*sp         26*/
      jgp=0                                                                     !/*sp         27*/
      jbnk=0                                                                    !/*sp         28*/
      kqss=0                                                                    !/*sp         29*/
      mbb=0                                                                     !/*sp         30*/
      lev=0                                                                     !/*sp         31*/
      lsb=0                                                                     !/*sp         32*/
      gamef=one                                                                 !/*spx        10*/
      mspf=1                                                                    !/*spx        11*/
      do 60 i=1,nipt                                                            !/*spx        12*/
   60 nch(i)=0                                                                  !/*sp         34*/
      ncp=0                                                                     !/*sp         35*/
      nter=0                                                                    !/*sp         36*/
      jev=0                                                                     !/*sp         37*/
      do 70 i=1,mxa                                                             !/*sp         38*/
   70 ipac2(kpc2+i)=0                                                           !/*sp         40*/
      ipsc=3                                                                    !/*sp         41*/
      ipt=nips                                                                  !/*sp         42*/
#ifdef MESHTAL                                                                  /*!spx        13*/
      do 75 i=1,10                                                              !/*spx        14*/
   75 zd(i)=0.                                                                  !/*spx        15*/
      do 76 i=1,5                                                               !/*spx        16*/
   76 jz(i)=0                                                                   !/*spx        17*/
#endif                                                                          /*!spx        18*/
!     TEMPORARY WARNING: Not yet correct for self-anti-particles.               !/*spx        19*/
      if (isanti.eq.0) then                                                     !/*spx        20*/
         jan = not_self_anti(ipt)                                               !/*spxc        1*/
      else                                                                      !/*spx        22*/
         jan = -not_self_anti(ipt)                                              !/*spxc        2*/
      endif                                                                     !/*spx        24*/
      smultc(1)=0.                                                              !/*sp4c        4*/
      smultc(4)=0.                                                              !/*sp4c2       1*/
      if(iwwg.gt.0)call avrnrm(0)                                               !/*sp4c        5*/
!                                                                               !/*sp         43*/
!        use the source subroutine specified by nsr.                            !/*sp         44*/
   80 if(nsr.ne.6)go to 90                                                      !/*spxa        1*/
      call sursrc(0)                                                            !/*sp4a        2*/
      if(nomore.ne.0.or.kdb.ne.0)return                                         !/*sp         47*/
      do 85 i=1,nipt                                                            !/*spx        25*/
      if(wc1(i).ge.0.)go to 85                                                  !/*sp4c        6*/
      wcs1tc(i)=-wc1(i)*wgt                                                     !/*sp4c        7*/
      wcs2tc(i)=-wc2(i)*wgt                                                     !/*sp4c        8*/
   85 continue                                                                  !/*sp4b       11*/
      go to 110                                                                 !/*sp         51*/
   90 if(nsr.ne.0)go to 100                                                     !/*sp         52*/
      call isos(uuu,lev)                                                        !/*sp4b       12*/
      call source                                                               !/*sp         54*/
      if(kdb.ne.0)return                                                        !/*sp         55*/
      if(npstc.le.90.or.krflg.ne.0)call chksrc                                  !/*sp         56*/
      if(kdb.ne.0)return                                                        !/*sp         57*/
      go to 110                                                                 !/*sp         58*/
  100 if(nsr.eq.40.or.ksdef.gt.0) then                                          !/*spx        26*/
        call sourcb                                                             !/*spx        27*/
        if(kdb.ne.0)return                                                      !/*spx        28*/
        jt=srv(1,20)                                                            !/*spx        29*/
!                                                                               !/*spx        30*/
!          sample occurence.                                                    !/*spx        31*/
        if(imt.eq.1)go to 211                                                   !/*spx        32*/
        kd=ksd(lksd+13,ivdis(20))                                               !/*spx        33*/
        r=rang()                                                                !/*spx        34*/
        do 201 jm=1,imt-1                                                       !/*spx        35*/
  201   if(r.le.spf(kd+2,jm))go to 191                                          !/*spx        36*/
  191   w1=spf(kd+3,jm)                                                         !/*spx        37*/
        jt=spf(ksd(lksd+13,ivdis(20))+1,jm)                                     !/*spx        38*/
        wgt=wgt*w1                                                              !/*spx        39*/
        wns(kwns+ksd(lksd+16,ivdis(20))+1,jm)=                                  !/*spxc        3*/
     &    wns(kwns+ksd(lksd+16,ivdis(20))+1,jm)+one                             !/*spxc        4*/
        wns(kwns+ksd(lksd+16,ivdis(20))+2,jm)=                                  !/*spxc        5*/
     &    wns(kwns+ksd(lksd+16,ivdis(20))+2,jm)+wgt                             !/*spxc        6*/
!                                                                               !/*spx        40*/
!          do the transformation and define the surface number.                 !/*spx        41*/
  211   if(jt.eq.0)go to 241                                                    !/*spx        42*/
        do 221 i=1,6                                                            !/*spx        43*/
  221   tpp(i)=gpblcm(i)                                                        !/*spx        44*/
        do 231 i=1,3                                                            !/*spx        45*/
        gpblcm(i)=trf(ltrf+14+i,jt)                                             !/*spx        46*/
        gpblcm(3+i)=0.                                                          !/*spx        47*/
        n=4+3*(i-1)                                                             !/*spx        48*/
        do 231 j=1,3                                                            !/*spx        49*/
        gpblcm(i)=gpblcm(i)+trf(ltrf+n+j,jt)*tpp(j)                             !/*spx        50*/
  231   gpblcm(3+i)=gpblcm(3+i)+trf(ltrf+n+j,jt)*tpp(3+j)                       !/*spx        51*/
        jsu=0                                                                   !/*spx        52*/
        icl=0                                                                   !/*spx        53*/
        call findlv                                                             !/*spx        54*/
  241   continue                                                                !/*spx        55*/
      endif                                                                     !/*spx        56*/
      if(nsr.eq.71.and.ksdef.le.0)call sourck                                   !/*sp4a        5*/
!                                                                               !/*sp         62*/
!        determine the multigroup energy or group number.                       !/*sp         63*/
  110 if(mcal.eq.0)go to 150                                                    !/*sp         64*/
      l=jxs(ljxs+1,mgegbt(ipt))-1                                               !/*sp         65*/
      if(img.eq.0)go to 120                                                     !/*sp         66*/
      if(nsr.eq.6)go to 150                                                     !/*sp         67*/
      jgp=erg                                                                   !/*sp         68*/
      erg=xss(l+jgp)+(rang()-.5)*xss(l+jgp+jgm(1))                              !/*sp         69*/
      rm=xss(jxs(ljxs+1,mgegbt(1))+jgp-1+2*jgm(1))                              !/*sp         70*/
      vel=slite*sqrt(erg*(erg+2.*rm))/(erg+rm)                                  !/*sp         71*/
      go to 150                                                                 !/*sp         72*/
  120 do 130 jgp=1,jgm(ipt)-1                                                   !/*sp         73*/
  130 if(erg.gt.xss(l+jgp)-.5*xss(l+jgm(ipt)+jgp))go to 140                     !/*sp         74*/
  140 erg=xss(l+jgp)                                                            !/*sp         75*/
!                                                                               !/*sp         76*/
!        report, count, and resample the source if energy > emx.                !/*sp         77*/
  150 if(erg.le.emx(ipt))go to 160                                              !/*sp4a        6*/
      kl=1                                                                      !/*sp4a        7*/
      if(nsr.eq.6.or.nsr.eq.71)kl=0                                             !/*sp4a        8*/
      call errprn(kl,netb(1),4,erg,zero,'erg',' ',                              !/*sp4b       13*/
     & 'source energy > emax.')                                                 !/*sp4a       10*/
      if(netb(1).le.20.or.netb(1).le.2*npstc)go to 80                           !/*sp4a       11*/
      call expirx(0,'startp','too many cases of erg > emax.')                   !/*sp         82*/
      return                                                                    !/*sp         83*/
!                                                                               !/*sp4a       12*/
!        print the initial parameters for the first fifty histories.            !/*sp4a       13*/
!        facet of macrobody surface source not printed to save space.           !/*sp4c        9*/
  160 if(npstc.gt.50)go to 190                                                  !/*sp4a       14*/
      if(ink(110).eq.0)go to 190                                                !/*sp4a       15*/
      if(krflg.eq.2)go to 190                                                   !/*sp4a       16*/
#ifdef MULTT                                                                    /*!spxk        1*/
      call thread_lockon(jlock)                                                 !/*spxk        2*/
#endif                                                                          /*!spxk        3*/
      if(npstc.eq.1.and.nlat.eq.0)write(iuo,170)                                !/*sp4a       18*/
  170 format(//5x,3hnps,4x,1hx,10x,1hy,10x,1hz,7x,4hcell,3x,4hsurf,5x,          !/*sp4a       19*/
     & 1hu,10x,1hv,10x,1hw,8x,6henergy,5x,6hweight,6x,4htime)                   !/*sp4a       20*/
      if(npstc.eq.1.and.nlat.ne.0)write(iuo,175)                                !/*sp4a       21*/
  175 format(//4h nps,3x,1hx,10x,1hy,10x,1hz,7x,19hcell lattice[i j k],         !/*sp4b       14*/
     & 2x,7hsurface,4x,1hu,10x,1hv,10x,1hw,8x,6henergy,5x,6hweight,6x,          !/*sp4a       23*/
     & 4htime)                                                                  !/*sp4a       24*/
      if(npstc.le.nint(dbcn(4))+1.and.npstc.gt.nint(dbcn(3)))                   !/*sp4a       25*/
     & write(iuo,'(1h )')                                                       !/*sp4a       26*/
      i=0                                                                       !/*spx        58*/
      if(jsu.gt.0)i=nsf(lnsf+jsu)                                               !/*spx        59*/
      write(hs,'(i7)')i                                                         !/*spx        60*/
      if(i.gt.0.and.i.le.99999.and.kfq.gt.0)write(hs,'(i5,1h.,i1)')i,kfq        !/*spx        61*/
      write(ha,'(i2,95x,1p3e11.3)')npstc,erg,wgt,tme                            !/*sp4a       29*/
!                                                                               !/*sp4a       30*/
!        print the data for each level.                                         !/*sp4a       31*/
      do 185 l=0,lev                                                            !/*sp4a       32*/
      ic=icl                                                                    !/*sp4a       33*/
      if(l.eq.lev)go to 179                                                     !/*sp4a       34*/
      ic=nint(udt(7,l))                                                         !/*sp4a       35*/
      write(ha(3:97),180)(udt(i,l),i=1,3),ncl(lncl+ic),' ',                     !/*sp4a       36*/
     & (udt(i,l),i=4,6)                                                         !/*sp4a       37*/
      go to 181                                                                 !/*sp4a       38*/
  179 write(ha(3:97),180)xxx,yyy,zzz,ncl(lncl+ic),hs,uuu,vvv,www                !/*sp4a       39*/
  180 format(1p3e11.3,i6,16x,a7,3e11.3)                                         !/*sp4a       40*/
  181 if(nlat.eq.0)go to 184                                                    !/*sp4a       41*/
      if(lat(llat+1,ic).eq.0)go to 183                                          !/*sp4a       42*/
      write(ha(42:57),'(1h[,3x,1h*,4x,1h*,4x,2h*])')                            !/*sp4b       15*/
      do 182 i=1,3                                                              !/*sp4a       44*/
      j=jpblcm(10+i)                                                            !/*sp4a       45*/
      if(l.ne.lev)j=nint(udt(7+i,l))                                            !/*sp4a       46*/
  182 if(abs(j).lt.1000)write(ha(43+5*(i-1):46+5*(i-1)),'(i4)')j                !/*sp4a       47*/
  183 write(iuo,'(1x,a35,a94)')ha(1:35),ha(37:130)                              !/*sp4a       48*/
      go to 185                                                                 !/*sp4a       49*/
  184 write(iuo,'(5x,a41,a73)')ha(1:41),ha(58:130)                              !/*sp4a       50*/
  185 ha=' '                                                                    !/*sp4a       51*/
  190 if(krflg.ne.0)call eventp(1)                                              !/*sp4a       53*/
      if(iptr.ne.0)call ptrak(1)                                                !/*sp4a       54*/
      if(abs(iptr).eq.2)call ptrak(6)                                           !/*sp4b       16*/
#ifdef MULTT                                                                    /*!spxk        4*/
      call thread_lockoff(jlock)                                                !/*spxk        5*/
#endif                                                                          /*!spxk        6*/
!                                                                               !/*sp         93*/
!        quit if source energy, weight or importance is zero or less.           !/*sp         94*/
      if(erg.gt.0..and.wgt.gt.0..and.fim(lfim+ipt,icl).ne.0.)go to 200          !/*sp         97*/
      if(erg.le.0.)call expirx(1,'startp',                                      !/*sp         98*/
     & 'the energy of a source particle is zero or less.')                      !/*sp         99*/
      if(wgt.le.0.)call expirx(1,'startp',                                      !/*sp        100*/
     & 'the weight of a source particle is zero or less.')                      !/*sp        101*/
      if(fim(lfim+ipt,icl).eq.0.)call expirx(1,'startp',                        !/*sp        102*/
     & 'the source particle started in a cell of zero importance.')             !/*sp        103*/
      return                                                                    !/*sp        104*/
!                                                                               !/*sp        105*/
!        mark the particle if it is born inside a dxtran sphere.                !/*sp        106*/
  200 do 210 idx=1,ndx(ipt)                                                     !/*sp        107*/
  210 if((dxx(ipt,1,idx)-xxx)**2+(dxx(ipt,2,idx)-yyy)**2+                       !/*sp        108*/
     & (dxx(ipt,3,idx)-zzz)**2.lt.dxx(ipt,5,idx))go to 220                      !/*sp        109*/
      idx=0                                                                     !/*sp        110*/
!                                                                               !/*sp        111*/
!        increment source accounts and save some initial variables.             !/*sp        112*/
  220 call gain_pax (igain_all_pls_source, ipt, 1, wgt, wgt*erg)                !/*spx        63*/
      call contrib_pwb (ipt,ipwb_external_source,icl,wgt)                       !/*spx        64*/
      pac(kpac+ipt,1,icl)=pac(kpac+ipt,1,icl)+1.                                !/*sp        117*/
      pac(kpac+ipt,2,icl)=pac(kpac+ipt,2,icl)+1.                                !/*sp        118*/
      ipac2(kpc2+icl)=1                                                         !/*sp        119*/
      if(nmaz.ne.0)call rslmaz(1,1)                                             !/*sp4b       18*/
      if(nmaz.ne.0)call rslmaz(2,1)                                             !/*sp4b       19*/
      wtfasv=1.                                                                 !/*sp        120*/
      fismg=1.                                                                  !/*sp        121*/
      wgtstc(1)=min(wgtstc(1),wgt)                                              !/*sp        122*/
      wgtstc(2)=max(wgtstc(2),wgt)                                              !/*sp        123*/
      if(ipt.eq.1)smultc(4)=smultc(4)+wgt                                       !/*sp4c2      11*/
      rlttc(3,2)=rlttc(3,2)+wgt                                                 !/*sp4b       20*/
      do 230 i=1,nipt                                                           !/*spx        65*/
      elc(i)=elp(lelp+i,icl)                                                    !/*sp        126*/
      fiml(i)=fim(lfim+i,icl)                                                   !/*sp        127*/
      do 230 l=1,lev                                                            !/*sp        128*/
      elc(i)=max(elc(i),elp(lelp+i,int(udt(7,l-1))))                            !/*sp        129*/
  230 fiml(i)=fiml(i)*fim(lfim+i,int(udt(7,l-1)))                               !/*sp        130*/
      if(img.ne.0)go to 240                                                     !/*sp        131*/
      vel=slite                                                                 !/*sp        132*/
      if(gpt(ipt).ne.0.)                                                        !/*spx        66*/
     & vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                     !/*spx        67*/
  240 npb=1                                                                     !/*sp        134*/
      call savpar(1,0)                                                          !/*sp4c2      12*/
      if(iwwg.gt.0)call avrwwg(-one)                                            !/*sp4c       11*/
!                                                                               !/*sp        140*/
!        set up user bin indexes for special tally treatments.                  !/*sp        141*/
      if(ifft.eq.0)go to 300                                                    !/*sp        142*/
      do 290 i=1,ntal                                                           !/*sp        143*/
      l=lft(llft+6,i)                                                           !/*sp        144*/
      if(l.eq.0)go to 270                                                       !/*sp        145*/
      m=nint(tds(l+1))                                                          !/*sp4b       21*/
      k=max(0,nint(fdd(kfdd+1,m))-1+ksd(lksd+5,m))                              !/*sp4b       22*/
      go to 290                                                                 !/*sp        147*/
  270 l=lft(llft+7,i)                                                           !/*sp        148*/
      if(l.eq.0)go to 290                                                       !/*sp        149*/
      do 280 k=1,iptal(lipt+3,4,i)-1                                            !/*sp        150*/
      m=nint(tds(l+k))                                                          !/*sp4b       25*/
  280 if(fdd(kfdd+1,m).ne.0.)go to 290                                          !/*sp4b       27*/
      k=0                                                                       !/*sp        152*/
  290 jft(kjft+i)=k                                                             !/*sp        153*/
!                                                                               !/*sp        154*/
!        check source energy, time, and weight.                                 !/*sp4a       55*/
  300 if(mcal.lt.2.eqv.erg.le.elc(ipt))call errprn(1,nwse,4,erg,zero,           !/*sp4b       28*/
     & 'erg',' ','source energy less than energy cutoff.')                      !/*sp4a       57*/
      if(tme.ge.tco(ipt))call errprn(1,nwst,4,tme,zero,'tme',' ',               !/*sp4b       29*/
     & 'source time greater than time cutoff.')                                 !/*sp4a       59*/
      if(nww(ipt).eq.0.or.mcal.ne.0.and.iplt.ne.2)go to 420                     !/*sp4a       60*/
      t1=erg                                                                    !/*sp4c       12*/
      if(wwp(ipt,6).ne.0.)t1=tme                                                !/*sp4c       13*/
      ic=0                                                                      !/*sp        181*/
      ib=nww(ipt)                                                               !/*sp        182*/
  370 if(ib-ic.eq.1)go to 390                                                   !/*sp        183*/
      ih=(ic+ib)/2                                                              !/*sp        184*/
      if(t1.lt.wwe(lwwe+ih+mww(ipt)))go to 380                                  !/*sp4c       14*/
      ic=ih                                                                     !/*sp        186*/
      go to 370                                                                 !/*sp        187*/
  380 ib=ih                                                                     !/*sp        188*/
      go to 370                                                                 !/*sp        189*/
  390 ww=wwval(ipt,icl,ib,1,ix)                                                 !/*sp4c2      13*/
      if(ww.eq.0.)go to 420                                                     !/*sp        191*/
      if(wgt.ge.ww.and.wgt.le.wwp(ipt,1)*ww)go to 420                           !/*sp        192*/
      i=1                                                                       !/*sp        193*/
      if(wgt.lt.ww)i=2                                                          !/*sp        194*/
      if(i.eq.1)call errprn(1,nwsg(i),5,erg,wgt,'erg','wgt',                    !/*sp4b       31*/
     & 'weight of source particle is above window.')                            !/*sp4a       62*/
      if(i.eq.2)call errprn(1,nwsg(i),5,erg,wgt,'erg','wgt',                    !/*sp4b       32*/
     & 'weight of source particle is below window.')                            !/*sp4a       64*/
      nwws(i,ib)=nwws(i,ib)+1                                                   !/*sp4b       33*/
!                                                                               !/*sp        205*/
!        report and count cases where source weight is below cutoff.            !/*sp        206*/
  420 if(wgt.gt.wcs2tc(ipt))go to 460                                           !/*sp        207*/
      if(erg.le.emcf(ipt))go to 460                                             !/*sp        208*/
      if(nww(ipt).eq.0)go to 430                                                !/*sp        209*/
      if(wwval(ipt,icl,1,0,ix).ne.0.)go to 460                                  !/*sp4c2      14*/
  430 call errprn(1,nwsb,4,wgt,zero,'wgt',' ',                                  !/*sp4b       35*/
     & 'source particle weight is below cutoff.')                               !/*sp4a       66*/
!                                                                               !/*spxk        7*/
!        sample spontaneous fission if required.                                !/*spxk        8*/
  460 if(isfiss.ne.0)call spon_fis                                              !/*spxk        9*/
!                                                                               !/*sp        239*/
!        score tallies and create dxtran particles.                             !/*sp        240*/
      if(locph(llph+icl).ne.0)call talph(icl,erg)                               !/*spxk       10*/
      if(kdb.ne.0)return                                                        !/*sp        242*/
      if(ndet(ipt).ne.0)call tallyd                                             !/*sp        243*/
      if(kdb.ne.0)return                                                        !/*sp        244*/
      if(ndx(ipt).gt.1.or.ndx(ipt).eq.1.and.idx.eq.0)call dxtran                !/*sp        245*/
      if(kdb.ne.0)return                                                        !/*sp        246*/
#ifdef MESHTAL                                                                  /*!spx        69*/
      if (mspflg.ne.0) then                                                     !/*spx        70*/
         do 465 i=1,6                                                           !/*spx        71*/
            if(lev.eq.0) then                                                   !/*spx        72*/
               zd(i)=gpblcm(i)                                                  !/*spx        73*/
            else                                                                !/*spx        74*/
               zd(i)=udt(i,0)                                                   !/*spx        75*/
            endif                                                               !/*spx        76*/
  465       continue                                                            !/*spx        77*/
         zd(7)=wgt                                                              !/*spx        78*/
         zd(8)=erg                                                              !/*spx        79*/
         zd(9)=0.0                                                              !/*spx        80*/
         zd(10)=0.0                                                             !/*spx        81*/
         jz(1)=mat(lmat+icl)                                                    !/*spx        82*/
         jz(2)=mspf                                                             !/*spx        83*/
         call gdtal(1,zero,zd,jz)                                               !/*spx        84*/
      endif                                                                     !/*spx        85*/
#endif                                                                          /*!spx        86*/
!                                                                               !/*sp        247*/
!        play the weight-window game.                                           !/*sp        248*/
      if(wwp(ipt,4).ne.0.)call wtwndo(3)                                        !/*sp4c       16*/
      return                                                                    !/*sp        250*/
      end                                                                       !/*sp        251*/

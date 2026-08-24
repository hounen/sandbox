!#ident sursrc.F from deck rs                                                   !/*rs          1*/
      subroutine sursrc(ns)                                                     !/*rs4a        1*/
!        read from the rssa surface-source file.                                !/*rs          3*/
!        multitasking lock is left on by trnspt for sursrc.                     !/*rs          4*/
      use global_data                                                           !/*rsx         1*/
!                                                                               !/*rsx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rsx         3*/
      implicit integer(kindi) (i-n)                                             !/*rsx         4*/
!                                                                               !/*rsx         5*/
!                                                                               !/*rs          6*/
      nr=0                                                                      !/*rs          7*/
      nt=0                                                                      !/*rs          8*/
      n2=0                                                                      !/*rs          9*/
      jm=1                                                                      !/*rs         10*/
      jf=0                                                                      !/*rs         11*/
      jq=0                                                                      !/*rsxc        1*/
      jt=srv(1,5)                                                               !/*rs         12*/
!                                                                               !/*rs         13*/
!        read all of the tracks for the current history from rssa               !/*rs         14*/
!        and bank all but the first one.                                        !/*rs         15*/
   10 npsr=abs(ssb(1))                                                          !/*rs         16*/
      npsrtc=npsr                                                               !/*rs         17*/
      nrrs=nrrs+1                                                               !/*rs         18*/
      twss=twss+ssb(3)                                                          !/*rs         19*/
      if(n2.ge.0)go to 20                                                       !/*rs         20*/
      nssi(8)=nssi(8)+1                                                         !/*rs         21*/
      go to 340                                                                 !/*rs         22*/
   20 npa=1                                                                     !/*rs         23*/
      na=1                                                                      !/*rs         24*/
      ncp=1                                                                     !/*rs         25*/
      if(ssb(1).lt.0.)ncp=0                                                     !/*rs         26*/
#ifdef MESHTAL                                                                  /*!rsx         6*/
      mspf=3                                                                    !/*rsx         7*/
#endif                                                                          /*!rsx         8*/
      nx=abs(ssb(2))                                                            !/*rs         27*/
      kq=(abs(ssb(2))+.001-nx)*10.                                              !/*rs4c2       1*/
      wgt=ssb(3)                                                                !/*rs         28*/
      erg=ssb(4)                                                                !/*rs         29*/
      tme=ssb(5)                                                                !/*rs         30*/
      lev=0                                                                     !/*rs4b        1*/
      po=ssb(6)                                                                 !/*rs         31*/
      if(nrcd.eq.6)go to 40                                                     !/*rs         32*/
      do 30 i=1,5                                                               !/*rs         33*/
   30 gpblcm(i)=ssb(5+i)                                                        !/*rs         34*/
      www=sign(sqrt(max(zero,1.-uuu**2-vvv**2)),ssb(2))                         !/*rs         35*/
   40 i=nx/1000000                                                              !/*rs         36*/
      jn=nx-1000000*i                                                           !/*rs         37*/
      if(img.eq.0)ipt=i                                                         !/*rs         38*/
      if(img.ne.0)jgp=i                                                         !/*rs         39*/
      jc=0                                                                      !/*rs         40*/
      if(img.ne.0.or.i.lt.100)go to 50                                          !/*rs         41*/
      jgp=-i/200                                                                !/*rs         42*/
      jc=i/100+2*jgp                                                            !/*rs         43*/
      ipt=i-100*jc+200*jgp                                                      !/*rs         44*/
!                                                                               !/*rs         45*/
!        reject by particle type.                                               !/*rs         46*/
   50 jan = not_self_anti(ipt)                                                  !/*rsxc        2*/
      if(kpt(ipt).ne.0)go to 60                                                 !/*rsxc        3*/
      nssi(1)=nssi(1)+1                                                         !/*rs         48*/
      wssi(1)=wssi(1)+wgt                                                       !/*rs         49*/
      go to 340                                                                 !/*rs         50*/
!                                                                               !/*rs         51*/
!        reject particle as collided or not.                                    !/*rs         52*/
   60 if(srv(1,4).eq.0.)go to 70                                                !/*rs         53*/
      if(srv(1,4).lt.0..eqv.ncp.eq.0)go to 70                                   !/*rs         54*/
      nssi(2)=nssi(2)+1                                                         !/*rs         55*/
      wssi(2)=wssi(2)+wgt                                                       !/*rs         56*/
      go to 340                                                                 !/*rs         57*/
!                                                                               !/*rs         58*/
!        reject particles by surface or cell number.                            !/*rs         59*/
   70 do 80 ji=1+njsr(1)*jc,njsr(1)+nilr(1)*jc                                  !/*rs4c        1*/
!        check surface facet.                                                   !/*rs4c2       2*/
      if(jc.eq.0.and.kq.ne.jasr(ljar+2,ji))go to 80                             !/*rs4c2       3*/
      if(jn.eq.jasr(ljar+1,ji))go to 90                                         !/*rs4c2       4*/
   80 continue                                                                  !/*rs4c2       5*/
      nssi(3)=nssi(3)+1                                                         !/*rs         62*/
      wssi(3)=wssi(3)+wgt                                                       !/*rs         63*/
      go to 340                                                                 !/*rs         64*/
!                                                                               !/*rs         65*/
!        reject particles by energy cutoff.                                     !/*rs         66*/
   90 if(erg.gt.ecf(ipt))go to 100                                              !/*rs         67*/
      nssi(5)=nssi(5)+1                                                         !/*rs         68*/
      wssi(5)=wssi(5)+wgt                                                       !/*rs         69*/
      go to 340                                                                 !/*rs         70*/
!                                                                               !/*rs         71*/
!       reject particles by time cutoff.                                        !/*rs         72*/
  100 if(tme.lt.tco(ipt))go to 110                                              !/*rs         73*/
      nssi(6)=nssi(6)+1                                                         !/*rs         74*/
      wssi(6)=wssi(6)+wgt                                                       !/*rs         75*/
      go to 340                                                                 !/*rs         76*/
!                                                                               !/*rs         77*/
!        reject particles by angle.                                             !/*rs         78*/
  110 if(nrcd.ne.6)go to 120                                                    !/*rs         79*/
      if(po.gt.srv(1,6))go to 120                                               !/*rs         80*/
      nssi(7)=nssi(7)+1                                                         !/*rs         81*/
      wssi(7)=wssi(7)+wgt                                                       !/*rs         82*/
      go to 340                                                                 !/*rs         83*/
!                                                                               !/*rs         84*/
!        do splitting or russian roulette on snit = npp/np1.                    !/*rs         85*/
  120 if(snit.eq.1.)go to 160                                                   !/*rs         86*/
      if(snit.lt.1.)go to 140                                                   !/*rs         87*/
      if(n2.eq.npsr)go to 130                                                   !/*rs         88*/
      n2=npsr                                                                   !/*rs         89*/
      ni=snit                                                                   !/*rs         90*/
      if(rang().lt.(snit-ni))ni=ni+1                                            !/*rs         91*/
  130 npa=ni                                                                    !/*rs         92*/
      wgt=wgt/ni                                                                !/*rs         93*/
      if(nrcd.ne.6)go to 160                                                    !/*rs         94*/
      na=npa                                                                    !/*rs         95*/
      npa=1                                                                     !/*rs         96*/
      go to 160                                                                 !/*rs         97*/
  140 if(n2.eq.npsr)go to 150                                                   !/*rs         98*/
      n2=npsr                                                                   !/*rs         99*/
      if(rang().le.snit)go to 150                                               !/*rs        100*/
      n2=-1                                                                     !/*rs        101*/
      nssi(8)=nssi(8)+1                                                         !/*rs        102*/
      go to 340                                                                 !/*rs        103*/
  150 wgt=wgt/snit                                                              !/*rs        104*/
  160 wgt=wgt*srv(1,17)                                                         !/*rs        105*/
      wt=wgt                                                                    !/*rs        106*/
!                                                                               !/*rs        107*/
!        sample from a spherically-symmetric surface source.                    !/*rs        108*/
  170 na=na-1                                                                   !/*rs        109*/
      if(nrcd.eq.6)call ssmsrc(po,jf)                                           !/*rs        110*/
      if(kdb.ne.0)return                                                        !/*rs        111*/
      if(jf.ne.0)go to 330                                                      !/*rs        112*/
!                                                                               !/*rs        113*/
!        sample occurence.                                                      !/*rs        114*/
      if(imt.eq.1)go to 210                                                     !/*rs        115*/
      if(nr.eq.npsr)go to 200                                                   !/*rs        116*/
      nr=npsr                                                                   !/*rs        117*/
      kd=ksd(lksd+13,ivdis(5))                                                  !/*rs        118*/
      r=rang()                                                                  !/*rs        119*/
      do 180 jm=1,imt-1                                                         !/*rs        120*/
  180 if(r.le.spf(kd+2,jm))go to 190                                            !/*rs        121*/
  190 w1=spf(kd+3,jm)                                                           !/*rs        122*/
      jt=spf(ksd(lksd+13,ivdis(5))+1,jm)                                        !/*rs        123*/
      if(njsx(1).ne.imt*njsr(1))jq=1                                            !/*rsxc        4*/
  200 wgt=wgt*w1                                                                !/*rs        124*/
!                                                                               !/*rs        125*/
!        do the transformation and define the surface number.                   !/*rs        126*/
  210 if(jt.eq.0)go to 240                                                      !/*rs        127*/
      do 220 i=1,6                                                              !/*rs        128*/
  220 tpp(i)=gpblcm(i)                                                          !/*rs        129*/
      do 230 i=1,3                                                              !/*rs        130*/
      gpblcm(i)=trf(ltrf+14+i,jt)                                               !/*rs        131*/
      gpblcm(3+i)=0.                                                            !/*rs        132*/
      n=4+3*(i-1)                                                               !/*rs        133*/
      do 230 j=1,3                                                              !/*rs        134*/
      gpblcm(i)=gpblcm(i)+trf(ltrf+n+j,jt)*tpp(j)                               !/*rs        135*/
  230 gpblcm(3+i)=gpblcm(3+i)+trf(ltrf+n+j,jt)*tpp(3+j)                         !/*rs        136*/
  240 jsu=0                                                                     !/*rs        137*/
      if(jc.eq.0.and.jq.eq.0)jsu=iss(liss+1,(jm-1)*njsr(1)+ji)                  !/*rsxc        5*/
      icl=0                                                                     !/*cor4-2    189*/
      call findlv                                                               !/*cor4-2    190*/
!                                                                               !/*rs        152*/
!        increment the summary accounts.                                        !/*rs        153*/
      vel=slite                                                                 !/*cor4-2    191*/
      if(gpt(ipt).ne.0.)                                                        !/*rsx         9*/
     & vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                     !/*rsx        10*/
      twac=twac+wgt*npa                                                         !/*rs        156*/
      if(nt.eq.0)npa=npa-1                                                      !/*rs        157*/
#ifdef MULTP                                                                    /*!rs4a        2*/
      if(ns.ne.0)go to 250                                                      !/*rs4a        3*/
#endif                                                                          /*!cpp       265*/
      call gain_pax (igain_all_pls_source, ipt,                                 !/*rsx        11*/
     &               npa, npa*wgt, npa*wgt*erg)                                 !/*rsx        12*/
      if(ipt.eq.1)smultc(4)=smultc(4)+wgt*npa                                   !/*rs4c2       6*/
      call contrib_pwb (ipt,ipwb_external_source,icl,npa*wgt)                   !/*rsx        13*/
      pac(kpac+ipt,1,icl)=pac(kpac+ipt,1,icl)+npa                               !/*rs        162*/
      if(nmaz.eq.0.or.npa.eq.0)go to 245                                        !/*rs4b        2*/
      call rslmaz(1,npa)                                                        !/*rs4b        3*/
      call rslmaz(2,npa)                                                        !/*rs4b        4*/
  245 wtfasv=1.                                                                 !/*rs4b        5*/
      fismg=1.                                                                  !/*rs        164*/
!                                                                               !/*rs        165*/
!        save the first accepted particle to start the history with.            !/*rs        166*/
#ifdef MULTP                                                                    /*!rs4a        4*/
  250 if(ns.ne.0.and.nt.ne.0)go to 330                                          !/*rs4a        5*/
#endif                                                                          /*!cpp       266*/
      if(nt.ne.0)go to 310                                                      !/*rs        167*/
      nt=1                                                                      !/*rs        168*/
      ntss=ntss+1                                                               !/*rs        169*/
#ifdef MULTP                                                                    /*!rs4a        6*/
      if(ns.ne.0)go to 330                                                      !/*rs4a        7*/
#endif                                                                          /*!cpp       267*/
      npb=1                                                                     !/*rs4a        8*/
      call savpar(1,0)                                                          !/*rs4c2       7*/
!                                                                               !/*rs        174*/
!        bank all the rest of them.                                             !/*rs        175*/
      if(npa.le.0)go to 330                                                     !/*rs        176*/
  310 jsu=-jsu                                                                  !/*rs        177*/
      do 320 i=1,nipt                                                           !/*rsx        14*/
      elc(i)=elp(lelp+i,icl)                                                    !/*rs        179*/
      fiml(i)=fim(lfim+i,icl)                                                   !/*rs        180*/
      do 320 l=1,lev                                                            !/*rs        181*/
      elc(i)=max(elc(i),elp(lelp+i,int(udt(7,l-1))))                            !/*rs        182*/
  320 fiml(i)=fiml(i)*fim(lfim+i,int(udt(7,l-1)))                               !/*rs        183*/
      if(jsu.eq.0)jsu=-1234567                                                  !/*rs4b        6*/
      call bankit(0)                                                            !/*rs4a        9*/
  330 npa=1                                                                     !/*rs        185*/
      wgt=wt                                                                    !/*rs        186*/
      if(na.ne.0)go to 170                                                      !/*rs        187*/
!                                                                               !/*rs        188*/
!        read the next track.                                                   !/*rs        189*/
  340 if(nrrs.ge.nrss)go to 350                                                 !/*rs        190*/
      read(iusr,end=410)(ssb(i),i=1,nrcd)                                       !/*rs        191*/
      if(npsr.eq.abs(ssb(1)))go to 10                                           !/*rs        192*/
!                                                                               !/*rs        193*/
!        all tracks associated with this history have been read.                !/*rs        194*/
      nqss=nqss+1                                                               !/*rs        195*/
      n2=0                                                                      !/*rs        196*/
      if(nt.eq.0)go to 10                                                       !/*rs        197*/
      go to 380                                                                 !/*rs        198*/
!                                                                               !/*rs        199*/
!        terminate the problem when all tracks are read.                        !/*rs        200*/
  350 nqss=nqss+1                                                               !/*rs        201*/
      if(twss.gt.0.)go to 360                                                   !/*rs        202*/
      call expirx(0,'sursrc','no weight is on the '//rssa//' file.')            !/*rs        203*/
      return                                                                    !/*rs        204*/
  360 if(ntss.gt.0)go to 370                                                    !/*rs        205*/
      call expirx(0,'sursrc',                                                   !/*rs        206*/
     & 'no particles accepted from the '//rssa//' file.')                       !/*rs        207*/
      return                                                                    !/*rs        208*/
  370 if(nt.ne.0)go to 380                                                      !/*rs        209*/
      nomore=1                                                                  !/*rs        210*/
      nps=nps-1                                                                 !/*rs        211*/
!                                                                               !/*rs        212*/
!        retrieve the starting track and return.                                !/*rs        213*/
  380 call getpar(1,0)                                                          !/*rs4c2       8*/
      ipsc=12                                                                   !/*rs        218*/
      return                                                                    !/*rs        219*/
!                                                                               !/*rs        220*/
!        quit if the rssa file is bad.                                          !/*rs        221*/
  410 call expirx(0,'sursrc','unexpected eof on the '//rssa//' file.')          !/*rs        222*/
      return                                                                    !/*rs        223*/
      end                                                                       !/*rs        224*/

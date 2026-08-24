!#ident inpert.F from deck iv4b                                                 !/*iv4b        1*/
      subroutine inpert                                                         !/*iv4b        2*/
!        process the perturbation input and initialize the arrays.              !/*iv4b        3*/
      use global_data                                                           !/*ivx         1*/
      use imcn_data                                                             !/*ivx         2*/
!                                                                               !/*ivx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ivx         4*/
      implicit integer(kindi) (i-n)                                             !/*ivx         5*/
!                                                                               !/*ivx         6*/
!                                                                               !/*iv4b        6*/
!        increase the tally storage and loop over all perturbations.            !/*iv4b        7*/
      if(mnnm.le.0)go to 190                                                    !/*iv4b        8*/
      mxfp=mxf                                                                  !/*iv4b        9*/
      mxf=mxf*(npert+1)                                                         !/*iv4b       10*/
      nz=0                                                                      !/*iv4b       11*/
      nptb(lnpt+1)=0                                                            !/*iv4b       12*/
      do 180 ip=1,npert                                                         !/*iv4b       13*/
      np=iptb(lipb+1,ip)                                                        !/*iv4b       14*/
      k=kpptb(lpptb+1,ip)                                                       !/*ivx         7*/
      if(k.eq.0.and.kpptb(lpptb+2,ip).ne.0)k=2                                  !/*ivx         8*/
      nptb(lnpt+ip+1)=nptb(lnpt+ip)                                             !/*iv4b       17*/
!                                                                               !/*iv4b       18*/
!        set iptb(lipb+13,ip)=method, set d1, d2 according to method,           !/*iv4c        1*/
!        and iptb(lipb+14,ip)=0/1 according to sign of method.                  !/*iv4c        2*/
      d1=1.                                                                     !/*iv4c        3*/
      d2=1.                                                                     !/*iv4c        4*/
      t=1.                                                                      !/*iv4c        5*/
      if(iptb(lipb+13,ip).ne.0.)t=rptb(lrpt+iptb(lipb+14,ip))                   !/*iv4c        6*/
      iptb(lipb+14,ip)=0                                                        !/*iv4c        7*/
      if(t.lt.0.)iptb(lipb+14,ip)=1                                             !/*iv4c        8*/
      iptb(lipb+13,ip)=nint(abs(t))                                             !/*iv4c        9*/
      if(iptb(lipb+13,ip).eq.2)d2=0.                                            !/*iv4c       10*/
      if(iptb(lipb+13,ip).eq.3)d1=0.                                            !/*iv4c       11*/
!                                                                               !/*iv4b       26*/
!        check that all cells contain the same material.                        !/*iv4b       27*/
      mt=0                                                                      !/*iv4c       12*/
      rt=0.                                                                     !/*iv4b       29*/
      do 20 i=1,iptb(lipb+3,ip)                                                 !/*iv4b       30*/
      ic=nint(rptb(lrpt+iptb(lipb+4,ip)+i-1))                                   !/*iv4b       31*/
      m=mat(lmat+ic)                                                            !/*iv4b       32*/
      r=rho(lrho+ic)                                                            !/*iv4b       33*/
      if(mt.eq.0.and.m.ne.0)mt=m                                                !/*iv4b       34*/
      if(rt.eq.0..and.r.ne.0.)rt=r                                              !/*iv4b       35*/
   20 if(mt.ne.m.or.rt.ne.r)go to 140                                           !/*iv4b       36*/
      if(mt.eq.0)go to 150                                                      !/*iv4b       37*/
!                                                                               !/*iv4b       38*/
!        if mat keyword used, find the perturbed material index.                !/*iv4b       39*/
      mx=mt                                                                     !/*iv4b       40*/
      if(iptb(lipb+5,ip).eq.0)go to 70                                          !/*iv4b       41*/
      m=nint(rptb(lrpt+iptb(lipb+6,ip)))                                        !/*iv4b       42*/
      do 30 mx=1,nmat                                                           !/*iv4b       43*/
   30 if(nmt(lnmt+mx).eq.m)go to 40                                             !/*iv4b       44*/
      go to 160                                                                 !/*iv4b       45*/
!                                                                               !/*iv4b       46*/
!        check if perturbed material contains new elements.                     !/*iv4b       47*/
   40 do 60 m=jmd(ljmd+mx),jmd(ljmd+mx+1)-1                                     !/*iv4b       48*/
      do 50 n=jmd(ljmd+mt),jmd(ljmd+mt+1)-1                                     !/*iv4b       49*/
   50 if(lme(llme+k,n).eq.lme(llme+k,m))go to 60                                !/*iv4b       50*/
      go to 170                                                                 !/*iv4b       51*/
   60 continue                                                                  !/*iv4b       52*/
!                                                                               !/*iv4b       53*/
!        if rho keyword used, convert mass density to atom density.             !/*iv4b       54*/
   70 rx=rt                                                                     !/*iv4b       55*/
      if(iptb(lipb+7,ip).eq.0)go to 90                                          !/*iv4b       56*/
      rx=rptb(lrpt+iptb(lipb+8,ip))                                             !/*iv4b       57*/
      if(rx.ge.0.)go to 90                                                      !/*iv4b       58*/
      sw=0.                                                                     !/*iv4b       59*/
      do 80 m=jmd(ljmd+mx),jmd(ljmd+mx+1)-1                                     !/*iv4b       60*/
   80 sw=sw+fme(lfme+m)*awc(lawc+m)                                             !/*iv4b       61*/
      rx=-rx*avgdn/sw                                                           !/*iv4b       62*/
!                                                                               !/*iv4b       63*/
!        find delta rho - loop over elements of unperturbed material.           !/*iv4b       64*/
   90 dr=(rx-rt)/rt                                                             !/*iv4c       13*/
      do 130 m=jmd(ljmd+mt),jmd(ljmd+mt+1)-1                                    !/*iv4b       66*/
      if(iptb(lipb+5,ip).eq.0)go to 120                                         !/*iv4b       67*/
!                                                                               !/*iv4c       14*/
!        if unperturbed material has duplicate nuclides,                        !/*iv4c       15*/
!        sum the first time, skip subsequent times.                             !/*iv4c       16*/
      fu=0.                                                                     !/*iv4c       17*/
      do 100 m1=jmd(ljmd+mt),jmd(ljmd+mt+1)-1                                   !/*iv4c       18*/
      if(lme(llme+k,m1).ne.lme(llme+k,m))go to 100                              !/*iv4c       19*/
      if(m1.lt.m)go to 130                                                      !/*iv4c       20*/
      fu=fu+fme(lfme+m1)                                                        !/*iv4c       21*/
  100 continue                                                                  !/*iv4c       22*/
!                                                                               !/*iv4c       23*/
!        sum over perturbed materials.                                          !/*iv4c       24*/
      fp=0.                                                                     !/*iv4c       25*/
      do 110 n=jmd(ljmd+mx),jmd(ljmd+mx+1)-1                                    !/*iv4c       26*/
  110 if(lme(llme+k,n).eq.lme(llme+k,m))fp=fp+fme(lfme+n)                       !/*iv4c       27*/
      dr=(rx*fp-rt*fu)/(rt*fu)                                                  !/*iv4c       28*/
!                                                                               !/*iv4c       29*/
!        store nonzero delta rho.                                               !/*iv4c       30*/
  120 if(abs(dr).lt.1.e-5)go to 130                                             !/*iv4c       31*/
      nz=nz+1                                                                   !/*iv4b       74*/
      if(nz.gt.npert*mnnm)go to 200                                             !/*iv4b       75*/
      dptb(ldpt+1,nz)=m                                                         !/*iv4b       76*/
      dptb(ldpt+2,nz)=d1*dr                                                     !/*iv4b       77*/
      dptb(ldpt+3,nz)=d2*dr                                                     !/*iv4b       78*/
  130 continue                                                                  !/*iv4b       79*/
      nptb(lnpt+ip+1)=nz                                                        !/*iv4b       80*/
      if(nptb(lnpt+ip+1).eq.nptb(lnpt+ip))call erprnt(1,2,1,np,0,0,0,0,         !/*iv4b       81*/
     & '13hperturbation ,i5,39h disabled - all density changes < 1e-6.')        !/*iv4b       82*/
!                                                                               !/*iv4c       32*/
!        check perturbation mt reaction numbers (rxn).                          !/*iv4c       33*/
      nr=iptb(lipb+9,ip)                                                        !/*iv4c       34*/
      if(nr.eq.0)go to 180                                                      !/*iv4c       35*/
      if(iptb(lipb+2,ip).gt.1)call erprnt(1,1,1,iptb(lipb+1,ip),0,0,0,0,        !/*ivx         9*/
     & '52hrxn option and n,p option incompatible, perturbation,i5')            !/*iv4c       37*/
      do 138 mi=1,nr                                                            !/*iv4c       38*/
      mp=nint(rptb(lrpt+iptb(lipb+10,ip)+mi-1))                                 !/*iv4c       39*/
!        neutrons and multigroup.                                               !/*iv4c       40*/
      if(k.eq.2.and.mcal.eq.0)go to 132                                         !/*iv4c       41*/
      if(abs(mp).le.1)go to 134                                                 !/*iv4c       42*/
      if(mcal.eq.0.and.mp.lt.-3.and.mp.ne.-6)go to 136                          !/*iv4c       43*/
      if(mcal.ne.0.and.mp.lt.-2.and.mp.ne.-5)go to 136                          !/*iv4c       44*/
      go to 138                                                                 !/*iv4c       45*/
!        photons.                                                               !/*iv4c       46*/
  132 if(mp.eq.-5.or.mp.eq.501)go to 134                                        !/*iv4c       47*/
      if(mp.ge.-4.and.mp.lt.0)go to 138                                         !/*iv4c       48*/
      if(mp.eq.504.or.mp.eq.502.or.mp.eq.522.or.mp.eq.516)go to 138             !/*iv4c       49*/
      go to 136                                                                 !/*iv4c       50*/
!        rxn=mp=total:  no further need for separate reactions.                 !/*iv4c       51*/
  134 iptb(lipb+9,ip)=0                                                         !/*iv4c       52*/
      go to 180                                                                 !/*iv4c       53*/
  136 call erprnt(1,1,2,mp,iptb(lipb+1,ip),0,0,0,                               !/*iv4c       54*/
     & '8hreaction,i4,24h illegal in perturbation,i5')                          !/*iv4c       55*/
  138 continue                                                                  !/*iv4c       56*/
      go to 180                                                                 !/*iv4b       83*/
!                                                                               !/*iv4b       84*/
!        print an error message.                                                !/*iv4b       85*/
  140 call erprnt(1,1,1,np,0,0,0,0,                                             !/*iv4b       86*/
     & '42hcells contain different material for pert ,i5,1h.')                  !/*iv4b       87*/
      go to 180                                                                 !/*iv4b       88*/
  150 call erprnt(1,1,1,np,0,0,0,0,                                             !/*iv4b       89*/
     & '46hremoval of void not possible for perturbation ,i5,1h.')              !/*iv4b       90*/
      go to 180                                                                 !/*iv4b       91*/
  160 call erprnt(1,1,2,m,np,0,0,0,                                             !/*iv4b       92*/
     & '9hmaterial ,i5,26h invalid for perturbation ,i5,1h.')                   !/*iv4b       93*/
      go to 180                                                                 !/*iv4b       94*/
  170 call erprnt(1,1,4,iza(liza+m),nmt(lnmt+mx),np,nmt(lnmt+mt),0,             !/*iv4b       95*/
     & 'i6,8h in mat ,i5,9h of pert ,i5,16h must be in mat ,i5,1h.')            !/*iv4b       96*/
  180 continue                                                                  !/*iv4b       97*/
!                                                                               !/*iv4b       98*/
      if(ntal.gt.0.or.nsr.eq.71)go to 220                                       !/*iv4c       57*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*iv4b      100*/
     & '47hperturbation disabled due to a lack of tallies.')                    !/*iv4b      101*/
      go to 210                                                                 !/*iv4b      102*/
  190 call erprnt(1,2,0,0,0,0,0,0,                                              !/*iv4b      103*/
     & '49hperturbation disabled due to a lack of materials.')                  !/*iv4b      104*/
      go to 210                                                                 !/*iv4b      105*/
  200 call erprnt(1,1,0,0,0,0,0,0,                                              !/*iv4b      106*/
     & '50hinternal error - inadequate storage for ptb array.')                 !/*iv4b      107*/
  210 npert=0                                                                   !/*iv4b      108*/
      nptb(lnpt+1)=0                                                            !/*iv4b      109*/
  220 do 230 ip=1,mipt                                                          !/*iv4b      110*/
  230 if(ndx(ip).ne.0)call erprnt(1,1,0,0,0,0,0,0,                              !/*iv4b      111*/
     & '48hperturbation capability and dxtran incompatible.')                   !/*iv4b      112*/
!                                                                               !/*iv4c       58*/
!        warn about correction of heating tallies.                              !/*iv4c       59*/
      do 340 ital=1,ntal                                                        !/*iv4c       60*/
      if(jptal(ljpt+2,ital).ne.6.and.jptal(ljpt+2,ital).ne.7)go to 340          !/*iv4c       61*/
      jf=0                                                                      !/*iv4c       62*/
      do 320 ipt=1,2                                                            !/*iv4c       63*/
      do 310 ic=1,mxa                                                           !/*iv4c       64*/
      if(locct(llct+ipt,ic).eq.0)go to 310                                      !/*iv4c       65*/
      mf=0                                                                      !/*iv4c       66*/
      nf=0                                                                      !/*iv4c       67*/
      do 250 ip=1,npert                                                         !/*iv4c       68*/
      do 240 i=1,iptb(lipb+3,ip)                                                !/*iv4c       69*/
  240 if(nint(rptb(lrpt+iptb(lipb+4,ip)+i-1)).eq.ic)nf=1                        !/*iv4c       70*/
  250 if(iptb(lipb+5,ip).ne.0)mf=ip                                             !/*iv4c       71*/
      if(nf.eq.0)go to 310                                                      !/*iv4c       72*/
!                                                                               !/*iv4c       73*/
!        tally cell ic is perturbed.                                            !/*iv4c       74*/
      lo=locct(llct+ipt,ic)                                                     !/*iv4c       75*/
      lx=lo+2                                                                   !/*iv4c       76*/
      do 300 ml=1,itds(lo)                                                      !/*iv4c       77*/
      n1=itds(lx)                                                               !/*iv4c       78*/
      if(itds(lx-1).ne.ital)go to 300                                           !/*iv4c       79*/
!                                                                               !/*iv4c       80*/
!        perturbed cell ic is in heating tally ital.                            !/*iv4c       81*/
      kf=1                                                                      !/*iv4c       82*/
      if(iptal(lipt+4,2,ital).eq.0)go to 290                                    !/*iv4c       83*/
      kf=0                                                                      !/*iv4c       84*/
!                                                                               !/*iv4c       85*/
!        loop over tally ital bins involving cell ic.                           !/*iv4c       86*/
      do 280 ir=1,itds(lx)                                                      !/*iv4c       87*/
      ib=itds(lx+ir)                                                            !/*iv4c       88*/
!                                                                               !/*iv4c       89*/
!        loop over segment bins of tally bin ir.                                !/*iv4c       90*/
      do 270 is=1,iptal(lipt+4,3,ital)                                          !/*iv4c       91*/
      kr=1                                                                      !/*iv4c       92*/
      sd=tds(iptal(lipt+4,2,ital)+iptal(lipt+4,3,ital)*(ib-1)+is)               !/*iv4c       93*/
      if(abs(1.-sd).le.1.e-4)kr=2                                               !/*iv4c       94*/
      if(kf.eq.0)kf=kr                                                          !/*iv4c       95*/
  270 if(kr.ne.kf)kf=3                                                          !/*iv4c       96*/
  280 continue                                                                  !/*iv4c       97*/
!                                                                               !/*iv4c       98*/
!        set flags = 1/2/3 for sd.ne.1, sd=1, sd=inconsistent.                  !/*iv4c       99*/
  290 if(jf.eq.0)jf=kf                                                          !/*iv4c      100*/
      if(jf.ne.kf)jf=3                                                          !/*iv4c      101*/
      if(jf.eq.3)go to 330                                                      !/*iv4c      102*/
  300 lx=lx+n1+2                                                                !/*iv4c      103*/
  310 continue                                                                  !/*iv4c      104*/
  320 continue                                                                  !/*iv4c      105*/
  330 if(jf.eq.1)call erprnt(1,2,1,jptal(ljpt+1,ital),0,0,0,0,                  !/*iv4c      106*/
     & '44hperturbation correction not applied to tally,i5')                    !/*iv4c      107*/
      if(jf.eq.2)call erprnt(1,2,1,jptal(ljpt+1,ital),0,0,0,0,                  !/*iv4c      108*/
     & '40hperturbation correction applied to tally,i5')                        !/*iv4c      109*/
      if(jf.eq.3)call erprnt(1,1,1,jptal(ljpt+1,ital),0,0,0,0,                  !/*iv4c      110*/
     & '27hpert/sd inconsistent, tally,i5,5x,14hanswers wrong.')                !/*iv4c      111*/
      if((jf.eq.1.or.jf.eq.3).and.mf.ne.0)                                      !/*iv4c      112*/
     & call erprnt(1,1,2,iptb(lipb+1,mf),jptal(ljpt+1,ital),0,0,0,              !/*iv4c      113*/
     & '19hmat keyword of pert,i5,24h requires sd 1 for tally,i5')              !/*iv4c      114*/
  340 continue                                                                  !/*iv4c      115*/
      return                                                                    !/*iv4b      113*/
      end                                                                       !/*iv4b      114*/

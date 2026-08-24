!#ident acecas.F from deck as                                                   !/*as          1*/
      subroutine acecas(ls,ip,q,ia,ka,id,kd)                                    !/*as4c        1*/
!        sample the emission energy and scattering angle from the               !/*as4c        2*/
!        appropriate law data in the laboratory coordinate system.              !/*as4c        3*/
!                                                                               !/*as4c        4*/
!        ls - the current particle index in the colout array                    !/*as4c        5*/
!        ip - the ipt particle type for the incident particle                   !/*as4c        6*/
!         q - the q value for the reaction being sampled                        !/*as4c        7*/
!        ia - the first word of the relevant AND block in the xss array         !/*as4c        8*/
!        ka - the offset to the first word of the table in AND block            !/*as4c        9*/
!        id - the first word of the relevant DLW block in the xss array         !/*as4c       10*/
!        kd - the offset to the first word of the table in DLW block            !/*as4c       11*/
!                                                                               !/*as4c       12*/
!        returns the sample emission parameters in the lab system:              !/*as4c       13*/
!          colout(1,ls) - the emission energy                                   !/*as4c       14*/
!          colout(2,ls) - the emission scattering angle                         !/*as4c       15*/
!                                                                               !/*as4c       16*/
!        Law 4/44/61 makes use of a biased distribution which                   !/*as4c       17*/
!        can affect the outgoing particle weight, wgt.                          !/*as4c       18*/
!                                                                               !/*as4c       19*/
!        ipsc, kdb, and tpd also modified.                                      !/*as4c       20*/
      use global_data                                                           !/*asx         1*/
!                                                                               !/*asx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*asx         3*/
      implicit integer(kindi) (i-n)                                             !/*asx         4*/
!                                                                               !/*asx         5*/
      parameter (ep=0.000001)                                                   !/*as4c       22*/
      character ht*10                                                           !/*as4c       23*/
!                                                                               !/*as4c       24*/
!***********************************************************************        !/*as4c       25*/
!                                                                      *        !/*as4c       26*/
!        select the law.                                                        !/*as4c       27*/
      nx=0                                                                      !/*as4c       28*/
      colout(3,ls)=0.                                                           !/*as4c       29*/
   10 t1=rang()                                                                 !/*as4c       30*/
      n=id+kd                                                                   !/*as4c       31*/
      go to 25                                                                  !/*as4c       32*/
   20 n=id+nint(xss(n-1))                                                       !/*as4c       33*/
   25 if(nint(xss(n-1)).eq.0)go to 30                                           !/*as4c       34*/
      t1=t1-acefcn(n+2,erg,ln)                                                  !/*as4c       35*/
      if(t1.ge.0.)go to 20                                                      !/*as4c       36*/
!                                                                               !/*as4c       37*/
!***********************************************************************        !/*as4c       38*/
!                                                                      *        !/*as4c       39*/
!        use the selected law to sample the energy (and possibly angle).        !/*as4c       40*/
!        if law samples without error, go to sample angle or coordinate         !/*as4c       41*/
!        transform as appropriate.                                              !/*as4c       42*/
   30 colout(1,ls)=-huge                                                        !/*as4c       43*/
      lw=nint(xss(n))                                                           !/*as4c       44*/
      iw=id-1+nint(xss(n+1))                                                    !/*as4c       45*/
      if(lw.eq.1)go to 40                                                       !/*as4c       46*/
      if(lw.eq.2)go to 60                                                       !/*as4c       47*/
      if(lw.eq.3)go to 70                                                       !/*as4c       48*/
      if(lw.eq.4)go to 80                                                       !/*as4c       49*/
      if(lw.eq.5)go to 160                                                      !/*as4c       50*/
      if(lw.eq.7)go to 170                                                      !/*as4c       51*/
      if(lw.eq.9)go to 190                                                      !/*as4c       52*/
      if(lw.eq.11)go to 210                                                     !/*as4c       53*/
      if(lw.eq.22)go to 230                                                     !/*as4c       54*/
      if(lw.eq.24)go to 242                                                     !/*as4c       55*/
      if(lw.eq.33)go to 70                                                      !/*as4c       56*/
      if(lw.eq.44)go to 80                                                      !/*as4c       57*/
      if(lw.eq.61)go to 80                                                      !/*as4c       58*/
      if(lw.eq.66)go to 245                                                     !/*as4c       59*/
      if(lw.eq.67)go to 255                                                     !/*as4c       60*/
      go to 300                                                                 !/*as4c       61*/
!                                                                               !/*as4c       62*/
! >>>>>  law 1 (from endf Law 1) -- tabular equiprobable energy bins.           !/*as4c       63*/
   40 call acetbl(iw,ic,r,ln)                                                   !/*as4c       64*/
      nt=nint(xss(iw+ln))                                                       !/*as4c       65*/
      iw=iw+ln+nt*(ic-1)                                                        !/*as4c       66*/
      k=int(rang()*(nt-1)+1)                                                    !/*as4c       67*/
      fr=rang()                                                                 !/*as         32*/
      if(r.ne.0.)go to 50                                                       !/*as         33*/
!        sample from single table.                                              !/*as         34*/
      colout(1,ls)=xss(iw+k)+fr*(xss(iw+k+1)-xss(iw+k))                         !/*as         35*/
      go to 260                                                                 !/*as         36*/
!        sample by scaled interpolation between tables.                         !/*as         37*/
   50 t1=xss(iw+1)+r*(xss(iw+1+nt)-xss(iw+1))                                   !/*as         38*/
      i=iw                                                                      !/*as         39*/
      if(rang().le.r)i=i+nt                                                     !/*as         40*/
      colout(1,ls)=t1+(xss(i+k)+fr*(xss(i+k+1)-xss(i+k))-xss(i+1))*             !/*as         41*/
     & (xss(iw+nt)+r*(xss(iw+2*nt)-xss(iw+nt))-t1)/(xss(i+nt)-xss(i+1))         !/*as         42*/
      go to 260                                                                 !/*as         43*/
!                                                                               !/*as         44*/
! >>>>>  law 2 -- discrete photon lines.                                        !/*as         45*/
!        photon production from neutrons only.                                  !/*as4c       68*/
!        (see endf-102 rev. 2/97 manual page 12.3).                             !/*as4c       69*/
   60 colout(1,ls)=xss(iw+1)                                                    !/*as4c       70*/
      if(nint(xss(iw)).eq.2)colout(1,ls)=colout(1,ls)+                          !/*as4c       71*/
     & erg*awn(lawn+iex)/(awn(lawn+iex)+1.)                                     !/*as4c       72*/
      go to 260                                                                 !/*as4c       73*/
!                                                                               !/*as4c       74*/
! >>>>>  law 3 & 33 -- level scattering.                                        !/*as4c       75*/
!        law 3 applies only for neutron in-neutron out scattering.              !/*as4c       76*/
!        law 33 for a more general combination of particle types.               !/*as4c       77*/
   70 colout(1,ls)=xss(iw+1)*(erg-xss(iw))                                      !/*as         51*/
      go to 260                                                                 !/*as         52*/
!                                                                               !/*as         53*/
! >>>>>  law 4 (from endf law 1) -- continuous erg tabular distribution.        !/*as4c       78*/
! >>>>>  law 44 (from endf law 1) -- kalbach-87 correlated formalism.           !/*as4c       79*/
! >>>>>  law 61 (from endf law 1) -- correlated tab energy-angle dist.          !/*as4c       80*/
   80 call acetbl(iw,ic,r,ln)                                                   !/*as4c       81*/
      nr=nint(xss(iw))                                                          !/*as4c       82*/
      lb=iw-1+ln+ic                                                             !/*as4c       83*/
      lc=id+nint(xss(lb))                                                       !/*as4c       84*/
      ld=lc                                                                     !/*as4a       12*/
      lf=lc                                                                     !/*as4a       13*/
!                                                                               !/*as4c       85*/
!        xss(lc,ld,lf) is an overloaded variable.  it contains the              !/*as4c       86*/
!        number of points in the emission distribution and if part              !/*as4c       87*/
!        of the continuum has been expunged, it contains 0.5 times              !/*as4c       88*/
!        the cumulative probability of the portion expunged.                    !/*as4c       89*/
      np=int(xss(lc)+ep)                                                        !/*as4c       90*/
      mp=np                                                                     !/*as4a       15*/
      jj=nint(xss(lc-1))                                                        !/*as4c       91*/
      nd=0                                                                      !/*as4a       17*/
      if(jj.lt.9999)nd=jj/10                                                    !/*as4a       18*/
      if(r.eq.0.)go to 90                                                       !/*as4a       20*/
      ld=id+nint(xss(lb+1))                                                     !/*as4c       92*/
      mp=int(xss(ld)+ep)                                                        !/*as4c       93*/
      t1=xss(lc+nd+1)+r*(xss(ld+nd+1)-xss(lc+nd+1))                             !/*as4a       23*/
      t2=xss(lc+np)+r*(xss(ld+mp)-xss(lc+np))                                   !/*as4a       24*/
      ra=rang()                                                                 !/*as4a       25*/
      if(ra.ge.r)go to 90                                                       !/*as4a       26*/
      lf=ld                                                                     !/*as4a       27*/
      jj=nint(xss(ld-1))                                                        !/*as4c       94*/
      if(jj.lt.9999.and.jj/10.ne.nd)call expirx(1,'acecas',                     !/*as4a       29*/
     & 'wrong number of discrete lines for law 4/44')                           !/*as4a       30*/
      if(jj.gt.10000)nd=0                                                       !/*as4b        1*/
   90 if(jj.eq.9999)return                                                      !/*as4a       31*/
      jj=jj-nd*10                                                               !/*as4a       32*/
      r1=rang()                                                                 !/*as4a       33*/
!                                                                               !/*as4a       34*/
!        check for a hit in the discrete part.                                  !/*as4a       35*/
!        use histogram or corresponding-point interpolation.                    !/*as4a       36*/
      if(nd.eq.0)go to 97                                                       !/*as4a       37*/
      do 95 ih=1,nd                                                             !/*as4a       38*/
      cc=xss(lc+2*np+ih)+r*(xss(ld+2*mp+ih)-xss(lc+2*np+ih))                    !/*as4a       39*/
   95 if(cc.ge.r1)go to 96                                                      !/*as4a       40*/
      if(np.eq.nd.and.mp.eq.nd)go to 152                                        !/*as4a       41*/
      go to 97                                                                  !/*as4a       42*/
   96 t=xss(lc+ih)+r*(xss(ld+ih)-xss(lc+ih))                                    !/*as4a       43*/
      if(t.lt.0.)t=erg*awn(lawn+iex)/(awn(lawn+iex)+1.)-t                       !/*as4a       44*/
      if(lw.ne.44)go to 152                                                     !/*as4a       45*/
      tpd(1)=xss(lc+ih+3*np)+r*(xss(ld+ih+3*mp)-xss(lc+ih+3*np))                !/*as4a       46*/
      tpd(2)=xss(lc+ih+4*np)+r*(xss(ld+ih+4*mp)-xss(lc+ih+4*np))                !/*as4a       47*/
      go to 152                                                                 !/*as4a       48*/
!                                                                               !/*as4a       49*/
!        handle a hit in the continuous part.                                   !/*as4a       50*/
!        use histogram or unit-base interpolation inside table.                 !/*as4a       51*/
!        use scaled interpolation between tables.                               !/*as4a       52*/
   97 np=int(xss(lf)+ep)                                                        !/*as4c       95*/
      ns=nd+1                                                                   !/*as4a       54*/
      if(nd.ne.0)r1=1.-(1.-r1)*(1.-xss(lf+2*np+nd))/(1.-cc)                     !/*as4a       55*/
!                                                                               !/*as         67*/
!        adjust for the energy cutoff if necessary.                             !/*as         68*/
!        see subroutine expung for law 4/44 distribution. 0 < wc < 1            !/*as4a       56*/
      if(jj.lt.9999)go to 100                                                   !/*as4a       57*/
      jj=jj/10000                                                               !/*as         70*/
      ns=nint(xss(lf-1))-jj*10000                                               !/*as4c       96*/
      wc=2.*(xss(lf)-np)                                                        !/*as4a       59*/
      r1=1.-wc*(1.-r1)                                                          !/*as         74*/
      wgt=wgt*wc                                                                !/*as         75*/
!                                                                               !/*as         76*/
!        find the energy bin in the continuous part.                            !/*as4b        2*/
  100 ic=lf+2*np+ns                                                             !/*as4a       60*/
      ib=lf+3*np                                                                !/*as4a       61*/
  110 if(ib-ic.eq.1)go to 130                                                   !/*as         80*/
      ih=(ic+ib)/2                                                              !/*as         81*/
      if(r1.lt.xss(ih))go to 120                                                !/*as         82*/
      ic=ih                                                                     !/*as         83*/
      go to 110                                                                 !/*as         84*/
  120 ib=ih                                                                     !/*as         85*/
      go to 110                                                                 !/*as         86*/
  130 ln=ic-2*np                                                                !/*as4c       97*/
      fa=xss(ln+np)                                                             !/*as4c       98*/
      ea=xss(ln)                                                                !/*as4c       99*/
!                                                                               !/*as         90*/
!        sample from linear-linear interpolated bin.                            !/*as         91*/
      if(jj.eq.1)go to 140                                                      !/*as         92*/
      bb=(xss(ln+np+1)-fa)/(xss(ln+1)-ea)                                       !/*as4c      100*/
      if(bb.eq.0)go to 140                                                      !/*as         94*/
      t=ea+(sqrt(max(zero,fa**2+2.*bb*(r1-xss(ic))))-fa)/bb                     !/*as4a       63*/
      if(lw.ne.44)go to 150                                                     !/*as4c      101*/
      fb=(t-xss(ln))/(xss(ln+1)-xss(ln))                                        !/*as4c      102*/
      tpd(1)=xss(ic+np)+fb*(xss(ic+1+np)-xss(ic+np))                            !/*as4a       66*/
      tpd(2)=xss(ic+2*np)+fb*(xss(ic+1+2*np)-xss(ic+2*np))                      !/*as4a       67*/
      go to 150                                                                 !/*as         97*/
!                                                                               !/*as         98*/
!        sample from histogram bin.                                             !/*as         99*/
  140 t=ea+(r1-xss(ic))/fa                                                      !/*as        100*/
      if(lw.ne.44)go to 150                                                     !/*as4c      103*/
      tpd(1)=xss(ic+np)                                                         !/*as4a       69*/
      tpd(2)=xss(ic+2*np)                                                       !/*as4a       70*/
!                                                                               !/*as        102*/
!        use scaled interpolation between energies.                             !/*as        103*/
  150 if(r.ne.0.)t=t1+(t-xss(lf+nd+1))*(t2-t1)/(xss(lf+np)-xss(lf+nd+1))        !/*as4c2       1*/
  152 colout(1,ls)=t                                                            !/*as4c      106*/
      if(lw.eq.4)go to 260                                                      !/*as4c      107*/
      if(lw.eq.44)go to 153                                                     !/*as4c      108*/
      if(lw.eq.61)go to 156                                                     !/*as4c      109*/
      go to 295                                                                 !/*as4c      110*/
!                                                                               !/*as4c      111*/
!        sample law 44 -- kalbach-87 angular systematics.                       !/*as4c      112*/
!        tpd(1)=r, tpd(2)=a                                                     !/*as4c      113*/
  153 if(ka.ne.-1.or.ntyn.ge.0)go to 295                                        !/*as4c      114*/
      ipsc=14                                                                   !/*as4a       84*/
      if(rang().le.tpd(1))go to 155                                             !/*as4a       85*/
      t1=(2.*rang()-1.)*sinh(tpd(2))                                            !/*as4a       86*/
      colout(2,ls)=log(t1+sqrt(t1**2+1.))/tpd(2)                                !/*as4a       87*/
      go to 280                                                                 !/*as4a       88*/
  155 r2=rang()                                                                 !/*as4a       89*/
      colout(2,ls)=log(r2*exp(tpd(2))+(1.-r2)*exp(-tpd(2)))/tpd(2)              !/*as4a       90*/
      go to 280                                                                 !/*as4a       91*/
!                                                                               !/*as        111*/
!        sample law 61 -- tabulated angular distribution.                       !/*as4c      115*/
  156 ipsc=16                                                                   !/*as4c      116*/
!                                                                               !/*as4c      117*/
!        if jj=1 (i.e., histogram on e-primes) always use "ic."                 !/*as4c      118*/
!        if jj=2 (lin-lin on e-primes), use the distribution for                !/*as4c      119*/
!        the e-prime closest to "r1" (in cdf space).                            !/*as4c      120*/
      lb=ic+np                                                                  !/*as4c      121*/
      if(jj.ne.1.and.(xss(ib)-r1.lt.r1-xss(ic)))lb=lb+1                         !/*as4c      122*/
!                                                                               !/*as4c      123*/
!        sample from appropriate distribution.  unlike the and block, in        !/*as4c      124*/
!        law 61 only isotropic or tabular angular information is passed.        !/*as4c      125*/
      ixcos=0                                                                   !/*as4c      126*/
      lm=nint(xss(lb))                                                          !/*as4c      127*/
      if(lm.eq.0)colout(2,ls)=2.*rang()-1.                                      !/*as4c      128*/
      if(lm.gt.0)colout(2,ls)=acecos(id,-lm)                                    !/*as4c      129*/
      if(lm.lt.0)call expirx(1,'acecas',                                        !/*as4c      130*/
     & 'data: law 61 contained a bad table pointer')                            !/*as4c      131*/
      go to 280                                                                 !/*as4c      132*/
!                                                                               !/*as4c      133*/
! >>>>>  law 5 (from endf law 5) -- general evaporation spectrum.               !/*as4c      134*/
  160 t1=acefcn(iw,erg,ln)                                                      !/*as4c      135*/
      i=iw+ln+1+int(rang()*(nint(xss(iw+ln))-1))                                !/*as4c2       2*/
      colout(1,ls)=t1*(xss(i)+rang()*(xss(i+1)-xss(i)))                         !/*as        114*/
      go to 260                                                                 !/*as        115*/
!                                                                               !/*as        116*/
! >>>>>  law 7 (from endf law 7) -- simple maxwell fission spectrum.            !/*as4c      138*/
  170 t1=acefcn(iw,erg,ln)                                                      !/*as4c      139*/
      t3=erg-xss(iw+ln)                                                         !/*as4c      140*/
      if(t3.le.0.)go to 260                                                     !/*as        119*/
  180 t4=rang()**2                                                              !/*as        120*/
      t2=t4+rang()**2                                                           !/*as        121*/
      if(t2.gt.1.)go to 180                                                     !/*as        122*/
      t2=log(rang())*t4/t2                                                      !/*as        123*/
      colout(1,ls)=-t1*(t2+log(rang()))                                         !/*as        124*/
!        reject if outside range 0 ... e-u                                      !/*as        125*/
      if(colout(1,ls).gt.t3)go to 180                                           !/*as        126*/
      go to 260                                                                 !/*as        127*/
!                                                                               !/*as        128*/
! >>>>>  law 9 (from endf law 9) -- evaporation spectrum.                       !/*as4c      141*/
  190 t1=acefcn(iw,erg,ln)                                                      !/*as4c      142*/
      t2=erg-xss(iw+ln)                                                         !/*as4c      143*/
      if(t2.le.0.)go to 260                                                     !/*as        131*/
  200 fr=rang()                                                                 !/*as        132*/
      colout(1,ls)=-t1*log(fr*rang())                                           !/*as        133*/
!        reject if outside range 0 ... e-u                                      !/*as4c      144*/
      if(colout(1,ls).gt.t2)go to 200                                           !/*as        134*/
      go to 260                                                                 !/*as        135*/
!                                                                               !/*as        136*/
! >>>>>  law 11 (from endf law 11) -- energy dependent watt spectrum.           !/*as4c      145*/
  210 t1=acefcn(iw,erg,ln)                                                      !/*as4c      146*/
      t2=acefcn(iw+ln,erg,lb)                                                   !/*as4c      147*/
      if(erg.le.xss(iw+ln+lb))go to 260                                         !/*as4c      148*/
      t5=sqrt((1.+.125*t1*t2)**2-1.)+1.+.125*t1*t2                              !/*as        140*/
  220 t=-log(rang())                                                            !/*as        141*/
      colout(1,ls)=t1*t5*t                                                      !/*as        142*/
      if(((1.-t5)*(1.+t)-log(rang()))**2.gt.t2*colout(1,ls))go to 220           !/*as        143*/
      go to 260                                                                 !/*as        144*/
!                                                                               !/*as        145*/
! >>>>>  law 22 (from uk law 2) -- tabular linear functions.                    !/*as4c      149*/
  230 call acetbl(iw,ic,r,ln)                                                   !/*as4c      150*/
      ie=id-1+nint(xss(iw+ln+ic-1))                                             !/*as4c      151*/
      nf=nint(xss(ie))                                                          !/*as4c      152*/
  235 iw=ie                                                                     !/*as4a       94*/
      fr=rang()                                                                 !/*as        149*/
  240 iw=iw+1                                                                   !/*as        150*/
      fr=fr-xss(iw)                                                             !/*as        151*/
      if(fr.ge.0.)go to 240                                                     !/*as        152*/
      if(iw.gt.ie+nf)go to 235                                                  !/*as4a       95*/
      colout(1,ls)=xss(iw+2*nf)*(erg-xss(iw+nf))                                !/*as4a       96*/
      go to 260                                                                 !/*as        154*/
!                                                                               !/*as        155*/
! >>>>>  law 24 (from uk law 6) -- tabular energy multipliers.                  !/*as4c      153*/
  242 call acetbl(iw,ic,r,ln)                                                   !/*as4c      154*/
      i=iw+ln+1+nint(xss(iw+ln))*(ic-1)+int(rang()*(nint(xss(iw+ln))-1))        !/*as4c      155*/
      colout(1,ls)=erg*(xss(i)+rang()*(xss(i+1)-xss(i)))                        !/*as        158*/
      go to 260                                                                 !/*as4a       98*/
!                                                                               !/*as4a       99*/
! >>>>>  law 66 (from endf law 6) -- n-body phase space distribution.           !/*as4c      156*/
  245 nb=nint(xss(iw))                                                          !/*as4c      157*/
      ap=xss(iw+1)                                                              !/*as4a      102*/
      if(ipt.gt.2)ap=ap*gpt(1)/gpt(ipt)                                         !/*as4c      158*/
  246 r=rang()**2                                                               !/*as4a      103*/
      s=r+rang()**2                                                             !/*as4a      104*/
      if(s.gt.1.)go to 246                                                      !/*as4a      105*/
      x=-r*log(s)/s-log(rang())                                                 !/*as4a      106*/
  248 r=rang()**2                                                               !/*as4a      107*/
      s=r+rang()**2                                                             !/*as4a      108*/
      if(s.gt.1.)go to 248                                                      !/*as4a      109*/
      p=rang()                                                                  !/*as4a      110*/
      go to(252,251,250)nb-2                                                    !/*as4a      111*/
  250 p=p*rang()*rang()                                                         !/*as4a      112*/
  251 p=p*rang()                                                                !/*as4a      113*/
  252 y=-r*log(s)/s-log(p)                                                      !/*as4a      114*/
      t=x/(x+y)                                                                 !/*as4a      115*/
      aw=awn(lawn+iex)                                                          !/*as4a      116*/
      colout(1,ls)=t*((ap-1.)/ap)*(erg*aw/(aw+1.)+q)                            !/*as4c      159*/
      colout(2,ls)=2.*rang()-1.                                                 !/*as4c      160*/
      ixcos=0                                                                   !/*as4a      120*/
!     if(ntyn.ge.0)go to 295                                                    !/*asx         6*/
      go to 280                                                                 !/*as4a      123*/
!                                                                               !/*as4a      124*/
! >>>>>  law 67 (endf/b-vi law 7) -- correleted energy-angle scatter.           !/*as4a      125*/
  255 call acetbl(iw,ic,r,ln)                                                   !/*as4c      161*/
      cs=acecos(ia,ka)                                                          !/*as4c      162*/
      ipsc=15                                                                   !/*as4a      128*/
      colout(2,ls)=cs                                                           !/*as4a      129*/
      colout(1,ls)=acecs6(0,id,iw,ic,r,cs)                                      !/*as4c      163*/
      if(ntyn.le.0)go to 295                                                    !/*as4a      131*/
      if(colout(1,ls).lt.0.)go to 300                                           !/*as4c      164*/
      go to 290                                                                 !/*as4a      133*/
!                                                                               !/*as        159*/
!***********************************************************************        !/*as4c      165*/
!                                                                      *        !/*as4c      166*/
!        if not correlated energy-angle, calculate the cosine.                  !/*as4c      167*/
  260 colout(2,ls)=acecos(ia,ka)                                                !/*as4c      168*/
!                                                                               !/*as        181*/
!***********************************************************************        !/*as4c      169*/
!                                                                      *        !/*as4c      170*/
!        adjust if energy and cosine are given in center-of-mass system.        !/*as        182*/
  280 if(colout(1,ls).lt.0.)go to 300                                           !/*as4b        5*/
      ergace=colout(1,ls)                                                       !/*as4b        6*/
      if(ntyn.ge.0)go to 290                                                    !/*as4a      136*/
!        formulas below are from p. 2 of x-6:res-93-68.                         !/*as4c      171*/
!        these formulas assume two-body kinematics.                             !/*as4c      172*/
!        seamon's formulas specify atomic weight ratios (to neutron)            !/*as4c      173*/
!        for incident particle (a), awr=gpt(ipt_incident)/gpt(1)                !/*as4c      174*/
!        for exiting particle (b), awr=gpt(ipt)/gpt(1)                          !/*as4c      175*/
!        for target (a), awr=awn(lawn+iex)                                      !/*as4c      176*/
      a1=gpt(ip)/gpt(1)                                                         !/*as4c      177*/
      a2=gpt(ipt)/gpt(1)                                                        !/*as4c      178*/
      a3=awn(lawn+iex)                                                          !/*as4c      179*/
      t1=colout(1,ls)                                                           !/*as4c      180*/
      t2=erg*a1*a2/(a3+a1)**2                                                   !/*as4c      181*/
      t3=2.*sqrt(a2*a1*erg*colout(1,ls))*colout(2,ls)/(a3+a1)                   !/*as4c      182*/
      t4=t1+t2+t3                                                               !/*as4c      183*/
      s1=colout(2,ls)*sqrt(colout(1,ls)/t4)                                     !/*as4c      184*/
      s2=sqrt(a1*a2*erg/t4)/(a3+a1)                                             !/*as4c      185*/
      colout(1,ls)=t4                                                           !/*as4c      186*/
      colout(2,ls)=s1+s2                                                        !/*as4c      187*/
!                                                                               !/*as4a      137*/
!***********************************************************************        !/*as4c      188*/
!                                                                      *        !/*as4c      189*/
!        resample energy up to 100 times if > emx(1).                           !/*as4a      138*/
  290 if(colout(1,ls).le.emx(ipt))return                                        !/*as4c      190*/
      nx=nx+1                                                                   !/*as4a      140*/
      call errprn(1,netb(2),4,erg,zero,'erg',' ',                               !/*as4b        7*/
     & 'energy of particle from inelastic collision > emx')                     !/*as4c      191*/
      if(nx.lt.100)go to 10                                                     !/*as4a      143*/
      call expirx(1,'acecas','erg>emx 100 times in one collision.')             !/*as4a      144*/
      return                                                                    !/*as        190*/
!                                                                               !/*as        191*/
!***********************************************************************        !/*as4c      192*/
!                                                                      *        !/*as4c      193*/
!        print debug information for cross-section table errors.                !/*as        192*/
  295 colout(1,ls)=huge                                                         !/*as4c      194*/
  300 call zaid(2,ht,ixl(lixl+1,iex))                                           !/*as        193*/
#ifdef MULTT                                                                    /*!asxk        1*/
      call thread_lockon(jlock)                                                 !/*asxk        2*/
#endif                                                                          /*!asxk        3*/
      write(iuo,310)ht,erg,ixre,mtp,ntyn,lw,colout(1,ls)                        !/*as4c      195*/
  310 format(/30h error in cross-section table ,a10/12h energy in =,            !/*as        197*/
     & 1pe12.4,5x,16hreaction index =,i3,5x,4hmt =,i4,5x,4hty =,i4,             !/*as        198*/
     & 5x,5hlaw =,i3,5x,12henergy out =,1pe12.4)                                !/*as        199*/
      if(colout(1,ls).eq.-huge)call expirx(1,'acecas',                          !/*as        200*/
     & 'an inappropriate or non-existent law was selected.')                    !/*as        201*/
      if(colout(1,ls).lt.0.)call expirx(1,'acecas',                             !/*as        202*/
     & 'emission energy was negative.')                                         !/*as4c      196*/
      if(colout(1,ls).eq.huge)call expirx(1,'acecas',                           !/*as4c      197*/
     & 'faulty cross-section data.')                                            !/*as4c      198*/
      if(colout(1,ls).gt.erg)call expirx(1,'acecas',                            !/*as        204*/
     & 'emission energy exceeds incident energy.')                              !/*as4c      199*/
      return                                                                    !/*as        216*/
      end                                                                       !/*as        217*/

!#ident prstat.F from deck ph4a                                                 !/*ph4a        1*/
      subroutine prstat(nn)                                                     !/*ph4a        2*/
!        print tfc bin history score info and call prtfcc and prhpdf.           !/*ph4a        3*/
!        nn=number of entries in the tfc nps array npc.                         !/*ph4a        4*/
!        hsb has the nsp-1 tally boundaries from hsb(1) to hsb(nsp-1).          !/*ph4a        5*/
!        nhsd contains the number of history scores in each hsb bin.            !/*ph4a        6*/
!        shsd contains the weight of history scores in each hsb bin.            !/*ph4a        7*/
      use global_data                                                           !/*phx         1*/
!                                                                               !/*phx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*phx         3*/
      implicit integer(kindi) (i-n)                                             !/*phx         4*/
!                                                                               !/*phx         5*/
      character he*7,hf*8,hg*5                                                  !/*ph4a        9*/
!                                                                               !/*ph4a       10*/
!        check if the tally fluctuation chart (tfc) bin has any tallies.        !/*ph4a       11*/
      it=ital+iper*ntal                                                         !/*ph4b        1*/
      nt=nhsd(lnhs+nsp+2,it)                                                    !/*ph4b        2*/
      if(nt.gt.0)go to 20                                                       !/*ph4a       13*/
      write(iuo,10)jptal(ljpt+1,ital)                                           !/*ph4a       14*/
   10 format(/54h there are no nonzero tallies in the tally fluctuation,        !/*ph4a       15*/
     & 20h chart bin for tally,i4)                                              !/*ph4a       16*/
      return                                                                    !/*ph4a       17*/
!                                                                               !/*ph4a       18*/
!        correct for the sign change requested by dbcn(16).                     !/*ph4a       19*/
   20 tt=hsb(nsp)*shsd(lshs+nsp+2,it)                                           !/*ph4b        3*/
      tn=tt*fpi                                                                 !/*ph4a       21*/
      tm=hsb(nsp)*shsd(lshs+nsp+1,it)*fpi                                       !/*ph4b        4*/
      tp=tn-tm                                                                  !/*ph4a       23*/
!                                                                               !/*ph4a       24*/
!        set cumulative tally scores >0 but below grid bg and total dn.         !/*ph4a       25*/
      dn=abs(tp)                                                                !/*ph4a       26*/
      bg=abs(shsd(lshs+1,it)*fpi)-abs(tm)                                       !/*ph4b        5*/
!                                                                               !/*ph4a       28*/
!        find number of +(nq) and -(nx) tallies and switch if necessary.        !/*ph4a       29*/
      nx=nhsd(lnhs+nsp+1,it)                                                    !/*ph4b        6*/
      nq=nhsd(lnhs+nsp+2,it)-nx                                                 !/*ph4b        7*/
      ni=nq                                                                     !/*ph4a       32*/
      hf='largest '                                                             !/*ph4a       33*/
      he='maximum'                                                              !/*ph4a       34*/
      hg='above'                                                                !/*ph4a       35*/
      if(hsb(nsp).gt.0.)go to 30                                                !/*ph4a       36*/
      hf='smallest'                                                             !/*ph4a       37*/
      he='minimum'                                                              !/*ph4a       38*/
      hg='below'                                                                !/*ph4a       39*/
      na=nx                                                                     !/*ph4a       40*/
      nx=nq                                                                     !/*ph4a       41*/
      nq=na                                                                     !/*ph4a       42*/
      ta=tm                                                                     !/*ph4a       43*/
      tm=tp                                                                     !/*ph4a       44*/
      tp=ta                                                                     !/*ph4a       45*/
!                                                                               !/*ph4a       46*/
!        set tfc quantities for later use.                                      !/*ph4a       47*/
   30 t1=tfc(ltfc+1,nn,it)                                                      !/*ph4b        8*/
      t2=tfc(ltfc+2,nn,it)                                                      !/*ph4b        9*/
      t3=tfc(ltfc+3,nn,it)                                                      !/*ph4b       10*/
      t4=tfc(ltfc+4,nn,it)                                                      !/*ph4b       11*/
      t5=min(ten,tfc(ltfc+5,nn,it))                                             !/*phx         6*/
      if(t5.le.1..and.t5.ne.0.)t5=10.                                           !/*ph4a       53*/
!                                                                               !/*ph4a       54*/
!        find tfc bin intrinsic(ri) and zero tally(re) relative errors.         !/*ph4a       55*/
      re=max(zero,1./nt-fpi)                                                    !/*ph4a       56*/
      ri=sqrt(max(zero,t2**2-re))                                               !/*ph4a       57*/
      re=sqrt(re)                                                               !/*ph4a       58*/
!                                                                               !/*ph4a       59*/
!        find fractional mean change (t*nps+d1)/(t(nps+1))-1.(=xc),             !/*ph4a       60*/
!        rel er(=rm),vov(=vm) if next history score is extreme sampled.         !/*ph4a       61*/
      d1=hsb(nsp)*ddm(lddm+1,it)                                                !/*ph4b       13*/
      ft=1./(nps-nskk+1.)                                                       !/*ph4a       63*/
      tc=(d1-tn)*ft/tn                                                          !/*ph4a       64*/
      s2=tal(ltal+2*mxf+jptal(ljpt+7,ital)+iper*mxfp)                           !/*ph4b       14*/
      p2=s2+d1**2                                                               !/*ph4a       66*/
      s3=shsd(lshs+nsp+3,it)                                                    !/*ph4b       15*/
      p3=s3+d1**3                                                               !/*ph4a       68*/
      rr=1.                                                                     !/*ph4a       70*/
      rm=t2                                                                     !/*ph4a       71*/
      vr=0.                                                                     !/*ph4a       72*/
      vm=t4                                                                     !/*ph4a       73*/
!                                                                               !/*ph4a       74*/
!        skip re and vov calculations if the re (t2) or vov (t4)=0.             !/*ph4a       75*/
      if(t2.eq.0..or.t4.eq.0.)go to 40                                          !/*ph4a       76*/
      p1=tt+d1                                                                  !/*ph4a       77*/
      rm=sqrt(min(one,max(zero,p2/p1**2-ft)))                                   !/*ph4a       78*/
      rr=1.                                                                     !/*ph4c        2*/
      if(rm.gt.0.)rr=(t2/rm)**2                                                 !/*ph4c        3*/
      p4=shsd(lshs+nsp+4,it)+d1**4                                              !/*ph4b       16*/
      v1=p2-ft*p1**2                                                            !/*ph4a       82*/
      v4=p4-4.*p1*p3*ft+8.*p2*(p1*ft)**2-4.*p1**4*ft**3-ft*p2**2                !/*ph4a       83*/
      if(v4.le.0..or.v1.le.0.)go to 40                                          !/*ph4a       84*/
      vm=max(zero,min(v4/v1**2,one))                                            !/*ph4a       85*/
      if(v4.lt.1.e-30.and.vm.eq.one)vm=0.                                       !/*ph4a       86*/
      vr=vm/t4-1.                                                               !/*ph4a       87*/
!                                                                               !/*ph4a       88*/
!        calculate the shift of the center of confidence interval(ci).          !/*ph4a       89*/
!        see a-1/87-749 for description of third central moment shift.          !/*ph4a       90*/
!        the ci shift is scaled to the normalized mean t1 from tn.              !/*ph4a       91*/
   40 xd=s2-fpi*tt**2                                                           !/*ph4a       92*/
      xs=0.                                                                     !/*ph4a       93*/
      if(tn*xd.ne.0.)                                                           !/*ph4a       94*/
     & xs=max(zero,.5*fpi*t1*(s3-3.*s2*tn+2.*tt**3*fpi**2)/(tn*xd))             !/*ph4a       95*/
      ac=t1+sign(xs,t1)                                                         !/*ph4a       96*/
      x1=p2-ft*tt**2                                                            !/*ph4a       97*/
      xm=0.                                                                     !/*ph4a       98*/
      if(tn*x1.ne.0.)xm=max(zero,.5*ft*t1*(p3-3.*p2*tn+2.*tt**3*ft**2)/         !/*ph4a       99*/
     & (tn*x1))                                                                 !/*ph4a      100*/
      am=t1+sign(xm,t1)                                                         !/*ph4a      101*/
      if(ink(160).eq.0)go to 310                                                !/*ph4a      102*/
!                                                                               !/*ph4a      103*/
!        print tally information in the tally fluctuation chart bin.            !/*ph4a      104*/
      write(iuo,50)jptal(ljpt+1,ital),nps                                       !/*ph4a      105*/
   50 format(49h1analysis of the results in the tally fluctuation,              !/*ph4a      106*/
     & 26h chart bin (tfc) for tally,i4,11h with nps =,i10,4x,                  !/*ph4a      107*/
     & 15hprint table 160)                                                      !/*ph4a      108*/
      if(hsb(nsp).lt.0.)write(iuo,60)                                           !/*ph4a      109*/
   60 format(48h the empirical history score probability density,               !/*ph4a      110*/
     & 34h function is for negative tallies.)                                   !/*ph4a      111*/
      write(iuo,70)t1,tn,t2,t4,re,ri,nhsd(lnhs+nsp+2,ital),nt*fpi               !/*ph4a      112*/
   70 format(//36h normed average tally per history  =,1pe12.5,9x,              !/*ph4a      113*/
     & 38h unnormed average tally per history  =,e12.5/                         !/*ph4a      114*/
     & 36h estimated tally relative error    =,0pf7.4,14x,                      !/*ph4a      115*/
     & 38h estimated variance of the variance  =,f7.4/                          !/*ph4a      116*/
     & 36h relative error from zero tallies  =,f7.4,14x,                        !/*ph4a      117*/
     & 38h relative error from nonzero scores  =,f7.4//                         !/*ph4a      118*/
     & 36h number of nonzero history tallies =,i12,9x,                          !/*ph4a      119*/
     & 38h efficiency for the nonzero tallies  =,f7.4)                          !/*ph4a      120*/
!                                                                               !/*ph4a      121*/
!        insert two lines if any negative history tallies occurred.             !/*ph4a      122*/
      if(nx.gt.0)write(iuo,80)nq,tp,nx,tm                                       !/*ph4a      123*/
   80 format(36h number of positive history tallies=,i12,9x,                    !/*ph4a      124*/
     & 38h unnormed avg positive history tally =,1pe12.5/                       !/*ph4a      125*/
     & 36h number of negative history tallies=,i12,9x,                          !/*ph4a      126*/
     & 38h unnormed avg negative history tally =,1pe12.5)                       !/*ph4a      127*/
!                                                                               !/*ph4a      128*/
!        print information about the extreme tally.                             !/*ph4a      129*/
      write(iuo,90)hf,nint(ddm(lddm+2,it)),hf,d1,hf,d1/tn,hf,                   !/*ph4b       17*/
     & d1*nt/tt,xs/t1,ac                                                        !/*ph4a      131*/
   90 format(19h history number of ,a8,9h tally  =,i12,10x,                     !/*ph4a      132*/
     & a8,29h unnormalized history tally =,1pe12.5/                             !/*ph4a      133*/
     & 2h (,a8,26h tally)/(average tally)  =,e12.5,9x,                          !/*ph4a      134*/
     & 2h (,a8,28h tally)/(avg nonzero tally)=,e12.5//                          !/*ph4a      135*/
     & 36h (confidence interval shift)/mean  =,0pf7.4,14x,                      !/*ph4a      136*/
     & 38h shifted confidence interval center  =,1pe12.5//)                     !/*ph4a      137*/
!                                                                               !/*ph4a      138*/
!        print results if extreme history score occurs on next history.         !/*ph4a      139*/
      write(iuo,100)hf                                                          !/*ph4a      140*/
  100 format(8h if the ,a8,37h history score sampled so far were to,            !/*ph4a      141*/
     & 56h occur on the next history, the tfc bin quantities would,             !/*ph4b       18*/
     & 19h change as follows:)                                                  !/*ph4a      143*/
      j=nps-nint(pax(1,1,1))                                                    !/*ph4c        4*/
      if(j.gt.0.and.nsr.eq.71)write(iuo,110)nint(pax(1,1,1)),ikz,j              !/*ph4c        5*/
  110 format(6h nps =,i10,23h for this table because,i4,12h keff cycles,        !/*ph4a      145*/
     & 4h and,i10,50h histories were skipped before tally accumulation.)        !/*ph4a      146*/
      if(mct.ge.0)write(iuo,120)t1,t1*(1.+tc),tc,t2,rm,sqrt(1/rr)-1.,t4,        !/*ph4a      147*/
     & vm,vr,ac,am,am/ac-1.,t3,t3*rr,rr-1.                                      !/*ph4a      148*/
      if(mct.lt.0)write(iuo,120)t1,t1*(1.+tc),tc,t2,rm,sqrt(1/rr)-1.,t4,        !/*ph4a      149*/
     & vm,vr,ac,am,am/ac-1.,0.,0.,0.                                            !/*ph4a      150*/
  120 format(/5x,21h estimated quantities,10x,13h value at nps,10x,             !/*ph4a      151*/
     & 15h value at nps+1,10x,14h value(nps+1)/,13hvalue(nps)-1.//              !/*ph4a      152*/
     & 5x,5h mean,1pe39.5,e24.5,0pf29.6/                                        !/*ph4a      153*/
     & 5x,15h relative error,1pe29.5,e24.5,0pf29.6/                             !/*ph4a      154*/
     & 5x,25h variance of the variance,1pe19.5,e24.5,0pf29.6/                   !/*ph4a      155*/
     & 5x,15h shifted center,1pe29.5,e24.5,0pf29.6/                             !/*ph4a      156*/
     & 5x,16h figure of merit,1pe28.5,e24.5,0pf29.6/)                           !/*ph4a      157*/
!                                                                               !/*ph4a      158*/
!        check to determine if there any history scores in score grid.          !/*ph4a      159*/
      nu=nhsd(lnhs+nsp+4,it)-1                                                  !/*ph4b       19*/
      nl=nhsd(lnhs+nsp+3,it)-1                                                  !/*ph4b       20*/
      if(nl.gt.-1)go to 140                                                     !/*ph4a      162*/
      write(iuo,130)hsb(1),hsb(nsp-1)                                           !/*ph4a      163*/
  130 format(51h all contributions to the history score probability,            !/*ph4a      164*/
     & 59h density function analysis are outside the score range from,          !/*ph4a      165*/
     & 1pe8.1,4h to ,e8.1,1h.)                                                  !/*ph4a      166*/
      go to 310                                                                 !/*ph4a      167*/
!                                                                               !/*ph4a      168*/
!        print info about pareto fit to the big,small history tallies.          !/*ph4a      169*/
  140 np=nhsd(lnhs+nsp+5,it)                                                    !/*ph4b       21*/
      tb=shsd(lshs+nspt,it)                                                     !/*ph4b       22*/
      if(t5.eq.0..and.np.eq.0)write(iuo,150)hf                                  !/*ph4a      172*/
  150 format(40h there is not enough information in the ,a8,                    !/*ph4a      173*/
     & 52h history scores (usually less than 500 scores) for a,                 !/*ph4a      174*/
     & 32h reliable estimate of the slope.)                                     !/*ph4a      175*/
      if(t5.eq.0..and.np.gt.0)write(iuo,160)np,hg,hsb(nsp)*tb,hf                !/*ph4a      176*/
  160 format(17h the slope of the,i4,9h tallies ,a5,1pe12.5,                    !/*ph4a      177*/
     & 55h was unable to be calculated: examine the tally pdf and,              !/*ph4a      178*/
     & 5h the ,a8,9h tallies.)                                                  !/*ph4a      179*/
      if(t5.gt.0..and.t5.lt.10.)write(iuo,170)np,hf,hsb(nsp)*tb,t5              !/*ph4a      180*/
  170 format(41h the estimated inverse power slope of the,i4,1h ,a8,            !/*ph4a      181*/
     & 20h tallies starting at,1pe12.5,3h is,0pf7.4)                            !/*ph4a      182*/
      if(t5.eq.10..and.np.eq.0)write(iuo,180)hf,he,                             !/*ph4a      183*/
     & shsd(lshs+nspt-2,it)                                                     !/*ph4b       23*/
  180 format(9h the 100 ,a8,34h history tallies appear to have a ,a8,           !/*ph4a      185*/
     & 15h value of about,1pe12.5)                                              !/*ph4a      186*/
      if(t5.eq.10..and.np.gt.0)write(iuo,190)np,hf,hsb(nsp)*tb                  !/*ph4a      187*/
  190 format(27h the estimated slope of the,i4,1h ,a8,8h tallies,               !/*ph4a      188*/
     & 13h starting at ,1pe12.5,34h appears to be decreasing at least,          !/*ph4a      189*/
     & 15h exponentially.)                                                      !/*ph4a      190*/
!                                                                               !/*ph4a      191*/
!        see if extreme f(x) values decline monotonically last 5 cycles.        !/*ph4a      192*/
!        skip the check if f(x) appears bounded or has only one bin.            !/*ph4a      193*/
      if(t5.eq.10..and.np.eq.0.or.nu.eq.nl)go to 260                            !/*ph4a      194*/
      do 200 j=nu-min(5,nu-nl-1),nu                                             !/*ph4a      195*/
  200 if(nhsd(lnhs+j,it).ne.0)go to 210                                         !/*ph4b       24*/
      go to 260                                                                 !/*ph4a      197*/
  210 nb=nhsd(lnhs+j,it)                                                        !/*ph4b       25*/
      do 230 k=j+1,nu+1                                                         !/*ph4a      199*/
      if(nhsd(lnhs+k,it).eq.0)go to 230                                         !/*ph4b       26*/
      if(nb.ge.nhsd(lnhs+k,it))go to 220                                        !/*ph4b       27*/
      if(nb+nint(6.*sqrt(float(nb))).lt.nhsd(lnhs+k,it))go to 240               !/*ph4b       28*/
  220 nb=nhsd(lnhs+k,it)                                                        !/*ph4b       29*/
  230 continue                                                                  !/*ph4a      204*/
      go to 260                                                                 !/*ph4a      205*/
  240 write(iuo,250)hf                                                          !/*ph4a      206*/
  250 format(48h the empirical history score probability density,               !/*ph4a      207*/
     & 42h function appears to be increasing at the ,a8,                        !/*ph4a      208*/
     & 32h history scores: please examine.)                                     !/*ph4a      209*/
!                                                                               !/*ph4a      210*/
!        check for holes in the last 5 bins of the high tally tail.             !/*ph4a      211*/
  260 do 270 j=nu-min(5,nu-nl-1),nu                                             !/*ph4a      212*/
  270 if(nhsd(lnhs+j,it).eq.0)go to 290                                         !/*ph4b       30*/
      write(iuo,280)                                                            !/*ph4a      214*/
  280 format(52h the large score tail of the empirical history score,           !/*ph4a      215*/
     & 58h probability density function appears to have no unsampled,           !/*ph4a      216*/
     & 9h regions.)                                                             !/*ph4a      217*/
      go to 310                                                                 !/*ph4a      218*/
  290 write(iuo,300)hf                                                          !/*ph4a      219*/
  300 format(55h the history score probability density function appears,        !/*ph4a      220*/
     & 36h to have an unsampled region at the ,a8,16h history scores:,          !/*ph4a      221*/
     & 16h please examine.)                                                     !/*ph4a      222*/
!                                                                               !/*ph4a      223*/
!        make the 10 statistical checks on tfc bin result.                      !/*ph4a      224*/
  310 call prtfcc(ac,nn)                                                        !/*ph4a      225*/
!                                                                               !/*ph4b       31*/
!        print fom with histories/time (pt) and signal/noise ratio (sn).        !/*ph4b       32*/
      pt=t3*t2**2/fpi                                                           !/*ph4b       33*/
      if(pt.eq.0.)pt=60.*nps/max(one,cts-cpk)                                   !/*phx         9*/
      sn=sign(sqrt(t3/pt),t1)                                                   !/*ph4b       35*/
      if(t2.eq.0..and.mct.lt.0)sn=1.e15                                         !/*ph4b       36*/
      if(mct.ge.0)write(iuo,320)pt,sn,pt,sn**2,t3                               !/*ph4b       37*/
      if(mct.lt.0)write(iuo,320)0.,sn,0.,sn**2,0.                               !/*ph4b       38*/
  320 format(/54h fom = (histories/minute)*(f(x) signal-to-noise ratio),        !/*ph4b       39*/
     & 7h**2 = (,1pe9.3,3h)*(,e10.3,8h)**2 = (,e9.3,3h)*(,e9.3,3h) =,           !/*ph4b       40*/
     & e10.3)                                                                   !/*ph4b       41*/
!                                                                               !/*ph4a      226*/
!        print history score probability density function if required.          !/*ph4a      227*/
      if(ink(160).gt.0.and.(ink(161).gt.0.or.nhsd(lnhs+nsp+7,it).gt.0)          !/*ph4b       42*/
     & .and.nhsd(lnhs+nsp+3,it).gt.0)call prhpdf(bg,dn,ni,nn)                   !/*ph4b       43*/
      return                                                                    !/*ph4a      230*/
      end                                                                       !/*ph4a      231*/

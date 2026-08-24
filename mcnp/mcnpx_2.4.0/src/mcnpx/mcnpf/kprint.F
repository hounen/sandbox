!#ident kprint.F from deck kp4a                                                 !/*kp4a        1*/
      subroutine kprint                                                         !/*kp4a        2*/
!        calculate and print information about the keff results.                !/*kp4a        3*/
!        subroutine kprint is called from subroutine sumary.                    !/*kp4a        4*/
!                                                                               !/*kp4a        5*/
!        cc,cg,cm = confidence interval multipliers for 68, 95, 99 pct.         !/*kp4b        1*/
!        cz has covariances for pairs of estimators, zc.                        !/*kp4b        2*/
!        ea has standard deviations for za.                                     !/*kp4b        3*/
!        ec has standard deviations for zc.                                     !/*kp4b        4*/
!        ek has the high, low values of the cycle keffs for three keffs.        !/*kp4a        8*/
!        ik has the cycles on which the high and thr low keffs occurred.        !/*kp4a        9*/
!        nx is the maximum number of unsampled cells on one print line.         !/*kp4a       10*/
!        za simple averages of col, abs, tl keffs or lifetimes.                 !/*kp4b        5*/
!        zc covariance weighted averages of keffs or lifetimes.                 !/*kp4b        6*/
!        za contains average keff for 3 individual ks and k(col/abs/tl).        !/*kp4a       12*/
!        hz = xx/xx/xx col,abs,tl where xx=95,99,or no (for not normal).        !/*kp4a       13*/
!        zk is a dummy for calling knorm.                                       !/*kp4a       14*/
!                                                                               !/*kp4a       15*/
      use global_data                                                           !/*kpx         1*/
!                                                                               !/*kpx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*kpx         3*/
      implicit integer(kindi) (i-n)                                             !/*kpx         4*/
!                                                                               !/*kpx         5*/
      character hc(2)*10,hk(7)*15,hn(3)*23,hz*8                                 !/*kp4b        7*/
      dimension cc(3),cg(3),cm(3),cz(3),ea(4),ec(4),ek(3,2),za(4),zc(4),        !/*kp4b        8*/
     & zk(3,1),ik(3,2),nc(3),nz(20)                                             !/*kp4b        9*/
      data hc/'decreasing','increasing'/,hk/'      collision',                  !/*kp4a       20*/
     & '     absorption','   track length','     col/absorp',                   !/*kp4a       21*/
     & '    abs/trk len','    col/trk len','col/abs/trk len'/,                  !/*kp4a       22*/
     & hn/'neutron tracks entering','neutron collisions     ',                  !/*kp4b       10*/
     & 'fission source points  '/                                               !/*kp4b       11*/
      je=3                                                                      !/*kp4c        1*/
!                                                                               !/*kp4a       26*/
!        print the keff results table for the problem if kcz > ikz.             !/*kp4a       27*/
      if(mct.ge.0)write(iuo,10)aid,probid                                       !/*kp4a       28*/
   10 format(19h1keff results for: ,a80,5x,9hprobid = ,a19/)                    !/*kp4a       29*/
      if(kcz.le.ikz)call erprnt(1,2,0,0,0,0,0,0,                                !/*kp4a       30*/
     & '57hno final keff estimate made because no active cycles run.')          !/*kp4a       31*/
!                                                                               !/*kp4a       32*/
!        print starting fission source info for keff calculation.               !/*kp4a       33*/
      if(nsrc.gt.0)write(iuo,20)nsrc/3                                          !/*kp4a       34*/
   20 format(/53h the initial fission neutron source distribution used,         !/*kp4a       35*/
     & 4h the,i4,48h source points that were input on the ksrc card.)           !/*kp4a       36*/
      if(ksdef.ne.0)write(iuo,30)                                               !/*kp4a       37*/
   30 format(/52h the initial fission neutron source distribution was,          !/*kp4a       38*/
     & 50h generated from a general sdef source description.)                   !/*kp4a       39*/
      if(ksdef.eq.0.and.nsrc.eq.0)write(iuo,40)srctp                            !/*kp4a       40*/
   40 format(/52h the initial fission neutron source distribution was,          !/*kp4a       41*/
     & 32h read from the srctp file named ,a8,1h.)                              !/*kp4a       42*/
!                                                                               !/*kp4a       43*/
!        print 3 lines of information about the keff calculation.               !/*kp4a       44*/
      write(iuo,50)ikz,kct,nsrck,min(kcz,ikz),min(nps,nskk),                    !/*kp4a       45*/
     & max(0,kcz-ikz),max(0,nps-nskk)                                           !/*kp4a       46*/
   50 format(46h the criticality problem was scheduled to skip,i4,              !/*kp4a       47*/
     & 26h cycles and run a total of,i5,22h cycles with nominally,i9,           !/*kp4a       48*/
     & 20h neutrons per cycle./21h this problem has run,i4,                     !/*kp4a       49*/
     & 21h inactive cycles with,i9,22h neutron histories and,i5,                !/*kp4a       50*/
     & 19h active cycles with,i12,19h neutron histories.)                       !/*kp4a       51*/
      if(kcz.eq.kct)write(iuo,60)nps                                            !/*kp4a       52*/
   60 format(//52h this calculation has completed the requested number,         !/*kp4a       53*/
     & 32h of keff cycles using a total of,i12,16h fission neutron,             !/*kp4a       54*/
     & 18h source histories.)                                                   !/*kp4a       55*/
      if(kcz.ne.kct)write(iuo,70)kcz,kct                                        !/*kp4a       56*/
   70 format(//45h this calculation did not finish because only,i5,             !/*kp4a       57*/
     & 17h of the requested,i5,28h keff cycles were completed.)                 !/*kp4a       58*/
!                                                                               !/*kp4a       59*/
!        check for fissionable materials cells without tracks,                  !/*kp4a       60*/
!        collisions, or source points.                                          !/*kp4a       61*/
      ns=0                                                                      !/*kp4a       62*/
      mc=0                                                                      !/*kp4a       63*/
      do 120 j=1,3                                                              !/*kp4a       64*/
      if(nmaz.ne.0)go to 115                                                    !/*kp4b       12*/
      jj=j                                                                      !/*kp4a       65*/
      if(j.ne.1)jj=5-j                                                          !/*kp4a       66*/
      nc(j)=0                                                                   !/*kp4a       67*/
      do 110 i=1,mxa                                                            !/*kp4a       68*/
      if(lfcl(llfc+i).eq.0.or.(j.lt.3.and.pac(lpac+1,jj,i).ne.0.).or.           !/*kp4a       69*/
     & (j.eq.3.and.pwb(lpwb+1,ipwb_external_source,i).ne.0.))go to 110          !/*kpx         8*/
      if(ns.eq.0.and.nc(j).eq.0)write(iuo,80)                                   !/*kp4a       71*/
   80 format(/1x,131(1hx))                                                      !/*kp4a       72*/
      if(nc(j).eq.0)write(iuo,90)hn(j)                                          !/*kp4a       73*/
   90 format(/50h the following cells with fissionable material had,            !/*kp4a       74*/
     & 4h no ,a23)                                                              !/*kp4b       13*/
      ns=1                                                                      !/*kp4a       76*/
      mc=mod(nc(j),20)+1                                                        !/*kp4a       77*/
      nc(j)=nc(j)+1                                                             !/*kp4a       78*/
      nz(mc)=ncl(lncl+i)                                                        !/*kp4a       79*/
      if(mc.eq.20)write(iuo,100)(nz(l),l=1,20)                                  !/*kp4a       80*/
  100 format(1x,20i6)                                                           !/*kp4a       81*/
  110 continue                                                                  !/*kp4a       82*/
      if(mc.gt.0)write(iuo,100)(nz(l),l=1,mc)                                   !/*kp4a       83*/
      if(nc(j).ne.0)call erprnt(1,2,1,nc(j),0,0,0,0,                            !/*kp4b       14*/
     & 'i4,49h fissionable cells had no '//hn(j)(1:23))                         !/*kp4b       15*/
      go to 120                                                                 !/*kp4b       16*/
  115 jj=mod(j,3)+1                                                             !/*kp4b       17*/
      if(mazf(jj).eq.0)go to 120                                                !/*kp4b       18*/
      if(ns.eq.0)write(iuo,80)                                                  !/*kp4b       19*/
      ns=1                                                                      !/*kp4b       20*/
      call erprnt(1,2,1,mazf(jj),0,0,0,0,                                       !/*kp4b       21*/
     & 'i6,53h fission cell elements had no '//hn(j)(1:23))                     !/*kp4b       22*/
  120 continue                                                                  !/*kp4b       23*/
      if(ns.ne.0)write(iuo,122)                                                 !/*kp4b       24*/
  122 format(/45h the keff results could be too small because ,                 !/*kp4b       25*/
     & 49hcells with fissionable material were not sampled.)                    !/*kp4b       26*/
      if(nmaz.eq.0)go to 130                                                    !/*kp4b       27*/
      if(ns.ne.0)write(iuo,124)                                                 !/*kp4b       28*/
  124 format(43h see print table 128 for unsampled repeated,                    !/*kp4b       29*/
     & 39h structures or finite lattice elements.)                              !/*kp4b       30*/
!                                                                               !/*kp4b       31*/
!        warn of infinite lattices.                                             !/*kp4b       32*/
      do 126 i=1,mxa                                                            !/*kp4b       33*/
  126 if(lat(llat+1,i).ne.0.and.mfl(lmfl+1,i).ge.0)write(iuo,128)               !/*kp4b       34*/
     & ncl(lncl+i)                                                              !/*kp4b       35*/
  128 format(46h cannot guarantee sampling of all elements in ,                 !/*kp4b       36*/
     & 21hinfinite lattice cell,i5,1h.)                                         !/*kp4b       37*/
  130 if(ns.eq.0)go to 140                                                      !/*kp4b       38*/
      write(iuo,'(/1x,131(1hx)/)')                                              !/*kp4b       39*/
      go to 160                                                                 !/*kp4a       98*/
!                                                                               !/*kp4a       99*/
!        print that all cells with fissionable material were sampled.           !/*kp4a      100*/
  140 write(iuo,150)                                                            !/*kp4a      101*/
  150 format(53h all cells with fissionable material were sampled and,          !/*kp4a      102*/
     & 35h had fission neutron source points.//)                                !/*kp4a      103*/
      if(nmaz.eq.0.and.nmzu.ne.0)write(iuo,152)                                 !/*kp4b       40*/
  152 format(55h print table 128 off:  cannot determine if all repeated,        !/*kp4b       41*/
     & 44h structures / lattice elements were sampled.//)                       !/*kp4b       42*/
!                                                                               !/*kp4c        6*/
!        check for any weight lost to energy or time cutoffs.                   !/*kp4c        7*/
  160 if(pax(5,2,1).gt.0.)call erprnt(1,2,0,0,0,0,0,0,                          !/*kp4c        8*/
     & '56hweight was lost to energy cutoff in a kcode calculation.')           !/*kp4c        9*/
      if(pax(5,3,1).gt.0.)call erprnt(1,2,0,0,0,0,0,0,                          !/*kp4c       10*/
     & '54hweight was lost to time cutoff in a kcode calculation.')             !/*kp4c       11*/
!                                                                               !/*kp4a      104*/
!        see if at least 4 complete cycles were run for a combined keff.        !/*kp4a      105*/
      if(kcz.le.ikz)go to 440                                                   !/*kp4c       12*/
      mk=min(kcz,mrkp)                                                          !/*kp4a      107*/
      if(kcz-ikz.lt.4)write(iuo,170)kcz-ikz                                     !/*kp4a      108*/
  170 format(48h there is no combined collision/absorption/track,               !/*kp4a      109*/
     & 38h-length estimate for keff because only,i2,                            !/*kp4a      110*/
     & 24h active cycles were run.)                                             !/*kp4a      111*/
      if(kcz-ikz.lt.4)go to 270                                                 !/*kp4a      112*/
!                                                                               !/*kp4a      113*/
!        check decrease(is=1),increase(is=2) in avg comb k last 10 cyc.         !/*kp4a      114*/
      call confid(kcz-ikz-3,cg)                                                 !/*kp4a      115*/
      if(kcz-ikz.lt.10)go to 200                                                !/*kp4a      116*/
      is=1                                                                      !/*kp4a      117*/
      if(rkpl(lrkp+12,mk-8).gt.rkpl(lrkp+12,mk-9))is=2                          !/*kp4a      118*/
      do 180 i=mk-8,mk-1                                                        !/*kp4a      119*/
  180 if(rkpl(lrkp+12,i+1).gt.rkpl(lrkp+12,i).and.is.eq.1.or.                   !/*kp4a      120*/
     & rkpl(lrkp+12,i+1).lt.rkpl(lrkp+12,i).and.is.eq.2)go to 200               !/*kp4a      121*/
      write(iuo,190)hc(is)                                                      !/*kp4a      122*/
  190 format(31h warning.  there appears to be ,a10,13h trend in the,           !/*kp4a      123*/
     & 58h average col/abs/tl keff estimator over the last 10 active,           !/*kp4a      124*/
     & 8h cycles.)                                                              !/*kp4a      125*/
      call erprnt(1,2,0,0,0,0,0,-1,                                             !/*kp4a      126*/
     & '10h'//hc(is)//'43h k(c/a/t) trend over last 10 active cycles.')         !/*kp4a      127*/
!                                                                               !/*kp4a      128*/
!        check cycle keffs to see if they appear normally distributed.          !/*kp4a      129*/
  200 if(kcz-ikz.ge.5)call knorm(1,ikz,hz,zk,je)                                !/*kp4a      130*/
      if(je.eq.3)write(iuo,210)                                                 !/*kp4a      131*/
  210 format(//50h no final keff results will be printed because all,           !/*kp4a      132*/
     & 58h three keff estimator active data sets do not appear to be,           !/*kp4a      133*/
     & 22h normally distributed.//)                                             !/*kp4a      134*/
      if(je.eq.3)go to 270                                                      !/*kp4a      135*/
!                                                                               !/*kp4a      136*/
!        do not print final results unless >29 active cycles were run.          !/*kp4a      137*/
      if(kcz-ikz.lt.30)write(iuo,220)kcz-ikz                                    !/*kp4a      138*/
  220 format(//51h no final keff results will be printed because only,          !/*kp4a      139*/
     & i3,56h active cycles were run, which is less than the required,          !/*kp4a      140*/
     & 18h 30 active cycles.//)                                                 !/*kp4a      141*/
      if(kcz-ikz.lt.30)go to 270                                                !/*kp4a      142*/
!                                                                               !/*kp4a      143*/
!        print final estimated combined keff and standard deviation.            !/*kp4a      144*/
      rk=rkpl(lrkp+12,mk)                                                       !/*kp4a      145*/
      sd=rkpl(lrkp+13,mk)                                                       !/*kp4a      146*/
      write(iuo,230)rk,sd                                                       !/*kp4a      147*/
  230 format(1h ,131(1h-)/4h |  ,127x,1h|/23h | the final estimated ,           !/*kp4a      148*/
     & 49hcombined collision/absorption/track-length keff =,f8.5,               !/*kp4a      149*/
     & 40h with an estimated standard deviation of,f8.5,3x,1h|/                 !/*kp4a      150*/
     & 4h |  ,127x,1h|)                                                         !/*kp4a      151*/
      write(jtty,240)rk,sd                                                      !/*kp4a      152*/
  240 format(/26hfinal k(col/abs/trk len) =,f8.5,5x,9hstd dev =,f8.5/)          !/*kp4a      153*/
!                                                                               !/*kp4a      154*/
!        print estimated keff confidence intervals and prompt lifetimes.        !/*kp4b       43*/
      write(iuo,250)(max(zero,rk-cg(i)*sd),rk+cg(i)*sd,i=1,3)                   !/*kp4a      156*/
  250 format(53h | the estimated 68, 95, & 99 percent keff confidence,          !/*kp4a      157*/
     & 14h intervals are,f8.5,3h to,f8.5,1h,,f8.5,3h to,f8.5,1h,,               !/*kp4a      158*/
     & 4h and,f8.5,3h to,f8.5,2h |)                                             !/*kp4a      159*/
      write(iuo,260)1.e-8*rkpl(lrkp+16,mk),1.e-8*rkpl(lrkp+17,mk)               !/*kp4a      160*/
  260 format(4h |  ,127x,1h|/35h | the final combined (col/abs/tl) ,            !/*kp4b       44*/
     & 25hprompt removal lifetime =,1pe11.4,17h seconds with an ,               !/*kp4b       45*/
     & 31hestimated standard deviation of,e11.4,2h |/4h |  ,127x,1h|/           !/*kp4b       46*/
     & 1h ,131(1h-))                                                            !/*kp4a      164*/
!                                                                               !/*kp4a      165*/
!        print estimates, std. deviations, confidence intervals of keff.        !/*kp4b       47*/
  270 if(kcz-ikz.lt.2)go to 440                                                 !/*kp4a      167*/
      write(iuo,280)                                                            !/*kp4a      168*/
  280 format(//42h the estimated average keffs, one standard,                   !/*kp4a      169*/
     & 60h deviations, and 68, 95, and 99 percent confidence intervals,         !/*kp4a      170*/
     & 5h are://7x,15h keff estimator,6x,5h keff,4x,                            !/*kp4a      171*/
     & 19h standard deviation,5x,15h 68% confidence,7x,                         !/*kp4a      172*/
     & 15h 95% confidence,7x,15h 99% confidence,7x,4hcorr/)                     !/*kp4a      173*/
      call covar(osum,osum2,za,ea,tpp(1),tpp(5),zc,ec,cz,kcz-ikz)               !/*kp4b       48*/
      call confid(kcz-ikz-1,cm)                                                 !/*kp4a      179*/
      if(je.eq.3)write(iuo,300)(hk(i),za(i),ea(i),i=1,3)                        !/*kp4a      180*/
  300 format(7x,a15,f12.5,f16.5)                                                !/*kp4a      181*/
      if(je.ne.3)write(iuo,310)(hk(i),za(i),ea(i),                              !/*kp4a      182*/
     & (max(zero,za(i)-cm(j)*ea(i)),za(i)+cm(j)*ea(i),j=1,3),i=1,3)             !/*kp4a      183*/
  310 format(7x,a15,f12.5,f16.5,f17.5,3h to,f8.5,f11.5,3h to,f8.5,f11.5,        !/*kp4a      184*/
     & 3h to,f8.5)                                                              !/*kp4a      185*/
      if(kcz-ikz.lt.4)go to 350                                                 !/*kp4b       49*/
      call confid(kcz-ikz-2,cc)                                                 !/*kp4b       50*/
      if(je.eq.3)write(iuo,320)(hk(i+3),zc(i),ec(i),zc(i),i=1,3),               !/*kp4b       51*/
     & hk(7),zc(4),ec(4)                                                        !/*kp4b       52*/
  320 format(7x,a15,f12.5,f16.5,75x,f7.4)                                       !/*kp4b       53*/
      if(je.ne.3)write(iuo,340)(hk(i+3),zc(i),ec(i),(max(zero,zc(i)             !/*kp4b       54*/
     & -cc(j)*ec(i)),zc(i)+cc(j)*ec(i),j=1,3),cz(i),i=1,3),hk(7),zc(4),         !/*kp4b       55*/
     & ec(4),(max(zero,zc(4)-cg(j)*ec(4)),zc(4)+cg(j)*ec(4),j=1,3)              !/*kp4b       56*/
  340 format(7x,a15,f12.5,f16.5,f17.5,3h to,f8.5,f11.5,3h to,f8.5,f11.5,        !/*kp4b       57*/
     & 3h to,f8.5,f10.4)                                                        !/*kp4b       58*/
!                                                                               !/*kp4a      202*/
!        find largest and smallest of each active cycle keff estimator.         !/*kp4a      203*/
  350 do 380 j=1,3                                                              !/*kp4a      204*/
      sk=huge                                                                   !/*kp4a      205*/
      bk=-huge                                                                  !/*kp4a      206*/
      do 370 i=ikz+1,mk                                                         !/*kp4a      207*/
      if(rkpl(lrkp+j,i).le.bk)go to 360                                         !/*kp4a      208*/
      bk=rkpl(lrkp+j,i)                                                         !/*kp4a      209*/
      ek(j,1)=bk                                                                !/*kp4a      210*/
      ik(j,1)=i                                                                 !/*kp4a      211*/
  360 if(rkpl(lrkp+j,i).ge.sk)go to 370                                         !/*kp4a      212*/
      sk=rkpl(lrkp+j,i)                                                         !/*kp4a      213*/
      ek(j,2)=sk                                                                !/*kp4a      214*/
      ik(j,2)=i                                                                 !/*kp4a      215*/
  370 continue                                                                  !/*kp4a      216*/
  380 za(j)=ek(j,1)                                                             !/*kp4a      217*/
!                                                                               !/*kp4a      218*/
!        calculate and print keffs if largest keffs occur on next cycle.        !/*kp4a      219*/
      if(kcz-ikz.lt.3)go to 440                                                 !/*kp4a      220*/
      call kskcyc(1,za,ea,je)                                                   !/*kp4a      221*/
      write(iuo,390)                                                            !/*kp4a      222*/
  390 format(//49h if the largest of each keff occurred on the next,            !/*kp4a      223*/
     & 51h cycle, the keff results and 68, 95, and 99 percent,                  !/*kp4a      224*/
     & 31h confidence intervals would be:/)                                     !/*kp4a      225*/
      write(iuo,400)                                                            !/*kp4a      226*/
  400 format(7x,15h keff estimator,6x,5h keff,4x,                               !/*kp4a      227*/
     & 19h standard deviation,5x,15h 68% confidence,7x,                         !/*kp4a      228*/
     & 15h 95% confidence,7x,15h 99% confidence/)                               !/*kp4a      229*/
      call confid(kcz-ikz,cm)                                                   !/*kp4a      230*/
      if(je.eq.3)write(iuo,300)(hk(i),za(i),ea(i),i=1,3)                        !/*kp4a      231*/
      if(je.ne.3)write(iuo,310)(hk(i),za(i),ea(i),                              !/*kp4a      232*/
     & (max(zero,za(i)-cm(j)*ea(i)),za(i)+cm(j)*ea(i),j=1,3),i=1,3)             !/*kp4a      233*/
      call confid(kcz-ikz-2,cg)                                                 !/*kp4a      234*/
      if(je.eq.3)write(iuo,300)hk(7),za(4),ea(4)                                !/*kp4a      235*/
      if(je.ne.3)write(iuo,310)hk(7),za(4),ea(4),                               !/*kp4a      236*/
     & (max(zero,za(4)-cg(j)*ea(4)),za(4)+cm(j)*ea(4),j=1,3)                    !/*kp4a      237*/
!                                                                               !/*kp4a      238*/
!        print estimates, std. deviations, confidence intervals of              !/*kp4b       59*/
!        prompt removal lifetimes.                                              !/*kp4b       60*/
      write(iuo,410)                                                            !/*kp4b       61*/
  410 format(//52h the estimated average prompt removal lifetimes, one,         !/*kp4b       62*/
     & 59h standard deviations, and 68, 95, and 99 percent confidence,          !/*kp4b       63*/
     & 21h intervals are (sec)://7x,9hestimator,6x,8hlifetime,5x,               !/*kp4b       64*/
     & 9hstd. dev.,9x,14h68% confidence,11x,14h95% confidence,11x,              !/*kp4b       65*/
     & 14h99% confidence,11x,4hcorr/)                                           !/*kp4b       66*/
      call covar(rsum,rsum2,za,ea,tpp(1),tpp(5),zc,ec,cz,kcz-ikz)               !/*kp4b       67*/
      if(je.eq.3)write(iuo,412)(hk(i),1.e-8*za(i),1.e-8*ea(i),i=1,3)            !/*kp4b       68*/
  412 format(1x,a15,1p2e14.5)                                                   !/*kp4b       69*/
      if(je.ne.3)write(iuo,414)(hk(i),1.e-8*za(i),1.e-8*ea(i),                  !/*kp4b       70*/
     & (1.e-8*max(zero,za(i)-cm(j)*ea(i)),1.e-8*(za(i)+cm(j)*ea(i)),            !/*kp4b       71*/
     & j=1,3),i=1,3)                                                            !/*kp4b       72*/
  414 format(1x,a15,1p2e14.5,e14.4,3h to,2e11.4,3h to,2e11.4,                   !/*kp4b       73*/
     & 3h to,e11.4)                                                             !/*kp4b       74*/
      if(kcz-ikz.lt.4)go to 420                                                 !/*kp4b       75*/
      if(je.eq.3)write(iuo,416)(hk(i+3),1.e-8*zc(i),1.e-8*ec(i),zc(i),          !/*kp4b       76*/
     & i=1,3),hk(7),1.e-8*zc(4),1.e-8*ec(4)                                     !/*kp4b       77*/
  416 format(1x,a15,1p2e14.5,75x,0pf7.4)                                        !/*kp4b       78*/
      if(je.ne.3)write(iuo,418)(hk(i+3),1.e-8*zc(i),1.e-8*ec(i),(1.e-8          !/*kp4b       79*/
     & *max(zero,zc(i)-cc(j)*ec(i)),1.e-8*(zc(i)+cc(j)*ec(i)),j=1,3),           !/*kp4b       80*/
     & cz(i),i=1,3),hk(7),1.e-8*zc(4),1.e-8*ec(4),(1.e-8                        !/*kp4b       81*/
     & *max(zero,zc(4)-cg(k)*ec(4)),1.e-8*(zc(4)+cg(k)*ec(4)),k=1,3)            !/*kp4b       82*/
  418 format(1x,a15,1p2e14.5,e14.4,3h to,2e11.4,3h to,2e11.4,                   !/*kp4b       83*/
     & 3h to,e11.4,0pf10.4)                                                     !/*kp4b       84*/
!                                                                               !/*kp4b       85*/
!        print escape, capture, and fission lifespans and lifetimes.            !/*kp4b       86*/
  420 tpp(4)=pax(5,loss_all_pls_escape,1)+pax(5,loss_neutron_capture,1)+        !/*kpx        11*/
     &       pax(5,loss_neutron_loss_to_fission,1)                              !/*kpx        12*/
      if(tpp(4).eq.0.)go to 430                                                 !/*kp4b       88*/
      tpp(1)=pax(5,loss_all_pls_escape,1)                                       !/*kpx        13*/
      tpp(2)=pax(5,loss_neutron_capture,1)                                      !/*kpx        14*/
      tpp(3)=pax(5,loss_neutron_loss_to_fission,1)                              !/*kpx        15*/
      tpp(5)=tmav(1,1)                                                          !/*kp4b       92*/
      tpp(6)=tmav(1,2)                                                          !/*kp4b       93*/
      tpp(7)=rlt(4,1)                                                           !/*kp4b       94*/
      tpp(8)=tpp(5)+tpp(6)+tpp(7)                                               !/*kp4b       95*/
      do 422 i=1,4                                                              !/*kp4b       96*/
      tpp(i+8)=zero                                                             !/*kpx        16*/
  422 if(tpp(i).ne.zero)tpp(i+8)=1.e-8/tpp(i)                                   !/*kpx        17*/
      write(iuo,424)(tpp(i+4)*tpp(i+8),i=1,4),(tpp(j)/tpp(4),j=1,4),            !/*kp4b       99*/
     & (za(2)*tpp(4)*tpp(k+8),k=1,4),(zc(4)*tpp(4)*tpp(l+8),l=1,4)              !/*kp4b      100*/
  424 format(/48h absorption estimates of prompt lifetimes (sec)://             !/*kp4b      101*/
     & 29x,6hescape,8x,7hcapture,8x,7hfission,8x,7hremoval//                    !/*kp4b      102*/
     & 12x,8hlifespan,1p4e15.5/12x,8hfraction,4e15.5/                           !/*kp4b      103*/
     & 7x,13hlifetime(abs),4e15.5/5x,15hlifetime(c/a/t),4e15.5/)                !/*kp4b      104*/
!                                                                               !/*kp4c       14*/
!        print perturbed track length estimates of keff.                        !/*kp4c       15*/
      if(npert.eq.0.or.kcz.le.ikz)go to 430                                     !/*kp4c       16*/
      write(iuo,426)                                                            !/*kp4c       17*/
  426 format(//1x,121(1h=)/2h =,119x,1h=/2h =,5x,13hthe following,              !/*kp4c       18*/
     & 57h output gives the predicted changes in keff (track length,            !/*kp4c       19*/
     & 34h estimator) for the perturbations.,10x,1h=/2h =,5x,                   !/*kp4c       20*/
     & 51hthe differential operator method was used to obtain,                  !/*kp4c       21*/
     & 38h these results (1st and/or 2nd order).,25x,1h=,                       !/*kp4c       22*/
     & /2h =,5x,44hwarning:  fundamental eigenfunction (fission,                !/*kp4c2       1*/
     & 43h distribution) approximated as unperturbed.,27x,1h=,                  !/*kp4c2       2*/
     & /2h =,119x,1h=/1x,121(1h=)//                                             !/*kp4c       25*/
     & 10x,12hperturbation,6x,9hk(trk ln),3x,9hstd. dev.)                       !/*kp4c       26*/
      f1=1./(kcz-ikz)                                                           !/*kp4c       27*/
      f2=sqrt(f1/max(1,kcz-ikz-1))                                              !/*kp4c       28*/
      do 428 i=1,npert                                                          !/*kp4c       29*/
  428 write(iuo,'(i20,0pf17.5,f12.5)')i,f1*sump(lsum+npert+i),                  !/*kp4c       30*/
     & f2*sqrt(abs(sump(lsum+2*npert+i)-f1*sump(lsum+npert+i)**2))              !/*kp4c       31*/
!                                                                               !/*kp4a      254*/
!        do batches over cycles of the 3 individual keffs and pass hz.          !/*kp4a      255*/
  430 if(kcz-ikz.gt.3)call kbatch(hz)                                           !/*kp4b      105*/
!                                                                               !/*kp4a      257*/
!        print individual and avg keffs and deviations for the problem.         !/*kp4a      258*/
  440 call ktable                                                               !/*kp4a      259*/
      if(kcz-ikz.lt.2)return                                                    !/*kp4a      260*/
!                                                                               !/*kp4a      261*/
!        print the largest and smallest active cycle keff estimators.           !/*kp4a      262*/
      write(iuo,450)                                                            !/*kp4a      263*/
  450 format(//50h  the largest active cycle keffs by estimator are:,           !/*kp4a      264*/
     & 30x,50h the smallest active cycle keffs by estimator are:/)              !/*kp4a      265*/
      write(iuo,460)(hk(i),ek(i,1),ik(i,1),hk(i),ek(i,2),ik(i,2),i=1,3)         !/*kp4a      266*/
  460 format(13x,a15,f8.5,9h on cycle,i4,44x,a15,f8.5,9h on cycle,i4)           !/*kp4a      267*/
      if(kcz-ikz.lt.7)return                                                    !/*kp4a      268*/
!                                                                               !/*kp4a      269*/
!        create printed plot of keff one deviation confidence interval.         !/*kp4a      270*/
      call prplot(1)                                                            !/*kp4a      271*/
!                                                                               !/*kp4a      272*/
!        calculate and print the keffs and deviations by cycles skipped.        !/*kp4a      273*/
      call kskcyc(2,za,ea,je)                                                   !/*kp4a      274*/
!                                                                               !/*kp4a      275*/
!        print a plot of keff as a function of cycle skipped.                   !/*kp4a      276*/
      if(kcz.gt.10.and.kcz.le.mrkp)call prplot(2)                               !/*kp4a      277*/
      return                                                                    !/*kp4a      278*/
      end                                                                       !/*kp4a      279*/

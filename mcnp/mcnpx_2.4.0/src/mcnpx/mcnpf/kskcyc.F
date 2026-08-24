!#ident kskcyc.F from deck ky4a                                                 !/*ky4a        1*/
      subroutine kskcyc(nw,za,ea,jt)                                            !/*ky4a        2*/
!        calculate and print keff and deviations by cycles skipped.             !/*ky4a        3*/
!        called from subroutine kprint twice with different nw values.          !/*ky4a        4*/
!                                                                               !/*ky4a        5*/
!        ci contains 68, 95, 99% conf int for two halves and final keff.        !/*ky4a        6*/
!        cg are the conf interval multipliers for 68,95,99 percentiles.         !/*ky4a        7*/
!        ea(4)=estimates of standard deviations for four estimators.            !/*ky4a        9*/
!        ea(1)-ea(3) is passed into kskcyc for nw = 1 with largest keff.        !/*ky4a       10*/
!        ea(1)-ea(4) is passed back to kprint for nw = 1 with std devs.         !/*ky4a       11*/
!        nw = 1 for one pass for keffs with largest value on next cycle.        !/*ky4a       12*/
!           = 2 to produce the table keffs by cycles skipped.                   !/*ky4a       13*/
!        os is equivalent to osum, the sum of the individual keffs.             !/*ky4a       14*/
!        o2 is equivalent to osum2, the sum of the keff products.               !/*ky4a       15*/
!        za(4)=avg keff estimators for coll, abs, trk length, combined.         !/*ky4a       16*/
!        za(1) to za(3) is passed into subroutine kskcyc for nw = 1.            !/*ky4a       17*/
!        jt is the number of keff data sets that are normal at 99%.             !/*ky4a       18*/
!        hz = xx/xx/xx col,abs,tl where xx=95,99,or no (for not normal).        !/*ky4a       19*/
!        hz is returned from the call to subroutine knorm.                      !/*ky4a       20*/
!        zk is a dummy for calling knorm.                                       !/*ky4a       21*/
!                                                                               !/*ky4a       22*/
      use global_data                                                           !/*kyx         1*/
!                                                                               !/*kyx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*kyx         3*/
      implicit integer(kindi) (i-n)                                             !/*kyx         4*/
!                                                                               !/*kyx         5*/
      character hc(3)*12,hk(3)*10,hp(3)*2,hs,hz*8                               !/*ky4c        1*/
      dimension ci(2,3,3),cg(3),ea(4),nf(3),os(3),o2(3,3),za(4),zk(3,1)         !/*ky4c2       1*/
      data hc/'  first half',' second half','final result'/,                    !/*ky4a       27*/
     & hk/' collision','absorption','trk length'/,hp/'68','95','99'/            !/*ky4c        2*/
!                                                                               !/*ky4a       29*/
      if(nw.eq.2.or.nw.eq.0)go to 20                                            !/*ky4c        3*/
!                                                                               !/*ky4a       31*/
!        set up for calculation of the keffs with the largest values.           !/*ky4a       32*/
      do 10 i=1,3                                                               !/*ky4a       33*/
      os(i)=osum(i)+za(i)                                                       !/*ky4a       34*/
      do 10 j=1,3                                                               !/*ky4a       35*/
   10 o2(i,j)=osum2(i,j)+za(i)*za(j)                                            !/*ky4a       36*/
      go to 80                                                                  !/*ky4a       37*/
!                                                                               !/*ky4a       38*/
!        do not print table if number of cycles is greater than mrkp.           !/*ky4a       39*/
   20 if(kcz.le.mrkp)go to 40                                                   !/*ky4a       40*/
      write(iuo,30)kcz,mrkp                                                     !/*ky4a       41*/
   30 format(/47h the table of keffs versus the number of cycles,               !/*ky4a       42*/
     & 40h skipped will not be printed because the,i5,7h cycles,                !/*ky4a       43*/
     & 24h exceeds mrkp storage of,i6,1h.)                                      !/*ky4a       44*/
      return                                                                    !/*ky4a       45*/
!                                                                               !/*ky4a       46*/
!        find sums and cross-product matrix by estimator for all cycles.        !/*ky4a       47*/
   40 if(nw.eq.0)go to 310                                                      !/*ky4c        4*/
      do 50 i=1,3                                                               !/*ky4c        5*/
      os(i)=0.                                                                  !/*ky4a       49*/
      do 50 j=1,3                                                               !/*ky4a       50*/
   50 o2(i,j)=0.                                                                !/*ky4a       51*/
      do 60 j=1,3                                                               !/*ky4a       52*/
      do 60 i=1,kcz                                                             !/*ky4a       53*/
      os(j)=os(j)+rkpl(lrkp+j,i)                                                !/*ky4a       54*/
      do 60 k=1,3                                                               !/*ky4a       55*/
   60 o2(j,k)=o2(j,k)+rkpl(lrkp+j,i)*rkpl(lrkp+k,i)                             !/*ky4a       56*/
      ns=nps+nsa-nint(rkpl(lrkp+18,kcz+1))                                      !/*ky4a       57*/
      sd=huge                                                                   !/*ky4a       58*/
!                                                                               !/*ky4a       59*/
!        print the header for the keff table by cycles skipped.                 !/*ky4a       60*/
      write(iuo,70)                                                             !/*ky4a       61*/
   70 format(55h1individual and collision/absorption/track-length keffs,        !/*ky4a       62*/
     & 61h for different numbers of inactive cycles skipped for fission,        !/*ky4a       63*/
     & 16h source settling//37h   skip  active     active    average,           !/*ky4a       64*/
     & 53h keff estimators and deviations     normality average,                !/*ky4a       65*/
     & 41h k(c/a/t)   k(c/a/t) confidence intervals/16h  cycles cycles ,        !/*ky4a       66*/
     & 56h  neutrons  k(col) st dev  k(abs) st dev  k(trk) st dev ,             !/*ky4a       67*/
     & 60h co/ab/tl  k(c/a/t) st dev   95% confidence   99% confidence/)        !/*ky4a       68*/
      ln=0                                                                      !/*ky4a       69*/
      ms=(kcz+1)/119+1                                                          !/*ky4a       70*/
      nc=0                                                                      !/*ky4c        6*/
!                                                                               !/*ky4a       71*/
!        get covariance matrix for l active cycles: start is all cycles.        !/*ky4a       72*/
   80 do 190 l=kcz,2,-1                                                         !/*ky4a       73*/
      lg=l                                                                      !/*ky4a       74*/
      if(nw.eq.1)lg=kcz+1-ikz                                                   !/*ky4a       75*/
      call covar(os,o2,za,ea,tpp(1),tpp(5),tpp(9),tpp(13),tpp(17),lg)           !/*ky4b        1*/
      za(4)=tpp(12)                                                             !/*ky4b        2*/
      ea(4)=tpp(16)                                                             !/*ky4b        3*/
!                                                                               !/*ky4a       87*/
!        print only the three keff estimators for 2 or 3 active cycles.         !/*ky4a       88*/
      if(l.gt.3.or.nw.eq.1)go to 140                                            !/*ky4b        4*/
      hs=' '                                                                    !/*ky4a       90*/
      if(kcz-l.eq.ikz)hs='*'                                                    !/*ky4a       91*/
      write(iuo,160)kcz-l,l,hs,ns,(za(i),ea(i),i=1,3)                           !/*ky4a       92*/
      ln=ln+1                                                                   !/*ky4a       93*/
      if(mod(ln,10).eq.1.and.ln.ne.1)write(iuo,'(1x,131(1h-))')                 !/*ky4a       94*/
      go to 170                                                                 !/*ky4a       95*/
!                                                                               !/*ky4a      137*/
!        return with calculated keffs using largest keffs on next cycle.        !/*ky4a      138*/
  140 if(nw.eq.1)return                                                         !/*ky4a      139*/
!                                                                               !/*ky4a      140*/
!        save skipped cycle combined keff and deviation for later use.          !/*ky4a      141*/
      rkpl(lrkp+14,kcz+1-l)=za(4)                                               !/*ky4a      142*/
      rkpl(lrkp+15,kcz+1-l)=ea(4)                                               !/*ky4a      143*/
!                                                                               !/*ky4a      144*/
!        find the active cycle for minimum deviation on combined keff.          !/*ky4a      145*/
      if(ea(4).gt.sd)go to 150                                                  !/*ky4a      146*/
      sd=ea(4)                                                                  !/*ky4a      147*/
      if(l.gt.30)nc=l                                                           !/*ky4c        7*/
!                                                                               !/*ky4a      149*/
!        print average keff results for l active cycles every ms values.        !/*ky4a      150*/
  150 if(mod(l,ms).ne.0.and.kcz-l.ne.ikz.and.kcz-l.gt.20.and.                   !/*ky4a      151*/
     & l.ne.kcz+1-ikz-(kcz-ikz)/2-1)go to 170                                   !/*ky4a      152*/
      hs=' '                                                                    !/*ky4a      153*/
      if(kcz-l.eq.ikz)hs='*'                                                    !/*ky4a      154*/
!                                                                               !/*ky4a      155*/
!        return hz with the results of the normality tests.                     !/*ky4a      156*/
      call knorm(2,kcz-l,hz,zk,je)                                              !/*ky4a      157*/
      call confid(l-3,cg)                                                       !/*ky4a      158*/
      write(iuo,160)kcz-l,l,hs,ns,(za(i),ea(i),i=1,3),hz,za(4),ea(4),           !/*ky4a      159*/
     & (max(zero,za(4)-cg(j)*ea(4)),za(4)+cg(j)*ea(4),j=2,3)                    !/*ky4a      160*/
  160 format(i6,i7,a1,i12,1h|,3(2f7.4,1x),1h|,a8,1h|,2f8.5,                     !/*ky4a      161*/
     & f9.5,1h-,f7.5,f9.5,1h-,f7.5)                                             !/*ky4a      162*/
      ln=ln+1                                                                   !/*ky4a      163*/
      if(mod(ln,10).eq.1.and.ln.ne.1)write(iuo,'(1x,131(1h-))')                 !/*ky4a      164*/
!                                                                               !/*ky4a      165*/
!        subtract off contributions from the next cycle to be skipped.          !/*ky4a      166*/
  170 do 180 j=1,3                                                              !/*ky4a      167*/
      os(j)=os(j)-rkpl(lrkp+j,kcz+1-l)                                          !/*ky4a      168*/
      do 180 k=1,3                                                              !/*ky4a      169*/
  180 o2(j,k)=o2(j,k)-rkpl(lrkp+j,kcz+1-l)*rkpl(lrkp+k,kcz+1-l)                 !/*ky4a      170*/
  190 ns=ns-nint(rkpl(lrkp+18,kcz+1-l))                                         !/*ky4a      171*/
!                                                                               !/*ky4a      172*/
!        print the cycle number for the minimum combined keff deviation.        !/*ky4a      173*/
      if(nc.gt.0)write(iuo,200)kcz-nc,nc                                        !/*ky4c        8*/
  200 format(//49h the minimum estimated standard deviation for the,            !/*ky4a      175*/
     & 38h col/abs/tl keff estimator occurs with,i4,16h inactive cycles,        !/*ky4a      176*/
     & 4h and,i5,15h active cycles.)                                            !/*ky4a      177*/
      if(kcz-ikz.lt.12)return                                                   !/*ky4a      178*/
!                                                                               !/*ky4a      179*/
!        compare combined keffs for 1st and 2nd halves of the problem.          !/*ky4a      180*/
!        this test uses at least 6 active cycles for each half.                 !/*ky4a      181*/
      l=ikz+(kcz-ikz)/2-1                                                       !/*ky4a      182*/
      do 210 i=1,2                                                              !/*ky4a      183*/
      za(i)=rkpl(lrkp+2*i+10,l+i)                                               !/*ky4a      184*/
  210 ea(i)=rkpl(lrkp+2*i+11,l+i)                                               !/*ky4a      185*/
      za(3)=rkpl(lrkp+12,kcz)                                                   !/*ky4a      186*/
      ea(3)=rkpl(lrkp+13,kcz)                                                   !/*ky4a      187*/
      write(iuo,220)ikz,l-ikz+1,l+1,kcz-l-1                                     !/*ky4a      188*/
  220 format(//43h the first active half of the problem skips,i3,               !/*ky4a      189*/
     & 16h cycles and uses,i4,37h active cycles; the second half skips,         !/*ky4a      190*/
     & i4,9h and uses,i4,8h cycles./30h the col/abs/trk-len keff, one,          !/*ky4a      191*/
     & 57h standard deviation, and 68, 95, and 99 percent intervals,            !/*ky4a      192*/
     & 41h for each active half of the problem are://13x,8h problem,7x,         !/*ky4a      193*/
     & 5h keff,4x,19h standard deviation,6x,15h 68% confidence,8x,              !/*ky4a      194*/
     & 15h 95% confidence,8x,15h 99% confidence/)                               !/*ky4a      195*/
!                                                                               !/*ky4a      196*/
!        store degrees of freedom for first and second halves and final.        !/*ky4a      197*/
      nf(1)=l-ikz-2                                                             !/*ky4a      198*/
      nf(2)=kcz-l-4                                                             !/*ky4a      199*/
      nf(3)=kcz-ikz-3                                                           !/*ky4a      200*/
!                                                                               !/*ky4a      201*/
!        find conf int multipliers and print both halves and final keff.        !/*ky4a      202*/
      do 230 i=1,3                                                              !/*ky4a      203*/
      call confid(nf(i),cg)                                                     !/*ky4a      204*/
      do 230 j=1,3                                                              !/*ky4a      205*/
      ci(1,j,i)=max(zero,za(i)-cg(j)*ea(i))                                     !/*ky4a      206*/
  230 ci(2,j,i)=za(i)+cg(j)*ea(i)                                               !/*ky4a      207*/
      write(iuo,240)(hc(i),za(i),ea(i),((ci(k,j,i),k=1,2),j=1,3),i=1,2)         !/*ky4a      208*/
      if(jt.eq.3)write(iuo,240)hc(3),za(3),ea(3)                                !/*ky4a      209*/
      if(jt.ne.3)write(iuo,240)hc(3),za(3),ea(3),                               !/*ky4a      210*/
     & ((ci(k,j,3),k=1,2),j=1,3)                                                !/*ky4a      211*/
  240 format(7x,a15,f12.5,f16.5,f18.5,3h to,f8.5,f12.5,3h to,f8.5,f12.5,        !/*ky4a      212*/
     & 3h to,f8.5)                                                              !/*ky4a      213*/
!                                                                               !/*ky4a      214*/
!        check if estimated standard deviations for both halves agree.          !/*ky4a      215*/
!        this is a very approximate f test for standard deviations.             !/*ky4a      216*/
      if(ea(1).lt.1.e-6.and.ea(2).lt.1.e-6)go to 260                            !/*ky4c        9*/
      ra=ea(1)/ea(2)                                                            !/*ky4a      217*/
      rg=max(ra,1./ra)                                                          !/*ky4a      218*/
      if(rg.lt.1.+max(zero+0.6,4.*max(ea(1),ea(2))))go to 260                   !/*ky4a      219*/
      call erprnt(1,2,0,0,0,0,0,-1,                                             !/*ky4a      220*/
     & '58hratio of standard deviations for problem halves too large.')         !/*ky4a      221*/
      write(iuo,250)rg                                                          !/*ky4a      222*/
  250 format(/46h warning.  the ratio of the estimated standard,                !/*ky4a      223*/
     & 48h deviations for the two halves of the problem is,f8.3,                !/*ky4a      224*/
     & 26h, which appears too large.)                                           !/*ky4a      225*/
!                                                                               !/*ky4a      226*/
!        check if first,second half keffs are same at 68,95,99% levels.         !/*ky4a      227*/
  260 dk=abs(za(1)-za(2))                                                       !/*ky4a      228*/
      ek=sqrt(ea(1)**2+ea(2)**2)                                                !/*ky4a      229*/
!                                                                               !/*ky4a      230*/
!        there are kcz-ikz-6 degrees of freedom in ek.                          !/*ky4a      231*/
      call confid(kcz-ikz-6,cg)                                                 !/*ky4a      232*/
      do 270 j=1,3                                                              !/*ky4a      233*/
  270 if(dk-cg(j)*ek.lt.0.)go to 290                                            !/*ky4a      234*/
      call erprnt(1,2,0,0,0,0,0,-1,                                             !/*ky4a      235*/
     & '57hratio of k(col/abs/trk len) for problem halves too large.')          !/*ky4a      236*/
      write(iuo,280)                                                            !/*ky4a      237*/
  280 format(/46h warning.  the first and second half values of,                !/*ky4a      238*/
     & 60h k(col/abs/trk len) appear to be different at the 99 percent,         !/*ky4a      239*/
     & 18h confidence level.)                                                   !/*ky4a      240*/
      return                                                                    !/*ky4a      241*/
!                                                                               !/*ky4a      242*/
!        the confidence intervals overlap at the hp(j) level.                   !/*ky4a      243*/
  290 write(iuo,300)hp(j)                                                       !/*ky4a      244*/
  300 format(/48h the first and second half values of k(collision,              !/*ky4a      245*/
     & 55h/absorption/track length) appear to be the same at the ,a2,           !/*ky4a      246*/
     & 26h percent confidence level.)                                           !/*ky4a      247*/
      return                                                                    !/*ky4a      248*/
  310 continue                                                                  !/*kyx         8*/
      end                                                                       !/*ky4a      249*/

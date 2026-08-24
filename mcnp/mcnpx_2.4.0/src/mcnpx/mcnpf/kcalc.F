!#ident kcalc.F from deck kc                                                    !/*kc          1*/
      subroutine kcalc                                                          !/*kc          2*/
!        calculate and print keffs and prompt removal lifetimes and             !/*kc4b        1*/
!        prepare for the next cycle.                                            !/*kc4b        2*/
!                                                                               !/*kc4b        3*/
!        zz = keff (col,abs,tl) for cycle kcy.                                  !/*kc4b        4*/
!        za = keff simple average over mc cycles = osum/mc                      !/*kc4b        5*/
!        zb = keff simple combined averages (c/a),(a/t),(c/t),(c/a/t).          !/*kc4b        6*/
!        zc = keff covariance-weighted averages.                                !/*kc4b        7*/
!        cz = keff covariances (c/a),(a/t),(c/t),(c/a/t).                       !/*kc4b        8*/
!        ea,eb,ec = standard deviations for keff averages za,zb,zc.             !/*kc4b        9*/
!        rl,ra,rb,rc = prompt removal lifetimes averaged as above.              !/*kc4b       10*/
!        cr = prompt removal lifetime covariances.                              !/*kc4b       11*/
!        sa,sb,sc = standard deviations for prompt removal lifetimes.           !/*kc4b       12*/
      use global_data                                                           !/*kcx         1*/
!                                                                               !/*kcx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*kcx         3*/
      implicit integer(kindi) (i-n)                                             !/*kcx         4*/
!                                                                               !/*kcx         5*/
      character ha(2)*9,hf*8                                                    !/*kc4c        1*/
      dimension zz(3),za(3),zb(4),zc(4),ea(3),eb(4),ec(4),cz(3),                !/*kc4b       15*/
     & rl(3),ra(3),rb(4),rc(4),sa(3),sb(4),sc(4),cr(3)                          !/*kc4b       16*/
      data ha/'generated','used     '/                                          !/*kc4c        2*/
      logical lf                                                                !/*kc4c        3*/
      lf=ink(175).ne.0.or.mod(kcy,100).eq.0                                     !/*kc4c        4*/
!                                                                               !/*kc4b       17*/
      if(nss.eq.0)call expire(0,'kcalc',                                        !/*kc4b       18*/
     & 'no new source points were generated.')                                  !/*kc4b       19*/
      if(kcy.eq.mrkp+1)call erprnt(1,2,0,0,0,0,0,0,                             !/*kc4b       20*/
     & '41hno more room in rkpl for kcode plot data.')                          !/*kc4b       21*/
      mc=kcy-ikz                                                                !/*kc4b       22*/
      f0=mc                                                                     !/*kc4b       23*/
      kc=min(kcy,mrkp)                                                          !/*kc4c        5*/
!                                                                               !/*kc4b       24*/
!        calculate 3 estimates (col,abs,tl) of keff and lifetimes.              !/*kc4b       25*/
      do 10 i=1,3                                                               !/*kc4b       26*/
      rl(i)=rlt(i,1)                                                            !/*kc4b       27*/
      if(rlt(i,2).gt.0.)rl(i)=rlt(i,1)/rlt(i,2)                                 !/*kc4b       28*/
      zz(i)=sumk(i)/nsrck                                                       !/*kc4b       30*/
   10 rkpl(lrkp+i,kc)=zz(i)                                                     !/*kc4c        6*/
      rkpl(lrkp+4,kc)=rl(1)                                                     !/*kc4c        7*/
      rkpl(lrkp+5,kc)=rl(2)                                                     !/*kc4c        8*/
      if(kcy.ge.mrkp)rkpl(lrkp+18,mrkp-1)=rkpl(lrkp+18,mrkp)                    !/*kc4b       34*/
      rkpl(lrkp+18,min(kcy+1,mrkp))=nss                                         !/*kc4b       35*/
      rkk=zz(1)                                                                 !/*kc4b       36*/
!                                                                               !/*kc4b       37*/
!        print kcode pre-settling messages.                                     !/*kc4c        9*/
      if(kcy.eq.1)write(iuo,20)                                                 !/*kc4c       11*/
   20 format(32h1estimated keff results by cycle,72x,15hprint table 175)        !/*kc4c       12*/
      if(kcy.eq.1)write(jtty,25)                                                !/*kc4c       13*/
   25 format(/6h cycle,4x,6hk(col),7x,3hctm,5x,6hactive,5x,6hk(col),            !/*kc4c       14*/
     & 4x,7hstd dev,8x,3hfom)                                                   !/*kc4c       15*/
      if(mc.le.1.and.lf)write(iuo,30)kcy,rkk,rl(2),nss                          !/*kc4c       16*/
   30 format(/6h cycle,i6,4x,12hk(collision),f10.6,4x,14hprompt removal,        !/*kc4c       17*/
     & 14h lifetime(abs),1pe12.4,4x,23hsource points generated,i7)              !/*kc4c       18*/
      if(mc.le.1)write(jtty,35)kcy,rkk,cts/60                                   !/*kc4c       19*/
   35 format(i6,4f10.5)                                                         !/*kc4c       20*/
      if(mc.eq.0)write(jtty,40)                                                 !/*kc4c       21*/
   40 format(1x,20(1h*),2x,24hbegin active keff cycles,2x,21(1h*))              !/*kc4c       22*/
      if(mc.le.0)go to 180                                                      !/*kc4c       23*/
!                                                                               !/*kc4c       31*/
!        sum zz,rl over mc cycles.  osum,rsum = simple sums.                    !/*kc4c       32*/
!        osum2,rsum2 = cross product sums.                                      !/*kc4c       33*/
   60 do 70 i=1,3                                                               !/*kc4c       34*/
      osum(i)=osum(i)+zz(i)                                                     !/*kc4b       57*/
      rsum(i)=rsum(i)+rl(i)                                                     !/*kc4b       58*/
      do 70 j=1,3                                                               !/*kc4b       59*/
      osum2(i,j)=osum2(i,j)+zz(i)*zz(j)                                         !/*kc4b       60*/
   70 rsum2(i,j)=rsum2(i,j)+rl(i)*rl(j)                                         !/*kc4b       61*/
!                                                                               !/*kc4c       35*/
!        accumulate perturbed track length estimates of keff.                   !/*kc4c       36*/
      f1=1./nsrck                                                               !/*kc4c       37*/
      do 75 i=1,npert                                                           !/*kc4c       38*/
      sump(lsum+npert+i)=sump(lsum+npert+i)+f1*sump(lsum+i)                     !/*kc4c       39*/
      sump(lsum+2*npert+i)=sump(lsum+2*npert+i)+(f1*sump(lsum+i))**2            !/*kc4c       40*/
   75 sump(lsum+i)=0.                                                           !/*kc4c       41*/
!                                                                               !/*kc4b       62*/
!        calculate simple and covariance-weighted averages of keff              !/*kc4b       63*/
!        and prompt removal lifetimes.                                          !/*kc4b       64*/
      if(mc.le.1)go to 180                                                      !/*kc4b       65*/
      call covar(osum,osum2,za,ea,zb,eb,zc,ec,cz,mc)                            !/*kc4b       66*/
      call covar(rsum,rsum2,ra,sa,rb,sb,rc,sc,cr,mc)                            !/*kc4b       67*/
      rkpl(lrkp+12,kc)=zc(4)                                                    !/*kc4c       42*/
      rkpl(lrkp+13,kc)=ec(4)                                                    !/*kc4c       43*/
      rkpl(lrkp+16,kc)=rc(4)                                                    !/*kc4c       44*/
      rkpl(lrkp+17,kc)=sc(4)                                                    !/*kc4c       45*/
      do 80 i=1,3                                                               !/*kc4b       74*/
      rkpl(lrkp+2*i+4,kc)=za(i)                                                 !/*kc4c       46*/
   80 rkpl(lrkp+2*i+5,kc)=ea(i)                                                 !/*kc4c       47*/
!                                                                               !/*kc4b       86*/
!        convert standard deviations to relative errors.                        !/*kc4b       87*/
      do 90 i=1,3                                                               !/*kc4b       88*/
      if(za(i).ne.0.)ea(i)=ea(i)/za(i)                                          !/*kc4b       89*/
   90 if(ra(i).ne.0.)sa(i)=sa(i)/ra(i)                                          !/*kc4b       90*/
      do 100 i=1,4                                                              !/*kc4b       91*/
      if(zb(i).ne.0.)eb(i)=eb(i)/zb(i)                                          !/*kc4b       92*/
      if(zc(i).ne.0.)ec(i)=ec(i)/zc(i)                                          !/*kc4b       93*/
      if(rb(i).ne.0.)sb(i)=sb(i)/rb(i)                                          !/*kc4b       94*/
  100 if(rc(i).ne.0.)sc(i)=sc(i)/rc(i)                                          !/*kc4b       95*/
!                                                                               !/*kc4c       48*/
!        get the figure of merits, fc(col/abs/tl) and fm(col).                  !/*kc4c       49*/
      xe=ea(1)                                                                  !/*kc4c       50*/
      fm=(cts-cpk)*xe**2                                                        !/*kc4c       52*/
      if(fm.ne.0.)fm=60./fm                                                     !/*kc4c       53*/
      if(fm.le.9999999..and.fm.ge.9.95)write(hf,'(i8)')nint(fm)                 !/*kc4c       54*/
      if(fm.gt.9999999..or.fm.lt.9.95)write(hf,'(1pe8.1)')fm                    !/*kc4c       55*/
      fc=(cts-cpk)*ec(4)**2                                                     !/*kc4c       56*/
      if(fc.ne.0.)fc=60.*zc(4)**2/fc                                            !/*kc4c       57*/
      rkpl(lrkp+19,kc)=fc                                                       !/*kc4c       58*/
!                                                                               !/*kc4b       96*/
!        print cycle summary of collision keff estimator to terminal.           !/*kc4b       97*/
      write(jtty,105)kcy,zz(1),cts/60.,mc,za(1),ea(1)*za(1),hf                  !/*kc4c       60*/
  105 format(i6,2f10.5,i11,2f11.5,3x,a8)                                        !/*kc4c       61*/
      ll=1                                                                      !/*kc4c       62*/
      nn=nss                                                                    !/*kc4c       63*/
!                                                                               !/*kc4b      100*/
!        print individual and average keffs for this keff cycle.                !/*kc4b      101*/
  130 if(lf.or.kct-kcy.lt.5)write(iuo,135)kcy,mc,                               !/*kc4c       79*/
     & (zz(i),za(i),ea(i),zb(i),eb(i),zc(i),ec(i),cz(i),i=1,3)                  !/*kc4c       80*/
  135 format(/11h  estimator,5x,5hcycle,i6,3x,6have of,i6,7h cycles,6x,         !/*kc4c       81*/
     & 11hcombination,9x,14hsimple average,4x,16hcombined average,5x,           !/*kc4b      106*/
     & 4hcorr/13h k(collision),f14.6,f15.6,f7.4,5x,10hk(col/abs),6x,            !/*kc4b      107*/
     & f12.6,f7.4,f13.6,f7.4,f9.4/14h k(absorption),f13.6,f15.6,f7.4,5x,        !/*kc4b      108*/
     & 12hk(abs/tk ln),4x,f12.6,f7.4,f13.6,f7.4,f9.4/                           !/*kc4b      109*/
     & 14h k(trk length),f13.6,f15.6,f7.4,5x,12hk(tk ln/col),4x,                !/*kc4b      110*/
     & f12.6,f7.4,f13.6,f7.4,f9.4)                                              !/*kc4b      111*/
!                                                                               !/*kc4b      112*/
!        print prompt removal lifetime information.                             !/*kc4b      113*/
      if(.not.lf.and.kct-kcy.ge.5)go to 180                                     !/*kc4c       82*/
      if(mc.lt.4)write(iuo,140)rl(1),ra(1),sa(1)                                !/*kc4b      115*/
  140 format(14h rem life(col),1pe13.4,e15.4,0pf7.4)                            !/*kc4b      116*/
      if(mc.ge.4)write(iuo,150)rl(1),ra(1),sa(1),zb(4),eb(4),zc(4),ec(4)        !/*kc4b      117*/
  150 format(14h rem life(col),1pe13.4,e15.4,0pf7.4,                            !/*kc4b      118*/
     & 5x,16hk(col/abs/tk ln),f12.6,f7.4,f13.6,f7.4)                            !/*kc4b      119*/
      if(mc.lt.4)write(iuo,160)rl(2),ra(2),sa(2),rb(1),                         !/*kc4c       83*/
     & sb(1),rc(1),sc(1),cr(1),ha(ll),nn                                        !/*kc4c       84*/
  160 format(14h rem life(abs),1pe13.4,e15.4,0pf7.4,                            !/*kc4b      122*/
     & 5x,16hlife(col/abs)   ,1pe12.4,0pf7.4,1pe13.4,0pf7.4,f9.4/               !/*kc4b      123*/
     & 15h source points ,a9,i7)                                                !/*kc4c       85*/
      if(mc.ge.4)write(iuo,170)rl(2),ra(2),sa(2),                               !/*kc4c       86*/
     & rb(4),sb(4),rc(4),sc(4),ha(ll),nn                                        !/*kc4c       87*/
  170 format(14h rem life(abs),1pe13.4,e15.4,0pf7.4,                            !/*kc4b      127*/
     & 5x,16hlife(col/abs/tl),1pe12.4,0pf7.4,1pe13.4,0pf7.4/                    !/*kc4b      128*/
     & 15h source points ,a9,i7)                                                !/*kc4c       88*/
!                                                                               !/*kc4b      130*/
!        prepare for the next cycle.                                            !/*kc4b      131*/
  180 kcz=kcy                                                                   !/*kcx        18*/
      nsa=nss                                                                   !/*kc        153*/
      nss=0                                                                     !/*kc        154*/
      if(kcy.lt.ikz)nskk=nskk+nsa                                               !/*kc4a      136*/
      do 190 i=1,3                                                              !/*kc4b      133*/
      rlt(i,1)=0.                                                               !/*kc4b      134*/
      rlt(i,2)=0.                                                               !/*kc4b      135*/
  190 sumk(i)=0.                                                                !/*kc4b      136*/
      wt0=(nsrck+zero)/nsa                                                      !/*kc        161*/
      if(nsa.gt.msrk)kcsf=1                                                     !/*kc4c2       1*/
      if(kcsf.eq.1)go to 240                                                    !/*kc        162*/
!                                                                               !/*kc        163*/
!        reorder fso if multitasking requires it.                               !/*kc        164*/
      if(ntasks.le.1)go to 230                                                  !/*kc        165*/
  200 k=ixak                                                                    !/*kc        166*/
      m=0                                                                       !/*kc        167*/
      do 220 i=1,nsa-1                                                          !/*kc        168*/
      l=k+1                                                                     !/*kc        169*/
      if(k.eq.msrk)l=1                                                          !/*kc        170*/
      if(fso(lfso+6*k).le.fso(lfso+6*l))go to 220                               !/*kc        171*/
      m=1                                                                       !/*kc        172*/
      do 210 j=1,6                                                              !/*kc        173*/
      t=fso(lfso+6*(k-1)+j)                                                     !/*kc        174*/
      fso(lfso+6*(k-1)+j)=fso(lfso+6*(l-1)+j)                                   !/*kc        175*/
  210 fso(lfso+6*(l-1)+j)=t                                                     !/*kc        176*/
  220 k=l                                                                       !/*kc        177*/
      if(m.ne.0)go to 200                                                       !/*kc        178*/
!                                                                               !/*kc        179*/
!        turn off flag if settling cycles are all done.                         !/*kc        180*/
  230 if(kcy.eq.ikz)cpk=cts                                                     !/*kcx        22*/
      if(ksdef.ne.0)ksdef=-1                                                    !/*kc4a      143*/
      go to 250                                                                 !/*kc4b      137*/
!                                                                               !/*kc        185*/
!        if source overran in the first cycle, use initial source again.        !/*kc        186*/
  240 kcsf=2                                                                    !/*kc        187*/
      ist=1                                                                     !/*kc        188*/
      nsa=nsrck                                                                 !/*kc        189*/
      if(ksdef.le.0)call ksrctp(2)                                              !/*kc4a      144*/
!                                                                               !/*kc4b      138*/
!        zero out summary table information at start of active keff             !/*kc4c       92*/
!        cycles.                                                                !/*kcx        24*/
  250 if(kcy.ne.ikz.or.kc8.le.0)return                                          !/*kcx        27*/
      kc8=-1                                                                    !/*kcx        28*/
      do 260 i=1,6                                                              !/*kc4b      144*/
      do 260 j=1,items_pax_total                                                !/*kcx        30*/
  260 pax(i,j,1)=zero                                                           !/*kcx        31*/
      do 270 i=1,10                                                             !/*kc4b      147*/
      do 270 j=1,mxa                                                            !/*kc4b      148*/
  270 pac(lpac+1,i,j)=zero                                                      !/*kcx        32*/
      do 280 i=1,8                                                              !/*kc4c2       3*/
      do 280 j=1,npn                                                            !/*kc4b      151*/
  280 pan(lpan+1,i,j)=zero                                                      !/*kcx        33*/
      do 290 i=1,items_pwb_total                                                !/*kcx        34*/
      do 290 j=1,mxa                                                            !/*kc4b      154*/
  290 pwb(lpwb+1,i,j)=zero                                                      !/*kcx        35*/
      do 300 i=1,7                                                              !/*kc4c2       5*/
  300 smul(i)=0.                                                                !/*kc4c2       6*/
      do 310 i=1,3                                                              !/*kc4c2       7*/
  310 tmav(1,i)=0.                                                              !/*kc4c2       8*/
      rlt(4,1)=0.                                                               !/*kc4b      159*/
      coll(1)=0.                                                                !/*kc4b      160*/
      return                                                                    !/*kc        191*/
      end                                                                       !/*kc        192*/

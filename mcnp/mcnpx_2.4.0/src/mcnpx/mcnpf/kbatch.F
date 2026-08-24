!#ident kbatch.F from deck kb4a                                                 !/*kb4a        1*/
      subroutine kbatch(hz)                                                     !/*kb4a        2*/
!        calculate and print batched cycle keffs to reduce correlations.        !/*kb4a        3*/
!        called from subroutine kprint with at least 4 active cycles.           !/*kb4a        4*/
!                                                                               !/*kb4a        5*/
!        cg are confidence interval multipliers for 68,95,99 percents.          !/*kb4a        6*/
!        nb is the maximum allowed number of different batches.                 !/*kb4a        8*/
!        nc is the maximum allowed number of cycle collapses in a batch.        !/*kb4a        9*/
!        np is the maximum number of batch lines for a printed table.           !/*kb4a       10*/
!        os is equivalent to osum, the sum of the individual keffs.             !/*kb4a       11*/
!        o2 is equivalent to osum2, the sum of the keff products.               !/*kb4a       12*/
!        za and ea are the keff and deviations for the four estimators.         !/*kb4a       13*/
!        zb is the keff and deviation for 4 estimators over nb cycles.          !/*kb4a       14*/
!        zk contains the batched values of keff for three estimators.           !/*kb4a       15*/
!        hz brings in the normality test results for the base problem.          !/*kb4a       16*/
!        hv = xx/xx/xx col,abs,tl where xx=95,99,or no (for not normal).        !/*kb4a       17*/
!        hv is returned from the call to subroutine knorm.                      !/*kb4a       18*/
!                                                                               !/*kb4a       19*/
      use global_data                                                           !/*kbx         1*/
!                                                                               !/*kbx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*kbx         3*/
      implicit integer(kindi) (i-n)                                             !/*kbx         4*/
!                                                                               !/*kbx         5*/
      parameter (nb=120,np=54,nc=1001)                                          !/*kb4a       21*/
      character hv(nb)*8,hy(3)*16,hz*8                                          !/*kb4a       22*/
      dimension cg(3),ea(4),os(3),o2(3,3),za(4),zb(4,2,nb),iz(2,nb),            !/*kb4b        1*/
     & zk(3,nc)                                                                 !/*kb4b        2*/
      data hy/' ','col/abs/tl keff','k(c/a/t) st dev'/                          !/*kb4a       25*/
!                                                                               !/*kb4a       26*/
!        do not do the batch table if mrkp is too small.                        !/*kb4a       27*/
      if(kcz.le.mrkp)go to 20                                                   !/*kb4a       28*/
      write(iuo,10)kcz,mrkp                                                     !/*kb4a       29*/
   10 format(//53h the table of batched cycle keffs will not be printed,        !/*kb4a       30*/
     & 12h because the,i5,31h cycles exceeds mrkp storage of,i6,1h.)            !/*kb4a       31*/
      return                                                                    !/*kb4a       32*/
!                                                                               !/*kb4a       33*/
!        create keff batches summed over ik cycles if a factor of mb.           !/*kb4a       34*/
   20 kp=0                                                                      !/*kb4a       35*/
      lz=1                                                                      !/*kb4a       36*/
      mb=kcz-ikz                                                                !/*kb4a       37*/
      do 200 ik=2,mb/2                                                          !/*kb4a       38*/
      nk=mb/ik                                                                  !/*kb4a       39*/
      if(mod(mb,ik).ne.0.or.nk.gt.nc)go to 200                                  !/*kb4a       40*/
!                                                                               !/*kb4a       41*/
!        print title lines for the batched over cycle keff estimators.          !/*kb4a       42*/
      lp=0                                                                      !/*kb4a       43*/
      if(nk.gt.np.or.ink(178).eq.0)go to 60                                     !/*kb4a       44*/
      if(kp.eq.0)write(iuo,30)ik,nk                                             !/*kb4a       45*/
   30 format(33h1average keff results summed over,i4,12h cycles each,           !/*kb4a       46*/
     & 8h to form,i3,21h batch values of keff,23x,15hprint table 178/)          !/*kb4a       47*/
      if(kp.ne.0)write(iuo,40)ik,nk                                             !/*kb4a       48*/
   40 format(///33h average keff results summed over,i4,12h cycles each,        !/*kb4a       49*/
     & 8h to form,i3,21h batch values of keff/)                                 !/*kb4a       50*/
      if(nk.le.3)write(iuo,50)hy(1),hy(1)                                       !/*kb4a       51*/
      if(nk.gt.3)write(iuo,50)hy(2),hy(3)                                       !/*kb4a       52*/
   50 format(21h  batch  start    end,4x,25h keff estimators by batch,          !/*kb4a       53*/
     & 13x,39h average keff estimators and deviations,14x,a15/                  !/*kb4a       54*/
     & 21h number  cycle  cycle,4x,26h k(coll)  k(abs)  k(track),6x,            !/*kb4a       55*/
     & 52h k(coll)  st dev    k(abs)  st dev   k(track) st dev,7x,a15/)         !/*kb4a       56*/
      kp=1                                                                      !/*kb4a       57*/
      lp=1                                                                      !/*kb4a       58*/
!                                                                               !/*kb4a       59*/
!        collapse ik cycle values for 3 keffs to form nk batches in zk.         !/*kb4a       60*/
   60 do 80 k=1,3                                                               !/*kb4a       61*/
      do 80 j=1,nk                                                              !/*kb4a       62*/
      zk(k,j)=0.                                                                !/*kb4a       63*/
      do 70 l=ikz+(j-1)*ik+1,ikz+j*ik                                           !/*kb4a       64*/
   70 zk(k,j)=zk(k,j)+rkpl(lrkp+k,l)                                            !/*kb4a       65*/
   80 zk(k,j)=zk(k,j)/ik                                                        !/*kb4a       66*/
!                                                                               !/*kb4a       67*/
!        zero the sum arrays to calculate keff and keff covariances.            !/*kb4a       68*/
      do 90 i=1,3                                                               !/*kb4a       69*/
      os(i)=0.                                                                  !/*kb4a       70*/
      do 90 j=1,3                                                               !/*kb4a       71*/
   90 o2(i,j)=0.                                                                !/*kb4a       72*/
!                                                                               !/*kb4a       73*/
!        sum over nk batches of ik cycles each and find covariances.            !/*kb4a       74*/
      kl=0                                                                      !/*kb4a       75*/
      do 180 l=1,nk                                                             !/*kb4a       76*/
      do 100 i=1,3                                                              !/*kb4a       77*/
      os(i)=os(i)+zk(i,l)                                                       !/*kb4a       78*/
      do 100 j=1,3                                                              !/*kb4a       79*/
  100 o2(i,j)=o2(i,j)+zk(i,l)*zk(j,l)                                           !/*kb4a       80*/
!                                                                               !/*kb4a       81*/
!        skip all but except l=nk if there is no print to the output.           !/*kb4a       82*/
      if(lp.eq.0.and.l.ne.nk)go to 180                                          !/*kb4a       83*/
      if(l.eq.1)go to 160                                                       !/*kb4a       84*/
      kl=min(max(l,3),4)                                                        !/*kb4b        3*/
      call covar(os,o2,za,ea,tpp(1),tpp(5),tpp(9),tpp(13),tpp(17),l)            !/*kb4b        4*/
      za(4)=tpp(12)                                                             !/*kb4b        5*/
      ea(4)=tpp(16)                                                             !/*kb4b        6*/
!                                                                               !/*kb4a      140*/
!        write the next line of the batch keff results.                         !/*kb4a      141*/
  150 if(lp.eq.0)go to 180                                                      !/*kb4a      142*/
  160 write(iuo,170)l,ikz+(l-1)*ik+1,ikz+l*ik,(zk(i,l),i=1,3),                  !/*kb4a      143*/
     & (za(i),ea(i),i=1,kl)                                                     !/*kb4a      144*/
  170 format(3i7,3x,3f9.5,4x,3(f10.5,f8.5),6x,2f8.5)                            !/*kb4a      145*/
      if(mod(l,10).eq.0.and.l.ne.nk)write(iuo,'(1x,131(1h-))')                  !/*kb4a      146*/
  180 continue                                                                  !/*kb4a      147*/
!                                                                               !/*kb4a      148*/
!        save k(c/a/t) results of batch size ik for batch summary table.        !/*kb4a      149*/
      if(kl.ne.4)go to 200                                                      !/*kb4a      150*/
      lz=lz+1                                                                   !/*kb4a      151*/
      if(lz.gt.nb)go to 220                                                     !/*kb4a      152*/
      iz(1,lz)=ik                                                               !/*kb4a      153*/
      iz(2,lz)=nk                                                               !/*kb4a      154*/
      do 190 i=1,4                                                              !/*kb4a      155*/
      zb(i,1,lz)=za(i)                                                          !/*kb4a      156*/
  190 zb(i,2,lz)=ea(i)                                                          !/*kb4a      157*/
!                                                                               !/*kb4a      158*/
!        check 3 sets of nk values in zk for normality (hv is returned).        !/*kb4a      159*/
      call knorm(3,nk,hv(lz),zk,je)                                             !/*kb4a      160*/
  200 continue                                                                  !/*kb4a      161*/
!                                                                               !/*kb4a      162*/
!        print message that no batches were formed(>3 batches required).        !/*kb4a      163*/
      if(lz.ne.1)go to 220                                                      !/*kb4a      164*/
      write(iuo,210)mb                                                          !/*kb4a      165*/
  210 format(/50h no batch keff table is available because there is,            !/*kb4a      166*/
     & 17h not a factor for,i5,36h active cycles to produce a combined,         !/*kb4a      167*/
     & 16h keff estimator.)                                                     !/*kb4a      168*/
      return                                                                    !/*kb4a      169*/
!                                                                               !/*kb4a      170*/
!        print batch summary table if any batched comb keffs were done.         !/*kb4a      171*/
  220 iz(1,1)=1                                                                 !/*kb4a      172*/
      iz(2,1)=kcz-ikz                                                           !/*kb4a      173*/
      hv(1)=hz                                                                  !/*kb4a      174*/
      do 230 i=1,4                                                              !/*kb4a      175*/
      zb(i,1,1)=rkpl(lrkp+2*i+4,kcz)                                            !/*kb4a      176*/
  230 zb(i,2,1)=rkpl(lrkp+2*i+5,kcz)                                            !/*kb4a      177*/
      write(iuo,240)lz                                                          !/*kb4a      178*/
  240 format(53h1average individual and combined collision/absorption,          !/*kb4a      179*/
     & 30h/track-length keff results for,i3,22h different batch sizes//         !/*kb4a      180*/
     & 53h cycles per  number of    average keff estimators and,                !/*kb4a      181*/
     & 56h deviations      normality  average k(c/a/t)    k(c/a/t),             !/*kb4a      182*/
     & 21h confidence intervals)                                                !/*kbxc        1*/
      write(iuo,245)                                                            !/*kbxc        2*/
  245 format(30h keff batch  k batches  k(col),                                 !/*kbxc        3*/
     & 50h st dev  k(abs) st dev  k(trk) st dev   co/ab/trk ,                   !/*kb4a      184*/
     & 52h k(c/a/t) st dev    95% confidence    99% confidence/)                !/*kb4a      185*/
!                                                                               !/*kb4a      186*/
!        use correct conf int multiplier for batch 95, 99 percentiles.          !/*kb4a      187*/
      do 260 l=1,lz                                                             !/*kb4a      188*/
      call confid(iz(2,l)-3,cg)                                                 !/*kb4a      189*/
      write(iuo,250)(iz(i,l),i=1,2),(zb(j,1,l),zb(j,2,l),j=1,3),hv(l),          !/*kb4a      190*/
     & zb(4,1,l),zb(4,2,l),zb(4,1,l)-cg(2)*zb(4,2,l),zb(4,1,l)+cg(2)*           !/*kb4a      191*/
     & zb(4,2,l),zb(4,1,l)-cg(3)*zb(4,2,l),zb(4,1,l)+cg(3)*zb(4,2,l)            !/*kb4a      192*/
  250 format(i8,i11,3h  |,3(1x,2f7.4),3h  |,a8,2h| ,2f8.5,f10.5,1h-,            !/*kb4a      193*/
     & f7.5,f10.5,1h-,f7.5)                                                     !/*kb4a      194*/
  260 if(mod(l,10).eq.0)write(iuo,'(1x,131(1h-))')                              !/*kb4a      195*/
      return                                                                    !/*kb4a      196*/
      end                                                                       !/*kb4a      197*/

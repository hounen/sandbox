!#ident eventp.F from deck et                                                   !/*et          1*/
      subroutine eventp(m)                                                      !/*et          2*/
!        print a line for the current event, whose type is m.                   !/*et          3*/
      use global_data                                                           !/*etx         1*/
!                                                                               !/*etx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*etx         3*/
      implicit integer(kindi) (i-n)                                             !/*etx         4*/
!                                                                               !/*etx         5*/
      character ha*10,he(8)*9,hf(3)*15,hg*48,hl*20,hp(0:5)*3,hq*8,              !/*et4c        1*/
     & hs(34)*9,hz*10                                                           !/*etx         6*/
      data hp/' ','inc','coh','fl','dfl','pp'/                                  !/*et          7*/
      data hs/'dxt track','erg split','wws split','wwc split',                  !/*et4a        3*/
     & 'unc track','imp split','(n,xn,f) ','(n,xg)   ','fluoresce',             !/*et4a        4*/
     & 'annihilat','photo e- ','compt e- ','pair e-  ','auger e- ',             !/*et4a        5*/
     & 'pair e+  ','bremsstra','knock e- ','k x-ray  ','(n,xg) mg',             !/*et4a        6*/
     & '(n,xf) mg','(n,xn) mg','(g,xg) mg','adj split','wwt split',             !/*et4c        4*/
     & 'photonucl',4*' '      ,'(n,cpart)','(h,cpart)','n<=cut_n ',             !/*etck        1*/
     & 'updat1   ','update   '/                                                 !/*etx         8*/
!                                                                               !/*et         15*/
!        print no more than dbcn(5) events per history.  default = 600.         !/*et         16*/
      if(krflg.eq.2.and.dbcn(11).eq.0..and.m.eq.4)return                        !/*et         17*/
      jev=jev+1                                                                 !/*et         18*/
      i=dbcn(5)                                                                 !/*et         19*/
      if(i.eq.0)i=600                                                           !/*et         20*/
      if(jev.eq.i+1)write(iuo,10)i                                              !/*et         21*/
   10 format(54h event printing is terminated for this history because,         !/*et         22*/
     & i5,33h lines have been printed already.)                                 !/*et         23*/
      if(jev.gt.i)return                                                        !/*et         24*/
      write(hg,'(3f16.0)')rijk,rnr+rnrtc,rnk                                    !/*et4a        8*/
      read(hg,'(a15,1x,a15,1x,a15)')hf                                          !/*et4a        9*/
!                                                                               !/*et         28*/
!        prepare floating-point quantities without the useless e.               !/*et         29*/
      do 20 i=1,8                                                               !/*et         30*/
      write(ha,'(1pe10.3)')gpblcm(i)                                            !/*et         31*/
      if(lev.gt.0.and.i.lt.7)write(ha,'(1pe10.3)')udt(i,0)                      !/*et4a       10*/
   20 he(i)=ha(1:6)//ha(8:10)                                                   !/*et         32*/
      ic=icl                                                                    !/*et4a       11*/
      if(lev.gt.0)ic=udt(7,0)                                                   !/*et4a       12*/
!                                                                               !/*et         33*/
!        use a format appropriate for each kind of event.                       !/*et         34*/
      go to(30,70,100,120,170)m                                                 !/*et         35*/
!                                                                               !/*et         36*/
! >>>>>  m=1 -- particle from the source.  print page heading.                  !/*et         37*/
   30 if(krflg.eq.1)write(iuo,40)nps,hf(1)                                      !/*et4a       13*/
   40 format(40h1     event log for particle history no.,i10,                   !/*et4a       14*/
     & 10x,6hijk = ,a15//5x,4hcell,5x,1hx,9x,1hy,9x,1hz,9x,1hu,9x,              !/*et4a       15*/
     & 1hv,9x,1hw,7x,3herg,6x,3hwgt,22x,3hnch,17x,3hnrn/)                       !/*et4a       16*/
      if(krflg.eq.2)write(iuo,50)nerr+1,nps,hf(1)                               !/*et4a       17*/
   50 format(37h1     event log for lost particle no.,i4,10x,5hnps =,           !/*et4a       18*/
     & i10,10x,6hijk = ,a15//5x,4hcell,5x,1hx,9x,1hy,9x,1hz,9x,1hu,9x,          !/*et4a       19*/
     & 1hv,9x,1hw,7x,3herg,6x,3hwgt,22x,3hnch,17x,3hnrn/)                       !/*et4a       20*/
      write(iuo,60)ncl(lncl+ic),he,hf(2)                                        !/*et4a       21*/
   60 format(4h src,i5,6(1x,a9),2a9,28x,a15)                                    !/*etx         9*/
      go to 190                                                                 !/*et4a       23*/
!                                                                               !/*et         49*/
! >>>>>  m=2 -- particle from the bank.                                         !/*et         50*/
!        tpp(1) (set in bankit) is reason particle was banked.                  !/*et4c2       6*/
   70 ha=hip(ipt:ipt)                                                           !/*et         51*/
      write(iuo,90)ncl(lncl+ic),he,ha,hs(nint(tpp(1))),hf(3),hf(2)              !/*et4c2       7*/
   90 format(/4h bnk,i5,6(1x,a9),2a9,2x,a1,1x,a9,2a15)                          !/*etx        10*/
      go to 190                                                                 !/*et4a       27*/
!                                                                               !/*et         57*/
! >>>>>  m=3 -- particle at a surface.                                          !/*et         58*/
  100 jq=nsf(lnsf+jsu)                                                          !/*et4c        6*/
      write(hq,'(i6,1h.,i1)')jq,kfq                                             !/*et4c        7*/
      if(kfq.eq.0)hq(7:8)='  '                                                  !/*et4c        8*/
      write(iuo,110)ncl(lncl+ic),he,hq,npa,hf(2)                                !/*et4c        9*/
  110 format(3x,1hs,i5,6(1x,a9),2a9,3x,5hsurf=,a8,1x,4hnpa=,i2,5x,a15)          !/*etx        11*/
      go to 190                                                                 !/*et4a       30*/
!                                                                               !/*et         62*/
! >>>>>  m=4 -- particle at a collision.                                        !/*et         63*/
  120 if(iex.gt.0)call zaid(2,hz,ixl(lixl+1,iex))                               !/*et4c       11*/
      if(mcal.ne.0.or.ipt.eq.1.and.ntyn.ne.19)write(iuo,130)                    !/*et4b        4*/
     & ncl(lncl+ic),he,hz,mtp,nch(ipt),hf(2)                                    !/*et4c2       8*/
  130 format(3x,1hc,i5,6(1x,a9),2a9,1x,a10,1x,2hr=,i4,i6,4x,a15)                !/*etx        15*/
      if(mcal.ne.0)go to 190                                                    !/*et4a       35*/
      if(ipt.eq.1.and.ntyn.eq.19)write(iuo,140)ncl(lncl+ic),he,                 !/*et4b        6*/
     & hz,mtp,int(cmult),nch(1),hf(2)                                           !/*et4c2       9*/
  140 format(3x,1hc,i5,6(1x,a9),2a9,1x,a10,1x,2hr=,i3,1h:,i2,i4,4x,a15)         !/*etx        16*/
      if(ipt.eq.2)write(iuo,150)ncl(lncl+ic),he,                                !/*et4a       39*/
     & hz(1:3),hp(ntyn),nch(2),hf(2)                                            !/*et4a       40*/
  150 format(3x,1hc,i5,6(1x,a9),2a9,4x,2hz=,a3,2x,a3,i9,5x,a15)                 !/*etx        17*/
      if(ipt.eq.3)write(iuo,160)ncl(lncl+icl),he,nch(3),hf(2)                   !/*et4a       42*/
  160 format(3x,1hc,i5,6(1x,a9),2a9,13x,i9,6x,a15)                              !/*etx        18*/
      go to 190                                                                 !/*et4a       44*/
!                                                                               !/*et         77*/
! >>>>>  m=5 -- terminated particle.                                            !/*et         78*/
  170 continue                                                                  !/*etx        19*/
      if (nter.le.itm_pax_all_pls_end) then                                     !/*etx        20*/
         write(iuo,180)                                                         !/*etx        21*/
     &         ncl(lncl+ic),he,label_pax_loss_all_pls(nter),hf(2)               !/*etx        22*/
      else                                                                      !/*etx        23*/
         if (ipt.eq.1) then                                                     !/*etx        24*/
            ik = id_category_neutron                                            !/*etx        25*/
         else if (ipt.eq.2) then                                                !/*etx        26*/
            ik = id_category_photon                                             !/*etx        27*/
         else if (ipt.eq.3) then                                                !/*etx        28*/
            ik = id_category_electron                                           !/*etx        29*/
         else if (charge(ipt).eq.zero) then                                     !/*etx        30*/
            ik = id_category_gen_neu                                            !/*etx        31*/
         else                                                                   !/*etx        32*/
            ik = id_category_gen_chg                                            !/*etx        33*/
         endif                                                                  !/*etx        34*/
         write(iuo,180)                                                         !/*etx        35*/
     &         ncl(lncl+ic),he,label_pax_loss_specific(nter,ik),hf(2)           !/*etx        36*/
      endif                                                                     !/*etx        37*/
  180 format(3x,1ht,i5,6(1x,a9),2a9,3x,a17,8x,a15)                              !/*etx        38*/
!                                                                               !/*et4a       47*/
!        print the data for each level.                                         !/*et4a       48*/
  190 if(nlat.eq.0.or.lat(llat+1,ic).eq.0)go to 210                             !/*et4a       49*/
      if(lev.eq.0)write(iuo,200)iii,jjj,kkk                                     !/*et4a       50*/
      if(lev.gt.0)write(iuo,200)(nint(udt(k,0)),k=8,10)                         !/*et4a       51*/
  200 format(70x,5hlat=[,2i5,i4,1h])                                            !/*et4b        8*/
  210 do 240 l=1,lev                                                            !/*et4a       53*/
      do 220 j=1,6                                                              !/*et4a       54*/
      if(l.ne.lev)write(ha,'(1pe10.3)')udt(j,l)                                 !/*et4a       55*/
      if(l.eq.lev)write(ha,'(1pe10.3)')gpblcm(j)                                !/*et4a       56*/
  220 he(j)=ha(1:6)//ha(8:10)                                                   !/*et4a       57*/
      ic=icl                                                                    !/*et4a       58*/
      if(l.ne.lev)ic=nint(udt(7,l))                                             !/*et4a       59*/
      hl=' '                                                                    !/*et4a       60*/
      if(nlat.eq.0.or.lat(llat+1,ic).eq.0)go to 240                             !/*et4a       61*/
      if(l.eq.lev)write(hl,230)iii,jjj,kkk                                      !/*et4a       62*/
      if(l.lt.lev)write(hl,230)(nint(udt(k,l)),k=8,10)                          !/*et4a       63*/
  230 format(5hlat=[,2i5,i4,1h])                                                !/*et4b        9*/
  240 write(iuo,250)ncl(lncl+ic),(he(k),k=1,6),hl                               !/*et4a       65*/
  250 format(4x,i5,6(1x,a9),2x,a20)                                             !/*etx        39*/
      return                                                                    !/*et         81*/
      end                                                                       !/*et         82*/

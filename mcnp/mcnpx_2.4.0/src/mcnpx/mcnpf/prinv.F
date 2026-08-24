!#ident prinv.F from deck pv                                                    !/*pv          1*/
      subroutine prinv                                                          !/*pv          2*/
!        print and check the results of the volume and area calculations        !/*pv          3*/
!        and move them to permanent arrays.                                     !/*pv          4*/
      use global_data                                                           !/*pvx         1*/
      use imcn_data                                                             !/*pvx         2*/
!                                                                               !/*pvx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*pvx         4*/
      implicit integer(kindi) (i-n)                                             !/*pvx         5*/
!                                                                               !/*pvx         6*/
      dimension nt(20)                                                          !/*pv4a        1*/
      character ht*9,hs*3,lb*11,ih(8)*16,jh(3)*7,kh(3)*7,lh(3)*8                !/*pv4c        1*/
      data ih/' ','infinite','asymmetric','not a boundary',                     !/*pv          8*/
     & 'qnc7 failure','quart failure','lgc overflow',' '/                       !/*pv4c        2*/
      data jh/'area','volume','mass'/,kh/'areas','volumes','masses'/            !/*pv         10*/
      data lh/'surface',' cell',' cell'/                                        !/*pv         11*/
!                                                                               !/*pv         12*/
!        set up and print the volumes and masses of the cells.                  !/*pv         13*/
      if(novol.ne.0)go to 150                                                   !/*pv         14*/
      if(ink(50).ne.0)write(iuo,10)                                             !/*pv         15*/
   10 format(1h1,23hcell volumes and masses,80x,14hprint table 50//             !/*pv         16*/
     & 8x,4hcell,5x,4hatom,10x,4hgram,9x,5hinput,7x,10hcalculated,28x,          !/*pv         17*/
     & 13hreason volume/16x,7hdensity,7x,7hdensity,7x,6hvolume,8x,              !/*pv         18*/
     & 6hvolume,9x,4hmass,7x,24hpieces    not calculated/)                      !/*pv         19*/
      m=0                                                                       !/*pv         20*/
      do 30 i=1,mxa                                                             !/*pv         21*/
      k=min(8,1-int(min(zero,vols(lvls+1,i))))                                  !/*pv4c        3*/
      g=vol(lvol+i)*den(lden+i)                                                 !/*pv         23*/
      if(g.eq.0..and.vols(lvls+1,i).gt.0.)g=vols(lvls+1,i)*den(lden+i)          !/*pv         24*/
      if(ink(50).ne.0)write(iuo,20)i,ncl(lncl+i),rho(lrho+i),                   !/*pv         25*/
     & den(lden+i),vol(lvol+i),max(zero,vols(lvls+1,i)),g,                      !/*pv         26*/
     & int(vols(lvls+2,i)),ih(k)                                                !/*pv         27*/
   20 format(2i6,1pe13.5,4e14.5,i7,6x,a16)                                      !/*pv         28*/
      if(vol(lvol+i).eq.0.)vol(lvol+i)=max(zero,vols(lvls+1,i))                 !/*pv         29*/
   30 if(vols(lvls+2,i).gt.1.)m=m+1                                             !/*pv         30*/
      if(m.ne.0)call erprnt(1,2,1,m,0,0,0,0,                                    !/*pv         31*/
     & 'i4,48h cells appear to consist of more than one piece.')                !/*pv         32*/
!                                                                               !/*pv         33*/
!        set up and print the areas of the surfaces.                            !/*pv         34*/
      if(ink(50).ne.0)write(iuo,40)                                             !/*pv         35*/
   40 format(1h1,13hsurface areas,90x,14hprint table 50//                       !/*pv         36*/
     & 9x,7hsurface,4x,5hinput,6x,10hcalculated,4x,11hreason area,/             !/*pv4c        4*/
     & 20x,4harea,10x,4harea,7x,14hnot calculated/)                             !/*pv4c        5*/
      do 60 j=1,mxj                                                             !/*pv         39*/
      if(ksm(lksm+j).lt.0)go to 60                                              !/*pv4c        6*/
      if(idnt(lidt+j).ne.0.and.idnt(lidt+j).ne.j)go to 60                       !/*pv4c        7*/
      k=min(8,1-int(min(zero,max(aras(lars+1,j),aras(lars+2,j)))))              !/*pv4c        8*/
      nj=nsf(lnsf+j)                                                            !/*pv         41*/
      if(ksm(lksm+j).eq.0)go to 46                                              !/*pv4c        9*/
      do 44 kk=1,nlja                                                           !/*pv4c       10*/
   44 if(j.eq.abs(lja(llja+kk)))go to 48                                        !/*pv4c       11*/
      k=4                                                                       !/*pv4c       12*/
      go to 48                                                                  !/*pv4c       13*/
   46 if(namchg(3,nj).eq.0)k=4                                                  !/*pv4c       14*/
   48 a=max(zero,aras(lars+1,j),aras(lars+2,j))                                 !/*pv4c       15*/
      if(min(aras(lars+1,j),aras(lars+2,j)).gt.0.)                              !/*pv         44*/
     & a=.5*(aras(lars+1,j)+aras(lars+2,j))                                     !/*pv         45*/
      write(ht,'(i7,1h.,i1)')nj,kfq                                             !/*pv4c       16*/
      if(kfq.eq.0)write(ht,'(i9)')nj                                            !/*pv4c       17*/
      if(ink(50).ne.0)write(iuo,50)j,ht,ara(lara+j),a,ih(k)                     !/*pv4c       18*/
   50 format(i6,a9,1pe13.5,e14.5,3x,a16)                                        !/*pv4c       19*/
      if(ara(lara+j).eq.0.)ara(lara+j)=a                                        !/*pv4c       20*/
   60 continue                                                                  !/*pv4c       21*/
!                                                                               !/*pv         49*/
!        do the areas, volumes, or masses of the tally segments.                !/*pv         50*/
      m1=0                                                                      !/*pv         51*/
      nd=0                                                                      !/*pv         52*/
      do 140 ital=1,ntal                                                        !/*pv         53*/
      lr=0                                                                      !/*pv4a        2*/
#ifdef RADIOG                                                                   /*!pvx         7*/
      if(jptal(ljpt+8,ital).gt.2)go to 140                                      !/*pvx         8*/
#endif                                                                          /*!pvx         9*/
      if(lsat(llsa+ital).eq.0)go to 140                                         !/*pv         54*/
      iw=jptal(ljpt+2,ital)/2                                                   !/*pv         55*/
      if(m1.eq.0.and.ink(50).ne.0)write(iuo,70)lh(iw)                           !/*pv         56*/
   70 format(1h1,5x,39htally segment areas, volumes, or masses,59x,             !/*pv         57*/
     & 14hprint table 50//35h tally    tally   segment   segment,5x,            !/*pv4c       22*/
     & 5hinput,7x,10hcalculated,7x,4hused,8x,21hreason not calculated/          !/*pv         59*/
     & 9x,a9,10x,7hsurface/)                                                    !/*pv4c       23*/
      m1=1                                                                      !/*pv         61*/
      ns=iptal(lipt+4,4,ital)-1                                                 !/*pv         62*/
      write(hs,'(i3)')jptal(ljpt+1,ital)                                        !/*pv         63*/
      do 130 ic=1,iptal(lipt+1,3,ital)                                          !/*pv         64*/
      t1=0.                                                                     !/*pv         65*/
      t2=0.                                                                     !/*pv         66*/
      l1=itds(iptal(lipt+1,1,ital)+ic)                                          !/*pv4b        5*/
      call binval(ic,1,lb,ib)                                                   !/*pv         67*/
      l=lsat(llsa+ital)+(ic-1)*iptal(lipt+4,3,ital)                             !/*pv         68*/
      i=iptal(lipt+4,2,ital)+(ic-1)*iptal(lipt+4,3,ital)                        !/*pv         69*/
      do 90 ib=1,ns                                                             !/*pv         70*/
      js=lja(llja+abs(lca(llca+iptal(lipt+4,1,ital)))+ib-1)                     !/*pv4c       24*/
      if(ib.eq.ns)go to 71                                                      !/*pv4c       25*/
      nj=sign(nsf(lnsf+abs(js)),js)                                             !/*pv4c       26*/
      write(ht,'(i7,1h.,i1)')nj,kfq                                             !/*pv4c       27*/
      if(kfq.eq.0)write(ht,'(i9)')nj                                            !/*pv4c       28*/
   71 if(ib.eq.ns)ht='     rest'                                                !/*pv4c       29*/
      a=atsa(lats+1,l+ib)                                                       !/*pv         73*/
      if(iw.eq.1)a=max(a,atsa(lats+2,l+ib))                                     !/*pv         74*/
      if(iw.eq.1.and.atsa(lats+1,l+ib).gt.0..and.atsa(lats+2,l+ib).gt.          !/*pv         75*/
     & 0.)a=.5*(atsa(lats+1,l+ib)+atsa(lats+2,l+ib))                            !/*pv         76*/
      k=min(8,1-int(min(zero,a)))                                               !/*pv4c       30*/
      s=tds(i+ib)                                                               !/*pv         78*/
      if(s.eq.0..and.a.gt.0..and.itds(l1+itds(l1)+1).gt.0.and.                  !/*pv4b        7*/
     & ic.ne.iptal(lipt+1,4,ital))lr=1                                          !/*pv4b        8*/
      if(s.eq.0.)s=max(a,zero)                                                  !/*pv         79*/
      if(ink(50).ne.0)write(iuo,80)hs,lb,ib,ht,tds(i+ib),max(a,zero),s,         !/*pv         80*/
     & ih(k)                                                                    !/*pv         81*/
   80 format(2x,a3,2x,a11,i5,2x,a9,1pe15.5,2e14.5,4x,a16)                       !/*pv4c       31*/
      tds(i+ib)=s                                                               !/*pv         83*/
      hs=' '                                                                    !/*pv         84*/
      ht=' '                                                                    !/*pv         85*/
      lb=' '                                                                    !/*pv         86*/
   90 t1=t1+s                                                                   !/*pv         87*/
      if(iptal(lipt+4,3,ital).ne.ns+1)go to 100                                 !/*pv         88*/
      if(tds(i+ns+1).eq.0.)tds(i+ns+1)=t1                                       !/*pv         89*/
  100 do 110 i=1,itds(l1)                                                       !/*pv4b        9*/
      if(jptal(ljpt+2,ital).eq.2)s=ara(lara+itds(l1+i))                         !/*pv         92*/
      if(jptal(ljpt+2,ital).ge.4)s=vol(lvol+itds(l1+i))                         !/*pv         93*/
      if(jptal(ljpt+2,ital).eq.6.or.jptal(ljpt+2,ital).eq.7)                    !/*pv4c       32*/
     & s=s*den(lden+itds(l1+i))                                                 !/*pv4c       33*/
  110 t2=t2+s                                                                   !/*pv         95*/
      if(ink(50).ne.0)write(iuo,120)kh(iw),t1,jh(iw),lh(iw),t2                  !/*pv         96*/
  120 format(6x,17htotal of segment ,a7,1pe45.5,/6x,a7,9hof whole ,a8,          !/*pv         97*/
     & e45.5/)                                                                  !/*pv         98*/
  130 if(abs(t1-t2).gt..00001*max(t1,t2).and.t1.ne.0..and.t2.ne.0.)             !/*pv         99*/
     & nd=nd+1                                                                  !/*pv        100*/
  140 if(lr.ne.0)call erprnt(1,2,1,jptal(ljpt+1,ital),0,0,0,0,                  !/*pv4a        5*/
     & '37hvol/area for repeated structure tally,i5,14h may be wrong.')         !/*pv4a        6*/
      if(nd.ne.0)call erprnt(1,2,1,nd,0,0,0,0,                                  !/*pv        102*/
     & '42hsum of segment sizes differs from total in,i4,7h cases.')            !/*pv        103*/
!                                                                               !/*pv        104*/
!        do case of only one segment bin.  set divisors still 0. to 1.          !/*pv        105*/
  150 n=0                                                                       !/*pv        106*/
      do 200 ital=1,ntal                                                        !/*pv        107*/
      lr=0                                                                      !/*pv4a        7*/
      n1=n                                                                      !/*pv4a        8*/
#ifdef RADIOG                                                                   /*!pvx        10*/
      if(jptal(ljpt+8,ital).gt.2)go to 200                                      !/*pvx        11*/
#endif                                                                          /*!pvx        12*/
      if(iptal(lipt+4,2,ital).eq.0)go to 200                                    !/*pv        108*/
      ns=iptal(lipt+4,3,ital)*iptal(lipt+1,3,ital)                              !/*pv        109*/
      if(jptal(ljpt+2,ital).eq.1.or.iptal(lipt+4,1,ital).ne.0)go to 180         !/*pv        110*/
      do 170 ib=1,ns                                                            !/*pv        111*/
      j=iptal(lipt+4,2,ital)+ib                                                 !/*pv        112*/
      if(tds(j).ne.0.)go to 170                                                 !/*pv        113*/
      l1=itds(iptal(lipt+1,1,ital)+ib)                                          !/*pv4b       10*/
      do 160 i=1,itds(l1)                                                       !/*pv4b       11*/
      i1=itds(itds(iptal(lipt+1,1,ital)+ib)+i)                                  !/*pv        115*/
      if(jptal(ljpt+2,ital).eq.2)a=ara(lara+i1)                                 !/*pv        116*/
      if(jptal(ljpt+2,ital).eq.4)a=vol(lvol+i1)                                 !/*pv        117*/
      if(jptal(ljpt+2,ital).ge.6)a=vol(lvol+i1)*den(lden+i1)                    !/*pv        118*/
      tds(j)=tds(j)+a                                                           !/*pv        119*/
      if(a.eq.0.)tds(j)=0.                                                      !/*pv        120*/
  160 if(a.eq.0.)go to 170                                                      !/*pv        121*/
      if(itds(l1+itds(l1)+1).gt.0.and.ib.lt.iptal(lipt+1,4,ital))lr=1           !/*pv4b       12*/
  170 continue                                                                  !/*pv        122*/
  180 do 190 ib=1,ns                                                            !/*pv        123*/
      if(tds(iptal(lipt+4,2,ital)+ib).eq.0.)n=n+1                               !/*pv        124*/
  190 if(tds(iptal(lipt+4,2,ital)+ib).eq.0.)                                    !/*pv        125*/
     & tds(iptal(lipt+4,2,ital)+ib)=1.                                          !/*pv        126*/
      if(n.ne.n1.and.ink(50).ne.0)write(iuo,195)jptal(ljpt+1,ital),n-n1         !/*pv4a       11*/
  195 format(/6h tally,i5,6h lacks,i5,30h segment volume(s) or area(s).)        !/*pv4a       12*/
  200 if(lr.ne.0)call erprnt(1,2,1,jptal(ljpt+1,ital),0,0,0,0,                  !/*pv4a       13*/
     & '37hvol/area for repeated structure tally,i5,14h may be wrong.')         !/*pv4a       14*/
      if(n.ne.0)call erprnt(1,1,1,n,0,0,0,0,                                    !/*pv        128*/
     & 'i4,54h tally volumes or areas were not input nor calculated.')          !/*pv        129*/
!                                                                               !/*pv        130*/
!        check volumes for multigroup weight-window generator.                  !/*pv        131*/
      if(icw.eq.0)return                                                        !/*pv        132*/
      n=0                                                                       !/*pv        133*/
      do 210 i=1,mxa                                                            !/*pv        134*/
  210 if((fim(lfim+1,i).ne.0..or.fim(lfim+2,i).ne.0.).and.                      !/*pv        135*/
     & vol(lvol+i).eq.0.)n=n+1                                                  !/*pv        136*/
      if(n.eq.0)return                                                          !/*pv4a       15*/
      call erprnt(1,1,1,n,0,0,0,0,                                              !/*pv4a       16*/
     & 'i4,48h cell volumes were neither input nor calculated.')                !/*pv4a       17*/
      if(ink(50).eq.0)return                                                    !/*pv4a       18*/
      write(iuo,220)                                                            !/*pv4a       19*/
  220 format(/42h the following cell volume(s) are lacking:/)                   !/*pv4a       20*/
      n=0                                                                       !/*pv4a       21*/
      do 250 i=1,mxa                                                            !/*pv4a       22*/
      if((fim(lfim+1,i).ne.0..or.fim(lfim+2,i).ne.0.).and.                      !/*pv4a       23*/
     & vol(lvol+i).eq.0.)go to 230                                              !/*pv4a       24*/
      go to 250                                                                 !/*pv4a       25*/
  230 n=n+1                                                                     !/*pv4a       26*/
      nt(n)=ncl(lncl+i)                                                         !/*pv4a       27*/
      if(n.lt.20)go to 250                                                      !/*pv4a       28*/
      write(iuo,240)(nt(k),k=1,20)                                              !/*pv4a       29*/
  240 format(1x,20i6)                                                           !/*pv4a       30*/
      n=0                                                                       !/*pv4a       31*/
  250 continue                                                                  !/*pv4a       32*/
      if(n.gt.0)write(iuo,240)(nt(k),k=1,n)                                     !/*pv4a       33*/
      return                                                                    !/*pv        139*/
      end                                                                       !/*pv        140*/

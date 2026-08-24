!#ident crtcze.F from deck ez                                                   !/*ez          1*/
      subroutine crtcze(mm)                                                     !/*ez          2*/
!        print a table of cells whose importances or weight windows             !/*ez          3*/
!        are more than a factor of 4 different from their neighbors.            !/*ez          4*/
!        mm=0 to do importances or input weight windows.                        !/*ez          5*/
!        mm=1 to do generated weight windows.                                   !/*ez          6*/
      use global_data                                                           !/*ezx         1*/
!                                                                               !/*ezx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ezx         3*/
      implicit integer(kindi) (i-n)                                             !/*ezx         4*/
!                                                                               !/*ezx         5*/
      character ha*8,hb*8                                                       !/*ez          8*/
!                                                                               !/*ez          9*/
!        loop over particle types.                                              !/*ezx         6*/
      do 170 ip=1,mipt                                                          !/*ez         11*/
      if(kpt(ip).eq.0)go to 170                                                 !/*ez         12*/
      im=0                                                                      !/*ez4c        1*/
      if(mm.eq.0.and.wwp(ip,5).lt.0.)im=1                                       !/*ez4c        2*/
      kf=0                                                                      !/*ez         13*/
      ks=0                                                                      !/*ez         14*/
!                                                                               !/*ez         15*/
!        loop over weight-window energy groups, if any.                         !/*ez         16*/
!        otherwise just do the cell importances.                                !/*ez         17*/
      ng=max(1,nww(ip))                                                         !/*ez         18*/
      if(im.eq.1)ng=1                                                           !/*ez4c        3*/
      if(mm.ne.0)ng=ngww(ip)                                                    !/*ez         19*/
      do 140 ie=1,ng                                                            !/*ez         20*/
!                                                                               !/*ez         21*/
!        look for big importance ratios between neighboring cells.              !/*ez         22*/
      kf=min(kf,2)                                                              !/*ez         23*/
      do 130 ic=1,mxa                                                           !/*ez         24*/
      if(junf.eq.0)go to 5                                                      !/*cor4-1    198*/
      if(lat(llat+1,ic).ne.0)go to 130                                          !/*cor4-1    199*/
    5 wc=fim(lfim+ip,ic)                                                        !/*cor4-1    200*/
      if(nww(ip).ne.0.and.im.eq.0)wc=wwf(lwwf+ic+mxa*(ie-1+mww(ip)))            !/*ez4c        4*/
      if(mm.ne.0)wc=wwfa(lwfa+ic+mxa*(ie-1+mgww(ip)))                           !/*ez4c        5*/
      if(wc.le.0.)go to 130                                                     !/*ez         29*/
      i1=ic                                                                     !/*ez         30*/
      i2=ic                                                                     !/*ez         31*/
      w1=0.                                                                     !/*ez         32*/
      w2=huge                                                                   !/*ez         33*/
      do 30 js=abs(lca(llca+ic)),abs(lca(llca+ic+1))-1                          !/*ez         34*/
      if(abs(lja(llja+js)).gt.1000000)go to 30                                  !/*ez         35*/
      ij=0                                                                      !/*ez4a        1*/
      do 20 jj=abs(lcaj(llcj+js)),abs(lcaj(llcj+js+1))-1                        !/*ez         36*/
      if(ij.eq.1000001.and.laj(jj).ne.1000002)go to 20                          !/*ez         37*/
      ij=abs(laj(jj))                                                           !/*ez         38*/
      if(ij.gt.1000000)go to 20                                                 !/*ez         39*/
      wj=fim(lfim+ip,ij)                                                        !/*ez         40*/
      if(nww(ip).ne.0.and.im.eq.0)wj=wwf(lwwf+ij+mxa*(ie-1+mww(ip)))            !/*ez4c        6*/
      if(mm.ne.0)wj=wwfa(lwfa+ij+mxa*(ie-1+mgww(ip)))                           !/*ez4c        7*/
      if(i1.eq.ic)i1=0                                                          !/*ez         43*/
      if(wj.le.0.)go to 20                                                      !/*ez         44*/
      if(wj.lt.w1)go to 10                                                      !/*ez         45*/
      i1=ij                                                                     !/*ez         46*/
      w1=wj                                                                     !/*ez         47*/
   10 if(wj.gt.w2)go to 20                                                      !/*ez         48*/
      i2=ij                                                                     !/*ez         49*/
      w2=wj                                                                     !/*ez         50*/
   20 continue                                                                  !/*ez         51*/
   30 continue                                                                  !/*ez         52*/
      kc=idefv(14)                                                              !/*ez         53*/
      if(kc.eq.0)go to 60                                                       !/*ez         54*/
      if(ivdis(14).ne.0)go to 40                                                !/*ez         55*/
      if(ic.ne.srv(1,14))kc=0                                                   !/*ez         56*/
      go to 60                                                                  !/*ez         57*/
   40 do 50 k=1,ksd(lksd+4,ivdis(14))                                           !/*ez         58*/
   50 if(ic.eq.spf(ksd(lksd+13,ivdis(14))+1,k))go to 60                         !/*ez         59*/
      kc=0                                                                      !/*ez         60*/
   60 if(w1.gt.4.*wc.or.wc.gt.4.*w2)ks=1                                        !/*ez         61*/
      if(ink(120)+mm.eq.0.or.w1.le.4.*wc.and.wc.le.4.*w2.and.                   !/*ez         62*/
     & (i1.ne.ic.or.nps.le.1000.or.kc.ne.0))go to 130                           !/*ez         63*/
!                                                                               !/*ez         64*/
      if(kf.lt.2.and.(nww(ip)+mm.eq.0.or.im.eq.1))write(iuo,70)hnp(ip)          !/*ez4c        8*/
   70 format(47h1the following cells are bounded by cells with ,a11,            !/*ezx         7*/
     & 12h importances,34x,15hprint table 120/                                  !/*ezx         8*/
     & 48h that may be a factor of four or more different.                      !/*ez         68*/
     & //27x,7hmaximum,29x,7hminimum/                                           !/*ez         69*/
     & 2x,4hcell,5x,10himportance,5x,8hneighbor,5x,10himportance,3x,            !/*ez         70*/
     & 5hratio,5x,8hneighbor,5x,10himportance,3x,5hratio/)                      !/*ez         71*/
      if(kf.lt.2.and.nww(ip).gt.0.and.mm+im.eq.0)write(iuo,80)hnp(ip)           !/*ez4c        9*/
   80 format(47h1the following cells are bounded by cells with ,a11,            !/*ezx         9*/
     & 21h weight window bounds,25x,15hprint table 120/                         !/*ezx        10*/
     & 48h that may be a factor of four or more different.)                     !/*ez         75*/
      if(kf.lt.2.and.mm.ne.0)write(iuo,90)hnp(ip)                               !/*ez         76*/
   90 format(46h1the following cells are bounded by cells with,                 !/*ez         77*/
     & 11h generated ,a11/39h weight-window bounds that are a factor,           !/*ezx        11*/
     & 27h of four or more different.)                                          !/*ez         79*/
      ha=' energy '                                                             !/*ez4c       10*/
      if((mm.eq.0.and.wwp(ip,6).gt.0.).or.(mm.ne.0.and.wwg(8).ne.0.))           !/*ez4c       11*/
     & ha=' time   '                                                            !/*ez4c       12*/
      if(mm.eq.0.and.nww(ip).gt.0.and.im.eq.0)we=wwe(lwwe+ie+mww(ip))           !/*ez4c       13*/
      if(mm.ne.0)we=ewwg(lewg+ie+mgww(ip))                                      !/*ez4c       14*/
      if(kf.ne.3.and.(mm.ne.0.or.(nww(ip).gt.0.and.im.eq.0)))                   !/*ez4c       15*/
     & write(iuo,100)hnp(ip),ie,ha,we                                           !/*ez4c       16*/
  100 format(//1x,a11,13h window group,i3,5x,5hupper,a8,1h=,1pe11.4//           !/*ezx        12*/
     & 13x,6hwindow,8x,7hmaximum,7x,6hwindow,16x,7hminimum,                     !/*ez         85*/
     & 7x,6hwindow/2x,4hcell,7x,6hweight,7x,8hneighbor,7x,                      !/*ez         86*/
     & 6hweight,5x,5hratio,5x,8hneighbor,7x,6hweight,5x,5hratio/)               !/*ez         87*/
      kf=3                                                                      !/*ez         88*/
!                                                                               !/*ez         89*/
!        print a line in the body of the table.                                 !/*ez         90*/
      if(i1.eq.ic)write(iuo,110)ncl(lncl+ic)                                    !/*ez         91*/
  110 format(i6,5x,36hno data available yet for this cell.)                     !/*ez         92*/
      if(i1.eq.ic)go to 130                                                     !/*ez         93*/
      if(w1.ge.9999.9*wc)write(ha,'(1pe8.1)')w1/wc                              !/*ez         94*/
      if(w1.lt.9999.9*wc)write(ha,'(f8.1)')w1/wc                                !/*ez         95*/
      hb='infinite'                                                             !/*ez4c       18*/
      if(wc.ge.9999.9*w2.and.w2.ne.0.)write(hb,'(1pe8.1)')wc/w2                 !/*ez4c       19*/
      if(wc.lt.9999.9*w2)write(hb,'(f8.1)')wc/w2                                !/*ez         97*/
      write(iuo,120)ncl(lncl+ic),wc,ncl(lncl+i1),w1,ha,ncl(lncl+i2),            !/*ez         98*/
     & w2,hb                                                                    !/*ez         99*/
  120 format(i6,1pe15.5,i13,1pe15.5,a8,i13,1pe15.5,a8)                          !/*ez        100*/
  130 if(ink(120)+mm.eq.0.and.ks.ne.0)go to 150                                 !/*ez        101*/
!                                                                               !/*ez        102*/
  140 continue                                                                  !/*ez        103*/
!                                                                               !/*ez        104*/
!        print warning messages if any big importance ratios were found.        !/*ez        105*/
  150 if(ks.ne.0.and.mm.eq.0)call erprnt(1,2,0,0,0,0,0,0,                       !/*ez4a        6*/
     & '54himportance function may be poor.  see print table 120.')             !/*ez4a        7*/
      if(im.eq.1)call erprnt(1,2,0,0,0,0,0,0,                                   !/*ez4c       20*/
     & '56hsome of the geometry is covered by a weight window mesh.')           !/*ez4c       21*/
!                                                                               !/*ez        112*/
  170 continue                                                                  !/*ez        113*/
      return                                                                    !/*ez        114*/
      end                                                                       !/*ez        115*/

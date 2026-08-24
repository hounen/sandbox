!#ident iwtwnd.F from deck iw                                                   !/*iw          1*/
      subroutine iwtwnd                                                         !/*iw          2*/
!        set up the weight windows.                                             !/*iw          3*/
      use global_data                                                           !/*iwx         1*/
      use imcn_data                                                             !/*iwx         2*/
!                                                                               !/*iwx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*iwx         4*/
      implicit integer(kindi) (i-n)                                             !/*iwx         5*/
!                                                                               !/*iwx         6*/
      character hc*13                                                           !/*iw4c        1*/
!                                                                               !/*iw          6*/
      do 140 ip=1,mipt                                                          !/*iw4c        2*/
      if(mcal.ne.0.and.iplt.ne.2)then                                           !/*iwx         7*/
         wwp(ip,4)=0.                                                           !/*iwx         8*/
         go to 140                                                              !/*iwx         9*/
      endif                                                                     !/*iwx        10*/
      if(nww(ip).eq.0)go to 140                                                 !/*iw          8*/
      if(nwgeom.eq.3)ink(20)=1                                                  !/*iw4c        3*/
      if(ink(20).ne.0)write(iuo,10)hnp(ip)                                      !/*iw          9*/
   10 format(1h1,a11,14h weight window,78x,14hprint table 20)                   !/*iwx        11*/
      if(wwp(ip,1).le.0.)wwp(ip,1)=5.                                           !/*iw4c        4*/
      if(wwp(ip,3).le.0.)wwp(ip,3)=5.                                           !/*iw4c        5*/
      if(wwp(ip,2).eq.0.)wwp(ip,2)=.6*wwp(ip,1)                                 !/*iw         11*/
      wwp(ip,4)=2.*wwp(ip,4)+1.                                                 !/*iw         12*/
!                                                                               !/*iw4c        6*/
!        wwp(ip,5) < 0 -- windows input via the wwinp file.                     !/*iw4c        7*/
      if(nwgeom.gt.0.and.wwp(ip,5).lt.0.)call wwfile(ip)                        !/*iw4c        8*/
!                                                                               !/*iw         13*/
!        check input importances and weight-windows.                            !/*iw4b       13*/
      a=0.                                                                      !/*iw         15*/
      b=0.                                                                      !/*iw         16*/
      c=huge                                                                    !/*iw4c        9*/
      do 20 i=1,mxa                                                             !/*iw         18*/
      a=max(a,fim(lfim+ip,i))                                                   !/*iw4c       10*/
      if(fim(lfim+ip,i).ne.0.)c=min(c,fim(lfim+ip,i))                           !/*iw4c       11*/
      if(wwm(1).ne.0.)go to 20                                                  !/*iw4c       12*/
      do 15 j=1,nww(ip)                                                         !/*iw4b       15*/
   15 b=b+abs(wwf(lwwf+i+mxa*(j-1+mww(ip))))                                    !/*iwx        12*/
   20 continue                                                                  !/*iw4b       17*/
      if(wwm(1).eq.0.)go to 25                                                  !/*iw4c       14*/
      if(a.eq.0.)call erprnt(1,1,0,0,0,0,0,0,'45h5th entry on wwp:'             !/*iw4c       15*/
     & //hip(ip:ip)//' < 0 and imp:'//hip(ip:ip)//' card absent.')              !/*iw4c       16*/
      if(a.gt.1.01.or.c.lt..99)                                                 !/*iw4c       17*/
     & call erprnt(1,2,0,0,0,0,0,0,'58h5th entry wwp:'                          !/*iw4c       18*/
     & //hip(ip:ip)//' < 0, cell importances in mesh should be 1.')             !/*iw4c       19*/
      go to 70                                                                  !/*iw4b       26*/
!                                                                               !/*iw4b       27*/
!        switch.gt.0 -- derive windows from cell importances.                   !/*iw4b       28*/
   25 if(wwp(ip,5).le.0.)go to 40                                               !/*iw4c       20*/
      if(b.ne.0.)call erprnt(1,1,0,0,0,0,0,0,'47hswitch on wwp:'                !/*iw4a        1*/
     & //hip(ip:ip)//' .ne. 0 and wwn:'//hip(ip:ip)//' cards present.')         !/*iw4a        2*/
      if(a.eq.0.)call erprnt(1,1,0,0,0,0,0,0,'45hswitch on wwp:'                !/*iw4a        3*/
     & //hip(ip:ip)//' .ne. 0 and imp:'//hip(ip:ip)//' card absent.')           !/*iw4a        4*/
      if(nww(ip).gt.1)call erprnt(1,1,0,0,0,0,0,0,'46hswitch on wwp:'           !/*iw4a        5*/
     & //hip(ip:ip)//' .ne. 0 and wwe:'//hip(ip:ip)//' card present.')          !/*iw4a        6*/
      do 30 i=1,mxa                                                             !/*iw         30*/
      wwf(lwwf+i+mxa*mww(ip))=-1.                                               !/*iw         31*/
      if(fim(lfim+ip,i).eq.0.)go to 30                                          !/*iw         32*/
      wwf(lwwf+i+mxa*mww(ip))=wwp(ip,5)/fim(lfim+ip,i)                          !/*iw         33*/
      fim(lfim+ip,i)=1.                                                         !/*iw         34*/
   30 continue                                                                  !/*iw         35*/
      wwe(lwwe+1+mww(ip))=1000.                                                 !/*iw         36*/
      go to 70                                                                  !/*iw         37*/
!                                                                               !/*iw         38*/
!        switch=0 -- set up cell importances.                                   !/*iw         39*/
   40 if(b.eq.0.)call erprnt(1,1,0,0,0,0,0,0,                                   !/*iw         40*/
     & '24hwwn:'//hip(ip:ip)//' cards are missing.')                            !/*iw         41*/
      if(a.ne.0.)call erprnt(1,2,0,0,0,0,0,0,                                   !/*iw         42*/
     & '57hthis problem has both weight window and cell importances.')          !/*iw         43*/
      do 60 i=1,mxa                                                             !/*iw         44*/
      m=0                                                                       !/*iw         45*/
      do 50 j=2,nww(ip)                                                         !/*iw         46*/
   50 if(wwf(lwwf+i+mxa*(j-1+mww(ip)))*wwf(lwwf+i+mxa*mww(ip)).lt.0.)m=1        !/*iw         47*/
      if(m.ne.0)call erprnt(1,1,1,ncl(lncl+i),0,0,0,0,                          !/*iw         48*/
     & '35hinconsistent weight windows in cell,i6')                             !/*iw         49*/
      if(a.eq.0.)fim(lfim+ip,i)=1.                                              !/*iw         50*/
   60 if(wwf(lwwf+i+mxa*mww(ip)).lt.0.)fim(lfim+ip,i)=0.                        !/*iw         51*/
!                                                                               !/*iw         55*/
!        check the weight-window energies or times.                             !/*iw4c       21*/
   70 n=nww(ip)                                                                 !/*iw         57*/
      if(wwp(ip,6).eq.0.)go to 72                                               !/*iw4c       22*/
      if(wwe(lwwe+n+mww(1)).lt.tco(ip))call erprnt(1,2,0,0,0,0,0,0,             !/*iw4c       23*/
     & '41htime of top weight window bin set to tco.')                          !/*iw4c       24*/
      wwe(lwwe+n+mww(1))=max(wwe(lwwe+n+mww(1)),tco(ip))                        !/*iw4c       25*/
      go to 75                                                                  !/*iw4c       26*/
   72 if(ip.eq.1.and.wwe(lwwe+n+mww(1)).lt.emx(1))call erprnt(1,2,0,0,0,        !/*iw4c       27*/
     & 0,0,0,'52henergy of top neutron weight-window bin set to emax.')         !/*iw         59*/
      if(ip.eq.2.and.wwe(lwwe+n+mww(2)).lt.100.)call erprnt(1,2,0,0,0,0,        !/*iw         60*/
     & 0,0,'50henergy of top photon weight-window bin set to 100.')             !/*iw         61*/
      if(ip.eq.1)wwe(lwwe+n+mww(1))=max(wwe(lwwe+n+mww(1)),emx(1))              !/*iw         62*/
      if(ip.eq.2)wwe(lwwe+n+mww(2))=max(wwe(lwwe+n+mww(2)),zero+100.)           !/*iw         63*/
!                                                                               !/*iw4c       28*/
!        renormalize by 7th entry on wwp card and count zeros.                  !/*iw4c2       1*/
   75 nz=0                                                                      !/*iw4c2       2*/
      m=mxa                                                                     !/*iw4c2       3*/
      if(wwp(ip,5).lt.0.)m=nwwm                                                 !/*iw4c2       4*/
      do 77 j=1,n                                                               !/*iw4c2       5*/
      do 77 i=1,m                                                               !/*iw4c2       6*/
      k=lwwf+(j-1+mww(ip))*m+i                                                  !/*iw4c2       7*/
      if(wwf(k).eq.0.)nz=nz+1                                                   !/*iw4c2       8*/
   77 if(wwp(ip,7)*wwf(k).gt.0.)wwf(k)=wwp(ip,7)*wwf(k)                         !/*iw4c2       9*/
!                                                                               !/*iw4c2      10*/
!        check for consistent weight cutoffs.                                   !/*iw4c2      11*/
      if(nz.ne.0.and.wc2(ip).ne.0.)call erprnt(1,2,0,0,0,0,0,0,                 !/*iw4c2      12*/
     & '52hnonzero '//hnp(ip)//' weight cutoff with zero windows.')             !/*iwx        13*/
      if(nint(wwp(ip,4)).eq.3.and.wc1(ip).ne.0.)call erprnt(1,2,0,0,0,0,        !/*iw4c       36*/
     & 0,0,'51hsurface-only weight window requires analog capture.')            !/*iw4c       37*/
      if(nwgeom.eq.3)wwp(ip,5)=0.                                               !/*iw4c       38*/
!                                                                               !/*iw         64*/
!        print table of weight-window energies/times and lower bounds.          !/*iw4c       39*/
      if(ink(20).eq.0)go to 140                                                 !/*iw         66*/
      write(iuo,80)(i,wwp(ip,i),i=1,7)                                          !/*iw4c2      14*/
   80 format(/30h weight-window game parameters/(i10,1pe13.5))                  !/*iw4c2      15*/
      if(nint(wwp(ip,4)).eq.-1)write(iuo,                                       !/*iw4c       40*/
     & '(/42h weight window applied at collisions only.)')                      !/*iw4c       41*/
      if(nint(wwp(ip,4)).eq.1)write(iuo,                                        !/*iw4c       42*/
     & '(/50h weight window applied at collisions and surfaces.)')              !/*iw4c       43*/
      if(nint(wwp(ip,4)).eq.3)write(iuo,                                        !/*iw4c       44*/
     & '(/40h weight window applied at surfaces only.)')                        !/*iw4c       45*/
      if(wwp(ip,7).ne.0.)write(iuo,                                             !/*iw4c2      16*/
     & '(/38h all windows multiplied by a factor of,1pe11.4)')wwp(ip,7)         !/*iw4c2      17*/
      if(nz.gt.0)write(iuo,'(/i10,27h zero weight windows input.)')nz           !/*iw4c2      18*/
      if(wwm(1).ne.0.)go to 140                                                 !/*iw4c       46*/
      write(iuo,90)                                                             !/*iw         69*/
   90 format(//27h weight-window lower bounds)                                  !/*iw         70*/
      do 120 k=1,n,10                                                           !/*iw         71*/
      l=min(k+9,n)                                                              !/*iw         72*/
      hc(1:9)=' energy: '                                                       !/*iw4c       47*/
      if(wwp(ip,6).ne.0.)hc(1:9)='   time: '                                    !/*iw4c       48*/
      write(iuo,100)hc(1:9),(wwe(lwwe+j+mww(ip)),j=k,l)                         !/*iw4c       49*/
  100 format(/a9,8x,1p10e11.3)                                                  !/*iw4c       50*/
      write(iuo,'(4x,4hcell)')                                                  !/*iw4c       51*/
      do 120 i=1,nwwm                                                           !/*iw4c       52*/
      write(hc,'(i6,7x)')ncl(lncl+i)                                            !/*iw4c       53*/
  120 write(iuo,130)hc,(wwf(lwwf+i+nwwm*(j-1+mww(ip))),j=k,l)                   !/*iw4c       54*/
  130 format(1x,a13,3x,1p10e11.3)                                               !/*iw4b       42*/
!                                                                               !/*iw         80*/
  140 continue                                                                  !/*iw         81*/
      return                                                                    !/*iw         82*/
      end                                                                       !/*iw         83*/

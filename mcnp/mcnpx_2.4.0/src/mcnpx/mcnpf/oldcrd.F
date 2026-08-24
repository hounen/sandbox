!#ident oldcrd.F from deck oc                                                   !/*oc          1*/
      subroutine oldcrd                                                         !/*oc          2*/
!        complete the checking and processing of the previous card.             !/*oc          3*/
      use global_data                                                           !/*ocx         1*/
      use imcn_data                                                             !/*ocx         2*/
!                                                                               !/*ocx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ocx         4*/
      implicit integer(kindi) (i-n)                                             !/*ocx         5*/
!                                                                               !/*ocx         6*/
      dimension ns(39)                                                          !/*oc4c        1*/
!        correct number of input coefficients for each surface type.            !/*oc          7*/
      data ns/4,4*1,4,3*2,3*3,3*1,3*5,3*3,2*10,3*6,3*0,12,6,4,7,                !/*oc4c        2*/
     & 12,7,8,12,30,15/                                                         !/*oc4c2       1*/
!                                                                               !/*oc          9*/
      go to( 20, 40,190,200, 10,210,220,230,500,500,500,500,240, 10,330,        !/*oc         10*/
     &      500,270,290,310,310,290, 10, 10,330,330,330,330,330,330,330,        !/*oc         11*/
     &      330,330, 10,330,330,330,330,330,330,330,330,330,330,330, 10,        !/*oc         12*/
     &      330,350,390,400,410,420, 10, 10, 10, 10)ica                         !/*oc         13*/
      go to(500,450, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,470,490, 10,        !/*oc         14*/
     &      495,510,550,590,500, 10,500, 10, 10,605, 10,610,620,630,730,        !/*oc4c        4*/
     &      780,490,900, 10, 10, 10, 10, 10, 10, 10,910,330,330,330,920,        !/*ocx         7*/
     &       10,10,9910,9920,9930)ica-55                                        !/*ocxc        1*/
   10 return                                                                    !/*oc         17*/
!                                                                               !/*oc         18*/
! >>>>>  cell descriptions                                                      !/*oc         19*/
!        m1c=indicator of incorrect use of parentheses.                         !/*oc         20*/
   20 if(likef.ne.0)go to 30                                                    !/*oc         21*/
      if(m1c.ne.0)                                                              !/*oc         22*/
     & call erprnt(2,1,0,0,0,0,0,0,'29hparentheses used incorrectly.')          !/*oc         23*/
      lca(llca+mxa+1)=nlja+1                                                    !/*oc         24*/
      if(lca(llca+mxa).eq.nlja+1)                                               !/*oc         25*/
     & call erprnt(2,1,0,0,0,0,0,0,'26hthis cell has no surfaces.')             !/*oc         26*/
      return                                                                    !/*oc         27*/
   30 call likebt(2)                                                            !/*oc         28*/
      if(nwc.lt.2)call erprnt(2,1,0,0,0,0,0,0,                                  !/*oc         29*/
     & '25hnothing after the "like".')                                          !/*oc         30*/
      return                                                                    !/*oc         31*/
!                                                                               !/*oc         32*/
! >>>>>  surface descriptions                                                   !/*oc         33*/
!        m1c=surface type index.                                                !/*oc         34*/
!        m2c=1 if surface type symbol is the second item.                       !/*oc         35*/
   40 ix=lsc(llsc+mxj)                                                          !/*oc         36*/
      if(m1c.eq.1.and.nwc.gt.5+m2c)go to 170                                    !/*oc4c        6*/
      if((m1c.ge.27.and.m1c.le.29).or.m1c.gt.39)go to 170                       !/*oc4c        7*/
      nx=ix+ns(m1c)                                                             !/*oc         38*/
      n=nwc-1-m2c-ns(m1c)                                                       !/*oc         39*/
      if(m1c.ge.30.and.m1c.le.39.and.n.eq.0)go to 45                            !/*oc4c        8*/
      if(m1c.eq.30.and.nwc-1-m2c.eq.9)go to 45                                  !/*oc4c        9*/
      if(m1c.eq.39.and.nwc-1-m2c.gt.6.and.nwc-1-m2c.lt.16)go to 45              !/*oc4c       10*/
      if(m1c.eq.34.and.nwc-1-m2c.eq.10)go to 45                                 !/*oc4c2       3*/
      if(m1c.eq.32.and.nwc-1-m2c.eq.1)go to 45                                  !/*oc4c       11*/
      if(n.eq.0.or.m1c.ge.16.and.m1c.le.21.and.n.eq.-1)go to 50                 !/*oc         40*/
      call erprnt(2,1,0,0,0,0,0,0,'31hnumber of entries is incorrect.')         !/*oc         41*/
      go to 180                                                                 !/*oc         42*/
!                                                                               !/*oc4c       12*/
!        set up surfaces for macrobody.                                         !/*oc4c       13*/
   45 call mbody(1,ix)                                                          !/*oc4c       14*/
      return                                                                    !/*oc4c       15*/
!                                                                               !/*oc4c       16*/
   50 i=0                                                                       !/*oc         43*/
      if(m1c.ne.1.and.m1c.ne.6.and.abs(m1c-17).gt.1)go to 60                    !/*oc         44*/
      if(scf(ix+1).ne.0.)i=1                                                    !/*oc         45*/
      if(scf(ix+2).ne.0.)i=2+10*i                                               !/*oc         46*/
      if(scf(ix+3).ne.0.)i=3+10*i                                               !/*oc         47*/
   60 go to(70,180,180,180,80,80,80,80,80,80,80,80,80,80,80,100,100,100,        !/*oc         48*/
     & 100,100,100,180,110,130,130,130)m1c                                      !/*oc         49*/
!                                                                               !/*oc         50*/
!        p plane -- check for improper coefficients.                            !/*oc         51*/
   70 if(i.eq.0)call erprnt(2,1,0,0,0,0,0,0,                                    !/*oc         52*/
     & '25ha, b, and c are all zero.')                                          !/*oc         53*/
      if(abs(i-2).gt.1)go to 180                                                !/*oc         54*/
      if(scf(ix+i).lt.0.)go to 180                                              !/*oc         55*/
      scf(ix+1)=scf(nx)/scf(ix+i)                                               !/*oc         56*/
      if(i.ne.1)scf(ix+i)=0.                                                    !/*oc         57*/
      m1c=i+1                                                                   !/*oc         58*/
      go to 120                                                                 !/*oc         59*/
!                                                                               !/*oc         60*/
!        spheres and cylinders -- square the radius.                            !/*oc         61*/
   80 scf(nx)=scf(nx)**2                                                        !/*oc         62*/
      if(m1c.eq.5.or.m1c.gt.12.or.i.gt.3)go to 180                              !/*oc         63*/
      if(m1c.ne.6)go to 90                                                      !/*oc         64*/
      if(i.eq.0)scf(ix+1)=scf(nx)                                               !/*oc         65*/
      if(i.ne.0)scf(ix+1)=scf(ix+i)                                             !/*oc         66*/
      if(i.ne.0)scf(ix+2)=scf(nx)                                               !/*oc         67*/
      scf(ix+3)=0.                                                              !/*oc         68*/
      if(i.eq.0)m1c=5                                                           !/*oc         69*/
      if(i.ne.0)m1c=6+i                                                         !/*oc         70*/
      go to 120                                                                 !/*oc         71*/
   90 if(scf(ix+1).ne.0..or.m1c.gt.9.and.scf(ix+2).ne.0.)go to 180              !/*oc         72*/
      scf(ix+1)=scf(nx)                                                         !/*oc         73*/
      if(m1c.ge.10)m1c=m1c+3                                                    !/*oc         74*/
      if(m1c.le.9)m1c=5                                                         !/*oc         75*/
      go to 120                                                                 !/*oc         76*/
!                                                                               !/*oc         77*/
!        cones -- set up signed slope for cone of one sheet.                    !/*oc         78*/
  100 if(scf(nx).ne.0.)scf(nx)=sign(sqrt(abs(scf(nx-1))),scf(nx))               !/*oc         79*/
      if(scf(nx-1).lt.0.)                                                       !/*oc         80*/
     & call erprnt(2,1,0,0,0,0,0,0,'17ht**2 is negative.')                      !/*oc         81*/
      if(m1c.gt.18.or.i.ne.0.and.i.ne.m1c-15)go to 180                          !/*oc         82*/
      scf(ix+1)=scf(ix+i)                                                       !/*oc         83*/
      scf(ix+2)=scf(nx-1)                                                       !/*oc         84*/
      scf(ix+3)=scf(nx)                                                         !/*oc         85*/
      scf(nx-1)=0.                                                              !/*oc         86*/
      m1c=m1c+3                                                                 !/*oc         87*/
      go to 120                                                                 !/*oc         88*/
!                                                                               !/*oc         89*/
!        gq -- convert symmetric gq to sq.                                      !/*oc         90*/
  110 if(scf(ix+4).ne.0..or.scf(ix+5).ne.0..or.scf(ix+6).ne.0.)go to 180        !/*oc         91*/
      scf(ix+4)=.5*scf(ix+7)                                                    !/*oc         92*/
      scf(ix+5)=.5*scf(ix+8)                                                    !/*oc         93*/
      scf(ix+6)=.5*scf(ix+9)                                                    !/*oc         94*/
      scf(ix+7)=scf(ix+10)                                                      !/*oc         95*/
      scf(ix+8)=0.                                                              !/*oc         96*/
      scf(ix+9)=0.                                                              !/*oc         97*/
      m1c=22                                                                    !/*oc         98*/
!                                                                               !/*oc         99*/
!        comment on surface simplifications.                                    !/*oc        100*/
  120 scf(nx)=0.                                                                !/*oc        101*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*oc        102*/
     & '55hthis surface has been replaced by a surface of type '//              !/*oc        103*/
     & ksf(m1c))                                                                !/*oc        104*/
      go to 180                                                                 !/*oc        105*/
!                                                                               !/*oc        106*/
!        tori -- set up square of ellipse axis ratio.                           !/*oc        107*/
  130 if(scf(nx).gt.0..and.scf(nx-1).gt.0.)go to 140                            !/*oc        108*/
      call erprnt(2,1,0,0,0,0,0,0,                                              !/*oc        109*/
     & '46hat least one ellipse semi-axis length is zero.')                     !/*oc        110*/
      go to 180                                                                 !/*oc        111*/
  140 if(scf(nx-2).eq.0.)go to 150                                              !/*oc        112*/
      ix=ix+1                                                                   !/*oc        113*/
      scf(nx+1)=(scf(nx)/scf(nx-1))**2                                          !/*oc        114*/
      if(scf(nx).gt.scf(nx-2))                                                  !/*oc        115*/
     & call erprnt(2,2,0,0,0,0,0,0,'17hdegenerate torus.')                      !/*oc        116*/
      if(scf(nx).eq.scf(nx-2))call erprnt(2,2,0,0,0,0,0,0,                      !/*oc        117*/
     & '53hsingular torus.  it can fail in plotting or tracking.')              !/*oc        118*/
      go to 180                                                                 !/*oc        119*/
!                                                                               !/*oc        120*/
!        change a completely degenerate torus into an ellipsoid.                !/*oc        121*/
  150 scf(nx+1)=-scf(nx)**2                                                     !/*oc        122*/
      a=(scf(nx)/scf(nx-1))**2                                                  !/*oc        123*/
      do 160 i=1,3                                                              !/*oc        124*/
      scf(nx+1+i)=scf(nx-6+i)                                                   !/*oc        125*/
      scf(nx-6+i)=1.                                                            !/*oc        126*/
  160 scf(nx-3+i)=0.                                                            !/*oc        127*/
      scf(nx+m1c-29)=a                                                          !/*oc        128*/
      m1c=22                                                                    !/*oc        129*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*oc        130*/
     & '59hcompletely degenerate torus has been replaced by ellipsoid.')        !/*oc        131*/
      go to 180                                                                 !/*oc        132*/
!                                                                               !/*oc        133*/
!        translate point-defined surfaces into standard surfaces.               !/*oc        134*/
  170 call psurf                                                                !/*oc        135*/
!                                                                               !/*oc        136*/
!        increment coefficients count and set up the final surface type.        !/*oc        137*/
  180 lsc(llsc+mxj+1)=ix+ns(m1c)                                                !/*oc        138*/
      kst(lkst+mxj)=m1c                                                         !/*oc4a        2*/
      return                                                                    !/*oc        140*/
!                                                                               !/*oc        141*/
! >>>>>  surface transformations                                      tr        !/*oc        142*/
  190 call trfmat(mxtr)                                                         !/*oc        143*/
      return                                                                    !/*oc        144*/
!                                                                               !/*oc        145*/
! >>>>>  vectors                                                    vect        !/*oc        146*/
  200 if(mod(nwc,4).ne.0)call erprnt(2,1,0,0,0,0,0,0,                           !/*oc        147*/
     & '24hwrong number of entries.')                                           !/*oc        148*/
      return                                                                    !/*oc        149*/
!                                                                               !/*oc        150*/
! >>>>>  cell importances                                            imp        !/*oc        151*/
  210 if(nwc.ne.mxa)call erprnt(2,1,2,nwc,mxa,0,0,0,                            !/*oc4a        3*/
     & 'i5,39h entries not equal to number of cells =,i5,1h.')                  !/*oc4a        4*/
      return                                                                    !/*oc        154*/
!                                                                               !/*oc        155*/
! >>>>>  cell volumes for tallies                                    vol        !/*oc        156*/
  220 if(novol.eq.0.or.nwc.ne.0)go to 500                                       !/*oc        157*/
      return                                                                    !/*oc        158*/
!                                                                               !/*oc        159*/
! >>>>>  surface areas for tallies                                  area        !/*oc        160*/
  230 if(nwc.ne.mxj)call erprnt(2,2,2,nwc,mxj,0,0,0,                            !/*oc4a        5*/
     & 'i5,42h entries not equal to number of surfaces =,i5,1h.')               !/*oc4a        6*/
      return                                                                    !/*oc        163*/
!                                                                               !/*oc        164*/
! >>>>>  weight-window energies                                      wwe        !/*oc        165*/
  240 do 260 j=1,mipt                                                           !/*oc        166*/
      if(nqp(j).eq.0)go to 260                                                  !/*oc        167*/
      do 250 i=2,nww(j)                                                         !/*oc        168*/
  250 if(wwe(lwwe+i+mww(j)).lt.wwe(lwwe+i-1+mww(j)))go to 9010                  !/*oc        169*/
  260 continue                                                                  !/*oc        170*/
      return                                                                    !/*oc        171*/
!                                                                               !/*oc        172*/
! >>>>>  cells where fission is treated like capture                nonu        !/*oc        173*/
  270 if(nwc.ne.0)go to 500                                                     !/*oc        174*/
      do 280 i=1,mxa                                                            !/*oc        175*/
  280 lfcl(llfc+i)=-1                                                           !/*oc        176*/
      return                                                                    !/*oc        177*/
!                                                                               !/*oc        178*/
! >>>>>  source distributions                                      si,ds        !/*oc        179*/
!        m1c=distribution index.                                                !/*oc        180*/
!        m2c=current location in spf.                                           !/*oc        181*/
!        m3c=location of n in kcp.                                              !/*oc        182*/
!        m4c=nwc of last slash.                                                 !/*oc        183*/
  290 ksd(lksd+4,m1c)=m2c                                                       !/*oc        184*/
      if(ksd(lksd+20,m1c).eq.0)mxxs=mxxs+4*m2c                                  !/*oc        185*/
      if(abs(ksd(lksd+2,m1c))+ksd(lksd+5,m1c)+ksd(lksd+6,m1c)+                  !/*oc        186*/
     & ksd(lksd+7,m1c).ne.0)return                                              !/*oc        187*/
      do 300 i=2,nwc                                                            !/*oc        188*/
      t1=spf(ksd(lksd+13,m1c)+1,i)-spf(ksd(lksd+13,m1c)+1,i-1)                  !/*oc        189*/
      if(ksd(lksd+19,m1c).ne.0.and.t1.eq.0.)call erprnt(2,2,0,0,0,0,0,0,        !/*oc        190*/
     & '23hsome entries are equal.')                                            !/*oc        191*/
      if(ksd(lksd+19,m1c).ne.0.and.t1.eq.0.)return                              !/*oc        192*/
  300 if((ksd(lksd+19,m1c).eq.0.and.t1.eq.0.).or.t1.lt.0.)go to 9010            !/*oc        193*/
      return                                                                    !/*oc        194*/
!                                                                               !/*oc        195*/
! >>>>>  source distributions                                      sp,sb        !/*oc        196*/
!        m1c=distribution index.                                                !/*oc        197*/
!        m2c=flag for c.                                                        !/*oc        198*/
!        m3c=flag for function.                                                 !/*oc        199*/
  310 if(ich.eq.'sb'.and.m3c.ne.0.and.ksd(lksd+20,m1c).ne.0.or.                 !/*cor4-2    110*/
     & ich.eq.'sp'.and.m3c.eq.0.and.ksd(lksd+2,m1c).ne.0)                       !/*cor4-2    111*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*cor4-2    112*/
     & '49hregular table on sp card and function on sb card.')                  !/*cor4-2    113*/
      if(m3c.ne.0)return                                                        !/*cor4-2    114*/
      j=ichar('p')+ichar('b')-ichar(ich(2:2))                                   !/*oc        201*/
      if(ksd(lksd+20,m1c).ne.0.and.nwc.ne.ksd(lksd+20,m1c))                     !/*oc        202*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*oc        203*/
     & '41hdifferent number of entries from s'//char(j)//' card.')              !/*oc        204*/
      if(ksd(lksd+4,m1c)+ksd(lksd+20,m1c).eq.0)mxxs=mxxs+4*nwc                  !/*oc        205*/
      ksd(lksd+20,m1c)=nwc                                                      !/*oc        206*/
      if(m2c.eq.0)return                                                        !/*oc        207*/
      i=index('pb',ich(2:2))+1                                                  !/*oc        208*/
      do 320 j=2,nwc                                                            !/*oc        209*/
      spf(ksd(lksd+13,m1c)+i,nwc+2-j)=spf(ksd(lksd+13,m1c)+i,nwc+2-j)-          !/*oc        210*/
     & spf(ksd(lksd+13,m1c)+i,nwc+1-j)                                          !/*oc        211*/
  320 if(spf(ksd(lksd+13,m1c)+i,nwc+2-j).lt.0.)go to 9010                       !/*oc        212*/
      return                                                                    !/*oc        213*/
!                                                                               !/*oc        214*/
! >>>>>  tally cards                 pd,f,fx,fy,fz,pi,tir,tic,ft,fc,e,t,        !/*ocx         9*/
!                                  c,fm,de,df,em,tm,cm,cf,sf,fs,sd,fu,dd        !/*ocx        10*/
!        m1c=indicator of incorrect use of parentheses.                         !/*oc        217*/
!        m2c=nwc of last ft-card parameter count.                               !/*oc        218*/
  330 if(ich.eq.'dd'.and.(icn.eq.1.or.icn.eq.2))return                          !/*ocxc        2*/
#ifndef RADIOG                                                                  /*!ocx        16*/
      if(ich.eq.'pi'.or.ich.eq.'tir'.or.ich.eq.'tic')return                     !/*ocx        17*/
      if(ich.eq.'c'.and.mod(icn,10).eq.5)return                                 !/*ocx        18*/
      if(ich.eq.'fs'.and.mod(icn,10).eq.5)return                                !/*ocx        19*/
#endif                                                                          /*!ocx        20*/
      if(ich.ne.'ft')go to 334                                                  !/*oc4a        7*/
      if(nint(rtp(lrtp+ipl+m2c-1)).ne.2.or.nwc-m2c.ne.2)go to 332               !/*oc4a        8*/
      nwc=nwc+1                                                                 !/*oc4a        9*/
      rtp(lrtp+ipl+nwc)=0.                                                      !/*oc4a       10*/
  332 rtp(lrtp+ipl+m2c)=nwc-m2c                                                 !/*oc4a       11*/
  334 ipnt(lipn+2,krq(3,ica),ital)=nwc                                          !/*oc4a       12*/
      if(ich.eq.'pd')ipnt(lipn+2,krq(3,ica),ital)=mxa                           !/*oc        222*/
      if(ich.eq.'fc')ipnt(lipn+2,krq(3,ica),ital)=25*nwc                        !/*oc        223*/
      ipl=ipl+ipnt(lipn+2,krq(3,ica),ital)                                      !/*oc        224*/
      if(ich.eq.'pd')go to 500                                                  !/*oc        225*/
      if(m1c.ne.0)call erprnt(2,1,0,0,0,0,0,0,                                  !/*oc4c       17*/
     & '29hparentheses used incorrectly.')                                      !/*oc        227*/
      if((ich.eq.'f'.or.ich.eq.'fx'.or.ich.eq.'fy'.or.ich.eq.'fz').and.         !/*oc        228*/
     & nwc.eq.0)call erprnt(2,1,0,0,0,0,0,0,'11hno entries.')                   !/*oc        229*/
      if((ich.eq.'fx'.or.ich.eq.'fy'.or.ich.eq.'fz').and.mod(nwc,3).ne.0        !/*oc        230*/
#ifdef RADIOG                                                                   /*!ocx        21*/
     & .or.(ich.eq.'pi'.or.ich.eq.'tir'.or.ich.eq.'tic').and.                   !/*ocx        22*/
     & mod(icn,10).eq.5.and.mod(nwc,10).ne.0                                    !/*ocx        23*/
#endif                                                                          /*!ocx        24*/
     & .or.ich.eq.'f'.and.mod(icn,10).eq.5.and.mod(nwc,4).ne.0)                 !/*oc        231*/
     & call erprnt(2,1,0,0,0,0,0,0,'31hnumber of entries is incorrect.')        !/*oc        232*/
      if(ich.eq.'c'.and.                                                        !/*ocx        25*/
#ifdef RADIOG                                                                   /*!ocx        26*/
     & mod(icn,10).ne.5.and.                                                    !/*ocx        27*/
#endif                                                                          /*!ocx        28*/
     & rtp(lrtp+ipl).ne.1.)call erprnt(2,1,0,0,0,0,0,0,                         !/*ocx        29*/
     & '33hlast cosine bin does not equal 1.')                                  !/*ocx        30*/
      if(ich.eq.'fs'.and.                                                       !/*ocx        31*/
#ifdef RADIOG                                                                   /*!ocx        32*/
     & mod(icn,10).ne.5.and.                                                    !/*ocx        33*/
#endif                                                                          /*!ocx        34*/
     & nwc.gt.mlgc)call erprnt(2,1,1,mlgc,0,0,0,0,                              !/*ocx        35*/
     & '10hmore than ,i5,9h entries.')                                          !/*ocx        36*/
      if(ich.ne.'e'.and.ich.ne.'t'.and.ich.ne.'c')return                        !/*ocxc        3*/
      do 340 i=ipl-nwc+2,ipl                                                    !/*oc        238*/
  340 if(rtp(lrtp+i).lt.rtp(lrtp+i-1))go to 9010                                !/*oc        239*/
      return                                                                    !/*oc        240*/
!                                                                               !/*oc        241*/
! >>>>>  dxtran parameters                                           dxt        !/*oc        242*/
  350 do 380 i=1,mipt                                                           !/*oc        243*/
      if(nqp(i).eq.0)go to 380                                                  !/*oc        244*/
      if(ndx(i).gt.1.and.dxw(i,2).eq.0.)call erprnt(2,2,0,0,0,0,0,0,            !/*oc        245*/
     & '54hmore than one '//hnp(i)//' sphere and no weight cutoff.')            !/*ocx        37*/
      do 360 k=1,ndx(i)                                                         !/*oc        247*/
  360 if(dxx(i,5,k).lt.dxx(i,4,k))call erprnt(2,1,1,k,0,0,0,0,                  !/*oc        248*/
     & '22houter radius of sphere,i2,27h is less than inner radius.')           !/*oc        249*/
      if(min(dxw(i,1),dxw(i,2)).lt.0.)                                          !/*oc        250*/
     & call erprnt(2,1,0,0,0,0,0,0,'30hnegative dxtran weight cutoff.')         !/*oc        251*/
      if(dxw(i,1).lt.dxw(i,2))call erprnt(2,1,0,0,0,0,0,0,                      !/*oc        252*/
     & '24hwc1 is smaller than wc2.')                                           !/*oc        253*/
      do 370 k=1,ndx(i)                                                         !/*oc        254*/
      do 370 j=k+1,ndx(i)                                                       !/*oc        255*/
  370 if((dxx(i,1,k)-dxx(i,1,j))**2+(dxx(i,2,k)-dxx(i,2,j))**2+                 !/*oc        256*/
     & (dxx(i,3,k)-dxx(i,3,j))**2.lt.(sqrt(dxx(i,5,k))+                         !/*oc        257*/
     & sqrt(dxx(i,5,j)))**2)call erprnt(2,1,2,k,j,0,0,0,                        !/*oc        258*/
     & '14hdxtran spheres,i3,4h and,i3,9h overlap.')                            !/*oc        259*/
  380 continue                                                                  !/*oc        260*/
      return                                                                    !/*oc        261*/
!                                                                               !/*oc        262*/
! >>>>>  material specifications                                       m        !/*oc        263*/
  390 if(mod(nwc,2).ne.0)                                                       !/*oc        264*/
     & call erprnt(2,1,0,0,0,0,0,0,'22hodd number of entries.')                 !/*oc        265*/
      if(mix.lt.jmd(ljmd+nmat))call erprnt(2,1,0,0,0,0,0,0,                     !/*oc        266*/
     & '22hno non-zero fractions.')                                             !/*oc        267*/
      npq(lnpq+nmat)=nwc/2                                                      !/*oc        268*/
      jmd(ljmd+nmat+1)=jmd(ljmd+nmat)+npq(lnpq+nmat)                            !/*oc        269*/
      do 395 m=jmd(ljmd+nmat),jmd(ljmd+nmat+1)-2                                !/*oc4c2       4*/
      do 395 i=m+1,jmd(ljmd+nmat+1)-1                                           !/*oc4c       18*/
  395 if(iza(liza+m).eq.iza(liza+i))                                            !/*oc4a       16*/
     & call erprnt(1,2,2,iza(liza+m),nmt(lnmt+nmat),0,0,0,                      !/*oc4a       17*/
     & '18hduplicate isotope ,i6,17h within material ,i5,1h.')                  !/*oc4a       18*/
      return                                                                    !/*oc        270*/
!                                                                               !/*oc        271*/
! >>>>>  nuclides for discrete treatment                            drxs        !/*oc        272*/
  400 if(nwc.eq.0)kdr(lkdr+1)=-1                                                !/*oc        273*/
      return                                                                    !/*oc        274*/
!                                                                               !/*oc        275*/
! >>>>>  total or prompt nubar                                     totnu        !/*oc        276*/
  410 if(nwc.eq.0)itotnu=1                                                      !/*oc        277*/
      return                                                                    !/*oc        278*/
!                                                                               !/*oc        279*/
! >>>>>  atomic weights                                            awtab        !/*oc        280*/
  420 if(mod(nwc,2).ne.0)call erprnt(2,1,0,0,0,0,0,0,                           !/*oc        281*/
     & '22hodd number of entries.')                                             !/*oc        282*/
      do 430 i=1,nwc/2                                                          !/*oc        283*/
      do 430 j=i+1,nwc/2                                                        !/*oc        284*/
  430 if(kaw(lkaw+i).eq.kaw(lkaw+j))go to 440                                   !/*oc        285*/
      return                                                                    !/*oc        286*/
  440 call erprnt(2,1,0,0,0,0,0,0,'18hduplicate entries.')                      !/*oc        287*/
      return                                                                    !/*oc        288*/
!                                                                               !/*oc        289*/
! >>>>>  thermal times                                             thtme        !/*oc        290*/
  450 do 460 i=2,nwc                                                            !/*oc        291*/
  460 if(tth(ltth+i).lt.tth(ltth+i-1))go to 9010                                !/*oc        292*/
      return                                                                    !/*oc        293*/
!                                                                               !/*oc        294*/
! >>>>>  print control                                             print        !/*oc        295*/
  470 if(nwc.ne.0)return                                                        !/*oc        296*/
      do 480 i=1,mink                                                           !/*oc        297*/
  480 ink(i)=1                                                                  !/*oc        298*/
      return                                                                    !/*oc        299*/
!                                                                               !/*oc        300*/
! >>>>>  kcode specifications                                      kcode        !/*ocx        38*/
  490 if(nsrck.eq.0)nsrck=1000                                                  !/*oc4c       20*/
      if(ikz.eq.0)kc8=-1                                                        !/*oc4b        2*/
      return                                                                    !/*oc        304*/
!                                                                               !/*oc4c       23*/
! >>>>>  wwg parameters                                              wwg        !/*oc4c       24*/
  495 if(wwg(2).ne.0.)go to 496                                                 !/*oc4c       25*/
      if(iwwg.ne.0)return                                                       !/*oc4c       26*/
      call erprnt(1,1,0,0,0,0,0,1,                                              !/*oc4c       27*/
     & '53hreference cell equals 0 on wwg card but no mesh card.')              !/*oc4c       28*/
      return                                                                    !/*oc4c       29*/
  496 mai=namchg(1,nint(wwg(2)))                                                !/*oc4c       30*/
      if(mai.eq.0)call erprnt(1,1,0,0,0,0,0,1,                                  !/*oc4c       31*/
     & '33hreference cell on wwg card wrong.')                                  !/*oc4c       32*/
      if(imesh(1).eq.0)return                                                   !/*oc4c       33*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*oc4c       34*/
     & '48hcell-based wwg called for but mesh card present.')                   !/*oc4c       35*/
      iwwg=1                                                                    !/*oc4c       36*/
      return                                                                    !/*oc4c       37*/
!                                                                               !/*oc        305*/
! >>>>>  various cell data          vol,pwt,ext,fcl,wwn,pd,dxc,tmp,u,lat        !/*oc        306*/
  500 if(nwc.ne.mxa)call erprnt(2,2,2,nwc,mxa,0,0,0,                            !/*oc4a       19*/
     & 'i5,39h entries not equal to number of cells =,i5,1h.')                  !/*oc4a       20*/
      return                                                                    !/*oc        309*/
!                                                                               !/*oc        310*/
! >>>>>  energy bins for weight-window generator                    wwge        !/*oc        311*/
  510 do 540 j=1,mipt                                                           !/*oc        312*/
      if(nqp(j).eq.0)go to 540                                                  !/*oc        313*/
      do 520 i=2,nwc                                                            !/*oc        314*/
  520 if(ewwg(lewg+i+mgww(j)).le.ewwg(lewg+i-1+mgww(j)))go to 9010              !/*oc        315*/
      if(nwc.ne.0)go to 540                                                     !/*oc        316*/
      do 530 i=1,10                                                             !/*oc        317*/
  530 ewwg(lewg+i+mgww(j))=10.**(i-8)                                           !/*oc        318*/
  540 continue                                                                  !/*oc        319*/
      return                                                                    !/*oc        320*/
!                                                                               !/*oc        321*/
! >>>>>  surface source write information                            ssw        !/*oc        322*/
  550 do 560 i=1,mipt                                                           !/*oc        323*/
  560 if(ipty(i).ne.0)go to 580                                                 !/*oc        324*/
      do 570 i=1,mipt                                                           !/*oc        325*/
  570 ipty(i)=1                                                                 !/*oc        326*/
  580 if(nsph.gt.2.or.nsph.lt.0)call erprnt(2,1,0,0,0,0,0,0,                    !/*oc4b        3*/
     & '34hthe value of sym must = 0, 1 or 2.')                                 !/*oc4b        4*/
      if(nsph.ne.1)return                                                       !/*oc        329*/
      if(njss.gt.1)call erprnt(2,1,0,0,0,0,0,0,                                 !/*oc        330*/
     & '36hsym=1 requires only one ssw surface.')                               !/*oc        331*/
      kx=kst(lkst+namchg(2,abs(jss(ljss+1))))                                   !/*oc        332*/
      if(kx.lt.5.or.kx.gt.9)call erprnt(2,1,0,0,0,0,0,0,                        !/*oc        333*/
     & '39hsym=1 requires a spherical ssw surface.')                            !/*oc        334*/
      if(nilw.ne.0)call erprnt(2,1,0,0,0,0,0,0,                                 !/*oc        335*/
     & '31hsym=1 is incompatible with cel.')                                    !/*oc        336*/
      return                                                                    !/*oc        337*/
!                                                                               !/*oc        338*/
! >>>>>  surface source read information                             ssr        !/*oc        339*/
  590 if(nrcd.ne.6)go to 600                                                    !/*oc        340*/
      if(idefv(11).ne.idefv(12))call erprnt(2,1,0,0,0,0,0,0,                    !/*oc        341*/
     & '33hboth ext and axs must be defined.')                                  !/*oc        342*/
      if(idefv(6).ne.0.and.idefv(10).ne.0)call erprnt(2,2,0,0,0,0,0,0,          !/*oc        343*/
     & '36hpoa is not used when bcw is defined.')                               !/*oc        344*/
      return                                                                    !/*oc        345*/
  600 if(idefv(11).ne.0)call erprnt(2,1,0,0,0,0,0,0,                            !/*oc        346*/
     & '54hext is illegal without a spherically symmetric source.')             !/*oc        347*/
      if(idefv(10).ne.0)call erprnt(2,1,0,0,0,0,0,0,                            !/*oc        348*/
     & '54hbcw is illegal without a spherically symmetric source.')             !/*oc        349*/
      if(idefv(6).ne.0)call erprnt(2,2,0,0,0,0,0,0,                             !/*oc        350*/
     & '58hpoa is ineffective without a spherically symmetric source.')         !/*oc        351*/
      return                                                                    !/*oc        352*/
!                                                                               !/*oc4a       21*/
! >>>>>  photon-production bias                                    pikmt        !/*oc4a       22*/
  605 if(mod(nwc,2).ne.0)call erprnt(2,1,0,0,0,0,0,0,                           !/*oc4a       23*/
     & '32hodd number of entries for pikmt.')                                   !/*oc4a       24*/
      return                                                                    !/*oc4a       25*/
!                                                                               !/*oc        353*/
! >>>>>  bremsstrahlung biasing                                    bbrem        !/*oc        354*/
  610 if(nwc.lt.49)call erprnt(2,1,2,nwc,49,0,0,0,                              !/*oc4c       38*/
     & '16htoo few entries:,i5,11h were read,,i5,14h are required.')            !/*oc4a       27*/
      m=49+max(kpt(3),kpt(2)*(1-ides))*nmat1                                    !/*oc4c       39*/
      if(nwc.gt.m)call erprnt(2,1,2,nwc,m,0,0,0,                                !/*oc4a       29*/
     & '17htoo many entries:,i5,11h were read,,i5,14h are required.')           !/*oc4a       30*/
      if(nwc.le.49)call erprnt(2,2,0,0,0,0,0,0,                                 !/*oc4c2       5*/
     & '55hbbrem material entry missing.  default is 1st material.')            !/*oc4c2       6*/
      return                                                                    !/*oc        356*/
!                                                                               !/*oc        357*/
! >>>>>  runtime tally monitor                                     mplot        !/*oc        358*/
  620 write(iu4,'(1h /6hreturn)')                                               !/*oc        359*/
      endfile iu4                                                               !/*oc        360*/
      return                                                                    !/*oc        361*/
!                                                                               !/*oc4a       31*/
! >>>>>  ptrac specifications                                      ptrac        !/*oc4a       32*/
  630 i1=0                                                                      !/*oc4a       33*/
      i2=0                                                                      !/*oc4a       34*/
      do 700 i=1,nptr                                                           !/*oc4a       35*/
      j=iptra(i)                                                                !/*oc4a       36*/
      if(j.eq.0)go to 700                                                       !/*oc4a       37*/
      if(j.lt.0)go to 640                                                       !/*oc4a       38*/
!                                                                               !/*oc4a       39*/
!        set keyword flags and check the number of entries.                     !/*oc4a       40*/
      if(hptr(i).eq.'cell'.or.hptr(i).eq.'surface')iptr=sign(2,iptr)            !/*oc4a       41*/
      if(hptr(i).eq.'tally'.and.abs(iptr).ne.2)iptr=sign(3,iptr)                !/*oc4a       42*/
      n=nint(ptr(j))                                                            !/*oc4a       43*/
      if(hptr(i).eq.'file'.or.hptr(i).eq.'max'.or.hptr(i).eq.'meph'.or.         !/*oc4a       44*/
     &   hptr(i).eq.'write'.or.hptr(i).eq.'buffer')go to 660                    !/*oc4a       45*/
      iptra(i)=-iptra(i)                                                        !/*oc4a       46*/
      if(hptr(i).eq.'nps')go to 670                                             !/*oc4a       47*/
      if(hptr(i).eq.'filter')go to 680                                          !/*oc4a       48*/
      if(hptr(i).eq.'type'.and.n.gt.3)go to 650                                 !/*oc4a       49*/
      if(hptr(i).eq.'event'.and.n.gt.5)go to 650                                !/*oc4a       50*/
      if(hptr(i).eq.'value')i1=n                                                !/*oc4a       51*/
      if(hptr(i).eq.'tally')i2=n                                                !/*oc4a       52*/
      go to 700                                                                 !/*oc4a       53*/
!                                                                               !/*oc4a       54*/
!        print error messages.                                                  !/*oc4a       55*/
  640 iptra(i)=1                                                                !/*oc4a       56*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*oc4a       57*/
     & '53hno entries for ptrac keyword '//hptr(i)//'-keyword ignored.')        !/*oc4a       58*/
      iptra(i)=0                                                                !/*oc4a       59*/
      go to 700                                                                 !/*oc4a       60*/
  650 call erprnt(1,1,0,0,0,0,0,0,                                              !/*oc4a       61*/
     & '52hinvalid number of entries for ptrac keyword '//hptr(i)//'.')         !/*oc4a       62*/
      go to 700                                                                 !/*oc4a       63*/
  660 if(n.gt.1)go to 650                                                       !/*oc4a       64*/
      k=nint(ptr(j+1))                                                          !/*oc4a       65*/
      if(hptr(i).eq.'max'.and.k.lt.0)iptr=-iptr                                 !/*oc4a       66*/
      iptra(i)=abs(k)                                                           !/*oc4a       67*/
      go to 700                                                                 !/*oc4a       68*/
  670 if(n.gt.2)go to 650                                                       !/*oc4a       69*/
      if(n.lt.2)go to 700                                                       !/*oc4a       70*/
  680 do 690 k=1,n,3                                                            !/*oc4a       71*/
  690 if(ptr(j+k).gt.ptr(j+k+1))call erprnt(1,1,0,0,0,0,0,0,                    !/*oc4a       72*/
     & '40hinvalid range for ptrac keyword '//hptr(i)//'.')                     !/*oc4a       73*/
  700 continue                                                                  !/*oc4a       74*/
!                                                                               !/*oc4a       75*/
!        set defaults and check for incompatible keywords.                      !/*oc4a       76*/
      do 720 i=1,nptr                                                           !/*oc4a       77*/
      if(iptra(i).eq.0)go to 710                                                !/*oc4a       78*/
      if(hptr(i).eq.'value'.and.i2.eq.0)call erprnt(1,1,0,0,0,0,0,0,            !/*oc4a       79*/
     & '43hptrac keyword value requires tally keyword.')                        !/*oc4a       80*/
      if(hptr(i).eq.'tally'.and.i1.ne.0.and.i1.ne.i2)                           !/*oc4a       81*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*oc4a       82*/
     & '52hptrac keyword value has incorrect number of entries.')               !/*oc4a       83*/
      go to 720                                                                 !/*oc4a       84*/
  710 if(hptr(i).eq.'file')iptra(i)=2                                           !/*oc4a       85*/
      if(hptr(i).eq.'max')iptra(i)=10000                                        !/*oc4a       86*/
      if(hptr(i).eq.'write')iptra(i)=1                                          !/*oc4a       87*/
      if(hptr(i).eq.'buffer')iptra(i)=100                                       !/*oc4a       88*/
  720 continue                                                                  !/*oc4a       89*/
      return                                                                    !/*oc4a       90*/
!                                                                               !/*oc4b        5*/
! >>>>>  perturbation specificatons                                 pert        !/*oc4b        6*/
  730 iptb(lipb+1,npert)=icn                                                    !/*oc4b        7*/
      iptb(lipb+2,npert)=nqw                                                    !/*oc4b        8*/
      do 735 i=1,mipt                                                           !/*ocx        41*/
  735 kpptb(lpptb+i,npert)=nqp(i)                                               !/*ocx        42*/
      i1=0                                                                      !/*oc4b        9*/
!                                                                               !/*oc4b       10*/
!        check the number of entries.                                           !/*oc4b       11*/
      do 770 i=1,npkey                                                          !/*oc4b       12*/
      n=iptb(lipb+1+2*i,npert)                                                  !/*oc4b       13*/
      if(n.eq.0)go to 740                                                       !/*oc4b       14*/
      if(hptb(i).eq.'mat'.or.hptb(i).eq.'rho')i1=1                              !/*oc4b       15*/
      if((hptb(i).eq.'mat'.or.hptb(i).eq.'rho').and.n.gt.1)go to 750            !/*oc4b       16*/
      if(hptb(i).ne.'erg')go to 770                                             !/*oc4b       17*/
      if(n.ne.2)go to 750                                                       !/*oc4b       18*/
      j=iptb(lipb+2+2*i,npert)                                                  !/*oc4b       19*/
      if(rptb(lrpt+j).ge.rptb(lrpt+j+1))go to 760                               !/*oc4b       20*/
      go to 770                                                                 !/*oc4b       21*/
!                                                                               !/*oc4b       22*/
!        print error messages.                                                  !/*oc4b       23*/
  740 if(hptb(i).eq.'cell')call erprnt(2,1,0,0,0,0,0,0,                         !/*oc4b       24*/
     & '40hinput required for pert keyword '//hptb(i)//'.')                     !/*oc4b       25*/
      go to 770                                                                 !/*oc4b       26*/
  750 call erprnt(2,1,0,0,0,0,0,0,                                              !/*oc4b       27*/
     & '51hinvalid number of entries for pert keyword '//hptb(i)//'.')          !/*oc4b       28*/
      go to 770                                                                 !/*oc4b       29*/
  760 call erprnt(2,1,0,0,0,0,0,0,                                              !/*oc4b       30*/
     & '54hinvalid energy interval specified by pert keyword erg.')             !/*oc4b       31*/
  770 continue                                                                  !/*oc4b       32*/
      if(i1.eq.0)call erprnt(2,1,0,0,0,0,0,0,                                   !/*oc4b       33*/
     & '43hinput required for pert keyword mat or rho.')                        !/*oc4b       34*/
      if(nqp(3).ne.0)call erprnt(2,1,0,0,0,0,0,0,                               !/*oc4b       35*/
     & '51hpert card is currently incompatible with electrons.')                !/*oc4b       36*/
      return                                                                    !/*oc4b       37*/
!                                                                               !/*oc4c       42*/
! >>>>>  superimposed weight window generator mesh                  mesh        !/*oc4c       43*/
  780 i=1                                                                       !/*oc4c       44*/
      if(imesh(i).gt.0.and.imesh(i).lt.3)go to 890                              !/*oc4c       45*/
      if(imesh(1).eq.0)call erprnt(1,1,0,0,0,0,0,1,                             !/*oc4c       46*/
     & '35href keyword missing from mesh card.')                                !/*oc4c       47*/
      if(iwwg.eq.2.or.nwgeoa.eq.3)return                                        !/*oc4c       48*/
      if(imesh(6).eq.0)call erprnt(1,1,0,0,0,0,0,1,                             !/*oc4c       49*/
     & '37himesh keyword missing from mesh card.')                              !/*oc4c       50*/
      if(imesh(8).eq.0)call erprnt(1,1,0,0,0,0,0,1,                             !/*oc4c       51*/
     & '37hjmesh keyword missing from mesh card.')                              !/*oc4c       52*/
      if(imesh(10).eq.0)call erprnt(1,1,0,0,0,0,0,1,                            !/*oc4c       53*/
     & '37hkmesh keyword missing from mesh card.')                              !/*oc4c       54*/
      if(imesh(2).eq.0)wwma(4)=0.                                               !/*oc4c       55*/
      if(imesh(2).eq.0)wwma(5)=0.                                               !/*oc4c       56*/
      if(imesh(2).eq.0)wwma(6)=0.                                               !/*oc4c       57*/
!                                                                               !/*oc4c       58*/
!        rectangular mesh.                                                      !/*oc4c       59*/
      if(nwgeoa.eq.2)go to 810                                                  !/*oc4c       60*/
      do 790 i=1,2                                                              !/*oc4c       61*/
  790 if(imesh(i).gt.0.and.imesh(i).lt.3)go to 890                              !/*oc4c       62*/
      if(imesh(3).ne.0.or.imesh(4).ne.0)                                        !/*oc4c       63*/
     & call erprnt(1,2,0,0,0,0,0,0,                                             !/*oc4c       64*/
     &  '58hrectangular mesh and axs or vec keyword.  keyword ignored.')        !/*oc4c       65*/
      ir=5                                                                      !/*oc4c       66*/
      do 800 i=1,3                                                              !/*oc4c       67*/
      if(imesh(i*2+4).ne.0.and.wgma(lwga+ir).le.wwma(i+3))                      !/*oc4c       68*/
     & call erprnt(1,1,0,0,0,0,0,1,'53hfirst point on mesh keyword '            !/*oc4c       69*/
     & //hmsh(i*2+4)//' less than origin.')                                     !/*oc4c       70*/
  800 ir=ir+3*nint(wwma(i+6))+3                                                 !/*oc4c       71*/
      return                                                                    !/*oc4c       72*/
!                                                                               !/*oc4c       73*/
!        cylindrical mesh.                                                      !/*oc4c       74*/
  810 ir=9+3*nint(wwma(7)+wwma(8)+wwma(9))-1                                    !/*oc4c       75*/
      if(wgma(lwga+ir).ne.1)call erprnt(1,1,0,0,0,0,0,1,                        !/*oc4c       76*/
     & '42hlast theta coarse mesh position must be 1.')                         !/*oc4c       77*/
      ir=6+3*nint(wwma(7)+wwma(8))-1                                            !/*oc4c       78*/
      wwma(23)=wgma(lwga+ir)                                                    !/*oc4c       79*/
      ir=3+3*nint(wwma(7))-1                                                    !/*oc4c       80*/
      wwma(19)=wgma(lwga+ir)                                                    !/*oc4c       81*/
      if(imesh(3).eq.0)wwma(22)=1.                                              !/*oc4c       82*/
      if(imesh(4).eq.0)wwma(16)=1.                                              !/*oc4c       83*/
      do 820 i=1,4                                                              !/*oc4c       84*/
  820 if(imesh(i).gt.0.and.imesh(i).lt.3)go to 890                              !/*oc4c       85*/
      r=0.                                                                      !/*oc4c       86*/
      s=0.                                                                      !/*oc4c       87*/
      do 830 i=1,3                                                              !/*oc4c       88*/
      r=r+wwma(i+19)**2                                                         !/*oc4c       89*/
  830 s=s+wwma(i+15)**2                                                         !/*oc4c       90*/
      if(r.eq.0.)call erprnt(1,1,0,0,0,0,0,1,                                   !/*oc4c       91*/
     & '47hlength of cylindrical mesh axs vector equals 0.')                    !/*oc4c       92*/
      if(s.eq.0.)call erprnt(1,1,0,0,0,0,0,1,                                   !/*oc4c       93*/
     & '47hlength of cylindrical mesh vec vector equals 0.')                    !/*oc4c       94*/
      if(r.eq.0..or.s.eq.0.)go to 870                                           !/*oc4c       95*/
      r=1./sqrt(r)                                                              !/*oc4c       96*/
      s=1./sqrt(s)                                                              !/*oc4c       97*/
      do 840 i=1,3                                                              !/*oc4c       98*/
      wwma(i+19)=wwma(i+19)*r                                                   !/*oc4c       99*/
  840 wwma(i+15)=wwma(i+15)*s                                                   !/*oc4c      100*/
      d=dotpro(wwma(16),wwma(20))                                               !/*oc4c      101*/
      if(abs(1.-d).le.1.e-6)call erprnt(1,1,0,0,0,0,0,1,                        !/*oc4c      102*/
     & '50hcylindrical mesh axs and vec vectors are parallel.')                 !/*oc4c      103*/
      r=0.                                                                      !/*oc4c      104*/
      do 850 i=1,3                                                              !/*oc4c      105*/
      wwma(i+15)=wwma(i+15)-d*wwma(i+19)                                        !/*oc4c      106*/
  850 r=r+wwma(i+15)**2                                                         !/*oc4c      107*/
      if(r.gt.0.)r=1./sqrt(r)                                                   !/*oc4c      108*/
      do 860 i=1,3                                                              !/*oc4c      109*/
  860 wwma(i+15)=wwma(i+15)*r                                                   !/*oc4c      110*/
      call crspro(wwma(20),wwma(16),wwma(24))                                   !/*oc4c      111*/
  870 ir=5                                                                      !/*oc4c      112*/
      do 880 i=1,3                                                              !/*oc4c      113*/
      if(wgma(lwga+ir).le.0.)call erprnt(1,1,0,0,0,0,0,1,                       !/*oc4c      114*/
     & '47hnegative or zero entry on mesh keyword '//hmsh(i*2+4)//'.')          !/*oc4c      115*/
  880 ir=ir+3*nint(wwma(i+6))+3                                                 !/*oc4c      116*/
      return                                                                    !/*oc4c      117*/
  890 call erprnt(1,1,0,0,0,0,0,1,                                              !/*oc4c      118*/
     & '40htoo few entries on mesh keyword '//hmsh(i)//'.')                     !/*oc4c      119*/
      return                                                                    !/*oc4c      120*/
!                                                                               !/*oc4c2       7*/
! >>>>>  photonuclear isotope override                               mpn        !/*oc4c2       8*/
!        m1c = material index on mpn and preceding m card.                      !/*oc4c2       9*/
  900 if(m1c.ne.0.and.nwc.lt.npq(lnpq+m1c))call erprnt(2,1,0,0,0,0,0,0,         !/*oc4c2      10*/
     & '52hfewer entries on mpn card than corresponding m card.')               !/*oc4c2      11*/
      return                                                                    !/*oc4c2      12*/
!                                                                               !/*ocx        45*/
! >>>>>  Tally no print card.                                      talnp        !/*ocx        46*/
  910 continue                                                                  !/*ocx        47*/
#ifdef RADIOG                                                                   /*!ocx        48*/
      if(nwc.eq.0)notal=-1                                                      !/*ocx        49*/
#endif                                                                          /*!ocx        50*/
      return                                                                    !/*ocx        51*/
!                                                                               !/*ocx        52*/
! >>>>>  Secondary Particle Biasing                                spabi        !/*ocx        53*/
  920 continue                                                                  !/*ocx        54*/
#ifdef SPABI                                                                    /*!ocx        55*/
      call increment_spb_length(nwc-1)                                          !/*ocx        56*/
#endif                                                                          /*!ocx        57*/
      return                                                                    !/*ocx        58*/
!                                                                               !/*oc        362*/
!        a common complaint                                                     !/*oc        363*/
 9010 call erprnt(2,1,0,0,0,0,0,0,                                              !/*oc        364*/
     & '41hentries are not monotonically increasing.')                          !/*oc        365*/
      return                                                                    !/*oc        366*/
!                                                                               !/*oc        367*/
! >>>>>  first spare card type                                        za        !/*oc        368*/
 9910 continue                                                                  !/*oc        369*/
      return                                                                    !/*oc        370*/
!                                                                               !/*oc        371*/
! >>>>>  second spare card type                                       zb        !/*oc        372*/
 9920 continue                                                                  !/*oc        373*/
      return                                                                    !/*oc        374*/
!                                                                               !/*oc        375*/
! >>>>>  third spare card type                                        zc        !/*oc        376*/
 9930 continue                                                                  !/*oc        377*/
      return                                                                    !/*oc        378*/
      end                                                                       !/*oc        379*/

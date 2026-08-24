!#ident nextit.F from deck ny                                                   !/*ny          1*/
      subroutine nextit                                                         !/*ny          2*/
!        process the next input item.                                           !/*ny          3*/
      use global_data                                                           !/*nyx         1*/
      use imcn_data                                                             !/*nyx         2*/
!                                                                               !/*nyx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*nyx         4*/
      implicit integer(kindi) (i-n)                                             !/*nyx         5*/
!                                                                               !/*nyx         6*/
      character ht*75                                                           !/*ny          6*/
!                                                                               !/*ny          7*/
!        check the item before storing it.                                      !/*ny          8*/
      nwc=nwc+1                                                                 !/*ny          9*/
      call chekit                                                               !/*ny         10*/
      if(ics.lt.0)return                                                        !/*ny         11*/
      ks=index('():#',hitm(1:1))                                                !/*ny         12*/
!                                                                               !/*ny         13*/
      go to( 20, 70,100,110, 10,120,140,150,160,170,230,250,270, 10,610,        !/*ny4c        1*/
     &      310,330,340,410,410,340,430,450,610,610,610,610,610,560,610,        !/*ny         15*/
     &      610,610,580,610,610,610,610,610,610,610,610,610,610,610,600,        !/*ny         16*/
     &      610,690,710,750,760,770,780,810,820,850)ica                         !/*ny         17*/
      go to(870,880,890,900,930,940,950,960,970,980,990,1000,1010,1050,         !/*ny         18*/
     &      1060,  10,1080,1100,1180,1250,1260,1330,1340, 10,1450,1460,         !/*ny4c        2*/
     &      1480,1490,1510,1620,1660,1720,1730,1800,1850,1900,1950,2000,        !/*nyx         7*/
     &        10,2050,2100,610,610,610,2200, 10,2210,9910,9920,9930)            !/*nyxc        1*/
     &      ica-55                                                              !/*nyxc        2*/
   10 return                                                                    !/*ny         21*/
!                                                                               !/*ny         22*/
! >>>>>  cell descriptions                                                      !/*ny         23*/
!        m2c=previous special character:  0=none  1=(  2=)  3=:  4=#            !/*ny         24*/
!        m3c=flag for cell parameters.                                          !/*ny         25*/
   20 if(hitm.eq.'like'.or.likef.ne.0)go to 50                                  !/*ny         26*/
      if(ks.eq.0.and.kitm.eq.0)m3c=1                                            !/*ny         27*/
      if(m3c.ne.0)return                                                        !/*ny         28*/
!                                                                               !/*ny         29*/
!        store the cell density.                                                !/*ny         30*/
      if(nwc.eq.2.and.mat(lmat+mxa).ne.0)rho(lrho+mxa)=ritm                     !/*ny         31*/
      if(nwc.eq.1.or.nwc.eq.2.and.mat(lmat+mxa).ne.0)go to 40                   !/*ny         32*/
!                                                                               !/*ny         33*/
!        prepare to store logical operator or surface name.                     !/*ny         34*/
      nlja=nlja+1                                                               !/*ny         35*/
      if(ks.eq.0)go to 30                                                       !/*ny         36*/
!                                                                               !/*ny         37*/
!        store logical operator as 1000000+ks.  ks:  1=(  2=)  3=:  4=#         !/*ny         38*/
      lca(llca+mxa)=-abs(lca(llca+mxa))                                         !/*ny         39*/
      lja(llja+nlja)=1000000+ks                                                 !/*ny         40*/
      go to 40                                                                  !/*ny         41*/
!                                                                               !/*ny         42*/
!        store the name of a surface.                                           !/*ny         43*/
   30 lja(llja+nlja)=iitm                                                       !/*ny         44*/
!                                                                               !/*ny4c        4*/
!        store macrobody surface and facet number.                              !/*ny4c        5*/
      if(kitm.eq.2)go to 40                                                     !/*ny4c        6*/
      lja(llja+nlja)=nint(sign(aint(abs(ritm)),ritm))                           !/*ny4c        7*/
      idna(lida+nlja)=-nint(10.*(abs(ritm)-aint(abs(ritm))))                    !/*ny4c        8*/
!                                                                               !/*ny         45*/
   40 m2c=ks                                                                    !/*ny         46*/
      return                                                                    !/*ny         47*/
   50 call likebt(1)                                                            !/*ny         48*/
      if(nwc.ne.1)return                                                        !/*ny         49*/
      do 60 i=1,ndup(1)                                                         !/*ny         50*/
      if(kdup(ldup+mipt+1,i).ne.icn)go to 60                                    !/*nyx         9*/
      kdup(ldup+mipt+1,i)=0                                                     !/*nyx        10*/
      kdup(ldup+mipt+2,i)=0                                                     !/*nyx        11*/
   60 continue                                                                  !/*nyx        12*/
      return                                                                    !/*ny         52*/
!                                                                               !/*ny         53*/
! >>>>>  surface descriptions                                                   !/*ny         54*/
!        m1c=surface type index.                                                !/*ny         55*/
!        m2c=1 if surface type symbol is the second item.                       !/*ny         56*/
   70 if(kitm.ne.0)go to 90                                                     !/*ny         57*/
      m2c=nwc-1                                                                 !/*ny         58*/
      do 80 m1c=1,39                                                            !/*ny4c        9*/
   80 if(ksf(m1c).eq.hitm)return                                                !/*ny         60*/
      if('hex'.eq.hitm)m1c=39                                                   !/*ny4c       10*/
      if('hex'.eq.hitm)return                                                   !/*ny4c       11*/
   90 if(nwc.eq.1.and.iitm.gt.0)jtr(ljtr+mxj)=iitm                              !/*ny4a        2*/
      if(nwc.eq.1.and.iitm.lt.0)ksu(lksu+mxj)=-iitm                             !/*ny4a        3*/
      if(nwc.gt.1)scf(lsc(llsc+mxj)+nwc-m2c-1)=ritm                             !/*ny         62*/
      return                                                                    !/*ny         63*/
!                                                                               !/*ny         64*/
! >>>>>  specification of coordinate transformations for surfaces     tr        !/*ny         65*/
  100 trf(ltrf+1+nwc,mxtr)=ritm                                                 !/*ny         66*/
      if(nwc.lt.4.or.nwc.gt.12)return                                           !/*ny         67*/
      if(icx.eq.1)trf(ltrf+1+nwc,mxtr)=cos(ritm*pie/180.)                       !/*ny4a        4*/
      if(abs(trf(ltrf+1+nwc,mxtr)-anint(trf(ltrf+1+nwc,mxtr))).gt.1e-10)        !/*ny         69*/
     & trf(ltrf+1,mxtr)=icn                                                     !/*ny         70*/
      return                                                                    !/*ny         71*/
!                                                                               !/*ny         72*/
! >>>>>  vectors                                                    vect        !/*ny         73*/
  110 n=(nwc+3)/4                                                               !/*ny         74*/
      if(nwc.eq.4*n-3)read(hitm(2:10),'(bn,i9)')jvc(ljvc+n)                     !/*ny         75*/
      if(nwc.ne.4*n-3)vec(lvec+nwc-4*n+3,n)=ritm                                !/*ny         76*/
      return                                                                    !/*ny         77*/
!                                                                               !/*ny         78*/
! >>>>>  cell importances                                            imp        !/*ny         79*/
!        m1c=flag for importances not all zero or single-valued.                !/*nyxc        3*/
  120 do 130 i=1,mipt                                                           !/*ny         80*/
      if(nqp(i).ne.0)fim(lfim+i,nwc)=ritm                                       !/*ny         81*/
  130 if(ifip(i).eq.0.and.nww(i).eq.0.and.kpt(i).ne.0.and.ritm.gt.0.)           !/*cor4-2    109*/
     & fim(lfim+i,nwc)=1.                                                       !/*ny         83*/
      if(nwc.eq.1.or.fiml(1).eq.zero)fiml(1)=ritm                               !/*nyxc        4*/
      if(ritm.eq.zero.or.ritm.eq.fiml(1))return                                 !/*nyxc        5*/
      m1c=m1c+1                                                                 !/*nyxc        6*/
      if(abs(kf8).eq.3.and.m1c.eq.1)kf8=sign(4,kf8)                             !/*nyxc        7*/
      return                                                                    !/*ny         84*/
!                                                                               !/*ny         85*/
! >>>>>  cell volumes for tallies                                    vol        !/*ny         86*/
  140 if(kitm.ne.0)vol(lvol+nwc)=ritm                                           !/*ny         87*/
      if(kitm.eq.0)novol=1                                                      !/*ny         88*/
      if(kitm.eq.0)nwc=nwc-1                                                    !/*ny         89*/
      return                                                                    !/*ny         90*/
!                                                                               !/*ny         91*/
! >>>>>  surface areas for tallies                                  area        !/*ny         92*/
  150 ara(lara+nwc)=ritm                                                        !/*ny         93*/
      return                                                                    !/*ny         94*/
!                                                                               !/*ny         95*/
! >>>>>  photon weight lower bounds                                  pwt        !/*ny         96*/
  160 gwt(lgwt+nwc)=ritm                                                        !/*ny         97*/
      return                                                                    !/*ny         98*/
!                                                                               !/*ny         99*/
! >>>>>  exponential transform                                       ext        !/*ny        100*/
  170 i=1                                                                       !/*ny        101*/
      if(hitm(1:1).eq.'+'.or.hitm(1:1).eq.'-')i=2                               !/*ny        102*/
      if(hitm(i:i).ne.'s')go to 180                                             !/*ny        103*/
      j=i+1                                                                     !/*ny        104*/
      a=0.                                                                      !/*ny        105*/
      go to 210                                                                 !/*ny        106*/
  180 do 190 j=i,nitm                                                           !/*ny        107*/
  190 if(index('vxyz',hitm(j:j)).ne.0)go to 200                                 !/*ny        108*/
  200 ht=hitm(i:j-1)                                                            !/*ny        109*/
      read(ht,'(bn,e21.0)')a                                                    !/*ny        110*/
      if(a.eq.0.)return                                                         !/*ny        111*/
  210 if(j.eq.nitm+1)m=4                                                        !/*ny        112*/
      if(j.lt.nitm)read(hitm(j+1:j+9),'(bn,i9)')m                               !/*ny        113*/
      if(j.lt.nitm)m=m+4                                                        !/*ny        114*/
      if(j.eq.nitm)m=index('xyz',hitm(j:j))                                     !/*ny        115*/
      a=m+a                                                                     !/*ny        116*/
      if(hitm(1:1).eq.'-')a=-a                                                  !/*ny        117*/
      do 220 i=1,mipt                                                           !/*ny        118*/
  220 if(nqp(i).ne.0)qax(lqax+i,nwc)=a                                          !/*ny        119*/
      return                                                                    !/*ny        120*/
!                                                                               !/*ny        121*/
! >>>>>  forced collisions                                           fcl        !/*ny        122*/
  230 do 240 i=1,mipt                                                           !/*ny        123*/
  240 if(nqp(i).ne.0)for(lfor+i,nwc)=ritm                                       !/*ny        124*/
      return                                                                    !/*ny        125*/
!                                                                               !/*ny        126*/
! >>>>>  weight-window lower bounds                                  wwn        !/*ny        127*/
  250 do 260 i=1,mipt                                                           !/*ny        128*/
  260 if(nqp(i).ne.0)wwf(lwwf+nwc+mxa*(max(1,icn)-1+mww(i)))=ritm               !/*ny        129*/
      return                                                                    !/*ny        130*/
!                                                                               !/*ny        131*/
! >>>>>  weight-window energies                                      wwe        !/*ny        132*/
  270 do 280 i=1,mipt                                                           !/*ny        133*/
  280 if(nqp(i).ne.0)wwe(lwwe+nwc+mww(i))=ritm                                  !/*ny        134*/
      return                                                                    !/*ny        135*/
!                                                                               !/*ny        141*/
! >>>>>  dxtran cell probabilities                                   dxc        !/*ny        142*/
  310 do 320 i=1,mipt                                                           !/*ny        143*/
  320 if(nqp(i).ne.0)dxcp(ldxp+icn,i,nwc)=ritm                                  !/*ny        144*/
      return                                                                    !/*ny        145*/
!                                                                               !/*ny        146*/
! >>>>>  cells where fission is treated like capture                nonu        !/*ny        147*/
  330 lfcl(llfc+nwc)=iitm-1                                                     !/*ny        148*/
      if(iitm.eq.2)lfcl(llfc+nwc)=-2                                            !/*ny        149*/
      return                                                                    !/*ny        150*/
!                                                                               !/*ny        151*/
! >>>>>  source distributions                                      si,ds        !/*ny        152*/
!        m1c=distribution index.                                                !/*ny        153*/
!        m2c=current location in spf.                                           !/*ny        154*/
!        m3c=location of n in kcp.                                              !/*ny        155*/
!        m4c=nwc of last colon.                                                 !/*ny        156*/
!        m5c=flag to indicate reading cell path in <[] format                   !/*nyxa        1*/
!        m6c=nwc of last ( that might start <[] path format                     !/*nyxa        2*/
!        m7c=temporary save of mkcp when m6c flag set                           !/*nyxa        3*/
!        m8c=item  of m6c+1                                                     !/*nyxa        4*/
!        m9c=temporary save of spf entry                                        !/*nyxa        5*/
  340 if(ksd(lksd+20,m1c).eq.0.or.m2c.lt.ksd(lksd+20,m1c))go to 360             !/*ny        157*/
      do 350 iz=m1c+1,msd                                                       !/*ny        158*/
      i=msd+m1c+1-iz                                                            !/*ny        159*/
      ksd(lksd+13,i)=ksd(lksd+13,i)+4                                           !/*ny        160*/
      m=max(ksd(lksd+4,i),ksd(lksd+20,i))                                       !/*ny        161*/
      do 350 j=1,m                                                              !/*ny        162*/
      do 350 k=1,4                                                              !/*ny        163*/
  350 spf(ksd(lksd+13,i)+k,m+1-j)=spf(ksd(lksd+13,i)+k,m-j)                     !/*ny        164*/
      mxxs=mxxs+4                                                               !/*ny        165*/
!        check about <[] source cell format                                     !/*nyxa        6*/
  360 if(m5c.ne.0)go to 343                                                     !/*nyxa        7*/
      if(nwc.eq.1.and.hitm.eq.'(')go to 342                                     !/*nyxa        8*/
      if(m6c.ne.0)go to 341                                                     !/*nyxa        9*/
      if(hitm.ne.'(')go to 349                                                  !/*nyxa       10*/
!        might be <[] format so set temporary flag                              !/*nyxa       11*/
      m6c=nwc                                                                   !/*nyxa       12*/
      m7c=mkcp                                                                  !/*nyxa       13*/
      go to 349                                                                 !/*nyxa       14*/
  341 if(nwc-m6c.eq.1.and.kitm.eq.0)call erprnt(1,1,0,0,0,0,0,0,                !/*nyxa       15*/
     & '44himproper format for cell path on si/ds card.')                       !/*nyxa       16*/
      if(nwc-m6c.eq.1)m8c=iitm                                                  !/*nyxa       17*/
      if(nwc-m6c.eq.1)go to 349                                                 !/*nyxa       18*/
      m6c=0                                                                     !/*nyxa       19*/
      if(hitm.ne.')'.and.hitm.ne.'<')m9c=0                                      !/*nyxa       20*/
      if(hitm.ne.')'.and.hitm.ne.'<')go to 349                                  !/*nyxa       21*/
      mkcp=m7c                                                                  !/*nyxa       22*/
      m4c=0                                                                     !/*nyxa       23*/
      kcp(lkcp+m3c)=kcp(lkcp+m3c)-2                                             !/*nyxa       24*/
      if(m9c.ne.0)spf(ksd(lksd+13,m1c)+1,m2c)=m9c                               !/*nyxa       25*/
      m9c=0                                                                     !/*nyxa       26*/
!        store information for <[] cell path format                             !/*nyxa       27*/
!         1000007 signals beginning of path, 1000008 end, 1000009 is <          !/*nyxa       28*/
!         1000005 is [ , 1000006 is ]                                           !/*nyxa       29*/
  342 mkcp=mkcp+1                                                               !/*nyxa       30*/
      if(nwc.eq.1.and.ksd(lksd+13,m1c).eq.0)ksd(lksd+13,m1c)=lspf+mxxs          !/*nyxa       31*/
      m2c=m2c+1                                                                 !/*nyxa       32*/
      kcp(lkcp+mkcp)=1000007                                                    !/*nyxa       33*/
      m3c=mkcp+2                                                                !/*nyxa       34*/
      kcp(lkcp+m3c-1)=-1                                                        !/*nyxa       35*/
      kcp(lkcp+m3c)=1                                                           !/*nyxa       36*/
      mkcp=mkcp+2                                                               !/*nyxa       37*/
      spf(ksd(lksd+13,m1c)+1,m2c)=-lkcp-m3c                                     !/*nyxa       38*/
      if(nwc.ne.1)mkcp=mkcp+1                                                   !/*nyxa       39*/
      if(nwc.ne.1)kcp(lkcp+mkcp)=m8c                                            !/*nyxa       40*/
      if(nwc.ne.1)mkcp=mkcp+1                                                   !/*nyxa       41*/
      if(nwc.ne.1.and.hitm.eq.'<')kcp(lkcp+mkcp)=1000009                        !/*nyxa       42*/
      m5c=1                                                                     !/*nyxa       43*/
      if(hitm.ne.')')return                                                     !/*nyxa       44*/
      m5c=0                                                                     !/*nyxa       45*/
      mkcp=mkcp+1                                                               !/*nyxa       46*/
      kcp(lkcp+mkcp)=1000008                                                    !/*nyxa       47*/
      return                                                                    !/*nyxa       48*/
  343 mkcp=mkcp+1                                                               !/*nyxa       49*/
      if(kitm.ne.0)kcp(lkcp+mkcp)=iitm                                          !/*nyxa       50*/
      if(kitm.ne.0.and.m5c.ne.1.and.m5c.ne.3)                                   !/*nyxa       51*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*nyxa       52*/
     & '37hformat of lattice universe incorrect.')                              !/*nyxa       53*/
      if(kitm.ne.0.and.m5c.eq.3)mkcp=mkcp+1                                     !/*nyxa       54*/
      if(kitm.ne.0.and.m5c.eq.3)kcp(lkcp+mkcp)=-999                             !/*nyxa       55*/
      if(kitm.ne.0.and.m5c.eq.3)m5c=4                                           !/*nyxa       56*/
      if(kitm.ne.0)return                                                       !/*nyxa       57*/
      if(hitm.eq.'<')kcp(lkcp+mkcp)=1000009                                     !/*nyxa       58*/
      if(hitm.eq.'[')kcp(lkcp+mkcp)=1000005                                     !/*nyxa       59*/
      if(hitm.eq.']')kcp(lkcp+mkcp)=1000006                                     !/*nyxa       60*/
      if(hitm.eq.':')kcp(lkcp+mkcp)=1000003                                     !/*nyxa       61*/
      if(hitm.eq.'u'.or.hitm.eq.'=')mkcp=mkcp-1                                 !/*nyxa       62*/
      if((hitm.eq.'u'.or.hitm.eq.'=').and.kcp(lkcp+mkcp).ne.1000005)            !/*nyxa       63*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*nyxa       64*/
     & '37hformat of lattice universe incorrect.')                              !/*nyxa       65*/
      if(hitm.eq.'u'.or.hitm.eq.'=')m5c=m5c+1                                   !/*nyxa       66*/
      if(m5c.eq.4.and.hitm.ne.']')                                              !/*nyxa       67*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*nyxa       68*/
     & '37hformat of lattice universe incorrect.')                              !/*nyxa       69*/
      if(hitm.eq.'(')call erprnt(1,1,0,0,0,0,0,0,                               !/*nyxa       70*/
     & '47hembedded ( in <[] cell path format not allowed.')                    !/*nyxa       71*/
      if(m5c.eq.4)m5c=1                                                         !/*nyxa       72*/
      if(hitm.ne.')')return                                                     !/*nyxa       73*/
      m5c=0                                                                     !/*nyxa       74*/
      kcp(lkcp+mkcp)=1000008                                                    !/*nyxa       75*/
!        reverse path order and split into multi-paths.                         !/*nyxa       76*/
      call nsourc(2)                                                            !/*nyxa       77*/
      return                                                                    !/*nyxa       78*/
  349 if(hitm(1:1).ne.'d'.or.m4c.ne.0.and.m4c.eq.nwc-1.or.nitm.lt.2)            !/*nyxa       79*/
     & go to 370                                                                !/*ny        167*/
      if(kdata(hitm(2:nitm)).eq.0)go to 370                                     !/*ny        168*/
      if(ksd(lksd+13,m1c).eq.0)ksd(lksd+13,m1c)=lspf+mxxs                       !/*ny        169*/
      m2c=m2c+1                                                                 !/*ny        170*/
      read(hitm(2:4),'(bn,e3.0)')spf(ksd(lksd+13,m1c)+1,m2c)                    !/*ny        171*/
      if(ksd(lksd+6,m1c).eq.0)spf(ksd(lksd+13,m1c)+1,m2c)=                      !/*ny        172*/
     & spf(ksd(lksd+13,m1c)+1,m2c)+100000                                       !/*ny        173*/
      return                                                                    !/*ny        174*/
  370 if(kitm.ne.0.or.hitm(1:1).eq.'d'.or.hitm.eq.':'.or.                       !/*ny        175*/
     & hitm.eq.'('.or.hitm.eq.')')go to 380                                     !/*ny        176*/
      nwc=nwc-1                                                                 !/*ny        177*/
      if(index('lsfqt',hitm(1:1)).ne.0)ksd(lksd+5,m1c)=1                        !/*ny        178*/
      if(index('sq',hitm(1:1)).ne.0)ksd(lksd+6,m1c)=1                           !/*ny        179*/
      if(hitm.eq.'q')ksd(lksd+8,m1c)=1                                          !/*ny        180*/
      if(hitm.eq.'t')ksd(lksd+9,m1c)=1                                          !/*ny        181*/
      if(hitm.eq.'f')ksd(lksd+11,m1c)=1                                         !/*ny        182*/
      if(hitm.eq.'a')ksd(lksd+19,m1c)=1                                         !/*ny        183*/
      return                                                                    !/*ny        184*/
  380 if(ksd(lksd+13,m1c).eq.0)ksd(lksd+13,m1c)=lspf+mxxs                       !/*ny        185*/
      if((m4c.eq.0.or.m4c.lt.nwc-1).and.hitm.ne.':'.and.                        !/*ny        186*/
     & hitm.ne.'('.and.hitm.ne.')')go to 400                                    !/*ny        187*/
      if(hitm.ne.':'.and.hitm.ne.'('.or.m4c.eq.nwc-2.and.m4c.ne.0)              !/*ny        188*/
     & go to 390                                                                !/*ny        189*/
      m3c=mkcp+2                                                                !/*ny        190*/
      kcp(lkcp+m3c-1)=-1                                                        !/*ny        191*/
      kcp(lkcp+m3c)=1                                                           !/*ny        192*/
      mkcp=mkcp+3                                                               !/*ny        193*/
      kcp(lkcp+mkcp)=spf(ksd(lksd+13,m1c)+1,m2c)                                !/*ny        194*/
      if(m6c.ne.0)m9c=spf(ksd(lksd+13,m1c)+1,m2c)                               !/*nyxa       80*/
      spf(ksd(lksd+13,m1c)+1,m2c)=-lkcp-m3c-1                                   !/*ny        195*/
  390 if(hitm.eq.':')m4c=nwc                                                    !/*ny        196*/
      if(hitm.eq.':')return                                                     !/*ny        197*/
      if(hitm.eq.'(')m4c=nwc+3                                                  !/*ny        198*/
      kcp(lkcp+m3c)=kcp(lkcp+m3c)+1                                             !/*ny        199*/
      mkcp=mkcp+1                                                               !/*ny        200*/
      kcp(lkcp+mkcp)=iitm                                                       !/*ny        201*/
      if(hitm.eq.'(')kcp(lkcp+mkcp)=1000001                                     !/*ny        202*/
      if(hitm.eq.')')kcp(lkcp+mkcp)=1000002                                     !/*ny        203*/
      if(hitm(1:1).ne.'d')return                                                !/*ny        204*/
      read(hitm(2:4),'(bn,i3)')kcp(lkcp+mkcp)                                   !/*ny        205*/
      kcp(lkcp+mkcp)=kcp(lkcp+mkcp)+100000                                      !/*ny        206*/
      return                                                                    !/*ny        207*/
  400 m2c=m2c+1                                                                 !/*ny        208*/
      spf(ksd(lksd+13,m1c)+1,m2c)=ritm                                          !/*ny        209*/
      return                                                                    !/*ny        210*/
!                                                                               !/*ny        211*/
! >>>>>  source distributions                                      sp,sb        !/*ny        212*/
!        m1c=distribution index.                                                !/*ny        213*/
!        m2c=flag for c.                                                        !/*ny        214*/
!        m3c=flag for function.                                                 !/*ny        215*/
  410 i=index('pb',ich(2:2))+1                                                  !/*ny        216*/
      if(kitm.ne.0)go to 420                                                    !/*ny        217*/
      nwc=nwc-1                                                                 !/*ny        218*/
      if(hitm.eq.'c')m2c=1                                                      !/*ny        219*/
      if(hitm.eq.'c'.and.ksd(lksd+19,m1c).eq.0)ksd(lksd+19,m1c)=-1              !/*ny        220*/
      if(hitm.eq.'v')ksd(lksd+10,m1c)=1                                         !/*ny        221*/
      return                                                                    !/*ny        222*/
  420 if(nwc.eq.1.and.iitm.lt.0)ksd(lksd+2,m1c)=iitm                            !/*ny        223*/
      if(nwc.eq.1.and.iitm.lt.0)m3c=1                                           !/*ny        224*/
      if(m3c.ne.0)sqq(lsqq+nwc+3*i-6,m1c)=ritm                                  !/*ny        225*/
      if(nwc.eq.1.and.iitm.eq.-21)sqq(lsqq+3*i-4,m1c)=12345.                    !/*ny        226*/
      if(m3c.ne.0)return                                                        !/*ny        227*/
      if(ksd(lksd+13,m1c).eq.0)ksd(lksd+13,m1c)=lspf+mxxs                       !/*ny        228*/
      spf(ksd(lksd+13,m1c)+i,nwc)=ritm                                          !/*ny        229*/
      return                                                                    !/*ny        230*/
!                                                                               !/*ny        231*/
! >>>>>  source comment                                               sc        !/*ny        232*/
!        m1c=distribution index.                                                !/*ny        233*/
  430 call nxtsym(klin,' ',1,it,iu,0)                                           !/*ny4a        5*/
      call nxtsym(klin,' ',iu+1,it,iu,0)                                        !/*ny4a        6*/
      ix=min(6,max(5,it))                                                       !/*ny4a        7*/
      ht=klin(ix:ix+74)                                                         !/*ny4a        8*/
      do 440 i=1,75,3                                                           !/*ny        235*/
  440 jscn(mssc+(i+2)/3)=ichar(ht(i:i))*65536+ichar(ht(i+1:i+1))*256+           !/*ny        236*/
     & ichar(ht(i+2:i+2))                                                       !/*ny        237*/
      ksd(lksd+3,m1c)=ksd(lksd+3,m1c)+25                                        !/*ny        238*/
      mssc=mssc+25                                                              !/*ny        239*/
      return                                                                    !/*ny        240*/
!                                                                               !/*ny        241*/
! >>>>>  source definition                                          sdef        !/*ny        242*/
!        m1c=nwc of variable name                                               !/*ny        243*/
!        m2c=index of current variable                                          !/*ny        244*/
!        m3c=index of depended-on variable or location of n in kcp.             !/*ny        245*/
!        m4c=nwc of last colon.                                                 !/*ny        246*/
  450 if(hitm.eq.'=')nwc=nwc-1                                                  !/*ny        247*/
      if(hitm.eq.'=')return                                                     !/*ny        248*/
      if((hitm.eq.':'.or.hitm.eq.'(').and.m1c.eq.nwc)m1c=m1c-2                  !/*ny        249*/
      if(m4c.ne.0.and.m4c.eq.nwc-2.and.hitm.ne.':'.and.hitm.ne.'(')             !/*ny        250*/
     & m1c=nwc                                                                  !/*ny        251*/
      go to(460,480,510,520)min(4,nwc-m1c+1)                                    !/*ny        252*/
  460 m3c=0                                                                     !/*ny4a        9*/
      do 470 m2c=1,maxv                                                         !/*ny4a       10*/
  470 if(hitm(1:3).eq.hbln(m2c,1))return                                        !/*ny4c       12*/
  480 if(kitm.ne.0)go to 520                                                    !/*ny        255*/
      if(m2c.eq.19)then                                                         !/*nyx        13*/
         srv(1,19)=index(hip,hitm(1:1))                                         !/*nyx        14*/
         m1c=nwc+1                                                              !/*nyx        15*/
         if(hitm(1:2).ne.'sf')return                                            !/*nyxk        1*/
         isfiss=1                                                               !/*nyxk        2*/
         srv(1,19)=1                                                            !/*nyxk        3*/
         return                                                                 !/*nyx        16*/
      endif                                                                     !/*nyx        17*/
      do 490 m3c=1,maxv                                                         !/*nyx        18*/
  490 if(hitm(1:4).eq.'f'//hbln(m3c,1))go to 500                                !/*ny4c       13*/
      go to 550                                                                 !/*ny        258*/
  500 ivdd(m2c)=m3c                                                             !/*ny        259*/
      return                                                                    !/*ny        260*/
  510 if(m3c.ne.0.and.hitm.ne.':'.and.hitm.ne.'(')go to 550                     !/*ny        261*/
  520 if(m4c.lt.nwc-1.and.hitm.ne.':'.and.hitm.ne.'(')go to 540                 !/*ny        262*/
      if(hitm.eq.':')m4c=nwc                                                    !/*ny        263*/
      if(nwc.ne.m1c+2)go to 530                                                 !/*ny        264*/
      m3c=mkcp+2                                                                !/*ny        265*/
      kcp(lkcp+m3c-1)=-1                                                        !/*ny        266*/
      kcp(lkcp+m3c)=1                                                           !/*ny        267*/
      mkcp=mkcp+3                                                               !/*ny        268*/
      kcp(lkcp+mkcp)=srv(1,m2c)                                                 !/*ny        269*/
      srv(1,m2c)=-lkcp-m3c-1                                                    !/*ny        270*/
      if(ivdis(1).gt.100000)kcp(lkcp+mkcp)=ivdis(1)                             !/*ny4a       11*/
  530 if(hitm.eq.':')return                                                     !/*ny        273*/
      if(hitm.eq.'(')m4c=nwc+3                                                  !/*ny        274*/
      kcp(lkcp+m3c)=kcp(lkcp+m3c)+1                                             !/*ny        275*/
      mkcp=mkcp+1                                                               !/*ny        276*/
      kcp(lkcp+mkcp)=iitm                                                       !/*ny        277*/
      if(hitm.eq.'(')kcp(lkcp+mkcp)=1000001                                     !/*ny        278*/
      if(hitm.eq.')')kcp(lkcp+mkcp)=1000002                                     !/*ny        279*/
      if(hitm(1:1).ne.'d')return                                                !/*ny        280*/
      read(hitm(2:4),'(bn,i3)')kcp(lkcp+mkcp)                                   !/*ny        281*/
      kcp(lkcp+mkcp)=kcp(lkcp+mkcp)+100000                                      !/*ny        282*/
      return                                                                    !/*ny        283*/
  540 srv(nwc-m1c,m2c)=ritm                                                     !/*ny        284*/
      if(nwc-m1c.lt.nvs(m2c))return                                             !/*ny        285*/
  550 m1c=nwc+1                                                                 !/*ny        286*/
      return                                                                    !/*ny        288*/
!                                                                               !/*ny        289*/
! >>>>>  tally comment                                                fc        !/*ny        290*/
  560 call nxtsym(klin,' ',1,it,iu,0)                                           !/*ny4a       12*/
      call nxtsym(klin,' ',iu+1,it,iu,0)                                        !/*ny4a       13*/
      ix=min(6,max(5,it))                                                       !/*ny4a       14*/
      ht=klin(ix:ix+74)                                                         !/*ny4a       15*/
      do 570 i=1,75,3                                                           !/*ny        292*/
  570 rtp(lrtp+ipl+(nwc-1)*25+(i+2)/3)=ichar(ht(i:i))*65536+                    !/*ny        293*/
     & ichar(ht(i+1:i+1))*256+ichar(ht(i+2:i+2))                                !/*ny        294*/
      return                                                                    !/*ny        295*/
!                                                                               !/*ny        296*/
! >>>>>  order of tally printing                                      fq        !/*ny        297*/
  580 k=index('fdusmcet',hitm(1:1))                                             !/*ny        298*/
      do 590 i=1,7                                                              !/*ny        299*/
      if(jfq(ljfq+i,ital).eq.k)jfq(ljfq+i,ital)=jfq(ljfq+i+1,ital)              !/*ny        300*/
  590 if(jfq(ljfq+i+1,ital).eq.jfq(ljfq+i,ital))jfq(ljfq+i+1,ital)=k            !/*ny        301*/
      jfq(ljfq+8,ital)=-k                                                       !/*ny        302*/
      return                                                                    !/*ny        303*/
!                                                                               !/*ny        304*/
! >>>>>  tally fluctuation chart bins                                 tf        !/*ny        305*/
  600 jtf(ljtf+nwc,ital)=iitm                                                   !/*ny        306*/
      return                                                                    !/*ny        307*/
!                                                                               !/*ny        308*/
! >>>>>  other tally cards            pd,f,fx,fy,fz,pi,tir,tic,ft,e,t,c,        !/*nyx        19*/
!                                    fm,de,df,em,tm,cm,cf,sf,fs,sd,fu,dd        !/*nyx        20*/
!        m2c=nwc of last ft-card parameter count.                               !/*ny        311*/
!        special handling for dxtran dd card.                                   !/*ny        312*/
  610 continue                                                                  !/*nyx        21*/
#ifndef RADIOG                                                                  /*!nyx        24*/
      if(ich.eq.'pi'.or.ich.eq.'tir'.or.ich.eq.'tic')return                     !/*nyx        25*/
      if(ich.eq.'c'.and.mod(icn,10).eq.5)return                                 !/*nyx        26*/
      if(ich.eq.'fs'.and.mod(icn,10).eq.5)return                                !/*nyx        27*/
#endif                                                                          /*!nyx        28*/
!        TEMPORARY RESTRICTION to shorthand dd1 and dd2                         !/*nyx        29*/
      if(ich.ne.'dd'.or.icn.gt.2)go to 640                                      !/*nyx        30*/
      i=2-mod(nwc,2)                                                            !/*ny        314*/
      j=(nwc+1)/2                                                               !/*ny        315*/
      if(icn.eq.0)go to 620                                                     !/*ny        316*/
      ddx(icn,i,j)=ritm                                                         !/*ny        317*/
      return                                                                    !/*ny        318*/
  620 do 630 k=1,mipt                                                           !/*ny        319*/
  630 if(ddx(k,i,j).eq.huge)ddx(k,i,j)=ritm                                     !/*ny        320*/
!                                                                               !/*ny        321*/
  640 if(kitm.eq.0)go to 650                                                    !/*ny        322*/
      rtp(lrtp+ipl+nwc)=ritm                                                    !/*ny        323*/
      if(ich.eq.'c'.and.icx.eq.1)rtp(lrtp+ipl+nwc)=cos(ritm*pie/180.)           !/*nyxa       81*/
      return                                                                    !/*ny        324*/
  650 if(ich.eq.'ft')go to 670                                                  !/*ny        325*/
      n=index('ntc',hitm(1:1))                                                  !/*ny4c       14*/
      if(n.eq.0)go to 660                                                       !/*ny        327*/
      i=(index('fu fs fm c  e  t  ',ich(1:3))+5)/3                              !/*ny        328*/
      if(i.eq.1.and.mod(icn,10).ne.5)i=0                                        !/*ny        329*/
      k=2**i                                                                    !/*ny        330*/
      l=mod(ipnt(lipn+1,1,ital)/k,2)                                            !/*ny        331*/
      if(n.eq.2.and.l.eq.0)ipnt(lipn+1,1,ital)=ipnt(lipn+1,1,ital)+k            !/*ny4c       15*/
      if(n.ne.2.and.l.eq.1)ipnt(lipn+1,1,ital)=ipnt(lipn+1,1,ital)-k            !/*ny4c       16*/
      if(n.eq.3.and.mod(ipnt(lipn+1,1,ital)/(64*k),2).eq.0)                     !/*ny4c       17*/
     & ipnt(lipn+1,1,ital)=ipnt(lipn+1,1,ital)+64*k                             !/*ny4c       18*/
      nwc=nwc-1                                                                 !/*ny        333*/
      return                                                                    !/*ny        334*/
  660 if(ich.eq.'de'.or.ich.eq.'df')then                                        !/*nyxc       12*/
         rtp(lrtp+ipl+nwc)=-index('liloiciufaac',hitm(1:2))                     !/*nyxc       13*/
         if(hitm(1:1).eq.'=')nwc=nwc-1                                          !/*nyxc       14*/
         return                                                                 !/*nyxc       15*/
      endif                                                                     !/*nyxc       16*/
      ks=index('():#<[],u=',hitm(1:1))                                          !/*nyxc       17*/
      if(ks.ne.0)rtp(lrtp+ipl+nwc)=1000000+ks                                   !/*nyxc       18*/
      return                                                                    !/*ny        339*/
  670 if(m2c.eq.0)go to 675                                                     !/*ny4a       18*/
      if(nint(rtp(lrtp+ipl+m2c-1)).ne.2.or.nwc-m2c.ne.3)go to 673               !/*ny4a       19*/
      rtp(lrtp+ipl+nwc)=0.                                                      !/*ny4a       20*/
      nwc=nwc+1                                                                 !/*ny4a       21*/
  673 rtp(lrtp+ipl+m2c)=nwc-m2c-1                                               !/*ny4a       22*/
  675 do 680 i=1,mkft                                                           !/*ny4a       23*/
  680 if(hitm.eq.hft(i))rtp(lrtp+ipl+nwc)=i                                     !/*ny        341*/
      nwc=nwc+1                                                                 !/*ny        343*/
      m2c=nwc                                                                   !/*ny        344*/
      return                                                                    !/*ny        345*/
!                                                                               !/*ny        346*/
! >>>>>  dxtran parameters                                           dxt        !/*ny        347*/
  690 j=mod(nwc-1,5)+1                                                          !/*ny        348*/
      k=(nwc+4)/5                                                               !/*ny        349*/
      do 700 i=1,mipt                                                           !/*ny        350*/
      if(nqp(i).eq.0)go to 700                                                  !/*ny        351*/
      if(k.le.mxdx)dxx(i,j,k)=ritm                                              !/*ny        352*/
      if(k.le.mxdx.and.j.ge.4)dxx(i,j,k)=(ritm*1.00001)**2                      !/*ny        353*/
      if(j.le.3)dxw(i,j)=ritm                                                   !/*ny        354*/
      if(j.ne.4)go to 700                                                       !/*ny        355*/
      dxw(i,1)=0.                                                               !/*ny        356*/
      dxw(i,2)=0.                                                               !/*ny        357*/
      dxw(i,3)=0.                                                               !/*ny        358*/
  700 continue                                                                  !/*ny        359*/
      return                                                                    !/*ny        360*/
!                                                                               !/*ny        361*/
! >>>>>  material specifications                                       m        !/*ny        362*/
!        m1c > 0, already processing material option m1c.                       !/*ny4c2       2*/
!        check for material option.                                             !/*ny4c2       3*/
  710 if(m1c.ne.0)go to 720                                                     !/*ny4a       25*/
      do 715 i=1,mopts                                                          !/*ny4a       26*/
  715 if(hitm.eq.hmopt(i))m1c=i                                                 !/*ny4c2       4*/
      if(m1c.eq.0)go to 730                                                     !/*ny4c2       5*/
      nwc=nwc-1                                                                 !/*ny4a       30*/
      return                                                                    !/*ny4a       31*/
!        process material option m1c: gas, estep, cond.                         !/*ny4c2       6*/
  720 nwc=nwc-1                                                                 !/*ny4a       32*/
      if(hitm.eq.'=')return                                                     !/*ny4a       33*/
      if(m1c.eq.1.and.nee_max.gt.0)jemi(ljem+nmat)=iitm                         !/*nyx        31*/
      if(m1c.eq.2.and.nee_max.gt.0)nsb(lnsb+nmat)=iitm                          !/*nyx        32*/
      if(m1c.eq.3.and.nee_max.gt.0)jcond(ljco+nmat)=iitm                        !/*nyx        33*/
      if(m1c.le.3)go to 728                                                     !/*ny4c2       8*/
!        process identifier value m1c: nlib, plib, elib, pnlib, hlib.           !/*nyx        34*/
      i=index(hitm,'.')                                                         !/*ny4a       38*/
      ht=hitm(i+1:i+3)                                                          !/*ny4a       39*/
      if(ht(1:1).eq.' ')ht(1:1)='0'                                             !/*ny4a       40*/
      if(ht(2:2).eq.' ')ht(2:2)='0'                                             !/*ny4a       41*/
!        content-free entry does not change system default.                     !/*ny4a       42*/
      if(ht(1:3).eq.'00 '.or.ht(1:3).eq.'000')return                            !/*ny4a       43*/
      if(ht(3:3).ne.' ')go to 726                                               !/*ny4a       44*/
      if(m1c.eq.4)ht(3:3)='c'                                                   !/*ny4c2      10*/
      if(m1c.eq.5)ht(3:3)='p'                                                   !/*ny4c2      11*/
      if(m1c.eq.6)ht(3:3)='e'                                                   !/*ny4c2      12*/
      if(m1c.eq.7)ht(3:3)='u'                                                   !/*ny4c2      13*/
      if(m1c.eq.12)ht(3:3)='h'                                                  !/*nyx        35*/
  726 lxd(llxd+m1c-3,nmat)=ichar(ht(1:1))+256*(ichar(ht(2:2))+                  !/*ny4c2      14*/
     & 256*ichar(ht(3:3)))                                                      !/*ny4a       49*/
  728 m1c=0                                                                     !/*ny4a       50*/
      return                                                                    !/*ny4a       51*/
!        process nuclide zaid.                                                  !/*ny4c2      15*/
  730 if(mod(nwc,2).eq.0)go to 740                                              !/*ny        376*/
      mix=mix+1                                                                 !/*ny        377*/
      ht=' '                                                                    !/*ny        378*/
      if(index(hitm,'.').eq.0)hitm(nitm+1:nitm+1)='.'                           !/*ny        379*/
      ht(8-index(hitm,'.'):10)=hitm                                             !/*ny        380*/
      if(ht(8:8).ne.' '.and.ht(9:9).eq.' ')ht(9:9)='0'                          !/*ny        381*/
      if(ht(8:10).eq.'00 '.or.ht(8:10).eq.'000')ht(8:10)='   '                  !/*nyx        36*/
      read(ht(1:6),'(i6)')iza(liza+mix)                                         !/*ny        383*/
      if(ispn.ne.0)izn(lizn+mix)=iza(liza+mix)                                  !/*ny4c2      16*/
      kmm(lkmm+mix)=ichar(ht(8:8))+256*(ichar(ht(9:9))+                         !/*ny        384*/
     & 256*ichar(ht(10:10)))                                                    !/*ny        385*/
      return                                                                    !/*ny        386*/
!        process nuclide fraction.                                              !/*ny4c2      17*/
  740 fme(lfme+mix)=ritm                                                        !/*ny        387*/
      if(ritm.eq.0.)mix=mix-1                                                   !/*ny        388*/
      return                                                                    !/*ny        389*/
!                                                                               !/*ny        390*/
! >>>>>  nuclides for discrete treatment                            drxs        !/*ny        391*/
  750 ht=' '                                                                    !/*ny        392*/
      if(index(hitm,'.').eq.0)hitm(nitm+1:nitm+1)='.'                           !/*ny        393*/
      ht(8-index(hitm,'.'):10)=hitm                                             !/*ny        394*/
      read(ht(1:6),'(i6)')kdr(lkdr+nwc)                                         !/*ny        395*/
      return                                                                    !/*ny        396*/
!                                                                               !/*ny        397*/
! >>>>>  total or prompt nubar                                     totnu        !/*ny        398*/
  760 itotnu=2                                                                  !/*ny        399*/
      return                                                                    !/*ny        400*/
!                                                                               !/*ny        401*/
! >>>>>  atomic weights                                            awtab        !/*ny        402*/
  770 if(mod(nwc,2).eq.1)kaw(lkaw+(nwc+1)/2)=iitm                               !/*ny        403*/
      if(mod(nwc,2).eq.0)awt(lawt+nwc/2)=ritm                                   !/*ny        404*/
      return                                                                    !/*ny        405*/
!                                                                               !/*ny        406*/
! >>>>>  cross-section directory information                          xs        !/*ny        407*/
!        m1c=last character position used so far in ixc.                        !/*ny        408*/
  780 if(nwc.ne.1)go to 790                                                     !/*ny        409*/
      nxsc=nxsc+1                                                               !/*ny        410*/
      ixc(lixc+61,nxsc)=icn                                                     !/*ny        411*/
      ht=' '                                                                    !/*ny        412*/
      ht(8-index(hitm,'.'):10)=hitm                                             !/*ny        413*/
      call zaid(1,ht,ixc(lixc+1,nxsc))                                          !/*ny        414*/
      m1c=10                                                                    !/*ny        415*/
      return                                                                    !/*ny        416*/
  790 do 800 i=1,nitm+1                                                         !/*ny        417*/
      l=lixc+(m1c+i+2)/3                                                        !/*ny        418*/
      k=256**mod(m1c+i-1,3)                                                     !/*ny        419*/
  800 ixc(l,nxsc)=ixc(l,nxsc)-ichar(' ')*k+ichar(hitm(i:i))*k                   !/*ny4a       53*/
      m1c=m1c+nitm+1                                                            !/*ny        421*/
      return                                                                    !/*ny        422*/
!                                                                               !/*ny        423*/
! >>>>>  void cells                                                 void        !/*ny        424*/
  810 ioid=0                                                                    !/*ny        425*/
      i=namchg(1,iitm)                                                          !/*ny        426*/
      rho(lrho+i)=0.                                                            !/*ny        427*/
      mat(lmat+i)=0                                                             !/*ny        428*/
      return                                                                    !/*ny        429*/
!                                                                               !/*ny        430*/
! >>>>>  physics parameters                                         phys        !/*ny        431*/
  820 if(nqp(1).eq.0)go to 830                                                  !/*ny        432*/
      if(nwc.eq.2)emcf(1)=ritm                                                  !/*ny        434*/
      if(nwc.eq.3.and.iitm.ne.0)iunr=100000                                     !/*ny4c       22*/
      if(nwc.eq.4.and.nsr.ne.71.and.ritm.ge.0.)dnb=ritm                         !/*ny4c       23*/
      if(nwc.eq.4.and.nsr.eq.71.and.ritm.eq.0.)dnb=ritm                         !/*ny4c       24*/
      if(nwc.eq.3.and.ritm.gt.10.)cut_n=ritm                                    !/*nyx        37*/
      if(nwc.eq.5)cut_n=ritm                                                    !/*nyx        38*/
      if(nwc.eq.7)rcoilf(1)=ritm                                                !/*nyxk        8*/
      return                                                                    !/*ny        435*/
  830 if(nqp(2).eq.0)go to 840                                                  !/*ny        436*/
      if(nwc.eq.1)emcf(2)=max(ritm,ecut_min(2))                                 !/*nyx        39*/
      if(nwc.eq.3.and.iitm.ne.0)nocoh=1                                         !/*ny        438*/
      return                                                                    !/*ny        439*/
  840 if(nqp(3).eq.0)go to 845                                                  !/*nyx        40*/
      if(nwc.eq.3)iphot=iitm                                                    !/*ny        441*/
      if(nwc.eq.4)ibad=iitm                                                     !/*ny        442*/
      if(nwc.eq.6.and.abs(1.-ritm).gt..00001)bnum=ritm                          !/*ny4c       25*/
      if(nwc.eq.6.and.bnum.ne.0.)enum=1./abs(bnum)                              !/*ny4c       26*/
      if(nwc.eq.10.and.ritm.ne.0.)numb=1                                        !/*ny4c       27*/
      if(ritm.lt.0..or.abs(1.-ritm).lt..00001)return                            !/*ny        443*/
      if(nwc.eq.7)xnum=max(zero,ritm)                                           !/*ny        445*/
      if(nwc.eq.8)rnok=max(zero,ritm)                                           !/*ny        446*/
      if(nwc.eq.9)enum=max(zero,ritm)                                           !/*ny        447*/
      return                                                                    !/*ny        448*/
  845 if(nqp(9).eq.0)return                                                     !/*nyx        41*/
      if(nwc.eq.3)cut_h=ritm                                                    !/*nyx        42*/
      if(nwc.eq.7)rcoilf(2)=ritm                                                !/*nyxk        9*/
      return                                                                    !/*nyx        43*/
!                                                                               !/*ny        449*/
! >>>>>  energy splitting                                          esplt        !/*ny        450*/
  850 do 860 i=1,mipt                                                           !/*ny        451*/
  860 if(nqp(i).ne.0)espl(i,nwc)=ritm                                           !/*ny        452*/
      return                                                                    !/*ny        453*/
!                                                                               !/*ny        454*/
! >>>>>  thermal temperatures                                        tmp        !/*ny        455*/
  870 tmp(ltmp+max(1,icn)+(nwc-1)*mxt)=ritm                                     !/*ny        456*/
      return                                                                    !/*ny        457*/
!                                                                               !/*ny        458*/
! >>>>>  thermal times                                             thtme        !/*ny        459*/
  880 tth(ltth+nwc)=ritm                                                        !/*ny        460*/
      return                                                                    !/*ny        461*/
!                                                                               !/*ny        462*/
! >>>>>  thermal s(a,b) data specifications                           mt        !/*ny        463*/
  890 indt=indt+1                                                               !/*ny        464*/
      jmt(ljmt+indt)=icn                                                        !/*ny        465*/
      ht=' '                                                                    !/*ny        466*/
      if(index(hitm,'.').eq.0)hitm(nitm+1:nitm+1)='.'                           !/*ny        467*/
      ht(8-index(hitm,'.'):10)=hitm                                             !/*ny        468*/
      if(ht(8:8).ne.' '.and.ht(9:9).eq.' ')ht(9:9)='0'                          !/*ny        469*/
      call zaid(1,ht,kmt(lkmt+1,indt))                                          !/*ny        470*/
      return                                                                    !/*ny        471*/
!                                                                               !/*ny        472*/
! >>>>>  cutoffs                                                     cut        !/*ny        473*/
  900 do 910 i=1,mipt                                                           !/*ny        474*/
      if(nqp(i).eq.0)go to 910                                                  !/*ny        475*/
      if(nwc.eq.1.and.ritm.ne.0.)tco(i)=ritm                                    !/*ny        476*/
      if(nwc.eq.3)wc1(i)=ritm                                                   !/*ny        478*/
      if(nwc.eq.3)wc2(i)=.5*wc1(i)                                              !/*ny        479*/
      if(nwc.eq.4)wc2(i)=sign(ritm,wc1(i))                                      !/*ny        480*/
  910 continue                                                                  !/*ny        481*/
      if(nwc.eq.5)swtm=ritm                                                     !/*ny        482*/
      if(nwc.eq.5.and.ritm.eq.0.)swtm=-1.                                       !/*ny        483*/
      if(nqp(1).eq.0.or.nwc.ne.1.or.ritm.eq.0.)return                           !/*ny        484*/
      do 920 i=2,mipt                                                           !/*ny        485*/
  920 if(tco(i).eq..001*huge)tco(i)=tco(1)                                      !/*ny        486*/
      return                                                                    !/*ny        487*/
!                                                                               !/*ny        488*/
! >>>>>  source particle cutoff number                               nps        !/*ny        489*/
  930 if(nwc.eq.1)npp=iitm                                                      !/*nyx        44*/
#ifdef RADIOG                                                                   /*!nyx        45*/
      if(nwc.eq.2.and.(konrun.eq.0.or.iitm.ne.0))npsmg=iitm                     !/*nyx        46*/
      if(konrun.ne.0.and.nwc.eq.2.and.iitm.gt.nps)                              !/*nyx        47*/
     & call erprnt(2,2,1,npsmg-nps,0,0,0,0,                                     !/*nyx        48*/
     & '7hAdding ,i5,35h more source contributions to mesh.')                   !/*nyx        49*/
#endif                                                                          /*!nyx        50*/
      return                                                                    !/*ny        491*/
!                                                                               !/*ny        492*/
! >>>>>  computer time cutoff                                       ctme        !/*ny        493*/
  940 ctme=ritm                                                                 !/*ny        494*/
      if(mct.lt.0)return                                                        !/*nyx        51*/
      if(ltasks.gt.1)call erprnt(1,1,0,0,0,0,0,0,                               !/*ny4c2      18*/
     & '33hpvm multitasking fails with ctme.')                                  !/*ny4c2      19*/
      return                                                                    !/*ny        495*/
!                                                                               !/*ny        496*/
! >>>>>  integer quantities for temporary code features             idum        !/*ny        497*/
  950 idum(nwc)=iitm                                                            !/*ny        498*/
      return                                                                    !/*ny        499*/
!                                                                               !/*ny        500*/
! >>>>>  real quantities for temporary code features                rdum        !/*ny        501*/
  960 rdum(nwc)=ritm                                                            !/*ny        502*/
      return                                                                    !/*ny        503*/
!                                                                               !/*ny        504*/
! >>>>>  print and dump controls                                   prdmp        !/*ny        505*/
  970 if(nwc.eq.1)prn=ritm                                                      !/*ny        506*/
      if(nwc.eq.2)dmp=ritm                                                      !/*ny        507*/
      if(nwc.eq.3)mct=iitm                                                      !/*ny        508*/
      if(nwc.eq.4)ndmp=iitm                                                     !/*ny        509*/
      if(nwc.eq.5.and.iitm.gt.0)npd=iitm                                        !/*ny4a       54*/
#ifdef MULTP                                                                    /*!ny4a       55*/
      if(nwc.eq.5)inpd=iitm                                                     !/*ny4a       56*/
#endif                                                                          /*!cpp       111*/
      return                                                                    !/*ny        510*/
!                                                                               !/*ny        511*/
! >>>>>  termination and print control for lost particles           lost        !/*ny        512*/
  980 lost(nwc)=iitm                                                            !/*ny        513*/
      return                                                                    !/*ny        514*/
!                                                                               !/*ny        515*/
! >>>>>  debugging controls                                         dbcn        !/*ny        516*/
  990 if(ritm.gt.0..or.nwc.ge.15)dbcn(nwc)=ritm                                 !/*ny4a       57*/
      if(ntasks.gt.1.and.abs(nwc-3).le.1)dbcn(nwc)=0.                           !/*ny4c       28*/
      return                                                                    !/*ny        518*/
!                                                                               !/*ny        519*/
! >>>>>  specifications for user files                             files        !/*ny        520*/
 1000 j=(nwc+4)/5                                                               !/*ny        521*/
      n=nwc-5*(j-1)                                                             !/*ny        522*/
      if(n.eq.1)kufil(1,j)=iitm                                                 !/*ny        523*/
      if(n.eq.2)ufil(1,j)=hitm                                                  !/*ny        524*/
      if(n.eq.3)ufil(2,j)=hsd(index('sd',hitm(1:1)))                            !/*ny        525*/
      if(n.eq.4)ufil(3,j)=hfu(index('fu',hitm(1:1)))                            !/*ny        526*/
      if(n.eq.5)kufil(2,j)=iitm                                                 !/*ny        527*/
      return                                                                    !/*ny        528*/
!                                                                               !/*ny        529*/
! >>>>>  print control                                             print        !/*ny        530*/
 1010 if(mnk.eq.1)return                                                        !/*ny        531*/
      if(iitm.lt.0)go to 1020                                                   !/*ny        532*/
      ink(iitm)=1                                                               !/*ny        533*/
      return                                                                    !/*ny        534*/
 1020 if(mnk.ne.0)go to 1040                                                    !/*ny        535*/
      mnk=-1                                                                    !/*ny        536*/
      do 1030 i=1,mink                                                          !/*ny        537*/
 1030 ink(i)=1                                                                  !/*ny        538*/
 1040 ink(-iitm)=0                                                              !/*ny        539*/
      return                                                                    !/*ny        540*/
!                                                                               !/*ny        541*/
! >>>>>  kcode specifications                                      kcode        !/*ny        542*/
 1050 if(nwc.eq.4)kct=iitm                                                      !/*ny4c       29*/
      if(nwc.eq.6.and.ritm.ne.0.)knrm=1                                         !/*ny        544*/
      if(konrun.ne.0)return                                                     !/*ny4c       30*/
      if(nwc.eq.2.and.ritm.gt.0.)rkk=ritm                                       !/*ny4c       31*/
      if(nwc.eq.3)ikz=ritm                                                      !/*ny4c       32*/
      if(nwc.eq.8.and.iitm.eq.0)kc8=0                                           !/*ny4c       33*/
      return                                                                    !/*ny        547*/
!                                                                               !/*ny        548*/
! >>>>>  locations of kcode source points                           ksrc        !/*ny        549*/
 1060 fso(lfso+nwc+3*((nwc-1)/3))=ritm                                          !/*ny        550*/
      return                                                                    !/*ny        551*/
!                                                                               !/*ny        556*/
! >>>>>  energy bins for weight-window generator                    wwge        !/*ny        557*/
 1080 do 1090 i=1,mipt                                                          !/*ny        558*/
 1090 if(nqp(i).ne.0)ewwg(lewg+nwc+mgww(i))=ritm                                !/*ny        559*/
      return                                                                    !/*ny        560*/
!                                                                               !/*ny        561*/
! >>>>>  surface source write information                            ssw        !/*ny        562*/
!        m1c=nwc of variable name.                                              !/*ny        563*/
!        m2c=surface-list index or index of current variable                    !/*ny        564*/
!        m3c=flag for in cells list                                             !/*ny        565*/
!        m4c=cells-list index                                                   !/*ny        566*/
 1100 if(m1c.ne.0)go to 1140                                                    !/*ny        567*/
      if(kitm.eq.0.and.hitm.ne.'('.and.hitm.ne.')')go to 1140                   !/*ny        568*/
      if(hitm.eq.')')go to 1130                                                 !/*ny        569*/
      if(m3c.ne.0)go to 1120                                                    !/*ny        570*/
      if(hitm.eq.'(')go to 1110                                                 !/*ny        571*/
      m2c=m2c+1                                                                 !/*ny        572*/
      jss(ljss+m2c)=ritm                                                        !/*ny4c2      20*/
      jsq(ljsq+m2c)=10*(abs(ritm)-aint(abs(ritm))+.001)                         !/*ny4c2      21*/
      jss(ljss+njss+nilw+m2c)=ljss+njss+nilw+njss+1+m4c+1                       !/*ny        574*/
      jss(ljss+njss+nilw+m2c+1)=ljss+njss+nilw+njss+1+m4c+1                     !/*ny        575*/
      return                                                                    !/*ny        576*/
 1110 m3c=1                                                                     !/*ny        577*/
      return                                                                    !/*ny        578*/
 1120 m4c=m4c+1                                                                 !/*ny        579*/
      jss(ljss+njss+nilw+njss+1+m4c)=iitm                                       !/*ny        580*/
      jss(ljss+njss+nilw+m2c+1)=ljss+njss+nilw+njss+1+m4c+1                     !/*ny        581*/
      return                                                                    !/*ny        582*/
 1130 m3c=0                                                                     !/*ny        583*/
      return                                                                    !/*ny        584*/
!                                                                               !/*ny        585*/
!        search for keywords after all surface numbers are read.                !/*ny        586*/
 1140 if(hitm.eq.'=')nwc=nwc-1                                                  !/*ny        587*/
      if(hitm.eq.'=')return                                                     !/*ny        588*/
      if(kitm.ne.0.or.nitm.ne.3)go to 1160                                      !/*ny        589*/
      if(kitm.ne.0.or.nitm.lt.3)go to 1160                                      !/*ny4c       35*/
      m1c=nwc                                                                   !/*ny        590*/
      do 1150 m2c=1,maxw                                                        !/*ny        591*/
 1150 if(hitm(1:3).eq.hblw(m2c))return                                          !/*ny4c       36*/
 1160 if(m2c.eq.1)nsph=iitm                                                     !/*ny        593*/
      if(m2c.eq.3)jss(ljss+njss+nwc-m1c)=iitm                                   !/*ny4a       58*/
      if(m2c.ne.2)return                                                        !/*ny4a       59*/
      i=index(hip,hitm(1:1))                                                    !/*ny4a       60*/
      if(i.ne.0)ipty(i)=1                                                       !/*ny4a       61*/
      return                                                                    !/*ny        602*/
!                                                                               !/*ny        603*/
! >>>>>  surface source read information                             ssr        !/*ny        604*/
!        m1c=nwc of variable name.                                              !/*ny        605*/
!        m2c=index of current variable.                                         !/*ny        606*/
 1180 if(hitm.eq.'=')nwc=nwc-1                                                  !/*ny        607*/
      if(hitm.eq.'=')return                                                     !/*ny        608*/
      if(nwc.gt.m1c)go to 1200                                                  !/*ny        609*/
      do 1190 m2c=1,maxv                                                        !/*ny        610*/
 1190 if(hitm(1:3).eq.hbln(m2c,2))return                                        !/*ny4c       37*/
 1200 if(kitm.eq.0)then                                                         !/*nyx        54*/
         if(m2c.eq.19)srv(1,19)=index(hip,hitm(1:1))                            !/*nyx        55*/
         m1c=nwc+1                                                              !/*nyx        56*/
         return                                                                 !/*nyx        57*/
      endif                                                                     !/*nyx        58*/
      ic=1                                                                      !/*ny4c       38*/
      if(m2c.gt.3)go to 1230                                                    !/*ny        613*/
      if(m2c.gt.2)go to 1220                                                    !/*ny        614*/
      if(m2c.gt.1)go to 1210                                                    !/*ny        615*/
      jasr(ljar+ic,njsr(ic)+nwc-m1c)=iitm                                       !/*ny4c       39*/
      if(nwc-m1c.lt.nilr(ic))return                                             !/*ny4c       40*/
      go to 1240                                                                !/*ny        618*/
 1210 jasr(ljar+ic,nwc-m1c)=ritm                                                !/*ny4c2      22*/
      jasr(ljar+2,nwc-m1c)=10*(abs(ritm)-aint(abs(ritm))+.001)                  !/*ny4c2      23*/
      if(nwc-m1c.lt.njsr(ic)-nilr(ic))return                                    !/*ny4c       42*/
      go to 1240                                                                !/*ny4c       43*/
 1220 iss(liss+ic,nwc-m1c)=ritm                                                 !/*ny4c2      24*/
      iss(liss+2,nwc-m1c)=10*(abs(ritm)-aint(abs(ritm))+.001)                   !/*ny4c2      25*/
      if(nwc-m1c.lt.njsx(ic))return                                             !/*ny4c       45*/
      go to 1240                                                                !/*ny4c       46*/
 1230 srv(nwc-m1c,m2c)=ritm                                                     !/*ny        625*/
      if(nwc-m1c.lt.nvs(m2c))return                                             !/*ny        626*/
 1240 m1c=nwc+1                                                                 !/*ny        627*/
      return                                                                    !/*ny        628*/
!                                                                               !/*ny        629*/
! >>>>>  universe designators                                          u        !/*ny        630*/
 1250 jun(ljun+nwc)=iitm                                                        !/*ny        631*/
      return                                                                    !/*ny        632*/
!                                                                               !/*ny        633*/
! >>>>>  cell transformations                                       trcl        !/*ny        634*/
!        m1c=cell index                                                         !/*ny        635*/
!        m2c=nwc of last cell seen, negative if left parens.                    !/*ny        636*/
 1260 go to(1270,1290,1300)1+index('()',hitm(1:1))                              !/*ny        637*/
 1270 if(m2c.lt.0)go to 1280                                                    !/*ny        638*/
      m1c=m1c+nwc-m2c                                                           !/*ny        639*/
      ktr(lktr+m1c)=iitm                                                        !/*ny        640*/
      m2c=nwc                                                                   !/*ny        641*/
      return                                                                    !/*ny        642*/
 1280 m=nwc+m2c+1                                                               !/*ny        643*/
      trf(ltrf+m,mxtr)=ritm                                                     !/*ny        644*/
      if(m.lt.5.or.m.gt.13)return                                               !/*ny        645*/
      if(icx.eq.1)trf(ltrf+m,mxtr)=cos(ritm*pie/180.)                           !/*ny4a       62*/
      if(abs(trf(ltrf+m,mxtr)-anint(trf(ltrf+m,mxtr))).gt.1e-10)                !/*ny        647*/
     & trf(ltrf+1,mxtr)=abs(trf(ltrf+1,mxtr))                                   !/*ny        648*/
      return                                                                    !/*ny        649*/
 1290 m1c=m1c+nwc-m2c                                                           !/*ny        650*/
      mxtr=mxtr+1                                                               !/*ny        651*/
      ktr(lktr+m1c)=1000+mxtr                                                   !/*ny        652*/
      trf(ltrf+1,mxtr)=-1000-mxtr                                               !/*ny        653*/
      m2c=-nwc                                                                  !/*ny        654*/
      return                                                                    !/*ny        655*/
 1300 if(nwc.ne.-m2c+2)go to 1310                                               !/*ny        656*/
      ktr(lktr+m1c)=trf(ltrf+2,mxtr)                                            !/*ny        657*/
      trf(ltrf+2,mxtr)=huge                                                     !/*ny        658*/
      mxtr=mxtr-1                                                               !/*ny        659*/
      go to 1320                                                                !/*ny        660*/
 1310 call trfmat(mxtr)                                                         !/*ny        661*/
 1320 m2c=nwc                                                                   !/*ny        662*/
      return                                                                    !/*ny        663*/
!                                                                               !/*ny        664*/
! >>>>>  lattice type                                                lat        !/*ny        665*/
 1330 if(iitm.eq.0)return                                                       !/*ny        666*/
      lat(llat+1,nwc)=iitm                                                      !/*ny        667*/
      nlat=nlat+1                                                               !/*ny        668*/
      lat(llat+2,nwc)=nlat                                                      !/*ny        669*/
      return                                                                    !/*ny        670*/
!                                                                               !/*ny        671*/
! >>>>>  cell-filling universes, with transformations               fill        !/*ny        672*/
!        m1c=cell index                                                         !/*ny        673*/
!        m2c(positive)=nwc of last item seen, not in parens                     !/*ny        674*/
!        m2c(negative)=-nwc of left parens, when in parens                      !/*ny        675*/
!        m3c=0 when not in lattice fill                                         !/*ny        676*/
!        m3c(negative)=-nwc of i2 in i1:i2                                      !/*ny        677*/
!        m3c(p0sitive)=n of laf(llaf+m,n)                                       !/*ny        678*/
!        m4c=maximum value of n                                                 !/*ny        679*/
 1340 go to(1350,1400,1410,1420)1+index(':()',hitm(1:1))                        !/*ny        680*/
 1350 if(m2c.lt.0)go to 1390                                                    !/*ny        681*/
      if(m3c.eq.0)go to 1380                                                    !/*ny        682*/
      if(m3c.gt.0)go to 1360                                                    !/*ny        683*/
      i=nwc+m3c                                                                 !/*ny        684*/
      if(mod(i,3).ne.0)laf(llaf+mlaf+2+i/3,1)=iitm                              !/*ny        685*/
      if(mod(i,3).eq.0)laf(llaf+mlaf+1+i/3,2)=iitm-                             !/*ny        686*/
     & laf(llaf+mlaf+1+i/3,1)+1                                                 !/*ny        687*/
      if(i.ne.6)return                                                          !/*ny        688*/
      m3c=2                                                                     !/*ny        689*/
      m2c=nwc                                                                   !/*ny        690*/
      m4c=2+laf(llaf+mlaf+1,2)*laf(llaf+mlaf+2,2)*                              !/*ny        691*/
     & laf(llaf+mlaf+3,2)                                                       !/*ny        692*/
      return                                                                    !/*ny        693*/
 1360 m3c=m3c+nwc-m2c                                                           !/*ny        694*/
      if(m3c.gt.m4c)go to 1370                                                  !/*ny        695*/
      laf(llaf+mlaf+1,m3c)=iitm                                                 !/*ny        696*/
      m2c=nwc                                                                   !/*ny        697*/
      return                                                                    !/*ny        698*/
 1370 mlaf=mlaf+m4c*3                                                           !/*ny        699*/
      m3c=0                                                                     !/*ny        700*/
 1380 m1c=m1c+nwc-m2c                                                           !/*ny        701*/
      mfl(lmfl+1,m1c)=iitm                                                      !/*ny        702*/
      m2c=nwc                                                                   !/*ny        703*/
      return                                                                    !/*ny        704*/
 1390 m=nwc+m2c+1                                                               !/*ny        705*/
      trf(ltrf+m,mxtr)=ritm                                                     !/*ny        706*/
      if(m.lt.5.or.m.gt.13)return                                               !/*ny        707*/
      if(icx.eq.1)trf(ltrf+m,mxtr)=cos(ritm*pie/180.)                           !/*ny4a       63*/
      if(abs(trf(ltrf+m,mxtr)-anint(trf(ltrf+m,mxtr))).gt.1e-10)                !/*ny        709*/
     & trf(ltrf+1,mxtr)=abs(trf(ltrf+1,mxtr))                                   !/*ny        710*/
      return                                                                    !/*ny        711*/
 1400 if(m3c.ne.0)return                                                        !/*ny        712*/
      m3c=-nwc-1                                                                !/*ny        713*/
      laf(llaf+mlaf+1,1)=mfl(lmfl+1,m1c)                                        !/*ny        714*/
      mfl(lmfl+1,m1c)=-llaf-mlaf                                                !/*ny        715*/
      return                                                                    !/*ny        716*/
 1410 mxtr=mxtr+1                                                               !/*ny        717*/
      if(m3c.eq.0)mfl(lmfl+3,m1c)=1000+mxtr                                     !/*ny        718*/
      if(m3c.ne.0)laf(llaf+mlaf+3,m3c)=1000+mxtr                                !/*ny        719*/
      trf(ltrf+1,mxtr)=-1000-mxtr                                               !/*ny        720*/
      m2c=-nwc                                                                  !/*ny        721*/
      return                                                                    !/*ny        722*/
 1420 if(nwc.ne.-m2c+2)go to 1430                                               !/*ny        723*/
      if(m3c.eq.0)mfl(lmfl+3,m1c)=trf(ltrf+2,mxtr)                              !/*ny        724*/
      if(m3c.ne.0)laf(llaf+mlaf+3,m3c)=trf(ltrf+2,mxtr)                         !/*ny        725*/
      trf(ltrf+2,mxtr)=huge                                                     !/*ny        726*/
      mxtr=mxtr-1                                                               !/*ny        727*/
      go to 1440                                                                !/*ny        728*/
 1430 call trfmat(mxtr)                                                         !/*ny        729*/
 1440 m2c=nwc                                                                   !/*ny        730*/
      return                                                                    !/*ny        731*/
!                                                                               !/*ny        732*/
! >>>>>  photon-production bias                                    pikmt        !/*ny        733*/
 1450 npikmt=npikmt+1                                                           !/*ny        734*/
      pik(lpik+npikmt)=ritm                                                     !/*ny        735*/
      return                                                                    !/*ny        736*/
!                                                                               !/*ny        737*/
! >>>>>  cell-wise energy cutoffs                                   elpt        !/*ny        738*/
 1460 do 1470 i=1,mipt                                                          !/*ny        739*/
 1470 if(nqp(i).ne.0)elp(lelp+i,nwc)=ritm                                       !/*ny        740*/
      return                                                                    !/*ny        741*/
!                                                                               !/*ny        742*/
! >>>>>  bremsstrahlung biasing                                    bbrem        !/*ny        743*/
 1480 if(nwc.le.49)bbrem(nwc)=1./ritm                                           !/*ny4c       47*/
      if(nwc.gt.49)mbi(lmbi+nwc-49)=-iitm                                       !/*ny4c       48*/
      return                                                                    !/*ny        746*/
!                                                                               !/*ny        747*/
! >>>>>  runtime tally monitor                                     mplot        !/*ny        748*/
 1490 i=index(klin,'freq')                                                      !/*ny        749*/
      if(i.eq.0)go to 1500                                                      !/*ny        750*/
      j=i+4                                                                     !/*ny        751*/
      if(kdarg(klin,j,0,ht,a).ge.1)freq=a                                       !/*ny        752*/
      klin(i:j-1)=' '                                                           !/*ny        753*/
 1500 l=index(klin,'$')                                                         !/*ny4a       64*/
      if(l.eq.0)l=81                                                            !/*ny4a       65*/
      m=index(klin,'&')                                                         !/*ny4a       66*/
      if(m.eq.0)m=81                                                            !/*ny4a       67*/
      if(m.lt.l)irc=-1                                                          !/*ny4a       68*/
      m=min(l,m)                                                                !/*ny4a       69*/
      call nxtsym(klin,' $&',1,i,j,0)                                           !/*ny4a       70*/
      if(klin(i:j).eq.'mplot')i=i+5                                             !/*ny4a       71*/
      do 1505 k=m-1,i,-1                                                        !/*ny4a       72*/
 1505 if(klin(k:k).ne.' ')go to 1506                                            !/*ny4a       73*/
      return                                                                    !/*ny4a       74*/
 1506 if(k-i.ge.78)call erprnt(2,1,0,0,0,0,0,1,                                 !/*ny4a       75*/
     & '44hinput line too long for mcplot scratch file.')                       !/*ny4a       76*/
      write(iu4,'(a75,2h &)')klin(i:k)                                          !/*ny4a       77*/
      return                                                                    !/*ny        755*/
!                                                                               !/*ny4a       78*/
! >>>>>  ptrac specifications                                      ptrac        !/*ny4a       79*/
!        m1c=index of current ptrac keyword.                                    !/*ny4a       80*/
!        m2c=pointer for ptr() storage.                                         !/*ny4a       81*/
!        m3c=number of entries following the current keyword.                   !/*ny4a       82*/
 1510 go to(1520,1600,1600)kitm+1                                               !/*ny4a       83*/
 1520 if(index(',=',hitm(1:1)).ne.0)return                                      !/*ny4a       84*/
      if(m1c.eq.0)go to 1570                                                    !/*ny4a       85*/
      i=0                                                                       !/*ny4a       86*/
      if(hptr(m1c).eq.'event')i=index('src bnk sur col ter ',hitm(1:4))         !/*ny4a       87*/
      if(hptr(m1c).eq.'file')i=index('asc bin ',hitm(1:4))                      !/*ny4a       88*/
      if(hptr(m1c).eq.'type')i=index('n   p   e   ',hitm(1:4))                  !/*ny4a       89*/
      if(hptr(m1c).eq.'write')i=index('pos all ',hitm(1:4))                     !/*ny4a       90*/
      i=(i+3)/4                                                                 !/*ny4a       91*/
      if(hptr(m1c).ne.'filter')go to 1540                                       !/*ny4a       92*/
      do 1530 i=1,24                                                            !/*ny4a       93*/
 1530 if(hitm(1:7).eq.hpbl(i))go to 1540                                        !/*ny4a       94*/
      go to 1570                                                                !/*ny4a       95*/
 1540 if(i.eq.0)go to 1570                                                      !/*ny4a       96*/
      if(m3c.ne.0)go to 1550                                                    !/*ny4a       97*/
      m2c=m2c+1                                                                 !/*ny4a       98*/
      iptra(m1c)=lptr+m2c                                                       !/*ny4a       99*/
 1550 j=1                                                                       !/*ny4a      100*/
      if(hptr(m1c).ne.'filter')go to 1560                                       !/*ny4a      101*/
      j=3-mod(m3c,3)                                                            !/*ny4a      102*/
      if(j.eq.2)ptr(lptr+m2c+1)=ptr(lptr+m2c)                                   !/*ny4a      103*/
 1560 m2c=m2c+j                                                                 !/*ny4a      104*/
      m3c=m3c+j                                                                 !/*ny4a      105*/
      ptr(iptra(m1c))=m3c                                                       !/*ny4a      106*/
      ptr(lptr+m2c)=i                                                           !/*ny4a      107*/
      return                                                                    !/*ny4a      108*/
 1570 do 1580 m1c=1,nptr                                                        !/*ny4a      109*/
 1580 if(hitm(1:7).eq.hptr(m1c).and.iptra(m1c).eq.0)go to 1590                  !/*ny4a      110*/
      m1c=0                                                                     !/*ny4a      111*/
      return                                                                    !/*ny4a      112*/
 1590 iptra(m1c)=-1                                                             !/*ny4a      113*/
      m3c=0                                                                     !/*ny4a      114*/
      return                                                                    !/*ny4a      115*/
 1600 if(m1c.eq.0)return                                                        !/*ny4a      116*/
      if(m3c.ne.0)go to 1610                                                    !/*ny4a      117*/
      m2c=m2c+1                                                                 !/*ny4a      118*/
      iptra(m1c)=lptr+m2c                                                       !/*ny4a      119*/
 1610 m2c=m2c+1                                                                 !/*ny4a      120*/
      m3c=m3c+1                                                                 !/*ny4a      121*/
      ptr(iptra(m1c))=m3c                                                       !/*ny4a      122*/
      if(kitm.eq.1)ptr(lptr+m2c)=ritm                                           !/*ny4a      123*/
      if(kitm.eq.2)ptr(lptr+m2c)=iitm                                           !/*ny4a      124*/
      return                                                                    !/*ny4a      125*/
!                                                                               !/*ny4b        5*/
! >>>>>  perturbation specificatons                                 pert        !/*ny4b        6*/
!        m1c=index of current pert keyword.                                     !/*ny4b        7*/
 1620 go to(1630,1650,1650)kitm+1                                               !/*ny4b        8*/
 1630 if(index(',=',hitm(1:1)).ne.0)return                                      !/*ny4b        9*/
      do 1640 m1c=1,npkey                                                       !/*ny4b       10*/
 1640 if(hitm(1:7).eq.hptb(m1c).and.iptb(lipb+1+2*m1c,npert).eq.0)return        !/*ny4b       11*/
      m1c=0                                                                     !/*ny4b       12*/
      return                                                                    !/*ny4b       13*/
 1650 if(m1c.eq.0)return                                                        !/*ny4b       14*/
      ipert=ipert+1                                                             !/*ny4b       15*/
      iptb(lipb+1+2*m1c,npert)=iptb(lipb+1+2*m1c,npert)+1                       !/*ny4b       16*/
      if(iptb(lipb+2+2*m1c,npert).eq.0)iptb(lipb+2+2*m1c,npert)=ipert           !/*ny4b       17*/
      rptb(lrpt+ipert)=ritm                                                     !/*ny4b       18*/
      if(hptb(m1c).eq.'cell')rptb(lrpt+ipert)=namchg(1,iitm)                    !/*ny4b       19*/
      return                                                                    !/*ny4b       20*/
!                                                                               !/*ny4c       49*/
! >>>>>  superimposed weight window generator mesh                  mesh        !/*ny4c       50*/
!        m1c=index of current mesh keyword.                                     !/*ny4c       51*/
!        m2c=number of entries for current keyword.                             !/*ny4c       52*/
 1660 if(kitm.ne.0)go to 1700                                                   !/*ny4c       53*/
      if(index(',=',hitm(1:1)).ne.0)return                                      !/*ny4c       54*/
      if(m1c.eq.0)go to 1670                                                    !/*nyx        59*/
      if(hmsh(m1c).ne.'geom'.or.imesh(m1c).ne.0)go to 1670                      !/*ny4c       55*/
      imesh(m1c)=imesh(m1c)+1                                                   !/*ny4c       56*/
      if(nwgeoa.eq.3)return                                                     !/*ny4c       57*/
      if(hitm(1:3).eq.'xyz'.or.hitm(1:3).eq.'rec')nwgeoa=1                      !/*ny4c       58*/
      if(hitm(1:3).eq.'rzt'.or.hitm(1:3).eq.'cyl')nwgeoa=2                      !/*ny4c       59*/
      return                                                                    !/*ny4c       60*/
 1670 do 1680 m1c=1,nmkey                                                       !/*ny4c       61*/
 1680 if(hitm(1:7).eq.hmsh(m1c))go to 1690                                      !/*ny4c       62*/
      m1c=0                                                                     !/*ny4c       63*/
 1690 m2c=0                                                                     !/*ny4c       64*/
      return                                                                    !/*ny4c       65*/
 1700 if(m1c.eq.0)return                                                        !/*ny4c       66*/
      imesh(m1c)=imesh(m1c)+1                                                   !/*ny4c       67*/
      if(hmsh(m1c).eq.'ref')then                                                !/*ny4c       68*/
         m2c=m2c+1                                                              !/*ny4c       69*/
         wgma(lwga+m2c)=ritm                                                    !/*ny4c       70*/
      else if(hmsh(m1c).eq.'origin')then                                        !/*ny4c       71*/
         m2c=m2c+1                                                              !/*ny4c       72*/
         wwma(m2c+3)=ritm                                                       !/*ny4c       73*/
      else if(hmsh(m1c).eq.'axs')then                                           !/*ny4c       74*/
         m2c=m2c+1                                                              !/*ny4c       75*/
         wwma(m2c+19)=ritm                                                      !/*ny4c       76*/
      else if(hmsh(m1c).eq.'vec')then                                           !/*ny4c       77*/
         m2c=m2c+1                                                              !/*ny4c       78*/
         wwma(m2c+15)=ritm                                                      !/*ny4c       79*/
      else if(hmsh(m1c).eq.'imesh')then                                         !/*ny4c       80*/
         m2c=m2c+3                                                              !/*ny4c       81*/
         ir=lwga+2+m2c                                                          !/*ny4c       82*/
         wgma(ir)=ritm                                                          !/*ny4c       83*/
         if(m2c.gt.3.and.ritm.le.wgma(ir-3))go to 1710                          !/*ny4c       84*/
      else if(hmsh(m1c).eq.'iints')then                                         !/*ny4c       85*/
         m2c=m2c+3                                                              !/*ny4c       86*/
         ir=lwga+1+m2c                                                          !/*ny4c       87*/
         wgma(ir)=ritm                                                          !/*ny4c       88*/
      else if(hmsh(m1c).eq.'jmesh')then                                         !/*ny4c       89*/
         m2c=m2c+3                                                              !/*ny4c       90*/
         ir=lwga+2+m2c+3*nint(wwma(7))+3                                        !/*ny4c       91*/
         wgma(ir)=ritm                                                          !/*ny4c       92*/
         if(m2c.gt.3.and.ritm.le.wgma(ir-3))go to 1710                          !/*ny4c       93*/
      else if(hmsh(m1c).eq.'jints')then                                         !/*ny4c       94*/
         m2c=m2c+3                                                              !/*ny4c       95*/
         ir=lwga+1+m2c+3*nint(wwma(7))+3                                        !/*ny4c       96*/
         wgma(ir)=ritm                                                          !/*ny4c       97*/
      else if(hmsh(m1c).eq.'kmesh')then                                         !/*ny4c       98*/
         m2c=m2c+3                                                              !/*ny4c       99*/
         ir=lwga+2+m2c+3*nint((wwma(7)+wwma(8)))+6                              !/*ny4c      100*/
         wgma(ir)=ritm                                                          !/*ny4c      101*/
         if(m2c.gt.3.and.ritm.le.wgma(ir-3))go to 1710                          !/*ny4c      102*/
      else if(hmsh(m1c).eq.'kints')then                                         !/*ny4c      103*/
         m2c=m2c+3                                                              !/*ny4c      104*/
         ir=lwga+1+m2c+3*nint((wwma(7)+wwma(8)))+6                              !/*ny4c      105*/
         wgma(ir)=ritm                                                          !/*ny4c      106*/
      end if                                                                    !/*ny4c      107*/
      return                                                                    !/*ny4c      108*/
 1710 call erprnt(2,1,0,0,0,0,0,1,                                              !/*ny4c      109*/
     & '41hentries are not monotonically increasing.')                          !/*ny4c      110*/
      return                                                                    !/*ny4c      111*/
!                                                                               !/*ny4c      112*/
 1720 return                                                                    !/*nyx        62*/
!                                                                               !/*ny4c2      26*/
! >>>>>  photonuclear isotope override                               mpn        !/*ny4c2      27*/
!        m1c = material index on mpn and preceding m card.                      !/*ny4c2      28*/
 1730 if(m1c.ne.0)izn(lizn+jmd(ljmd+m1c)+nwc-1)=iitm                            !/*ny4c2      29*/
      return                                                                    !/*ny4c2      30*/
!                                                                               !/*ny        756*/
! >>>>> lca                                                          lca        !/*nyx        64*/
 1800 if (nwc.ge.1.and.nwc.le.m_lca) lcaopt(nwc)=iitm                           !/*nyx        65*/
      return                                                                    !/*nyx        66*/
!                                                                               !/*nyx        67*/
! >>>>> lcb                                                          lcb        !/*nyx        68*/
 1850 if (nwc.ge.1.and.nwc.le.m_lcb) optlcb(nwc)=ritm                           !/*nyx        69*/
      return                                                                    !/*nyx        70*/
!                                                                               !/*nyx        71*/
! >>>>> lea                                                          lea        !/*nyx        72*/
 1900 if (nwc.ge.1.and.nwc.le.m_lea) leaopt(nwc)=iitm                           !/*nyx        73*/
      return                                                                    !/*nyx        74*/
!                                                                               !/*nyx        75*/
! >>>>> leb                                                          leb        !/*nyx        76*/
 1950 if (nwc.ge.1.and.nwc.le.m_leb) optleb(nwc)=ritm                           !/*nyx        77*/
      return                                                                    !/*nyx        78*/
!                                                                               !/*nyx        79*/
! >>>>> History tape writing.                                      histp        !/*nyx        80*/
 2000 continue                                                                  !/*nyx        81*/
#ifdef HISTP                                                                    /*!nyx        82*/
      do ic=1,mxa                                                               !/*nyx        83*/
         if (ncl(lncl+ic).eq.iitm) then                                         !/*nyx        84*/
            nhw(lnhw+ic) = 1                                                    !/*nyx        85*/
            return                                                              !/*nyx        86*/
         endif                                                                  !/*nyx        87*/
      enddo                                                                     !/*nyx        88*/
      call erprnt(2,1,1,iitm,0,0,0,1,'25hno such cell in problem: ,i9')         !/*nyx        89*/
#endif                                                                          /*!nyx        90*/
      return                                                                    !/*nyx        91*/
!                                                                               !/*nyx        92*/
! >>>>>  Turn off transport for direct detector contributions.     notrn        !/*nyx        93*/
 2050 continue                                                                  !/*nyx        94*/
#ifdef RADIOG                                                                   /*!nyx        95*/
      notrn=1                                                                   !/*nyx        96*/
#endif                                                                          /*!nyx        97*/
      return                                                                    !/*nyx        98*/
!                                                                               !/*nyx        99*/
! >>>>>  Tally no print card.                                      talnp        !/*nyx       100*/
 2100 continue                                                                  !/*nyx       101*/
#ifdef RADIOG                                                                   /*!nyx       102*/
      if(nwc.eq.1.and.iitm.eq.0)go to 2120                                      !/*nyx       103*/
      if(iitm.lt.0)go to 2130                                                   !/*nyx       104*/
      do 2110 i=1,notal                                                         !/*nyx       105*/
 2110 if(ntprt(i).eq.iitm)return                                                !/*nyx       106*/
      ntprt(notal+1)=iitm                                                       !/*nyx       107*/
      notal=notal+1                                                             !/*nyx       108*/
      return                                                                    !/*nyx       109*/
 2120 notal=0                                                                   !/*nyx       110*/
      return                                                                    !/*nyx       111*/
 2130 if(notal.le.0)go to 2170                                                  !/*nyx       112*/
      do 2140 i=1,notal                                                         !/*nyx       113*/
 2140 if(ntprt(i).eq.abs(iitm))go to 2150                                       !/*nyx       114*/
      return                                                                    !/*nyx       115*/
 2150 do 2160 j=i+1,notal                                                       !/*nyx       116*/
 2160 ntprt(j-1)=ntprt(j)                                                       !/*nyx       117*/
      notal=notal-1                                                             !/*nyx       118*/
      return                                                                    !/*nyx       119*/
 2170 j=0                                                                       !/*nyx       120*/
      do 2180 i=1,ntal                                                          !/*nyx       121*/
      k1=jptal(ljpt+1,i)                                                        !/*nyx       122*/
      if(k1.eq.abs(iitm))go to 2180                                             !/*nyx       123*/
      ntprt(j+1)=k1                                                             !/*nyx       124*/
      j=j+1                                                                     !/*nyx       125*/
 2180 continue                                                                  !/*nyx       126*/
      notal=j                                                                   !/*nyx       127*/
#endif                                                                          /*!nyx       128*/
      return                                                                    !/*nyx       129*/
!                                                                               !/*nyx       130*/
! >>>>>  Secondary Particle Biasing                                spabi        !/*nyx       131*/
 2200 continue                                                                  !/*nyx       132*/
#ifdef SPABI                                                                    /*!nyx       133*/
      call process_spb_items                                                    !/*nyx       134*/
#endif                                                                          /*!nyx       135*/
      return                                                                    !/*nyx       136*/
!                                                                               !/*nyxc       22*/
 2210 continue                                                                  !/*nyxc       28*/
!                                                                               !/*nyx       137*/
! >>>>>  first spare card type                                        za        !/*ny        757*/
 9910 continue                                                                  !/*ny        758*/
      return                                                                    !/*ny        759*/
!                                                                               !/*ny        760*/
! >>>>>  second spare card type                                       zb        !/*ny        761*/
 9920 continue                                                                  !/*ny        762*/
      return                                                                    !/*ny        763*/
!                                                                               !/*ny        764*/
! >>>>>  third spare card type                                        zc        !/*ny        765*/
 9930 continue                                                                  !/*ny        766*/
      return                                                                    !/*ny        767*/
      end                                                                       !/*ny        769*/

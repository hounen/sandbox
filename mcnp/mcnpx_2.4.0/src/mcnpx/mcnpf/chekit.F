!#ident chekit.F from deck ce                                                   !/*ce          1*/
      subroutine chekit                                                         !/*ce          2*/
!        check the next input item before it is stored.                         !/*cex         1*/
      use global_data                                                           !/*cex         2*/
      use imcn_data                                                             !/*cex         3*/
!                                                                               !/*cex         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*cex         5*/
      implicit integer(kindi) (i-n)                                             !/*cex         6*/
!                                                                               !/*cex         7*/
      character hs*10,ht*69                                                     !/*ce          7*/
!                                                                               !/*ce          8*/
!        reject wrong kinds of numerical data and too many entries.             !/*ce          9*/
      if(krq(5,ica).ne.0.and.kitm.eq.0)                                         !/*ce         10*/
     & call erprnt(2,1,0,0,0,0,0,1,'26hentries must be numerical.')             !/*ce         11*/
      if(krq(5,ica).eq.1.and.ritm.lt.0.)                                        !/*ce         12*/
     & call erprnt(2,1,0,0,0,0,0,1,'29hentries must not be negative.')          !/*ce         13*/
      if(krq(5,ica).eq.2.and.kitm.eq.1)                                         !/*ce         14*/
     & call erprnt(2,1,0,0,0,0,0,1,'25hentries must be integers.')              !/*ce         15*/
      if(nwc.gt.krq(6,ica).and.krq(6,ica).gt.0)call erprnt(2,1,1,               !/*ce         16*/
     & krq(6,ica),0,0,0,1,'29htoo many entries.  maximum is,i6')                !/*ce         17*/
      if(nwc.gt.-krq(6,ica).and.krq(6,ica).lt.0)                                !/*ce         18*/
     & call erprnt(2,1,2,nwc,-krq(6,ica),0,0,1,                                 !/*ce4a        1*/
     & '17htoo many entries:,i5,11h were read,,i5,14h are required.')           !/*ce4a        2*/
      if(ics.lt.0)return                                                        !/*ce         20*/
!                                                                               !/*ce         21*/
!        do the checks necessary for the particular card type.                  !/*ce         22*/
      go to( 20, 60,100,110,130, 10,140, 10,150,160,220,230,240,250, 10,        !/*cexc        1*/
     &       10,260,270,290,290,270, 10,310,400,400,400,400,440, 10,460,        !/*ce         24*/
     &      460,470,480,490,530,530, 10, 10, 10,540, 10,550,560,460, 10,        !/*ce         25*/
     &      570, 10,580, 10,640,650,660,700,710, 10)ica                         !/*ce         26*/
      go to( 10, 10, 10,730,760, 10, 10, 10, 10, 10,770,780,860,865, 10,        !/*ce4a        3*/
     &    870,10,910,960,10,1000,1030,1040,1050,10,10,1060,10,1090,1180,        !/*ce4c        1*/
     &     1240,1280,1290,1300,10,10,10,10,10,10,1350,400,400,400,10,           !/*cex         8*/
     &     10,1370,9910,9920,9930)ica-55                                        !/*cexc        2*/
   10 return                                                                    !/*ce         30*/
!                                                                               !/*ce         31*/
! >>>>>  cell descriptions                                                      !/*ce         32*/
!        m1c=indicator of incorrect use of parentheses.                         !/*ce         33*/
!        m2c=previous special character:  0=none  1=(  2=)  3=:  4=#            !/*ce         34*/
!        m3c=flag for cell parameters.                                          !/*ce         35*/
   20 if(nwc.eq.1.and.hitm.eq.'like')return                                     !/*ce         36*/
      if(nwc.ne.1.and.hitm.eq.'like')call erprnt(2,1,1,icn,0,0,0,1,             !/*ce4a        5*/
     & '36hlike is not first entry on cell card,i5')                            !/*ce4a        6*/
      if(likef.eq.0)go to 30                                                    !/*ce         37*/
      if(nwc.eq.2.and.kitm.ne.2.or.nwc.eq.3.and.hitm.ne.'but')go to 9010        !/*ce         38*/
      if(nwc.ne.2)return                                                        !/*ce         39*/
      if(namchg(1,iitm).eq.0)call erprnt(2,1,1,iitm,0,0,0,0,                    !/*ce         40*/
     & 'i5,31h is not a cell of this problem.')                                 !/*ce         41*/
      return                                                                    !/*ce         42*/
   30 k=index('():#,=',hitm(1:1))                                               !/*ce         43*/
      if(m3c.ne.0)return                                                        !/*ce4a        7*/
      if(k.eq.5)go to 9010                                                      !/*ce         44*/
      if(k.eq.0.and.kitm.eq.0)return                                            !/*ce         45*/
      if(k.eq.1.or.k.eq.2)m1c=m1c+3-2*k                                         !/*ce         47*/
      if(m1c.lt.0)m1c=10000                                                     !/*ce         48*/
      if(nwc.eq.1.and.kitm.ne.2)call erprnt(2,1,0,0,0,0,0,1,                    !/*ce         49*/
     & '34hmaterial number is not an integer.')                                 !/*ce         50*/
      if(nwc.ne.1.or.iitm.eq.0.or.kitm.ne.2)go to 50                            !/*ce         51*/
      do 40 i=1,nmat1                                                           !/*ce         52*/
   40 if(nmt(lnmt+i).eq.iitm)go to 50                                           !/*ce         53*/
      call erprnt(2,1,1,iitm,0,0,0,0,'26hno m card for material no.,i7')        !/*cor4-1     45*/
   50 if(nwc.eq.2.and.mat(lmat+mxa).ne.0.and.kitm.eq.0)call erprnt              !/*ce         55*/
     & (2,1,0,0,0,0,0,1,'29hthe density is not numerical.')                     !/*ce         56*/
      if(nwc.eq.2.and.mat(lmat+mxa).ne.0.and.ritm.eq.0.)                        !/*ce         57*/
     & call erprnt(2,1,0,0,0,0,0,1,'16hdensity is zero.')                       !/*ce         58*/
      if(m2c.eq.3.and.k.eq.2.or.(m2c.eq.1.or.m2c.eq.3).and.k.eq.3)              !/*ce         62*/
     & call erprnt(2,1,0,0,0,0,0,1,'23hincorrect use of colon.')                !/*ce         63*/
      if(m2c.eq.4.and.k.ge.2)                                                   !/*ce         64*/
     & call erprnt(2,1,0,0,0,0,0,1,'19hincorrect use of #.')                    !/*ce         65*/
      return                                                                    !/*ce         66*/
!                                                                               !/*ce         67*/
! >>>>>  surface descriptions                                                   !/*ce         68*/
   60 if(nwc.ne.1.or.kitm.eq.0)go to 70                                         !/*ce         69*/
      if(kitm.eq.1.or.iitm.eq.0.or.iitm.gt.999)call erprnt(2,1,0,0,0,0,         !/*ce4a        8*/
     & 0,1,'51hform of surface transformation number is incorrect.')            !/*ce         71*/
      return                                                                    !/*ce         72*/
   70 if(nwc.gt.2.or.nwc.eq.2.and.jtr(ljtr+mxj).eq.0.and.ksu(lksu+mxj)          !/*ce4a        9*/
     & .le.0)go to 90                                                           !/*ce4a       10*/
      do 80 i=1,39                                                              !/*ce4c        3*/
   80 if(ksf(i).eq.hitm)return                                                  !/*ce         75*/
      if('hex'.eq.hitm)return                                                   !/*ce4c        4*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ce         76*/
     & '52hthe surface type is not recognized:  '//hitm(1:15))                  !/*ce         77*/
      return                                                                    !/*ce         78*/
   90 if(kitm.eq.0)go to 9010                                                   !/*ce         79*/
      return                                                                    !/*ce         80*/
!                                                                               !/*ce         81*/
! >>>>>  specification of coordinate transformations of surfaces      tr        !/*ce         82*/
  100 if(nwc.eq.13.and.abs(ritm).ne.1.)call erprnt(2,1,0,0,0,0,0,0,             !/*ce         83*/
     & '33hallowed values of m are 1 and -1.')                                  !/*ce         84*/
      return                                                                    !/*ce         85*/
!                                                                               !/*ce         86*/
! >>>>>  vectors                                                    vect        !/*ce         87*/
  110 if(mod(nwc,4).ne.1)go to 120                                              !/*ce         88*/
      if(hitm(1:1).ne.'v'.or.nitm.eq.1)go to 9010                               !/*ce         89*/
      if(kdata(hitm(2:nitm)).ne.2)go to 9010                                    !/*ce         90*/
      return                                                                    !/*ce         91*/
  120 if(kdata(hitm(1:nitm)).eq.0)go to 9010                                    !/*ce         92*/
      return                                                                    !/*ce         93*/
!                                                                               !/*ce         94*/
! >>>>>  problem mode                                               mode        !/*ce         95*/
  130 if(nitm.ne.1.or.index(hip,hitm(1:1)).eq.0)go to 9010                      !/*ce         96*/
      if(kpt(1).ne.0.and.kpt(2).ne.0.and.ispn.eq.0.and.nwc.eq.1)                !/*ce4c2       2*/
     & call erprnt(2,2,0,0,0,0,0,0,                                             !/*ce4c2       3*/
     & '44hphotonuclear physics may be needed (phys:p).')                       !/*ce4c2       4*/
      return                                                                    !/*ce         97*/
!                                                                               !/*ce         98*/
! >>>>>  cell volumes for tallies                                    vol        !/*ce         99*/
  140 if(kitm.eq.0.and.hitm.ne.'no')go to 9010                                  !/*ce        100*/
      return                                                                    !/*ce        101*/
!                                                                               !/*ce        102*/
! >>>>>  photon weight lower bounds                                  pwt        !/*ce        103*/
!        m1c=warning message latch.                                             !/*ce        104*/
  150 if(m1c.ne.0.or.ritm.eq.0..or.ritm.eq.-1e6.or.nww(2).eq.0)return           !/*ce        105*/
      m1c=1                                                                     !/*ce        106*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*ce        107*/
     & '54hphoton weight windows supersede values other than -1e6')             !/*ce4b        2*/
      return                                                                    !/*ce        109*/
!                                                                               !/*ce        110*/
! >>>>>  exponential transform                                       ext        !/*ce        111*/
  160 i=1                                                                       !/*ce        112*/
      if(hitm(1:1).eq.'+'.or.hitm(1:1).eq.'-')i=2                               !/*ce        113*/
      if(hitm(i:i).ne.'s')go to 170                                             !/*ce        114*/
      j=i+1                                                                     !/*ce        115*/
      if(index(' xyzv',hitm(j:j)).eq.0)go to 9010                               !/*ce        116*/
      go to 200                                                                 !/*ce        117*/
  170 do 180 j=i,nitm                                                           !/*ce        118*/
  180 if(index('vxyz',hitm(j:j)).ne.0)go to 190                                 !/*ce        119*/
  190 if(i.eq.j)go to 9010                                                      !/*ce        120*/
      if(kdata(hitm(i:j-1)).eq.0)go to 9010                                     !/*ce        121*/
      ht=hitm(i:j-1)                                                            !/*ce        122*/
      read(ht,'(bn,e21.0)')a                                                    !/*ce        123*/
      if(abs(a).ge.1.)go to 9010                                                !/*ce        124*/
  200 if(j.eq.nitm+1)return                                                     !/*ce        125*/
      if(hitm(j:j).ne.'v')go to 210                                             !/*ce        126*/
      if(j.eq.nitm)go to 9010                                                   !/*ce        127*/
      if(kdata(hitm(j+1:nitm)).ne.2)go to 9010                                  !/*ce        128*/
      return                                                                    !/*ce        129*/
  210 if(j.ne.nitm)go to 9010                                                   !/*ce        130*/
      return                                                                    !/*ce        131*/
!                                                                               !/*ce        132*/
! >>>>>  forced collisions                                           fcl        !/*ce        133*/
  220 if(abs(ritm).gt.1.)call erprnt(2,1,0,0,0,0,0,0,                           !/*ce        134*/
     & '55hentry with absolute value > 1:  '//hitm(1:23))                       !/*ce        135*/
      if(ritm.ne.0..and.rho(lrho+nwc).eq.0.)                                    !/*ce        136*/
     & call erprnt(2,2,1,ncl(lncl+nwc),0,0,0,0,                                 !/*ce        137*/
     & '40hforced collision specified for void cell,i6')                        !/*ce        138*/
      return                                                                    !/*ce        139*/
!                                                                               !/*ce        140*/
! >>>>>  weight-window lower bounds                                  wwn        !/*ce        141*/
  230 if(ritm.lt.0..and.ritm.ne.-1)call erprnt(2,1,0,0,0,0,0,1,                 !/*ce        142*/
     & '48hweight-window lower bounds must not be negative.')                   !/*ce        143*/
      return                                                                    !/*ce        144*/
!                                                                               !/*ce        145*/
! >>>>>  weight-window energies                                      wwe        !/*ce        146*/
  240 if(ritm.eq.0.)call erprnt(2,1,0,0,0,0,0,0,                                !/*ce        147*/
     & '28hweight-window energy = zero.')                                       !/*ce        148*/
      return                                                                    !/*ce        149*/
!                                                                               !/*ce        150*/
! >>>>>  weight-window game parameters                               wwp        !/*ce        151*/
  250 if(nwc.eq.1.and.ritm.lt.2.)call erprnt                                    !/*ce        152*/
     & (2,1,0,0,0,0,0,0,'19hwup is less than 2.')                               !/*ce        153*/
      if(nwc.eq.2.and.ritm.le.1.)call erprnt                                    !/*ce        154*/
     & (2,1,0,0,0,0,0,0,'13hwsurv .le. 1.')                                     !/*ce        155*/
      if(nwc.eq.3.and.ritm.lt.1.)call erprnt                                    !/*ce        156*/
     & (2,1,0,0,0,0,0,0,'21hmxspl is less than 1.')                             !/*ce        157*/
      if(nwc.eq.4.and.ritm.ne.0..and.abs(ritm).ne.1.)call erprnt                !/*ce        158*/
     & (2,1,0,0,0,0,0,0,'42hallowed values of mwhere are -1, 0, and 1.')        !/*ce        159*/
      return                                                                    !/*ce        160*/
!                                                                               !/*ce        161*/
! >>>>>  cells where fission is treated like capture                nonu        !/*ce        162*/
  260 if(kitm.ne.2.or.iitm.lt.0.or.iitm.gt.2)                                   !/*ce        163*/
     & call erprnt(2,1,0,0,0,0,0,1,'28hentry other than 0, 1, or 2.')           !/*ce        164*/
      return                                                                    !/*ce        165*/
!                                                                               !/*ce        166*/
! >>>>>  source distributions                                      si,ds        !/*ce        167*/
  270 if(hitm(1:1).ne.'d'.or.nitm.lt.2.or.nitm.gt.4)go to 280                   !/*ce        168*/
      if(kdata(hitm(2:nitm)).ne.2)go to 9010                                    !/*ce        169*/
      return                                                                    !/*ce        170*/
  280 if(kitm.eq.0.and.hitm.ne.':'.and.hitm.ne.'<'.and.hitm.ne.'['.and.         !/*cexa        1*/
     & hitm.ne.']'.and.hitm.ne.'('.and.hitm.ne.')'.and.hitm.ne.'u'.and.         !/*cexa        2*/
     & hitm.ne.'='.and.(nwc.ne.1.or.                                            !/*cexa        3*/
     & ich.eq.'si'.and.index('h l a s f ',hitm(1:2)).eq.0.or.                   !/*ce        173*/
     & ich.eq.'ds'.and.index('h l a s q t ',hitm(1:2)).eq.0))go to 9010         !/*ce        174*/
      if(hitm.eq.'a'.and.ksd(lksd+19,m1c).lt.0)                                 !/*ce        175*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*ce        176*/
     & '55hthe a here is inconsistent with the c on sp or sb card.')            !/*ce        177*/
      return                                                                    !/*ce        178*/
!                                                                               !/*ce        179*/
! >>>>>  source distributions                                      sp,sb        !/*ce        180*/
!        m1c=distribution index.                                                !/*ce        181*/
  290 if(kitm.eq.0.and.(nwc.ne.1.or.index('d c ',hitm(1:2)).eq.0.and.           !/*ce        182*/
     & (ich.ne.'sp'.or.hitm.ne.'v')))go to 9010                                 !/*ce        183*/
      j=ichar('p')+ichar('b')-ichar(ich(2:2))                                   !/*ce        184*/
      if(ksd(lksd+20,m1c).ne.0.and.nwc.gt.ksd(lksd+20,m1c))                     !/*ce        185*/
     & call erprnt(2,1,0,0,0,0,0,1,                                             !/*ce        186*/
     & '44hdifferent number of entries than on s'//char(j)//' card.')           !/*ce        187*/
      if(hitm.eq.'c'.and.ksd(lksd+19,m1c).gt.0)                                 !/*ce        188*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*ce        189*/
     & '53hthe c here is inconsistent with the a on the si card.')              !/*ce        190*/
      if(nwc.ne.1.or.ritm.ge.0.)return                                          !/*ce        191*/
      if(iitm.eq.-1)call erprnt(2,2,0,0,0,0,0,0,                                !/*ce        192*/
     & '57henergy function -1 will be obsolete soon.  use -3 and -4.')          !/*ce        193*/
      if(iitm.ne.ksd(lksd+2,m1c).and.ksd(lksd+2,m1c).ne.0)                      !/*ce        194*/
     & call erprnt(2,1,2,icn,icn,0,0,0,                                         !/*ce        195*/
     & '25hdifferent functions on sp,i2,7h and sb,i2,7h cards.')                !/*ce        196*/
      if(ich.eq.'sb'.and.iitm.gt.-21)call erprnt(2,1,0,0,0,0,0,0,               !/*ce        197*/
     & '35hillegal function number on sb card.')                                !/*ce        198*/
      do 300 j=1,mjsf                                                           !/*ce        199*/
  300 if(iitm.eq.-jsf(j))return                                                 !/*ce        200*/
      go to 9010                                                                !/*ce        201*/
!                                                                               !/*ce        202*/
! >>>>>  source definition                                          sdef        !/*ce        203*/
!        m1c=nwc of variable name.                                              !/*ce        204*/
!        m2c=index of current variable.                                         !/*ce        205*/
!        m3c=index of depended-on variable or location of n in kcp.             !/*ce        206*/
!        m4c=nwc of last colon.                                                 !/*ce        207*/
  310 if(hitm.eq.'='.or.hitm.eq.':'.or.hitm.eq.'('.or.hitm.eq.')')return        !/*ce        208*/
      if(m4c.ne.0.and.m4c.eq.nwc-2)go to 320                                    !/*ce        209*/
      go to(320,340,370,390)min(4,nwc-m1c+1)                                    !/*ce        210*/
  320 do 330 j=1,maxv                                                           !/*ce        211*/
  330 if(hitm(1:3).eq.hbln(j,1))return                                          !/*ce4c        5*/
      go to 9010                                                                !/*ce        213*/
  340 if(kitm.ne.0)go to 390                                                    !/*ce        214*/
      if(m2c.eq.19)then                                                         !/*cex        11*/
         if(index(hip,hitm(1:1)).eq.0)call erprnt(2,1,0,0,0,0,0,0,              !/*cex        12*/
     &   '30hinvalid source particle type '//hitm(1:1))                         !/*cex        13*/
         return                                                                 !/*cex        14*/
      endif                                                                     !/*cex        15*/
      do 350 k=1,maxv                                                           !/*cex        16*/
  350 if(hitm(1:4).eq.'f'//hbln(k,1))go to 360                                  !/*ce4c        6*/
      go to 380                                                                 !/*ce        217*/
  360 if(ivdis(k).eq.0)call erprnt(2,1,0,0,0,0,0,1,'45h'//hbln(m2c,1)//         !/*ce        218*/
     & ' depends on '//hbln(k,1)//' which has no distribution.')                !/*ce        219*/
      if(ivdis(k).ge.100000)call erprnt(2,1,0,0,0,0,0,1,'52h'//                 !/*ce4a       15*/
     & hbln(m2c,1)//' depends on cel which lacks a valid distribution.')        !/*ce4a       16*/
      return                                                                    !/*ce        220*/
  370 if(m3c.eq.0)go to 390                                                     !/*ce        221*/
  380 if(nitm.eq.1)go to 9010                                                   !/*ce        222*/
      if(hitm(1:1).ne.'d'.or.kdata(hitm(2:nitm)).ne.2)go to 9010                !/*ce        223*/
      if(m2c.ge.17.and.m2c.le.19)call erprnt(2,1,0,0,0,0,0,0,                   !/*cex        17*/
     & '45hvariable '//hbln(m2c,1)//' can take only an explicit value.')        !/*ce        225*/
      return                                                                    !/*ce        226*/
  390 if(kitm.eq.0.and.(hitm(1:1).ne.'d'.or.m2c.ne.1))go to 9010                !/*ce        227*/
      if(m2c.eq.10.or.m2c.eq.11)call erprnt(2,2,0,0,0,0,0,0,                    !/*ce        228*/
     & '52h'//hbln(m2c,1)//                                                     !/*ce        229*/
     & ' is constant.  in most problems it is a variable.')                     !/*ce        230*/
      if(m2c.gt.2.or.iitm.eq.0)return                                           !/*ce        231*/
      i=namchg(m2c,abs(iitm))                                                   !/*cex        18*/
      if(m2c.eq.2.and.kitm.eq.1)i=namchg(m2c,int(abs(ritm)))                    !/*cex        19*/
      if(i.le.0)call erprnt(2,1,1,abs(iitm),0,0,0,0,                            !/*cex        20*/
     &  '14hsource '//hcs(m2c)//',i6,16h is not defined.')                      !/*ce4a       17*/
      if((junf.eq.0.or.m2c.eq.2).and.iitm.lt.0)go to 9010                       !/*ce4c        7*/
      return                                                                    !/*ce        234*/
!                                                                               !/*ce        235*/
! >>>>>  tally f cards                             f,fx,fy,fz,pi,tir,tic        !/*cex        21*/
!        m1c=indicator of incorrect use of parentheses.                         !/*ce        237*/
!        m2c=current bin number.                                                !/*ce4a       18*/
!        m3c=number of entries within [].                                       !/*ce4a       19*/
!        m4c=1 for error, =2-5 for number of colons within [].                  !/*ce4a       20*/
!        m5c=cell number of previous entry - negative for u input.              !/*ce4a       21*/
!        m6c=number of levels within current bin.                               !/*ce4a       22*/
!        m7c=number of entries in current level.                                !/*ce4a       23*/
!        m8c=flag for colon input error.                                        !/*ce4a       24*/
  400 if(mod(icn,10).eq.5)go to 435                                             !/*ce4a       25*/
      go to(402,416,417)kitm+1                                                  !/*ce4a       26*/
  402 k=index('(  )  :  <  [  ]  ,  u  =  t  nt ',hitm(1:3))                    !/*ce4a       27*/
      if(k.eq.0)go to 9010                                                      !/*ce4a       28*/
      if(k.gt.3*9)go to 434                                                     !/*ce4a       29*/
      if(k.gt.3*2.and.m4c.eq.1)return                                           !/*ce4a       30*/
      go to(404,406,408,410,411,412,403,414,415)1+(k-1)/3                       !/*ce4a       31*/
  403 return                                                                    !/*ce4a       32*/
  404 if(m4c.gt.1)go to 420                                                     !/*ce4a       33*/
      m1c=m1c+1                                                                 !/*ce4a       34*/
      if(m1c.gt.2)go to 432                                                     !/*ce4a       35*/
      if(m1c.gt.1)return                                                        !/*ce4a       36*/
      m2c=m2c+1                                                                 !/*ce4a       37*/
      m4c=0                                                                     !/*ce4a       38*/
      return                                                                    !/*ce4a       39*/
  406 if(m4c.gt.1)go to 420                                                     !/*ce4a       40*/
      m1c=m1c-1                                                                 !/*ce4a       41*/
      if(m1c.lt.0)go to 432                                                     !/*ce4a       42*/
      if(m1c.gt.0)return                                                        !/*ce4a       43*/
      if(m7c.eq.0)go to 428                                                     !/*ce4a       44*/
      m5c=0                                                                     !/*ce4a       45*/
      m6c=0                                                                     !/*ce4a       46*/
      m7c=0                                                                     !/*ce4a       47*/
      return                                                                    !/*ce4a       48*/
  408 if(m4c.eq.0)go to 421                                                     !/*ce4a       49*/
      if(m4c.gt.4.or.m8c.eq.m3c)go to 422                                       !/*ce4a       50*/
      m4c=m4c+1                                                                 !/*ce4a       51*/
      m8c=m3c                                                                   !/*ce4a       52*/
      return                                                                    !/*ce4a       53*/
  410 if(m4c.gt.1)go to 420                                                     !/*ce4a       54*/
      if(m1c.ne.1)go to 432                                                     !/*ce4a       55*/
      if(m7c.eq.0)go to 428                                                     !/*ce4a       56*/
      m6c=m6c+1                                                                 !/*ce4a       57*/
      if(m6c.gt.mxlv)go to 433                                                  !/*ce4a       58*/
      m7c=0                                                                     !/*ce4a       59*/
      return                                                                    !/*ce4a       60*/
  411 if(m4c.gt.1)go to 420                                                     !/*ce4a       61*/
      if(m6c.eq.0)go to 423                                                     !/*ce4a       62*/
      if(m5c.eq.0)go to 424                                                     !/*ce4a       63*/
      if(junf.eq.0)go to 424                                                    !/*ce4a       64*/
      if(lat(llat+1,m5c).eq.0)go to 424                                         !/*ce4a       65*/
      m3c=0                                                                     !/*ce4a       66*/
      m4c=2                                                                     !/*ce4a       67*/
      m8c=0                                                                     !/*ce4a       68*/
      return                                                                    !/*ce4a       69*/
  412 if(m4c.eq.0)go to 430                                                     !/*ce4a       70*/
      if(m4c.eq.1)go to 413                                                     !/*ce4a       71*/
      if(m3c.eq.0)go to 422                                                     !/*ce4a       72*/
      if(m4c.eq.2.and.m3c.gt.1.and.mod(m3c,3).ne.0)go to 422                    !/*ce4a       73*/
      if(m4c.gt.2.and.m3c.ne.m4c+1)go to 422                                    !/*ce4a       74*/
  413 m4c=0                                                                     !/*ce4a       75*/
      return                                                                    !/*ce4a       76*/
  414 m5c=-1                                                                    !/*ce4a       77*/
      return                                                                    !/*ce4a       78*/
  415 if(m5c.ge.0)go to 429                                                     !/*ce4a       79*/
      return                                                                    !/*ce4a       80*/
  416 iitm=ritm                                                                 !/*ce4c        8*/
      if(mod(icn,10).eq.1.or.mod(icn,10).eq.2)go to 417                         !/*ce4c        9*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ce4c       10*/
     & '29hf card data must be integers.')                                      !/*ce4a       82*/
      return                                                                    !/*ce4a       83*/
  417 if(m4c.gt.1)m3c=m3c+1                                                     !/*ce4a       84*/
      if(m4c.gt.1)return                                                        !/*ce4a       85*/
      if(m1c.eq.0)m2c=m2c+1                                                     !/*ce4a       86*/
      if(m1c.eq.0)m4c=0                                                         !/*ce4a       87*/
      if(m4c.eq.1)return                                                        !/*ce4a       88*/
      if(m5c.lt.0)go to 419                                                     !/*ce4a       89*/
      m5c=0                                                                     !/*ce4a       90*/
      if(mod(icn,10).lt.3.and.m6c.eq.0)go to 418                                !/*ce4a       91*/
      k=namchg(1,iitm)                                                          !/*ce4a       92*/
      if(k.eq.0)go to 425                                                       !/*ce4a       93*/
      if(m6c.gt.0.and.junf.eq.0)go to 426                                       !/*ce4a       94*/
      if(m6c.gt.0.and.mfl(lmfl+1,k)+lat(llat+1,k).eq.0)go to 426                !/*ce4a       95*/
      m5c=k                                                                     !/*ce4a       96*/
  418 m7c=m7c+1                                                                 !/*ce4c       11*/
      return                                                                    !/*ce4a      102*/
  419 if(mod(icn,10).lt.3.and.m6c.eq.0)go to 429                                !/*ce4a      103*/
      m5c=0                                                                     !/*ce4a      104*/
      m7c=m7c+1                                                                 !/*ce4a      105*/
      return                                                                    !/*ce4a      106*/
!                                                                               !/*ce4a      107*/
!        common complaints - fatal.                                             !/*ce4a      108*/
  420 call erprnt(2,1,2,m2c,icn,0,0,0,                                          !/*ce4a      109*/
     & '38hillegal character in [] of f card bin ,i4,7h tally ,i4,1h.')         !/*ce4a      110*/
      go to 431                                                                 !/*ce4a      111*/
  421 call erprnt(2,1,2,m2c,icn,0,0,0,                                          !/*ce4a      112*/
     & '32hinvalid colon use in f card bin ,i4,7h tally ,i4,1h.')               !/*ce4a      113*/
      go to 431                                                                 !/*ce4a      114*/
  422 call erprnt(2,1,2,m2c,icn,0,0,0,                                          !/*ce4a      115*/
     & '32hinput error in [] of f card bin ,i4,7h tally ,i4,1h.')               !/*ce4a      116*/
      go to 431                                                                 !/*ce4a      117*/
  423 call erprnt(2,1,2,m2c,icn,0,0,0,                                          !/*ce4a      118*/
     & '38hillegal [] in 1st level of f card bin ,i3,7h tally ,i5,1h.')         !/*ce4a      119*/
      go to 431                                                                 !/*ce4a      120*/
  424 call erprnt(2,1,2,m2c,icn,0,0,0,                                          !/*ce4a      121*/
     & '35hinvalid lattice cell in f card bin ,i4,7h tally ,i4,1h.')            !/*ce4a      122*/
      go to 431                                                                 !/*ce4a      123*/
  425 call erprnt(2,1,3,iitm,m2c,icn,0,0,                                       !/*ce4a      124*/
     & '12hf card cell ,i5,8h of bin ,i3,7h tally ,i5,11h not found.')          !/*ce4a      125*/
      go to 431                                                                 !/*ce4a      126*/
  426 call erprnt(2,1,3,iitm,m2c,icn,0,0,                                       !/*ce4a      127*/
     & '12hf card cell ,i5,8h of bin ,i3,7h tally ,i5,12h not filled.')         !/*ce4a      128*/
      go to 431                                                                 !/*ce4a      129*/
  428 call erprnt(2,1,3,m6c+1,m2c,icn,0,0,                                      !/*ce4a      133*/
     & '6hlevel ,i3,15h of f card bin ,i4,7h tally ,i4,10h is empty.')          !/*ce4a      134*/
      go to 431                                                                 !/*ce4a      135*/
  429 call erprnt(2,1,2,m2c,icn,0,0,0,                                          !/*ce4a      136*/
     & '38hinvalid universe format in f card bin ,i4,7h tally ,i4,1h.')         !/*ce4a      137*/
      go to 431                                                                 !/*ce4a      138*/
  430 call erprnt(2,1,2,m2c,icn,0,0,0,                                          !/*ce4a      139*/
     & '37hproblem with use of [] in f card bin ,i4,7h tally ,i4,1h.')          !/*ce4a      140*/
  431 m4c=1                                                                     !/*ce4a      141*/
      return                                                                    !/*ce4a      142*/
  432 call erprnt(2,1,2,m2c,icn,0,0,1,                                          !/*ce4a      143*/
     & '32hparentheses error on f card bin ,i4,7h tally ,i4,1h.')               !/*ce4a      144*/
      return                                                                    !/*ce4a      145*/
  433 call erprnt(2,1,2,m2c,icn,0,0,1,                                          !/*ce4a      146*/
     & '38hmaximum levels exceeded in f card bin ,i4,7h tally ,i4,1h.')         !/*ce4a      147*/
      return                                                                    !/*ce4a      148*/
  434 if(m1c.ne.0)call erprnt(2,1,2,m2c,icn,0,0,1,                              !/*ce4a      149*/
     & '38hillegal entry within () of f card bin ,i4,7h tally ,i4,1h.')         !/*ce4a      150*/
      return                                                                    !/*ce4a      151*/
  435 if(kitm.eq.0.and.hitm.ne.'nd')go to 9010                                  !/*ce4a      152*/
      return                                                                    !/*ce        261*/
!                                                                               !/*ce        262*/
! >>>>>  tally special treatments                                     ft        !/*ce        263*/
  440 if(kitm.ne.0)return                                                       !/*ce        264*/
      do 450 i=1,mkft                                                           !/*ce        265*/
  450 if(hitm.eq.hft(i))return                                                  !/*ce        266*/
      go to 9010                                                                !/*ce        267*/
!                                                                               !/*ce        268*/
! >>>>>  energy, time, or user bins                               e,t,fu        !/*ce        269*/
  460 if(kitm.eq.0.and.hitm.ne.'t'.and.hitm.ne.'nt'.and.hitm.ne.'c')            !/*ce4c       12*/
     & go to 9010                                                               !/*ce4c       13*/
      return                                                                    !/*ce        271*/
!                                                                               !/*ce        272*/
! >>>>>  cosine bins                                                   c        !/*ce        273*/
  470 if(kitm.eq.0.and.hitm.ne.'t'.and.hitm.ne.'nt'.and.hitm.ne.'c')            !/*ce4c       14*/
     & go to 9010                                                               !/*ce4c       15*/
      if(icx.eq.1)return                                                        !/*cexa        4*/
#ifdef RADIOG                                                                   /*!cex        22*/
      if(mod(icn,10).eq.5)return                                                !/*cex        23*/
#endif                                                                          /*!cex        24*/
      if(kitm.ne.0.and.abs(ritm).gt.1.)call erprnt(2,1,0,0,0,0,0,1,             !/*ce        275*/
     & '47hcosine bins must have values between 1. and -1.')                    !/*ce        276*/
      return                                                                    !/*ce        277*/
!                                                                               !/*ce        278*/
! >>>>>  order of tally printing                                      fq        !/*ce        279*/
  480 k=index(' f d u s m c e t ',hitm(1:2))/2                                  !/*ce        280*/
      if(k.eq.0)call erprnt(2,1,0,0,0,0,0,1,                                    !/*ce        281*/
     & '55hallowed values on fq card are f, d, u, s, m, c, e or t.')            !/*ce        282*/
      if(mod(m2c/2**k,2).ne.0)call erprnt(2,1,0,0,0,0,0,1,                      !/*ce        283*/
     & '18hduplicate entries.')                                                 !/*ce        284*/
      m2c=m2c+2**k                                                              !/*ce        285*/
      return                                                                    !/*ce        286*/
!                                                                               !/*ce        287*/
! >>>>>  multiplier bins                                              fm        !/*ce        288*/
!        m1c=indicator of incorrect use of parentheses.                         !/*ce        289*/
!        m2c=index of next material number.                                     !/*ce        290*/
  490 k=index('(  )  :  t  nt c  ',hitm(1:3))                                   !/*ce4c       16*/
      if(kitm.ne.0)go to 500                                                    !/*ce        292*/
      if(k.eq.0)go to 9010                                                      !/*ce        293*/
      if(k.eq.1.or.k.eq.4)m1c=m1c+1-k/2                                         !/*ce        294*/
      if(m1c.lt.0.or.m1c.gt.3)m1c=10000                                         !/*ce        295*/
  500 if(k.eq.1.and.m2c.eq.nwc+1)m2c=0                                          !/*ce        296*/
      if(k.eq.1.and.m2c.ne.0)m2c=-m2c                                           !/*ce        297*/
      if(k.eq.1.and.m2c.eq.0)m2c=nwc+2                                          !/*ce        298*/
      if(nwc.eq.1.and.k.ne.1)m2c=2                                              !/*ce        299*/
      if(k.eq.4.and.m2c.gt.0)m2c=0                                              !/*ce        300*/
      if(k.eq.4.and.m2c.lt.0)m2c=-m2c                                           !/*ce        301*/
      if(nwc.ne.m2c.and.nwc+100000.ne.m2c.or.k.ne.0.or.                         !/*ce        302*/
     & nwc.eq.2.and.iitm.eq.0)return                                            !/*ce        303*/
      if(iitm.ge.0.or.m2c.gt.100000)go to 510                                   !/*ce        304*/
      m2c=m2c+100001                                                            !/*ce        305*/
      return                                                                    !/*ce        306*/
  510 if(m2c.gt.100000)m2c=m2c+2                                                !/*ce        307*/
      do 520 i=1,nmat1                                                          !/*ce        308*/
  520 if(nmt(lnmt+i).eq.iitm.and.iitm.gt.0)return                               !/*ce        309*/
      call erprnt(2,1,1,iitm,0,0,0,0,'26hno m card for material no.,i7')        !/*ce        310*/
      return                                                                    !/*ce        311*/
!                                                                               !/*ce        312*/
! >>>>>  dose function                                         de and df        !/*ce        313*/
  530 if(kitm.ne.0)then                                                         !/*cexc        3*/
         if(ritm.ge.zero)return                                                 !/*cexc        4*/
         if(nwc.eq.1.or.ich.eq.'de')go to 9010                                  !/*cexc        5*/
         if(rtp(lrtp+ipl+nwc-1).gt.-8.)go to 9010                               !/*cexc        6*/
         return                                                                 !/*cexc        7*/
      else                                                                      !/*cexc        8*/
         if(hitm.eq.'='.or.hitm.eq.'log'.or.hitm.eq.'lin')return                !/*cexc        9*/
         if(ich.eq.'de')return                                                  !/*cexc       10*/
         if(hitm.ne.'ic'.and.hitm.ne.'iu'.and.hitm.ne.'fac'.and.                !/*cexc       11*/
     &   hitm.ne.'acr')go to 9010                                               !/*cexc       12*/
      endif                                                                     !/*cexc       13*/
      return                                                                    !/*ce        316*/
!                                                                               !/*ce        317*/
! >>>>>  flagging cells                                               cf        !/*ce        318*/
  540 if(namchg(1,iitm).eq.0)call erprnt(2,1,1,iitm,0,0,0,0,                    !/*ce        319*/
     & '15hspecified cell,,i5,17h, is not defined.')                            !/*ce        320*/
      return                                                                    !/*ce        321*/
!                                                                               !/*ce        322*/
! >>>>>  segment bins                                                 fs        !/*ce        323*/
  550 if(kitm.eq.0.and.hitm.ne.'t'.and.hitm.ne.'nt'.and.hitm.ne.'c')            !/*ce4c       17*/
     & go to 9010                                                               !/*ce4c       18*/
      return                                                                    !/*ce        329*/
!                                                                               !/*ce        330*/
! >>>>>  segment divisors                                             sd        !/*ce        331*/
  560 if(kitm.eq.0.and.hitm.ne.'('.and.hitm.ne.')')go to 9010                   !/*ce        332*/
      return                                                                    !/*ce        333*/
!                                                                               !/*ce        334*/
! >>>>>  detector and dxtran diagnostics                              dd        !/*ce        335*/
  570 if(mod(nwc,2).eq.1.and.ritm.gt.1.)                                        !/*ce        336*/
     & call erprnt(2,1,0,0,0,0,0,0,'6hk > 1.')                                  !/*ce        337*/
      return                                                                    !/*ce        338*/
!                                                                               !/*ce        339*/
! >>>>>  material specifications                                       m        !/*ce        340*/
!        m1c > 0, already processing material option m1c.                       !/*ce4c2       5*/
!        check for valid material option name or nuclide zaid.                  !/*ce4c2       6*/
  580 if(mod(nwc,2).eq.0)go to 630                                              !/*ce        342*/
      if(hitm.eq.'=')return                                                     !/*ce4a      154*/
      i=index(hitm,'.')                                                         !/*ce4a      155*/
      mc=0                                                                      !/*ce4c2       7*/
      do 582 m=1,mopts                                                          !/*ce4c2       8*/
  582 if(hitm.eq.hmopt(m))mc=m                                                  !/*ce4c2       9*/
      if(mc.ne.0)return                                                         !/*ce4c2      10*/
      j=0                                                                       !/*ce4c2      11*/
      if(i.gt.4)j=kdata(hitm(1:i-1))                                            !/*ce4c2      12*/
      if(mc.eq.0.and.m1c.eq.0.and.j.eq.0.and.kitm.eq.0)go to 9010               !/*ce4c2      13*/
!        check for valid material option value.                                 !/*ce4c2      14*/
      if(m1c.eq.0)go to 584                                                     !/*ce4c2      15*/
      if(m1c.le.3.and.kitm.eq.0)go to 9010                                      !/*ce4c2      16*/
      if(m1c.le.3)return                                                        !/*ce4c2      17*/
      if(i.gt.1.or.nitm-i.gt.3)go to 9010                                       !/*ce4a      159*/
      if(m1c.eq.4.and.index(' cdym',hitm(i+3:i+3)).eq.0)                        !/*ce4c2      18*/
     & call erprnt(2,1,0,0,0,0,0,1,                                             !/*ce4c2      19*/
     & '49hdefault neutron table set to wrong particle type.')                  !/*ce4c2      20*/
      if(m1c.eq.5.and.index(' pg',hitm(i+3:i+3)).eq.0)                          !/*ce4c2      21*/
     & call erprnt(2,1,0,0,0,0,0,1,                                             !/*ce4c2      22*/
     & '53hdefault photoatomic table set to wrong particle type.')              !/*ce4c2      23*/
      if(m1c.eq.6.and.index(' e',hitm(i+3:i+3)).eq.0)                           !/*ce4c2      24*/
     & call erprnt(2,1,0,0,0,0,0,1,                                             !/*ce4c2      25*/
     & '50hdefault electron table set to wrong particle type.')                 !/*ce4c2      26*/
      if(m1c.eq.7.and.index(' u',hitm(i+3:i+3)).eq.0)                           !/*ce4c2      27*/
     & call erprnt(2,1,0,0,0,0,0,1,                                             !/*ce4c2      28*/
     & '54hdefault photonuclear table set to wrong particle type.')             !/*ce4c2      29*/
      if(m1c.eq.12.and.index(' h',hitm(i+3:i+3)).eq.0)                          !/*cex        25*/
     &   call erprnt(2,1,0,0,0,0,0,1,                                           !/*cex        26*/
     &   '48hdefault proton table set to wrong particle type.')                 !/*cex        27*/
      return                                                                    !/*ce4a      164*/
!        check for valid nuclide zaid.                                          !/*ce4c2      30*/
  584 if(i.eq.0.or.nitm.eq.i)return                                             !/*ce4c2      31*/
      do 590 j=i+1,nitm                                                         !/*ce        346*/
  590 if(hitm(j:j).ne.'0')go to 600                                             !/*ce        347*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*ce        348*/
     & '59hzeros after the period will be dropped from '//hitm(1:15))           !/*ce        349*/
      return                                                                    !/*ce4a      166*/
  600 if(kpt(1).eq.0.and.hitm(i+3:i+3).eq.' ')call erprnt(2,2,0,0,0,0,0,        !/*ce4a      167*/
     & 0,'53h1- or 2-character identifiers request neutron tables.')            !/*ce4a      168*/
      if(kpt(1).eq.0.and.index('cdym',hitm(i+3:i+3)).ne.0)                      !/*ce4a      169*/
     & call erprnt(2,2,0,0,0,0,0,0,                                             !/*ce4a      170*/
     & '53hneutron table inconsistent with mode will be ignored.')              !/*ce4a      171*/
      if(kpt(2).eq.0.and.index('pgu',hitm(i+1:i+1)).ne.0)                       !/*ce4c2      32*/
     & call erprnt(2,2,0,0,0,0,0,0,                                             !/*ce4a      173*/
     & '52hphoton table inconsistent with mode will be ignored.')               !/*ce4a      174*/
      if(kpt(3).eq.0.and.(kpt(2).eq.0.or.ides.ne.0).and.                        !/*ce4a      175*/
     & hitm(i+3:i+3).eq.'e')call erprnt(2,2,0,0,0,0,0,0,                        !/*ce4a      176*/
     & '54helectron table inconsistent with mode will be ignored.')             !/*ce4a      177*/
      if(kpt(9).eq.0.and.index('h',hitm(i+1:i+1)).ne.0)                         !/*cex        28*/
     & call erprnt(2,2,0,0,0,0,0,0,                                             !/*cex        29*/
     & '52hproton table inconsistent with mode will be ignored.')               !/*cex        30*/
!        ensure that dosimetry data not used for transport.                     !/*ce4c2      33*/
      if(hitm(i+3:i+3).ne.'y')return                                            !/*ce4a      178*/
      do 610 i=1,mxa                                                            !/*ce        353*/
  610 if(mat(lmat+i).eq.icn)go to 620                                           !/*ce        354*/
      return                                                                    !/*ce        355*/
  620 call erprnt(2,1,1,ncl(lncl+i),0,0,0,1,                                    !/*ce        356*/
     & '45hdosimetry table specified in material of cell,i5')                   !/*ce        357*/
      return                                                                    !/*ce        358*/
!        check nuclide fractions.                                               !/*ce4c2      34*/
  630 if(ritm.eq.0.)call erprnt(2,2,1,iza(liza+mix),0,0,0,0,                    !/*ce        359*/
     & '21hthe fraction for za =,i7,9h is zero.')                               !/*ce        360*/
      if(mix.eq.jmd(ljmd+nmat))return                                           !/*ce        361*/
      if(ritm*fme(lfme+mix-1).lt.0.)call erprnt                                 !/*ce        362*/
     & (2,1,0,0,0,0,0,1,'32hmixed atom and weight fractions.')                  !/*ce        363*/
      return                                                                    !/*ce        364*/
!                                                                               !/*ce        365*/
! >>>>>  total or prompt nubar                                     totnu        !/*ce        366*/
  640 if(hitm.ne.'no')go to 9010                                                !/*ce        367*/
      return                                                                    !/*ce        368*/
!                                                                               !/*ce        369*/
! >>>>>  atomic weights                                            awtab        !/*ce        370*/
  650 if(mod(nwc,2).eq.0.and.ritm.eq.0.)go to 9010                              !/*ce        371*/
      return                                                                    !/*ce        372*/
!                                                                               !/*ce        373*/
! >>>>>  cross-section directory information                          xs        !/*ce        374*/
!        m1c=last character position used so far in ixc.                        !/*ce        375*/
  660 if(nwc.ne.1)go to 690                                                     !/*ce        376*/
      if(nxsc.eq.mxe1)call erprnt(2,1,0,0,0,0,0,1,                              !/*ce4b        3*/
     & '28hmore xs cards than nuclides.')                                       !/*ce4b        4*/
      i=index(hitm,'.')                                                         !/*ce        377*/
      if(i.eq.0)call erprnt(2,1,0,0,0,0,0,1,                                    !/*ce        378*/
     & '50hdecimal point missing in '//hitm(1:25))                              !/*ce        379*/
      if(i.eq.0)return                                                          !/*ce        380*/
      ht=' '                                                                    !/*ce        381*/
      ht(8-i:10)=hitm                                                           !/*ce        382*/
      if(index(htn,ht(10:10)).eq.0)call erprnt(2,1,0,0,0,0,0,1,                 !/*ce4c2      35*/
     & '46hzaid must end in c, d, y, t, p, m, g, e, or u.')                     !/*ce4c2      36*/
      do 670 j=1,nxsc                                                           !/*ce        385*/
      call zaid(2,hs,ixc(lixc+1,j))                                             !/*ce        386*/
  670 if(ht.eq.hs)go to 680                                                     !/*ce        387*/
      return                                                                    !/*ce        388*/
  680 call erprnt(2,1,1,ixc(lixc+61,j),0,0,0,1,                                 !/*ce        389*/
     & '18hsame zaid as on xs,i3')                                              !/*ce        390*/
      return                                                                    !/*ce        391*/
  690 if(nwc.le.11.and.m1c+nitm+1.gt.181)call erprnt(2,1,0,0,0,0,0,1,           !/*ce4c       19*/
     & '35htoo much data.  ixc array overflow.')                                !/*ce        393*/
      if(hitm.eq.'ptable')return                                                !/*ce4c       20*/
      if(nwc.ge.5.and.kitm.eq.0)go to 9010                                      !/*ce        394*/
      if(nwc.ne.5.or.iitm.eq.1.or.iitm.eq.2)return                              !/*ce        395*/
      go to 9010                                                                !/*ce        398*/
!                                                                               !/*ce        399*/
! >>>>>  void cells                                                 void        !/*ce        400*/
  700 if(namchg(1,iitm).eq.0)go to 9010                                         !/*ce        401*/
      return                                                                    !/*ce        402*/
!                                                                               !/*ce        403*/
! >>>>>  physics parameters                                         phys        !/*ce        404*/
  710 if(nqp(1).eq.0)go to 720                                                  !/*ce        405*/
      if(nwc.eq.1.and.ritm.le.0.)call erprnt(2,1,0,0,0,0,0,0,                   !/*ce        406*/
     & '12hemax .le. 0.')                                                       !/*ce        407*/
      if(nwc.eq.1.and.ritm.lt.ecf(1))call erprnt(2,1,0,0,0,0,0,0,               !/*ce        408*/
     & '40hemax is less than neutron energy cutoff.')                           !/*ce        409*/
      if(nwc.eq.2.and.ritm.lt.0.)call erprnt(2,1,0,0,0,0,0,0,                   !/*ce        410*/
     & '28hneutron emcf less than zero.')                                       !/*ce        411*/
      if(nwc.eq.3.and.iitm.ne.0)call erprnt(2,2,0,0,0,0,0,0,                    !/*ce4c       21*/
     & '51hunresolved resonance probability tables turned off.')                !/*ce4c       22*/
      if(nwc.eq.3.and.ritm.ge.10.)call erprnt(2,2,0,0,0,0,0,0,                  !/*cex        33*/
     & '47hinvalid 3rd entry (iunr). using as 5th (cut_n).')                    !/*cex        34*/
      if(nwc.eq.4.and.nsr.eq.71.and.ritm.gt.0.)call erprnt(2,2,0,0,0,           !/*ce4c       23*/
     & 0,0,0,'44hdelayed neutron biasing disallowed in kcode.')                 !/*ce4c       24*/
      if(nwc.eq.5.and.ritm.le.0.)call erprnt(2,1,0,0,0,0,0,0,                   !/*cex        35*/
     & '26hLCS neutron cutoff .le. 0.')                                         !/*cex        36*/
      if(nwc.eq.7.and.(ritm.lt.zero.or.ritm.gt.1.01))go to 9010                 !/*cexk        1*/
  720 if(nqp(2).eq.0)go to 725                                                  !/*ce4a      179*/
      if(dbcn(26).ne.0.)then                                                    !/*cex        37*/
      if(nwc.eq.1.and.ritm.lt.ecut_min(2))call erprnt(2,2,0,0,0,0,0,0,          !/*cex        38*/
     & '36hphoton emcf will be set to .001 mev.')                               !/*cex        39*/
      else                                                                      !/*cex        40*/
      if(nwc.eq.1.and.ritm.lt.ecut_min(2))call erprnt(2,2,0,0,0,0,0,0,          !/*cex        41*/
     & '39hphoton emcf will be set to ecut_min(2).')                            !/*cex        42*/
      endif                                                                     !/*cex        43*/
      if(nwc.eq.4.and.ritm.ne.0..and.kpt(2).eq.0)call erprnt(2,2,0,0,0,         !/*ce4c2      39*/
     & 0,0,0,'50hphotonuclear turned off. photons not on mode card.')           !/*ce4c2      40*/
  725 if(nqp(3).eq.0)go to 728                                                  !/*cexk        2*/
      if(nwc.eq.1.and.ritm.gt.emax_top)call erprnt(2,2,0,0,0,0,0,0,             !/*cex        44*/
     & '37helectron emx .gt. parameter emax_top.')                              !/*cex        45*/
      if(bnum.ne.0..and.numb.ne.0)call erprnt(1,1,0,0,0,0,0,0,                  !/*ce4c       25*/
     & '39honly one kind of brems biasing allowed.')                            !/*ce4c       26*/
  728 if(nqp(9).eq.0)return                                                     !/*cexk        3*/
      if(nwc.eq.7.and.(ritm.lt.zero.or.ritm.gt.1.01))go to 9010                 !/*cexk        4*/
      return                                                                    !/*ce        415*/
!                                                                               !/*ce        416*/
! >>>>>  cutoffs                                                     cut        !/*ce        417*/
  730 if(nwc.ne.2.or.nqp(1).eq.0)go to 740                                      !/*ce        418*/
      if(ritm.gt.emx(1))call erprnt(2,1,0,0,0,0,0,0,                            !/*ce        419*/
     & '43hneutron energy cutoff is greater than emax.')                        !/*ce        420*/
      if(ritm.ne.0..and.nsr.eq.71)call erprnt(2,2,0,0,0,0,0,0,                  !/*ce        421*/
     & '56hneutron energy cutoff is not zero in this kcode problem.')           !/*ce        422*/
      if(ritm.ne.0..and.kpt(1)*kpt(2).ne.0.and.mcal.lt.2)                       !/*ce        423*/
     & call erprnt(2,2,0,0,0,0,0,0,                                             !/*ce        424*/
     & '56hneutron energy cutoff >0 in this neutron-photon problem.')           !/*ce        425*/
  740 do 745 mp=2,mipt                                                          !/*cex        46*/
  745 if(nqp(mp).ne.0.and.nwc.eq.2.and.ritm.lt..99999*ecut_min(mp))             !/*cex        47*/
     & call erprnt(2,2,1,mp,0,0,0,0,                                            !/*cex        48*/
     & '43h'//hnp(mp)//' ecf will be raised to ecut_min(,i2,2h).')              !/*cex        49*/
      if(nwc.ne.4)return                                                        !/*ce        429*/
      do 750 i=1,mipt                                                           !/*ce        430*/
  750 if(nqp(i).ne.0.and.abs(wc1(i)).lt.abs(ritm))call erprnt                   !/*ce        431*/
     & (2,1,0,0,0,0,0,0,'36h'//hnp(i)//' wc1 is smaller than wc2.')             !/*cex        50*/
!                                                                               !/*ce        433*/
! >>>>>  source particle cutoff number                               nps        !/*ce        434*/
  760 if(nsr.eq.71.and.iitm.gt.0)call erprnt(2,2,0,0,0,0,0,1,                   !/*ce        435*/
     & '42hnps card is ineffective in kcode problems.')                         !/*ce        436*/
      return                                                                    !/*ce        437*/
!                                                                               !/*ce        438*/
! >>>>>  debugging controls                                         dbcn        !/*ce        439*/
  770 if(nwc.eq.1.and.ritm.ne.0..and.ritm-aint(ritm*.5)*2..ne.1.)               !/*ce        440*/
     & call erprnt                                                              !/*ce        441*/
     & (2,1,0,0,0,0,0,0,'39hthe random number is not an odd number.')           !/*ce        442*/
      if(nwc.eq.10.and.ritm.gt.300.)call erprnt                                 !/*ce        443*/
     & (2,1,0,0,0,0,0,0,'33hseconds between interrupts > 300.')                 !/*ce        444*/
      if(ntasks.le.1.or.ritm.eq.0.)return                                       !/*ce        445*/
      if(nwc.eq.2)call erprnt(2,1,0,0,0,0,0,0,                                  !/*ce        446*/
     & '46hdebug print and multitasking are incompatible.')                     !/*ce        447*/
      if(nwc.eq.3.or.nwc.eq.4)call erprnt(2,1,0,0,0,0,0,0,                      !/*ce        448*/
     & '44hevent log and multitasking are incompatible.')                       !/*ce        449*/
      return                                                                    !/*ce        452*/
!                                                                               !/*ce        453*/
! >>>>>  specifications for user files                             files        !/*ce        454*/
  780 j=(nwc+4)/5                                                               !/*ce        455*/
      n=nwc-5*(j-1)                                                             !/*ce        456*/
      if(kitm.ne.2.and.(n.eq.1.or.n.eq.5))call erprnt                           !/*ce        457*/
     & (2,1,0,0,0,0,0,1,'50hentry must be integer:  '//hitm(1:26))              !/*ce        458*/
      if(ics.eq.-1)return                                                       !/*ce        459*/
      go to(850,800,830,840,850)n                                               !/*ce        460*/
      do 790 i=1,j-1                                                            !/*ce        461*/
  790 if(kufil(1,i).eq.iitm)call erprnt(2,1,1,iitm,0,0,0,1,                     !/*ce        462*/
     & '11hunit number,i10,24h appears more than once.')                        !/*ce        463*/
      return                                                                    !/*ce        464*/
  800 do 810 i=1,ndef                                                           !/*ce4c       27*/
  810 if(hitm.eq.isub(i).and.hitm(1:3).ne.'dum')go to 9010                      !/*ce4c       28*/
      do 820 i=1,j-1                                                            !/*ce        467*/
  820 if(ufil(1,i).eq.hitm)call erprnt(2,1,0,0,0,0,0,1,                         !/*ce        468*/
     & '44hfile name '//hitm(1:10)//' appears more than once.')                 !/*ce        469*/
      return                                                                    !/*ce        470*/
  830 if(hitm.ne.hsd(1)(1:nitm).and.hitm.ne.hsd(2)(1:nitm))go to 9010           !/*ce        471*/
      return                                                                    !/*ce        472*/
  840 if(hitm.ne.hfu(1)(1:nitm).and.hitm.ne.hfu(2)(1:nitm))go to 9010           !/*ce        473*/
  850 return                                                                    !/*ce        474*/
!                                                                               !/*ce        475*/
! >>>>>  print control                                             print        !/*ce        476*/
  860 if(iitm.eq.0.or.abs(iitm).gt.mink)go to 9010                              !/*ce        477*/
      return                                                                    !/*ce        478*/
!                                                                               !/*ce4a      185*/
! >>>>>  kcode specifications                                      kcode        !/*ce4a      186*/
  865 if(nwc.eq.4.and.iitm.gt.mrkp)call erprnt(2,2,0,0,0,0,0,0,                 !/*ce4a      187*/
     & '49hkct > mrkp.  some cycle information will be lost.')                  !/*ce4a      188*/
      if(nwc.eq.4.and.kct.gt.mrkp)call erprnt(2,2,2,kct,mrkp,0,0,0,             !/*ce4c       29*/
     & '14htotal cycles =,i4,26h is greater than storage =,i4,1h.')             !/*ce4c       30*/
      if(nwc.ne.9.and.ritm.lt.0.)call erprnt(2,1,1,nwc,0,0,0,0,                 !/*ce4c       31*/
     & '4hthe ,i2,37hth entry on kcode cannot be negative.')                    !/*ce4c       32*/
      return                                                                    !/*ce4a      189*/
!                                                                               !/*ce        479*/
! >>>>>  weight-window generator parameters                          wwg        !/*ce        480*/
  870 if(nwc.ne.1)go to 890                                                     !/*ce        481*/
      do 880 i=1,ntal                                                           !/*ce        482*/
  880 if(jptal(ljpt+1,i).eq.iitm)go to 890                                      !/*ce        483*/
      call erprnt(2,1,1,iitm,0,0,0,0,                                           !/*ce        484*/
     & 'i7,32h is not a tally of this problem.')                                !/*ce        485*/
  890 if(nwc.ne.2)go to 900                                                     !/*ce        486*/
      if(iitm.eq.0)go to 900                                                    !/*ce4c       33*/
      i=namchg(1,iitm)                                                          !/*ce        487*/
      if(i.eq.0)call erprnt(2,1,1,iitm,0,0,0,0,                                 !/*ce        488*/
     & '4hcell,i6,31h is not a cell of this problem.')                          !/*ce        489*/
  900 if(nwc.eq.3.and.ritm.lt.0.)call erprnt(2,1,0,0,0,0,0,0,                   !/*ce        490*/
     & '30hweight multiplier is negative.')                                     !/*ce        491*/
      return                                                                    !/*ce        492*/
!                                                                               !/*ce        493*/
! >>>>>  surface source write information                            ssw        !/*ce        494*/
!        m1c=nwc of variable name.                                              !/*ce        495*/
!        m2c=surface-list index or index of current variable                    !/*ce        496*/
!        m3c=flag for in cells list                                             !/*ce        497*/
!        m4c=cells-list index                                                   !/*ce        498*/
  910 if(m1c.ne.0)go to 920                                                     !/*ce        499*/
      if(kitm.eq.0.and.hitm.ne.'('.and.hitm.ne.')')go to 920                    !/*ce        500*/
      if(hitm.eq.'('.and.m3c.ne.0.or.hitm.eq.')'.and.m3c.eq.0)go to 9010        !/*ce        501*/
      if(kitm.eq.0)return                                                       !/*ce        502*/
      if(kitm.eq.1.and.2-m3c.eq.2)iitm=ritm                                     !/*ce4c2      41*/
      if(namchg(2-m3c,abs(iitm)).eq.0)call erprnt(2,1,1,abs(iitm),              !/*ce        503*/
     & 0,0,0,0,'7h'//hcs(2-m3c)//',i6,16h is not defined.')                     !/*ce        504*/
      return                                                                    !/*ce        505*/
  920 if(hitm.eq.'=')return                                                     !/*ce        506*/
      if(kitm.ne.0.or.nitm.lt.3)go to 940                                       !/*ce4c       34*/
      do 930 i=1,maxw                                                           !/*ce        508*/
  930 if(hitm(1:3).eq.hblw(i))return                                            !/*ce4c       35*/
      go to 9010                                                                !/*ce        510*/
  940 if(m2c.ne.2)go to 950                                                     !/*ce        511*/
      if(nitm.ne.1.or.index(hip,hitm(1:1)).eq.0)go to 9010                      !/*ce4a      190*/
  950 if(m2c.ne.3)return                                                        !/*ce        516*/
      if(namchg(1,iitm).eq.0)call erprnt(2,1,1,iitm,0,0,0,0,                    !/*ce        517*/
     & '4hcell,i6,16h is not defined.')                                         !/*ce        518*/
      return                                                                    !/*ce        519*/
!                                                                               !/*ce        520*/
! >>>>>  surface source read information                             ssr        !/*ce        521*/
!        m1c=nwc of variable name.                                              !/*ce        522*/
!        m2c=index of current variable.                                         !/*ce        523*/
  960 if(hitm.eq.'=')return                                                     !/*ce        524*/
      if(nwc.gt.m1c)go to 980                                                   !/*ce        525*/
      do 970 j=1,maxv                                                           !/*ce        526*/
  970 if(hitm(1:3).eq.hbln(j,2))return                                          !/*ce4c       36*/
      go to 9010                                                                !/*ce        528*/
  980 if(kitm.ne.0)go to 990                                                    !/*ce        529*/
      if(m2c.eq.19)then                                                         !/*cex        51*/
         if(index(hip,hitm(1:1)).eq.0)call erprnt(2,1,0,0,0,0,0,0,              !/*cex        52*/
     &   '29hinvalid source particle type '//hitm(1:1))                         !/*cex        53*/
         return                                                                 !/*cex        54*/
      endif                                                                     !/*cex        55*/
      if(nitm.eq.1)go to 9010                                                   !/*ce        530*/
      if(hitm(1:1).ne.'d'.or.kdata(hitm(2:nitm)).ne.2)go to 9010                !/*ce        531*/
      return                                                                    !/*ce        532*/
  990 if(m2c.ne.3)return                                                        !/*ce        533*/
      if(kitm.eq.1)iitm=ritm                                                    !/*ce4c2      42*/
      if(namchg(2,iitm).eq.0)call erprnt(2,1,1,iitm,0,0,0,0,                    !/*ce        534*/
     & '7hsurface,i6,16h is not defined.')                                      !/*ce        535*/
      return                                                                    !/*ce        536*/
!                                                                               !/*ce        537*/
! >>>>>  cell transformations                                       trcl        !/*ce        538*/
!        m1c=cell index                                                         !/*ce        539*/
!        m2c=nwc of last cell seen, negative if left parens                     !/*ce        540*/
 1000 k=index('()',hitm(1:1))                                                   !/*ce        541*/
      if(k.eq.1.and.m2c.lt.0.or.k.eq.2.and.m2c.ge.0)go to 1020                  !/*ce        542*/
      if(k.eq.0.and.(m2c.lt.0.or.iitm.eq.0).or.k.eq.2)return                    !/*ce        543*/
      m=m1c+nwc-m2c                                                             !/*ce        544*/
      if(m.gt.mxa)call erprnt(2,1,2,m,mxa,0,0,1,                                !/*ce4a      191*/
     & '17htoo many entries:,i5,11h were read,,i5,14h are required.')           !/*ce4a      192*/
      if(m.gt.mxa)return                                                        !/*cor4-1     54*/
      if(ncl(lncl+m).gt.999)call erprnt(2,1,1,ncl(lncl+m),0,0,0,0,              !/*ce        545*/
     & '20hproblem name of cell,i6,8h is >999')                                 !/*ce        546*/
      do 1010 l=abs(lca(llca+m)),abs(lca(llca+m+1))-1                           !/*ce        547*/
 1010 if(lja(llja+l).lt.1000000.and.abs(lja(llja+l)).gt.999)                    !/*cor4-2    107*/
     & call erprnt(2,1,2,abs(lja(llja+l)),ncl(lncl+m),0,0,0,                    !/*cor4-2    108*/
     & '23hproblem name of surface,i6,8h of cell,i6,8h is >999')                !/*ce        551*/
      return                                                                    !/*ce        552*/
 1020 call erprnt(2,1,0,0,0,0,0,1,'29hincorrect use of parentheses.')           !/*ce        553*/
      return                                                                    !/*ce        554*/
!                                                                               !/*ce        555*/
! >>>>>  lattice type                                                lat        !/*ce        556*/
 1030 if(iitm.lt.0.or.iitm.gt.2)call erprnt(2,1,0,0,0,0,0,0,                    !/*ce        557*/
     & '31hallowed values are 0, 1, and 2.')                                    !/*ce        558*/
      if(iitm.eq.0)return                                                       !/*ce        559*/
      if(lca(llca+nwc).lt.0)call erprnt(2,1,1,ncl(lncl+nwc),0,0,0,0,            !/*ce        560*/
     & '35hlattice cells must be simple cells.,i7,8h is not.')                  !/*ce        561*/
      n=abs(lca(llca+nwc+1))-lca(llca+nwc)                                      !/*ce        562*/
!        may be a box or hexagon.                                               !/*ce4c       37*/
      if((iitm.eq.1.or.iitm.eq.2).and.n.eq.1)return                             !/*ce4c       38*/
      if(iitm.eq.1.and.n.ne.2.and.n.ne.4.and.n.ne.6.or.iitm.eq.2.and.           !/*ce        563*/
     & n.ne.6.and.n.ne.8)call erprnt(2,1,2,ncl(lncl+nwc),n,0,0,0,               !/*ce        564*/
     & '12hlattice cell,i7,4h has,i5,10h surfaces.')                            !/*ce        565*/
      return                                                                    !/*ce        566*/
!                                                                               !/*ce        567*/
! >>>>>  cell-filling universes, with transformations               fill        !/*ce        568*/
!        m1c=cell index                                                         !/*ce        569*/
!        m2c(positive)=nwc of last item seen, not in parens                     !/*ce        570*/
!        m2c(negative)=-nwc of left parens, when in parens                      !/*ce        571*/
!        m3c=0 when not in lattice fill                                         !/*ce        572*/
!        m3c(negative)=-nwc of i2 in i1:i2                                      !/*ce        573*/
!        m3c(p0sitive)=n of laf(llaf+m,n)                                       !/*ce        574*/
!        m4c=maximum value of n                                                 !/*ce        575*/
 1040 k=index('()',hitm(1:1))                                                   !/*ce        576*/
      if(k.eq.1.and.m2c.lt.0.or.k.eq.2.and.m2c.ge.0)                            !/*ce        577*/
     & call erprnt(2,1,0,0,0,0,0,1,'29hincorrect use of parentheses.')          !/*ce        578*/
      if(m3c.gt.0.and.hitm.eq.':'.or.                                           !/*ce        579*/
     & m3c.lt.0.and.(nwc+m3c.eq.2.or.nwc+m3c.eq.5.neqv.hitm.eq.':'))            !/*ce        580*/
     & call erprnt(2,1,0,0,0,0,0,1,'31hsyntax error involving a colon.')        !/*ce        581*/
      if(hitm.eq.'('.and.nwc.ne.m2c+1)call erprnt(2,1,0,0,0,0,0,1,              !/*ce        582*/
     & '43htransformation without universe designator.')                        !/*ce        583*/
      if(m3c.ge.0)return                                                        !/*ce        584*/
      if(mod(nwc+m3c,3).ne.0)return                                             !/*ce        585*/
      if(iitm.lt.laf(llaf+mlaf+1+(nwc+m3c)/3,1))call erprnt                     !/*ce        586*/
     & (1,1,0,0,0,0,0,1,'41hincorrect dimension bounds specification.')         !/*ce        587*/
      return                                                                    !/*ce        588*/
!                                                                               !/*ce        589*/
! >>>>>  multigroup options                                        mgopt        !/*ce        590*/
 1050 if(nwc.eq.1.and.hitm.ne.'f'.and.hitm.ne.'a')go to 9010                    !/*ce        591*/
      if(nwc.gt.1.and.kitm.eq.0)go to 9010                                      !/*ce        592*/
      if((nwc.eq.3.or.nwc.eq.4).and.(iitm.lt.0.or.iitm.gt.2))go to 9010         !/*ce        593*/
      if(nwc.eq.7.and.ritm.le.0.)go to 9010                                     !/*ce        594*/
      return                                                                    !/*ce        595*/
!                                                                               !/*ce        596*/
! >>>>>  bremsstrahlung biasing                                    bbrem        !/*ce        597*/
 1060 if(nwc.gt.49)go to 1070                                                   !/*ce4c       39*/
      if(ritm.eq.0.)call erprnt(2,1,0,0,0,0,0,1,'16hbias factor = 0.')          !/*ce        599*/
      return                                                                    !/*ce        600*/
 1070 do 1080 i=1,mxa                                                           !/*cor4-1     55*/
 1080 if(iitm.eq.mat(lmat+i))return                                             !/*ce        602*/
      call erprnt(2,2,1,iitm,0,0,0,0,                                           !/*ce        603*/
     & '12hmaterial no.,i7,25h is not used in any cell.')                       !/*ce        604*/
      return                                                                    !/*ce        605*/
!                                                                               !/*ce4a      193*/
! >>>>>  ptrac specifications                                      ptrac        !/*ce4a      194*/
!        m1c=index of current ptrac keyword.                                    !/*ce4a      195*/
!        m4c=error flag.                                                        !/*ce4a      196*/
 1090 go to(1100,1160,1160)kitm+1                                               !/*ce4a      197*/
 1100 if(index(',=',hitm(1:1)).ne.0)return                                      !/*ce4a      198*/
      if(m1c.eq.0)go to 1130                                                    !/*ce4a      199*/
      if(hptr(m1c).eq.'event'.and.                                              !/*ce4a      200*/
     & index('src bnk sur col ter ',hitm(1:4)).ne.0)return                      !/*ce4a      201*/
      if(hptr(m1c).eq.'file'.and.index('asc bin ',hitm(1:4)).ne.0)return        !/*ce4a      202*/
      if(hptr(m1c).eq.'type'.and.                                               !/*ce4a      203*/
     & index('n   p   e   ',hitm(1:4)).ne.0)return                              !/*ce4a      204*/
      if(hptr(m1c).eq.'write'.and.                                              !/*ce4a      205*/
     & index('pos all ',hitm(1:4)).ne.0)return                                  !/*ce4a      206*/
      if(hptr(m1c).ne.'filter')go to 1130                                       !/*ce4a      207*/
      do 1110 i=1,24                                                            !/*ce4a      208*/
 1110 if(hitm(1:7).eq.hpbl(i))go to 1120                                        !/*ce4a      209*/
      go to 1130                                                                !/*ce4a      210*/
 1120 if(mod(m3c+1,3).eq.1)call erprnt(2,1,0,0,0,0,0,0,                         !/*ce4a      211*/
     & '40hinvalid format for ptrac keyword filter.')                           !/*ce4a      212*/
      return                                                                    !/*ce4a      213*/
 1130 m4c=1                                                                     !/*ce4a      214*/
      do 1140 i=1,nptr                                                          !/*ce4a      215*/
 1140 if(hitm(1:7).eq.hptr(i))go to 1150                                        !/*ce4a      216*/
      go to 9010                                                                !/*ce4a      217*/
 1150 if(iptra(i).eq.0)return                                                   !/*ce4a      218*/
      call erprnt(2,1,0,0,0,0,0,0,                                              !/*ce4a      219*/
     & '32hredundant ptrac keyword '//hptr(i)//'.')                             !/*ce4a      220*/
      return                                                                    !/*ce4a      221*/
 1160 if(m1c.eq.0.and.m4c.eq.0)go to 9010                                       !/*ce4a      222*/
      if(m1c.eq.0)return                                                        !/*ce4a      223*/
      if(hptr(m1c).eq.'filter'.and.mod(m3c+1,3).eq.0)                           !/*ce4a      224*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*ce4a      225*/
     & '40hinvalid format for ptrac keyword filter.')                           !/*ce4a      226*/
      if(kitm.eq.2)go to 1170                                                   !/*ce4a      227*/
      if(hptr(m1c).eq.'surface')iitm=ritm                                       !/*ce4c2      43*/
      if(hptr(m1c).eq.'surface')go to 1170                                      !/*ce4c2      44*/
      if(hptr(m1c).eq.'value'.or.hptr(m1c).eq.'filter')return                   !/*ce4a      228*/
      go to 9010                                                                !/*ce4a      229*/
 1170 if(hptr(m1c).eq.'event'.or.hptr(m1c).eq.'file'.or.                        !/*ce4a      230*/
     &   hptr(m1c).eq.'type')go to 9010                                         !/*ce4a      231*/
      if(hptr(m1c).ne.'value'.and.hptr(m1c).ne.'tally'.and.                     !/*ce4a      232*/
     &   hptr(m1c).ne.'filter'.and.hptr(m1c).ne.'max'.and.                      !/*ce4a      233*/
     & iitm.lt.0)go to 9010                                                     !/*ce4a      234*/
      if(hptr(m1c).eq.'cell'.and.namchg(1,iitm).eq.0)                           !/*ce4a      235*/
     & call erprnt(2,1,1,iitm,0,0,0,0,                                          !/*ce4a      236*/
     & '17hptrac cell entry ,i5,21h is not a valid cell.')                      !/*ce4a      237*/
      if(hptr(m1c).eq.'surface'.and.namchg(2,iitm).eq.0)                        !/*ce4a      238*/
     & call erprnt(2,1,1,iitm,0,0,0,0,                                          !/*ce4a      239*/
     & '20hptrac surface entry ,i5,24h is not a valid surface.')                !/*ce4a      240*/
      return                                                                    !/*ce4a      241*/
!                                                                               !/*ce4b        5*/
! >>>>>  perturbation specificatons                                 pert        !/*ce4b        6*/
!        m1c=index of current pert keyword.                                     !/*ce4b        7*/
 1180 go to(1190,1220,1220)kitm+1                                               !/*ce4b        8*/
 1190 if(index(',=',hitm(1:1)).ne.0)return                                      !/*ce4b        9*/
      do 1200 i=1,npkey                                                         !/*ce4b       10*/
 1200 if(hitm(1:7).eq.hptb(i))go to 1210                                        !/*ce4b       11*/
      go to 9010                                                                !/*ce4b       12*/
 1210 if(iptb(lipb+1+2*i,npert).eq.0)return                                     !/*ce4b       13*/
      call erprnt(2,1,0,0,0,0,0,0,                                              !/*ce4b       14*/
     & '31hredundant pert keyword '//hptb(i)//'.')                              !/*ce4b       15*/
      return                                                                    !/*ce4b       16*/
 1220 if(m1c.eq.0)go to 9010                                                    !/*ce4b       17*/
      if(kitm.eq.2)go to 1230                                                   !/*ce4b       18*/
      if(hptb(m1c).eq.'rho')return                                              !/*ce4b       19*/
      if(hptb(m1c).eq.'erg'.and.ritm.ge.0.)return                               !/*ce4b       20*/
      go to 9010                                                                !/*ce4b       21*/
 1230 if(hptb(m1c).ne.'rho'.and.hptb(m1c).ne.'rxn'.and.                         !/*ce4b       22*/
     &   hptb(m1c).ne.'method'.and.iitm.lt.0)go to 9010                         !/*ce4b       23*/
      if(hptb(m1c).eq.'cell'.and.namchg(1,iitm).eq.0)                           !/*ce4b       24*/
     & call erprnt(2,1,1,iitm,0,0,0,0,                                          !/*ce4b       25*/
     & '16hpert cell entry ,i5,21h is not a valid cell.')                       !/*ce4b       26*/
      if(hptb(m1c).eq.'method'.and.abs(iitm).gt.3)call erprnt                   !/*ce4c       40*/
     & (2,1,0,0,0,0,0,0,'21hillegal method value.')                             !/*ce4c       41*/
      return                                                                    !/*ce4b       27*/
!                                                                               !/*ce4c       42*/
! >>>>>  superimposed weight window generator mesh                  mesh        !/*ce4c       43*/
!        m1c=index of current mesh keyword.                                     !/*ce4c       44*/
!        m2c=number of entries for current keyword.                             !/*ce4c       45*/
 1240 if(kitm.ne.0)go to 1270                                                   !/*ce4c       46*/
      if(index(',=',hitm(1:1)).ne.0)return                                      !/*ce4c       47*/
      do 1250 i=1,nmkey                                                         !/*ce4c       48*/
 1250 if(hitm(1:7).eq.hmsh(i))go to 1260                                        !/*ce4c       49*/
      if(hitm(1:3).ne.'xyz'.and.hitm(1:3).ne.'rec'                              !/*ce4c       50*/
     & .and.hitm(1:3).ne.'rzt'.and.hitm(1:3).ne.'cyl')go to 9010                !/*ce4c       51*/
      return                                                                    !/*ce4c       52*/
 1260 if(imesh(i).eq.0)return                                                   !/*ce4c       53*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ce4c       54*/
     & '31hredundant mesh keyword '//hmsh(i)//'.')                              !/*ce4c       55*/
      return                                                                    !/*ce4c       56*/
 1270 if(m1c.eq.0)return                                                        !/*ce4c       57*/
      if(m1c.le.4.and.m2c.gt.2)go to 9010                                       !/*ce4c       58*/
      return                                                                    !/*ce4c       59*/
!                                                                               !/*ce4c       60*/
 1280 return                                                                    !/*cex        60*/
!                                                                               !/*ce4c2      45*/
! >>>>>  photonuclear isotope override                               mpn        !/*ce4c2      46*/
!        m1c = material index on mpn and preceding m card.                      !/*ce4c2      47*/
 1290 if(nwc.gt.npq(lnpq+m1c))call erprnt(2,1,0,0,0,0,0,1,                      !/*ce4c2      48*/
     & '51hmore entries on mpn card than corresponding m card.')                !/*ce4c2      49*/
      if(nwc.gt.npq(lnpq+m1c))return                                            !/*ce4c2      50*/
      if(iitm.lt.0.or.iitm.gt.999999)call erprnt(2,1,0,0,0,0,0,1,               !/*ce4c2      51*/
     & '50hmpn entries must be integers between 0 and 999999.')                 !/*ce4c2      52*/
      i=iza(liza+jmd(ljmd+m1c)+nwc-1)                                           !/*ce4c2      53*/
      if(iitm.ne.i)call erprnt(2,2,2,iitm,i,0,0,0,                              !/*ce4c2      54*/
     & '17hphotonuclear za =,i6,28h different from nuclear za =,i6')            !/*ce4c2      55*/
      return                                                                    !/*ce4c2      56*/
!                                                                               !/*ce        606*/
! >>>>> lca                                                          lca        !/*cex        62*/
 1300 continue                                                                  !/*cex        63*/
#ifndef CEM                                                                     /*!cex        64*/
      if(nwc.eq.9.and.iitm.ne.0)call erprnt(2,2,0,0,0,0,0,0,                    !/*cex        65*/
     & '43hThis version of MCNPX does not include CEM.')                        !/*cex        66*/
#endif                                                                          /*!cex        67*/
      return                                                                    !/*cex        68*/
!                                                                               !/*cex        69*/
! >>>>>  Tally no print card.                                      talnp        !/*cex        70*/
 1350 continue                                                                  !/*cex        71*/
#ifdef RADIOG                                                                   /*!cex        72*/
      if(nwc.eq.1)lftnp=iitm                                                    !/*cex        73*/
      if(nwc.eq.1)return                                                        !/*cex        74*/
      if(iitm.eq.0)go to 1280                                                   !/*cex        75*/
      if(nwc.eq.2.and.notal.eq.0)go to 1360                                     !/*cex        76*/
      if(lftnp*iitm.gt.0)return                                                 !/*cex        77*/
      call erprnt(2,1,0,0,0,0,0,0,                                              !/*cex        78*/
     & '54hentries on talnp must be all positive or all negative.')             !/*cex        79*/
      return                                                                    !/*cex        80*/
 1360 call erprnt(2,1,0,0,0,0,0,0,                                              !/*cex        81*/
     & '41honly one entry allowed if a zero is used.')                          !/*cex        82*/
#endif                                                                          /*!cex        83*/
      return                                                                    !/*cex        84*/
!                                                                               !/*cexc       14*/
 1370 continue                                                                  !/*cexc       21*/
!                                                                               !/*cex        85*/
!        a common complaint                                                     !/*ce        607*/
 9010 call erprnt(2,1,0,0,0,0,0,1,'50hillegal entry:  '//hitm(1:34))            !/*ce        608*/
      return                                                                    !/*ce        609*/
!                                                                               !/*ce        610*/
! >>>>>  first spare card type                                        za        !/*ce        611*/
 9910 continue                                                                  !/*ce        612*/
      return                                                                    !/*ce        613*/
!                                                                               !/*ce        614*/
! >>>>>  second spare card type                                       zb        !/*ce        615*/
 9920 continue                                                                  !/*ce        616*/
      return                                                                    !/*ce        617*/
!                                                                               !/*ce        618*/
! >>>>>  third spare card type                                        zc        !/*ce        619*/
 9930 continue                                                                  !/*ce        620*/
      return                                                                    !/*ce        621*/
      end                                                                       !/*ce        622*/

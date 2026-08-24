!#ident mctalr.F from deck my                                                   !/*my          1*/
      subroutine mctalr(ln)                                                     !/*my4b        1*/
#ifdef MCPLOT                                                                   /*!myx         1*/
!        read tally data from file mctal.                                       !/*my          4*/
      use allocate_memory                                                       !/*myx         2*/
      use global_data                                                           !/*myx         3*/
      use mcplot_data                                                           !/*myx         4*/
!                                                                               !/*myx         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*myx         6*/
      implicit integer(kindi) (i-n)                                             !/*myx         7*/
!                                                                               !/*myx         8*/
      character hd*80,ha*30,hk(14)*8                                            !/*my          7*/
      dimension nk(15),nn(14)                                                   !/*my          8*/
      data hk/kod,'ntal','tally','f','d','u','s','m','c','e','t','vals',        !/*my          9*/
     & 'tfc','kcode'/,nk/8,4,5,8*1,4,3,5,0/,nn/2*1,25,3,10*1/                   !/*my         10*/
!                                                                               !/*my         11*/
!        open the mctal read file.                                              !/*my         12*/
      open(iuz,file=mctal,status='old')                                         !/*my4b        2*/
      rewind iuz                                                                !/*my         18*/
      ktfile=0                                                                  !/*my         19*/
      mxf=0                                                                     !/*my         20*/
      ltal=0                                                                    !/*my         21*/
!                                                                               !/*my         22*/
!        read the file thru once to determine ntal, nw, npert, and mrkp.        !/*my4b        3*/
      npert=0                                                                   !/*my4b        4*/
      np=0                                                                      !/*my4b        5*/
      ntal=0                                                                    !/*my         24*/
      nw=100                                                                    !/*my         25*/
      n=1                                                                       !/*my         26*/
   20 read(iuz,'(a80)',end=60)hd                                                !/*my         27*/
      call nxtsym(hd,' ,=',1,it,iu,0)                                           !/*my4c        1*/
      if(iu.eq.2.and.(hd(2:2).eq.'t'.or.hd(2:2).eq.'c'))iu=1                    !/*my4c        2*/
      do 30 i=1,14                                                              !/*my4c        3*/
   30 if(nk(i).eq.iu.and.hd(1:iu).eq.hk(i))go to 40                             !/*my4c        4*/
   40 if(i.ne.15)n=nn(i)                                                        !/*my         30*/
      if(i.eq.12)np=np+1                                                        !/*my4b        6*/
      if(i.eq.3)npert=max(np-1,npert)                                           !/*my4b        7*/
      if(i.eq.3)np=0                                                            !/*my4b        8*/
      if(ntal.eq.0.and.i.ge.4.and.i.le.13)ntal=1                                !/*my         32*/
      if(i.eq.3)ntal=ntal+1                                                     !/*my         33*/
      iu=nk(i)                                                                  !/*my         34*/
      if(i.ne.14)go to 50                                                       !/*my4b        9*/
      nsr=71                                                                    !/*my4b       10*/
      jc=6                                                                      !/*my4b       11*/
      if(kdarg(hd,jc,0,ha,r).ge.1)mrkp=nint(r)                                  !/*my4b       12*/
   50 call nxtsym(hd,' ',iu+1,it,iu,1)                                          !/*my4a        1*/
      if(iu.eq.0)go to 20                                                       !/*my         36*/
      nw=nw+n                                                                   !/*my         37*/
      if(n.eq.25)go to 20                                                       !/*my         38*/
      go to 50                                                                  !/*my         39*/
   60 rewind iuz                                                                !/*my         40*/
!                                                                               !/*my         41*/
!        set up dynamically allocated storage.                                  !/*my         42*/
!                                                                               !/*myx         9*/
!        fixed dynamically allocated common.                                    !/*myx        10*/
      if(allocated (iptal)) deallocate (iptal)                                  !/*myx        11*/
      allocate (iptal(8,6,ntal), stat=is)                                       !/*myx        12*/
      if(is.ne.0)go to 370                                                      !/*myx        13*/
      iptal=0                                                                   !/*myx        14*/
      if(allocated (iptb)) deallocate (iptb)                                    !/*myx        15*/
      allocate (iptb(2+2*npkey,npert), stat=is)                                 !/*myx        16*/
      if(is.ne.0)go to 370                                                      !/*myx        17*/
      iptb=0                                                                    !/*myx        18*/
      if(allocated (jptal)) deallocate (jptal)                                  !/*myx        19*/
      allocate (jptal(18,ntal), stat=is)                                        !/*myx        20*/
      if(is.ne.0)go to 370                                                      !/*myx        21*/
      jptal=0                                                                   !/*myx        22*/
      if(allocated (jtf)) deallocate (jtf)                                      !/*myx        23*/
      allocate (jtf(8,ntal), stat=is)                                           !/*myx        24*/
      if(is.ne.0)go to 370                                                      !/*myx        25*/
      jtf=0                                                                     !/*myx        26*/
      if(allocated (ktp)) deallocate (ktp)                                      !/*myx        27*/
      allocate (ktp(mipt,ntal), stat=is)                                        !/*myx        28*/
      if(is.ne.0)go to 370                                                      !/*myx        29*/
      ktp=0                                                                     !/*myx        30*/
      if(allocated (itds)) deallocate (itds)                                    !/*myx        31*/
      allocate (itds(nw+4), stat=is)                                            !/*myx        32*/
      if(is.ne.0)go to 370                                                      !/*myx        33*/
      itds=0                                                                    !/*myx        34*/
      litd=1                                                                    !/*myx        35*/
      if(allocated (tds)) deallocate (tds)                                      !/*myx        36*/
      allocate (tds(nw+4), stat=is)                                             !/*myx        37*/
      if(is.ne.0)go to 370                                                      !/*myx        38*/
      tds=0.                                                                    !/*myx        39*/
      ltds=1                                                                    !/*myx        40*/
!                                                                               !/*myx        41*/
!        variable dynamically allocated common.                                 !/*myx        42*/
      if(allocated (tfc)) deallocate (tfc)                                      !/*myx        43*/
      allocate (tfc(6,20,ntal*(npert+1)), stat=is)                              !/*myx        44*/
      if(is.ne.0)go to 370                                                      !/*myx        45*/
      tfc=0.                                                                    !/*myx        46*/
      if(nsr.eq.71) then                                                        !/*myx        47*/
        if (allocated (rkpl)) deallocate (rkpl)                                 !/*myx        48*/
        allocate (rkpl(mkpl,mrkp), stat=is)                                     !/*myx        49*/
        if(is.ne.0)go to 370                                                    !/*myx        50*/
        rkpl=0.                                                                 !/*myx        51*/
      endif                                                                     !/*myx        52*/
      tal => chg_mem (tal,nw+4,is)                                              !/*myx        53*/
      if(is.ne.0)go to 370                                                      !/*myx        54*/
      tal=0.                                                                    !/*myx        55*/
!                                                                               !/*myx        56*/
      ld=1                                                                      !/*myx        57*/
      do 90 i=1,ntal                                                            !/*my         53*/
      do 80 j=1,8                                                               !/*my         54*/
      jptal(ljpt+j,i)=0                                                         !/*my         55*/
      iptal(lipt+j,1,i)=0                                                       !/*my         56*/
      iptal(lipt+j,2,i)=0                                                       !/*my         57*/
      iptal(lipt+j,3,i)=1                                                       !/*my         58*/
      iptal(lipt+j,4,i)=2                                                       !/*my4c        8*/
   80 iptal(lipt+j,6,i)=0                                                       !/*my4c        9*/
      jptal(ljpt+3,i)=1                                                         !/*myx        58*/
      ktp(lktp+1,i)=1                                                           !/*myx        59*/
      do 85 j=2,mipt                                                            !/*myx        60*/
      ktp(lktp+j,i)=0                                                           !/*myx        61*/
   85 continue                                                                  !/*myx        62*/
   90 continue                                                                  !/*myx        63*/
!                                                                               !/*my         61*/
!        read the file again to pick up the tally data.                         !/*my         62*/
      aid=' '                                                                   !/*my         63*/
      knod=0                                                                    !/*my         64*/
      mtal=0                                                                    !/*my         65*/
      lpert=0                                                                   !/*my4b       16*/
      kf=1                                                                      !/*my         66*/
  100 read(iuz,'(a80)',end=360)hd                                               !/*my         67*/
      call nxtsym(hd,' ,=',1,it,iu,0)                                           !/*my4c       10*/
      if(iu.eq.2.and.(hd(2:2).eq.'t'.or.hd(2:2).eq.'c'))iu=1                    !/*my4c       11*/
      do 110 kk=1,14                                                            !/*my4c       12*/
  110 if(nk(kk).eq.iu.and.hd(1:iu).eq.hk(kk))go to 120                          !/*my4c       13*/
      if(knod.eq.0.and.kf.eq.1)kk=1                                             !/*my4c       14*/
      if(kk.gt.14)go to 190                                                     !/*my4c       15*/
  120 kf=kk                                                                     !/*my4c       16*/
      jc=nk(kk)+1                                                               !/*my         73*/
      jt=0                                                                      !/*my         74*/
      if(mtal.eq.0.and.kk.ge.4.and.kk.le.13)mtal=1                              !/*my         75*/
      go to(130,100,140,150,150,150,150,150,150,150,150,155,160,180)kk          !/*my4b       17*/
!                                                                               !/*my         77*/
! >>>>>  kod line.                                                              !/*my         78*/
  130 probid=hd(17:35)                                                          !/*my         79*/
      jc=36                                                                     !/*my         80*/
      if(kdarg(hd,jc,0,ha,r).lt.1)go to 200                                     !/*my4c       17*/
      knod=max(1,nint(r))                                                       !/*my4c       18*/
      if(kdarg(hd,jc,0,ha,r).lt.1)go to 200                                     !/*my4c       19*/
      nps=r                                                                     !/*my4c       20*/
      if(kdarg(hd,jc,0,ha,r).lt.1)go to 200                                     !/*my4c       21*/
      rnr=r                                                                     !/*my         86*/
      go to 100                                                                 !/*my         87*/
!                                                                               !/*my         88*/
! >>>>>  tally line.                                                            !/*my         89*/
  140 k=kdarg(hd,jc,0,ha,r)                                                     !/*my         90*/
      if(k.lt.0)go to 100                                                       !/*my         91*/
      if(k.eq.0)go to 350                                                       !/*my         92*/
      lpert=0                                                                   !/*my4b       18*/
      mtal=mtal+1                                                               !/*my         93*/
      jptal(ljpt+1,mtal)=r                                                      !/*my         94*/
      jptal(ljpt+2,mtal)=mod(jptal(ljpt+1,mtal),10)                             !/*my         95*/
      if(jptal(ljpt+2,mtal).eq.5)jptal(ljpt+8,mtal)=1                           !/*my         96*/
      k=kdarg(hd,jc,0,ha,r)                                                     !/*my         97*/
      if(k.lt.0)go to 100                                                       !/*my         98*/
      if(k.eq.0)go to 350                                                       !/*my         99*/
      l=nint(r)                                                                 !/*myx        64*/
      if(l.eq.0)go to 100                                                       !/*myx        65*/
      if(l.lt.0)go to 144                                                       !/*myx        66*/
!                                                                               !/*myx        67*/
      ll=0                                                                      !/*myx        68*/
      do 142 mp=1,mipt                                                          !/*myx        69*/
      ktp(lktp+mp,mtal)=0                                                       !/*myx        70*/
      if(mod(l,2).eq.0)go to 142                                                !/*myx        71*/
      ll=ll+1                                                                   !/*myx        72*/
      ktp(lktp+mp,mtal)=1                                                       !/*myx        73*/
  142 l=l/2                                                                     !/*myx        74*/
      jptal(ljpt+3,mtal)=ll                                                     !/*myx        75*/
      go to 100                                                                 !/*my        101*/
!                                                                               !/*myx        76*/
  144 jptal(ljpt+3,mtal)=-l                                                     !/*myx        77*/
      ll=0                                                                      !/*myx        78*/
      mp=0                                                                      !/*myx        79*/
  146 read(iuz,'(a80)',end=360)hd                                               !/*myx        80*/
      jc=1                                                                      !/*myx        81*/
  148 k=kdarg(hd,jc,0,ha,r)                                                     !/*myx        82*/
      if(k.lt.0)go to 146                                                       !/*myx        83*/
      if(k.eq.0)go to 350                                                       !/*myx        84*/
      mp=mp+1                                                                   !/*myx        85*/
      ktp(lktp+mp,mtal)=0                                                       !/*myx        86*/
      if(nint(r).eq.0)go to 149                                                 !/*myx        87*/
      ktp(lktp+mp,mtal)=1                                                       !/*myx        88*/
      ll=ll+1                                                                   !/*myx        89*/
  149 continue                                                                  !/*myx        90*/
      if(mp.lt.mipt)go to 148                                                   !/*myx        91*/
      if(ll.eq.jptal(ljpt+3,mtal))go to 100                                     !/*myx        92*/
      write(jtty,'(45h inconsistent list of tallied particle types.)')          !/*myx        93*/
      go to 350                                                                 !/*myx        94*/
!                                                                               !/*my        102*/
! >>>>>  f, d, u, s, m, c, e, t.                                                !/*my        103*/
  150 k=kdarg(hd,jc,0,ha,r)                                                     !/*my4c       22*/
      if(ha.eq.'t'.or.ha.eq.'c')k=kdarg(hd,jc,0,ha,r)                           !/*my4c       23*/
      if(k.le.0)go to 350                                                       !/*my4c       24*/
      m=max(1,int(r))                                                           !/*my        107*/
      iptal(lipt+kk-3,3,mtal)=m                                                 !/*my        108*/
      iptal(lipt+kk-3,4,mtal)=m+1                                               !/*my4c       25*/
      if(hd(2:2).eq.'t')iptal(lipt+kk-3,4,mtal)=m                               !/*my4c       26*/
      if(hd(2:2).eq.'c')iptal(lipt+kk-3,6,mtal)=1                               !/*my4c       27*/
      k=kdarg(hd,jc,0,ha,r)                                                     !/*my        110*/
      if(k.lt.0)go to 100                                                       !/*my        111*/
      if(k.lt.1)go to 350                                                       !/*my        112*/
      if(r.ne.0.)iptal(lipt+kk-3,2,mtal)=1                                      !/*my        113*/
      go to 100                                                                 !/*my        114*/
!                                                                               !/*my4b       19*/
! >>>>>  vals line.                                                             !/*my4b       20*/
  155 k=kdarg(hd,jc,0,ha,r)                                                     !/*my4b       21*/
      if(k.lt.0)go to 100                                                       !/*my4b       22*/
      if(k.ne.0)go to 350                                                       !/*my4b       23*/
      k=kdarg(hd,jc,0,ha,r)                                                     !/*my4b       24*/
      if(k.lt.0)go to 100                                                       !/*my4b       25*/
      if(k.eq.0)go to 350                                                       !/*my4b       26*/
      lpert=lpert+1                                                             !/*my4b       27*/
      iptb(lipb+1,lpert)=r                                                      !/*my4b       28*/
      go to 100                                                                 !/*my4b       29*/
!                                                                               !/*my        115*/
! >>>>>  tfc line.                                                              !/*my        116*/
  160 do 170 i=0,8                                                              !/*my        117*/
      if(kdarg(hd,jc,0,ha,r).lt.1)go to 350                                     !/*my        118*/
  170 if(i.ne.0)jtf(ljtf+i,mtal)=r                                              !/*my        119*/
      go to 100                                                                 !/*my        120*/
!                                                                               !/*my        121*/
! >>>>>  kcode line.                                                            !/*my        122*/
  180 ikz=0                                                                     !/*my        123*/
      if(kdarg(hd,jc,0,ha,r).lt.1)go to 350                                     !/*my        124*/
      if(kdarg(hd,jc,0,ha,r).ge.1)ikz=r                                         !/*my4b       30*/
      mk=5                                                                      !/*my4b       31*/
      if(kdarg(hd,jc,0,ha,r).ge.1)mk=r                                          !/*my4b       32*/
      kct=mrkp                                                                  !/*my4b       33*/
      kcz=mrkp                                                                  !/*my4b       34*/
      go to 100                                                                 !/*my        126*/
!                                                                               !/*my        127*/
!        anything else.                                                         !/*my        128*/
  190 jc=1                                                                      !/*my        129*/
      go to(200,210,220,250,250,250,250,250,250,250,250,280,310,330)kf          !/*my        130*/
!                                                                               !/*my        131*/
! >>>>>  after the kod line:  problem identification line.                      !/*my        132*/
  200 aid=hd(2:80)                                                              !/*my        133*/
      go to 100                                                                 !/*my        134*/
!                                                                               !/*my        135*/
! >>>>>  after the ntal line:  list of names of the tallies.                    !/*my        136*/
  210 k=kdarg(hd,jc,0,ha,r)                                                     !/*my        137*/
      if(k.lt.0)go to 100                                                       !/*my        138*/
      if(k.eq.0)go to 350                                                       !/*my        139*/
      jt=jt+1                                                                   !/*my        140*/
      jptal(ljpt+1,jt)=r                                                        !/*my        141*/
      jptal(ljpt+2,jt)=mod(jptal(ljpt+1,jt),10)                                 !/*my        142*/
      if(jptal(ljpt+2,mtal).eq.5)jptal(ljpt+8,mtal)=1                           !/*my        143*/
      go to 210                                                                 !/*my        144*/
!                                                                               !/*my        145*/
! >>>>>  after a tally line:  fc card data (tally comment).                     !/*my        146*/
  220 if(jt.ne.0)go to 230                                                      !/*my        147*/
      jptal(ljpt+6,mtal)=ld                                                     !/*myx        97*/
      jt=ld                                                                     !/*myx        98*/
      li=jt+1                                                                   !/*my        150*/
      ld=ld+1                                                                   !/*my        151*/
  230 do 240 i=6,80,3                                                           !/*my        152*/
  240 itds(li+(i-6)/3)=ichar(hd(i:i))*65536+                                    !/*my        153*/
     & ichar(hd(i+1:i+1))*256+ichar(hd(i+2:i+2))                                !/*my        154*/
      itds(jt)=itds(jt)+25                                                      !/*my        155*/
      li=li+25                                                                  !/*my        156*/
      ld=ld+25                                                                  !/*myx        99*/
      go to 100                                                                 !/*my        158*/
!                                                                               !/*my        159*/
! >>>>>  after f, d, u, s, m, c, e or t line:  bin values.                      !/*my        160*/
  250 if(jt.ne.0)go to 260                                                      !/*my        161*/
      jt=ld                                                                     !/*myx       100*/
      iptal(lipt+kf-3,1,mtal)=ld-1                                              !/*my        163*/
      if(kf.ne.4)go to 260                                                      !/*my        164*/
      iptal(lipt+1,1,mtal)=jt-1                                                 !/*my        165*/
      li=jt+iptal(lipt+1,3,mtal)                                                !/*my        166*/
      ld=li                                                                     !/*myx       101*/
  260 k=kdarg(hd,jc,0,ha,r)                                                     !/*my        168*/
      if(k.lt.0)go to 100                                                       !/*my        169*/
      if(k.eq.0)go to 350                                                       !/*my        170*/
      if(kf.ne.4)go to 270                                                      !/*my        171*/
      itds(jt)=li                                                               !/*my        172*/
      itds(li)=1                                                                !/*my        173*/
      itds(li+1)=r                                                              !/*my        174*/
      jt=jt+1                                                                   !/*my        175*/
      li=li+2                                                                   !/*my        176*/
      ld=ld+2                                                                   !/*myx       102*/
      go to 260                                                                 !/*my        178*/
  270 tds(ld)=r                                                                 !/*my        179*/
      ld=ld+1                                                                   !/*my        180*/
      go to 260                                                                 !/*my        181*/
!                                                                               !/*my        182*/
! >>>>>  after vals:  tally/error pairs.                                        !/*my        183*/
  280 if(jt.ne.0.or.lpert.ne.0)go to 300                                        !/*my4b       35*/
      jt=1                                                                      !/*my        185*/
      jptal(ljpt+5,mtal)=ld-1                                                   !/*my        186*/
      iptal(lipt+8,5,mtal)=iptal(lipt+8,3,mtal)                                 !/*my        187*/
      do 290 i=1,7                                                              !/*my        188*/
  290 iptal(lipt+8-i,5,mtal)=iptal(lipt+9-i,5,mtal)*                            !/*my        189*/
     & iptal(lipt+8-i,3,mtal)                                                   !/*my        190*/
  300 k=kdarg(hd,jc,0,ha,r)                                                     !/*my        191*/
      if(k.lt.0)go to 100                                                       !/*my        192*/
      if(k.eq.0)go to 350                                                       !/*my        193*/
      tal(ld)=r                                                                 !/*my        194*/
      ld=ld+1                                                                   !/*my        195*/
      go to 300                                                                 !/*my        196*/
!                                                                               !/*my        197*/
! >>>>>  after tfc line:  nps,tally,error,fom.                                  !/*my        198*/
  310 jt=jt+1                                                                   !/*my        199*/
      do 320 i=0,3                                                              !/*my        200*/
      if(kdarg(hd,jc,0,ha,r).lt.1.and.i.lt.3)go to 350                          !/*my4a        7*/
      if(i.eq.0)npc(jt)=r                                                       !/*my        202*/
  320 if(i.ne.0)tfc(ltfc+i,jt,mtal+lpert*ntal)=r                                !/*my4b       36*/
      go to 100                                                                 !/*my        204*/
!                                                                               !/*my        205*/
! >>>>>  after kcode line:  up to mk values of k's and prompt lifetimes.        !/*my4b       37*/
  330 if(jt.gt.0) go to 340                                                     !/*my4b       38*/
      ld=0                                                                      !/*my4b       39*/
      do 335 i=1,mkpl                                                           !/*my4c       31*/
      do 335 j=1,mrkp                                                           !/*my4b       41*/
  335 rkpl(lrkp+i,j)=0.                                                         !/*my4b       42*/
  340 k=kdarg(hd,jc,0,ha,r)                                                     !/*my4b       43*/
      if(k.lt.0)go to 100                                                       !/*my4b       44*/
      if(k.eq.0)go to 350                                                       !/*my4b       45*/
      if(ld.eq.0)jt=jt+1                                                        !/*my4b       46*/
      ld=ld+1                                                                   !/*my4b       47*/
      if(jt.gt.mrkp)go to 350                                                   !/*my4b       48*/
      rkpl(lrkp+ld,jt)=r                                                        !/*my4b       49*/
      if(ld.eq.mk)ld=0                                                          !/*my4b       50*/
      go to 340                                                                 !/*my4b       51*/
!                                                                               !/*my        212*/
!        complain about trouble.                                                !/*my        213*/
  350 if(hd.eq.' ')go to 100                                                    !/*my4c       32*/
      write(jtty,'(21h error in mctal file ,a8/a80)')mctal,hd                   !/*my4c       33*/
      ntal=0                                                                    !/*my        215*/
      ln=1                                                                      !/*my        216*/
!                                                                               !/*my        217*/
  360 close(iuz)                                                                !/*my        218*/
      mtal=min(1,ntal)                                                          !/*my        219*/
      lpert=0                                                                   !/*my4b       52*/
      return                                                                    !/*my        220*/
  370 call expire(0,'mctalr','error getting memory')                            !/*myx       103*/
#endif                                                                          /*!myx       104*/
      end                                                                       !/*my        221*/

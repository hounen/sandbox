!#ident jdecod.F from deck jd                                                   !/*jd          1*/
      function jdecod(ln)                                                       !/*jd          3*/
#ifdef PLOT                                                                     /*!jdx         1*/
!        decode the next command keyword and its arguments.                     !/*jd          4*/
!        return jdecod=1 for end command, otherwise jdecod=0.                   !/*jd4c2       1*/
!        return ln=1 for end of line, otherwise ln=0.                           !/*jd          6*/
      use global_data                                                           !/*jdx         2*/
      use plot_data                                                             !/*jdx         3*/
!                                                                               !/*jdx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*jdx         5*/
      implicit integer(kindi) (i-n)                                             !/*jdx         6*/
!                                                                               !/*jdx         7*/
      dimension np(mclb),nn(mclb)                                               !/*jd          9*/
      character ha*80,hc(mclb)*4,hm*8,hs*12                                     !/*jd         10*/
      data hc/'cel','imp','rho','den','vol','fcl','mas','pwt','mat',            !/*jd         11*/
     & 'tmp','wwn','ext','pd','dxc','u','lat','fill','nonu','pac'/              !/*jd         12*/
      data np/0,1,3*0,1,4*0,1,1,0,1,4*0,1/,nn/9*0,1,1,0,1,1,4*0,1/              !/*jd4a        1*/
!                                                                               !/*jd         14*/
!        identify the next keyword from the command line.                       !/*jd         15*/
      jdecod=0                                                                  !/*jd         16*/
      k=kdarg(klin,jchar,0,hm,r)                                                !/*jd         17*/
      call nxtsym(hm,' ',1,i,j,2)                                               !/*jd4a        2*/
      ky=0                                                                      !/*jd         18*/
      lc=leng(hm)                                                               !/*jd         19*/
      do 10 k=1,nkeyp                                                           !/*jd         20*/
      if(keyp(k)(1:lc).ne.hm)go to 10                                           !/*jd         21*/
      if(ky.ne.0)ky=-1                                                          !/*jd         22*/
      if(ky.eq.0)ky=k                                                           !/*jd         23*/
   10 continue                                                                  !/*jd         24*/
!                                                                               !/*jd         25*/
!      complain about an unrecognized or ambiguous keyword.                     !/*jd         26*/
      if(ky.eq.0)write(jtty,'(24h unrecognized command:  ,a8)')hm               !/*jd         27*/
      if(ky.lt.0)write(jtty,'(21h ambiguous command:  ,a8)')hm                  !/*jd         28*/
      if(ky.le.0)go to 9020                                                     !/*jd         29*/
!                                                                               !/*jd         30*/
!      branch to the appropriate command decoding section.                      !/*jd         31*/
      go to( 20, 30, 50, 60, 70, 90,100,110,130,150,170,210,240,250,250,        !/*jd         32*/
     &      250,270,440,450,460,470,490,530,550,580,620,110,110,710,750,        !/*jd4c        1*/
     &      760,770,780,9020,9020)ky                                            !/*jd4c2       2*/
!                                                                               !/*jd         34*/
! >>>>>  term.                                                                  !/*jd         35*/
   20 if(kdarg(klin,jchar,0,ha,r).lt.1)go to 9010                               !/*jd         36*/
      iterm=r                                                                   !/*jd         37*/
      if(kdarg(klin,jchar,1,ha,r).gt.0)lspeed=r                                 !/*jd         38*/
      return                                                                    !/*jd         39*/
!                                                                               !/*jd         40*/
! >>>>>  file -- control use of graphics metafile.                              !/*jd         41*/
   30 j=jchar                                                                   !/*jd         43*/
      k=kdarg(klin,jchar,0,ha,r)                                                !/*jd         44*/
      jgf=1                                                                     !/*jd         45*/
      if(ha.eq.'all')jgf=2                                                      !/*jd         46*/
      if(ha.eq.'none')jgf=0                                                     !/*jd         47*/
      if(jgf.eq.1.and.k.ge.0)jchar=j                                            !/*jd         48*/
      return                                                                    !/*jd         49*/
!                                                                               !/*jd         54*/
! >>>>>  viewport. -- specify rectangular or square picture.                    !/*jd         55*/
   50 k=kdarg(klin,jchar,0,ha,r)                                                !/*jd         56*/
      call nxtsym(ha,' ',1,i,j,2)                                               !/*jd4a        7*/
      if(ha.ne.'rect'.and.ha.ne.'square')go to 9010                             !/*jd         57*/
      jvp=0                                                                     !/*jd         58*/
      if(ha.eq.'square')jvp=1                                                   !/*jd         59*/
      return                                                                    !/*jd         60*/
!                                                                               !/*jd         61*/
! >>>>>  &. -- the next line is a continuation line.                            !/*jd         62*/
   60 ln=1                                                                      !/*jd         63*/
      inform=1                                                                  !/*jd         64*/
      return                                                                    !/*jd         65*/
!                                                                               !/*jd         66*/
! >>>>>  end. -- terminate plot.                                                !/*jd         67*/
   70 call gxskip                                                               !/*jd         68*/
      call gxoff(1)                                                             !/*jd         69*/
      call gxoff(2)                                                             !/*jd         70*/
   80 jdecod=1                                                                  !/*jd         71*/
      return                                                                    !/*jd         72*/
!                                                                               !/*jd         73*/
! >>>>>  return. -- return to mcplot if called from there.                      !/*jd         74*/
   90 if(iovr.ne.2)go to 80                                                     !/*jd         75*/
      write(jtty,'(31h inappropriate command:  return)')                        !/*jd         76*/
      go to 9020                                                                !/*jd         77*/
!                                                                               !/*jd         78*/
! >>>>>  mcplot. -- call or return to mcplot.                                   !/*jd         79*/
  100 ln=1                                                                      !/*jd4c2       3*/
#ifndef MCPLOT                                                                  /*!jd4c2       4*/
      write(jtty,'(25h mcplot is not available.)')                              !/*jd4c2       5*/
      if(ln.eq.1)go to 9020                                                     !/*jd4c2       6*/
#else                                                                           /*!jdx         8*/
      call mcplot                                                               !/*jd         82*/
#endif                                                                          /*!cpp       147*/
      hovr='plot'                                                               !/*jd         83*/
      inform=1                                                                  !/*jd         84*/
      return                                                                    !/*jd         86*/
!                                                                               !/*jd         90*/
! >>>>>  options. -- print a list of the available commands.                    !/*jd         91*/
  110 write(jtty,'(20h available commands:)')                                   !/*jd         92*/
      n=(nkeyp+2)/3                                                             !/*jd         93*/
      do 120 i=1,n                                                              !/*jd         94*/
  120 write(jtty,'(2x,3(10x,a8))')(keyp(k),k=i,nkeyp,n)                         !/*jd         95*/
      if(mcolor.le.2)go to 9030                                                 !/*jd4a        8*/
      write(jtty,'(18h available colors:)')                                     !/*jd4a        9*/
      do 125 i=1,4                                                              !/*jdxk        1*/
  125 write(jtty,'(6x,3(6x,a12))')(hcolor(k),k=i,10,4)                          !/*jdxk        2*/
      go to 9030                                                                !/*jd         96*/
!                                                                               !/*jd         97*/
! >>>>>  status. -- print the current state of the plot controls.               !/*jd         98*/
  130 write(jtty,140)extent,basis,origin,sclabl(1),sclabl(2),iterm,             !/*jd         99*/
     & lspeed                                                                   !/*jd        100*/
  140 format(9h extent =,1p2e16.6/9h  basis =,3e16.6/9x,3e16.6/9x,              !/*jd        101*/
     & 3e16.6/9h origin =,3e16.6/9h  label =,0p2f8.4/,9h   term =,2i10)         !/*jd        102*/
      go to 9030                                                                !/*jd        103*/
!                                                                               !/*jd        104*/
! >>>>>  runtpe. -- read a dump from a runtpe file.                             !/*jd        105*/
  150 k=kdarg(klin,jchar,0,ha,r)                                                !/*jd        106*/
      if(inqire(ha(1:8)).ne.0)go to 160                                         !/*jd        107*/
      write(jtty,'(8h runtpe ,a8,16h does not exist.)')ha                       !/*jd        108*/
      go to 9020                                                                !/*jd        109*/
  160 runtpe=ha                                                                 !/*jd        110*/
      idmp=0                                                                    !/*jd        111*/
      if(kdarg(klin,jchar,1,ha,r).gt.0)idmp=r                                   !/*jd        112*/
      if(idmp.lt.2.and.idmp.ne.0)go to 9010                                     !/*jd        113*/
      call plotg_allocate(0)                                                    !/*jdx         9*/
      call tpefil(7)                                                            !/*jdx        10*/
      call plotg_allocate(1)                                                    !/*jdx        11*/
      if(ktfile.ne.17)go to 9020                                                !/*jd        115*/
      ktfile=1                                                                  !/*jd        116*/
      return                                                                    !/*jd        117*/
!                                                                               !/*jd        118*/
! >>>>>  basis.                                                                 !/*jd        119*/
  170 do 180 i=1,6                                                              !/*jd        120*/
  180 if(kdarg(klin,jchar,0,ha,tpp(i)).le.0)go to 9010                          !/*jd        121*/
      a=sqrt(tpp(1)**2+tpp(2)**2+tpp(3)**2)                                     !/*jd        122*/
      ab=tpp(1)*tpp(4)+tpp(2)*tpp(5)+tpp(3)*tpp(6)                              !/*jd        123*/
      b=sqrt(tpp(4)**2+tpp(5)**2+tpp(6)**2)                                     !/*jd        124*/
      rb=a*sqrt(abs((a*b)**2-ab**2))                                            !/*jd        125*/
      if(min(a,b).lt.1e-8.or.rb.lt.a**2*b*1e-4)go to 200                        !/*jd        126*/
      do 190 i=1,3                                                              !/*jd        127*/
      basis(i+3)=(tpp(i+3)*a**2-tpp(i)*ab)/rb                                   !/*jd        128*/
  190 basis(i)=tpp(i)/a                                                         !/*jd        129*/
      call crspro(basis(1),basis(4),basis(7))                                   !/*jd        130*/
      return                                                                    !/*jd        131*/
  200 write(jtty,'(36h basis vectors are zero or parallel.)')                   !/*jd        132*/
      go to 9020                                                                !/*jd        133*/
!                                                                               !/*jd        134*/
! >>>>>  origin.                                                                !/*jd        135*/
  210 do 220 i=1,3                                                              !/*jd        136*/
  220 if(kdarg(klin,jchar,0,ha,tpp(i)).le.0)go to 9010                          !/*jd        137*/
      do 230 i=1,3                                                              !/*jd        138*/
  230 origin(i)=tpp(i)                                                          !/*jd        139*/
      return                                                                    !/*jd        140*/
!                                                                               !/*jd        141*/
! >>>>>  extent.                                                                !/*jd        142*/
  240 if(kdarg(klin,jchar,0,ha,r).le.0)go to 9010                               !/*jd        143*/
      if(r.le.0.)go to 9010                                                     !/*jd        144*/
      extent(1)=r                                                               !/*jd        145*/
      extent(2)=r                                                               !/*jd        146*/
      if(kdarg(klin,jchar,1,ha,r).le.0)return                                   !/*jd        147*/
      if(r.le.0.)go to 9010                                                     !/*jd        148*/
      extent(2)=r                                                               !/*jd        149*/
      return                                                                    !/*jd        150*/
!                                                                               !/*jd        151*/
! >>>>>  px or py or pz.                                                        !/*jd        152*/
  250 if(kdarg(klin,jchar,0,ha,r).le.0)go to 9010                               !/*jd        153*/
      i=index('xyz',hm(2:2))                                                    !/*jd        154*/
      origin(i)=r                                                               !/*jd        155*/
      do 260 j=1,9                                                              !/*jd        156*/
      basis(j)=0.                                                               !/*jd        157*/
  260 if(j.eq.max(3-i,1).or.j.eq.min(8-i,6).or.j.eq.6+i)basis(j)=1.             !/*jd        158*/
      return                                                                    !/*jd        159*/
!                                                                               !/*jd        160*/
! >>>>>  label.                                                                 !/*jd        161*/
  270 if(kdarg(klin,jchar,0,ha,r).le.0)go to 9010                               !/*jd        162*/
      if(r.ne.0..and.(r.lt..1999.or.r.gt.100.))go to 370                        !/*jd        163*/
      sclabl(1)=r                                                               !/*jd        164*/
      if(kdarg(klin,jchar,1,ha,r).le.0)return                                   !/*jd        165*/
      if(r.ne.0..and.(r.lt..1999.or.r.gt.100.))go to 370                        !/*jd        166*/
      sclabl(2)=r                                                               !/*jd        167*/
      j=jchar                                                                   !/*jd        168*/
      if(kdarg(klin,jchar,0,ha,r).lt.0)return                                   !/*jd        169*/
      call nxtsym(ha,' ',1,i,jj,2)                                              !/*jd4a       13*/
      do 280 m=2,10                                                             !/*jd        170*/
  280 if(index('abcdefghijklmnopqrstuvwxyz',ha(m:m)).eq.0)go to 290             !/*jd        171*/
  290 do 300 i=1,mclb                                                           !/*jd        172*/
  300 if(ha(1:m-1).eq.hc(i))go to 310                                           !/*jd        173*/
      go to 430                                                                 !/*jd        174*/
  310 if(i.eq.8.and.kpt(1)*kpt(2).eq.0.or.i.eq.14.and.                          !/*jd        175*/
     & ndx(1)+ndx(2).eq.0.or.i.ge.15.and.i.le.18.and.junf.eq.0)go to 410        !/*jd        176*/
      if(np(i)+nn(i).eq.0.and.ha(m:m).ne.' ')go to 430                          !/*jd        177*/
      k=index(ha,':')                                                           !/*jd        178*/
      if(np(i).ne.0.eqv.k.eq.0)go to 430                                        !/*jd        179*/
      l=index(hip,ha(k+1:k+1))                                                  !/*jd        180*/
      if(np(i).ne.0.and.l.eq.0)go to 430                                        !/*jd        181*/
      if(k.eq.0)hs=ha(m:m+11)                                                   !/*jd        182*/
      if(k.eq.m)hs=ha(m+2:m+13)                                                 !/*jd        183*/
      if(k.gt.m)hs=ha(m:k-1)                                                    !/*jd        184*/
      if(nn(i).eq.0.and.hs.ne.' ')go to 430                                     !/*jd        185*/
      n=1                                                                       !/*jd        186*/
      if(nn(i).eq.0.or.hs.eq.' ')go to 360                                      !/*jd        187*/
      if(kdata(hs).ne.2)go to 430                                               !/*jd        188*/
      read(hs,'(bn,i12)')n                                                      !/*jd        189*/
      n=max(1,n)                                                                !/*jd        190*/
      if(i.eq.10.and.n.gt.mxt.or.i.eq.11.and.n.gt.nww(l).or.                    !/*jd        191*/
     & i.eq.19.and.n.gt.8)go to 390                                             !/*jd        192*/
      if(i.ne.13)go to 360                                                      !/*jd        193*/
      if(mod(n,5).ne.0)go to 340                                                !/*jd        194*/
      do 320 m=1,ntal                                                           !/*jd        195*/
  320 if(jptal(ljpt+1,m).eq.n)go to 330                                         !/*jd        196*/
      go to 340                                                                 !/*jd        197*/
  330 n=m                                                                       !/*jd        198*/
      if(iptal(lipt+2,2,n).ne.0)go to 360                                       !/*jd        199*/
  340 write(jtty,350)ha(1:10)                                                   !/*jd        200*/
  350 format(21h bad tally number in ,a10)                                      !/*jd        201*/
      go to 9020                                                                !/*jd        202*/
  360 if(np(i).ne.0)n=n+1000*l                                                  !/*jd        203*/
      sclabl(3)=i                                                               !/*jd        204*/
      sclabl(4)=n                                                               !/*jd        205*/
      return                                                                    !/*jd        206*/
  370 write(jtty,380)ha                                                         !/*jd        207*/
  380 format(16h label argument ,a10,25h is too big or too small.)              !/*jd        208*/
      go to 9020                                                                !/*jd        209*/
  390 write(jtty,400)n,ha(1:3)                                                  !/*jd        210*/
  400 format(i7,7h after ,a3,17h is out of range.)                              !/*jd        211*/
      go to 9020                                                                !/*jd        212*/
  410 write(jtty,420)ha(1:5)                                                    !/*jd        213*/
  420 format(1x,a5,36h is not used in the current problem.)                     !/*jd        214*/
      go to 9020                                                                !/*jd        215*/
  430 jchar=j                                                                   !/*jd        216*/
      return                                                                    !/*jd        217*/
!                                                                               !/*jd        218*/
! >>>>>  scales.                                                                !/*jd        219*/
  440 if(kdarg(klin,jchar,0,ha,r).le.0)go to 9010                               !/*jd        220*/
      jscal=r                                                                   !/*jd        221*/
      return                                                                    !/*jd        222*/
!                                                                               !/*jd        223*/
! >>>>>  center.                                                                !/*jd        224*/
  450 if(kdarg(klin,jchar,0,ha,r).le.0)go to 9010                               !/*jd        225*/
      if(kdarg(klin,jchar,0,ha,s).le.0)go to 9010                               !/*jd        226*/
      origin(1)=origin(1)+r*basis(1)+s*basis(4)                                 !/*jd        227*/
      origin(2)=origin(2)+r*basis(2)+s*basis(5)                                 !/*jd        228*/
      origin(3)=origin(3)+r*basis(3)+s*basis(6)                                 !/*jd        229*/
      return                                                                    !/*jd        230*/
!                                                                               !/*jd        231*/
! >>>>>  factor.                                                                !/*jd        232*/
  460 if(kdarg(klin,jchar,0,ha,r).lt.0)go to 9010                               !/*jd        233*/
      if(r.lt.1e-6)go to 9010                                                   !/*jd        234*/
      extent(1)=extent(1)*r                                                     !/*jd        235*/
      extent(2)=extent(2)*r                                                     !/*jd        236*/
      return                                                                    !/*jd        237*/
!                                                                               !/*jd        238*/
! >>>>>  theta.                                                                 !/*jd        239*/
  470 if(kdarg(klin,jchar,0,ha,r).lt.0)go to 9010                               !/*jd        240*/
      c=cos(r*pie/180.)                                                         !/*jd        241*/
      s=sin(r*pie/180.)                                                         !/*jd        242*/
      do 480 i=1,3                                                              !/*jd        243*/
      r=basis(i)                                                                !/*jd        244*/
      basis(i)=c*r-s*basis(i+3)                                                 !/*jd        245*/
  480 basis(i+3)=c*basis(i+3)+s*r                                               !/*jd        246*/
      return                                                                    !/*jd        247*/
!                                                                               !/*jd        248*/
! >>>>>  cursor.                                                                !/*jd        249*/
#ifndef QWIN                                                                    /*!jd4c        3*/
  490 ln=1                                                                      !/*jd        251*/
      if(iterm.eq.0)go to 510                                                   !/*jd        252*/
      icurs=1                                                                   !/*jd        253*/
      if(icurs1.ne.0)return                                                     !/*jd        254*/
      do 500 i=1,3                                                              !/*jd        255*/
      orsav(i)=origin(i)                                                        !/*jd        256*/
  500 if(i.le.2)exsav(i)=extent(i)                                              !/*jd        257*/
      icurs1=1                                                                  !/*jd        258*/
      return                                                                    !/*jd        259*/
  510 write(jtty,520)                                                           !/*jd        260*/
  520 format(30h cursor input is not possible.)                                 !/*jd        261*/
      go to 9030                                                                !/*jd        262*/
#else                                                                           /*!jdx        12*/
  490 write(jtty,500)                                                           !/*jd        264*/
  500 format(50h cursor is not supported by this graphics package.)             !/*jd4a       16*/
      go to 9020                                                                !/*jd        266*/
#endif                                                                          /*!cpp       149*/
!                                                                               !/*jd        267*/
! >>>>>  restore.                                                               !/*jd        268*/
  530 ln=1                                                                      !/*jd        269*/
      if(icurs1.eq.0)return                                                     !/*jd        270*/
      icurs1=0                                                                  !/*jd        271*/
      do 540 i=1,3                                                              !/*jd        272*/
      origin(i)=orsav(i)                                                        !/*jd        273*/
  540 if(i.le.2)extent(i)=exsav(i)                                              !/*jd        274*/
      return                                                                    !/*jd        275*/
!                                                                               !/*jd        276*/
! >>>>>  locate.                                                                !/*jd        277*/
#ifndef QWIN                                                                    /*!jd4c        5*/
  550 ln=1                                                                      !/*jd        279*/
      if(iterm.eq.0)go to 560                                                   !/*jd        280*/
      jloc=1                                                                    !/*jd        281*/
      yhom=yhom-2.*sch*extent(2)                                                !/*jd4a       18*/
      return                                                                    !/*jd        283*/
  560 write(jtty,570)                                                           !/*jd        284*/
  570 format(24h locate is not possible.)                                       !/*jd        285*/
      go to 9030                                                                !/*jd        286*/
#else                                                                           /*!jdx        13*/
  550 write(jtty,560)                                                           !/*jd        288*/
  560 format(50h locate is not supported by this graphics package.)             !/*jd4a       20*/
      go to 9020                                                                !/*jd        290*/
#endif                                                                          /*!cpp       151*/
!                                                                               !/*jd4a       21*/
! >>>>>  color.                                                                 !/*jd4a       22*/
  580 if(kdarg(klin,jchar,0,ha,r).gt.0)go to 590                                !/*jd4a       23*/
      if(ha.ne.'on'.and.ha.ne.'off')go to 9010                                  !/*jd4a       24*/
      mcolor=abs(mcolor)                                                        !/*jd4a       25*/
      if(ha.eq.'off')mcolor=-mcolor                                             !/*jd4a       26*/
      if(mcolor.lt.0)return                                                     !/*jdxk        3*/
      sclabl(5)=sclabl(3)                                                       !/*jdxk        4*/
      sclabl(6)=sclabl(4)                                                       !/*jdxk        5*/
      return                                                                    !/*jd4a       27*/
  590 mcolor=abs(mcolor)                                                        !/*jd4a       28*/
      if(r.lt.50.or.r.gt.5000.)go to 600                                        !/*jd4b        6*/
      lcolor=nint(r)                                                            !/*jd4a       30*/
      return                                                                    !/*jd4a       31*/
  600 write(jtty,610)                                                           !/*jd4a       32*/
  610 format(50h resolution parameter must be between 50 and 5000.)             !/*jd4b        7*/
      go to 9020                                                                !/*jd4a       34*/
!                                                                               !/*jd4a       35*/
! >>>>>  shade.                                                                 !/*jd4a       36*/
  620 j=jchar                                                                   !/*jd4a       37*/
      if(kdarg(klin,jchar,0,ha,r).le.0)go to 700                                !/*jd4a       38*/
      n=nint(r)                                                                 !/*jd4a       39*/
      do 630 k=1,nmat                                                           !/*jd4a       40*/
  630 if(nmt(lnmt+k).eq.n)go to 650                                             !/*jd4a       41*/
      write(jtty,640)n                                                          !/*jd4a       42*/
  640 format(10h material ,i6,16h does not exist.)                              !/*jd4a       43*/
      go to 9020                                                                !/*jd4a       44*/
  650 if(k.le.mplm)go to 670                                                    !/*jd4a       45*/
      write(jtty,660)mplm                                                       !/*jd4a       46*/
  660 format(15h increase mplm=,i4,28h for shading more materials.)             !/*jd4a       47*/
      go to 9020                                                                !/*jd4a       48*/
  670 if(kdarg(klin,jchar,0,ha,r).gt.0)go to 9010                               !/*jd4a       49*/
      do 680 i=1,ncolor                                                         !/*jd4b        8*/
  680 if(ha.eq.hcolor(i))go to 690                                              !/*jd4a       51*/
      go to 9010                                                                !/*jd4a       52*/
  690 icolor(k)=(i-1)*int(float(ncolor)/10.)+1                                  !/*jdxk        6*/
      go to 620                                                                 !/*jd4a       54*/
  700 jchar=j                                                                   !/*jd4a       55*/
      return                                                                    !/*jd4a       56*/
!                                                                               !/*jd4c        7*/
! >>>>>  pause nj.  hold picture for nj seconds.                                !/*jd4c        8*/
  710 if(kdarg(klin,jchar,1,ha,r).le.0)go to 730                                !/*jd4c        9*/
      nj=r                                                                      !/*jd4c       10*/
      if(nj.le.0)go to 730                                                      !/*jd4c       11*/
      call getidt(ha)                                                           !/*jd4c       12*/
      read(ha,'(11x,i2,1x,i2,1x,i2)')jh,jm,js                                   !/*jd4c       13*/
      ks=3600*jh+60*jm+js                                                       !/*jd4c       14*/
      n=-1000000                                                                !/*jd4c       15*/
  720 call getidt(ha)                                                           !/*jd4c       16*/
      read(ha,'(11x,i2,1x,i2,1x,i2)')jh,jm,js                                   !/*jd4c       17*/
      ls=3600*jh+60*jm+js                                                       !/*jd4c       18*/
      if(irup.ne.0)call qttyin(nj-ls+ks,'7hpausing,i5,8hseconds.')              !/*jd4c       19*/
      if(ls-ks.ge.nj)go to 740                                                  !/*jd4c       20*/
      n=n+1                                                                     !/*jd4c       21*/
      if(n.gt.1000000.and.ls.le.ks)go to 740                                    !/*jd4c       22*/
      go to 720                                                                 !/*jd4c       23*/
  730 write(jtty,'(27hstrike any key to continue.)')                            !/*jd4c       24*/
      read(itty,'(a8)',end=740)                                                 !/*jd4c       25*/
  740 return                                                                    !/*jd4c       26*/
!                                                                               !/*jd4c       27*/
! >>>>>  level nj.  plot data in universe level nn only.                        !/*jd4c       28*/
  750 if(junf.eq.0)return                                                       !/*jd4c       29*/
      levplt=-1                                                                 !/*jd4c       30*/
      if(kdarg(klin,jchar,0,ha,r).gt.0)levplt=nint(r)                           !/*jd4c2       8*/
      if(levplt.ge.nlev)levplt=-1                                               !/*jd4c       32*/
      return                                                                    !/*jd4c       33*/
!                                                                               !/*jd4c       34*/
! >>>>>  mbody.  plot macrobody facet option.                                   !/*jd4c       35*/
  760 k=kdarg(klin,jchar,0,ha,r)                                                !/*jd4c       36*/
      call nxtsym(ha,' ',1,i,j,2)                                               !/*jd4c       37*/
      if(ha.ne.'on'.and.ha.ne.'off')go to 9010                                  !/*jd4c       38*/
      if(ha.eq.'on')idne(lide+idne(lide+2)+4)=0                                 !/*jd4c       39*/
      if(ha.eq.'off')idne(lide+idne(lide+2)+4)=1                                !/*jd4c       40*/
      return                                                                    !/*jd4c       41*/
!                                                                               !/*jd4c2       9*/
! >>>>>  interact.  return to interactive plot mode.                            !/*jd4c2      10*/
  770 if(jvp.ne.0)jvp=0                                                         !/*jd4c2      11*/
#ifdef XLIB                                                                     /*!jd4c2      12*/
      inform=-1                                                                 !/*jd4c2      13*/
#endif                                                                          /*!cpp       152*/
      return                                                                    !/*jd4c2      14*/
!                                                                               !/*jd4c2      15*/
! >>>>>  mesh.  meshpl = 0/1/2/3 = outline none/cells/ww mesh/both.             !/*jd4c2      16*/
  780 meshpl=0                                                                  !/*jd4c2      17*/
      if(kdarg(klin,jchar,0,ha,r).gt.0)meshpl=nint(abs(r))                      !/*jd4c2      18*/
      if(mod(meshpl/2,2).ne.0.and.(nwgeom+1)/2.ne.1)meshpl=1                    !/*jd4c2      19*/
      return                                                                    !/*jd4c2      20*/
!                                                                               !/*jd        291*/
!        common complaints.                                                     !/*jd        292*/
 9010 write(jtty,'(17h bad argument of ,a8,11h command:  ,a8)')hm,ha            !/*jd        293*/
 9020 write(jtty,'(41h the rest of the command line is ignored.)')              !/*jd        294*/
      ln=1                                                                      !/*jd        295*/
 9030 inform=1                                                                  !/*jd        296*/
#else                                                                           /*!jdx        14*/
      jdecod=0                                                                  !/*jdx        15*/
#endif                                                                          /*!jdx        16*/
      return                                                                    !/*jd        297*/
      end                                                                       !/*jd        298*/

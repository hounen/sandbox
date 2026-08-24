!#ident mdecod.F from deck md                                                   !/*md          1*/
      function mdecod(ln)                                                       !/*md          3*/
#ifdef MCPLOT                                                                   /*!mdx         1*/
#ifdef LAHEY                                                                    /*!md4c        1*/
      use WINTERACTER                                                           !/*md4c        2*/
#endif                                                                          /*!cpp       292*/
!        decode the next command keyword and its arguments.                     !/*md          4*/
!        return 1 for any termination command, otherwise 0.                     !/*md          5*/
!        return ln=1 for end of line, otherwise ln=0.                           !/*md          6*/
      use global_data                                                           !/*mdx         2*/
      use mcplot_data                                                           !/*mdx         3*/
!                                                                               !/*mdx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*mdx         5*/
      implicit integer(kindi) (i-n)                                             !/*mdx         6*/
!                                                                               !/*mdx         7*/
      character ha*80,hm*8                                                      !/*md          9*/
!                                                                               !/*md         10*/
!        identify the next keyword from the command line.                       !/*md         11*/
      mdecod=0                                                                  !/*md         12*/
      k=kdarg(klin,jchar,0,hm,r)                                                !/*md         13*/
      call nxtsym(hm,' ',1,i,j,2)                                               !/*md4a        1*/
      ky=0                                                                      !/*md         14*/
      lc=leng(hm)                                                               !/*md         15*/
      do 10 k=1,nkeys                                                           !/*md         16*/
      if(keys(k)(1:lc).ne.hm)go to 10                                           !/*md         17*/
      if(ky.ne.0)ky=-1                                                          !/*md         18*/
      if(ky.eq.0)ky=k                                                           !/*md         19*/
   10 continue                                                                  !/*md         20*/
!                                                                               !/*md         21*/
!        complain about an unrecognized, ambiguous, or illegal keyword.         !/*md         22*/
      if(ky.eq.0)write(jtty,'(24h unrecognized command:  ,a8)')hm               !/*md         23*/
      if(ky.lt.0)write(jtty,'(21h ambiguous command:  ,a8)')hm                  !/*md         24*/
      if(ky.le.0)go to 9020                                                     !/*md         25*/
      if(koplot.eq.0.or.legalc(ky).ne.0)go to 20                                !/*md         26*/
      write(jtty,'(1x,a8,37h is not legal after a coplot command.)')hm          !/*md         27*/
      go to 9020                                                                !/*md         28*/
   20 if(iovr.ne.4.or.legalm(ky).ne.0)go to 30                                  !/*md4b        1*/
      write(jtty,'(1x,a8,30h illegal with runtime monitor.)')hm                 !/*md4b        2*/
      go to 9020                                                                !/*md4b        3*/
   30 if(kxsplt.eq.0.and.legalx(ky).le.0)go to 40                               !/*md4b        4*/
      if(kxsplt.ne.0.and.legalx(ky).ge.0)go to 40                               !/*md4b        5*/
      if(kxsplt.ne.0)write(jtty,'(1x,a8,22h illegal with xs plot.)')hm          !/*md4b        6*/
      if(kxsplt.eq.0)                                                           !/*md4b        7*/
     & write(jtty,'(1x,a8,25h illegal with tally plot.)')hm                     !/*md4b        8*/
      if(abs(legalx(ky)).eq.1)go to 9020                                        !/*md4b        9*/
      if(kxsplt.eq.0.and.jovr(1)+jovr(3)+jovr(5).ne.3)go to 9020                !/*md4b       10*/
      if(kxsplt.eq.0)write(jtty,'(22h switching to xs plot.)')                  !/*md4b       11*/
      if(kxsplt.ne.0)write(jtty,'(25h switching to tally plot.)')               !/*md4b       12*/
      write(jtty,'(21h retype command line.)')                                  !/*md4b       13*/
      kxsplt=max(0,1-kxsplt)                                                    !/*md4b       14*/
      call mreset(21)                                                           !/*md4b       15*/
      ln=1                                                                      !/*md4b       16*/
      go to 9030                                                                !/*md4b       17*/
!                                                                               !/*md         33*/
!        branch to the appropriate command decoding section.                    !/*md         34*/
   40 go to( 50, 60,80,90,150,170,180,190,200,230,250,280,320,330,350,          !/*md         35*/
     &      380,410,430,470,480,490,530,560,530,530,530,530,530,570,580,        !/*md         36*/
     &      590,610,620,640,640,640,640,650,650,660,670,670,680,670,710,        !/*md         37*/
     &      720,730,740,770,800,820,230,230,840,900,920,940,970,                !/*mdxl        1*/
     &      9010,9010)ky                                                        !/*mdxl        2*/
!                                                                               !/*md         39*/
! >>>>>  term.                                                                  !/*md         40*/
   50 if(kdarg(klin,jchar,0,ha,r).lt.1)go to 9010                               !/*md         41*/
      iterm=r                                                                   !/*md         42*/
      if(kdarg(klin,jchar,1,ha,r).gt.0)lspeed=r                                 !/*md         43*/
      return                                                                    !/*md         44*/
!                                                                               !/*md         45*/
! >>>>>  file. -- control use of graphics metafile.                             !/*md         46*/
   60 j=jchar                                                                   !/*md         48*/
      k=kdarg(klin,jchar,0,ha,r)                                                !/*md         49*/
      jgf=1                                                                     !/*md         50*/
      if(ha.eq.'all')jgf=2                                                      !/*md         51*/
      if(ha.eq.'none')jgf=0                                                     !/*md         52*/
      if(jgf.eq.1.and.k.ge.0)jchar=j                                            !/*md         53*/
      return                                                                    !/*md         54*/
!                                                                               !/*md         59*/
! >>>>>  &. -- the next line is a continuation line.                            !/*md         60*/
   80 ln=1                                                                      !/*md         61*/
      inform=1                                                                  !/*md         62*/
      return                                                                    !/*md         63*/
!                                                                               !/*md         64*/
! >>>>>  coplot.                                                                !/*md         65*/
   90 if(iprpts.eq.0)go to 110                                                  !/*md         66*/
      write(jtty,100)                                                           !/*md         67*/
  100 format(38h can't coplot in a printpts situation.)                         !/*md         68*/
      go to 9020                                                                !/*md         69*/
  110 if(inform.eq.0)go to 130                                                  !/*md         70*/
      write(jtty,120)                                                           !/*md         71*/
  120 format(53h can't coplot after an information or error response.)          !/*md         72*/
      go to 9020                                                                !/*md         73*/
  130 if(nfree.ne.2)koplot=koplot+1                                             !/*md         74*/
      if(nfree.ne.2)return                                                      !/*md         75*/
      write(jtty,140)                                                           !/*md         76*/
  140 format(50h coplot is not possible with a contour or 3d plot.)             !/*md         77*/
      go to 9020                                                                !/*md         78*/
!                                                                               !/*md         79*/
! >>>>>  end. -- terminate mcplot.                                              !/*md         80*/
  150 call gxskip                                                               !/*md4a        2*/
      call gxoff(1)                                                             !/*md4a        3*/
      call gxoff(2)                                                             !/*md4a        4*/
  160 koplot=0                                                                  !/*md4a        5*/
      mdecod=1                                                                  !/*md4a        6*/
      return                                                                    !/*md         83*/
!                                                                               !/*md         84*/
! >>>>>  return. -- return to mcrun or plot if called from there.               !/*md         85*/
  170 if(iovr.ne.5)go to 160                                                    !/*md         86*/
      write(jtty,'(31h inappropriate command:  return)')                        !/*md         87*/
      go to 9020                                                                !/*md         88*/
!                                                                               !/*md         89*/
! >>>>>  plot. -- call or return to plot.                                       !/*md         90*/
  180 if(iovr.eq.2)go to 160                                                    !/*md         91*/
      call plotg                                                                !/*md4a        7*/
      hovr='mcplot'                                                             !/*md         93*/
      jvp=0                                                                     !/*md4b       19*/
      inform=1                                                                  !/*md         94*/
      ln=1                                                                      !/*md         95*/
      return                                                                    !/*md         96*/
!                                                                               !/*md         97*/
! >>>>>  freq. -- frequency of calls from mcrun.                                !/*md         98*/
  190 if(kdarg(klin,jchar,0,ha,r).lt.1)go to 9010                               !/*md         99*/
      freq=r                                                                    !/*md        100*/
      return                                                                    !/*md        101*/
!                                                                               !/*md        102*/
! >>>>>  iptal. -- print the iptal array for the current tally.                 !/*md        103*/
  200 inform=1                                                                  !/*md        104*/
      if(mtal.ne.0)write(jtty,210)jptal(ljpt+1,mtal),                           !/*md        105*/
     & ((iptal(lipt+i,j,mtal),j=1,5),i=1,8)                                     !/*md        106*/
  210 format(24h iptal for tally number ,i4,1h:/(5i10))                         !/*md        107*/
      if(mtal.eq.0)write(jtty,220)                                              !/*md        108*/
  220 format(54h can't print iptal.  tally number is as yet undefined.)         !/*md        109*/
      return                                                                    !/*md        110*/
!                                                                               !/*md        111*/
! >>>>>  options. -- print a list of the available commands.                    !/*md        112*/
  230 inform=1                                                                  !/*md        113*/
      write(jtty,'(20h available commands:)')                                   !/*md        114*/
      n=(nkeys+2)/3                                                             !/*md        115*/
      do 240 i=1,n                                                              !/*md        116*/
  240 write(jtty,'(2x,3(10x,a8))')(keys(k),k=i,nkeys,n)                         !/*md        117*/
      return                                                                    !/*md        118*/
!                                                                               !/*md        119*/
! >>>>>  printal. -- print tally numbers.                                       !/*md        120*/
  250 inform=1                                                                  !/*md        121*/
      if(ntal.ne.0)write(jtty,260)(jptal(ljpt+1,i),i=1,ntal)                    !/*md        122*/
  260 format(15h tally numbers:/(10i5))                                         !/*md        123*/
      if(ntal.eq.0)write(jtty,270)                                              !/*md        124*/
  270 format(50h can't print tally numbers.  there are no tallies.)             !/*md        125*/
      return                                                                    !/*md        126*/
!                                                                               !/*md        127*/
! >>>>>  printpts. -- print the points instead of plotting them.                !/*md        128*/
  280 if(nfree.eq.1.and.koplot.eq.0)go to 310                                   !/*md        129*/
      if(nfree.eq.1)write(jtty,290)                                             !/*md        130*/
  290 format(32h can't print points in a coplot.)                               !/*md        131*/
      if(nfree.eq.2)write(jtty,300)                                             !/*md        132*/
  300 format(44h can't print points in a 3d or contour plot.)                   !/*md        133*/
      go to 9030                                                                !/*md        134*/
  310 iprpts=1                                                                  !/*md        135*/
      call nxtsym(klin,' ,=',jchar,it,iu,0)                                     !/*md4c        3*/
      if(iu.eq.0)return                                                         !/*md4c        4*/
      ha=klin(it:iu)                                                            !/*md4c        5*/
      jchar=iu+1                                                                !/*md4c        6*/
      if(ha.eq.'='.or.ha.eq.',')go to 310                                       !/*md4c        7*/
      call unique(ha,jtty)                                                      !/*md4b       23*/
      open(iupx,file=ha,status='new')                                           !/*md4b       24*/
      iprpts=2                                                                  !/*md4b       25*/
      return                                                                    !/*md        136*/
!                                                                               !/*md        137*/
! >>>>>  status. -- print the current state of the plot controls.               !/*md        138*/
  320 inform=1                                                                  !/*md        139*/
      call status(jtty)                                                         !/*md        140*/
      return                                                                    !/*md        141*/
!                                                                               !/*md        142*/
! >>>>>  runtpe. -- read a dump from a runtpe file.                             !/*md        143*/
  330 call nxtsym(klin,' ,=',jchar,it,iu,0)                                     !/*md4c        8*/
      if(iu.eq.0)go to 9010                                                     !/*md4c        9*/
      ha=klin(it:iu)                                                            !/*md4c       10*/
      jchar=iu+1                                                                !/*md4c       11*/
      if(ha.eq.'='.or.ha.eq.',')go to 330                                       !/*md4c       12*/
      if(inqire(ha(1:8)).ne.0)go to 340                                         !/*md        145*/
      write(jtty,'(8h runtpe ,a8,16h does not exist.)')ha                       !/*md        146*/
      go to 9020                                                                !/*md        147*/
  340 runtpe=ha                                                                 !/*md        148*/
      idmp=0                                                                    !/*md        149*/
      if(kdarg(klin,jchar,1,ha,r).gt.0)idmp=r                                   !/*md        150*/
      if(idmp.lt.2.and.idmp.ne.0)go to 9010                                     !/*md        151*/
      call tpefil(7)                                                            !/*md        152*/
      if(ktfile.ne.17)go to 9020                                                !/*md        153*/
      ktfile=1                                                                  !/*md        154*/
      mtal=min(1,ntal)                                                          !/*md        155*/
      return                                                                    !/*md        156*/
!                                                                               !/*md        157*/
! >>>>>  dump. -- read a dump from the current runtpe.                          !/*md        158*/
  350 if(ktfile.ne.1)go to 360                                                  !/*md        159*/
      if(kdarg(klin,jchar,0,ha,r).lt.1)go to 9010                               !/*md        160*/
      idmp=r                                                                    !/*md        161*/
      if(idmp.lt.2.and.idmp.ne.0)go to 9010                                     !/*md        162*/
      call tpefil(7)                                                            !/*md        163*/
      if(ktfile.ne.17)go to 9020                                                !/*md        164*/
      ktfile=1                                                                  !/*md        165*/
      return                                                                    !/*md        166*/
  360 write(jtty,370)                                                           !/*md        167*/
  370 format(54h the dump command may be used only with a runtpe file.)         !/*md        168*/
      go to 9020                                                                !/*md        169*/
!                                                                               !/*md        170*/
! >>>>>  wmctal. -- write tally data from runtpe to a mctal file.               !/*md        171*/
  380 inform=1                                                                  !/*md        172*/
      ha=mctal                                                                  !/*md4c       13*/
      call nxtsym(klin,' ,=',jchar,it,iu,0)                                     !/*md4c       14*/
      if(iu.ne.0)ha=klin(it:iu)                                                 !/*md4c       15*/
      if(iu.ne.0)jchar=iu+1                                                     !/*md4c       16*/
      if(ha.eq.'='.or.ha.eq.',')go to 380                                       !/*md4c       17*/
      if(ktfile.eq.1)call mctalw(ha(1:8))                                       !/*md        175*/
      if(ktfile.eq.1)write(jtty,390)runtpe,ha(1:8)                              !/*md        176*/
  390 format(22h tally data from file ,a8,17h written to file ,a8)              !/*md        177*/
      if(ktfile.ne.1)write(jtty,400)ha                                          !/*md        178*/
  400 format(12h mctal file ,a8,33h is not written because no runtpe,           !/*md        179*/
     & 17h dump is present.)                                                    !/*md        180*/
      return                                                                    !/*md        181*/
!                                                                               !/*md        182*/
! >>>>>  rmctal. -- read tally data from a mctal file.                          !/*md        183*/
  410 ha=mctal                                                                  !/*md4c       18*/
      call nxtsym(klin,' ,=',jchar,it,iu,0)                                     !/*md4c       19*/
      if(iu.eq.0)go to 9010                                                     !/*md4c       20*/
      ha=klin(it:iu)                                                            !/*md4c       21*/
      jchar=iu+1                                                                !/*md4c       22*/
      if(ha.eq.'='.or.ha.eq.',')go to 410                                       !/*md4c       23*/
      if(inqire(ha(1:8)).ne.0)go to 420                                         !/*md        185*/
      write(jtty,'(12h mctal file ,a8,16h does not exist.)')ha                  !/*md        186*/
      go to 9020                                                                !/*md        187*/
  420 mctal=ha                                                                  !/*md        188*/
      call mctalr(ln)                                                           !/*md4b       26*/
      if(ln.ne.0)go to 9020                                                     !/*md        190*/
      ktfile=2                                                                  !/*md        191*/
      return                                                                    !/*md        192*/
!                                                                               !/*md        193*/
! >>>>>  tally.                                                                 !/*md        194*/
  430 if(kdarg(klin,jchar,0,ha,r).lt.1)go to 9010                               !/*md        195*/
      if(ktfile.eq.0)go to 450                                                  !/*md        196*/
      i=r                                                                       !/*md        197*/
      do 440 it=1,ntal                                                          !/*md        198*/
  440 if(i.eq.jptal(ljpt+1,it))go to 460                                        !/*md        199*/
  450 write(jtty,'(10h no tally ,a8,8h exists.)')ha                             !/*md        200*/
      go to 9020                                                                !/*md        201*/
  460 mtal=it                                                                   !/*md        202*/
      return                                                                    !/*md        203*/
!                                                                               !/*md        204*/
! >>>>>  nonorm.                                                                !/*md        205*/
  470 nonorm=1                                                                  !/*md        206*/
      return                                                                    !/*md        207*/
!                                                                               !/*md        208*/
! >>>>>  factor.                                                                !/*md        209*/
  480 k=kdarg(klin,jchar,0,ha,r)                                                !/*md        210*/
      i=(index('x y z ',ha(1:2))+1)/2                                           !/*md        211*/
      if(i.eq.0)go to 9010                                                      !/*md        212*/
      if(kdarg(klin,jchar,0,ha,r).le.0)go to 9010                               !/*md        213*/
      if(r.eq.0.)go to 9010                                                     !/*md4b       27*/
      scalf(1,i)=r                                                              !/*md        215*/
      k=kdarg(klin,jchar,1,ha,scalf(2,i))                                       !/*md        216*/
      return                                                                    !/*md        217*/
!                                                                               !/*md        218*/
! >>>>>  reset.                                                                 !/*md        219*/
  490 if(kdarg(klin,jchar,0,ha,r).lt.0)go to 9010                               !/*md        220*/
      call nxtsym(ha,' ',1,i,j,2)                                               !/*md4a       10*/
      ks=0                                                                      !/*md        221*/
      if(ha.eq.'all')ha='reset'                                                 !/*md        222*/
      lc=leng(ha)                                                               !/*md        223*/
      do 500 k=1,nkeys                                                          !/*md        224*/
      if(keys(k)(1:lc).ne.ha)go to 500                                          !/*md        225*/
      if(ks.ne.0)ks=-1                                                          !/*md        226*/
      if(ks.eq.0)ks=k                                                           !/*md        227*/
  500 continue                                                                  !/*md        228*/
      if(ks.lt.16.and.ks.ne.5)go to 9010                                        !/*md        229*/
      if(koplot.ne.0.and.legalc(ks).eq.0)go to 510                              !/*md        230*/
      call mreset(ks)                                                           !/*md        231*/
      return                                                                    !/*md        232*/
  510 write(jtty,520)ha                                                         !/*md        233*/
  520 format(6hreset ,a8,37h is not legal after a coplot command.)              !/*md        234*/
      go to 9020                                                                !/*md        235*/
!                                                                               !/*md        236*/
! >>>>>  title or subtitle or xtitle or ytitle or ztitle or label.              !/*md        237*/
  530 i=index('xyzltqs',hm(1:1))                                                !/*md        238*/
      if(i.ne.5)go to 540                                                       !/*md        239*/
      if(kdarg(klin,jchar,0,ha,r).ne.2)go to 9010                               !/*md        240*/
      i=4+r                                                                     !/*md        241*/
      if(i.lt.5.or.i.gt.6)go to 9010                                            !/*md        242*/
  540 if(i.ne.7)go to 550                                                       !/*md        243*/
      if(kdarg(klin,jchar,0,ha,xst).lt.1)go to 9010                             !/*md        244*/
      if(kdarg(klin,jchar,0,ha,yst).lt.1)go to 9010                             !/*md        245*/
  550 ha=klin(min(jchar,80):80)                                                 !/*md        246*/
      ip=index(klin(min(jchar,80):80),'"')+jchar-1                              !/*md        247*/
      if(ip.eq.jchar-1.or.ip.gt.78)go to 9010                                   !/*md        248*/
      iq=index(klin(ip+1:80),'"')+ip                                            !/*md        249*/
      if(iq.lt.ip+2.or.i.ge.5.and.iq-ip.gt.41)go to 9010                        !/*md        250*/
      titles(i)=klin(ip+1:iq-1)                                                 !/*md        251*/
      jchar=iq+1                                                                !/*md        252*/
      ititle(i)=1                                                               !/*md        253*/
      return                                                                    !/*md        254*/
!                                                                               !/*md        255*/
! >>>>>  below. -- put the title below the plot.                                !/*md        256*/
  560 lput=1                                                                    !/*md        257*/
      return                                                                    !/*md        258*/
!                                                                               !/*md        259*/
! >>>>>  free.                                                                  !/*md        260*/
  570 ititle(1)=0                                                               !/*md        261*/
      ititle(2)=0                                                               !/*md        262*/
      ititle(3)=0                                                               !/*md        263*/
      itfc=0                                                                    !/*md        264*/
      jlim(1)=0                                                                 !/*md        265*/
      jlim(2)=0                                                                 !/*md        266*/
      kbin(ifree(1),1)=0                                                        !/*md        267*/
      if(nfree.eq.2)kbin(ifree(2),1)=0                                          !/*md        268*/
      kurv=0                                                                    !/*md        269*/
      if(kdarg(klin,jchar,0,ha,r).lt.0)go to 9010                               !/*md        270*/
      call nxtsym(ha,' ',1,i,j,2)                                               !/*md4a       11*/
      if(leng(ha).gt.2)go to 9010                                               !/*md        271*/
      jm=index(ibin,ha(1:1))                                                    !/*md        272*/
      km=index(ibin,ha(2:2))                                                    !/*md        273*/
      if(jm.eq.0.or.ha(2:2).ne.' '.and.km.eq.0.or.jm.eq.km)go to 9010           !/*md        274*/
      ifree(1)=max(jm,km)                                                       !/*md        275*/
      ifree(2)=min(jm,km)                                                       !/*md        276*/
      nfree=1                                                                   !/*md        277*/
      if(km.eq.0)return                                                         !/*md        278*/
      nfree=2                                                                   !/*md        279*/
      return                                                                    !/*md        280*/
!                                                                               !/*md        281*/
! >>>>>  fixed.                                                                 !/*md        282*/
  580 if(kdarg(klin,jchar,0,ha,r).lt.0)go to 9010                               !/*md        283*/
      call nxtsym(ha,' ',1,i,j,2)                                               !/*md4a       12*/
      i=index(ibin,ha(1:1))                                                     !/*md        284*/
      if(i.eq.0)go to 9010                                                      !/*md        285*/
      if(nfree.eq.2.and.ifree(2).eq.i)nfree=1                                   !/*md        286*/
      if(ifree(1).eq.i)ifree(1)=7                                               !/*md        287*/
      if(ifree(1).eq.i)ifree(1)=8                                               !/*md        288*/
      if(kdarg(klin,jchar,0,ha,r).lt.1)go to 9010                               !/*md        289*/
      if(r.lt.0.)go to 9010                                                     !/*md        290*/
      kbin(i,1)=r                                                               !/*md        291*/
      return                                                                    !/*md        292*/
!                                                                               !/*md        293*/
! >>>>>  set.                                                                   !/*md        294*/
  590 ititle(1)=0                                                               !/*md        295*/
      ititle(2)=0                                                               !/*md        296*/
      ititle(3)=0                                                               !/*md        297*/
      itfc=0                                                                    !/*md        298*/
      jlim(1)=0                                                                 !/*md        299*/
      jlim(2)=0                                                                 !/*md        300*/
      kurv=0                                                                    !/*md        301*/
      n=0                                                                       !/*md        302*/
      do 600 j=1,8                                                              !/*md        303*/
      k=kdarg(klin,jchar,0,ha,r)                                                !/*md        304*/
      if(k.lt.0)go to 9010                                                      !/*md        305*/
      kbin(j,1)=r                                                               !/*md        306*/
      if(k.gt.0)go to 600                                                       !/*md        307*/
      if(ha.ne.'*')go to 9010                                                   !/*md        308*/
      n=n+1                                                                     !/*md        309*/
      if(n.le.2)ifree(n)=j                                                      !/*md        310*/
  600 continue                                                                  !/*md        311*/
      if(n.ne.1.and.n.ne.2)go to 9010                                           !/*md        312*/
      nfree=n                                                                   !/*md        313*/
      return                                                                    !/*md        314*/
!                                                                               !/*md        315*/
! >>>>>  tfc.                                                                   !/*md        316*/
  610 k=kdarg(klin,jchar,0,ha,r)                                                !/*md        317*/
      call nxtsym(ha,' ',1,i,j,2)                                               !/*md4a       13*/
      i=(index('m e f v s p n t l 1 2 3 4 5 6 7 8 1c2c3c4c5c6c7c8c',            !/*md4a       14*/
     & ha(1:2))+1)/2                                                            !/*md4a       15*/
      if(i.eq.0)go to 9010                                                      !/*md        319*/
      itfc=i                                                                    !/*md        320*/
      return                                                                    !/*md        321*/
!                                                                               !/*md        322*/
! >>>>>  kcode.                                                                 !/*md        323*/
  620 if(nsr.eq.71)go to 630                                                    !/*md        324*/
      write(jtty,'(29h this is not a kcode problem.)')                          !/*md        325*/
      go to 9020                                                                !/*md        326*/
  630 k=kdarg(klin,jchar,0,ha,r)                                                !/*md        327*/
      i=r+30.                                                                   !/*md4a       16*/
      if(i.lt.31.or.(i.gt.35.and.i.lt.41).or.i.gt.34+mkpl)go to 9010            !/*md4c       24*/
      itfc=i                                                                    !/*md        330*/
      return                                                                    !/*md        331*/
!                                                                               !/*md        332*/
! >>>>>  linlin, linlog, loglin, or loglog.                                     !/*md        333*/
  640 lax=(index('nlinlogliglo',hm(3:5))+2)/3                                   !/*md        334*/
      return                                                                    !/*md        335*/
!                                                                               !/*md        336*/
! >>>>>  xlims or ylims.                                                        !/*md        337*/
!        negative limits on log axes are rejected.                              !/*md4c       25*/
!        limit range of log axes to 16 orders of magnitude.                     !/*md4c       26*/
  650 if(kdarg(klin,jchar,0,ha,x).lt.1)go to 9010                               !/*md        338*/
      if(kdarg(klin,jchar,0,ha,y).lt.1)go to 9010                               !/*md        339*/
      if(x.eq.y)go to 9010                                                      !/*md        340*/
      i=index('xy',hm(1:1))                                                     !/*md        341*/
      if(x.lt.0..and.i.eq.1.and.lax.gt.2)go to 9010                             !/*md4c       27*/
      if(y.lt.0..and.i.eq.2.and.(mod(lax,2).eq.0))go to 9010                    !/*md4c       28*/
      jlim(i)=1                                                                 !/*md        342*/
      xyzmx(i)=max(x,y)                                                         !/*md4b       28*/
      xyzmn(i)=min(x,y)                                                         !/*md4c       29*/
      if((i.eq.1.and.lax.gt.2).or.(i.eq.2.and.mod(lax,2).eq.0))                 !/*md4c       30*/
     & xyzmn(i)=max(1e-16*xyzmx(i),xyzmn(i))                                    !/*md4c       31*/
      itik(i)=0                                                                 !/*md        345*/
      if(kdarg(klin,jchar,1,ha,r).lt.0)return                                   !/*md        346*/
      if(r.ge.1.)itik(i)=r                                                      !/*md        347*/
      return                                                                    !/*md        348*/
!                                                                               !/*md        349*/
! >>>>>  scales.                                                                !/*md        350*/
  660 if(kdarg(klin,jchar,0,ha,r).lt.1)go to 9010                               !/*md        351*/
      i=r                                                                       !/*md        352*/
      if(i.lt.0.or.i.gt.2)go to 9010                                            !/*md        353*/
      mscal=i                                                                   !/*md        354*/
      return                                                                    !/*md        355*/
!                                                                               !/*md        356*/
! >>>>>  hist or plinear or bar.                                                !/*md        357*/
  670 kurv=index('hpsb',hm(1:1))                                                !/*md        358*/
      return                                                                    !/*md        359*/
!                                                                               !/*md        360*/
! >>>>>  spline. -- also set the tension if it is specified.                    !/*md        361*/
  680 if(kdarg(klin,jchar,1,ha,tensn).lt.0)go to 700                            !/*md        362*/
#ifndef DISSPLA                                                                 /*!md        363*/
      write(jtty,690)                                                           !/*md        364*/
  690 format(34h rational spline is not available.)                             !/*md        365*/
      go to 9020                                                                !/*md        366*/
#else                                                                           /*!mdx         8*/
      if(tensn.lt.0.)tensn=3.                                                   !/*md        368*/
#endif                                                                          /*!cpp       294*/
  700 kurv=3                                                                    !/*md        369*/
      return                                                                    !/*md        370*/
!                                                                               !/*md        371*/
! >>>>>  noerrbar.                                                              !/*md        372*/
  710 noerbr=1                                                                  !/*md        373*/
      return                                                                    !/*md        374*/
!                                                                               !/*md        375*/
! >>>>>  thick.                                                                 !/*md        376*/
  720 if(kdarg(klin,jchar,0,ha,r).lt.1)go to 9010                               !/*md        377*/
      if(r.lt..01.or.r.gt..1)go to 9010                                         !/*md        378*/
      cthick=r                                                                  !/*md        379*/
      return                                                                    !/*md        380*/
!                                                                               !/*md        381*/
! >>>>>  thin.                                                                  !/*md        382*/
  730 cthick=.01                                                                !/*md        383*/
      return                                                                    !/*md        384*/
!                                                                               !/*md        385*/
! >>>>>  legend.                                                                !/*md        386*/
  740 if(kdarg(klin,jchar,1,ha,r).ge.0)go to 750                                !/*md        387*/
      legend=1                                                                  !/*md        388*/
      return                                                                    !/*md        389*/
  750 if(kdarg(klin,jchar,1,ha,s).ge.0)go to 760                                !/*md        390*/
      if(r.ne.0.)go to 9010                                                     !/*md        391*/
      legend=0                                                                  !/*md        392*/
      return                                                                    !/*md        393*/
  760 legend=2                                                                  !/*md        394*/
      xlg=r                                                                     !/*md        395*/
      ylg=s                                                                     !/*md        396*/
      return                                                                    !/*md        397*/
!                                                                               !/*md        398*/
! >>>>>  contour.                                                               !/*md        399*/
  770 do 780 i=1,3                                                              !/*md        400*/
  780 if(kdarg(klin,jchar,0,ha,tpp(i)).le.0)go to 9010                          !/*md        401*/
      contur(1)=min(tpp(1),tpp(2))                                              !/*md        402*/
      contur(2)=max(tpp(1),tpp(2))                                              !/*md        403*/
      contur(3)=tpp(3)                                                          !/*md        404*/
      if((mclevs-1)*contur(3).lt.contur(2)-contur(1))go to 790                  !/*md        405*/
      j=jchar                                                                   !/*md        406*/
      k=kdarg(klin,jchar,0,ha,r)                                                !/*md        407*/
      ipct=1                                                                    !/*md        408*/
      if(ha.eq.'%')return                                                       !/*md        409*/
      ipct=0                                                                    !/*md        410*/
      jchar=j                                                                   !/*md        411*/
      return                                                                    !/*md        412*/
  790 write(jtty,'(25h too many contour levels.)')                              !/*md        413*/
      go to 9020                                                                !/*md        414*/
!                                                                               !/*md        415*/
! >>>>>  3d.                                                                    !/*md        416*/
#ifndef DISSPLA                                                                 /*!md        417*/
  800 write(jtty,810)                                                           !/*md        418*/
  810 format(46h 3d plotting is not available without disspla.)                 !/*md        419*/
      go to 9020                                                                !/*md        420*/
#else                                                                           /*!mdx         9*/
  800 j3d=1                                                                     !/*md        422*/
      return                                                                    !/*md        423*/
#endif                                                                          /*!cpp       296*/
!                                                                               !/*md        424*/
! >>>>>  view.                                                                  !/*md        425*/
  820 do 830 i=1,2                                                              !/*md        426*/
  830 if(kdarg(klin,jchar,0,ha,pht(i)).lt.1)go to 9010                          !/*md        427*/
      return                                                                    !/*md        428*/
!                                                                               !/*md4b       30*/
! >>>>>  pert.                                                                  !/*md4b       31*/
  840 k=kdarg(klin,jchar,0,ha,r)                                                !/*md4b       32*/
      if(k.gt.0)go to 860                                                       !/*md4b       33*/
      inform=1                                                                  !/*md4b       34*/
      write(jtty,'(25h available perturbations:)')                              !/*md4b       35*/
      n=(npert+9)/10                                                            !/*md4b       36*/
      do 850 i=1,n                                                              !/*md4b       37*/
  850 write(jtty,'(2x,10(2x,i5))')(iptb(lipb+1,k),k=i,npert,n)                  !/*md4b       38*/
      return                                                                    !/*md4b       39*/
  860 if(k.eq.1)go to 9010                                                      !/*md4b       40*/
      i=r                                                                       !/*md4b       41*/
      ip=0                                                                      !/*md4b       42*/
      if(i.eq.0)go to 880                                                       !/*md4b       43*/
      do 870 ip=1,npert                                                         !/*md4b       44*/
  870 if(i.eq.iptb(lipb+1,ip))go to 880                                         !/*md4b       45*/
      write(jtty,'(14h perturbation ,a,16h does not exist.)')                   !/*md4b       46*/
     & ha(1:leng(ha))                                                           !/*md4b       47*/
      go to 9020                                                                !/*md4b       48*/
  880 lpert=ip                                                                  !/*md4b       49*/
      return                                                                    !/*md4b       50*/
!                                                                               !/*md4b       51*/
! >>>>>  xs - cross section plot.                                               !/*md4b       52*/
  900 if(kdarg(klin,jchar,0,ha,r).lt.0)go to 9010                               !/*md4b       53*/
      if(mxe.eq.0)write(jtty,'(25h no materials. void card?)')                  !/*md4b       54*/
      if(mxe.eq.0)go to 9020                                                    !/*md4b       55*/
!                                                                               !/*md4b       56*/
!        initialize all the variables needed for an xs plot.                    !/*md4b       57*/
!        set kxsplt=1 (number of nuclides in mat. or 1 for single zaid.)        !/*md4b       58*/
!            kxspma=0 material number from inp card.                            !/*md4b       59*/
!            kxspmt=first mt value.                                             !/*md4b       60*/
!            kxspkm=pointer to zaids in a material.                             !/*md4b       61*/
!            kxspar=nips = source particle default type.                        !/*md4b       62*/
!            kxsmat=1 first material in nmat array.                             !/*md4b       63*/
!            lax=4    xs plots are defaulted to loglog.                         !/*md4b       64*/
      xspttl=' '                                                                !/*md4b       65*/
      kxspkm=0                                                                  !/*md4b       66*/
      kxsplt=1                                                                  !/*md4b       67*/
      kxspma=0                                                                  !/*md4b       68*/
      if(kxspar.eq.0)kxspar=nips                                                !/*md4b       69*/
      if(kxspmt.eq.0.and.kxspar.eq.2.and.mcal.eq.0)kxspmt=-5                    !/*md4b       70*/
      if(kxspmt.eq.0.and.kxspar.eq.3)kxspmt=3                                   !/*md4b       71*/
      if(kxspmt.eq.0)kxspmt=-1                                                  !/*md4b       72*/
      kxsmat=1                                                                  !/*md4b       73*/
      itfc=0                                                                    !/*md4b       74*/
      noerbr=1                                                                  !/*md4b       75*/
      kurv=2                                                                    !/*md4b       76*/
      if(cthick.eq.0.02)cthick=.01                                              !/*md4b       77*/
!                                                                               !/*md4b       78*/
!        single nuclide plot.  right justify the zaid.                          !/*md4b       79*/
      call nxtsym(ha,' ',1,iu,ju,1)                                             !/*md4b       80*/
      if(ju.le.0.or.ju.gt.10)go to 9010                                         !/*md4b       81*/
      xspttl(11-ju:10)=ha(1:ju)                                                 !/*md4b       82*/
      if(ha(1:1).eq.'m')go to 910                                               !/*md4b       83*/
      if(ha(1:1).ne.'?')return                                                  !/*md4b       84*/
!                                                                               !/*md4b       85*/
!        primer on xs plotting.                                                 !/*md4b       86*/
      j=jtty                                                                    !/*md4b       87*/
      call xsprmr(j)                                                            !/*md4b       88*/
      inform=1                                                                  !/*md4b       89*/
      return                                                                    !/*md4b       90*/
!                                                                               !/*md4b       91*/
!        material plot.                                                         !/*md4b       92*/
!        get material number, kxspma, from the input.                           !/*md4b       93*/
  910 if(kdata(ha(2:ju)).ne.2)go to 9010                                        !/*md4b       94*/
      read(ha,'(1x,bn,i10)')kxspma                                              !/*md4b       95*/
      return                                                                    !/*md4b       96*/
!                                                                               !/*md4b       97*/
! >>>>>  mt - reaction number for xs plot.                                      !/*md4b       98*/
  920 if(kdarg(klin,jchar,1,ha,r).lt.2)go to 9010                               !/*md4b       99*/
      kxspmt=r                                                                  !/*md4b      100*/
      if(kxsplt.eq.0)write(jtty,930)                                            !/*md4b      101*/
  930 format(47h reaction numbers are only valid with xs plots.)                !/*md4b      102*/
      if(kxsplt.eq.0)go to 9020                                                 !/*md4b      103*/
      return                                                                    !/*md4b      104*/
!                                                                               !/*md4b      105*/
! >>>>>  par - particle type for xs plot.                                       !/*md4b      106*/
  940 k=kdarg(klin,jchar,0,ha,r)                                                !/*mdxl        3*/
      if(k.lt.0)go to 9010                                                      !/*mdxl        4*/
      call nxtsym(ha,' ',1,iu,ju,1)                                             !/*mdxl        5*/
      if(k.eq.0)kxspar=index(hip,ha(1:1))                                       !/*mdxl        6*/
      if(k.gt.0)kxspar=nint(r)                                                  !/*mdxl        7*/
      kxspmt=-1                                                                 !/*md4b      113*/
      if(kxspar.eq.2.and.mcal.eq.0)kxspmt=-5                                    !/*md4b      114*/
      if(kxspar.eq.3)kxspmt=3                                                   !/*md4b      115*/
      if((kxspar.gt.0.and.kxspar.lt.4).or.kxspar.eq.9)return                    !/*mdxl        8*/
      write(jtty,960)kxspar                                                     !/*md4b      117*/
  960 format(i2,1x,'is an illegal particle type')                               !/*md4b      118*/
      go to 9010                                                                !/*md4b      119*/
!                                                                               !/*mdxl        9*/
! >>>>>  pause nj.  hold picture for nj seconds.                                !/*mdxl       10*/
  970 if(kdarg(klin,jchar,1,ha,r).le.0)go to 990                                !/*mdxl       11*/
      nj=r                                                                      !/*mdxl       12*/
      if(nj.le.0)go to 990                                                      !/*mdxl       13*/
      call getidt(ha)                                                           !/*mdxl       14*/
      read(ha,'(11x,i2,1x,i2,1x,i2)')jh,jm,js                                   !/*mdxl       15*/
      ks=3600*jh+60*jm+js                                                       !/*mdxl       16*/
      n=-1000000                                                                !/*mdxl       17*/
  980 call getidt(ha)                                                           !/*mdxl       18*/
      read(ha,'(11x,i2,1x,i2,1x,i2)')jh,jm,js                                   !/*mdxl       19*/
      ls=3600*jh+60*jm+js                                                       !/*mdxl       20*/
      if(irup.ne.0)call qttyin(nj-ls+ks,'7hpausing,i5,8hseconds.')              !/*mdxl       21*/
      if(ls-ks.ge.nj)go to 1000                                                 !/*mdxl       22*/
      n=n+1                                                                     !/*mdxl       23*/
      if(n.gt.1000000.and.ls.le.ks)go to 1000                                   !/*mdxl       24*/
      go to 980                                                                 !/*mdxl       25*/
  990 write(jtty,'(27hstrike any key to continue.)')                            !/*mdxl       26*/
      read(itty,'(a8)',end=1000)                                                !/*mdxl       27*/
 1000 return                                                                    !/*mdxl       28*/
!                                                                               !/*md        429*/
!        common complaints.                                                     !/*md        430*/
 9010 write(jtty,'(17h bad argument of ,a8,11h command:  ,a8)')hm,ha            !/*md        431*/
#ifdef LAHEY                                                                    /*!md4b      120*/
 9020 if(koplot.ne.0)call WindowClose                                           !/*md4c       32*/
      write(jtty,'(41h the rest of the command line is ignored.)')              !/*md4b      122*/
#else                                                                           /*!mdx        10*/
 9020 write(jtty,'(41h the rest of the command line is ignored.)')              !/*md        432*/
#endif                                                                          /*!cpp       298*/
      ln=1                                                                      !/*md        433*/
 9030 inform=1                                                                  !/*md        434*/
      if(mrm.eq.1.and.kmplot.eq.0)mdecod=1                                      !/*md4b      124*/
      koplot=0                                                                  !/*md        435*/
#else                                                                           /*!mdx        11*/
      mdecod=0                                                                  !/*mdx        12*/
#endif                                                                          /*!mdx        13*/
      return                                                                    !/*md        436*/
      end                                                                       !/*md        437*/

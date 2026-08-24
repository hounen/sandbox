!#ident chkxss.F from deck cb4b                                                 !/*cb4b        1*/
      subroutine chkxss                                                         !/*cb4b        3*/
#ifdef MCPLOT                                                                   /*!cbx         1*/
!        subroutine to check and set up an xs plot.                             !/*cb4b        4*/
      use global_data                                                           !/*cbx         2*/
      use mcplot_data                                                           !/*cbx         3*/
!                                                                               !/*cbx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*cbx         5*/
      implicit integer(kindi) (i-n)                                             !/*cbx         6*/
!                                                                               !/*cbx         7*/
      character hz*10                                                           !/*cb4b        7*/
!                                                                               !/*cb4b        8*/
!        do an input check and set up.                                          !/*cb4b        9*/
!        check requests for xs plots and if valid, define                       !/*cb4b       10*/
!        number of points in xs plot and set up pointers.                       !/*cb4b       11*/
      if(jovr(1)+jovr(3)+jovr(5).ge.3)go to 20                                  !/*cb4b       12*/
      write(jtty,10)                                                            !/*cb4b       13*/
   10 format(' must have ixz in execute options for an xs plot')                !/*cb4b       14*/
      go to 9090                                                                !/*cb4b       15*/
!                                                                               !/*cb4b       16*/
!        check consistency of particle type, mode and class.                    !/*cb4b       17*/
!        this is a check of availability of data, not on the sanity             !/*cb4b       18*/
!        of the plot request yet.                                               !/*cb4b       19*/
   20 if(kxspma.eq.0)go to 190                                                  !/*cb4b       20*/
      if(kxspar.ne.9)kxspar=min(3,max(1,kxspar))                                !/*cbxl        1*/
      if(kpt(kxspar).ne.0)go to 60                                              !/*cbxl        2*/
      if(kxspar.eq.3.and.kpt(2).ne.0.and.ides.eq.0)go to 60                     !/*cbxl        3*/
      write(jtty,'(1x,a11,34h data not loaded.  cannot plot xs.)')              !/*cbxl        4*/
     & hnp(kxspar)                                                              !/*cbxl        5*/
      go to 9090                                                                !/*cb4b       30*/
!                                                                               !/*cb4b       31*/
!        this is a material plot.                                               !/*cb4b       32*/
!        search for material number in material list.                           !/*cb4b       33*/
   60 do 70 i=1,nmat                                                            !/*cb4b       34*/
   70 if(nmt(lnmt+i).eq.kxspma)go to 110                                        !/*cb4b       35*/
!                                                                               !/*cb4b       36*/
!        failed to find material in list.  print allowed list.                  !/*cb4b       37*/
      write(jtty,80)kxspma                                                      !/*cb4b       38*/
   80 format(' material ',i10,' is not in the material list '/                  !/*cb4b       39*/
     & 23h allowed materials are:)                                              !/*cb4b       40*/
      do 90 i=1,nmat                                                            !/*cb4b       41*/
   90 write(jtty,100)nmt(lnmt+i)                                                !/*cb4b       42*/
  100 format(10x,1hm,i4)                                                        !/*cb4b       43*/
      go to 9090                                                                !/*cb4b       44*/
!                                                                               !/*cb4b       45*/
!        material found.                                                        !/*cb4b       46*/
  110 kxsmat=i                                                                  !/*cb4b       47*/
      kxspkm=jmd(ljmd+i)-1                                                      !/*cb4b       48*/
      kxsplt=jmd(ljmd+i+1)-jmd(ljmd+i)                                          !/*cb4b       49*/
!                                                                               !/*cb4b       50*/
!        create list of iex values.                                             !/*cb4b       51*/
      if(kxspar.eq.2.and.kxsptp.eq.9)go to 170                                  !/*cbxl        6*/
      do 160 i=1,kxsplt                                                         !/*cb4b       52*/
      ie=lme(llme+kxspar,kxspkm+i)                                              !/*cb4b       53*/
      if(ie.gt.0)go to 160                                                      !/*cb4b       54*/
      do 120 ip=1,mipt                                                          !/*cb4b       55*/
  120 if(lme(llme+ip,kxspkm+i).ne.0)go to 140                                   !/*cb4b       56*/
      write(jtty,130)kxspma                                                     !/*cb4b       57*/
  130 format(9h material,i5,10h not used.)                                      !/*cb4b       58*/
      go to 9090                                                                !/*cb4b       59*/
  140 ie=lme(llme+ip,kxspkm+i)                                                  !/*cb4b       60*/
      call zaid(2,hz,ixl(lixl+1,ie))                                            !/*cb4b       61*/
      write(jtty,150)hz,hip(kxspar:kxspar)                                      !/*cb4b       62*/
  150 format(1x,a10,32h data type conflicts with par = ,a1)                     !/*cb4b       63*/
      go to 9090                                                                !/*cb4b       64*/
  160 kxspie(lxie+i)=ie                                                         !/*cb4b       65*/
      go to 260                                                                 !/*cb4b       66*/
!                                                                               !/*cbxl        7*/
!        create list of iex values for photonuclear data.                       !/*cbxl        8*/
  170 k=kxsplt                                                                  !/*cbxl        9*/
      kxsplt=0                                                                  !/*cbxl       10*/
      do 180 ii=1,k                                                             !/*cbxl       11*/
      ie=lmn(llmn+kxspkm+ii)                                                    !/*cbxl       12*/
      if(ie.eq.0)go to 180                                                      !/*cbxl       13*/
      kxsplt=kxsplt+1                                                           !/*cbxl       14*/
      kxspie(lxie+kxsplt)=ie                                                    !/*cbxl       15*/
  180 continue                                                                  !/*cbxl       16*/
      if(kxsplt.gt.0)go to 260                                                  !/*cbxl       17*/
      write(jtty,'(34hphotonuclear data not found, mat =,i5)')kxspma            !/*cbxl       18*/
      go to 9090                                                                !/*cbxl       19*/
!                                                                               !/*cb4b       67*/
!        single nuclide info here.                                              !/*cb4b       68*/
!        search for given zaid(s) in material libraries.                        !/*cb4b       69*/
  190 do 200 ie=1,mxe                                                           !/*cb4b       70*/
      call zaid(2,hz,ixl(lixl+1,ie))                                            !/*cb4b       71*/
  200 if(hz.eq.xspttl)go to 250                                                 !/*cb4b       72*/
!                                                                               !/*cb4b       73*/
!        cannot find zaid in libraries.  print zaid list.                       !/*cb4b       74*/
      write(jtty,210)xspttl                                                     !/*cb4b       75*/
  210 format(17h can't find zaid ,a10,3x,16havailable zaids:)                   !/*cb4b       76*/
      do 220 ie=1,mxe                                                           !/*cb4b       77*/
      call zaid(2,hz,ixl(lixl+1,ie))                                            !/*cb4b       78*/
  220 write(jtty,230)hz                                                         !/*cb4b       79*/
  230 format(17x,a10)                                                           !/*cb4b       80*/
      if(kpt(3).ne.0.or.kpt(2).ne.0.and.ides.eq.0)write(jtty,240)               !/*cb4b       81*/
  240 format(' if electrons have been loaded, they will have'/                  !/*cb4b       82*/
     & ' a similar zaid set as the photons, with p changed to e.')              !/*cb4b       83*/
      go to 9090                                                                !/*cb4b       84*/
!                                                                               !/*cb4b       85*/
!        zaid or material found, now test data type, kxsptp,                    !/*cb4b       86*/
!        and compatibility with particle type, kxspar,                          !/*cb4b       87*/
!        branch based on type of data.                                          !/*cb4b       88*/
  250 kxspie(lxie+1)=ie                                                         !/*cb4b       89*/
  260 kxsptp=nty(lnty+kxspie(lxie+1))                                           !/*cb4b       90*/
      k=kxspar                                                                  !/*cb4b       91*/
      if(kxsptp.le.4.or.kxsptp.eq.6)kxspar=1                                    !/*cbx         9*/
      if(kxsptp.eq.5.or.kxsptp.eq.7.or.kxsptp.eq.9)kxspar=2                     !/*cbx        10*/
      if(kxsptp.eq.10)kxspar=3                                                  !/*cbx        11*/
      if(kxsptp.eq.8)kxspar=9                                                   !/*cbxl       20*/
      if(k.ne.kxspar)write(jtty,'(26h setting particle type to ,a1)')           !/*cb4b       95*/
     & hip(kxspar:kxspar)                                                       !/*cb4b       96*/
      npt(1)=0                                                                  !/*cbxl       21*/
      go to(270,270,270,560,630,430,430,650,670,690)kxsptp                      !/*cbxl       22*/
      go to 9090                                                                !/*cb4b       98*/
!                                                                               !/*cb4b       99*/
! >>>>>  load neutron pointers.                                                 !/*cb4b      100*/
!        continuous, discrete or dosimetry data class.                          !/*cb4b      101*/
!        branch based on mt number or dosimetry class.                          !/*cb4b      102*/
  270 if(kxspmt.eq.0.or.kxspmt.lt.-6)go to 750                                  !/*cb4b      103*/
      if(kxspmt.eq.1.and.kxsptp.ne.3)kxspmt=-1                                  !/*cb4b      104*/
      if(kxspmt.gt.0)go to 350                                                  !/*cb4b      105*/
      if(kxspmt.le.-5)go to 300                                                 !/*cb4b      106*/
!                                                                               !/*cb4b      107*/
!        mt is -1,-2,-3 or -4.                                                  !/*cb4b      108*/
!        sum up points for all given zaids and set energy                       !/*cb4b      109*/
!        and cross section pointers.                                            !/*cb4b      110*/
      kf=0                                                                      !/*cb4b      112*/
      do 290 i=1,kxsplt                                                         !/*cb4b      113*/
      kxsptp=nty(lnty+kxspie(lxie+i))                                           !/*cb4b      114*/
      kxspen(lxen+i)=-1                                                         !/*cb4b      115*/
      kxspxs(lxxs+i)=-1                                                         !/*cb4b      116*/
      if(nhtfl(lnht+kxspie(lxie+i)).ne.0.or.kxspmt.ne.-4)go to 280              !/*cb4b      117*/
      call zaid(2,hz,ixl(lixl+1,kxspie(lxie+i)))                                !/*cb4b      118*/
      write(jtty,'(22h heating expunged for ,a10)')hz                           !/*cb4b      119*/
      go to 9090                                                                !/*cb4b      120*/
  280 kf=1                                                                      !/*cb4b      121*/
      ne=nxs(lnxs+3,kxspie(lxie+i))                                             !/*cb4b      122*/
      npt(1)=npt(1)+ne                                                          !/*cb4b      123*/
      if(kxsptp.eq.2)npt(1)=npt(1)+ne                                           !/*cb4b      124*/
      j1=jxs(ljxs+1,kxspie(lxie+i))-1                                           !/*cb4b      125*/
      kxspen(lxen+i)=j1                                                         !/*cb4b      126*/
      kxspxs(lxxs+i)=j1-kxspmt*ne                                               !/*cb4b      127*/
  290 kxspnx(lxnx+i)=ne                                                         !/*cb4b      128*/
      if(kf.eq.1)go to 700                                                      !/*cb4b      129*/
      write(jtty,'(21h could not find data )')                                  !/*cb4b      130*/
      go to 9090                                                                !/*cb4b      131*/
!                                                                               !/*cb4b      132*/
!        mt = -5, -6 :  gamma production, total fission.                        !/*cb4b      133*/
  300 kf=0                                                                      !/*cbxl       23*/
      do 340 i=1,kxsplt                                                         !/*cb4b      136*/
      kxsptp=nty(lnty+kxspie(lxie+i))                                           !/*cb4b      137*/
      kxspen(lxen+i)=-1                                                         !/*cb4b      138*/
      kxspxs(lxxs+i)=-1                                                         !/*cb4b      139*/
      if(kxspmt.eq.-6)go to 320                                                 !/*cb4b      140*/
      if(ngmfl(lngm+kxspie(lxie+i)).ne.0)go to 310                              !/*cb4b      141*/
      call zaid(2,hz,ixl(lixl+1,kxspie(lxie+i)))                                !/*cb4b      142*/
      write(jtty,'(32h photon production expunged for ,a10)')hz                 !/*cb4b      143*/
      go to 340                                                                 !/*cb4b      144*/
  310 if(jxs(ljxs+12,kxspie(lxie+i)).eq.0)go to 340                             !/*cb4b      145*/
      ne=nxs(lnxs+3,kxspie(lxie+i))                                             !/*cb4b      146*/
      kxspen(lxen+i)=jxs(ljxs+1,kxspie(lxie+i))-1                               !/*cb4b      147*/
      kxspxs(lxxs+i)=jxs(ljxs+12,kxspie(lxie+i))-1                              !/*cb4b      148*/
      go to 330                                                                 !/*cb4b      149*/
  320 jx=jxs(ljxs+21,kxspie(lxie+i))                                            !/*cb4b      150*/
      if(jx.eq.0)go to 340                                                      !/*cb4b      151*/
      ne=nint(xss(jx+1))                                                        !/*cb4b      152*/
      kxspen(lxen+i)=jxs(ljxs+1,kxspie(lxie+i))-1+nint(xss(jx))-1               !/*cb4b      153*/
      kxspxs(lxxs+i)=jx+1                                                       !/*cb4b      154*/
  330 npt(1)=npt(1)+ne                                                          !/*cb4b      155*/
      if(kxsptp.eq.2)npt(1)=npt(1)+ne                                           !/*cb4b      156*/
      kxspnx(lxnx+i)=ne                                                         !/*cb4b      157*/
      kf=1                                                                      !/*cb4b      158*/
  340 continue                                                                  !/*cb4b      159*/
      if(kf.eq.1)go to 700                                                      !/*cb4b      160*/
      if(kxspmt.eq.-5)write(jtty,                                               !/*cb4b      161*/
     & '(37h no photon production data found for ,a10)')xspttl                  !/*cb4b      162*/
      if(kxspmt.eq.-6)write(jtty,                                               !/*cb4b      163*/
     & '(27h no fission data found for ,a10)')xspttl                            !/*cb4b      164*/
      go to 9090                                                                !/*cb4b      165*/
!                                                                               !/*cb4b      166*/
!        mt > 2 - endf reaction cross section                                   !/*cb4b      167*/
!        or dosimetry library.  do special set up for endfb                     !/*cb4b      168*/
!        or dosimetry plot.                                                     !/*cb4b      169*/
  350 kf=0                                                                      !/*cbxl       24*/
      do 400 i=1,kxsplt                                                         !/*cb4b      172*/
      kxsptp=nty(lnty+kxspie(lxie+i))                                           !/*cb4b      173*/
      ne=0                                                                      !/*cb4b      174*/
      kxspen(lxen+i)=-1                                                         !/*cb4b      175*/
      kxspxs(lxxs+i)=-1                                                         !/*cb4b      176*/
      np=nxs(lnxs+4,kxspie(lxie+i))                                             !/*cb4b      177*/
      if(np.eq.0)go to 400                                                      !/*cb4b      178*/
!                                                                               !/*cb4b      179*/
!        there are mt values to search through for this zaid.                   !/*cb4b      180*/
!        search and store pointers.                                             !/*cb4b      181*/
!        flag those that are missing with negative pointers.                    !/*cb4b      182*/
      do 360 k=1,np                                                             !/*cb4b      183*/
      km=k-1                                                                    !/*cb4b      184*/
  360 if(nint(xss(jxs(ljxs+3,kxspie(lxie+i))+km)).eq.kxspmt)go to 370           !/*cbx        13*/
      go to 400                                                                 !/*cb4b      186*/
  370 j6=jxs(ljxs+6,kxspie(lxie+i))                                             !/*cb4b      187*/
      j7=jxs(ljxs+7,kxspie(lxie+i))                                             !/*cb4b      188*/
      lo=nint(xss(j6+km))                                                       !/*cb4b      189*/
      if(kxsptp.eq.3)go to 380                                                  !/*cb4b      190*/
      ne=nint(xss(j7+lo))                                                       !/*cb4b      191*/
      if(kxsptp.eq.2)npt(1)=npt(1)+ne                                           !/*cb4b      192*/
      ie=nint(xss(j7+lo-1))                                                     !/*cb4b      193*/
      kxspen(lxen+i)=jxs(ljxs+1,kxspie(lxie+i))-1+ie-1                          !/*cb4b      194*/
      kxspxs(lxxs+i)=j7+lo                                                      !/*cb4b      195*/
      go to 390                                                                 !/*cb4b      196*/
!                                                                               !/*cb4b      197*/
!        special lookup for dosimetry class.                                    !/*cb4b      198*/
  380 nr=nint(xss(j7+lo-1))                                                     !/*cb4b      199*/
      ne=nint(xss(j7+lo+2*nr))                                                  !/*cb4b      200*/
      kxspen(lxen+i)=j7+lo+2*nr                                                 !/*cb4b      201*/
      kxspxs(lxxs+i)=kxspen(lxen+i)+ne                                          !/*cb4b      202*/
!                                                                               !/*cb4b      203*/
!        found at least one mt in material or zaid.                             !/*cb4b      204*/
  390 kf=1                                                                      !/*cb4b      205*/
      npt(1)=npt(1)+ne                                                          !/*cb4b      206*/
      kxspnx(lxnx+i)=ne                                                         !/*cb4b      207*/
  400 continue                                                                  !/*cb4b      208*/
      if(kf.eq.1)go to 700                                                      !/*cb4b      209*/
      if(kxsptp.ge.3)go to 410                                                  !/*cb4b      210*/
      if(kxspmt.eq.101)kxspmt=-2                                                !/*cb4b      211*/
      if(kxspmt.eq.2)kxspmt=-3                                                  !/*cb4b      212*/
      if(kxspmt.eq.301)kxspmt=-4                                                !/*cb4b      213*/
      if(kxspmt.eq.202)kxspmt=-5                                                !/*cb4b      214*/
      if(kxspmt.eq.18)kxspmt=-6                                                 !/*cb4b      215*/
      if(kxspmt.lt.0)go to 270                                                  !/*cb4b      216*/
  410 write(jtty,420)kxspmt,xspttl                                              !/*cb4b      217*/
  420 format(26h cannot find endf reaction,i4,5h for ,a10)                      !/*cb4b      218*/
      go to 750                                                                 !/*cb4b      219*/
!                                                                               !/*cb4b      220*/
! >>>>>  multigroup library.                                                    !/*cb4b      221*/
!        search for multigroup edit reactions.                                  !/*cb4b      222*/
  430 ie=0                                                                      !/*cb4b      223*/
      npt(1)=2*nxs(lnxs+5,kxspie(lxie+1))                                       !/*cb4b      224*/
      kxspen(lxen+1)=jxs(ljxs+1,kxspie(lxie+1))-1                               !/*cb4b      225*/
      do 450 i=1,kxsplt                                                         !/*cb4b      226*/
      kxspxs(lxxs+i)=0                                                          !/*cb4b      227*/
      if(npt(1).ne.2*nxs(lnxs+5,kxspie(lxie+i)))ie=-1000                        !/*cb4b      228*/
      if(ie.lt.0.or.kxspmt.lt.0)go to 450                                       !/*cb4b      229*/
      do 440 j=1,nxs(lnxs+4,kxspie(lxie+i))                                     !/*cb4b      230*/
  440 if(kxspmt.eq.nint(xss(jxs(ljxs+9,kxspie(lxie+i))+j-1)))                   !/*cb4b      231*/
     & kxspxs(lxxs+i)=jxs(ljxs+10,kxspie(lxie+i))+                              !/*cb4b      232*/
     & (j-1)*nxs(lnxs+5,kxspie(lxie+1))-1                                       !/*cb4b      233*/
      if(kxspxs(lxxs+i).gt.0)ie=ie+1                                            !/*cb4b      234*/
  450 continue                                                                  !/*cb4b      235*/
      if(ie.ne.0)go to 470                                                      !/*cb4b      236*/
!                                                                               !/*cb4b      237*/
!        get multigroup standard (mt<0) reactions.                              !/*cb4b      238*/
      kx=abs(kxspmt)                                                            !/*cb4b      239*/
      if(kxspmt.eq.1)kx=1                                                       !/*cb4b      240*/
      if(kxspmt.eq.18)kx=2                                                      !/*cb4b      241*/
      if(kxspmt.eq.101)kx=5                                                     !/*cb4b      242*/
      do 460 i=1,kxsplt                                                         !/*cb4b      243*/
      if(ie.le.0.and.kx.ne.0.and.kx.le.7)                                       !/*cb4b      244*/
     & kxspxs(lxxs+i)=jxs(ljxs+kx+1,kxspie(lxie+i))-1                           !/*cb4b      245*/
  460 if(kxspxs(lxxs+i).le.0.and.ie.le.0)ie=ie-1                                !/*cb4b      246*/
      kxspmt=-kx                                                                !/*cb4b      247*/
!                                                                               !/*cb4b      248*/
!        error messages.                                                        !/*cb4b      249*/
  470 if(ie.eq.0.or.ie.eq.kxsplt)go to 700                                      !/*cb4b      250*/
      if(ie.gt.0.and.ie.lt.kxsplt)write(jtty,480)kxspmt,ie,kxsplt               !/*cb4b      251*/
  480 format(' multigroup edit reaction',i5,' found for only',i3,               !/*cb4b      252*/
     & ' of',i3,' nuclides.')                                                   !/*cb4b      253*/
      if(ie.lt.0.and.ie.gt.-kxsplt)write(jtty,490)kxspmt,-ie,kxsplt             !/*cb4b      254*/
  490 format(' multigroup standard reaction',i3,' not found for',i3,            !/*cb4b      255*/
     & ' of',i3,' nuclides.')                                                   !/*cb4b      256*/
      if(ie.gt.-kxsplt)go to 700                                                !/*cb4b      257*/
      if(ie.le.-1000)write(jtty,500)                                            !/*cb4b      258*/
  500 format(' multigroup structure not same for all nuclides.')                !/*cb4b      259*/
      if(ie.ne.-kxsplt)go to 9090                                               !/*cb4b      260*/
      do 540 i=1,kxsplt                                                         !/*cb4b      261*/
      ir=0                                                                      !/*cb4b      262*/
      call zaid(2,hz,ixl(lixl+1,kxspie(lxie+i)))                                !/*cb4b      263*/
      write(jtty,510)hz                                                         !/*cb4b      264*/
  510 format(' available mt numbers for nuclide ',a10,1h:)                      !/*cb4b      265*/
      do 520 j=1,7                                                              !/*cb4b      266*/
      if(jxs(ljxs+j+1,kxspie(lxie+i)).eq.0)go to 520                            !/*cb4b      267*/
      ir=ir+1                                                                   !/*cb4b      268*/
      tpp(ir)=-j                                                                !/*cb4b      269*/
  520 if(ie.gt.0.and.kxspmt.eq.j)tpp(ir)=-j                                     !/*cb4b      270*/
      do 530 j=1,nxs(lnxs+4,kxspie(lxie+i))                                     !/*cb4b      271*/
      ir=ir+1                                                                   !/*cb4b      272*/
      if(ir.le.10)go to 530                                                     !/*cb4b      273*/
      write(jtty,'(10i5)')(nint(tpp(k)),k=1,10)                                 !/*cb4b      274*/
      ir=1                                                                      !/*cb4b      275*/
  530 tpp(ir)=xss(jxs(ljxs+9,kxspie(lxie+i))+j-1)                               !/*cb4b      276*/
  540 write(jtty,'(10i5)')(nint(tpp(k)),k=1,ir)                                 !/*cb4b      277*/
      go to 9090                                                                !/*cb4b      278*/
!                                                                               !/*cb4b      279*/
! >>>>>  s(a,b) library.  kxsplt=1 always.                                      !/*cb4b      280*/
  560 if(kxspmt.eq.-1)kxspmt=1                                                  !/*cb4b      281*/
      if(kxspmt.eq.1.or.kxspmt.eq.2.or.kxspmt.eq.4)go to 580                    !/*cb4b      282*/
      write(jtty,570)                                                           !/*cb4b      283*/
  570 format(' only mt = 1,2,4 allowed for s(a,b)')                             !/*cb4b      284*/
      go to 9090                                                                !/*cb4b      285*/
!                                                                               !/*cb4b      286*/
!        check for missing reactions.                                           !/*cb4b      287*/
  580 kxspen(lxen+1)=jxs(ljxs+1,kxspie(lxie+1))                                 !/*cb4b      288*/
      kxspen(lxen+2)=jxs(ljxs+4,kxspie(lxie+1))                                 !/*cb4b      289*/
      if(kxspen(lxen+1).gt.0.and.kxspen(lxen+2).gt.0)go to 610                  !/*cb4b      290*/
      if(kxspen(lxen+1).eq.0)kx=2                                               !/*cb4b      291*/
      if(kxspen(lxen+2).eq.0)kx=4                                               !/*cb4b      292*/
      if(kxspmt.eq.kx)go to 620                                                 !/*cb4b      293*/
      call zaid(2,hz,ixl(lixl+1,kxspie(lxie+1)))                                !/*cb4b      294*/
      write(jtty,590)kxspmt,hz                                                  !/*cb4b      295*/
  590 format(' reaction mt =',i2,' missing for material',a10)                   !/*cb4b      296*/
      if(kxspmt.ne.1)go to 9090                                                 !/*cb4b      297*/
      write(jtty,600)kx,hlbl(jlbl(1,(6-kx)/2))                                  !/*cb4b      298*/
  600 format(28h set total x-section to mt =,i2,1h,,1x,a40)                     !/*cb4b      299*/
      kxspmt=kx                                                                 !/*cb4b      300*/
!                                                                               !/*cb4b      301*/
!        kxspmt=1 (total).                                                      !/*cb4b      302*/
!        set kxsplt=2 to sum inelastic and elastic s(a,b).                      !/*cb4b      303*/
  610 if(kxspmt.gt.1)go to 620                                                  !/*cb4b      304*/
      kxsplt=2                                                                  !/*cb4b      305*/
      kxspie(lxie+2)=kxspie(lxie+1)                                             !/*cb4b      306*/
      kxspnx(lxnx+1)=nint(xss(kxspen(lxen+1)))                                  !/*cb4b      307*/
      kxspnx(lxnx+2)=nint(xss(kxspen(lxen+2)))                                  !/*cb4b      308*/
      kxspxs(lxxs+1)=kxspen(lxen+1)+kxspnx(lxnx+1)                              !/*cb4b      309*/
      kxspxs(lxxs+2)=kxspen(lxen+2)+kxspnx(lxnx+2)                              !/*cb4b      310*/
      npt(1)=kxspnx(lxnx+1)+kxspnx(lxnx+2)                                      !/*cb4b      311*/
      if(nxs(lnxs+5,kxspie(lxie+2)).eq.4)npt(1)=npt(1)+kxspnx(lxnx+2)           !/*cb4b      312*/
      go to 700                                                                 !/*cb4b      313*/
!                                                                               !/*cb4b      314*/
!        kxspmt=4 (inelastic) or kxspmt=2 (elastic).                            !/*cb4b      315*/
  620 kxsplt=1                                                                  !/*cb4b      316*/
      if(kxspmt.eq.2)kxspen(lxen+1)=jxs(ljxs+4,kxspie(lxie+1))                  !/*cb4b      317*/
      kxspnx(lxnx+1)=nint(xss(kxspen(lxen+1)))                                  !/*cb4b      318*/
      kxspxs(lxxs+1)=kxspen(lxen+1)+kxspnx(lxnx+1)                              !/*cb4b      319*/
      npt(1)=kxspnx(lxnx+1)                                                     !/*cb4b      320*/
      if(nxs(lnxs+5,kxspie(lxie+1)).eq.4.and.kxspmt.eq.2)                       !/*cb4b      321*/
     & npt(1)=npt(1)+kxspnx(lxnx+1)                                             !/*cb4b      322*/
      go to 700                                                                 !/*cb4b      323*/
!                                                                               !/*cb4b      324*/
! >>>>>  photon library.                                                        !/*cb4b      325*/
  630 if(kxspmt.ge.-6.and.kxspmt.le.-1)go to 635                                !/*cbxl       25*/
      if(kxspmt.eq.504)kxspmt=-1                                                !/*cb4b      328*/
      if(kxspmt.eq.502)kxspmt=-2                                                !/*cb4b      329*/
      if(kxspmt.eq.522)kxspmt=-3                                                !/*cb4b      330*/
      if(kxspmt.eq.516)kxspmt=-4                                                !/*cb4b      331*/
      if(kxspmt.eq.501)kxspmt=-5                                                !/*cb4b      332*/
      if(kxspmt.eq.301)kxspmt=-6                                                !/*cb4b      333*/
      if(kxspmt.gt.0.and.ispn.ne.0.and.kxspma.ne.0)then                         !/*cbxl       26*/
         write(jtty,'(41h mt > 0:  switching to photonuclear data.)')           !/*cbxl       27*/
         kxsptp=9                                                               !/*cbxl       28*/
         go to 20                                                               !/*cbxl       29*/
      elseif(kxspmt.lt.-6.or.kxspmt.ge.0)then                                   !/*cbxl       30*/
         write(jtty,'(30h illegal reaction for photons ,i10)')kxspmt            !/*cbxl       31*/
         go to 9090                                                             !/*cbxl       32*/
      endif                                                                     !/*cbxl       33*/
  635 do 640 i=1,kxsplt                                                         !/*cbxl       34*/
      np=nxs(lnxs+3,kxspie(lxie+i))                                             !/*cb4b      339*/
!                                                                               !/*cb4b      340*/
!        set energy and xs grid pointers.                                       !/*cb4b      341*/
      kxspnx(lxnx+i)=np                                                         !/*cb4b      342*/
      npt(1)=npt(1)+np                                                          !/*cb4b      343*/
      kxspen(lxen+i)=jxs(ljxs+1,kxspie(lxie+i))-1                               !/*cb4b      344*/
      if(kxspmt.gt.-5)kxspxs(lxxs+i)=kxspen(lxen+i)-kxspmt*np                   !/*cb4b      345*/
      if(kxspmt.eq.-5)kxspxs(lxxs+i)=kxspen(lxen+i)                             !/*cb4b      346*/
      if(kxspmt.eq.-6)kxspxs(lxxs+i)=jxs(ljxs+5,kxspie(lxie+i))-1               !/*cb4b      347*/
  640 continue                                                                  !/*cbxl       35*/
      go to 700                                                                 !/*cbxl       36*/
!                                                                               !/*cbxl       37*/
! >>>>> proton library.                                                         !/*cbxl       38*/
  650 do im=1,kxsplt                                                            !/*cbxl       39*/
         kx=kxspie(lxie+im)                                                     !/*cbxl       40*/
         call getpnx(kx,kxspmt,n1,ne,je,jx,nf)                                  !/*cbxl       41*/
         kxspen(lxen+im)=je+n1-1                                                !/*cbxl       42*/
         kxspxs(lxxs+im)=jx                                                     !/*cbxl       43*/
         kxspnx(lxnx+im)=ne                                                     !/*cbxl       44*/
         npt(1)=npt(1)+ne                                                       !/*cbxl       45*/
      enddo                                                                     !/*cbxl       46*/
      if(npt(1).gt.0)go to 700                                                  !/*cbxl       47*/
      write(jtty,'(27h no data found for reaction,i6)')kxspmt                   !/*cbxl       48*/
      mp=kxspmt/1000                                                            !/*cbxl       49*/
      if(mp.eq.0.or.mp.eq.9)go to 750                                           !/*cbxl       50*/
      do im=1,kxsplt                                                            !/*cbxl       51*/
         kx=kxspie(lxie+im)                                                     !/*cbxl       52*/
         do ip=1,nxs(lnxs+7,kx)                                                 !/*cbxl       53*/
            if(nint(xss(jxs(ljxs+30,kx)+ip-1)).eq.mp)go to 750                  !/*cbxl       54*/
         enddo                                                                  !/*cbxl       55*/
         write(jtty,'(44havailable proton secondary particles, zaid =,          !/*cbxl       56*/
     &   i6/(10i5))')nxs(lnxs+2,kx),(nint(xss(jxs(ljxs+30,kx)+ip-1)),           !/*cbxl       57*/
     &   ip=1,nxs(lnxs+7,kx))                                                   !/*cbxl       58*/
      enddo                                                                     !/*cbxl       59*/
      go to 750                                                                 !/*cbxl       60*/
!                                                                               !/*cbxl       61*/
! >>>>>  photonuclear library.                                                  !/*cbxl       62*/
  670 if(ispn.eq.0)then                                                         !/*cbxl       63*/
         write(jtty,'(33h photonuclear physics turned off.)')                   !/*cbxl       64*/
         go to 9090                                                             !/*cbxl       65*/
      elseif(kxspmt.lt.0.and.kxspma.ne.0)then                                   !/*cbxl       66*/
         write(jtty,'(35h mt < 0:  switching to photon data.)')                 !/*cbxl       67*/
         kxsptp=5                                                               !/*cbxl       68*/
         go to 20                                                               !/*cbxl       69*/
      endif                                                                     !/*cbxl       70*/
      do 680 im=1,kxsplt                                                        !/*cbxl       71*/
      kx=kxspie(lxie+im)                                                        !/*cbxl       72*/
      call getpnx(kx,kxspmt,n1,ne,je,jx,nf)                                     !/*cbxl       73*/
      kxspen(lxen+im)=je+n1-1                                                   !/*cbxl       74*/
      kxspxs(lxxs+im)=jx                                                        !/*cbxl       75*/
      kxspnx(lxnx+im)=ne                                                        !/*cbxl       76*/
      npt(1)=npt(1)+ne                                                          !/*cbxl       77*/
  680 continue                                                                  !/*cbxl       78*/
      if(npt(1).gt.0)go to 700                                                  !/*cbxl       79*/
!                                                                               !/*cbxl       80*/
!        print allowable mt numbers for available reactions.                    !/*cbxl       81*/
      write(jtty,'(/27h no data found for reaction,i4)')kxspmt                  !/*cbxl       82*/
      do im=1,kxsplt                                                            !/*cbxl       83*/
         call zaid(2,hz,ixl(lixl+1,kxspie(lxie+im)))                            !/*cbxl       84*/
         kx=kxspie(lxie+im)                                                     !/*cbxl       85*/
         write(jtty,'(/a10,21h allowable reactions:/(10i8))')hz,                !/*cbxl       86*/
     &   1,2,3,4,(nint(xss(jxs(ljxs+6,kx)+ir-1)),ir=1,nxs(lnxs+4,kx))           !/*cbxl       87*/
         write(jtty,'(10i8)')                                                   !/*cbxl       88*/
     &   (1000*ixs(lixs+1,ip,kx)+1,1000*ixs(lixs+1,ip,kx)+4,                    !/*cbxl       89*/
     &   (1000*ixs(lixs+1,ip,kx)+nint(xss(ixs(lixs+5,ip,kx)+ir-1)),             !/*cbxl       90*/
     &   ir=1,ixs(lixs+2,ip,kx)),ip=1,nxs(lnxs+5,kx))                           !/*cbxl       91*/
      enddo                                                                     !/*cbxl       92*/
      go to 9090                                                                !/*cbxl       93*/
!                                                                               !/*cb4b      350*/
! >>>>>  electron library.                                                      !/*cb4b      351*/
!        for print table 85 the electron energy grid is the same                !/*cb4b      352*/
!        throughout the problem and there is no need for pointers.              !/*cb4b      353*/
  690 npt(1)=nee(3)-1                                                           !/*cbxl       94*/
      if(kxspmt.gt.0.and.kxspmt.lt.14.and.kxspar.eq.3.and.kxspma.ne.0)          !/*cb4b      355*/
     & go to 700                                                                !/*cb4b      356*/
      if(kxspmt.le.0.or.kxspmt.gt.14)write(jtty,                                !/*cbxl       95*/
     & '(39h not an allowed reaction for electrons ,i10)')kxspmt                !/*cbxl       96*/
      if(kxspar.ne.3.or.kxspma.eq.0)write(jtty,                                 !/*cbxl       97*/
     & '(49h electron transport properties for materials only)')                !/*cbxl       98*/
      go to 9090                                                                !/*cb4b      361*/
!                                                                               !/*cb4b      362*/
!        check if enough points to plot.                                        !/*cb4b      363*/
  700 if(npt(1).ge.2)go to 720                                                  !/*cb4b      364*/
      write(jtty,710)                                                           !/*cb4b      365*/
  710 format(37h xs plot contains less than 2 points.)                          !/*cb4b      366*/
      go to 9090                                                                !/*cb4b      367*/
!                                                                               !/*cb4b      368*/
!        check that user-specified limits for log axes are positive.            !/*cb4b      369*/
  720 if((jlim(1).eq.0.or.lax.lt.3.or.xyzmn(1).gt.0.).and.                      !/*cb4b      370*/
     & (jlim(2).eq.0.or.abs(lax-3).ne.1.or.xyzmn(2).gt.0.))go to 740            !/*cb4b      371*/
      write(jtty,730)                                                           !/*cb4b      372*/
  730 format(34h non-positive limit on a log axis.)                             !/*cb4b      373*/
      go to 9090                                                                !/*cb4b      374*/
!                                                                               !/*cb4b      375*/
!        set legend to zaid number                                              !/*cb4b      376*/
  740 ititle(4)=1                                                               !/*cb4b      377*/
      titles(4)=xspttl                                                          !/*cb4b      378*/
      call pltitl                                                               !/*cb4b      379*/
      if(ititle(1).eq.0)call axlabl(1)                                          !/*cb4b      380*/
      if(ititle(2).eq.0)call axlabl(2)                                          !/*cb4b      381*/
      if(nfree.eq.2.and.j3d.ne.0.and.ititle(3).eq.0)call axlabl(3)              !/*cb4b      382*/
      return                                                                    !/*cb4b      383*/
!                                                                               !/*cb4b      384*/
!        if kxspmt=0 or if there is an error, list possible reaction.           !/*cb4b      385*/
  750 write(jtty,'(41h available (not expunged) endf reactions:)')              !/*cb4b      386*/
      if(kxspma.ne.0)write(jtty,'(14h for material ,a10,8h made of)')           !/*cb4b      387*/
     & xspttl                                                                   !/*cb4b      388*/
      if(kxspma.eq.0)write(jtty,'(13h for isotope ,a10)')xspttl                 !/*cb4b      389*/
      do 780 i=1,kxsplt                                                         !/*cb4b      390*/
      call zaid(2,hz,ixl(lixl+1,kxspie(lxie+i)))                                !/*cb4b      391*/
      write(jtty,'(6h zaid ,a10,7h has : )')hz                                  !/*cb4b      392*/
      do 760 km=1,nxs(lnxs+4,kxspie(lxie+i))                                    !/*cb4b      393*/
  760 write(jtty,770)xss(jxs(ljxs+3,kxspie(lxie+i))+km-1)                       !/*cb4b      394*/
  770 format(6x,4hmt =,f4.0)                                                    !/*cb4b      395*/
  780 continue                                                                  !/*cb4b      396*/
      write(jtty,790)                                                           !/*cb4b      397*/
  790 format(' some mts may have been expunged or may not exist'/               !/*cb4b      398*/
     & ' in the material or zaid cross sections given.'/                        !/*cb4b      399*/
     & ' if an mt exists but is not listed here, call it out'/                  !/*cb4b      400*/
     & ' with an fm tally card.')                                               !/*cb4b      401*/
      go to 9090                                                                !/*cb4b      402*/
!                                                                               !/*cb4b      403*/
!        error exit.                                                            !/*cb4b      404*/
 9090 inform=1                                                                  !/*cb4b      405*/
#endif                                                                          /*!cbx        15*/
      return                                                                    !/*cb4b      406*/
      end                                                                       !/*cb4b      407*/

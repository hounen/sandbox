!#ident plotg.F from deck pl                                                    !/*pl          1*/
      subroutine plotg                                                          !/*pl4a        1*/
#ifdef PLOT                                                                     /*!plx         1*/
!        make plots of the geometry in response to input from the user.         !/*pl          4*/
      use global_data                                                           !/*plx         2*/
      use plot_data                                                             !/*plx         3*/
!                                                                               !/*plx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*plx         5*/
      implicit integer(kindi) (i-n)                                             !/*plx         6*/
!                                                                               !/*plx         7*/
      dimension gg(4,4)                                                         !/*pl          7*/
      real sl,su,tl,tu                                                          !/*pl          8*/
      character hl*29                                                           !/*pl          9*/
!                                                                               !/*pl         10*/
      hovr='plot'                                                               !/*pl         11*/
#ifndef XLIB                                                                    /*!pl4c2       1*/
      nq=0                                                                      !/*plx         8*/
#endif                                                                          /*!cpp       133*/
#ifdef XLIB                                                                     /*!pl4c2       3*/
      nq=-1                                                                     !/*pl4c2       4*/
#endif                                                                          /*!cpp       134*/
!                                                                               !/*plx         9*/
!        allocate dynamic plot variables.                                       !/*plx        10*/
      call plotg_allocate(1)                                                    !/*plx        11*/
!                                                                               !/*pl         14*/
!        if the input command file is not the terminal, open it.                !/*pl         15*/
      if(iovr.eq.4)go to 50                                                     !/*pl         16*/
      if(ifile.ne.0)go to 10                                                    !/*pl         17*/
      ifile=itty                                                                !/*pl         18*/
      if(com.eq.' ')go to 10                                                    !/*pl         19*/
      ifile=iuk                                                                 !/*pl         20*/
      if(inqire(com).eq.0)                                                      !/*pl         21*/
     & call expire(0,'mcplot',com//' does not exist.')                          !/*pl         22*/
      open(ifile,file=com,status='old')                                         !/*pl         23*/
      rewind ifile                                                              !/*pl         24*/
!                                                                               !/*pl         25*/
!        create the output command file comout with a unique name.              !/*pl         26*/
   10 if(komout.ne.0)go to 20                                                   !/*pl         27*/
      komout=1                                                                  !/*pl         28*/
      call unique(comout,jtty)                                                  !/*pl         29*/
      open(iuc,file=comout,status='new')                                        !/*pl         30*/
!                                                                               !/*pl         31*/
!        read the last dump from the runtpe file if necessary.                  !/*pl         32*/
   20 if(jovr(1).ne.0.or.ktfile.eq.1)go to 50                                   !/*pl         33*/
      if(inqire(runtpe).eq.0)go to 30                                           !/*pl         35*/
      idmp=0                                                                    !/*pl         36*/
      call plotg_allocate(0)                                                    !/*plx        12*/
      call tpefil(7)                                                            !/*plx        13*/
      call plotg_allocate(1)                                                    !/*plx        14*/
      if(ktfile.eq.17)ktfile=1                                                  !/*pl         38*/
   30 if(ktfile.ne.1.and.ifile.eq.jtty)write(jtty,40)                           !/*pl         39*/
   40 format(54h the first thing you need to do is read a runtpe file.)         !/*pl         40*/
   50 ln=1                                                                      !/*pl4c2       5*/
!                                                                               !/*pl4c2       6*/
!        interactive setup / point and click menu.                              !/*pl4c2       7*/
   60 if(ifile.ne.itty.or.iterm.eq.0)go to 62                                   !/*pl4c2       8*/
      if(nq.ne.0)call pltsup(nq)                                                !/*pl4c2       9*/
      if(nq.eq.1)go to 110                                                      !/*pl4c2      10*/
      if(nq.eq.2) then                                                          !/*plx        15*/
        call plotg_allocate(0)                                                  !/*plx        16*/
        return                                                                  !/*plx        17*/
      endif                                                                     !/*plx        18*/
!                                                                               !/*pl4c2      12*/
!        read the next line of commands.                                        !/*pl4c2      13*/
   62 inform=0                                                                  !/*pl4c2      14*/
      if(ifile.eq.itty)write(jtty,'(6h plot>)')                                 !/*pl4a        6*/
      read(ifile,'(a80)',end=70)klin                                            !/*pl         45*/
      go to 90                                                                  !/*pl         46*/
   70 klin=' '                                                                  !/*pl         47*/
      if(ifile.eq.itty)go to 90                                                 !/*pl         48*/
      write(jtty,80)com                                                         !/*pl         49*/
   80 format(/47h command input is now being switched from file ,a8/            !/*pl         50*/
     & 21h    to your terminal.)                                                !/*pl         51*/
      ifile=itty                                                                !/*pl         52*/
      ln=1                                                                      !/*pl         53*/
      go to 60                                                                  !/*pl         54*/
   90 if(iovr.ne.4)write(iuc,'(a80)')klin                                       !/*pl         55*/
      if(klin.eq.' ')go to 110                                                  !/*pl         56*/
      ln=0                                                                      !/*pl         57*/
      jchar=1                                                                   !/*pl         58*/
!                                                                               !/*pl         59*/
!        decode a keyword and its parameters from the command line.             !/*pl         60*/
  100 if(jdecod(ln).ne.0) then                                                  !/*plx        19*/
        call plotg_allocate(0)                                                  !/*plx        20*/
        return                                                                  !/*plx        21*/
      endif                                                                     !/*plx        22*/
      if(jchar.gt.80.or.klin(min(jchar,80):80).eq.' ')ln=1                      !/*pl         62*/
      if(ln.eq.0)go to 100                                                      !/*pl         63*/
      if(inform.lt.0)nq=1                                                       !/*pl4c2      15*/
      if(inform.gt.0)go to 60                                                   !/*pl4c2      16*/
!                                                                               !/*pl         65*/
!        set up the plot matrix.                                                !/*pl         66*/
  110 plmx(1,1)=1.                                                              !/*pl         67*/
      do 120 i=2,4                                                              !/*pl         68*/
      plmx(1,i)=0.                                                              !/*pl         69*/
      plmx(i,1)=origin(i-1)                                                     !/*pl         70*/
      plmx(i,2)=basis(i-1)                                                      !/*pl         71*/
      plmx(i,3)=basis(i+2)                                                      !/*pl         72*/
  120 plmx(i,4)=basis(i+5)                                                      !/*pl         73*/
!                                                                               !/*pl         74*/
      kcl=0                                                                     !/*plx        23*/
!                                                                               !/*pl         98*/
!        construct amx matrices for all surfaces other than tori.               !/*pl         99*/
      do 150 j=1,mxj                                                            !/*pl        100*/
      if(kst(lkst+j).ge.24)go to 150                                            !/*pl4a        7*/
      call amatrx(j,gg)                                                         !/*pl        102*/
      do 140 k=1,4                                                              !/*pl        103*/
      do 140 l=1,4                                                              !/*pl        104*/
  140 amx(lamx+k,l,j)=gg(k,l)                                                   !/*pl        105*/
  150 continue                                                                  !/*pl        106*/
!                                                                               !/*pl        107*/
!        display the coordinates of the locator if required.                    !/*pl        108*/
      if(jloc.eq.0)go to 180                                                    !/*pl        109*/
      jloc=0                                                                    !/*pl        110*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+3))                              !/*pl4a        9*/
      call ginlc(1,1,1,0.,0.,1,0.,0.,0.,0.,1,' ')                               !/*pl        111*/
      call grqlc(1,1,i,j,sl,tl)                                                 !/*pl        112*/
      do 160 i=1,3                                                              !/*pl        113*/
  160 tpp(i)=origin(i)+sl*basis(i)+tl*basis(3+i)                                !/*pl        114*/
      write(hl,170)(tpp(i),i=1,3)                                               !/*pl        115*/
  170 format(f9.2,1h,,f9.2,1h,,f9.2)                                            !/*pl        116*/
      write(jtty,'(1x,a29)')hl                                                  !/*pl4a       10*/
      call gschh(real(sch*extent(2)*(1.+.1*(1-jvp))))                           !/*pl4a       11*/
#ifdef XLIB                                                                     /*!pl4a       12*/
!        put locator output at home                                             !/*plx        24*/
      if(jvp.eq.0)sl=xhom                                                       !/*pl4a       13*/
      if(jvp.eq.0)tl=yhom                                                       !/*pl4a       14*/
#endif                                                                          /*!cpp       137*/
#ifdef LAHEY                                                                    /*!pl4c        1*/
      call WindowOutString(100,5000,hl)                                         !/*pl4c        2*/
#endif                                                                          /*!cpp       138*/
#ifndef LAHEY                                                                   /*!pl4c        3*/
      call gtx(sl,tl,hl)                                                        !/*pl        117*/
#endif                                                                          /*!cpp       139*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*pl4a       16*/
#ifndef LAHEY                                                                   /*!pl4c        4*/
      call gxhome(xhom,yhom)                                                    !/*pl        118*/
#endif                                                                          /*!cpp       140*/
      go to 60                                                                  !/*pl        119*/
!                                                                               !/*pl        120*/
!        define a new origin and extent via cursor input if required.           !/*pl        121*/
  180 if(icurs.eq.0)go to 200                                                   !/*pl        122*/
      icurs=0                                                                   !/*pl        123*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+3))                              !/*pl4a       17*/
      call ginlc(1,1,1,0.,0.,1,0.,0.,0.,0.,1,' ')                               !/*pl        124*/
#ifdef XLIB                                                                     /*!pl4a       18*/
!        use the 2nd entry to flag this is a cursor call.                       !/*plx        25*/
      call grqlc(1,2,i,j,sl,tl)                                                 !/*pl4a       19*/
#else                                                                           /*!plx        26*/
      call grqlc(1,1,i,j,sl,tl)                                                 !/*pl        125*/
#endif                                                                          /*!cpp       142*/
      call ginlc(1,1,1,0.,0.,1,0.,0.,0.,0.,1,' ')                               !/*pl        126*/
#ifdef XLIB                                                                     /*!pl4a       21*/
!        use the 2nd entry to flag this is a cursor call.                       !/*plx        27*/
      call grqlc(1,3,i,j,su,tu)                                                 !/*pl4a       22*/
#else                                                                           /*!plx        28*/
      call grqlc(1,1,i,j,su,tu)                                                 !/*pl        127*/
#endif                                                                          /*!cpp       144*/
      c=abs(su-sl)*.5                                                           !/*pl        128*/
      d=abs(tu-tl)*.5                                                           !/*pl        129*/
      if(abs(c).le.1e-4*extent(1).and.abs(d).le.1e-4*extent(2))go to 200        !/*pl        130*/
      if(abs(c).gt.1e-4*extent(1))extent(1)=c                                   !/*pl        131*/
      if(abs(d).gt.1e-4*extent(2))extent(2)=d                                   !/*pl        132*/
      do 190 i=1,3                                                              !/*pl        133*/
      origin(i)=plmx(i+1,1)+.5*((sl+su)*plmx(i+1,2)+(tl+tu)*plmx(i+1,3))        !/*pl        134*/
  190 plmx(i+1,1)=origin(i)                                                     !/*pl        135*/
      if(exsav(1).ne.exsav(2))go to 200                                         !/*pl        136*/
      extent(1)=max(extent(1),extent(2))                                        !/*pl        137*/
      extent(2)=extent(1)                                                       !/*pl        138*/
!                                                                               !/*pl        139*/
!        construct the description of the picture in array zst.                 !/*pl        140*/
  200 call viewz                                                                !/*pl        141*/
!                                                                               !/*pl4c2      18*/
!        if plot is empty, try level=-1 or previous origin or extent.           !/*pl4c2      19*/
      if(zst(lzst+1).ne.0.)go to 220                                            !/*pl        142*/
      if(mod(meshpl,2).eq.0.and.mcolor.gt.2)go to 220                           !/*pl4c2      20*/
      write(jtty,210)                                                           !/*pl        143*/
  210 format(42h no plot because it would have been empty.)                     !/*pl        144*/
      if(levplt.lt.0)go to 215                                                  !/*pl4c2      21*/
      levplt=-1                                                                 !/*pl4c2      22*/
      write(iuc,'(9hlevel off)')                                                !/*pl4c2      23*/
      go to 200                                                                 !/*pl4c2      24*/
  215 oe=abs(origin(1)-orsav(1))+abs(origin(2)-orsav(2))+abs(origin(3)-         !/*pl4c2      25*/
     & orsav(3))+abs(extent(1)-exsav(1))+abs(extent(2)-exsav(2))                !/*pl4c2      26*/
      if(oe.eq.0.)go to 60                                                      !/*pl4c2      27*/
      if(exsav(1)*exsav(2).le.0.)go to 60                                       !/*pl4c2      28*/
      origin(1)=orsav(1)                                                        !/*pl4c2      29*/
      origin(2)=orsav(2)                                                        !/*pl4c2      30*/
      origin(3)=orsav(3)                                                        !/*pl4c2      31*/
      extent(1)=exsav(1)                                                        !/*pl4c2      32*/
      extent(2)=exsav(2)                                                        !/*pl4c2      33*/
      write(iuc,'(6horigin,1p3e12.4,7h extent,1p2e12.4)')                       !/*pl4c2      34*/
     & (origin(i),i=1,3),extent(1),extent(2)                                    !/*pl4c2      35*/
      go to 110                                                                 !/*pl4c2      36*/
!                                                                               !/*pl        146*/
!        draw the picture at the terminal and/or on the metafile.               !/*pl        147*/
  220 call gxskip                                                               !/*pl        148*/
      if(iterm.ne.0)call gxon(1)                                                !/*pl        149*/
      if(iterm.eq.0)call gxoff(1)                                               !/*pl        150*/
      if(jgf.ne.0.or.iterm.eq.0)call gxon(2)                                    !/*pl        151*/
      if(jgf.eq.0.and.iterm.ne.0)call gxoff(2)                                  !/*pl        152*/
      if(jgxo(1)+jgxo(2).ne.0)call tekdvr                                       !/*pl4a       27*/
      if(jgf.eq.1)jgf=0                                                         !/*pl        154*/
      go to 60                                                                  !/*pl        155*/
#else                                                                           /*!plx        29*/
      return                                                                    !/*plx        30*/
#endif                                                                          /*!plx        31*/
      end                                                                       !/*pl        156*/

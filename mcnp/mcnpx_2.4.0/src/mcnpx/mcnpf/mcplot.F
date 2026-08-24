!#ident mcplot.F from deck mz                                                   !/*mz          1*/
      subroutine mcplot                                                         !/*mz          3*/
#ifdef MCPLOT                                                                   /*!mzx         1*/
#ifdef LAHEY                                                                    /*!mz4c        1*/
      use WINTERACTER                                                           !/*mz4c        2*/
#endif                                                                          /*!cpp       287*/
!        make plots of tally data in response to input from the user.           !/*mz          4*/
      use global_data                                                           !/*mzx         2*/
      use mcplot_data                                                           !/*mzx         3*/
!                                                                               !/*mzx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*mzx         5*/
      implicit integer(kindi) (i-n)                                             !/*mzx         6*/
!                                                                               !/*mzx         7*/
!                                                                               !/*mz          7*/
      hovr='mcplot'                                                             !/*mz          8*/
      if(jovr(1)+jovr(3)+jovr(5).eq.3)kxsplt=1                                  !/*mz4b        1*/
      if(iovr.eq.4)kxsplt=0                                                     !/*mz4b        2*/
      if (.not.allocated (kxspie).and.kxsplt.ne.0) then                         !/*mzx         8*/
         allocate (kxspie(mxe1), stat=is)                                       !/*mzx         9*/
         if(is.ne.0) go to 170                                                  !/*mzx        10*/
         kxspie=0                                                               !/*mzx        11*/
         allocate (kxspen(mxe1), stat=is)                                       !/*mzx        12*/
         if(is.ne.0) go to 170                                                  !/*mzx        13*/
         kxspen=0                                                               !/*mzx        14*/
         allocate (kxspxs(mxe1), stat=is)                                       !/*mzx        15*/
         if(is.ne.0) go to 170                                                  !/*mzx        16*/
         kxspxs=0                                                               !/*mzx        17*/
         allocate (kxspnx(2*mxe1), stat=is)                                     !/*mzx        18*/
         if(is.ne.0) go to 170                                                  !/*mzx        19*/
         kxspnx=0                                                               !/*mzx        20*/
      endif                                                                     !/*mzx        21*/
      if(kxsplt.ne.0)call mreset(21)                                            !/*mz4b        3*/
      jvp=0                                                                     !/*mz         11*/
!                                                                               !/*mz         12*/
!        if the input command file is not the terminal, open it.                !/*mz         13*/
      if(iovr.eq.4)ktfile=1                                                     !/*mz         14*/
      if(iovr.eq.4)go to 50                                                     !/*mz         15*/
      if(ifile.ne.0)go to 10                                                    !/*mz         16*/
      ifile=itty                                                                !/*mz         17*/
      if(com.eq.' ')go to 10                                                    !/*mz         18*/
      ifile=iuk                                                                 !/*mz         19*/
      if(inqire(com).eq.0)                                                      !/*mz         20*/
     & call expire(0,'mcplot',com//' does not exist.')                          !/*mz         21*/
      if(jovr(2).ne.0)go to 10                                                  !/*mz4b        4*/
      open(ifile,file=com,status='old')                                         !/*mz         22*/
      rewind ifile                                                              !/*mz         23*/
!                                                                               !/*mz         24*/
!        create the output command file comout with a unique name.              !/*mz         25*/
   10 if(komout.ne.0)go to 20                                                   !/*mz         26*/
      komout=1                                                                  !/*mz         27*/
      call unique(comout,jtty)                                                  !/*mz         28*/
      open(iuc,file=comout,status='new')                                        !/*mz         29*/
!                                                                               !/*mz         30*/
!        read the last dump from the runtpe file if it exists.                  !/*mz         31*/
   20 if(ktfile.ne.0)go to 50                                                   !/*mz         32*/
      mtal=0                                                                    !/*mz         33*/
      if(inqire(runtpe).eq.0)go to 30                                           !/*mz         35*/
      idmp=0                                                                    !/*mz         36*/
      call tpefil(7)                                                            !/*mz         37*/
      if(ktfile.eq.17)ktfile=1                                                  !/*mz         38*/
      mtal=min(1,ntal)                                                          !/*mz         39*/
   30 if(ktfile.eq.0.and.ifile.eq.itty)write(jtty,40)                           !/*mz         40*/
   40 format(54h the first thing you need to do is to read a runtpe or,         !/*mz         41*/
     & 12h mctal file.)                                                         !/*mz         42*/
!                                                                               !/*mz         43*/
!        read the next line of commands.                                        !/*mz         44*/
   50 ln=1                                                                      !/*mz         45*/
   60 inform=0                                                                  !/*mz         46*/
      if(ln.eq.0)go to 100                                                      !/*mz         47*/
      if(ifile.eq.itty)write(jtty,'(8h mcplot>)')                               !/*mz4a        1*/
      read(ifile,'(a80)',end=70)klin                                            !/*mz         48*/
#ifdef LAHEY                                                                    /*!mz4c        3*/
      call IGrAreaClear()                                                       !/*mz4c        4*/
#endif                                                                          /*!cpp       288*/
      go to 90                                                                  !/*mz         49*/
   70 klin=' '                                                                  !/*mz         50*/
      if(ifile.eq.itty)go to 90                                                 !/*mz         51*/
      write(jtty,80)com                                                         !/*mz         52*/
   80 format(/47h command input is now being switched from file ,a8/            !/*mz         53*/
     & 21h    to your terminal.)                                                !/*mz         54*/
      ifile=itty                                                                !/*mz         55*/
      ln=1                                                                      !/*mz         56*/
      go to 60                                                                  !/*mz         57*/
   90 if(iovr.ne.4)write(iuc,'(a80)')klin                                       !/*mz         58*/
      if(klin.eq.' ')go to 110                                                  !/*mz         59*/
      ln=0                                                                      !/*mz         60*/
      jchar=1                                                                   !/*mz         61*/
!                                                                               !/*mz         62*/
!        decode a keyword and its parameters from the command line.             !/*mz         63*/
  100 if(mdecod(ln).ne.0)return                                                 !/*mz         64*/
      if(jchar.gt.80.or.klin(min(jchar,80):80).eq.' ')ln=1                      !/*mz         65*/
      if(ln.eq.0.and.abs(koplot-1).eq.1)go to 100                               !/*mz         66*/
      if(inform.ne.0)go to 60                                                   !/*mz         67*/
!                                                                               !/*mz         68*/
!        define storage offsets for a 2d plot.                                  !/*mz4b        5*/
  110 kx=0                                                                      !/*mz4b        6*/
      if(jovr(1)+jovr(3)+jovr(5).eq.3)kx=1                                      !/*mz4b        7*/
      if (.not.allocated (kxspie).and.kx.eq.1) then                             !/*mzx        22*/
         allocate (kxspie(mxe1), stat=is)                                       !/*mzx        23*/
         if(is.ne.0) go to 170                                                  !/*mzx        24*/
         kxspie=0                                                               !/*mzx        25*/
         allocate (kxspen(mxe1), stat=is)                                       !/*mzx        26*/
         if(is.ne.0) go to 170                                                  !/*mzx        27*/
         kxspen=0                                                               !/*mzx        28*/
         allocate (kxspxs(mxe1), stat=is)                                       !/*mzx        29*/
         if(is.ne.0) go to 170                                                  !/*mzx        30*/
         kxspxs=0                                                               !/*mzx        31*/
         allocate (kxspnx(2*mxe1), stat=is)                                     !/*mzx        32*/
         if(is.ne.0) go to 170                                                  !/*mzx        33*/
         kxspnx=0                                                               !/*mzx        34*/
      endif                                                                     !/*mzx        35*/
!                                                                               !/*mz4b       18*/
!        check and complete the plot request definition.                        !/*mz4b       19*/
      if(kxsplt.eq.0)call chkprb                                                !/*mz4b       20*/
      if(kxsplt.ne.0)call chkxss                                                !/*mz4b       21*/
      if(inform.ne.0)go to 60                                                   !/*mz4b       22*/
!                                                                               !/*mz4b       23*/
!        define rest of the storage offsets for a 2d plot.                      !/*mz4b       24*/
      if(nfree.ge.2)go to 120                                                   !/*mz         76*/
      if (allocated(ab1)) deallocate (ab1)                                      !/*mzx        36*/
      allocate (ab1(npt(1)), stat=is)                                           !/*mzx        37*/
      if(is.ne.0) go to 170                                                     !/*mzx        38*/
      ab1=0.                                                                    !/*mzx        39*/
      if (allocated(erb)) deallocate (erb)                                      !/*mzx        40*/
      allocate (erb(npt(1)), stat=is)                                           !/*mzx        41*/
      if(is.ne.0) go to 170                                                     !/*mzx        42*/
      erb=0.                                                                    !/*mzx        43*/
      if (allocated(ord)) deallocate (ord)                                      !/*mzx        44*/
      allocate (ord(npt(1)), stat=is)                                           !/*mzx        45*/
      if(is.ne.0) go to 170                                                     !/*mzx        46*/
      ord=0.                                                                    !/*mzx        47*/
      if (allocated(xcc)) deallocate (xcc)                                      !/*mzx        48*/
      allocate (xcc(max(136,2*npt(1)+2)), stat=is)                              !/*mzx        49*/
      if(is.ne.0) go to 170                                                     !/*mzx        50*/
      xcc=0.                                                                    !/*mzx        51*/
      if (allocated(ycc)) deallocate (ycc)                                      !/*mzx        52*/
      allocate (ycc(max(136,2*npt(1)+2)), stat=is)                              !/*mzx        53*/
      if(is.ne.0) go to 170                                                     !/*mzx        54*/
      ycc=0.                                                                    !/*mzx        55*/
      if (allocated(xrr)) deallocate (xrr)                                      !/*mzx        56*/
      allocate (xrr(max(136,2*npt(1)+2)), stat=is)                              !/*mzx        57*/
      if(is.ne.0) go to 170                                                     !/*mzx        58*/
      xrr=0.                                                                    !/*mzx        59*/
      if (allocated(yrr)) deallocate (yrr)                                      !/*mzx        60*/
      allocate (yrr(max(136,2*npt(1)+2)), stat=is)                              !/*mzx        61*/
      if(is.ne.0) go to 170                                                     !/*mzx        62*/
      yrr=0.                                                                    !/*mzx        63*/
      go to 130                                                                 !/*mz         84*/
!                                                                               !/*mz         85*/
!        define storage offsets for a contour or 3d plot.                       !/*mz         86*/
  120 if (allocated(ab1)) deallocate (ab1)                                      !/*mzx        64*/
      allocate (ab1(npt(1)), stat=is)                                           !/*mzx        65*/
      if(is.ne.0) go to 170                                                     !/*mzx        66*/
      ab1=0.                                                                    !/*mzx        67*/
      if (allocated(ab2)) deallocate (ab2)                                      !/*mzx        68*/
      allocate (ab2(npt(2)), stat=is)                                           !/*mzx        69*/
      if(is.ne.0) go to 170                                                     !/*mzx        70*/
      ab2=0.                                                                    !/*mzx        71*/
      nd=min(npt(1),isbm)*min(npt(2),jsbm)                                      !/*mzx        72*/
      if (allocated(ord)) deallocate (ord)                                      !/*mzx        73*/
      allocate (ord(nd), stat=is)                                               !/*mzx        74*/
      if(is.ne.0) go to 170                                                     !/*mzx        75*/
      ord=0.                                                                    !/*mzx        76*/
      if (allocated(xcc)) deallocate (xcc)                                      !/*mzx        77*/
      allocate (xcc(nd), stat=is)                                               !/*mzx        78*/
      if(is.ne.0) go to 170                                                     !/*mzx        79*/
      xcc=0.                                                                    !/*mzx        80*/
      if (allocated(ycc)) deallocate (ycc)                                      !/*mzx        81*/
      allocate (ycc(nd), stat=is)                                               !/*mzx        82*/
      if(is.ne.0) go to 170                                                     !/*mzx        83*/
      ycc=0.                                                                    !/*mzx        84*/
      if (allocated(xrr)) deallocate (xrr)                                      !/*mzx        85*/
      allocate (xrr(nd), stat=is)                                               !/*mzx        86*/
      if(is.ne.0) go to 170                                                     !/*mzx        87*/
      xrr=0.                                                                    !/*mzx        88*/
      if (allocated(yrr)) deallocate (yrr)                                      !/*mzx        89*/
      allocate (yrr(nd), stat=is)                                               !/*mzx        90*/
      if(is.ne.0) go to 170                                                     !/*mzx        91*/
      yrr=0.                                                                    !/*mzx        92*/
  130 continue                                                                  !/*mzx        93*/
!                                                                               !/*mz         99*/
!        extract, check, and perhaps print the data for the plot.               !/*mz        100*/
      call extrct                                                               !/*mz        101*/
      iprpts=0                                                                  !/*mz        102*/
      if(inform.ne.0)go to 155                                                  !/*mz4a        2*/
!                                                                               !/*mz        104*/
!        advance the frame and turn the terminal and metafile on or off.        !/*mz        105*/
      if(koplot.gt.1)go to 140                                                  !/*mz        106*/
      call gxskip                                                               !/*mz        107*/
      if(iterm.ne.0)call gxon(1)                                                !/*mz        108*/
      if(iterm.eq.0)call gxoff(1)                                               !/*mz        109*/
      if(jgf.ne.0.or.iterm.eq.0)call gxon(2)                                    !/*mz        110*/
      if(jgf.eq.0.and.iterm.ne.0)call gxoff(2)                                  !/*mz        111*/
      if(jgxo(1)+jgxo(2).eq.0)go to 60                                          !/*mz4a        3*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+1))                              !/*mz4a        4*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*mz4a        5*/
!                                                                               !/*mz        112*/
!        make the plot.                                                         !/*mz        113*/
  140 if(nfree.eq.1)call plot2d                                                 !/*mz        114*/
      if(nfree.eq.2.and.j3d.eq.0)call plotcn                                    !/*mz        115*/
#ifdef DISSPLA                                                                  /*!mz        116*/
      if(nfree.eq.2.and.j3d.ne.0)call plot3d                                    !/*mz        117*/
#endif                                                                          /*!cpp       290*/
      do 150 i=1,3                                                              !/*mz        118*/
      scalf(1,i)=1.                                                             !/*mz        119*/
  150 scalf(2,i)=0.                                                             !/*mz        120*/
      ititle(4)=0                                                               !/*mz        121*/
  155 if(abs(koplot-2).eq.1)go to 160                                           !/*mz4a        6*/
      if(jgf.eq.1)jgf=0                                                         !/*mz        125*/
  160 koplot=2*mod(koplot,2)                                                    !/*mz        126*/
      go to 60                                                                  !/*mz        127*/
  170 call expire(0,'mcplot','error getting memory')                            !/*mzx        94*/
#else                                                                           /*!mzx        95*/
      return                                                                    !/*mzx        96*/
#endif                                                                          /*!mzx        97*/
      end                                                                       !/*mz        128*/

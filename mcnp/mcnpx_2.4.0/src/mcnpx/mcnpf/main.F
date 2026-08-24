!#ident main.F from deck mc                                                     !/*mc          1*/
      subroutine mcnp                                                           !/*mcx         1*/
!        general monte carlo n-particle transport code.                         !/*mc4c        2*/
!                                                                               !/*mc          5*/
!        files used:  unit, name, contents, access/form, and use modes.         !/*mc          6*/
!        = means that the user can change the name.                             !/*mc          7*/
!        iui =inp     problem specifications           s/f  input only          !/*mc          8*/
!        iuo =outp    output for printing              s/f  output only         !/*mc          9*/
!        iur =runtpe  problem start-restart data       s/u  input/output        !/*mc         10*/
!        ims =mdata   mesh data output and restart    s/u,f input/output        !/*mcx         2*/
!                     (if MESHTAL is defined)                                   !/*mcx         3*/
!        iux          cross-section files                   input only          !/*mc         11*/
!        iud =xsdir   cross-section tables directory   s/f  input only          !/*mc         12*/
!        iub          bank overflow                    s/u  input/output        !/*mc         13*/
!        ius =srctp   kcode source distribution        s/u  input/output        !/*mc         14*/
!        iusw=wssa    surface source write file        s/u  input/output        !/*mc         15*/
!        iusr=rssa    surface source read file         s/u  input/output        !/*mc         16*/
!        iuc =comout  output command file              s/f  output only         !/*mc         17*/
!        iuk          input command file               s/f  input only          !/*mc         18*/
!            =plotm   graphics metafile                s/f  output only         !/*mc         19*/
!        iut =mctal   tally output file                s/f  output only         !/*mc         20*/
!        iuz          tally input file                 s/f  input only          !/*mc         21*/
!        iusc wxxa    surface source scratch file      s/u  input/output        !/*mc         22*/
!        iupc         ptrak scratch file               s/u  input/output        !/*mc4b        2*/
!        iupw ptrac   ptrak particle track file        s/u  output only         !/*mc4b        3*/
!        iupx         mcplot printpts file             s/f  output only         !/*mc4b        4*/
!        iu1          scratch file                     s/f  input/output        !/*mc         23*/
!        iu2          scratch file                     s/f  input/output        !/*mc         24*/
!        iu3          scratch file                     s/u  input/output        !/*mc         25*/
!        iu4          scratch file                     s/f  input/output        !/*mc         26*/
!        itty         messages from the terminal       s/f  input only          !/*mc         27*/
!        jtty         messages to the terminal         s/f  output only         !/*mc         28*/
!                     user-defined files                                        !/*mc         30*/
!            =dumn1   dummy file name                                           !/*mc         31*/
!            =dumn2   dummy file name                                           !/*mc         32*/
!                                                                               !/*mc         33*/
#ifdef QWIN                                                                     /*!mc4c        3*/
      use dflib,only: setactiveqq,getwindowconfig,setwindowconfig,              !/*mc4c        4*/
     & windowconfig,setexitqq,qwin$exitpersist                                  !/*mc4c2       1*/
#endif                                                                          /*!cpp        45*/
      use global_data                                                           !/*mcx         4*/
      use message_passing                                                       !/*mcxk        1*/
!                                                                               !/*mcx         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*mcx         6*/
      implicit integer(kindi) (i-n)                                             !/*mcx         7*/
!                                                                               !/*mcx         8*/
      dimension ib(9)                                                           !/*mc         35*/
      character hp*80                                                           !/*mcx         9*/
      data ib/9*0/                                                              !/*mcx        10*/
#ifdef QWIN                                                                     /*!mc4c        7*/
      logical*4 st                                                              !/*mc4c        8*/
      type (windowconfig) wc                                                    !/*mc4c        9*/
#endif                                                                          /*!cpp        48*/
!                                                                               !/*mc         40*/
!****************************  start up.  ******************************        !/*mc         41*/
!                                                                      *        !/*mc         42*/
!                                                                               !/*mcx        11*/
!        initialize cpu clock.                                                  !/*mcx        12*/
      call secnd(tq)                                                            !/*mcx        13*/
!                                                                               !/*mc4b        6*/
!        get DATAPATH from environmental variable if possible.                  !/*mc4b        7*/
!ccc     DATAPATH must be upper case.                                           !/*mc4b        8*/
      hdpath=hdpth0                                                             !/*mc4c       10*/
! #ifdef UNIX                                                                   !/*mcxc        1*/
!       if(LIBPREFIX.ne.' ')hdpath=LIBPREFIX                                    !/*mcxc        2*/
! #endif                                                                        !/*mcxc        3*/
      hdpth=hdpath                                                              !/*mcxc        4*/
      hp=' '                                                                    !/*mc4c       12*/
#ifdef UNICOS                                                                   /*!mc4c       13*/
      call cgtenv('DATAPATH'//char(0),hp)                                       !/*mc4c       14*/
#endif                                                                          /*!cpp        49*/
#if defined(UNIX) || defined(DEC)                                               /*!mc4c       15*/
#ifdef GENERIC_ENV                                                              /*!mcx        14*/
      call generic_getenv('DATAPATH',hp)                                        !/*mcxc        5*/
#else                                                                           /*!mcx        16*/
      call getenv('DATAPATH',hp)                                                !/*mc4c       16*/
#endif                                                                          /*!mcx        17*/
#endif                                                                          /*!mcx        18*/
      if(hp.ne.' ')hdpath=hp                                                    !/*mc4c       17*/
!                                                                               !/*mc         51*/
!        open the file or files for the user's terminal.                        !/*mc         52*/
      itty=5                                                                    !/*mc         53*/
      jtty=6                                                                    !/*mc         54*/
#ifdef QWIN                                                                     /*!mc4c2       2*/
!                                                                               !/*mc4c       19*/
!        change the execute window title (unit 0).                              !/*mc4c       20*/
      k=setactiveqq(0)                                                          !/*mc4c       21*/
      st=getwindowconfig(wc)                                                    !/*mc4c       22*/
      wc.title="MCNP Execute Window"C                                           !/*mc4c       23*/
      st=setwindowconfig(wc)                                                    !/*mc4c       24*/
      k=setexitqq(qwin$exitpersist)                                             !/*mc4c2       3*/
#endif                                                                          /*!cpp        51*/
!                                                                               !/*mc4a        9*/
!        check for conversion of source code to upper case.                     !/*mc4a       10*/
!ccc     one "A" must be upper case.                                            !/*mc4a       11*/
      if('A'.eq.'a')call expire(0,kod,'source code not lower case.')            !/*mc4a       12*/
!                                                                               !/*mc         75*/
      mynum=0                                                                   !/*mc4c       25*/
#ifdef MULTP                                                                    /*!mc4a       13*/
!        enroll all tasks, shortcut for subtasks                                !/*mcx        19*/
      call menrl(mynum)                                                         !/*mc4a       14*/
      if(mynum.gt.0)call msgtsk                                                 !/*mc4c       26*/
#endif                                                                          /*!cpp        53*/
!                                                                               !/*mc         96*/
!        finish setting up storage.                                             !/*mc         97*/
   30 do 40 i=1,npblcm                                                          !/*mc         98*/
      gpblcm(i)=0.                                                              !/*mc         99*/
      do 40 j=1,mpb                                                             !/*mc        100*/
   40 gpb9cm(j,i)=0.                                                            !/*mc        101*/
      do 50 i=1,lpblcm                                                          !/*mc        102*/
      jpblcm(i)=0                                                               !/*mc        103*/
      do 50 j=1,mpb                                                             !/*mc        104*/
   50 jpb9cm(j,i)=0                                                             !/*mc        105*/
      do 60 i=1,ntskcm                                                          !/*mc        106*/
   60 gtskcm(i)=0.                                                              !/*mc        107*/
      do 70 i=1,ltskcm                                                          !/*mc        108*/
   70 jtskcm(i)=0                                                               !/*mc        109*/
      do 75 i=1,ltskpt                                                          !/*mc4b       16*/
   75 ktskpt(i)=0                                                               !/*mc4b       17*/
      do 80 i=1,nfixcm                                                          !/*mc        110*/
   80 gfixcm(i)=0.                                                              !/*mc        111*/
      do 90 i=1,lfixcm                                                          !/*mc        112*/
   90 jfixcm(i)=0                                                               !/*mc        113*/
      do 100 i=1,nvarcm                                                         !/*mc        114*/
  100 gvarcm(i)=0.                                                              !/*mc        115*/
      do 110 i=1,lvarcm                                                         !/*mc        116*/
  110 jvarcm(i)=0                                                               !/*mc        117*/
      do 120 i=1,nephcm                                                         !/*mc        118*/
  120 gephcm(i)=0.                                                              !/*mc        119*/
      do 130 i=1,lephcm                                                         !/*mc        120*/
  130 jephcm(i)=0                                                               !/*mc        121*/
      zfixcm=one                                                                !/*mc        122*/
      mfixcm=1                                                                  !/*mc        123*/
      zvarcm=one                                                                !/*mc        124*/
      mvarcm=1                                                                  !/*mc        125*/
      zephcm=one                                                                !/*mc        126*/
      mephcm=1                                                                  !/*mc        127*/
      zpblcm=one                                                                !/*mc        128*/
      zpb9cm(1)=one                                                             !/*mc        129*/
      mpblcm=1                                                                  !/*mc        130*/
      mpb9cm(1)=1                                                               !/*mc        131*/
      ztskcm=one                                                                !/*mc        132*/
      mtskcm=1                                                                  !/*mc        133*/
      mtskpt=1                                                                  !/*mc4b       18*/
      lockl=.false.                                                             !/*mc        134*/
      call lblocs(one)                                                          !/*mc        135*/
      do 135 i=1,ndef                                                           !/*mcx        20*/
  135 isub(i)=' '                                                               !/*mcx        21*/
!                                                                               !/*mc        136*/
!        get or set the initial conditions for the run.                         !/*mc        137*/
      iterm=3                                                                   !/*mc        138*/
#ifndef GKSSIM                                                                  /*!mc4a       16*/
#ifdef AIX                                                                      /*!mc4a       17*/
      iterm=1                                                                   !/*mc4a       18*/
#endif                                                                          /*!cpp        55*/
#ifdef UNICOS                                                                   /*!mc4a       19*/
      iterm=4115                                                                !/*mc4a       20*/
#endif                                                                          /*!cpp        56*/
#endif                                                                          /*!cpp        54*/
      mcolor=2                                                                  !/*mc4a       22*/
      lspeed=9600                                                               !/*mc        139*/
      call getexm(exms)                                                         !/*mc        140*/
      call getidt(idtm)                                                         !/*mc        144*/
!        inhibit warning messages from library subroutines. (xsetf)             !/*mc        146*/
!        set kprod.ne.0 if in production mode.                                  !/*mc        147*/
!        put the charge code in chcd.                                           !/*mc        148*/
!        set the code designator in the charge code id.                         !/*mc4c       27*/
!                                                                               !/*mc        165*/
!        print startup message at tty.  interpret execution messages.           !/*mc        166*/
      write(jtty,140)kod,ver,loddat,idtm                                        !/*mcx        22*/
  140 format(1x,a8,5h ver=,a5,4h ld=,a28,1x,a19/)                               !/*mcx        23*/
      call exemes                                                               !/*mc        169*/
      if(dbcn(26).eq.0.)call lex(jtty)                                          !/*mcx        24*/
      if(konrun.eq.0)probid=idtm                                                !/*mc        170*/
!                                                                               !/*mc        171*/
      ntasks=max(1,ntasks)                                                      !/*mc4a       24*/
      ltasks=max(1,ltasks)                                                      !/*mc4c       28*/
#ifdef MULTT                                                                    /*!mcxk        2*/
      call thread_init()                                                        !/*mcxk        3*/
#endif                                                                          /*!mcxk        4*/
!                                                                               !/*mc        192*/
!        set up default values for unspecified file names.                      !/*mc        193*/
      l=leng(iname)                                                             !/*mc4a       30*/
      do 160 i=1,ndef                                                           !/*mc        194*/
      if(isub(i).ne.' '.or.iname.eq.' '.or.i.gt.10)go to 160                    !/*mc4c       30*/
      isub(i)=iname(1:l)//msub(i)(1:1)                                          !/*mc4a       32*/
      if(msub(i).eq.'wwone')isub(i)=iname(1:l)//'1'                             !/*mc4c       31*/
      if(msub(i).eq.'wwout')isub(i)=iname(1:l)//'e'                             !/*mc4c       32*/
      if(inqire(isub(i)).ne.0)call expire(0,'main',                             !/*mc4c       33*/
     & 'name='//iname//' fails. '//isub(i)//' already exists.')                 !/*mc4a       34*/
  160 if(isub(i).eq.' '.and.msub(i).ne.'com')isub(i)=msub(i)                    !/*mc        195*/
!                                                                               !/*mc        196*/
!        create the output print file and write heading on it.                  !/*mc        197*/
      call unique(outp,jtty)                                                    !/*mc        198*/
      open(iuo,file=outp,status='new')                                          !/*mc        199*/
      iuou=iuo                                                                  !/*mc        200*/
      if(konrun.eq.0)write(iuo,170)kod,ver,loddat,chcd,idtm,probid              !/*mc        201*/
      if(konrun.ne.0)write(iuo,170)kod,ver,loddat,chcd,idtm                     !/*mc        202*/
  170 format(1h1,a8,9h version ,a5,4h ld=,a28,3x,a10,1x,a19/                    !/*mcx        25*/
     & 1x,87(1h*),:3x,9hprobid = ,a19)                                          !/*mcx        26*/
      write(iuo,'(1x,a80/)')exms                                                !/*mc        205*/
!                                                                               !/*mc        208*/
      call ttyint                                                               !/*mc        212*/
!                                                                               !/*mc        214*/
!**********************  execute the sections.  ************************        !/*mc        215*/
!                                                                      *        !/*mc        216*/
!        if none are specified, default to ixr.                                 !/*mc        217*/
      if(jovr(1)+jovr(2)+jovr(3)+jovr(5).eq.0)jovr(4)=1                         !/*mc        218*/
      if(jovr(3)+jovr(4).ne.0)jovr(1)=1                                         !/*mc        219*/
      if(konrun.ne.0)jovr(3)=0                                                  !/*mc        220*/
      if(konrun.eq.0.and.jovr(4).ne.0)jovr(3)=1                                 !/*mc        221*/
!                                                                               !/*mc        222*/
!        execute the required sections.                                         !/*mc        223*/
  175 do 230 iovr=1,novr                                                        !/*mc4a       38*/
      if(iovr.eq.4.and.nfer.ne.0.and.lfatl.eq.0)go to 240                       !/*mc4b       24*/
      if(jovr(iovr).eq.0)go to 230                                              !/*mc        229*/
      if(iovr.eq.1)call imcn                                                    !/*mc        230*/
#ifdef PLOT                                                                     /*!mc        231*/
      if(iovr.eq.2)call plotg                                                   !/*mc4a       42*/
#endif                                                                          /*!mcx        27*/
      if (iovr.eq.3) then                                                       !/*mcx        28*/
         if (lcaopt(8).eq.-1)go to 230                                          !/*mcx        29*/
         call xact                                                              !/*mcx        30*/
      endif                                                                     !/*mcx        31*/
      if (iovr.eq.4) then                                                       !/*mcx        32*/
         call mcrun                                                             !/*mcx        33*/
         ifile=0                                                                !/*mcx        34*/
         mrm=0                                                                  !/*mcx        35*/
      endif                                                                     !/*mcx        36*/
#ifdef MCPLOT                                                                   /*!mc        235*/
      if(iovr.eq.5)call mcplot                                                  !/*mc        236*/
#endif                                                                          /*!cpp        64*/
      write(jtty,180)hovr                                                       !/*mc        237*/
  180 format(1x,a6,8h is done)                                                  !/*mc        238*/
      if(nwer.eq.1)write(iuo,190)                                               !/*mc        239*/
  190 format(/10x,25h1 warning message so far.)                                 !/*mc        240*/
      if(nwer.gt.1)write(iuo,200)nwer                                           !/*mc        241*/
  200 format(/i10,25h warning messages so far.)                                 !/*mc        242*/
      if(nfer.eq.1)write(iuo,210)                                               !/*mc        243*/
  210 format(/10x,21h1 fatal error so far.)                                     !/*mc        244*/
      if(nfer.gt.1)write(iuo,220)nfer                                           !/*mc        245*/
  220 format(/i10,21h fatal errors so far.)                                     !/*mc        246*/
  230 continue                                                                  !/*mc4b       27*/
  240 iovr=0                                                                    !/*mc        248*/
!                                                                               !/*mc        254*/
!        print termination messages and quit.                                   !/*mc        255*/
  250 if(nfer.ne.0.and.lfatl.eq.0.and.jovr(4).ne.0)nst=nst+2048                 !/*mc        256*/
      do 260 i=1,11                                                             !/*mc        257*/
  260 if(mod(nst/2**i,2).ne.0)write(iuo,'(//16h run terminated ,'//             !/*mc        258*/
     & rfq(i)//')')                                                             !/*mc        259*/
      call secnd(tq)                                                            !/*mc4a       43*/
      if(mct.ge.0)write(iuo,'(/16h computer time =,f8.2,8h minutes)')           !/*mc4a       44*/
     & (tq+cp3)/60.                                                             !/*mcx        37*/
      call getidt(idtm)                                                         !/*mc        260*/
      if(mct.ge.0)write(iuo,270)kod,ver,loddat,chcd,idtm,probid                 !/*mc4a       46*/
  270 format(/1x,a8,9h version ,a5,1x,a28,4x,a10,1x,a19,5x,9hprobid = ,         !/*mcx        38*/
     & a19)                                                                     !/*mc        263*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!mc4a       47*/
!        terminate graphics.                                                    !/*mcx        39*/
      call gxquit                                                               !/*mc        265*/
#endif                                                                          /*!cpp        65*/
#ifdef MULTP                                                                    /*!mc4a       48*/
!        stop master task.                                                      !/*mcx        40*/
      if(mynum.eq.0)call mterm                                                  !/*mcx        41*/
#endif                                                                          /*!cpp        66*/
      stop                                                                      !/*mc        270*/
      end                                                                       !/*mc        271*/

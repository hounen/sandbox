!#ident exemes.F from deck em                                                   !/*em          1*/
      subroutine exemes                                                         !/*em          2*/
!        interpret the execute line message and the inp message block.          !/*em          3*/
      use global_data                                                           !/*emx         1*/
      use message_passing                                                       !/*emxk        1*/
!                                                                               !/*emx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*emx         3*/
      implicit integer(kindi) (i-n)                                             !/*emx         4*/
!                                                                               !/*emx         5*/
      parameter (nk=8)                                                          !/*em          5*/
      character hk(nk)*6,hm(64)*10,ho*132,hs*8                                  !/*em4a        1*/
      data hk/'c','cn','dbug','notek','fatal',' ','print','tasks'/              !/*em4b        1*/
!                                                                               !/*em          8*/
      jr=0                                                                      !/*em          9*/
      kb=0                                                                      !/*em         10*/
      kr=0                                                                      !/*em         11*/
      kt=0                                                                      !/*em         12*/
!                                                                               !/*em         13*/
!        do the execute line message and the inp message block.                 !/*em         14*/
      do 250 ls=1,2                                                             !/*em         15*/
      do 10 i=1,64                                                              !/*em         16*/
   10 hm(i)=' '                                                                 !/*em         17*/
!                                                                               !/*em         18*/
!        ls=1:  set up to interpret the execute line message.                   !/*em         19*/
      if(ls.ne.1)go to 30                                                       !/*em         20*/
      call ckchar(exms,jtty,iuou)                                               !/*em         21*/
      j=0                                                                       !/*em         22*/
      do 20 ni=0,62                                                             !/*em         23*/
      call nxtsym(exms,' =',j+1,i,j,0)                                          !/*em4a        2*/
      if(j.eq.0)go to 80                                                        !/*em         25*/
   20 hm(ni+1)=exms(i:j)                                                        !/*em         26*/
      go to 280                                                                 !/*em         27*/
!                                                                               !/*em         28*/
!        ls=2:  set up to interpret the inp message block.                      !/*em         29*/
   30 jr=jovr(1)+jovr(2)+jovr(3)+jovr(4)+jovr(5)                                !/*em         30*/
      if(jr.eq.1.and.jovr(2).eq.1)return                                        !/*em         31*/
      kr=konrun                                                                 !/*em         32*/
      kb=nde                                                                    !/*em         33*/
      kt=ntasks                                                                 !/*em         34*/
!                                                                               !/*em         35*/
!        open the inp file.                                                     !/*em         36*/
      if(inp.eq.' ')inp=iname                                                   !/*em4a        3*/
      if(inp.eq.' ')inp='inp'                                                   !/*em         37*/
      i=inqire(inp)                                                             !/*em         38*/
      if(i.eq.0.and.(konrun.eq.0.or.inp.ne.'inp').and.jr*jovr(5).ne.1)          !/*em         39*/
     & call expire(0,'exemes','input file '//inp//' does not exist.')           !/*em         40*/
      if(i.eq.0)go to 250                                                       !/*em         41*/
      open(iui,file=inp,status='old')                                           !/*em         45*/
      rewind iui                                                                !/*em         46*/
!                                                                               !/*em4a        4*/
!        check for data other than comments beyond column 80.                   !/*em4a        5*/
   36 read(iui,'(a132)',err=38,end=38)ho                                        !/*em4a        6*/
      i=index(ho,'$')                                                           !/*em4a        7*/
      if(i.gt.0)ho(i:132)=' '                                                   !/*em4a        8*/
      if(ho(81:132).eq.' ')go to 36                                             !/*em4a        9*/
      if(ho(1:5).eq.' ')go to 37                                                !/*em4a       10*/
      hs=ho(1:8)                                                                !/*em4a       11*/
      call nxtsym(hs,' ',1,it,iu,1)                                             !/*em4a       12*/
      if(hs(it:it).eq.'c')go to 36                                              !/*em4a       13*/
   37 write(jtty,'(a70)')ho(1:70)                                               !/*em4a       14*/
      call expire(0,'exemes','input file data beyond column 80.')               !/*em4a       15*/
   38 rewind iui                                                                !/*em4a       16*/
!                                                                               !/*em         47*/
!        get the symbols from the message block.                                !/*em         48*/
      ni=0                                                                      !/*em         50*/
      do 60 ip=1,3                                                              !/*em         51*/
   40 read(iui,'(a80)',end=260)aid1                                             !/*em         52*/
      call ckchar(aid1,jtty,iuou)                                               !/*em         53*/
      kf=0                                                                      !/*em4a       17*/
      if(ip.eq.2)go to 50                                                       !/*em4a       18*/
      call nxtsym(aid1,' ',1,i,kf,0)                                            !/*em4a       19*/
      hs=aid1(i:min(i+7,kf))                                                    !/*em4a       20*/
      call nxtsym(hs,' ',1,j,k,2)                                               !/*em4a       21*/
      if(ip.eq.3.or.ip.eq.1.and.hs.ne.'message:')go to 70                       !/*em4a       22*/
   50 call nxtsym(aid1,' =$&',kf+1,i,kf,0)                                      !/*em4a       23*/
      if(kf.eq.0)go to 60                                                       !/*em         56*/
      if(aid1(i:i).eq.'$'.or.aid1(i:i).eq.'&')go to 60                          !/*em4a       24*/
      if(ni.ge.62)go to 280                                                     !/*em         57*/
      ni=ni+1                                                                   !/*em         58*/
      hm(ni)=aid1(i:kf)                                                         !/*em         59*/
      hs=hm(ni)(1:8)                                                            !/*em4a       25*/
      call nxtsym(hs,' ',1,i,j,2)                                               !/*em4a       26*/
      if(hs.ne.'datapath')go to 50                                              !/*em4a       27*/
      call nxtsym(aid1,' =',kf+1,i,kf,0)                                        !/*em4a       28*/
      if(kf.ne.0.and.aid1(i:i).eq.'=')call nxtsym(aid1,' =',kf+1,i,kf,0)        !/*em4a       29*/
      if(kf.ne.0)hdpth=hdpath                                                   !/*em4b        2*/
      if(kf.ne.0)hdpath=aid1(i:kf)                                              !/*cor4-2     36*/
      ni=ni-1                                                                   !/*cor4-2     37*/
      go to 50                                                                  !/*em         60*/
   60 if(ip.eq.2.and.aid1.ne.' ')go to 40                                       !/*em         61*/
!                                                                               !/*em         62*/
!        check for the continue card for a continue run.                        !/*em         63*/
   70 if(konrun.eq.0.or.hs.eq.'continue')go to 80                               !/*em4a       30*/
      if(inp.ne.'inp')call expire(0,'exemes',                                   !/*em         65*/
     & 'no continue card in file '//inp)                                        !/*em         66*/
      return                                                                    !/*em         67*/
!                                                                               !/*em         68*/
!        interpret the message symbols.                                         !/*em         69*/
   80 ij=1                                                                      !/*em         70*/
      do 240 js=1,ni                                                            !/*em         71*/
      if(js.lt.ij)go to 240                                                     !/*em         72*/
!                                                                               !/*em         73*/
!        change a file name if the next symbol is =.                            !/*em         74*/
      if(hm(js+1).ne.'=')go to 110                                              !/*em         75*/
      call nxtsym(hm(js),' ',1,i,j,2)                                           !/*em4a       31*/
      l=leng(hm(js))                                                            !/*em4a       32*/
      do 90 j=1,ndef                                                            !/*em         76*/
   90 if(msub(j)(1:l).eq.hm(js)(1:l).neqv.(hm(js).eq.'com'.and.msub(j)          !/*em4a       33*/
     & .eq.'comout'))go to 100                                                  !/*em4a       34*/
      iname='name'                                                              !/*em4a       35*/
      if(iname(1:l).ne.hm(js)(1:l))call expire(0,'exemes',                      !/*em4a       36*/
     & 'file name '//hm(js)//' in the execution message unrecognized.')         !/*em4a       37*/
      if(leng(hm(js+2)).gt.7)call expire(0,'exemes',                            !/*em4a       38*/
     & 'too many characters in name '//hm(js+2)//'  maximum is 7.')             !/*em4a       39*/
      iname=hm(js+2)                                                            !/*em4a       40*/
      ij=js+3                                                                   !/*em4a       41*/
      go to 240                                                                 !/*em4a       42*/
  100 if(ls.eq.1.or.isub(j).eq.' ')isub(j)=hm(js+2)                             !/*em         80*/
      if(leng(hm(js+2)).gt.8)call expire(0,'exemes',                            !/*em4a       43*/
     & 'too many characters in file name '//hm(js+2)//'  maximum is 8.')        !/*em         83*/
      ij=js+3                                                                   !/*em         84*/
      go to 240                                                                 !/*em         85*/
!                                                                               !/*em         86*/
!        set section run flags if the symbol contains only ipxrz.               !/*em         87*/
  110 call nxtsym(hm(js),' ',1,i,j,2)                                           !/*em4a       44*/
      do 120 m=1,10                                                             !/*em4a       45*/
  120 if(index('ipxrz ',hm(js)(m:m)).eq.0)go to 140                             !/*em         89*/
      do 130 m=1,10                                                             !/*em         90*/
      j=index('ipxrz ',hm(js)(m:m))                                             !/*em         91*/
  130 if(jr.eq.0.and.j.lt.6)jovr(j)=1                                           !/*em         92*/
#ifndef PLOT                                                                    /*!em         93*/
      if(jovr(2).ne.0)call expire(0,'exemes',                                   !/*em         94*/
     & 'the plot feature is not included in this compilation.')                 !/*em4c        2*/
#endif                                                                          /*!cpp        69*/
#ifndef MCPLOT                                                                  /*!em         96*/
!        expire if mcplotting is wanted but is not available.                   !/*emx         6*/
      if(jovr(5).ne.0)call expire(0,'exemes',                                   !/*em         97*/
     & 'mcplot feature is not included in this compilation.')                   !/*em4c        3*/
#endif                                                                          /*!cpp        70*/
      go to 240                                                                 !/*em         99*/
!                                                                               !/*em        100*/
!        otherwise interpret the symbol as a keyword.                           !/*em        101*/
  140 i=index(hm(js),'.')                                                       !/*em        102*/
      if(i.gt.0)hm(js)(i:i)=' '                                                 !/*em        103*/
      do 150 j=1,nk                                                             !/*em        104*/
  150 if(hk(j).eq.hm(js))go to 160                                              !/*em        105*/
      go to 270                                                                 !/*em        106*/
  160 go to(170,170,180,190,200,240,220,230)j                                   !/*em4b        3*/
!                                                                               !/*em        108*/
! >>>>>  keyword c -- continue run from dump n.  run imcn and mcrun.            !/*em        109*/
!        keyword cn -- same as c but over-write the dumps.                      !/*em        110*/
  170 if(hm(js).eq.'cn')jfcn=1                                                  !/*em        111*/
      jovr(1)=1                                                                 !/*em        112*/
      jovr(4)=1                                                                 !/*em        113*/
      konrun=1                                                                  !/*em        114*/
      k=kdata(hm(js+1))                                                         !/*em        115*/
      if(k.ne.2.or.js.eq.ni)go to 240                                           !/*em        116*/
      if(kr.eq.0)read(hm(js+1),'(bn,i10)')idmp                                  !/*em        117*/
      ij=js+2                                                                   !/*em        118*/
      go to 240                                                                 !/*em        119*/
!                                                                               !/*em        120*/
! >>>>>  keyword dbug -- write a dbug line every n particles.                   !/*em        121*/
  180 k=kdata(hm(js+1))                                                         !/*em        122*/
      if(k.ne.2.or.js.eq.ni)call expire(0,'exemes',                             !/*em        123*/
     & 'trouble converting n after keyword dbug.')                              !/*em        124*/
      if(kb.eq.0)read(hm(js+1),'(bn,i10)')nde                                   !/*em        125*/
      ij=js+2                                                                   !/*em        126*/
      go to 240                                                                 !/*em        127*/
!                                                                               !/*em        128*/
! >>>>>  keyword notek -- the terminal is not a graphics terminal.              !/*em        129*/
  190 iterm=0                                                                   !/*em        130*/
      go to 240                                                                 !/*em        131*/
!                                                                               !/*em        132*/
! >>>>>  keyword fatal -- run in spite of fatal errors.                         !/*em        133*/
  200 lfatl=1                                                                   !/*em        134*/
      go to 240                                                                 !/*em        135*/
!                                                                               !/*em        140*/
! >>>>>  keyword print -- do maximum printing.                                  !/*em        141*/
  220 mnk=1                                                                     !/*em        142*/
      go to 240                                                                 !/*em        143*/
!                                                                               !/*em        144*/
! >>>>>  keyword tasks -- run n simultaneous tasks in mcrun.                    !/*em        145*/
!        form is <ntasks> or <jtasks>x<ntasks>.                                 !/*em4c        4*/
!        ltasks = abs(jtasks) controls pvm mode.                                !/*em4c        5*/
  230 if(kt.ne.0)go to 237                                                      !/*em4c        6*/
      if(js.eq.ni)go to 236                                                     !/*em4c        7*/
      j=index(hm(js+1),'x')                                                     !/*em4c        8*/
      l=leng(hm(js+1))                                                          !/*em4c        9*/
      if(j.eq.0)go to 232                                                       !/*em4c       10*/
      if(j.eq.1.or.j.eq.l)go to 236                                             !/*em4c       11*/
      read(hm(js+1)(1:j-1),*,err=236)jtasks                                     !/*emxk        2*/
      ltasks=abs(jtasks)                                                        !/*em4c       13*/
  232 read(hm(js+1)(j+1:l),*,err=236)ntasks                                     !/*emxk        3*/
#if !defined(MULTT) && !defined(MULTP)                                          /*!emx         7*/
!                                                                               !/*emx         8*/
!        no multiprocessing.                                                    !/*emx         9*/
      if(ntasks.gt.1.or.ltasks.gt.1)call expire(0,'exemes',                     !/*emx        10*/
     & 'multiprocessing is unavailable in this compilation.')                   !/*emx        11*/
#endif                                                                          /*!emx        12*/
#ifdef MULTT                                                                    /*!emx        13*/
#ifndef MULTP                                                                   /*!emx        14*/
!                                                                               !/*emx        15*/
!        multitasking only.                                                     !/*emx        16*/
      if(j.gt.0.and.ltasks.gt.1)call expire(0,'exemes',                         !/*emx        17*/
     & 'message-passing is unavailable in this compilation.')                   !/*emx        18*/
#endif                                                                          /*!emx        19*/
#endif                                                                          /*!emx        20*/
#ifdef MULTP                                                                    /*!emx        21*/
#ifndef MULTT                                                                   /*!emx        22*/
!                                                                               !/*emx        23*/
!        message-passing only.                                                  !/*emx        24*/
      if(j.eq.0)then                                                            !/*emx        25*/
       jtasks=ntasks                                                            !/*emxk        4*/
       ltasks=abs(jtasks)                                                       !/*emxk        5*/
       ntasks=1                                                                 !/*emx        27*/
      endif                                                                     !/*emx        28*/
      if(j.gt.0.and.ntasks.gt.1)call expire(0,'exemes',                         !/*emx        29*/
     & 'multitasking is unavailable in this compilation.')                      !/*emx        30*/
#endif                                                                          /*!emx        31*/
!                                                                               !/*emx        32*/
!        message-passing with or without multitasking.                          !/*emx        33*/
      if(ltasks.gt.1)then                                                       !/*emx        34*/
       if(mynum.lt.0)call expire(0,'exemes',                                    !/*emx        35*/
     & 'pvm not initialized, invoke pvm daemon first.')                         !/*emx        36*/
       call mnump(n,k)                                                          !/*emx        37*/
       if(k.eq.0.and.n.lt.ltasks)call erprnt(1,2,2,n,ltasks,0,0,-1,             !/*emx        38*/
     & '5honly ,i3,31h host(s) available through pvm,,i3,'//                    !/*emx        39*/
     & '17h tasks requested.')                                                  !/*emx        40*/
        lchnk=min(10000000,(625/ltasks)*100000)                                 !/*emx        41*/
      endif                                                                     !/*emx        42*/
#endif                                                                          /*!emx        43*/
#if defined(MULTT) && defined(MULTP)                                            /*!emx        44*/
!                                                                               !/*emx        45*/
!        multitasking and message-passing.                                      !/*emx        46*/
      if(j.eq.0)call expire(0,'exemes',                                         !/*emx        47*/
     & 'format of tasks keyword is #x# - see manual.')                          !/*emx        48*/
#endif                                                                          /*!emx        49*/
      if(ntasks.lt.mcpu.and.ltasks.lt.mcpu)go to 237                            !/*em4c       30*/
      call expire(0,'exemes',                                                   !/*em4c       31*/
     & 'number of tasks exceeds maximum, increase parameter mcpu.')             !/*em4a       59*/
  236 call expire(0,'exemes',                                                   !/*em4c       32*/
     & 'trouble converting parameter after keyword tasks.')                     !/*em4c       33*/
!                                                                               !/*em4c       34*/
  237 ij=js+2                                                                   !/*em4b       13*/
!                                                                               !/*em        155*/
  240 continue                                                                  !/*em        156*/
  250 continue                                                                  !/*em        157*/
      return                                                                    !/*em        158*/
!                                                                               !/*em        159*/
!        quit if anything is wrong in a message.                                !/*em        160*/
  260 call expire(0,'exemes','unexpected eof in file '//inp)                    !/*em        161*/
  270 call expire(0,'exemes',                                                   !/*em        162*/
     & 'unrecognized symbol in the execution message:  '//hm(js))               !/*em        163*/
  280 call expire(0,'exemes',                                                   !/*em        164*/
     & 'too many symbols in the execution message.')                            !/*em        165*/
      end                                                                       !/*em        166*/

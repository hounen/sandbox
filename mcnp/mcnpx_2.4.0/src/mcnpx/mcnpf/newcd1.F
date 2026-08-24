!#ident newcd1.F from deck nf                                                   !/*nf          1*/
      subroutine newcd1(ib)                                                     !/*nf          2*/
!        do the initial processing of a new card.                               !/*nf          3*/
      use allocate_memory                                                       !/*nfx         1*/
      use global_data                                                           !/*nfx         2*/
      use imcn_data                                                             !/*nfx         3*/
!                                                                               !/*nfx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*nfx         5*/
      implicit integer(kindi) (i-n)                                             !/*nfx         6*/
!                                                                               !/*nfx         7*/
!                                                                               !/*nf          7*/
!        initialize flags and scratch storage.                                  !/*nf          8*/
      nwc=0                                                                     !/*nf          9*/
      nii=0                                                                     !/*nf         10*/
      m1c=0                                                                     !/*nf         11*/
      m2c=0                                                                     !/*nf         12*/
      m3c=0                                                                     !/*nf         13*/
      m4c=0                                                                     !/*nf         14*/
      m5c=0                                                                     !/*nf4a        1*/
      m6c=0                                                                     !/*nf4a        2*/
      m7c=0                                                                     !/*nf4a        3*/
      m8c=0                                                                     !/*nf4a        4*/
      m9c=0                                                                     !/*nf4a        5*/
      m10c=0                                                                    !/*nf4a        6*/
!                                                                               !/*nf         15*/
!        set icn to the value of the number in the card type field.             !/*nf4a        7*/
!        set icx=0,1,2 for nothing , * , + in card type field.                  !/*nf4a        8*/
!        set ich and ica to the card symbol and its index.                      !/*nf         26*/
!        for the sake of easier maintenance, ica should only be used in         !/*nf         27*/
!        computed gotos and as the index of card attribute arrays.              !/*nf         28*/
      ich=' '                                                                   !/*nf         29*/
      icx=0                                                                     !/*nf4a        9*/
      icn=0                                                                     !/*nf4a       10*/
      nqw=0                                                                     !/*nf         30*/
      do 20 i=1,mipt                                                            !/*nf         31*/
   20 nqp(i)=0                                                                  !/*nf         32*/
      n=0                                                                       !/*nf         33*/
      call nxtsym(klin,' ',1,it,iu,1)                                           !/*nf4a       11*/
      do 30 irc=it,iu                                                           !/*nf4a       12*/
      if(klin(irc:irc).eq.':')go to 40                                          !/*nf4a       13*/
      if(klin(irc:irc).eq.'*')icx=1                                             !/*nf4a       14*/
      if(klin(irc:irc).eq.'+')icx=2                                             !/*nf4a       15*/
      m=index('0123456789',klin(irc:irc))                                       !/*nf4a       16*/
      if(m.ne.0)icn=10*icn+m-1                                                  !/*nf4a       17*/
      l=index('abcdefghijklmnopqrstuvwxyz',klin(irc:irc))                       !/*nf4a       18*/
      if(l.ne.0)n=n+1                                                           !/*nf4a       19*/
   30 if(l.ne.0)ich(n:n)=klin(irc:irc)                                          !/*nf4a       20*/
      go to 50                                                                  !/*nf4a       21*/
   40 irc=irc+1                                                                 !/*nf         42*/
      if(klin(irc:irc).eq.' ')go to 50                                          !/*nf         43*/
      i=index(','//hip,klin(irc:irc))-1                                         !/*nf         44*/
      if(i.gt.0)nqw=nqw+1                                                       !/*nfx         8*/
      if(i.gt.0)nqp(i)=1                                                        !/*nf         46*/
      if(i.ge.0)go to 40                                                        !/*nf         47*/
      go to 90                                                                  !/*nf         48*/
   50 continue                                                                  !/*nfx         9*/
#ifdef MESHTAL                                                                  /*!nfx        10*/
      if (mod(icn,10).eq.6.and.icx.eq.2.and.ich.eq.'f') then                    !/*nfx        11*/
         nqw=1                                                                  !/*nfx        12*/
         nqp(1)=1                                                               !/*nfx        13*/
      endif                                                                     !/*nfx        14*/
#endif                                                                          /*!nfx        15*/
      if(ib.eq.3)go to 60                                                       !/*nfx        16*/
      ica=ib                                                                    !/*nf         50*/
      if(ich.ne.' ')go to 90                                                    !/*nf         51*/
      go to 80                                                                  !/*nf         52*/
   60 do 70 ica=3,nkcd                                                          !/*nf         53*/
   70 if(cnm(ica).eq.ich)go to 80                                               !/*nf         54*/
      go to 90                                                                  !/*nf         55*/
   80 if(krq(2,ica).eq.0.or.nqw.ne.0 )go to 100                                 !/*nf         56*/
   90 ics=-1                                                                    !/*nf         57*/
      return                                                                    !/*nf         58*/
  100 ndup(min(ica,3))=ndup(min(ica,3))+1                                       !/*nf         59*/
#ifdef PCDOS                                                                    /*!nf4a       22*/
      if(lockl)call pttyin                                                      !/*nf4a       23*/
#endif                                                                          /*!cpp       105*/
      if(irup.ne.0)call qttyin(0,'30hreading input cards.     '//ich)           !/*nf4a       24*/
!                                                                               !/*nf         60*/
!        do the processing necessary for the particular card type.              !/*nf         61*/
!a       this and six similar computed go tos are in two sections to            !/*nf         62*/
!a       accomodate a limitation in the prime compiler.                         !/*nf         63*/
      go to(120,130,140,110,110,150,110,110,110,110,110,170,110,190,220,        !/*nf         64*/
     &      110,110,110,110,110,110,110,210,220,220,220,220,220,220,220,        !/*nf         65*/
     &      220,220,220,220,220,220,220,220,220,220,220,220,220,220,220,        !/*nf         66*/
     &      220,110,250,110,110,110,110,110,110,110)ica                         !/*nf         67*/
      go to(260,110,110,110,110,110,110,110,110,110,110,110,268,270,110,        !/*nf4b        1*/
     &      280,110,300,310,316,320,320,322,110,110,110,330,110,110,340,        !/*nf4b        2*/
     &      110,270,110,110,110,110,110,110,400,110,110,220,220,220,110,        !/*nfx        17*/
     &      110,110,9910,9920,9930)ica-55                                       !/*nfxc        1*/
  110 return                                                                    !/*nf         71*/
!                                                                               !/*nf         72*/
! >>>>>  cell descriptions                                                      !/*nf         73*/
  120 mxa=mxa+1                                                                 !/*nf         74*/
      mxafs=mxa                                                                 !/*nf         75*/
      if(mxa.gt.size(ncl))then                                                  !/*nfx        19*/
        ncl => chg_mem (ncl,mxa+100,is)                                         !/*nfx        20*/
        if(is.ne.0)call expire(0,'newcd1','chg_mem error')                      !/*nfx        21*/
      endif                                                                     !/*nfx        22*/
      ncl(lncl+mxa)=icn                                                         !/*nf         77*/
      return                                                                    !/*nf         78*/
!                                                                               !/*nf         79*/
! >>>>>  surface descriptions                                                   !/*nf         80*/
  130 mxj=mxj+1                                                                 !/*nf         81*/
      if(mxj.gt.size(nsfm))then                                                 !/*nfx        23*/
        nsfm => chg_mem (nsfm,mxj+100,is)                                       !/*nfx        24*/
        if(is.ne.0)call expire(0,'newcd1','chg_mem error')                      !/*nfx        25*/
      endif                                                                     !/*nfx        26*/
      nsfm(lnsf+mxj)=icn                                                        !/*nf4c        2*/
      return                                                                    !/*nf         84*/
!                                                                               !/*nf         85*/
! >>>>>  coordinate transformations for surfaces                      tr        !/*nf         86*/
  140 mxtr=mxtr+1                                                               !/*nf         87*/
      return                                                                    !/*nf         88*/
!                                                                               !/*nf         89*/
! >>>>>  cell importances                                            imp        !/*nf         90*/
  150 do 160 i=1,mipt                                                           !/*nf         91*/
  160 if(nqp(i).ne.0)ifip(i)=1                                                  !/*nf         92*/
      return                                                                    !/*nf         93*/
!                                                                               !/*nf         94*/
! >>>>>  weight-window lower bounds                                  wwn        !/*nf         95*/
  170 do 180 i=1,mipt                                                           !/*nf         96*/
  180 if(nqp(i).ne.0)nww(i)=max(1,icn,nww(i))                                   !/*nf         97*/
      return                                                                    !/*nf         98*/
!                                                                               !/*nf         99*/
! >>>>>  weight-window game parameters                               wwp        !/*nf        100*/
  190 do 200 i=1,mipt                                                           !/*nf        101*/
  200 if(nqp(i).ne.0)nww(i)=max(1,nww(i))                                       !/*nf        102*/
      return                                                                    !/*nf        103*/
!                                                                               !/*nf        104*/
! >>>>>  source definition                                          sdef        !/*nf        105*/
  210 if(nsr.eq.0.and.klin(it:iu).ne.'sdef0')nsr=40                             !/*nf4c        3*/
      if(nsr.eq.71)ksdef=1                                                      !/*nf        107*/
      return                                                                    !/*nf        108*/
!                                                                               !/*nf        109*/
! >>>>>  tally cards            pd,f,fx,fy,fz,pi,tir,tic,ft,fc,e,t,c,fq,        !/*nfx        27*/
!                                 fm,de,df,em,tm,cm,cf,sf,fs,sd,fu,tf,dd        !/*nfx        28*/
  220 if(ich.eq.'dd'.and.icn.le.2)return                                        !/*nfx        30*/
#ifndef RADIOG                                                                  /*!nfx        31*/
      if(ich.eq.'pi'.or.ich.eq.'tir'.or.ich.eq.'tic')return                     !/*nfx        32*/
      if(ich.eq.'c'.and.mod(icn,10).eq.5)return                                 !/*nfx        33*/
      if(ich.eq.'fs'.and.mod(icn,10).eq.5)return                                !/*nfx        34*/
#endif                                                                          /*!nfx        35*/
      do 230 ital=0,ntal                                                        !/*nf        114*/
  230 if(abs(ntl(ital)).eq.icn)go to 240                                        !/*nfx        36*/
      ntal=ntal+1                                                               !/*nf        116*/
      if(ntal.eq.ntalmx+1)call erprnt(1,1,1,ntalmx,0,0,0,0,                     !/*nf        117*/
     & '10hmore than ,i3,27h tallies.  increase ntalmx.')                       !/*nf        118*/
      ntl(ntal)=-icn                                                            !/*nfx        37*/
  240 if(index('f   fx  fy  fz  pi  tir tic ',ich(1:4)).eq.0)go to 248          !/*nfx        38*/
      ntl(ital)=icn                                                             !/*nfx        39*/
      do 244 i=1,mipt                                                           !/*nfx        40*/
  244 ktt(ital,i)=nqp(i)                                                        !/*nfx        41*/
  248 if((ich.eq.'f'.or.ich.eq.'e').and.mod(icn,10).eq.8)                       !/*nfx        42*/
     & kf8=max(kf8,3-icx)                                                       !/*nf4a       26*/
      if(ich.eq.'pd')ltd=ltd+mxa                                                !/*nf        123*/
      if(ich.eq.'fc')lit=lit+1                                                  !/*nf        124*/
      if(ich.eq.'cf'.and.kfl.lt.2)kfl=kfl+2                                     !/*nf        125*/
      if(ich.eq.'sf'.and.abs(kfl-1).eq.1)kfl=kfl+1                              !/*nf        126*/
#ifdef RADIOG                                                                   /*!nfx        43*/
      if(ich.eq.'fs'.and.mod(icn,10).ne.5)mxafs=mxafs+1                         !/*nfx        44*/
#else                                                                           /*!nfx        45*/
      if(ich.eq.'fs')mxafs=mxafs+1                                              !/*nf        127*/
#endif                                                                          /*!nfx        46*/
      return                                                                    !/*nf        128*/
!                                                                               !/*nf        129*/
! >>>>>  material specifications                                       m        !/*nf        130*/
  250 write(iu3)-icn                                                            !/*nf        131*/
      nmat1=nmat1+1                                                             !/*nf        132*/
      return                                                                    !/*nf        133*/
!                                                                               !/*nf        134*/
! >>>>>  thermal temperatures                                        tmp        !/*nf        135*/
  260 mxt=max(mxt,icn)                                                          !/*nf        136*/
      return                                                                    !/*nf        137*/
!                                                                               !/*nf4b        7*/
! >>>>>  print control                                             print        !/*nf4b        8*/
  268 ink(128)=1                                                                !/*nf4b        9*/
      return                                                                    !/*nf4b       10*/
!                                                                               !/*nf        138*/
! >>>>>  kcode, acode specifications                        kcode, acode        !/*nf4c        4*/
  270 if(nsr.eq.40)ksdef=1                                                      !/*nf        140*/
      nsr=71                                                                    !/*nf        141*/
      mrkp=nmrkp                                                                !/*nf4c        5*/
      return                                                                    !/*nf        142*/
!                                                                               !/*nf        143*/
! >>>>>  weight-window generator parameters                          wwg        !/*nf        144*/
  280 do 290 i=1,mipt                                                           !/*nf        145*/
  290 ngww(i)=max(ngww(i),1)                                                    !/*nf        146*/
      return                                                                    !/*nf        148*/
!                                                                               !/*nf        149*/
! >>>>>  surface source write information                            ssw        !/*nf        150*/
  300 issw=1                                                                    !/*nf        151*/
      return                                                                    !/*nf        152*/
!                                                                               !/*nf        153*/
! >>>>>  surface source read information                             ssr        !/*nf        154*/
  310 if(nsr.eq.0)nsr=6                                                         !/*nf        155*/
      nvs(2)=0                                                                  !/*nf        156*/
      nvs(3)=0                                                                  !/*nf        157*/
      nvs(10)=3                                                                 !/*nf        158*/
      return                                                                    !/*nf        159*/
!                                                                               !/*nf        160*/
! >>>>>  universe designators                                          u        !/*nf4b       11*/
  316 do 318 i=1,mxa                                                            !/*nf4b       12*/
  318 jun(ljun+i)=0                                                             !/*nf4b       13*/
!                                                                               !/*nf4b       14*/
! >>>>>  repeated structures / lattice information            u,trcl,lat        !/*nf4b       15*/
  320 junf=1                                                                    !/*nf4b       16*/
      return                                                                    !/*nf4b       17*/
!                                                                               !/*nf4b       18*/
! >>>>>  cell-filling universes, with transformations               fill        !/*nf4b       19*/
  322 junf=1                                                                    !/*nf4b       20*/
      nmzu=max(nmzu,mxa)                                                        !/*nf4b       21*/
      do 324 i=1,mxa                                                            !/*nf4b       22*/
      do 324 j=1,3                                                              !/*nf4b       23*/
      mazp(lmzp+j,i)=0                                                          !/*nf4b       24*/
  324 mfl(lmfl+j,i)=0                                                           !/*nf4b       25*/
      return                                                                    !/*nf        163*/
!                                                                               !/*nf        164*/
! >>>>>  bremsstrahlung biasing                                    bbrem        !/*nf        165*/
  330 bbrem(1)=1.                                                               !/*nf        166*/
      return                                                                    !/*nf        167*/
!                                                                               !/*nf4b       28*/
! >>>>>  perturbation input                                         pert        !/*nf4b       29*/
  340 npert=npert+1                                                             !/*nf4b       30*/
      return                                                                    !/*nf4b       31*/
!                                                                               !/*nfx        49*/
! >>>>>  Mesh tally data card.                                     tmesh        !/*nfx        50*/
  400 continue                                                                  !/*nfx        51*/
#ifdef MESHTAL                                                                  /*!nfx        52*/
      call rgdtal(1)                                                            !/*nfx        53*/
#endif                                                                          /*!nfx        54*/
      return                                                                    !/*nfx        55*/
!                                                                               !/*nf        168*/
! >>>>>  first spare card type                                        za        !/*nf        169*/
 9910 continue                                                                  !/*nf        170*/
      return                                                                    !/*nf        171*/
!                                                                               !/*nf        172*/
! >>>>>  second spare card type                                       zb        !/*nf        173*/
 9920 continue                                                                  !/*nf        174*/
      return                                                                    !/*nf        175*/
!                                                                               !/*nf        176*/
! >>>>>  third spare card type                                        zc        !/*nf        177*/
 9930 continue                                                                  !/*nf        178*/
      return                                                                    !/*nf        179*/
      end                                                                       !/*nf        180*/

!#ident pass1.F from deck pa                                                    !/*pa          1*/
      subroutine pass1                                                          !/*pa          2*/
!        read the inp file the first time, in order to get the                  !/*pa          3*/
!        dimensions for dynamically allocated storage.                          !/*pa          4*/
      use allocate_memory                                                       !/*pax         1*/
      use global_data                                                           !/*pax         2*/
      use imcn_data                                                             !/*pax         3*/
!                                                                               !/*pax         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pax         5*/
      implicit integer(kindi) (i-n)                                             !/*pax         6*/
!                                                                               !/*pax         7*/
!                                                                               !/*pa          7*/
!        process 3 data blocks, which are separated by blank lines.             !/*pa          8*/
      rewind iu1                                                                !/*pa          9*/
      do 50 ib=1,3                                                              !/*pa         10*/
      ics=0                                                                     !/*pa         12*/
      irc=0                                                                     !/*pa4a        1*/
      if(ib.eq.1)then                                                           !/*pax         8*/
        ncl => chg_mem (ncl,100_kindi,is)                                       !/*pax         9*/
        if(is.ne.0)call expire(0,'pass1','chg_mem error')                       !/*pax        10*/
        ncl=0                                                                   !/*pax        11*/
      endif                                                                     !/*pax        12*/
      if(ib.eq.2)then                                                           !/*pax        13*/
        nsfm => chg_mem (nsfm,100_kindi,is)                                     !/*pax        14*/
        if(is.ne.0)call expire(0,'pass1','chg_mem error')                       !/*pax        15*/
        nsfm=0                                                                  !/*pax        16*/
      endif                                                                     !/*pax        17*/
      if(ib.ne.3)go to 10                                                       !/*pa4a        2*/
      allocate (jun(mxa), stat=is)                                              !/*pax        18*/
      if(is.ne.0)call expire(0,'pass1','error getting memory')                  !/*pax        19*/
      jun=0                                                                     !/*pax        20*/
      kxs => chg_mem (kxs,100_kindi,is)                                         !/*pax        21*/
      if(is.ne.0)call expire(0,'pass1','chg_mem error')                         !/*pax        22*/
      kxs=0                                                                     !/*pax        23*/
      if(dbcn(6).eq.0.)then                                                     !/*pax        24*/
      laf => chg_mem (laf,3_kindi,100_kindi,is)                                 !/*pax        25*/
      else                                                                      !/*pax        26*/
      laf => chg_mem (laf,303_kindi,100_kindi,is)                               !/*pax        27*/
      endif                                                                     !/*pax        28*/
      if(is.ne.0)call expire(0,'pass1','chg_mem error')                         !/*pax        29*/
      laf=0                                                                     !/*pax        30*/
      llaf=3                                                                    !/*pax        31*/
      allocate (mat(mxa), stat=is)                                              !/*pax        32*/
      if(is.ne.0)call expire(0,'pass1','error getting memory')                  !/*pax        33*/
      mat=0                                                                     !/*pax        34*/
      allocate (mazp(3,mxa), stat=is)                                           !/*pax        35*/
      if(is.ne.0)call expire(0,'pass1','error getting memory')                  !/*pax        36*/
      mazp=0                                                                    !/*pax        37*/
      allocate (mfl(3,mxa), stat=is)                                            !/*pax        38*/
      if(is.ne.0)call expire(0,'pass1','error getting memory')                  !/*pax        39*/
      mfl=0                                                                     !/*pax        40*/
      if(ncparf.ne.0)call cprinp                                                !/*pa4b        7*/
      endfile iu3                                                               !/*pa4a        6*/
      rewind iu3                                                                !/*pa4a        7*/
      do 5 i=1,mxa                                                              !/*pa4a        8*/
    5 read(iu3)mat(lmat+i)                                                      !/*pa4a        9*/
      go to 20                                                                  !/*pa4a       10*/
!                                                                               !/*pa         16*/
!        read and process lines of data until a blank or eof is found.          !/*pa         17*/
   10 jui=iui                                                                   !/*pa         18*/
   20 kl=0                                                                      !/*pa         19*/
      read(jui,'(a80)',end=30)hlin                                              !/*pa         20*/
      if(hlin.eq.' ')kl=1                                                       !/*pa4b        8*/
      if(kl.eq.1)go to 35                                                       !/*pa4b        9*/
      call ckchar(hlin,jtty,iuou)                                               !/*pa         21*/
      call nxtsym(hlin,' ',1,it,iu,1)                                           !/*pa4a       11*/
      kl=4                                                                      !/*pa         22*/
!                                                                               !/*pa         23*/
!        quit on premature eof.                                                 !/*pa         24*/
   30 if(kl.eq.0.and.jui.ne.iui)go to 10                                        !/*pa         25*/
      if(kl.eq.0.and.ib.lt.3)call expire(0,'pass1',                             !/*pa         26*/
     & 'unexpected eof in file '//inp)                                          !/*pa         27*/
      if(kl.eq.0)hlin='     eof'                                                !/*pa         28*/
!                                                                               !/*pa         29*/
!        determine the kind of line.                                            !/*pa         30*/
!        kl:  0=eof  1=blank  2=comment  3=new card  4=continuation             !/*pa         31*/
      if(hlin(1:5).ne.' '.and.irc.ge.0.and.kl.eq.4)kl=3                         !/*pa4a       12*/
      if(hlin(it:iu).eq.'c'.and.hlin(1:5).ne.' ')kl=2                           !/*pa4a       13*/
   35 if(likef.ne.0.and.nwc.gt.1.and.kl.eq.1)kl=3                               !/*pa4b       10*/
!                                                                               !/*pa         36*/
!        complete the processing of the previous card, if any.                  !/*pa         37*/
      if((kl.le.1.or.kl.eq.3).and.ics.eq.1)call oldcd1                          !/*pa         38*/
      if(kl.eq.0)go to 50                                                       !/*pa         39*/
!                                                                               !/*pa         40*/
!        promote the new line to be the current line.                           !/*pa         41*/
      klin=hlin                                                                 !/*pa         42*/
      if(ib.eq.1.and.kl.ge.3.and.likef.eq.0)call celpar(1)                      !/*pa         43*/
      if(kl.ne.2)irc=0                                                          !/*pa4a       14*/
!                                                                               !/*pa         44*/
!        do the initial processing of a new card.                               !/*pa         45*/
      if(kl.ne.3)go to 40                                                       !/*pa         46*/
      ics=1                                                                     !/*pa         47*/
      if(hlin(it:iu).eq.'#')call colinp(0,ib)                                   !/*pa4a       15*/
      if(ics.eq.1)call newcd1(ib)                                               !/*pa         49*/
!                                                                               !/*pa         50*/
!        process the data items in the line.                                    !/*pa         51*/
   40 if(kl.ge.3.and.ics.eq.1)call items(1)                                     !/*pa         52*/
      if(kl.gt.1)go to 20                                                       !/*pa         54*/
   50 continue                                                                  !/*pa         55*/
      if(dbcn(6).ne.0.)ink(128)=1                                               !/*pax        41*/
      if(nmzu.ne.0)call unimaz                                                  !/*pa4b       11*/
      return                                                                    !/*pa         56*/
      end                                                                       !/*pa         57*/

!#ident rdprob.F from deck rp                                                   !/*rp          1*/
      subroutine rdprob                                                         !/*rp          2*/
!        read in the problem or continue-run specs from the inp file.           !/*rp          3*/
      use global_data                                                           !/*rpx         1*/
      use imcn_data                                                             !/*rpx         2*/
!                                                                               !/*rpx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*rpx         4*/
      implicit integer(kindi) (i-n)                                             !/*rpx         5*/
!                                                                               !/*rpx         6*/
      character hl*80                                                           !/*rp4a        1*/
!                                                                               !/*rp          6*/
!        set up the variable entries in krq(6,i).                               !/*rp          7*/
      do 10 i=1,nkcd                                                            !/*rp          8*/
      if(krq(6,i).eq.-2)krq(6,i)=-mxj                                           !/*rp          9*/
   10 if(krq(6,i).eq.-1)krq(6,i)=-mxa                                           !/*rp         10*/
!                                                                               !/*rp         11*/
!        process 1 or 3 data blocks, which are separated by blank lines.        !/*rp         12*/
      rewind iu1                                                                !/*rp         13*/
      jui=iui                                                                   !/*rp         14*/
      do 80 ib=1+2*konrun,3                                                     !/*rp         15*/
      if(ib.eq.3.and.ncparf.ne.0)call cprinp                                    !/*rp         16*/
      ics=0                                                                     !/*rp         17*/
      irc=0                                                                     !/*rp4a        2*/
      do 20 i=1,ndup(ib)                                                        !/*rp         18*/
      do 20 mp=1,mipt+2                                                         !/*rpx         7*/
   20 kdup(ldup+mp,i)=0                                                         !/*rpx         8*/
      if(ib.ne.1)go to 40                                                       !/*rp         20*/
      msd=0                                                                     !/*rp         21*/
      mssc=ljsc                                                                 !/*rp         22*/
      mxxs=0                                                                    !/*rp         23*/
      mxj=0                                                                     !/*rp         24*/
      mxa=0                                                                     !/*rp         25*/
      mxtr=0                                                                    !/*rp         26*/
      nlat=0                                                                    !/*rp         27*/
      mlaf=0                                                                    !/*rp         28*/
      mkcp=0                                                                    !/*rp         29*/
      npikmt=0                                                                  !/*rp         30*/
      mix=0                                                                     !/*rp         31*/
      indt=0                                                                    !/*rp         32*/
      ipert=0                                                                   !/*rp4b        1*/
      npert=0                                                                   !/*rp4b        2*/
#ifdef MESHTAL                                                                  /*!rpx         9*/
      nmesh=0                                                                   !/*rpx        10*/
#endif                                                                          /*!rpx        11*/
#ifdef SPABI                                                                    /*!rpx        12*/
      call zero_spb_length                                                      !/*rpx        13*/
#endif                                                                          /*!rpx        14*/
!                                                                               !/*rp         33*/
!        read and store lines of data until a blank or eof is found.            !/*rp         34*/
   30 jui=iui                                                                   !/*rp         35*/
   40 kl=0                                                                      !/*rp         36*/
      read(jui,'(a80)',end=50)hlin                                              !/*rp         37*/
      hl=hlin                                                                   !/*rp4a        3*/
      if(hlin.eq.' ')kl=1                                                       !/*rp4b        3*/
      if(kl.eq.1)go to 55                                                       !/*rp4b        4*/
      call nxtsym(hlin,' ',1,it,iu,1)                                           !/*rp4a        4*/
      kl=4                                                                      !/*rp         38*/
   50 if(kl.eq.0.and.jui.ne.iui)go to 30                                        !/*rp         39*/
      if(kl.eq.0)hlin='     eof'                                                !/*rp         40*/
!                                                                               !/*rp         41*/
!        determine the kind of line.                                            !/*rp         42*/
!        kl:  0=eof  1=blank  2=comment  3=new card  4=continuation             !/*rp         43*/
      if(hlin(1:5).ne.' '.and.irc.ge.0.and.kl.eq.4)kl=3                         !/*rp4a        5*/
      if(hlin(it:iu).eq.'c'.and.hlin(1:5).ne.' ')kl=2                           !/*rp4a        6*/
   55 if(likef.ne.0.and.nwc.gt.1.and.kl.eq.1)kl=3                               !/*rp4b        5*/
!                                                                               !/*rp         48*/
!        complete the processing of the previous card, if any.                  !/*rp         49*/
      if((kl.le.1.or.kl.eq.3).and.ics.eq.1)call oldcrd                          !/*rp         50*/
      if(ics.eq.-1.and.ica.ne.0)then                                            !/*rpx        15*/
         if(cnm(ica).eq.'mesh ')iwwg=-1                                         !/*rpx        16*/
      endif                                                                     !/*rpx        17*/
      if(kl.eq.0)go to 80                                                       !/*rp         51*/
!                                                                               !/*rp         52*/
!        promote the new line and print it.                                     !/*rp         53*/
      if(kl.ne.2)klin=hlin                                                      !/*rp         54*/
      if(jui.ne.iui)go to 60                                                    !/*rp         55*/
      iln1=abs(iln1)                                                            !/*rp4a        7*/
      iln=iln+1                                                                 !/*rp         56*/
      write(iuo,'(i5,1h-,7x,a80)')iln,hl                                        !/*rp4a        8*/
   60 if(ib.eq.1.and.kl.ge.3.and.likef.eq.0)call celpar(2)                      !/*rp         58*/
      if(kl.ne.2)irc=0                                                          !/*rp4a        9*/
!                                                                               !/*rp         59*/
!        set up and check a new card.                                           !/*rp         60*/
      if(kl.ne.3)go to 70                                                       !/*rp         61*/
      ics=1                                                                     !/*rp         62*/
      if(hlin(it:iu).eq.'#')call colinp(1,ib)                                   !/*rp4a       10*/
      if(jui.ne.iui)iln1=-abs(iln)                                              !/*rp4a       11*/
      if(ics.eq.1)call newcrd(ib)                                               !/*rp         65*/
!                                                                               !/*rp         66*/
!        check and store the data items in the line.                            !/*rp         67*/
   70 if(kl.eq.4.and.jui.ne.iui)iln1=-abs(iln)                                  !/*rp4a       12*/
      if(kl.eq.4.and.ics.eq.0)call erprnt(2,1,0,0,0,0,0,1,                      !/*rp4a       13*/
     & '48hcontinuation card not preceded by any main card.')                   !/*rp         69*/
      if(kl.ge.3.and.ics.eq.1)call items(2)                                     !/*rp         70*/
      if(kl.gt.1)go to 40                                                       !/*rp         72*/
   80 continue                                                                  !/*rp         73*/
      return                                                                    !/*rp         74*/
      end                                                                       !/*rp         75*/

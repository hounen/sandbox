!#ident items.F from deck ik                                                    !/*ik          1*/
      subroutine items(mm)                                                      !/*ik          2*/
!        process input data items.  mm=1 from pass1, mm=2 from rdprob.          !/*ik          3*/
      use global_data                                                           !/*ikx         1*/
      use imcn_data                                                             !/*ikx         2*/
!                                                                               !/*ikx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ikx         4*/
      implicit integer(kindi) (i-n)                                             !/*ikx         5*/
!                                                                               !/*ikx         6*/
      character hh*25,hs*16                                                     !/*ik          6*/
!                                                                               !/*ik          7*/
!        special treatment for pure text data cards.                            !/*ik          8*/
      if(ich.ne.'fc'.and.ich.ne.'sc'.and.ich.ne.'mplot')go to 10                !/*ik          9*/
      if(mm.eq.1)call nxtit1                                                    !/*ik         10*/
      if(mm.eq.2)call nextit                                                    !/*ik         11*/
      return                                                                    !/*ik         12*/
!                                                                               !/*ik         13*/
!        read the next item and determine its type.                             !/*ik         14*/
!        ki:  0=character  1=real  2=integer                                    !/*ik         15*/
   10 irc=max(1,irc)                                                            !/*ik4a        1*/
   20 call nxtsym(klin,' ():#,=<[]$&',irc,it,iu,1)                              !/*ik4a        2*/
      irc=iu+1                                                                  !/*ik4a        3*/
      if(iu.eq.0)return                                                         !/*ik         19*/
      if(klin(it:it).eq.'$')return                                              !/*ik         20*/
      if(klin(iu:iu).eq.'&')irc=-1                                              !/*ik4a        4*/
      if(irc.eq.-1)return                                                       !/*ik4a        5*/
      ki=kdata(klin(it:iu))                                                     !/*ik         21*/
      ii=0                                                                      !/*ik         22*/
      ri=0.                                                                     !/*ik         23*/
      if(ki.eq.0)go to 40                                                       !/*ik         24*/
      hh=' '                                                                    !/*ik         25*/
      hh(25-iu+it:25)=klin(it:iu)                                               !/*ik         26*/
      if(iu-it.gt.8)ki=1                                                        !/*ik         27*/
      if(ki.ne.2)go to 30                                                       !/*ik         28*/
      read(hh,'(i25)')ii                                                        !/*ik         29*/
      ri=ii                                                                     !/*ik         30*/
      go to 40                                                                  !/*ik         31*/
   30 read(hh,'(e25.0)')ri                                                      !/*ik         32*/
      if(abs(ri).le.2147483647.)ii=nint(ri)                                     !/*ik         33*/
      if(ri.eq.0.)ki=2                                                          !/*ik         34*/
      if(ii.eq.0)go to 40                                                       !/*ik         35*/
      if(abs(ri-anint(ri)).gt.5e-14*abs(ri))go to 40                            !/*ik         36*/
      ri=ii                                                                     !/*ik         37*/
      ki=2                                                                      !/*ik         38*/
!                                                                               !/*ik         39*/
!        store nii interpolated values before the current item.                 !/*ik         40*/
   40 if(nii.le.0)go to 60                                                      !/*ik         41*/
      if(ki.eq.0)go to 150                                                      !/*ik         42*/
      if(hs.eq.'log')go to 62                                                   !/*ikxc        1*/
      dl=(ri-ritm)/(nii+1)                                                      !/*ik         43*/
      il=(ii-iitm)/(nii+1)                                                      !/*ik         44*/
      kitm=min(kitm,ki)                                                         !/*ik         45*/
      if(il*(nii+1).ne.ii-iitm)kitm=1                                           !/*ik         46*/
      do 50 mi=1,nii                                                            !/*ik         47*/
      ritm=ritm+dl                                                              !/*ik         48*/
      iitm=iitm+il                                                              !/*ik         49*/
      if(kitm.eq.2)ritm=iitm                                                    !/*ik         50*/
      if(mm.eq.1)call nxtit1                                                    !/*ik         51*/
      if(mm.eq.2)call nextit                                                    !/*ik         52*/
   50 if(ics.lt.0)return                                                        !/*ik         53*/
      go to 130                                                                 !/*ik         54*/
!                                                                               !/*ikxc        2*/
!        look for log interpolation.                                            !/*ikxc        3*/
   60 if(ki.ne.0)go to 130                                                      !/*ikxc        4*/
      if(iu-it.le.2)go to 65                                                    !/*ikxc        5*/
      if(klin(iu-2:iu).ne.'log')go to 65                                        !/*ikxk        1*/
      if(nwc.eq.0.or.nii.ne.0.or.kitm.eq.0)go to 150                            !/*ikxc        8*/
      hs='log'                                                                  !/*ikxc        9*/
      nii=-1                                                                    !/*ikxk        2*/
      k=kdata(klin(it:iu-3))                                                    !/*ikxc       12*/
      if(k.ne.2.or.it-iu.gt.9)go to 150                                         !/*ikxk        3*/
      hh=' '                                                                    !/*ikxc       14*/
      hh(28-iu+it:25)=klin(it:iu-3)                                             !/*ikxc       15*/
      read(hh,'(i25)')nii                                                       !/*ikxc       16*/
      go to 20                                                                  !/*ikxc       17*/
!                                                                               !/*ikxc       18*/
!        store nii log interpolated values.                                     !/*ikxc       19*/
   62 if(ri.le.zero.or.ritm.le.zero)go to 150                                   !/*ikxk        4*/
      dl=exp(log(ri/ritm)/(nii+1))                                              !/*ikxk        5*/
      do 64 mi=1,nii                                                            !/*ikxc       21*/
      ritm=ritm*dl                                                              !/*ikxc       22*/
      kitm=1                                                                    !/*ikxc       23*/
      if(mm.eq.1)call nxtit1                                                    !/*ikxc       24*/
      if(mm.eq.2)call nextit                                                    !/*ikxc       25*/
   64 if(ics.lt.0)return                                                        !/*ikxc       26*/
      go to 130                                                                 !/*ikxc       27*/
!                                                                               !/*ik         55*/
!        look for a special r, i, m, or j item.                                 !/*ik         56*/
   65 ks=index('rimj',klin(iu:iu))                                              !/*ikxc       28*/
      if(ks.eq.0)go to 130                                                      !/*ik         59*/
      if(ich.eq.'fq'.or.ich.eq.'m'.or.ich.eq.'xs')go to 130                     !/*ik4b        1*/
      n=1                                                                       !/*ik         61*/
      if(iu.eq.it)go to 70                                                      !/*ik         62*/
      k=kdata(klin(it:iu-1))                                                    !/*ik         63*/
      if(k.eq.0.or.k.eq.1.and.ks.ne.3.or.k.eq.2.and.it-iu.gt.7)go to 130        !/*ik         64*/
      hh=' '                                                                    !/*ik         65*/
      hh(26-iu+it:25)=klin(it:iu-1)                                             !/*ik         66*/
      if(k.ge.1)read(hh,'(e25.0)')h                                             !/*ik         67*/
      if(k.eq.2)read(hh,'(i25)')n                                               !/*ik         68*/
   70 go to(80,100,110,120)ks                                                   !/*ik         69*/
!                                                                               !/*ik         70*/
! >>>>>  r:  repeat previous item n times.                                      !/*ik         71*/
   80 if(nwc.eq.0.or.nii.ne.0)go to 150                                         !/*ik         72*/
      do 90 i=1,n                                                               !/*ik         73*/
      if(mm.eq.1)call nxtit1                                                    !/*ik         74*/
      if(mm.eq.2)call nextit                                                    !/*ik         75*/
   90 if(ics.lt.0)return                                                        !/*ik         76*/
      go to 20                                                                  !/*ik         77*/
!                                                                               !/*ik         78*/
! >>>>>  i:  set up to store n interpolated values next time.                   !/*ik         79*/
  100 if(nwc.eq.0.or.nii.ne.0.or.kitm.eq.0)go to 150                            !/*ik         80*/
      hs=klin(it:iu)                                                            !/*ik         81*/
      nii=n                                                                     !/*ik         82*/
      go to 20                                                                  !/*ik         83*/
!                                                                               !/*ik         84*/
! >>>>>  m:  store an item h times the value of the previous item.              !/*ik         85*/
  110 if(nwc.eq.0.or.nii.ne.0.or.kitm.eq.0.or.iu.eq.it)go to 150                !/*ik         86*/
      ritm=h*ritm                                                               !/*ik         87*/
      iitm=n*iitm                                                               !/*ik         88*/
      go to 140                                                                 !/*ik         89*/
!                                                                               !/*ik         90*/
! >>>>>  j:  skip n storage spaces.                                             !/*ik         91*/
  120 if(krq(4,ica).eq.0)go to 150                                              !/*ik         92*/
      nwc=nwc+n                                                                 !/*ik         93*/
      nii=-1                                                                    !/*ik         94*/
      go to 20                                                                  !/*ik         95*/
!                                                                               !/*ik         96*/
!        set up the current item and store it.                                  !/*ik         97*/
  130 kitm=ki                                                                   !/*ik         98*/
      hitm=klin(it:iu)                                                          !/*ik         99*/
      nitm=iu-it+1                                                              !/*ik        100*/
      ritm=ri                                                                   !/*ik        101*/
      iitm=ii                                                                   !/*ik        102*/
      nii=0                                                                     !/*ik        103*/
  140 if(mm.eq.1)call nxtit1                                                    !/*ik        104*/
      if(mm.eq.2)call nextit                                                    !/*ik        105*/
      if(ics.lt.0)return                                                        !/*ik        106*/
      go to 20                                                                  !/*ik        107*/
!                                                                               !/*ik        108*/
!        return with ics=-1 if incorrect use of special syntax is found.        !/*ik        109*/
  150 ics=-1                                                                    !/*ik        110*/
      if(mm.eq.1)return                                                         !/*ik        111*/
      if(nii.le.0)hs=klin(it:iu)                                                !/*ik        112*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ik        113*/
     & '55hspecial syntax item used incorrectly:  '//hs)                        !/*ik        114*/
      return                                                                    !/*ik        115*/
      end                                                                       !/*ik        116*/

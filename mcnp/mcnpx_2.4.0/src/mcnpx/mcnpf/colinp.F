!#ident colinp.F from deck co                                                   !/*co          1*/
      subroutine colinp(ns,je)                                                  !/*co          2*/
!        convert a block of column input data into horizontal lines.            !/*co          3*/
!        generate errors and warnings only if ns is not zero.                   !/*co          4*/
!        je=1,2,3 for cell, surface, and general-data inp blocks.               !/*co          5*/
      use global_data                                                           !/*cox         1*/
      use imcn_data                                                             !/*cox         2*/
!                                                                               !/*cox         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*cox         4*/
      implicit integer(kindi) (i-n)                                             !/*cox         5*/
!                                                                               !/*cox         6*/
      character hi(30)*20,ht*5,hs*5,hk(100)*80,hn*80,hg*80                      !/*co          8*/
      dimension kk(100)                                                         !/*co          9*/
!                                                                               !/*co         10*/
      if(je.ne.3.and.ns.ne.0)call erprnt(2,1,0,0,0,0,0,1,                       !/*co         11*/
     & '46h# block is in the wrong place in the inp file.')                     !/*co         12*/
      if(je.ne.3)return                                                         !/*co         13*/
!                                                                               !/*co         14*/
!        get the card names from the # line.                                    !/*co         15*/
      nc=0                                                                      !/*co         16*/
      call nxtsym(klin,' ',1,it,iu,1)                                           !/*co4a        1*/
   10 call nxtsym(klin,' $&',iu+1,it,iu,1)                                      !/*co4a        2*/
      if(iu.eq.0)go to 20                                                       !/*co         19*/
      if(klin(it:it).eq.'$'.or.klin(it:it).eq.'&')go to 20                      !/*co4a        3*/
      nc=nc+1                                                                   !/*co         21*/
      hi(nc)=klin(it:iu)                                                        !/*co         22*/
      go to 10                                                                  !/*co         23*/
   20 if(nc.eq.0.and.ns.ne.0)call erprnt(2,1,0,0,0,0,0,0,                       !/*co         24*/
     & '24hno card names on # card.')                                           !/*co         25*/
      if(nc.eq.0)ics=-1                                                         !/*co         26*/
!                                                                               !/*co         27*/
!        identify and check the type of the card names.                         !/*co         28*/
      m=0                                                                       !/*co         29*/
      kt=0                                                                      !/*co         30*/
      do 80 j=1,nc                                                              !/*co         31*/
      hs=' '                                                                    !/*co         32*/
      n=0                                                                       !/*co         33*/
      do 30 i=1,5                                                               !/*co         34*/
      ht=hi(j)(i:i)                                                             !/*co         35*/
      if(ht.eq.':')go to 40                                                     !/*co         36*/
      if(index('abcdefghijklmnopqrstuvwxyz',ht(1:1)).eq.0)go to 30              !/*co         37*/
      n=n+1                                                                     !/*co         38*/
      hs(n:n)=ht(1:1)                                                           !/*co         39*/
   30 continue                                                                  !/*co         40*/
      if(hi(j)(6:6).ne.':')go to 50                                             !/*co         41*/
   40 i=i+1                                                                     !/*co         42*/
      if(hi(j)(i:i).eq.' ')go to 50                                             !/*co         43*/
      if(index(hip//',',hi(j)(i:i)).eq.0)call erprnt(2,1,0,0,0,0,0,0,           !/*co         44*/
     & '35hincorrect particle-type designator.')                                !/*co         45*/
      go to 40                                                                  !/*co         46*/
   50 do 60 i=1,nkcd                                                            !/*co         47*/
   60 if(cnm(i).eq.hs)go to 70                                                  !/*co         48*/
      if(ns.ne.0)call erprnt(2,1,0,0,0,0,0,0,                                   !/*co         49*/
     & '31h'//hs//' is not a valid card name.')                                 !/*co         50*/
      i=1                                                                       !/*co         51*/
   70 if(j.eq.1)kt=krq(7,i)                                                     !/*co         52*/
      if(krq(7,i).ne.kt.and.ns.ne.0.and.m.eq.0)call erprnt(2,1,                 !/*co         53*/
     & 0,0,0,0,0,0,'41hthe card names are not all the same type.')              !/*co         54*/
   80 if(krq(7,i).ne.kt)m=1                                                     !/*co         55*/
!                                                                               !/*co4a        4*/
!        set kt=0 if 1st entry not counter, that is, if the number of           !/*co4a        5*/
!        columns of data = number of cards (nc)                                 !/*co4a        6*/
      if(kt.eq.0)go to 84                                                       !/*co4a        7*/
      read(iui,'(a80)',end=180)klin                                             !/*co4a        8*/
      n=0                                                                       !/*co4a        9*/
      iu=0                                                                      !/*co4a       10*/
   82 call nxtsym(klin,' ',iu+1,it,iu,1)                                        !/*co4a       11*/
      n=n+1                                                                     !/*co4a       12*/
      if(iu.eq.0)go to 83                                                       !/*co4b        1*/
      if(klin(it:it).ne.'$'.and.klin(it:it).ne.'&')go to 82                     !/*co4b        2*/
   83 if(n-1.ne.nc+1)kt=0                                                       !/*co4b        3*/
      backspace iui                                                             !/*co4a       15*/
!                                                                               !/*co         56*/
!        copy the body of the # block from iui to iu1.                          !/*co         57*/
   84 ks=0                                                                      !/*co4a       16*/
      kp=0                                                                      !/*co         59*/
      nr=0                                                                      !/*co         60*/
      k=0                                                                       !/*co         61*/
      rewind iu1                                                                !/*co         62*/
   90 read(iui,'(a80)',end=180)klin                                             !/*co         63*/
      call nxtsym(klin,' ',1,it,iu,1)                                           !/*co4a       17*/
      if(klin.eq.' ')go to 180                                                  !/*co         64*/
      if(klin(it:iu).eq.'c'.and.klin(1:5).ne.' ')go to 110                      !/*co4a       18*/
      if(klin(1:5).eq.' ')go to 110                                             !/*co4a       19*/
      if(kdata(klin(it:iu)).ne.0)go to 110                                      !/*co4a       20*/
      if(it.eq.iu)go to 100                                                     !/*co4a       21*/
      if(kdata(klin(it:iu-1)).ne.2)go to 180                                    !/*co4a       22*/
  100 if(index('rimj',klin(iu:iu)).eq.0)go to 180                               !/*co4a       23*/
  110 if(ns.eq.0)go to 120                                                      !/*co         68*/
      iln=iln+1                                                                 !/*co         69*/
      write(iuo,'(i5,1h-,7x,a80)')iln,klin                                      !/*co         70*/
  120 if(klin(it:iu).eq.'c'.and.klin(1:5).ne.' ')go to 90                       !/*co4a       24*/
      nr=nr+1                                                                   !/*co         72*/
      if(kt.eq.0)go to 170                                                      !/*co         73*/
      ht=klin(it:min(it+4,iu))                                                  !/*co4a       25*/
      read(ht,'(bn,i5)')j                                                       !/*co4a       26*/
      klin(1:iu)=' '                                                            !/*co4a       27*/
      if(j.eq.0)go to 150                                                       !/*co         75*/
      if(kt.ne.1)k=namchg(kt,j)                                                 !/*co         76*/
      if(kt.ne.1)go to 140                                                      !/*co         77*/
      do 130 k=1,mxa                                                            !/*co         78*/
  130 if(mod(ncl(lncl+k),100000).eq.j)go to 140                                 !/*co         79*/
      k=0                                                                       !/*co         80*/
  140 if(k.eq.0.and.ns.ne.0)call erprnt(2,1,1,j,0,0,0,0,                        !/*co         81*/
     & 'i5,31h is not the name of any '//hcs(kt))                               !/*co         82*/
      go to 160                                                                 !/*co         83*/
  150 k=nr                                                                      !/*co         84*/
  160 if(k.lt.kp)ks=1                                                           !/*co         85*/
      kp=k                                                                      !/*co         86*/
  170 write(iu1,'(i5,a80)')k,klin                                               !/*co         87*/
      go to 90                                                                  !/*co         88*/
  180 backspace iui                                                             !/*co         89*/
!                                                                               !/*co         90*/
!        sort the # block if necessary.                                         !/*co         91*/
      ia=iu1                                                                    !/*co         92*/
      ib=iu2                                                                    !/*co         93*/
      if(ks.eq.0)go to 250                                                      !/*co         94*/
      nl=min(nr,100)                                                            !/*co         95*/
  190 nw=0                                                                      !/*co         96*/
      rewind ia                                                                 !/*co         97*/
      rewind ib                                                                 !/*co         98*/
      ks=0                                                                      !/*co         99*/
      kp=0                                                                      !/*co        100*/
      do 200 i=1,nl                                                             !/*co        101*/
  200 read(ia,'(i5,a80)')kk(i),hk(i)                                            !/*co        102*/
      nf=nl                                                                     !/*co        103*/
  210 iw=1                                                                      !/*co        104*/
      do 220 i=2,nl                                                             !/*co        105*/
  220 if(kk(i).lt.kk(iw))iw=i                                                   !/*co        106*/
  230 if(kk(iw).lt.kp)ks=1                                                      !/*co        107*/
      kp=kk(iw)                                                                 !/*co        108*/
      write(ib,'(i5,a80)')kk(iw),hk(iw)                                         !/*co        109*/
      nw=nw+1                                                                   !/*co        110*/
      kk(iw)=123456                                                             !/*co        111*/
      if(nw.eq.nr)go to 240                                                     !/*co        112*/
      if(nf.eq.nr)go to 210                                                     !/*co        113*/
      read(ia,'(i5,a80)')kk(iw),hk(iw)                                          !/*co        114*/
      nf=nf+1                                                                   !/*co        115*/
      if(kk(iw).lt.kp)go to 230                                                 !/*co        116*/
      go to 210                                                                 !/*co        117*/
  240 ia=ib                                                                     !/*co        118*/
      ib=iu1+iu2-ia                                                             !/*co        119*/
      if(ks.ne.0)go to 190                                                      !/*co        120*/
!                                                                               !/*co        121*/
!        write horizontal cards on the auxiliary input file.                    !/*co        122*/
  250 rewind ib                                                                 !/*co        123*/
      m=0                                                                       !/*co        124*/
      kp=0                                                                      !/*co        125*/
      do 330 ic=1,nc                                                            !/*co        126*/
      rewind ia                                                                 !/*co        127*/
      hn=hi(ic)                                                                 !/*co        128*/
      ip=max(5,index(hn,' ')-1)                                                 !/*co        129*/
      nj=0                                                                      !/*co        130*/
      do 320 j=1,nr                                                             !/*co        131*/
      read(ia,'(i5,a80)')k,hg                                                   !/*co        132*/
      if(kt.eq.0.or.m.ne.0.or.ic.ne.1)go to 260                                 !/*co        133*/
      if(k.ne.kp+1.and.ns.ne.0)call erprnt(1,1,0,0,0,0,0,0,                     !/*co        134*/
     & '50hduplicate or absent '//hcs(kt)//' in # '//hi(1)(1:10)//              !/*co        135*/
     & ' block.')                                                               !/*co        136*/
      if(k.ne.kp+1)m=1                                                          !/*co        137*/
      kp=k                                                                      !/*co        138*/
  260 iu=0                                                                      !/*co4a       28*/
      do 270 l=1,ic                                                             !/*co        140*/
      call nxtsym(hg,' $&',iu+1,it,iu,1)                                        !/*co4a       29*/
      if(iu.eq.0)go to 330                                                      !/*co        142*/
  270 if(hg(it:it).eq.'$'.or.hg(it:it).eq.'&')go to 330                         !/*co4a       30*/
      if(hg(it:iu).ne.'j')go to 280                                             !/*co        144*/
      nj=nj+1                                                                   !/*co        145*/
      go to 320                                                                 !/*co        146*/
  280 if(nj.eq.0)go to 300                                                      !/*co        147*/
      write(ht,'(i4,1hj)')nj                                                    !/*co        148*/
      nj=0                                                                      !/*co        149*/
      if(ip+6.le.72)go to 290                                                   !/*co        150*/
      write(ib,'(a80)')hn                                                       !/*co        151*/
      ip=4                                                                      !/*co        152*/
      hn=' '                                                                    !/*co        153*/
  290 hn(ip+2:ip+6)=ht                                                          !/*co        154*/
      ip=ip+6                                                                   !/*co        155*/
  300 if(ip+2+iu-it.le.72)go to 310                                             !/*co        156*/
      hn(ip+2:ip+2)='&'                                                         !/*co4a       31*/
      write(ib,'(a80)')hn                                                       !/*co        157*/
      ip=4                                                                      !/*co        158*/
      hn=' '                                                                    !/*co        159*/
  310 hn(ip+2:ip+2+iu-it)=hg(it:iu)                                             !/*co        160*/
      ip=ip+2+iu-it                                                             !/*co        161*/
  320 continue                                                                  !/*co        162*/
  330 write(ib,'(a80)')hn                                                       !/*co        163*/
      endfile ib                                                                !/*co        164*/
!                                                                               !/*co        165*/
!        read the first horizontal line and return.                             !/*co        166*/
      rewind ib                                                                 !/*co        167*/
      if(nc.ne.0)read(ib,'(a80)')klin                                           !/*co        168*/
      jui=ib                                                                    !/*co        169*/
      return                                                                    !/*co        170*/
      end                                                                       !/*co        171*/

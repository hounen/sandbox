!#ident ixsdir.F from deck ix                                                   !/*ix          1*/
      subroutine ixsdir(jc)                                                     !/*ix          2*/
!        match the table names in xlist with xs cards and with entries          !/*ix          3*/
!        in the cross-section tables directory file xsdir.                      !/*ix          4*/
!        return with jc=1 if xsdir does not exist.                              !/*ix          5*/
      use global_data                                                           !/*ixx         1*/
      use imcn_data                                                             !/*ixx         2*/
!                                                                               !/*ixx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ixx         4*/
      implicit integer(kindi) (i-n)                                             !/*ixx         5*/
!                                                                               !/*ixx         6*/
      character ha*1,hb*30,hc*181,hd*10,hl*181,hn*13,hs*10,ht*10,hz*10          !/*ix4b        1*/
!                                                                               !/*ix          9*/
!        match the xs cards.                                                    !/*ix         10*/
      jc=0                                                                      !/*ix         11*/
      nx=0                                                                      !/*ix         12*/
      do 90 ie=1,nxsc                                                           !/*ix         13*/
      if(nty(lnty+ie).gt.0)go to 90                                             !/*ix4b        2*/
   10 if(nx.eq.nxsc)go to 92                                                    !/*ix4a        2*/
      hc=' '                                                                    !/*ix         14*/
      call zaid(2,hc,ixc(lixc+1,ie))                                            !/*ix4a        3*/
      do 20 j=11,181                                                            !/*ix         16*/
   20 hc(j:j)=char(mod(ixc(lixc+(j+1)/3,ie)/256**mod(j+1,3),256))               !/*ix         17*/
      hz=hc(1:10)                                                               !/*ix         18*/
      ha=hc(10:10)                                                              !/*ix         19*/
      nt=index(htn,ha)                                                          !/*ix4c2       1*/
      do 30 je=1,mxe                                                            !/*ix         21*/
      if(nty(lnty+je).ne.0)go to 30                                             !/*ix         22*/
      call zaid(2,ht,ixl(lixl+1,je))                                            !/*ix         23*/
      if(ht(1:7).ne.hz(1:7))go to 30                                            !/*ix4b        3*/
      if(ht(8:9).ne.hz(8:9).and.ht(8:9).ne.' ')go to 30                         !/*ix4b        4*/
      if(ht(10:10).eq.ha)go to 40                                               !/*ix         26*/
!        if a 'c' table matches a 'd' request, hold it in case of no 'd'        !/*ix4b        5*/
      if(nt.eq.1.and.ht(10:10).eq.'d')nty(lnty+je)=-1                           !/*ix4b        6*/
      if(nty(lnty+je).lt.0)go to 50                                             !/*ix4b        7*/
   30 continue                                                                  !/*ix         28*/
!        match not found.  xs card unused.                                      !/*ix4b        8*/
      if(ixc(lixc+61,ie).gt.0)call erprnt(1,2,1,ixc(lixc+61,ie),0,0,0,0,        !/*ix4b        9*/
     & '11htable on xs,i3,34h card is not used in this problem.')               !/*ix4b       10*/
      ixc(lixc+61,ie)=-abs(ixc(lixc+61,ie))                                     !/*ix4b       11*/
      go to 90                                                                  !/*ix4b       12*/
!        ixl entry je matches ixc entry ie.                                     !/*ix4b       13*/
   40 call zaid(1,hz,ixl(lixl+1,je))                                            !/*ix         30*/
      nty(lnty+je)=nt                                                           !/*ix         31*/
      nx=nx+1                                                                   !/*ix         32*/
!        load atomic weight ratio awc if not s(alpha,beta).                     !/*ix4b       14*/
      if(kdata(hc(1:6)).ne.2)go to 50                                           !/*ix4b       15*/
      read(hc,'(bn,i6)')iz                                                      !/*ix4a        4*/
      call nxtsym(hc,' ',11,it,iu,1)                                            !/*ix4a        5*/
      if(iu.eq.0)go to 50                                                       !/*ix4a        6*/
      hb=hc(it:iu)                                                              !/*ix4a        7*/
      read(hb,'(bn,e30.0)')a                                                    !/*ix4a        8*/
      do 48 im=1,mix                                                            !/*ix4a        9*/
   48 if(iza(liza+im).eq.iz)awc(lawc+im)=a                                      !/*ix4a       10*/
!        move ixc entries at ie to corresponding ixl entries at je.             !/*ix4b       16*/
   50 if(je.eq.ie)go to 90                                                      !/*ix         33*/
      do 60 i=1,61                                                              !/*ix         34*/
      j=ixc(lixc+i,je)                                                          !/*ix         35*/
      ixc(lixc+i,je)=ixc(lixc+i,ie)                                             !/*ix         36*/
   60 ixc(lixc+i,ie)=j                                                          !/*ix         37*/
!        if new ie (old je) entry is xs card, ensure it is processed.           !/*ix4b       17*/
      if(je.gt.ie.and.ixc(lixc+61,ie).gt.0)go to 10                             !/*ix4b       18*/
   90 continue                                                                  !/*ix         44*/
   92 if(mxe.gt.0.and.nx.eq.mxe)go to 490                                       !/*ixx         7*/
!                                                                               !/*ix         46*/
!        open the cross-section directory file.                                 !/*ix         47*/
      jc=1                                                                      !/*ix         48*/
      hl=xsdir                                                                  !/*cor4-2    122*/
      if(inqire(hl).eq.1)go to 95                                               !/*cor4-2    123*/
#ifdef PCDOS                                                                    /*!ix4a       14*/
      if(leng(hdpath).gt.0)hl=hdpath(1:leng(hdpath))//'\'//xsdir                !/*ix4a       15*/
#else                                                                           /*!ixx         8*/
      if(leng(hdpath).gt.0)hl=hdpath(1:leng(hdpath))//'/'//xsdir                !/*cor4-2    124*/
#endif                                                                          /*!cpp       121*/
      if(inqire(hl).eq.1)go to 95                                               !/*cor4-2    125*/
#ifdef PCDOS                                                                    /*!ix4a       19*/
      if(leng(hdpth).gt.0)hl=hdpth(1:leng(hdpth))//'\'//xsdir                   !/*ix4a       20*/
#else                                                                           /*!ixx         9*/
      if(leng(hdpth).gt.0)hl=hdpth(1:leng(hdpth))//'/'//xsdir                   !/*cor4-2    127*/
#endif                                                                          /*!cpp       124*/
      if(inqire(hl).eq.1)go to 95                                               !/*cor4-2    128*/
      write(jtty,'(22h searched directories:/a80/a80)')hdpath,hdpth             !/*ix4b       19*/
      call expire(0,'ixsdir',                                                   !/*ix4c2       2*/
     & 'cross-section directory file '//xsdir//' does not exist.')              !/*ix4c2       3*/
      return                                                                    !/*cor4-2    130*/
   95 jc=2                                                                      !/*cor4-2    131*/
      open(iud,file=hl,status='old')                                            !/*cor4-2    133*/
      rewind iud                                                                !/*ix         58*/
!                                                                               !/*ix4b       21*/
!        check for data beyond column 80.                                       !/*ix4b       22*/
   96 read(iud,'(a132)',err=98,end=98)hc                                        !/*ix4b       23*/
      if(hc(81:132).eq.' ')go to 96                                             !/*ix4b       24*/
      write(jtty,'(a70)')hc(1:70)                                               !/*ix4b       25*/
      write(iuo,'(a70)')hc(1:70)                                                !/*ix4b       26*/
      call erprnt(1,1,0,0,0,0,0,0,                                              !/*ix4b       27*/
     & '33hxsdir file data beyond column 80.')                                  !/*ix4b       28*/
   98 rewind iud                                                                !/*ix4b       29*/
!                                                                               !/*ix         59*/
!        read the atomic weights for converting densities.                      !/*ix         60*/
  100 read(iud,'(a80)',end=160)klin                                             !/*ix         61*/
      hn=klin(1:13)                                                             !/*ix4a       24*/
      call nxtsym(hn,' ',1,i,j,2)                                               !/*ix4a       25*/
      if(hn.eq.'atomic weight')go to 105                                        !/*ix4a       26*/
      if(hn(1:8).ne.'datapath')go to 100                                        !/*ix4a       27*/
      call nxtsym(klin,' =',9,i,j,0)                                            !/*ix4a       28*/
      if(j.ne.0.and.klin(i:i).eq.'=')call nxtsym(klin,' =',j+1,i,j,0)           !/*ix4a       29*/
      if(j.ne.0)hdpth=klin(i:j)                                                 !/*cor4-2    138*/
      go to 100                                                                 !/*cor4-2    139*/
  105 iu=0                                                                      !/*cor4-2    140*/
  110 read(iud,'(a80)',end=170)klin                                             !/*ix         64*/
  120 call nxtsym(klin,' ',iu+1,it,iu,1)                                        !/*ix4a       30*/
      if(iu.eq.0)go to 110                                                      !/*ix         66*/
      if(kdata(klin(it:iu)).eq.0)go to 170                                      !/*ix         67*/
      hb=klin(it:iu)                                                            !/*ix         68*/
      read(hb,'(bn,i30)')i                                                      !/*ix         69*/
  130 call nxtsym(klin,' ',iu+1,it,iu,1)                                        !/*ix4a       31*/
      if(iu.ne.0)go to 140                                                      !/*ix         71*/
      read(iud,'(a80)',end=360)klin                                             !/*ix         72*/
      go to 130                                                                 !/*ix         73*/
  140 hb=klin(it:iu)                                                            !/*ix         74*/
      read(hb,'(bn,e30.0)')a                                                    !/*ix         75*/
      do 150 j=1,mix                                                            !/*ix         76*/
  150 if(iza(liza+j).eq.i)awc(lawc+j)=a                                         !/*ix         77*/
      go to 120                                                                 !/*ix         78*/
  160 call expire(0,'ixsdir','cross-section directory file '//xsdir//           !/*ix         79*/
     & ' has no atomic weights table.')                                         !/*ix         80*/
!                                                                               !/*ix         81*/
!        read the next directory file entry.                                    !/*ix         82*/
  170 if(mxe.eq.0)go to 470                                                     !/*ixx        10*/
      if(klin.eq.'directory')go to 190                                          !/*ixx        11*/
      rewind iud                                                                !/*ix         84*/
  180 read(iud,'(a80)',end=360)klin                                             !/*ix         85*/
      call nxtsym(klin,' ',1,i,j,2)                                             !/*ix4a       32*/
      if(klin.ne.'directory')go to 180                                          !/*ix         86*/
  190 na=0                                                                      !/*ix         87*/
      nb=0                                                                      !/*ix         88*/
      hl=' '                                                                    !/*ix         89*/
      hd=' '                                                                    !/*ix4b       30*/
  200 iu=0                                                                      !/*ix         91*/
      read(iud,'(a80)',end=370)klin                                             !/*ix         92*/
  210 if(na.eq.0)call nxtsym(klin,' ',iu+1,it,iu,1)                             !/*ix4a       33*/
      if(na.ne.0)call nxtsym(klin,' ',iu+1,it,iu,0)                             !/*ix4a       34*/
      if(iu.eq.0)go to 240                                                      !/*ix         94*/
      if(klin(it:iu).eq.'obsolete')nb=1                                         !/*ix         95*/
      if(klin(it:iu).eq.'obsolete'.or.klin(it:iu).eq.'+')go to 200              !/*ix         96*/
      na=na+1                                                                   !/*ix         97*/
      if(na.gt.1)go to 220                                                      !/*ix         98*/
      i=index(klin(it:iu),'.')                                                  !/*ix         99*/
      if(i.eq.0)go to 190                                                       !/*ix        100*/
      hl(8-i:10)=klin(it:iu)                                                    !/*ix        101*/
      ic=10                                                                     !/*ix        102*/
      go to 210                                                                 !/*ix        103*/
  220 if(na.gt.10.and.klin(it:iu).ne.'ptable')go to 230                         !/*ix4c        2*/
      hl(ic+2:ic+iu-it+2)=klin(it:iu)                                           !/*ix        105*/
      ic=ic+iu-it+2                                                             !/*ix        106*/
      go to 210                                                                 !/*ix        107*/
  230 i=index(klin(it:iu),'/')                                                  !/*ix        108*/
      j=index(klin(min(iu,it+i):iu),'/')                                        !/*ix        109*/
      if((i.eq.2.or.i.eq.3).and.(j.eq.2.or.j.eq.3))hd=klin(it:iu)               !/*ix4b       31*/
      go to 210                                                                 !/*cor4-2    141*/
  240 hz=hl(1:10)                                                               !/*ix        111*/
      ha=hz(10:10)                                                              !/*ix        112*/
#ifdef PCDOS                                                                    /*!ix4a       39*/
      if(lockl)call pttyin                                                      !/*ix4a       40*/
#endif                                                                          /*!cpp       127*/
      if(irup.ne.0)call qttyin(0,'24hreading xsdir '//hz)                       !/*ix4a       41*/
!                                                                               !/*ix        113*/
!        look for a match with the directory file entry.                        !/*ix        114*/
  250 do 260 je=1,mxe                                                           !/*ix4b       32*/
      if(nty(lnty+je).gt.0)go to 260                                            !/*ix4b       33*/
      call zaid(2,hs,ixl(lixl+1,je))                                            !/*ix4b       34*/
      if(hs(1:7).ne.hz(1:7).or.hs(10:10).ne.ha)go to 260                        !/*ix4b       35*/
      if(hs(8:9).eq.hz(8:9).or.hs(8:9).eq.' ')go to 290                         !/*ix4b       36*/
  260 continue                                                                  !/*ix4b       37*/
!                                                                               !/*ix4b       38*/
!        if a 'c' table matches a 'd' request, hold it in case no 'd'           !/*ix4b       39*/
!        table is found.                                                        !/*ix4b       40*/
      if(kdr(lkdr+1).eq.0.or.ha.ne.'c')go to 190                                !/*ix4b       41*/
      do 270 je=1,mxe                                                           !/*ix4b       42*/
      if(nty(lnty+je).ne.0)go to 270                                            !/*ix4b       43*/
      call zaid(2,ht,ixl(lixl+1,je))                                            !/*ix4b       44*/
      if(ht(1:7).ne.hz(1:7).or.ht(10:10).ne.'d')go to 270                       !/*ix4b       45*/
      if(ht(8:9).eq.hz(8:9).or.ht(8:9).eq.' ')go to 280                         !/*ix4b       46*/
  270 continue                                                                  !/*ix4b       47*/
      go to 190                                                                 !/*ix4b       48*/
  280 nty(lnty+je)=-1                                                           !/*ix4b       49*/
      go to 300                                                                 !/*ix4b       50*/
!                                                                               !/*ix4b       51*/
!        a match has been found. load the directory entry.                      !/*ix4b       52*/
  290 nty(lnty+je)=index(htn,ha)                                                !/*ix4c2       4*/
      call zaid(1,hz,ixl(lixl+1,je))                                            !/*ix4b       54*/
      nx=nx+1                                                                   !/*ix4b       55*/
  300 do 310 i=1,10                                                             !/*ix4b       56*/
      n=index('0123456789/',hd(i:i))                                            !/*ix4b       57*/
  310 if(n.ne.0)kxd(lkxd+je)=11*kxd(lkxd+je)+n                                  !/*ix4b       58*/
      kxs(lkxs+je)=nb                                                           !/*ix4b       59*/
      do 320 i=1,61                                                             !/*ix4b       60*/
  320 ixc(lixc+i,je)=0                                                          !/*ix4b       61*/
      call zaid(1,hl,ixc(lixc+1,je))                                            !/*ix4b       62*/
      do 330 i=11,181                                                           !/*ix4b       63*/
  330 ixc(lixc+(i+1)/3,je)=ixc(lixc+(i+1)/3,je)+                                !/*ix4b       64*/
     & ichar(hl(i:i))*256**mod(i+1,3)                                           !/*ix4b       65*/
      if(nx.eq.mxe)go to 385                                                    !/*ix4b       66*/
      go to 250                                                                 !/*ix4b       67*/
!                                                                               !/*ix4b       68*/
!        unexpected eof in xsdir file.                                          !/*ix4b       69*/
  360 call expire(0,'ixsdir',                                                   !/*ix        155*/
     & 'bad data in cross-section directory file '//xsdir)                      !/*ix        156*/
!                                                                               !/*ix        157*/
!        try to change any unmatched d tables to c.                             !/*ix        158*/
  370 do 380 ie=1,mxe                                                           !/*ix4b       70*/
      if(nty(lnty+ie).ge.0)go to 380                                            !/*ix4b       71*/
      call zaid(2,ht,ixl(lixl+1,ie))                                            !/*ix4b       72*/
      call zaid(2,hs,ixc(lixc+1,ie))                                            !/*ix4b       73*/
      call zaid(1,hs,ixl(lixl+1,ie))                                            !/*ix4b       74*/
      nty(lnty+ie)=1                                                            !/*ix4b       75*/
      nx=nx+1                                                                   !/*ix4b       76*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*ix4b       77*/
     & '57hcontinuous-energy cross-section table used for '//ht)                !/*ix4b       78*/
  380 continue                                                                  !/*ix4b       79*/
!                                                                               !/*ix4b       80*/
!        check for duplicate entries.                                           !/*ix4b       81*/
  385 ie=0                                                                      !/*ix4b       82*/
  390 if(ie.eq.mxe)go to 470                                                    !/*ix4b       83*/
      ie=ie+1                                                                   !/*ix4b       84*/
      call zaid(2,ht,ixl(lixl+1,ie))                                            !/*ix4b       85*/
  400 do 410 je=ie+1,mxe                                                        !/*ix4b       86*/
      call zaid(2,hz,ixl(lixl+1,je))                                            !/*ix4b       87*/
  410 if(hz.eq.ht)go to 420                                                     !/*ix4b       88*/
      go to 390                                                                 !/*ix4b       89*/
!                                                                               !/*ix4b       90*/
!        eliminate duplicate entries.                                           !/*ix4b       91*/
  420 do 430 m=1,mix                                                            !/*ix4b       92*/
      do 430 i=1,mipt                                                           !/*ix4b       93*/
      if(lme(llme+i,m).eq.je)lme(llme+i,m)=ie                                   !/*ix4b       94*/
  430 if(lme(llme+i,m).gt.je)lme(llme+i,m)=lme(llme+i,m)-1                      !/*ix4b       95*/
      do 435 m=1,mix                                                            !/*ix4c2       5*/
      if(lmn(llmn+m).eq.je)lmn(llmn+m)=ie                                       !/*ix4c2       6*/
  435 if(lmn(llmn+m).gt.je)lmn(llmn+m)=lmn(llmn+m)-1                            !/*ix4c2       7*/
      do 460 ke=je,mxe-1                                                        !/*ix4b       96*/
      do 440 i=1,3                                                              !/*ix4b       97*/
  440 ixl(lixl+i,ke)=ixl(lixl+i,ke+1)                                           !/*ix4b       98*/
      do 450 i=1,61                                                             !/*ix4b       99*/
  450 ixc(lixc+i,ke)=ixc(lixc+i,ke+1)                                           !/*ix4b      100*/
      kxd(lkxd+ke)=kxd(lkxd+ke+1)                                               !/*ix4b      101*/
      kxs(lkxs+ke)=kxs(lkxs+ke+1)                                               !/*ix4b      102*/
  460 nty(lnty+ke)=nty(lnty+ke+1)                                               !/*ix4b      103*/
      mxe=mxe-1                                                                 !/*ix4b      104*/
      nx=nx-1                                                                   !/*ix4b      105*/
      go to 400                                                                 !/*ix4b      106*/
!                                                                               !/*ix        196*/
!        print lists of missing tables.                                         !/*ix        197*/
  470 if(nx.eq.mxe)go to 490                                                    !/*ix4b      107*/
      if (lcaopt(8).ne.-1) then                                                 !/*ixx        12*/
      call erprnt(1,1,1,mxe-nx,0,0,0,0,                                         !/*ix        198*/
     & 'i3,47h cross-section tables are missing from '//xsdir)                  !/*ix        199*/
      np=0                                                                      !/*ix4c2       8*/
      do 480 ie=1,mxe                                                           !/*ix        200*/
      if(nty(lnty+ie).ne.0)go to 480                                            !/*ix        201*/
      call zaid(2,ht,ixl(lixl+1,ie))                                            !/*ix        202*/
      write(jtty,'(1x,a10)')ht                                                  !/*ix        203*/
      write(iuo,'(1x,a10)')ht                                                   !/*ix        204*/
      if(ht(10:10).ne.'u')go to 480                                             !/*ix4c2       9*/
      np=np+1                                                                   !/*ix4c2      10*/
      jxs(ljxs+1,ie)=-1                                                         !/*ix4c2      11*/
      do 475 i=1,mix                                                            !/*ix4c2      12*/
  475 if(lmn(llmn+i).eq.ie)izn(lizn+i)=0                                        !/*ix4c2      13*/
  480 continue                                                                  !/*ix        205*/
      if(nx+np.eq.mxe)call erprnt(1,1,1,np,0,0,0,0,                             !/*ix4c2      14*/
     & '40hphotonuclear production data missing for,i3,10h nuclides.')          !/*ix4c2      15*/
      if(nx+np.lt.mxe)call expire(0,'ixsdir',                                   !/*ix4c2      16*/
     & 'cannot continue with missing cross-section table(s).')                  !/*ix4c2      17*/
      endif                                                                     !/*ixx        13*/
!                                                                               !/*ix        206*/
!        print list of obsolete tables.                                         !/*ix        207*/
  490 do 500 ie=1,mxe                                                           !/*ix        208*/
      if(kxs(lkxs+ie).eq.0)go to 500                                            !/*ix        209*/
      call zaid(2,ht,ixl(lixl+1,ie))                                            !/*ix        210*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*ix        211*/
     & '54h'//ht//' is an obsolete table to be eliminated soon.')               !/*ix        212*/
  500 continue                                                                  !/*ix        213*/
      if(jc.ne.0)close(iud)                                                     !/*ix        214*/
      return                                                                    !/*ix        215*/
      end                                                                       !/*ix        216*/

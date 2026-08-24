!#ident getxst.F from deck gt                                                   !/*gt          1*/
      subroutine getxst(nt)                                                     !/*gt          2*/
!        get the cross-section tables.                                          !/*gt          3*/
!        nt=0 get neutron and photon tables.  nt>0 get electron tables.         !/*gt          4*/
      use allocate_memory                                                       !/*gtx         1*/
      use global_data                                                           !/*gtx         2*/
!                                                                               !/*gtx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*gtx         4*/
      implicit integer(kindi) (i-n)                                             !/*gtx         5*/
!                                                                               !/*gtx         6*/
      character ha*80,hc*181,hd*10,hf*64,hk*70,hm*10,hp*10,hr*70,ht*10          !/*gt4a        1*/
      dimension iq(3),iz(0:16),ly(5)                                            !/*gt          7*/
!                                                                               !/*gt          8*/
!        expand the field length to accomodate the largest table.               !/*gt          9*/
      et=huge                                                                   !/*gt         10*/
      em=0.                                                                     !/*gt         11*/
      ih=0                                                                      !/*gt4c        1*/
      th=0.                                                                     !/*gt4c        2*/
      mb=0                                                                      !/*gt         12*/
      lp=0                                                                      !/*gtx         7*/
      kf=0                                                                      !/*gt         15*/
      jb=1                                                                      !/*gt         16*/
      if(nt.ne.0)jb=mxe+1                                                       !/*gt         17*/
      do 40 i=jb,mxe+nt                                                         !/*gt         18*/
      call zaid(2,hc,ixc(lixc+1,i))                                             !/*gt         19*/
      do 10 j=11,181                                                            !/*gt         20*/
      k=mod(ixc(lixc+(j+1)/3,i)/256**mod(j+1,3),256)                            !/*gt         21*/
      if(k.eq.0)go to 20                                                        !/*gt         22*/
   10 hc(j:j)=char(k)                                                           !/*gt         23*/
   20 iu=10                                                                     !/*gt         24*/
      do 30 j=2,7                                                               !/*gt         25*/
   30 call nxtsym(hc,' ',iu+1,it,iu,0)                                          !/*gt4a        2*/
      ht=hc(it:iu)                                                              !/*gt         27*/
      read(ht,'(bn,i10)')m                                                      !/*gt         28*/
      if(nt.eq.0)mb=max(mb,m)                                                   !/*gt         29*/
   40 if(nt.ne.0)mb=mb+m+2170                                                   !/*gt         30*/
      if(nt.eq.0) then                                                          !/*gtx         8*/
        mb=mb+npikmt                                                            !/*gtx         9*/
        xss => chg_mem (xss,mb,is)                                              !/*gtx        10*/
        if(is.ne.0)call expire(0,'getxst','chg_mem error')                      !/*gtx        11*/
      else                                                                      !/*gtx        12*/
        if (allocated(exs)) deallocate (exs)                                    !/*gtx        13*/
        allocate (exs(mb), stat=is)                                             !/*gtx        14*/
        if(is.ne.0)call expire(0,'getxst','error getting memory')               !/*gtx        15*/
        exs=0.                                                                  !/*gtx        16*/
      endif                                                                     !/*gtx        17*/
!                                                                               !/*gt         34*/
!        print the page headings.                                               !/*gt         35*/
      if(nt.eq.0.or.mxe.eq.0)write(iuo,50)                                      !/*gt         36*/
   50 format(21h1cross-section tables,83x,15hprint table 100//                  !/*gt4a        6*/
     & 5x,5htable,4x,6hlength)                                                  !/*gt         38*/
!                                                                               !/*gt         39*/
!        get the tables from each indicated cross-section file.                 !/*gt         40*/
      nc=0                                                                      !/*gt         41*/
      do 410 je=jb,mxe+nt                                                       !/*gt         42*/
      if(jxs(ljxs+1,je).ne.0)go to 410                                          !/*gt         43*/
      hc=' '                                                                    !/*gt         44*/
      call zaid(2,hc,ixc(lixc+1,je))                                            !/*gt         45*/
      do 60 j=11,181                                                            !/*gt         46*/
      k=mod(ixc(lixc+(j+1)/3,je)/256**mod(j+1,3),256)                           !/*gt         47*/
      if(k.eq.0)go to 70                                                        !/*gt         48*/
   60 hc(j:j)=char(k)                                                           !/*gt         49*/
!                                                                               !/*gt         50*/
!        get the characteristics of the file.                                   !/*gt         51*/
   70 call nxtsym(hc,' ',11,it,iu,0)                                            !/*gt4a        7*/
      call nxtsym(hc,' ',iu+1,it,iu,0)                                          !/*gt4a        8*/
      hf=hc(it:iu)                                                              !/*gt         54*/
      call nxtsym(hc,' ',iu+1,it,iu,0)                                          !/*gt4a        9*/
      hr=hc(it:iu)                                                              !/*gt         56*/
      do 80 i=1,5                                                               !/*gt         57*/
      call nxtsym(hc,' ',iu+1,it,iu,0)                                          !/*gt4a       10*/
      if(iu.eq.0)go to 90                                                       !/*gt         59*/
      ht=hc(it:iu)                                                              !/*gt         60*/
   80 read(ht,'(bn,i10)')ly(i)                                                  !/*gt         61*/
!                                                                               !/*gt         62*/
!        fetch the file if it is not already present, and open it.              !/*gt         63*/
   90 ha=hf                                                                     !/*cor4-2    162*/
      if(inqire(ha).eq.1)go to 95                                               !/*cor4-2    163*/
#ifdef PCDOS                                                                    /*!gt4c2       1*/
      if(leng(hdpath).gt.0)ha=hdpath(1:leng(hdpath))//'\'//hf                   !/*gt4c2       2*/
#else                                                                           /*!gtx        18*/
      if(leng(hdpath).gt.0)ha=hdpath(1:leng(hdpath))//'/'//hf                   !/*gt4b        2*/
#endif                                                                          /*!cpp       179*/
      if(inqire(ha).eq.1)go to 95                                               !/*cor4-2    165*/
#ifdef PCDOS                                                                    /*!gt4c2       4*/
      if(leng(hdpth).gt.0)ha=hdpth(1:leng(hdpth))//'\'//hf                      !/*gt4c2       5*/
#else                                                                           /*!gtx        19*/
      if(leng(hdpth).gt.0)ha=hdpth(1:leng(hdpth))//'/'//hf                      !/*gt4b        4*/
#endif                                                                          /*!cpp       182*/
      if(inqire(ha).eq.1)go to 95                                               !/*cor4-2    167*/
      if(leng(hr).gt.0)ha=hr                                                    !/*cor4-2    168*/
      if(inqire(ha).eq.1)go to 95                                               !/*cor4-2    169*/
      ha=hf                                                                     !/*cor4-2    170*/
      call ffetch(hf,hr)                                                        !/*cor4-2    171*/
      if(inqire(ha).eq.0)call expire(0,'getxst','cross-section file '//         !/*cor4-2    172*/
     & hf(1:min(34,leng(hf)))//' does not exist.')                              !/*cor4-2    173*/
   95 nr=1                                                                      !/*cor4-2    174*/
      if(ly(1).eq.1)open(iux,file=ha,status='old')                              !/*cor4-2    175*/
      if(ly(1).eq.2)open(iux,file=ha,access='direct',                           !/*cor4-2    176*/
     & status='old',recl=ly(4))                                                 !/*gt         70*/
      if(ly(1).eq.1)rewind iux                                                  !/*gt         77*/
!                                                                               !/*gt         78*/
!        get all of the pertinent tables in the file.                           !/*gt         79*/
      write(iuo,100)hf                                                          !/*gt         80*/
  100 format(/24x,17htables from file ,a64/)                                    !/*gt         81*/
      do 400 iex=je,mxe+nt                                                      !/*gt         83*/
      if(iex.eq.je)go to 120                                                    !/*gt         84*/
      hc=' '                                                                    !/*gt         85*/
      call zaid(2,hc,ixc(lixc+1,iex))                                           !/*gt         86*/
      do 110 j=11,181                                                           !/*gt         87*/
      k=mod(ixc(lixc+(j+1)/3,iex)/256**mod(j+1,3),256)                          !/*gt         88*/
      if(k.eq.0)go to 120                                                       !/*gt         89*/
  110 hc(j:j)=char(k)                                                           !/*gt         90*/
  120 call nxtsym(hc,' ',11,it,iu,0)                                            !/*gt4a       17*/
      call nxtsym(hc,' ',iu+1,it,iu,0)                                          !/*gt4a       18*/
      if(hc(it:iu).ne.hf)go to 400                                              !/*gt         93*/
      call sread(nr,iz,hk,hm,hd,hc,lp)                                          !/*gt         94*/
      call zaid(2,ht,ixl(lixl+1,iex))                                           !/*gt         95*/
!                                                                               !/*gt4c        5*/
!        check for unresolved flag on xsdir entry.                              !/*gt4c        6*/
      if(nty(lnty+iex).ne.1)go to 128                                           !/*gt4c        7*/
      if(jxs(ljxs+23,iex).eq.0)go to 128                                        !/*gt4c        8*/
      do 124 j=4,15                                                             !/*gt4c        9*/
      call nxtsym(hc,' ',iu+1,it,iu,1)                                          !/*gt4c       10*/
      if(iu.eq.0)go to 126                                                      !/*gt4c       11*/
  124 if(j.gt.10.and.hc(it:iu).eq.'ptable')go to 128                            !/*gt4c       12*/
  126 call erprnt(1,1,0,0,0,0,0,0,                                              !/*gt4c       13*/
     & '49hxsdir entry for '//ht//' lacks unresolved flag.')                    !/*gt4c       14*/
!                                                                               !/*gt         96*/
!        convert the pointers to indexes in the buffer in xss.                  !/*gt         97*/
  128 do 130 i=1,32                                                             !/*gt4c       15*/
  130 if(jxs(ljxs+i,iex).ne.0)jxs(ljxs+i,iex)=jxs(ljxs+i,iex)+lp                !/*gt         99*/
!                                                                               !/*gtx        20*/
      if(index('cdh',ht(10:10)).eq.0)go to 131                                  !/*gtx        21*/
      if(nxs(lnxs+7,iex).eq.0) go to 131                                        !/*gtx        22*/
!                                                                               !/*gtx        23*/
!         quit if nxs(7).gt.maxsec for class c, d, or h data tables             !/*gtx        24*/
      if(nxs(lnxs+7,iex).le.maxsec) go to 132                                   !/*gtx        25*/
      call expire(0,'getxst',                                                   !/*gtx        26*/
     & 'nxs(7) greater than maxsec for '//ht(1:10)//'.')                        !/*gtx        27*/
!                                                                               !/*gtx        28*/
!         load ixs array.  change locators by lp.                               !/*gtx        29*/
!         Proton and neutron table version 1 have mixs == 10.                   !/*gtx        30*/
  132 do 133 j=1,nxs(lnxs+7,iex)                                                !/*gtx        31*/
      do 133 i=1,10                                                             !/*gtx        32*/
      if(nint(xss(jxs(ljxs+32,iex)+(j-1)*10+i-1)).ne.0)                         !/*gtx        33*/
     & ixs(lixs+i,j,iex)=nint(xss(jxs(ljxs+32,iex)+(j-1)*10+i-1))+lp            !/*gtx        34*/
  133 continue                                                                  !/*gtx        35*/
  131 continue                                                                  !/*gtx        36*/
!                                                                               !/*gtx        37*/
      if(nt.ne.0)lp=lp+nxs(lnxs+1,iex)+2170                                     !/*gt        100*/
!                                                                               !/*gt        101*/
      go to(140,140,140,200,290,310,310,353,301,360)nty(lnty+iex)               !/*gtx        38*/
!                                                                               !/*gt        103*/
! >>>>>  continuous or discrete reaction or dosimetry neutron table.            !/*gt        104*/
!        expunge data not needed by the problem.                                !/*gt        105*/
  140 if(nty(lnty+iex).ne.3)em=max(em,zero+xss(jxs(ljxs+1,iex)))                !/*gt        106*/
      call expung(hk(52:60),mg)                                                 !/*gt        107*/
!                                                                               !/*gt        108*/
!        adjust the temperature of the table.                                   !/*gt        109*/
      th=tbt(ltbt+iex)                                                          !/*gt4c       16*/
      if(nty(lnty+iex).ne.3)call broadn                                         !/*gt        110*/
      if(th.ne.tbt(ltbt+iex))ih=ih+1                                            !/*gt4c       17*/
!                                                                               !/*gt        111*/
!        form the total fission cross-section table if necessary.               !/*gt        112*/
      if(jxs(ljxs+2,iex).eq.0)go to 170                                         !/*gt        113*/
      kf=1                                                                      !/*gt        114*/
      if(jxs(ljxs+21,iex).eq.0.and.(itfxs.ne.0.or.jovr(5).ne.0.or.              !/*gtxc        1*/
     & jxs(ljxs+13,iex).ne.0))call acefpt                                       !/*gtxc        2*/
!                                                                               !/*gt        117*/
!        flag cells where fission is treated as capture.                        !/*gt        118*/
      do 160 i=1,mxa                                                            !/*gt        119*/
      if(nsr.ne.71.and.lfcl(llfc+i).ge.0)go to 160                              !/*gt        120*/
      j=mat(lmat+i)                                                             !/*gt        121*/
      do 150 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                      !/*gt        122*/
      if(lme(llme+1,m).ne.iex)go to 150                                         !/*gt        123*/
      if(nsr.eq.71.or.lfcl(llfc+i).eq.-1)lfcl(llfc+i)=1                         !/*gt        124*/
      if(lfcl(llfc+i).eq.-2)lfcl(llfc+i)=2                                      !/*gt        125*/
  150 continue                                                                  !/*gt        126*/
  160 continue                                                                  !/*gt        127*/
!                                                                               !/*gt        128*/
!        print information about the table.                                     !/*gt        129*/
  170 write(iuo,180)ht,nxs(lnxs+1,iex),hk,hm,hd                                 !/*gt        130*/
  180 format(1x,a10,i8,2x,a70,4x,a10,4x,a10)                                    !/*gt        131*/
      l=jxs(ljxs+23,iex)                                                        !/*gt4c       18*/
      if(l.ne.0)write(iuo,182)xss(l+6),xss(l+5+nint(xss(l)))                    !/*gt4c       19*/
  182 format(21x,28hprobability tables used from,1p1e11.4,3h to,e11.4,          !/*gt4c       20*/
     & 5h mev.)                                                                 !/*gt4c       21*/
      if(th.ne.tbt(ltbt+iex).and.tbt(ltbt+iex).ne.0.)                           !/*gt4c       22*/
     & write(iuo,184)th,tbt(ltbt+iex)                                           !/*gt4c       23*/
  184 format(21x,13htemperature =,1pe11.4,12h adjusted to,e11.4)                !/*gt4c       24*/
      if(th.ne.tbt(ltbt+iex).and.tbt(ltbt+iex).eq.0.)write(iuo,186)th           !/*gt4c       25*/
  186 format(21x,13htemperature =,1pe11.4,24h adjusted at collisions.)          !/*gt4c       26*/
      if(jxs(ljxs+2,iex).ne.0.and.nxs(lnxs+8,iex).eq.0.and.itotnu.eq.1          !/*gt4c2       8*/
     & .and.dnb.ne.0.)call erprnt(1,2,0,0,0,0,0,0,                              !/*gt4c2       9*/
     & '48h'//ht//' lacks delayed neutron cross sections.')                     !/*gt4c2      10*/
!                                                                               !/*gt        132*/
!        store maximum energy for the table (n/a to dosimetry tables)           !/*gtx        39*/
      if(nty(lnty+iex).ne.3)                                                    !/*gtx        40*/
     & emaxt(lemaxt+iex)=xss(jxs(ljxs+1,iex)+nxs(lnxs+3,iex)-1)                 !/*gtx        41*/
!                                                                               !/*gtx        42*/
!        print gamma-production message or warning.                             !/*gt        133*/
      if(mg.eq.0.or.jxs(ljxs+13,iex).ne.0)go to 380                             !/*gt        134*/
      if(jxs(ljxs+12,iex).ne.0)nc=nc+1                                          !/*gt        135*/
      if(jxs(ljxs+12,iex).ne.0)write(iuo,190)                                   !/*gt        136*/
  190 format(26x,42hobsolete gamma-ray production method using,                 !/*gt        137*/
     & 27h equally-probable energies.)                                          !/*gt        138*/
      if(jxs(ljxs+12,iex).eq.0)call erprnt(1,2,0,0,0,0,0,0,                     !/*gt        139*/
     & '53h'//ht//' lacks gamma-ray production cross sections.')                !/*gt        140*/
      go to 380                                                                 !/*gt        141*/
!                                                                               !/*gt        142*/
! >>>>>  thermal s(a,b) neutron table.                                          !/*gt        143*/
!        set up the cut-in energy.                                              !/*gt        144*/
  200 esa(lesa+iex)=xss(jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+1,iex))))             !/*gt4c       27*/
      if(jxs(ljxs+4,iex).ne.0.and.nxs(lnxs+5,iex).ne.4)esa(lesa+iex)=           !/*gt        146*/
     & min(zero+xss(jxs(ljxs+4,iex)+nint(xss(jxs(ljxs+4,iex)))),                !/*gt4c       28*/
     & esa(lesa+iex))                                                           !/*gt        148*/
!                                                                               !/*gt        149*/
!        print information about the table.                                     !/*gt        150*/
      write(iuo,210)ht,nxs(lnxs+1,iex),hk,iz(1),iz(2),iz(3),hd                  !/*gt        151*/
  210 format(1x,a10,i8,2x,a70,3i6,a10)                                          !/*gt        152*/
!                                                                               !/*gt        153*/
!        set up the lmt array.                                                  !/*gt        154*/
!        check that zaids implied by mt card entries are on m card.             !/*gt        155*/
      do 280 k=1,indt                                                           !/*gt        156*/
      call zaid(2,hm,kmt(lkmt+1,k))                                             !/*gt        157*/
      if(hm(1:9).ne.ht(1:9).and.hm(1:9).ne.ht(1:7)//'  ')go to 280              !/*gt        158*/
      do 220 j=1,3                                                              !/*gt        159*/
      iq(j)=0                                                                   !/*gt        160*/
  220 if(iz(j).ne.0)iq(j)=1                                                     !/*gt        161*/
      do 230 i=1,nmat                                                           !/*gt        162*/
  230 if(nmt(lnmt+i).eq.jmt(ljmt+k))go to 240                                   !/*gt        163*/
  240 do 260 m=jmd(ljmd+i),jmd(ljmd+i+1)-1                                      !/*gt        164*/
      ie=lme(llme+1,m)                                                          !/*gt        165*/
      do 250 j=1,3                                                              !/*gt        166*/
      if(nxs(lnxs+2,ie).ne.iz(j))go to 250                                      !/*gt        167*/
      lmt(llmt+m)=iex                                                           !/*gt        168*/
      iq(j)=0                                                                   !/*gt        169*/
      if(iz(mod(j,3)+1)/1000.eq.iz(j)/1000)iq(mod(j,3)+1)=0                     !/*gt        170*/
      if(iz(mod(j+1,3)+1)/1000.eq.iz(j)/1000)iq(mod(j+1,3)+1)=0                 !/*gt        171*/
  250 continue                                                                  !/*gt        172*/
  260 continue                                                                  !/*gt        173*/
      do 270 j=1,3                                                              !/*gt        174*/
  270 if(iq(j).ne.0)call erprnt(1,2,3,nmt(lnmt+i),iz(j),nmt(lnmt+i),0,0,        !/*gt        175*/
     & '1hm,i3,11h card lacks,i6,21h for '//hm//' on mt,i3,6h card.')           !/*gt        176*/
  280 continue                                                                  !/*gt        177*/
      go to 380                                                                 !/*gt        178*/
!                                                                               !/*gt        179*/
! >>>>>  photon table.                                                          !/*gt        180*/
!        find the maximum energy over all tables, and note the ZAID.            !/*gtx        43*/
  290 e=et                                                                      !/*gt4a       19*/
      et=min(et,zero+xss(jxs(ljxs+1,iex)+nxs(lnxs+3,iex)-1))                    !/*gt4a       20*/
      if(et.lt.e)hp=ht                                                          !/*gt4a       21*/
!                                                                               !/*gtx        44*/
!        save the maximum energy for this table.                                !/*gtx        45*/
      emaxt(lemaxt+iex)=exp(xss(jxs(ljxs+1,iex)+nxs(lnxs+3,iex)-1))             !/*gtx        46*/
!                                                                               !/*gtx        47*/
!        print information about the table.                                     !/*gtx        48*/
      write(iuo,300)ht,nxs(lnxs+1,iex),hk,hm,hd                                 !/*gt        183*/
  300 format(1x,a10,i8,2x,a70,4x,a10,4x,a10)                                    !/*gt        184*/
      go to 380                                                                 !/*gt        185*/
!                                                                               !/*gt4c2      11*/
! >>>>>  photonuclear table.                                                    !/*gt4c2      12*/
!        if secondary particle information exists, load ixs.                    !/*gt4c2      13*/
  301 if(nxs(lnxs+5,iex).eq.0)go to 306                                         !/*gt4c2      14*/
!        if nxs(5).gt.maxsec, exit to avoid memory error                        !/*gt4c2      15*/
      if(nxs(lnxs+5,iex).gt.maxsec)call expire(0,'getxst',                      !/*gt4c2      16*/
     & 'nxs(5) greater than maxsec for table '//ht(1:10)//'.')                  !/*gt4c2      17*/
      do 303 i=1,mixs                                                           !/*gt4c2      18*/
      do 303 j=1,nxs(lnxs+5,iex)                                                !/*gt4c2      19*/
      k=nint(xss(jxs(ljxs+10,iex)+i+mixs*(j-1)-1))                              !/*gt4c2      20*/
      ixs(lixs+i,j,iex)=k                                                       !/*gt4c2      21*/
  303 if(i.gt.2.and.k.ne.0)ixs(lixs+i,j,iex)=k+lp+lxs                           !/*gt4c2      22*/
!                                                                               !/*gt4c2      23*/
!        find minimum energy for each material                                  !/*gt4c2      24*/
  306 do 307 i=1,nmat                                                           !/*gt4c2      25*/
      do 307 j=jmd(ljmd+i),jmd(ljmd+i+1)-1                                      !/*gt4c2      26*/
  307 if(iex.eq.lmn(llmn+j))                                                    !/*gt4c2      27*/
     & pnt(lpnt+i)=min(pnt(lpnt+i),xss(jxs(ljxs+1,iex)))                        !/*gt4c2      28*/
!                                                                               !/*gt4c2      29*/
!        print table information                                                !/*gt4c2      30*/
      write(iuo,309)ht,nxs(lnxs+1,iex),hk,hm,hd                                 !/*gt4c2      31*/
  309 format(1x,a10,i8,2x,a70,4x,a10,4x,a10)                                    !/*gt4c2      32*/
      go to 380                                                                 !/*gt4c2      33*/
!                                                                               !/*gt        186*/
! >>>>>  multigroup table for any type of particle.                             !/*gt        187*/
  310 call mgxspt(hk(52:60))                                                    !/*gt        188*/
!                                                                               !/*gt        189*/
!        flag cells where fission is treated as capture.                        !/*gt        190*/
      if(jxs(ljxs+3,iex).eq.0)go to 340                                         !/*gt        191*/
      kf=1                                                                      !/*gt        192*/
      do 330 i=1,mxa                                                            !/*gt        193*/
      if(lfcl(llfc+i).ge.0.and.nsr.ne.71)go to 330                              !/*gt        194*/
      j=mat(lmat+i)                                                             !/*gt        195*/
      do 320 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                      !/*gt        196*/
      if(lme(llme+1,m).ne.iex)go to 320                                         !/*gt        197*/
      if(nsr.eq.71.or.lfcl(llfc+i).eq.-1)lfcl(llfc+i)=1                         !/*gt        198*/
      if(lfcl(llfc+i).eq.-2)lfcl(llfc+i)=2                                      !/*gt        199*/
  320 continue                                                                  !/*gt        200*/
  330 continue                                                                  !/*gt        201*/
!                                                                               !/*gt        202*/
!        print information about the table.                                     !/*gt        203*/
  340 write(iuo,350)ht,nxs(lnxs+1,iex),hk,hm,hd                                 !/*gt        204*/
  350 format(1x,a10,i8,2x,a70,4x,a10,4x,a10)                                    !/*gt        205*/
      go to 380                                                                 !/*gt        206*/
!                                                                               !/*gtx        49*/
! >>>>>  proton table.                                                          !/*gtx        50*/
!        save the maximum energy of the table                                   !/*gtx        51*/
 353  emaxt(lemaxt+iex)=xss(jxs(ljxs+1,iex)+nxs(lnxs+3,iex)-1)                  !/*gtx        52*/
!                                                                               !/*gtx        53*/
!        print information about the table.                                     !/*gtx        54*/
      write(iuo,357)ht,nxs(lnxs+1,iex),hk,hm,hd                                 !/*gtx        55*/
 357  format(1x,a10,i8,2x,a70,4x,a10,4x,a10)                                    !/*gtx        56*/
      go to 380                                                                 !/*gtx        57*/
!                                                                               !/*gt        207*/
! >>>>>  electron table.                                                        !/*gt        208*/
!        print information about the table.                                     !/*gt        209*/
  360 write(iuo,370)ht,nxs(lnxs+1,iex),hk,hm,hd                                 !/*gt        210*/
  370 format(1x,a10,i8,2x,a70,4x,a10,4x,a10)                                    !/*gt        211*/
      jxs(ljxs+7,iex)=jxs(ljxs+1,iex)+nxs(lnxs+1,iex)                           !/*gt        212*/
      jxs(ljxs+8,iex)=jxs(ljxs+7,iex)+2160                                      !/*gt        213*/
      if(its30.eq.0)its30=nxs(lnxs+16,iex)                                      !/*gt4c       29*/
      if(its30.ne.nxs(lnxs+16,iex))call erprnt(2,1,0,0,0,0,0,1,                 !/*gt4c       30*/
     & '33helectron tables are incompatible.')                                  !/*gt4c       31*/
      go to 400                                                                 !/*gt        214*/
!                                                                               !/*gt        215*/
!        convert the indexes in the buffer into indexes in xss.                 !/*gt        216*/
  380 do 390 i=1,32                                                             !/*gt        217*/
  390 if(jxs(ljxs+i,iex).ne.0)jxs(ljxs+i,iex)=jxs(ljxs+i,iex)+lxs               !/*gt        218*/
!                                                                               !/*gtx        58*/
      if(nty(lnty+iex).eq.1.or.nty(lnty+iex).eq.2.or.                           !/*gtx        59*/
     & nty(lnty+iex).eq.8) then                                                 !/*gtx        60*/
         ms=1                                                                   !/*gtx        61*/
         me=10                                                                  !/*gtx        62*/
         ns=nxs(lnxs+7,iex)                                                     !/*gtx        63*/
      else                                                                      !/*gtx        64*/
         ns=0                                                                   !/*gtx        65*/
      endif                                                                     !/*gtx        66*/
      do 395 j=1,ns                                                             !/*gtx        67*/
      do 395 i=ms,me                                                            !/*gtx        68*/
  395 if(ixs(lixs+i,j,iex).ne.0)ixs(lixs+i,j,iex)=ixs(lixs+i,j,iex)+lxs         !/*gtx        69*/
!                                                                               !/*gt        219*/
!        write the table on the runtpe file.                                    !/*gt        220*/
      call tpefil(2)                                                            !/*gt        221*/
      lxs=lxs+nxs(lnxs+1,iex)                                                   !/*gt        222*/
      if(nxs(lnxs+1,iex).eq.0)call erprnt(1,1,0,0,0,0,0,0,                      !/*gt        223*/
     & '50htable '//ht//' has been expunged to zero length.')                   !/*gt        224*/
  400 continue                                                                  !/*gt        225*/
      close(iux)                                                                !/*gt        226*/
  410 continue                                                                  !/*gt        227*/
      if(nt.ne.0)return                                                         !/*gt        228*/
!                                                                               !/*gt4c       32*/
!        check for unresolved probability tables.  If a nuclide with            !/*gt4c       33*/
!        probability tables (jxs(ljxs+23,i)>0) is specified at several          !/*gt4c       34*/
!        temperatures (same zaid numbers, nxs(lnxs+2,i), different i),          !/*gt4c       35*/
!        set iunr=-n to flag correlated multi-temperatures.                     !/*gt4c       36*/
      if(xunru.eq.0.)iunr=0                                                     !/*gt4c       37*/
      if(iunr.le.0)go to 418                                                    !/*gt4c       38*/
      do 414 ie=1,mxe-1                                                         !/*gt4c       39*/
      if(nty(lnty+ie).ne.1.or.jxs(ljxs+23,ie).le.0)go to 414                    !/*gt4c       40*/
      do 412 je=ie+1,mxe                                                        !/*gt4c       41*/
  412 if(nty(lnty+je).eq.1.and.jxs(ljxs+23,je).gt.0.and.nxs(lnxs+2,ie)          !/*gt4c       42*/
     & .eq.nxs(lnxs+2,je))go to 416                                             !/*gt4c       43*/
  414 continue                                                                  !/*gt4c       44*/
      go to 418                                                                 !/*gt4c       45*/
  416 iunr=-iunr                                                                !/*gt4c       46*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*gt4c       47*/
     & '56hmulti-temperature treatment used for neutron unresolved.')           !/*gt4c       48*/
!                                                                               !/*gt        229*/
!        turn off any irrelevant fission-as-capture flags.                      !/*gt        230*/
  418 do 420 i=1,mxa                                                            !/*gt4c       49*/
  420 if(lfcl(llfc+i).lt.0)lfcl(llfc+i)=0                                       !/*gt        232*/
!                                                                               !/*gt        233*/
!        print the total storage used and the expunge limits.                   !/*gt        234*/
      write(iuo,'(/7h  total,i12)')lxs-1                                        !/*gtx        70*/
      if(emx(1).lt.huge)write(iuo,430)emx(1)                                    !/*gt        236*/
  430 format(/45h any neutrons with energy greater than emax =,1pe12.5,         !/*gt        237*/
     & 55h from the source or from a collision will be resampled.)              !/*gt        238*/
      if((emx(1).lt.huge.or.ecf(1).ne.0.).and.mcal.eq.0)                        !/*gt        239*/
     & write(iuo,440)ecf(1),emx(1)                                              !/*gt        240*/
  440 format(/46h neutron cross sections outside the range from,                !/*gt        241*/
     & 1pe11.4,3h to,e11.4,18h mev are expunged.)                               !/*gt        242*/
!                                                                               !/*gt        243*/
!        print warnings.                                                        !/*gt        244*/
      if(nc.ne.0)call erprnt(1,2,1,nc,0,0,0,0,                                  !/*gt        245*/
     & 'i2,54h cross-section tables have poor gamma-production data.')          !/*gt        246*/
      if(max(zero+1.0001e-11,ecf(1)).lt.em)call erprnt(1,2,0,0,0,0,0,0,         !/*gt4b        5*/
     & '57hneutron energy cutoff is below some cross-section tables.')          !/*gt        248*/
      if(ih.ne.0)call erprnt(1,2,1,ih,0,0,0,0,                                  !/*gt4c       50*/
     & 'i4,55h cross sections modified by free gas thermal treatment.')         !/*gt4c       51*/
      if(kpt(2).ne.0.and.et.ne.huge)emx(2)=exp(et)                              !/*gt4a       22*/
      if(kpt(3).ne.0.or.(kpt(2).ne.0.and.ides.eq.0))                            !/*gt4a       23*/
     & emx(2)=min(emx(2),emx(3))                                                !/*gt4a       24*/
      if(abs(emx(2)-emx(3)).lt..00001*emx(3))emx(2)=emx(3)                      !/*gt4a       25*/
      if(emx(2).lt.emx(3))write(iuo,442)emx(2),hp                               !/*gt4a       26*/
  442 format(/29h maximum photon energy set to,f7.1,                            !/*gt4a       27*/
     & 26h mev (top energy of zaid =,a10,1h))                                   !/*gt4a       28*/
      if(emx(2).eq.emx(3))write(iuo,444)emx(2)                                  !/*gt4a       29*/
  444 format(/29h maximum photon energy set to,f7.1,                            !/*gt4a       30*/
     & 30h mev (maximum electron energy))                                       !/*gt4a       31*/
      if(emcf(2).ge.100..and.emcf(2).lt..9999*emx(2).and.kpt(2).ne.0)           !/*gt4a       32*/
     & call erprnt(1,2,0,0,0,0,0,0,                                             !/*gt4a       33*/
     & '47hsimple physics turned on for photons > 100 mev.')                    !/*gt4a       34*/
      if(kpt(3).ne.0.and.emx(3).gt.emx(2))call erprnt(1,1,0,0,0,0,0,0,          !/*gt4a       35*/
     & '59hmaximum electron energy > some photon cross-section tables.')        !/*gt        250*/
      do 450 i=1,ntal                                                           !/*gt        251*/
  450 if(ktp(lktp+2,i).ne.0)go to 460                                           !/*gt        252*/
      return                                                                    !/*gt        253*/
  460 if(kf*kpt(2).ne.0)call erprnt(1,2,0,0,0,0,0,0,                            !/*gt4c       52*/
     & '49hcannot generate delayed gamma-rays from fissions.')                  !/*gt4c       53*/
      return                                                                    !/*gt        257*/
      end                                                                       !/*gt        258*/

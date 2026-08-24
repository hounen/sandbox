!#ident issrc.F from deck us                                                    !/*us          1*/
      subroutine issrc                                                          !/*us          2*/
!        set up to read a surface source.                                       !/*us          3*/
      use global_data                                                           !/*usx         1*/
      use imcn_data                                                             !/*usx         2*/
!                                                                               !/*usx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*usx         4*/
      implicit integer(kindi) (i-n)                                             !/*usx         5*/
!                                                                               !/*usx         6*/
      character hq*80,hf*9                                                      !/*us4c2       1*/
!                                                                               !/*us          6*/
!        replace problem names of surfaces by program names.                    !/*us          7*/
      nx=1                                                                      !/*us4c        1*/
      do 10 j=1,njsx(1)                                                         !/*us4c        2*/
      i=iss(liss+1,j)                                                           !/*us4c        3*/
      k=namchg(2,i)                                                             !/*us4c2       2*/
      if(k.ne.0)k=k+iss(liss+2,j)                                               !/*us4c2       3*/
   10 iss(liss+1,j)=k                                                           !/*us4c2       4*/
!                                                                               !/*us         11*/
!        check for inconsistencies in the input.                                !/*us         12*/
      do 20 i=1,mipt                                                            !/*us         13*/
      if(ndet(i).eq.0.and.ndx(i).eq.0)go to 20                                  !/*us         14*/
      if(srv(1,18).lt.0.)call erprnt(1,1,0,0,0,0,0,0,                           !/*us         15*/
     & '41hnegative exponent for surface-source psc.')                          !/*us         16*/
      if(idefv(18).eq.0)call erprnt(1,1,0,0,0,0,0,0,                            !/*us         17*/
     & '43hexponent for surface-source psc is missing.')                        !/*us         18*/
   20 continue                                                                  !/*us         19*/
      if(njsx(1).ne.imt*njsr(1).and.njsx(1).ne.njsr(1))                         !/*usxc        1*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*usxc        2*/
     & '54hold and new surface no.s not compatable for s.s. read.')             !/*us         23*/
      if(imt.gt.1.and.ivdis(5).eq.0)call erprnt(1,1,0,0,0,0,0,0,                !/*us         24*/
     & '48hsurface source imt>1 requires a tr distribution.')                   !/*us         25*/
      do 50 j=1,imt                                                             !/*us         26*/
      jt=srv(1,5)                                                               !/*us         27*/
      if(imt.gt.1)jt=spf(ksd(lksd+13,ivdis(5))+1,j)                             !/*us         28*/
      if(jt.eq.0)go to 50                                                       !/*us         29*/
      do 30 i=1,mxtr                                                            !/*us         30*/
   30 if(abs(trf(ltrf+1,i)).eq.jt)go to 40                                      !/*us         31*/
      call erprnt(1,1,1,jt,0,0,0,0,                                             !/*us         32*/
     & '14htransformation,i4,35h for the surface source is missing.')           !/*us         33*/
   40 if(imt.eq.1)srv(1,5)=i                                                    !/*us         34*/
      if(imt.gt.1)spf(ksd(lksd+13,ivdis(5))+1,j)=i                              !/*us         35*/
   50 continue                                                                  !/*us         36*/
!                                                                               !/*usx         7*/
      if (imt.le.1) go to 51                                                    !/*usx         8*/
      ii=ksd(lksd+5,ivdis(5))+ksd(lksd+6,ivdis(5))+ksd(lksd+9,                  !/*usx         9*/
     & ivdis(5))+ksd(lksd+11,ivdis(5))                                          !/*usx        10*/
      if (ii.eq.1) go to 51                                                     !/*usx        11*/
      call erprnt(1,1,0,0,0,0,0,0,                                              !/*usx        12*/
     & '39hwrong SI option for SSR transformation.')                            !/*usx        13*/
   51 continue                                                                  !/*usx        14*/
!                                                                               !/*usx        15*/
      if(idefv(6).eq.0)srv(1,6)=-1.                                             !/*us         37*/
!                                                                               !/*us         38*/
!        print header information from the rssa file.                           !/*us         39*/
      if(mct.ge.0)write(iuo,70)rssa,kods,vers,probs,knods,aids                  !/*us4a        1*/
   70 format(//52h information from the header of surface-source file ,         !/*us         42*/
     & a8//25h the file was written by ,a8,9h version ,a5,                      !/*us         43*/
     & 15h with probid = ,a19,26h and ending with dump no. ,i5/                 !/*us4a        2*/
     & 34h the title of the creation run was,5x,a80)                            !/*us         45*/
!                                                                               !/*us         46*/
!        write summary totals.                                                  !/*us         47*/
      write(iuo,80)np1,nrss,niss                                                !/*us         48*/
   80 format(37h the original number of histories was,i8/                       !/*us         49*/
     & 40h the total number of tracks recorded was,i8,                          !/*us         50*/
     & 5h from,i7,23h independent histories.)                                   !/*us         51*/
!                                                                               !/*us         52*/
!        print surface information.                                             !/*us         53*/
      if(ink(10).ne.0.and.njsw.ne.0)write(iuo,90)                               !/*us4c        7*/
   90 format(/22h creation-run surfaces/7x,7hsurface,6x,4htype,6x,              !/*us         55*/
     & 12hcoefficients)                                                         !/*us         56*/
  100 do 110 i=1,njsw+niwr                                                      !/*us         57*/
      if(kjaq.eq.0.or.i.gt.njsw)read(iusr,end=320)                              !/*us4c2       5*/
     & jasw(ljaw+i),ks,n,(tpp(j),j=1,n)                                         !/*us4c2       6*/
      if(kjaq.eq.1.and.i.le.njsw)read(iusr,end=320)                             !/*us4c2       7*/
     & jasw(ljaw+i),jasq(ljaq+i),ks,n,(tpp(j),j=1,n)                            !/*us4c2       8*/
      if(ink(10).eq.0.or.i.gt.njsw.or.ks.eq.0)go to 110                         !/*us4c2       9*/
      if(kjaq.eq.0)write(iuo,115)i,jasw(ljaw+i),ksf(ks),(tpp(j),j=1,n)          !/*us4c2      10*/
      if(kjaq.eq.1)write(iuo,120)i,jasw(ljaw+i),jasq(ljaq+i),                   !/*us4c2      11*/
     & ksf(ks),(tpp(j),j=1,n)                                                   !/*us4c2      12*/
  110 continue                                                                  !/*us4c2      13*/
  115 format(i6,i7,7x,a3,7x,1p5e13.5:/(29x,5e13.5))                             !/*us4c2      14*/
  120 format(i6,i7,1h.,i1,5x,a3,7x,1p5e13.5:/(29x,5e13.5))                      !/*us4c2      15*/
      if(ink(10).ne.0.and.niwr.ne.0)                                            !/*us         62*/
     & write(iuo,130)(jasw(ljaw+njsw+i),i=1,niwr)                               !/*us         63*/
  130 format(/20h creation-run cells:,16i6:/(20x,16i6))                         !/*us         64*/
      if(nrcd.eq.6.or.nrcd.eq.7)rssp=sqrt(tpp(n))                               !/*us4c        8*/
!                                                                               !/*us         66*/
!        read and print the surface-source summary information.                 !/*us         67*/
      read(iusr,end=320)a,                                                      !/*us         68*/
     & ((nslr(lnsr+i,j),i=1,2+4*mipts),j=1,njsw+niwr)                           !/*us         69*/
      if(ink(10).eq.0)go to 170                                                 !/*us         70*/
      write(iuo,140)(i,nslr(lnsr+1,i),nslr(lnsr+2,i),i=1,njsw+niwr)             !/*us         71*/
  140 format(/47h summary for all particles per surface or cell./               !/*us         72*/
     & 5x,3hno.,16x,12htotal tracks,11x,21hindependent histories/               !/*us         73*/
     & (i7,2i24))                                                               !/*us         74*/
      do 150 k=1,mipts                                                          !/*us         75*/
  150 if(kpt(k).ne.0)write(iuo,160)hnp(k),                                      !/*us         76*/
     & (i,(nslr(lnsr+j,i),j=4*k-1,4*k+2),i=1,njsw+niwr)                         !/*us         77*/
  160 format(/1x,a11,29h summary per surface or cell./5x,3hno.,8x,              !/*usx        16*/
     & 6htracks,7x,12hindp. tracks,5x,10huncollided,5x,                         !/*us         79*/
     & 12hindp. uncol./(i7,4i15))                                               !/*us         80*/
!                                                                               !/*us         81*/
!        read information for the first track into the ssb array.               !/*us         82*/
  170 read(iusr,end=320)(ssb(i),i=1,nrcd)                                       !/*us         83*/
      nqss=dbcn(8)                                                              !/*us         84*/
      do 190 n=1,nqss-1                                                         !/*us         85*/
      a=ssb(1)                                                                  !/*us         86*/
  180 read(iusr,end=320)(ssb(i),i=1,nrcd)                                       !/*us         87*/
      nrrs=nrrs+1                                                               !/*us         88*/
  190 if(ssb(1).eq.a)go to 180                                                  !/*us         89*/
!                                                                               !/*us         90*/
!        set up defaulted arrays.                                               !/*us         91*/
      snit=1.                                                                   !/*us         92*/
      if(npp.gt.0.and.npp.ne.np1)snit=npp/(np1+zero)                            !/*us         93*/
      npp=0                                                                     !/*us         94*/
      if(jasr(ljar+1,1).ne.0)go to 210                                          !/*us4c        9*/
      do 200 j=1,njsr(1)+nilr(1)                                                !/*us4c       10*/
      jasr(ljar+2,j)=0                                                          !/*us4c2      16*/
      if(kjaq.ne.0)jasr(ljar+2,j)=jasq(ljaq+j)                                  !/*us4c2      17*/
  200 jasr(ljar+1,j)=abs(jasw(ljaw+j))                                          !/*us4c       11*/
  210 do 220 j=1,njsx(1)                                                        !/*us4c       12*/
      if(iss(liss+1,j).ne.0)go to 220                                           !/*us4c2      18*/
      iss(liss+1,j)=namchg(2,jasr(ljar+1,j))                                    !/*us4c2      19*/
      if(iss(liss+1,j).gt.0)iss(liss+1,j)=iss(liss+1,j)+jasr(ljar+2,j)          !/*us4c2      20*/
      if(iss(liss+1,j).gt.0)iss(liss+2,j)=jasr(ljar+2,j)                        !/*us4c2      21*/
  220 if(iss(liss+1,j).eq.0)call erprnt(1,1,1,jasr(ljar+1,j),0,0,0,0,           !/*us4c       14*/
     & '7hsurface,i7,34h of surface source is not defined.')                    !/*us        101*/
!                                                                               !/*us        102*/
!        set up for the cylindrical-window bias feature.                        !/*us        103*/
      if(nrcd.ne.6.and.nrcd.ne.7)go to 225                                      !/*us4c       15*/
      jx=iss(liss+nx,1)                                                         !/*us4c       16*/
      kt=abs(kst(lkst+jx))                                                      !/*us4c       17*/
      if(kt.lt.5.or.kt.gt.9)go to 224                                           !/*us4c       18*/
      i=2                                                                       !/*us4c       19*/
      if(kt.eq.5)i=1                                                            !/*us4c       20*/
      if(kt.eq.6)i=4                                                            !/*us4c       21*/
      ss=sqrt(scf(lsc(llsc+jx)+i))                                              !/*us4c       22*/
      if(abs(rssp-ss).gt.0.0001)call erprnt(1,2,0,0,0,0,0,0,                    !/*us4c       23*/
     & '36hss sphere radius not as was written.')                               !/*us4c       24*/
      rssp=ss                                                                   !/*us4c       25*/
      go to 225                                                                 !/*us4c       26*/
  224 call erprnt(1,1,1,nsf(lnsf+jx),0,0,0,0,                                   !/*us4c       27*/
     & '7hsurface,i5,35h is a '//ksf(kt)//' surface and not a sphere.')         !/*us4c       28*/
  225 if(idefv(10).eq.0)go to 260                                               !/*us4c       29*/
      r=srv(1,10)*(srv(3,10)+srv(2,10))/sqrt((srv(3,10)-srv(2,10))**2+          !/*us        105*/
     & 4.*srv(1,10)**2)                                                         !/*us        106*/
      if(r.lt.rssp.and.srv(1,10).lt.rssp)go to 230                              !/*us        107*/
      srv(1,6)=0.                                                               !/*us        108*/
      bcw(2,3)=.5*pie                                                           !/*us        109*/
      if(srv(1,10).lt.rssp)bcw(2,3)=asin(srv(1,10)/rssp)                        !/*us        110*/
      go to 240                                                                 !/*us        111*/
  230 srv(1,6)=cos(asin(r/rssp))                                                !/*us        112*/
      bcw(2,3)=asin(srv(1,10)/rssp)                                             !/*us        113*/
  240 bcw(1,3)=atan(srv(1,10)/(srv(3,10)-rssp))                                 !/*us        114*/
      do 250 i=1,2                                                              !/*us        115*/
      bcw(i,1)=sqrt(srv(4-i,10)**2+srv(1,10)**2)                                !/*us        116*/
  250 bcw(i,2)=atan(srv(1,10)/srv(4-i,10))                                      !/*us        117*/
!                                                                               !/*us        118*/
!        print the read options.                                                !/*us        119*/
  260 if(ink(10).eq.0)return                                                    !/*us        120*/
      write(iuo,270)int(srv(1,4))                                               !/*us        121*/
  270 format(/35h this problem uses collision option,i3,                        !/*us        122*/
     & 15h and reads from)                                                      !/*us        123*/
!                                                                               !/*us4c2      22*/
!        write surfaces if no facets.                                           !/*us4c2      23*/
      if(kjaq.eq.0.and.njsr(1).gt.0)                                            !/*us4c2      24*/
     & write(iuo,275)(jasr(ljar+1,i),i=1,njsr(1))                               !/*us4c2      25*/
  275 format(5x,10hsurface(s),14i7:/(15x,14i7))                                 !/*us4c2      26*/
!                                                                               !/*us4c2      27*/
!        write surfaces if facets present.                                      !/*us4c2      28*/
      if(kjaq.eq.0.and.njsr(1).gt.0)go to 282                                   !/*us4c2      29*/
      do 280 i=1,njsr(1)                                                        !/*us4c2      30*/
      n=mod(i-1,8)+1                                                            !/*us4c2      31*/
      if(n.eq.1)hq=' '                                                          !/*us4c2      32*/
      hf=' '                                                                    !/*us4c2      33*/
      if(jasr(ljar+2,i).eq.0)write(hf,'(i7,2h  )')jasr(ljar+1,i)                !/*us4c2      34*/
      if(jasr(ljar+2,i).ne.0)write(hf,'(i7,1h.,i1)')jasr(ljar+1,i),             !/*us4c2      35*/
     & jasr(ljar+2,i)                                                           !/*us4c2      36*/
      hq(1+(n-1)*9:n*9)=hf(1:9)                                                 !/*us4c2      37*/
      if(i.le.8.and.(n.eq.8.or.i.eq.njsr(1)))write(iuo,278)hq                   !/*us4c2      38*/
  278 format(5x,10hsurface(s),a80)                                              !/*us4c2      39*/
      if(i.gt.8.and.(n.eq.8.or.i.eq.njsr(1)))write(iuo,'(15x,a80)')hq           !/*us4c2      40*/
  280 continue                                                                  !/*us4c2      41*/
!                                                                               !/*us4c2      42*/
!        write fission volume source cells.                                     !/*us4c2      43*/
  282 if(nilr(1).ne.0)write(iuo,290)                                            !/*us4c2      44*/
     & (jasr(ljar+1,i),i=njsr(1)+1,njsr(1)+nilr(1))                             !/*us4c       32*/
  290 format(5x,7hcell(s),14i7:/(7x,14i7))                                      !/*us        127*/
      if(nrcd.eq.10)write(iuo,300)imt                                           !/*us        128*/
  300 format(31h this surface source will occur,i4,7h times.)                   !/*us        129*/
      if(nrcd.eq.6)write(iuo,310)imt                                            !/*us        130*/
  310 format(41h this spherical surface source will occur,i4,8h time(s))        !/*us        131*/
      return                                                                    !/*us        132*/
!                                                                               !/*us        133*/
!        quit if unexpected eof is encountered.                                 !/*us        134*/
  320 call expire(0,'issrc','unexpected eof on file '//rssa)                    !/*us        135*/
      end                                                                       !/*us        136*/

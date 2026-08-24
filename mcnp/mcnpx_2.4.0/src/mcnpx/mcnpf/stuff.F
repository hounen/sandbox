!#ident stuff.F from deck st                                                    !/*st          1*/
      subroutine stuff                                                          !/*st          2*/
!        set up the materials required for the problem.                         !/*st          3*/
      use global_data                                                           !/*stx         1*/
      use imcn_data                                                             !/*stx         2*/
!                                                                               !/*stx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*stx         4*/
      implicit integer(kindi) (i-n)                                             !/*stx         5*/
!                                                                               !/*stx         6*/
      character hs*10,ht*10,hb*30,hc*181                                        !/*st4c        1*/
!                                                                               !/*st          7*/
!        change mat from material names to material indexes.                    !/*st          8*/
#ifdef PCDOS                                                                    /*!st4a        1*/
      if(lockl)call pttyin                                                      !/*st4a        2*/
#endif                                                                          /*!cpp       118*/
      if(irup.ne.0)call qttyin(0,'30hbeginning material processing.')           !/*st4a        3*/
      do 30 i=1,mxa                                                             !/*st          9*/
      do 10 j=1,nmat                                                            !/*st         10*/
   10 if(nmt(lnmt+j).eq.mat(lmat+i))go to 20                                    !/*st         11*/
      mat(lmat+i)=0                                                             !/*st         12*/
      go to 30                                                                  !/*st         13*/
   20 mat(lmat+i)=j                                                             !/*st         14*/
   30 continue                                                                  !/*st         15*/
!                                                                               !/*st         16*/
!        set up bremsstrahlung-biasing material flags.                          !/*st         17*/
      do 70 m=1,nmat*kpt(3)                                                     !/*st         18*/
      if(mbi(lmbi+m).ge.0)go to 70                                              !/*st         19*/
      n=-mbi(lmbi+m)                                                            !/*st         20*/
      mbi(lmbi+m)=0                                                             !/*st         21*/
   40 do 50 i=1,nmat                                                            !/*st         22*/
   50 if(nmt(lnmt+i).eq.n)go to 60                                              !/*st         23*/
      go to 70                                                                  !/*st         24*/
   60 n=-mbi(lmbi+i)                                                            !/*st         25*/
      mbi(lmbi+i)=1                                                             !/*st         26*/
      if(n.gt.0)go to 40                                                        !/*st         27*/
   70 continue                                                                  !/*st         28*/
!                                                                               !/*st         49*/
!        set up the list of cross-section tables needed by the problem.         !/*st         50*/
      mn=1                                                                      !/*st         51*/
      do 240 km=1,mix                                                           !/*st         52*/
      if(km.ge.jmd(ljmd+mn+1))mn=mn+1                                           !/*st         53*/
      do 230 m=1,mipt                                                           !/*st         57*/
      if(kpt(m).eq.0.and.(m.ne.3.or.ides.ne.0.or.kpt(2).eq.0))go to 230         !/*st         58*/
      do 120 i=1,mxa                                                            !/*st         59*/
  120 if(mat(lmat+i).eq.mn.and.(fim(lfim+m,i).ne.0..or.m.eq.3.and.              !/*st         60*/
     & kpt(3).eq.0.and.fim(lfim+2,i).ne.0))go to 150                            !/*st         61*/
      do 140 i=1,nmfm,2                                                         !/*st         62*/
      if(mfm(lmfm+i).ne.nmt(lnmt+mn))go to 140                                  !/*st         63*/
      do 130 j=1,ntal                                                           !/*st         64*/
  130 if(jptal(ljpt+1,j).eq.mfm(lmfm+i+1).and.ktp(lktp+m,j).ne.0)               !/*st         65*/
     & go to 150                                                                !/*st         66*/
      do 135 j=1,npert                                                          !/*st4b        1*/
  135 if(iptb(lipb+1,j).eq.-mfm(lmfm+i+1).and.kpptb(lpptb+m,j).ne.0)            !/*stx         7*/
     & go to 150                                                                !/*stx         8*/
  140 continue                                                                  !/*st         67*/
      go to 230                                                                 !/*st         68*/
!                                                                               !/*st4a        6*/
!        identify this table, beginning with the zaid request.                  !/*st4a        7*/
  150 write(ht(1:7),'(i6,1h.)')iza(liza+km)                                     !/*stx         9*/
      l=kmm(lkmm+km)                                                            !/*stx        10*/
      ht(8:10)=char(mod(l,256))//char(mod(l/256,256))//char(l/65536)            !/*st4a        9*/
      if(m.eq.9)go to 192                                                       !/*stx        11*/
      if(m.gt.3)go to 230                                                       !/*stx        12*/
      go to(160,180,190)m                                                       !/*st4a       10*/
!                                                                               !/*st         70*/
!        neutron table.                                                         !/*st         71*/
  160 if(ht(8:10).ne.' '.and.index('pgue',ht(10:10)).eq.0)go to 168             !/*stxc        1*/
!        if wrong particle, or nothing was specified, use a default.            !/*st4a       12*/
      l=lxd(llxd+1,mn)                                                          !/*st4a       13*/
      ht(8:10)=char(mod(l,256))//char(mod(l/256,256))//char(l/65536)            !/*st4a       14*/
  168 if(mcal.ne.0)ht(10:10)='m'                                                !/*st4a       36*/
      if(mcal.ne.0.or.ht(10:10).eq.'y')go to 200                                !/*st         73*/
      if(ht(10:10).ne.'d')ht(10:10)='c'                                         !/*st4c2       1*/
      if(kdr(lkdr+1).lt.0)ht(10:10)='d'                                         !/*st         75*/
      do 170 i=1,mxe1                                                           !/*st         76*/
      if(kdr(lkdr+i).le.0)go to 200                                             !/*st         77*/
  170 if(kdr(lkdr+i).eq.iza(liza+km))ht(10:10)='d'                              !/*st         78*/
      go to 200                                                                 !/*st         79*/
!                                                                               !/*st         80*/
!        photoatomic table.                                                     !/*st4c2       2*/
  180 ht(4:6)='000'                                                             !/*st4a       38*/
      if(index('pg',ht(10:10)).ne.0)go to 185                                   !/*st4a       39*/
!        if not explicitly photons, use a photon default.                       !/*st4a       40*/
      l=lxd(llxd+2,mn)                                                          !/*st4a       41*/
      ht(8:10)=char(mod(l,256))//char(mod(l/256,256))//char(l/65536)            !/*st4a       42*/
  185 if(mcal.ne.0)ht(10:10)='g'                                                !/*st4a       43*/
      if(mcal.eq.0)ht(10:10)='p'                                                !/*st4a       44*/
      go to 200                                                                 !/*st         84*/
!                                                                               !/*st         85*/
!        electron table.                                                        !/*st         86*/
  190 ht(4:6)='000'                                                             !/*st4a       45*/
      if(ht(10:10).eq.'e')go to 200                                             !/*st4a       46*/
!        if not explicitly electrons, use an electron default.                  !/*st4a       47*/
      l=lxd(llxd+3,mn)                                                          !/*st4a       48*/
      ht(8:10)=char(mod(l,256))//char(mod(l/256,256))//char(l/65536)            !/*st4a       49*/
      go to 200                                                                 !/*stx        13*/
!                                                                               !/*stx        14*/
!        proton table.                                                          !/*stx        15*/
  192 if(cut_h.le.0.)go to 230                                                  !/*stx        16*/
      if(ht(10:10).eq.'h')go to 200                                             !/*stx        17*/
!        if not explicitly protons, use a proton default.                       !/*stx        18*/
      l=lxd(llxd+9,mn)                                                          !/*stx        19*/
      ht(8:10)=char(mod(l,256))//char(mod(l/256,256))//char(l/65536)            !/*stx        20*/
      if(mcal.eq.0)ht(10:10)='h'                                                !/*stx        21*/
!                                                                               !/*st         88*/
!        add the table to the list if it is not already there.                  !/*st         89*/
  200 do 210 i=1,mxe                                                            !/*st         90*/
      call zaid(2,hs,ixl(lixl+1,i))                                             !/*st         91*/
  210 if(hs.eq.ht)go to 220                                                     !/*st         92*/
      mxe=mxe+1                                                                 !/*st         93*/
      if(mxe.gt.mxe1)call erprnt(1,1,0,0,0,0,0,0,                               !/*st4b        4*/
     & '33hmaterial (mxe) overflow in stuff.')                                  !/*st4b        5*/
      call zaid(1,ht,ixl(lixl+1,i))                                             !/*st         94*/
  220 lme(llme+m,km)=i                                                          !/*st         95*/
  230 continue                                                                  !/*st         96*/
  240 continue                                                                  !/*st         97*/
!                                                                               !/*st4c2       3*/
!        add the photonuclear table names.                                      !/*st4c2       4*/
      if(ispn.eq.0)go to 248                                                    !/*st4c2       5*/
      mn=1                                                                      !/*st4c2       6*/
      do 247 km=1,mix                                                           !/*st4c2       7*/
      if(km.ge.jmd(ljmd+mn+1))mn=mn+1                                           !/*st4c2       8*/
      if(izn(lizn+km).eq.0)go to 247                                            !/*st4c2       9*/
      write(ht(1:7),'(i6,1h.)')izn(lizn+km)                                     !/*st4c2      10*/
      l=kmm(lkmm+km)                                                            !/*st4c2      11*/
      ht(8:10)=char(mod(l,256))//char(mod(l/256,256))//char(l/65536)            !/*st4c2      12*/
      if(index('u',ht(10:10)).ne.0)go to 242                                    !/*st4c2      13*/
!        if not explicitly a photonuclear table, use a default.                 !/*st4c2      14*/
      l=lxd(llxd+4,mn)                                                          !/*st4c2      15*/
      ht(8:10)=char(mod(l,256))//char(mod(l/256,256))//char(l/65536)            !/*st4c2      16*/
!                                                                               !/*st4c2      17*/
!        add the table to the list if it is not already there.                  !/*st4c2      18*/
  242 do 244 i=1,mxe                                                            !/*st4c2      19*/
      call zaid(2,hs,ixl(lixl+1,i))                                             !/*st4c2      20*/
  244 if(hs.eq.ht)go to 246                                                     !/*st4c2      21*/
      mxe=mxe+1                                                                 !/*st4c2      22*/
      i=mxe                                                                     !/*st4c2      23*/
      if(mxe.gt.mxe1)call erprnt(1,1,0,0,0,0,0,0,                               !/*st4c2      24*/
     & '33hmaterial (mxe) overflow in stuff.')                                  !/*st4c2      25*/
      call zaid(1,ht,ixl(lixl+1,i))                                             !/*st4c2      26*/
  246 lmn(llmn+km)=i                                                            !/*st4c2      27*/
  247 continue                                                                  !/*st4c2      28*/
!                                                                               !/*st         98*/
!        add the thermal s(a,b) table names.                                    !/*st         99*/
  248 if(mcal.ne.0)go to 262                                                    !/*st4c2      29*/
      do 260 km=1,indt                                                          !/*st        100*/
      call zaid(2,ht,kmt(lkmt+1,km))                                            !/*st        101*/
      ht(10:10)='t'                                                             !/*st        102*/
      do 250 i=1,mxe                                                            !/*st        103*/
      call zaid(2,hs,ixl(lixl+1,i))                                             !/*st        104*/
      if(hs.eq.ht)go to 260                                                     !/*st4a       50*/
      if(hs(1:6).ne.ht(1:6))go to 250                                           !/*st4a       51*/
      if(hs(8:9).ne.' '.and.ht(8:9).ne.' ')go to 250                            !/*st4a       52*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*st4a       53*/
     & '55h'//ht//' and '//hs//' are both present on mt cards.')                !/*st4a       54*/
      if(hs(8:9).eq.' ')call zaid(1,ht,ixl(lixl+1,i))                           !/*st4a       55*/
      go to 260                                                                 !/*st4a       56*/
  250 continue                                                                  !/*st4a       57*/
      mxe=mxe+1                                                                 !/*st        106*/
      if(mxe.gt.mxe1)call erprnt(1,1,0,0,0,0,0,0,                               !/*st4b        7*/
     & '33hmaterial (mxe) overflow in stuff.')                                  !/*st4b        8*/
      call zaid(1,ht,ixl(lixl+1,mxe))                                           !/*st        107*/
  260 continue                                                                  !/*st        108*/
!                                                                               !/*st        109*/
!        put the list of cross-section tables in ascending order, for           !/*st        110*/
!        faster access to cross-section files in some circumstances.            !/*st        111*/
  262 if(mxe.eq.0)call erprnt(1,2,0,0,0,0,0,0,                                  !/*st4b        9*/
     & '55hno cross-section tables are called for in this problem.')            !/*st        113*/
      if(mxe.eq.0)go to 325                                                     !/*stx        22*/
      do 310 ie=1,mxe                                                           !/*st        115*/
      do 300 je=ie+1,mxe                                                        !/*st        116*/
      call zaid(2,hs,ixl(lixl+1,ie))                                            !/*st        117*/
      mt=index(htn,hs(10:10))                                                   !/*st4c2      30*/
      call zaid(2,ht,ixl(lixl+1,je))                                            !/*st        119*/
      nt=index(htn,ht(10:10))                                                   !/*st4c2      31*/
      if(mt.lt.nt)go to 300                                                     !/*st        121*/
      if(mt.gt.nt)go to 270                                                     !/*st        122*/
      if(hs.le.ht)go to 300                                                     !/*st        123*/
  270 do 280 i=1,3                                                              !/*st        124*/
      l=ixl(lixl+i,ie)                                                          !/*st        125*/
      ixl(lixl+i,ie)=ixl(lixl+i,je)                                             !/*st        126*/
  280 ixl(lixl+i,je)=l                                                          !/*st        127*/
      do 290 i=1,mipt                                                           !/*st        128*/
      do 290 j=1,mix                                                            !/*st        129*/
      l=lme(llme+i,j)                                                           !/*st        130*/
      if(l.eq.ie)lme(llme+i,j)=je                                               !/*st        131*/
  290 if(l.eq.je)lme(llme+i,j)=ie                                               !/*st        132*/
      if(ispn.eq.0)go to 300                                                    !/*st4c2      32*/
      do 295 i=1,mix                                                            !/*st4c2      33*/
      l=lmn(llmn+i)                                                             !/*st4c2      34*/
      if(l.eq.ie)lmn(llmn+i)=je                                                 !/*st4c2      35*/
  295 if(l.eq.je)lmn(llmn+i)=ie                                                 !/*st4c2      36*/
  300 continue                                                                  !/*st        133*/
  310 continue                                                                  !/*st        134*/
!                                                                               !/*st        135*/
!        check for near duplicates.                                             !/*st        136*/
      do 320 i=1,mxe-1                                                          !/*st        137*/
      call zaid(2,hs,ixl(lixl+1,i))                                             !/*st        138*/
      call zaid(2,ht,ixl(lixl+1,i+1))                                           !/*st        139*/
  320 if(hs(1:6).eq.ht(1:6).and.hs(10:10).eq.ht(10:10))                         !/*st        140*/
     & call erprnt(1,2,0,0,0,0,0,0,                                             !/*st        141*/
     & '46h'//hs//' and '//ht//' are both called for.')                         !/*cor4-1     94*/
!                                                                               !/*st        143*/
!        put cross-section directory file information in ixc.                   !/*st        144*/
  325 call ixsdir(i)                                                            !/*stx        23*/
      if(i.eq.1)return                                                          !/*st        146*/
!                                                                               !/*st        147*/
!        read the neutron atomic weights and the temperatures.                  !/*st        148*/
      do 350 i=1,mxe                                                            !/*st        149*/
      hc=' '                                                                    !/*st        150*/
      call zaid(2,hc,ixc(lixc+1,i))                                             !/*st        151*/
      do 330 j=11,181                                                           !/*st        152*/
  330 hc(j:j)=char(mod(ixc(lixc+(j+1)/3,i)/256**mod(j+1,3),256))                !/*st        153*/
      call nxtsym(hc,' ',11,it,iu,1)                                            !/*st4a       58*/
      if(iu.eq.0)go to 350                                                      !/*st        155*/
      hb=hc(it:iu)                                                              !/*st        156*/
      read(hb,'(bn,e30.0)')awn(lawn+i)                                          !/*st        157*/
      if(nty(lnty+i).gt.2)go to 350                                             !/*st        158*/
      do 340 j=3,10                                                             !/*st4c        2*/
      call nxtsym(hc,' ',iu+1,it,iu,1)                                          !/*st4a       59*/
  340 if(iu.eq.0)go to 350                                                      !/*st        161*/
      hb=hc(it:iu)                                                              !/*st        162*/
      read(hb,'(bn,e30.0)')tbt(ltbt+i)                                          !/*st        163*/
!                                                                               !/*st4c        3*/
!        look for unresolved resonance probability tables.                      !/*st4c        4*/
      do 345 j=11,15                                                            !/*st4c        5*/
      call nxtsym(hc,' ',iu+1,it,iu,1)                                          !/*st4c        6*/
      if(iu.eq.0)go to 350                                                      !/*st4c        7*/
  345 if(hc(it:iu).eq.'ptable')iunr=iunr+1                                      !/*st4c        8*/
  350 continue                                                                  !/*st        164*/
      if(iunr.ge.100000)iunr=0                                                  !/*st4c        9*/
!                                                                               !/*st4a       60*/
!        atomic weights from awtab card always override table values.           !/*st        166*/
      do 360 j=1,naw                                                            !/*st        167*/
      do 360 km=1,mix                                                           !/*st        168*/
      if(iza(liza+km).ne.kaw(lkaw+j))go to 360                                  !/*st        169*/
      awc(lawc+km)=awt(lawt+j)                                                  !/*st        170*/
      if(lme(llme+1,km).ne.0)awn(lawn+lme(llme+1,km))=awt(lawt+j)               !/*st        171*/
  360 continue                                                                  !/*st        172*/
!                                                                               !/*st        173*/
!        calculate the thermal g-function table.                                !/*st        174*/
      b=1./sqrt(pie)                                                            !/*st        175*/
      thgf(0)=2.*b                                                              !/*st        176*/
      do 370 i=1,50                                                             !/*st        177*/
      a=i*.04                                                                   !/*st        178*/
  370 thgf(i)=(a+.5/a)*erf2(a)+b*exp(-a**2)                                     !/*st        179*/
!                                                                               !/*st        180*/
!        set pointers for the summary of nuclide activity.                      !/*st        181*/
      ipan(lipa+1)=1                                                            !/*st        182*/
      do 380 i=1,mxa                                                            !/*st        183*/
  380 ipan(lipa+i+1)=ipan(lipa+i)+npq(lnpq+mat(lmat+i))                         !/*st        184*/
!                                                                               !/*st        185*/
!        convert mass densities to atom densities.                              !/*st        186*/
!        modify the problem to run with all voids if required.                  !/*st        187*/
      call rhoden                                                               !/*st        188*/
      if(ioid.ne.0)call voidcd                                                  !/*st        189*/
      return                                                                    !/*st        190*/
      end                                                                       !/*st        191*/

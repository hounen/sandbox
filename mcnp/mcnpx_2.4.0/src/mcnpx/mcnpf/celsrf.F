!#ident celsrf.F from deck cs                                                   !/*cs          1*/
      subroutine celsrf                                                         !/*cs          2*/
!        finish up the cells and surfaces.                                      !/*cs          3*/
      use global_data                                                           !/*csx         1*/
      use imcn_data                                                             !/*csx         2*/
!                                                                               !/*csx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*csx         4*/
      implicit integer(kindi) (i-n)                                             !/*csx         5*/
!                                                                               !/*csx         6*/
      character hb(mipt)*11,hj(2,3)*10,hl*5,hp*130,hq*2,ht*4                    !/*csx         7*/
      dimension it(mipt)                                                        !/*csx         8*/
!                                                                               !/*cs          7*/
!        adjust the importances for default and printing.                       !/*cs          8*/
      do 20 mp=2,mipt                                                           !/*cs          9*/
      m=0                                                                       !/*cs         10*/
      do 10 ic=1,mxa                                                            !/*cs         11*/
      if(kpt(1).eq.0.or.kpt(mp).eq.0)go to 10                                   !/*cs         12*/
      if(fim(lfim+1,ic).eq.0..and.fim(lfim+mp,ic).ne.0.)m=m+1                   !/*cs         13*/
      if(fim(lfim+1,ic).ne.0..and.fim(lfim+mp,ic).eq.0..and.mp.eq.2)            !/*cs         14*/
     & gwt(lgwt+ic)=-1e6                                                        !/*cs         15*/
   10 if(kpt(1).eq.0.and.kpt(mp).ne.0.and.fim(lfim+mp,ic).ne.0.)                !/*csx         9*/
     & fim(lfim+1,ic)=fim(lfim+mp,ic)                                           !/*csx        10*/
   20 if(m.ne.0)call erprnt(1,1,1,m,0,0,0,0,                                    !/*cs4b        1*/
     & 'i5,47h cells have zero n. and non-zero '//hip(mp:mp)//                  !/*cs         18*/
     & '. importance.')                                                         !/*cs         19*/
      if(kpt(2).eq.0.or.kpt(3).eq.0)go to 40                                    !/*cs         20*/
      m=0                                                                       !/*cs         21*/
      do 30 ic=1,mxa                                                            !/*cs         22*/
   30 if(fim(lfim+2,ic).eq.0..neqv.fim(lfim+3,ic).eq.0.)m=m+1                   !/*cs         23*/
      if(m.ne.0)call erprnt(1,2,1,m,0,0,0,0,                                    !/*cs         24*/
     & '41hp. and e. importances are inconsistent in,i5,7h cells.')             !/*cs         25*/
!                                                                               !/*cs         26*/
!        bring cell-wise energy cutoffs up to the global cutoffs.               !/*cs         27*/
   40 do 50 ic=1,mxa                                                            !/*cs         28*/
      do 50 j=1,mipt                                                            !/*cs         29*/
   50 elp(lelp+j,ic)=max(ecf(j),elp(lelp+j,ic))                                 !/*cs         30*/
!                                                                               !/*cs         31*/
!        finish setting up dxtran cell probabilities.                           !/*cs4a        1*/
      do 120 i=1,mipt                                                           !/*cs         33*/
      if(ndx(i).eq.0)go to 120                                                  !/*cs         34*/
      if(dxw(i,1).eq.0..and.(wc1(i).eq.0..or.emcf(1).gt.0..or.                  !/*cs4c        2*/
     & emcf(2).lt.100.))call erprnt(1,2,0,0,0,0,0,0,                            !/*cs4c        3*/
     & '49hdxtran weight cutoffs needed with analog capture.')                  !/*cs4c        4*/
      do 110 ic=1,mxa                                                           !/*cs         35*/
      do 60 j=1,ndx(i)                                                          !/*cs         36*/
   60 if(dxcp(ldxp+j,i,ic).eq.huge)dxcp(ldxp+j,i,ic)=dxcp(ldxp,i,ic)            !/*cs         37*/
      do 70 j=0,ndx(i)                                                          !/*cs         38*/
   70 dxcp(ldxp+j,i,ic)=min(max(zero,dxcp(ldxp+j,i,ic)),one)                    !/*cs         39*/
      do 80 j=2,ndx(i)                                                          !/*cs         40*/
   80 if(dxcp(ldxp+j,i,ic).ne.dxcp(ldxp+1,i,ic))go to 100                       !/*cs         41*/
      dxcp(ldxp,i,ic)=dxcp(ldxp+1,i,ic)                                         !/*cs         42*/
      do 90 j=1,ndx(i)                                                          !/*cs         43*/
   90 dxcp(ldxp+j,i,ic)=1.                                                      !/*cs         44*/
      go to 110                                                                 !/*cs         45*/
  100 dxcp(ldxp,i,ic)=1.                                                        !/*cs         46*/
  110 continue                                                                  !/*cs         47*/
  120 continue                                                                  !/*cs         48*/
!                                                                               !/*cs         49*/
!        print the principal cell parameters.                                   !/*cs         50*/
      write(iuo,130)                                                            !/*cs         51*/
  130 format(6h1cells,98x,14hprint table 60/)                                   !/*cs         52*/
      hp=' '                                                                    !/*cs         53*/
      hp(23:26)='atom'                                                          !/*cs         54*/
      hp(35:38)='gram'                                                          !/*cs         55*/
      i=67                                                                      !/*cs         56*/
      if(novol.eq.0)i=74                                                        !/*csx        11*/
      np=0                                                                      !/*csx        12*/
      do 135 kp=1,mipt                                                          !/*csx        13*/
      if(ifip(kp).eq.0.and.kpt(kp).eq.0)go to 135                               !/*csx        14*/
      np=np+1                                                                   !/*csx        15*/
      it(np)=kp                                                                 !/*csx        16*/
      hb(np)=hnp(kp)                                                            !/*csx        17*/
  135 continue                                                                  !/*csx        18*/
      do 140 j=1,min(np,3)                                                      !/*csx        19*/
      hp(i+2:i+12)=hb(j)                                                        !/*csx        20*/
  140 i=i+12                                                                    !/*csx        21*/
      if(kpt(1).ne.0.and.kpt(2).ne.0)hp(i+3:i+11)='photon wt'                   !/*csx        22*/
      write(iuo,'(a121)')hp                                                     !/*csx        23*/
      hp='        cell  mat    density     density     volume'//                !/*cs         65*/
     & '       mass'                                                            !/*cs         66*/
      i=67                                                                      !/*cs         67*/
      if(novol.ne.0)go to 150                                                   !/*cs         68*/
      hp(i+1:i+6)='pieces'                                                      !/*cs         69*/
      i=74                                                                      !/*csx        24*/
  150 do 160 j=1,min(np,3)                                                      !/*csx        25*/
      hp(i+2:i+11)='importance'                                                 !/*csx        26*/
  160 i=i+12                                                                    !/*csx        27*/
      if(kpt(1).ne.0.and.kpt(2).ne.0)hp(i+2:i+11)='generation'                  !/*csx        28*/
      write(iuo,'(a121/)')hp                                                    !/*csx        29*/
      tv=0.                                                                     !/*cs         78*/
      tm=0.                                                                     !/*cs         79*/
      do 230 ic=1,mxa                                                           !/*cs         80*/
      hp=' '                                                                    !/*cs         81*/
      nx=0                                                                      !/*csx        30*/
      if(mat(lmat+ic).ne.0)nx=nmt(lnmt+mat(lmat+ic))                            !/*csx        31*/
      write(hp,170)ic,ncl(lncl+ic),nx,rho(lrho+ic),den(lden+ic),                !/*csx        32*/
     & vol(lvol+ic),den(lden+ic)*vol(lvol+ic)                                   !/*csx        33*/
  170 format(2i6,i5,1pe13.5,3e12.5)                                             !/*cs         84*/
      tv=tv+vol(lvol+ic)                                                        !/*cs         85*/
      tm=tm+den(lden+ic)*vol(lvol+ic)                                           !/*cs         86*/
      if(mat(lmat+ic).eq.0)go to 200                                            !/*csx        34*/
      do 180 i=1,indt                                                           !/*cs         87*/
  180 if(jmt(ljmt+i).eq.nmt(lnmt+mat(lmat+ic)))go to 190                        !/*cs         88*/
      go to 200                                                                 !/*cs         89*/
  190 hp(18:18)='s'                                                             !/*cs         90*/
  200 i=67                                                                      !/*cs         91*/
      if(novol.ne.0)go to 210                                                   !/*cs         92*/
      write(hp(i+1:i+6),'(i6)')int(vols(lvls+2,ic))                             !/*csx        35*/
      i=74                                                                      !/*csx        36*/
  210 do 220 j=1,min(np,3)                                                      !/*csx        37*/
      write(hp(i+1:i+11),'(1pe11.4)')fim(lfim+it(j),ic)                         !/*csx        38*/
  220 i=i+12                                                                    !/*csx        39*/
      if(kpt(1)*kpt(2).ne.0)write(hp(i+1:i+11),'(1pe11.3)')gwt(lgwt+ic)         !/*csx        40*/
  230 write(iuo,'(a121)')hp                                                     !/*csx        41*/
      write(iuo,240)tv,tm                                                       !/*cs        102*/
  240 format(/6h total,1pe48.5,e12.5)                                           !/*cs        103*/
      do 248 kb=1,(np+4)/8                                                      !/*csx        42*/
      hp=' '                                                                    !/*csx        43*/
      i=17                                                                      !/*csx        44*/
      kf=8*(kb-1)+4                                                             !/*csx        45*/
      kl=min(kf+7,np)                                                           !/*csx        46*/
      do 242 no=kf,kl                                                           !/*csx        47*/
      hp(i+3:i+13)=hnp(it(no))                                                  !/*csx        48*/
  242 i=i+13                                                                    !/*csx        49*/
      write(iuo,'(//a121)')hp                                                   !/*csx        50*/
      hp='        cell  mat'                                                    !/*csx        51*/
      i=17                                                                      !/*csx        52*/
      do 244 no=kf,kl                                                           !/*csx        53*/
      hp(i+3:i+12)='importance'                                                 !/*csx        54*/
  244 i=i+13                                                                    !/*csx        55*/
      write(iuo,'(a121/)')hp                                                    !/*csx        56*/
      do 248 ic=1,mxa                                                           !/*csx        57*/
      hp=' '                                                                    !/*csx        58*/
      nx=0                                                                      !/*csx        59*/
      if(mat(lmat+ic).ne.0)nx=nmt(lnmt+mat(lmat+ic))                            !/*csx        60*/
      write(hp,'(2i6,i5)')ic,ncl(lncl+ic),nx                                    !/*csx        61*/
      i=17                                                                      !/*csx        62*/
      do 246 no=kf,kl                                                           !/*csx        63*/
      write(hp(i+1:i+12),'(1pe12.4)')fim(lfim+it(no),ic)                        !/*csx        64*/
  246 i=i+13                                                                    !/*csx        65*/
  248 write(iuo,'(a121)')hp                                                     !/*csx        66*/
!                                                                               !/*cs        104*/
!        print exp. trans. and forced collision parameters, if any.             !/*cs        105*/
      k=0                                                                       !/*cs        106*/
      do 300 i=1,mxa                                                            !/*cs        107*/
      if((kpt(2).eq.0.or.qax(lqax+2,i).eq.0..and.for(lfor+2,i).eq.0.)           !/*cs        108*/
     & .and.(kpt(1).eq.0.or.qax(lqax+1,i).eq.0..and.for(lfor+1,i)               !/*cs        109*/
     & .eq.0))go to 300                                                         !/*cs        110*/
      if(k.eq.0)write(iuo,250)                                                  !/*cs        111*/
  250 format(1h1,5x,43hexponential transform and forced collision ,             !/*cs        112*/
     & 10hparameters,45x,14hprint table 62//24x,12hneutron exp.,11x,            !/*cs        113*/
     & 11hphoton exp.,7x,17hforced collisions/8x,4hcell,6x,3hdir,4x,            !/*cs        114*/
     & 9htransform,6x,3hdir,4x,9htransform,7x,7hneutron,8x,6hphoton/)           !/*cs        115*/
      k=1                                                                       !/*cs        116*/
      do 280 ip=1,2                                                             !/*cs        117*/
      hj(1,ip)=' '                                                              !/*cs        118*/
      hj(2,ip)=' '                                                              !/*cs        119*/
      hj(ip,3)=' '                                                              !/*cs        120*/
      m=int(abs(qax(lqax+ip,i)))-4                                              !/*cs        121*/
      ht='xyz'                                                                  !/*cs        122*/
      if(abs(m+2).le.1)hj(1,ip)(3:3)=ht(m+4:m+4)                                !/*cs        123*/
      if(m.le.0)go to 270                                                       !/*cs        124*/
      m=mod(m,100000)                                                           !/*cs        125*/
      write(hl,'(i5)')m                                                         !/*cs        126*/
      do 260 n=1,5                                                              !/*cs        127*/
  260 if(m.ge.10**(n-1))nn=n                                                    !/*cs        128*/
      hj(1,ip)=' v'//hl(6-nn:5)                                                 !/*cs        129*/
  270 if(m.ne.0.and.qax(lqax+ip,i).lt.0.)hj(1,ip)(1:1)='-'                      !/*cs        130*/
      p=qax(lqax+ip,i)-aint(qax(lqax+ip,i))                                     !/*cs        131*/
      if(p.ne.0.)write(hj(2,ip),'(f9.5)')abs(p)                                 !/*cs        132*/
      if(qax(lqax+ip,i).ne.0..and.p.eq.0.)hj(2,ip)='     s    '                 !/*cs        133*/
  280 if(for(lfor+ip,i).ne.0.)write(hj(ip,3),'(f9.5)')for(lfor+ip,i)            !/*cs        134*/
      write(iuo,290)i,ncl(lncl+i),hj                                            !/*cs        135*/
  290 format(2i6,5x,a7,a10,5x,a7,a10,5x,a10,4x,a10)                             !/*cs        136*/
  300 continue                                                                  !/*cs        137*/
!                                                                               !/*cs        138*/
!        check for exponential transformation incompatibilities.                !/*cs        139*/
      if(k.eq.0)go to 390                                                       !/*cs        140*/
      do 310 i=1,mxa                                                            !/*cs        141*/
      do 310 j=1,2                                                              !/*cs        142*/
  310 if(qax(lqax+j,i).ne.0..and.nww(j).eq.0)go to 320                          !/*cs        143*/
      go to 330                                                                 !/*cs        144*/
  320 call erprnt(1,2,0,0,0,0,0,0,                                              !/*cs        145*/
     & '50hexponential transform usually needs weight window.')                 !/*cs        146*/
  330 do 340 i=1,mxa                                                            !/*cs        147*/
      do 340 j=1,2                                                              !/*cs        148*/
  340 if(qax(lqax+j,i).ne.0..and.rho(lrho+i).eq.0.)go to 350                    !/*cs        149*/
      go to 360                                                                 !/*cs        150*/
  350 call erprnt(1,1,0,0,0,0,0,0,                                              !/*cs        151*/
     & '36hexponential transform in void cells.')                               !/*cs        152*/
  360 do 370 i=1,mxa                                                            !/*cs        153*/
      do 370 j=1,2                                                              !/*cs        154*/
  370 if(qax(lqax+j,i).ne.0..and.for(lfor+j,i).ne.0.)go to 380                  !/*cs        155*/
      go to 390                                                                 !/*cs        156*/
  380 call erprnt(1,1,0,0,0,0,0,0,                                              !/*cs        157*/
     & '56hforced collision and exponential transform in same cell.')           !/*cs        158*/
!                                                                               !/*cs        159*/
!        convert exponential transform vector names to indexes.                 !/*cs        160*/
  390 do 410 i=1,mxa                                                            !/*cs        161*/
      do 410 j=1,2                                                              !/*cs        162*/
      m=int(abs(qax(lqax+j,i)))-4                                               !/*cs        163*/
      if(m.lt.1)go to 410                                                       !/*cs        164*/
      do 400 k=1,nvec                                                           !/*cs        165*/
      if(jvc(ljvc+k).ne.m)go to 400                                             !/*cs        166*/
      qax(lqax+j,i)=sign(abs(qax(lqax+j,i))-m+k,qax(lqax+j,i))                  !/*cs        167*/
      go to 410                                                                 !/*cs        168*/
  400 continue                                                                  !/*cs        169*/
      call erprnt(1,1,1,m,0,0,0,0,                                              !/*cs        170*/
     & '28hexponential transform vector,i6,16h does not exist.')                !/*cs        171*/
  410 continue                                                                  !/*cs        172*/
      if(nvec.gt.0)write(iuo,'(/7h vector,i5,3x,1p3e13.4)')                     !/*cs        173*/
     & (jvc(ljvc+k),(vec(lvec+i,k),i=1,3),k=1,nvec)                             !/*cs        174*/
!                                                                               !/*cs        175*/
!        print the names and coefficients of the surfaces.                      !/*cs        176*/
  420 if(ink(70).eq.0)go to 470                                                 !/*cs        177*/
      write(iuo,430)                                                            !/*cs        178*/
  430 format(9h1surfaces,95x,14hprint table 70//                                !/*cs        179*/
     & 9x,7hsurface,5x,34htrans  type   surface coefficients/)                  !/*cs4c        8*/
      do 450 js=1,mxj                                                           !/*cs        181*/
      if(idnt(lidt+js).ne.0.and.idnt(lidt+js).ne.js)go to 450                   !/*cs4c        9*/
      n=lsc(llsc+js+1)-lsc(llsc+js)                                             !/*cs        182*/
      do 440 i=1,n                                                              !/*cs        183*/
  440 tpp(i)=scf(lsc(llsc+js)+i)                                                !/*cs        184*/
      k=kst(lkst+js)                                                            !/*cs4a        6*/
      if(k.ge.5.and.k.le.15)tpp(n)=sqrt(tpp(n))                                 !/*cs        186*/
      if(k.ge.16.and.k.le.21.and.tpp(n).ne.0.)tpp(n)=sign(one,tpp(n))           !/*cs        187*/
      if(k.ge.16.and.k.le.21.and.tpp(n).eq.0..or.k.ge.24)n=n-1                  !/*cs        188*/
      hl=' '                                                                    !/*cs        189*/
      if(ksu(lksu+js).eq.-1)hl='refl.'                                          !/*cs4a        7*/
      if(ksu(lksu+js).eq.-2)hl='white'                                          !/*cs4a        8*/
      if(ksu(lksu+js).gt.0)hl='pbc  '                                           !/*cs4a        9*/
      ht=' '                                                                    !/*cs        191*/
      if(jtr(ljtr+js).ne.0)write(ht,'(i4)')jtr(ljtr+js)                         !/*cs        192*/
      j1=nsf(lnsf+js)                                                           !/*cs4c       10*/
      hq=' '                                                                    !/*cs4c       11*/
      if(kfq.ne.0)write(hq,'(1h.,i1)')kfq                                       !/*cs4c       12*/
      write(iuo,460)js,j1,hq,hl,ht,ksf(k),(tpp(i),i=1,n)                        !/*cs4c       13*/
  450 continue                                                                  !/*cs4c       14*/
  460 format(i6,i7,a2,1x,a5,1x,a4,3x,a3,1p5e16.7/(32x,5e16.7))                  !/*cs4c       15*/
!                                                                               !/*cs4c       16*/
!        print out identical surface sets.                                      !/*cs4c       17*/
      l=1                                                                       !/*cs4c       18*/
      hp=' '                                                                    !/*cs4c       19*/
      do 464 j=1,idne(lide+1)                                                   !/*cs4c       20*/
      if(j.eq.1)j3=3                                                            !/*cs4c       21*/
      if(j.eq.1)write(iuo,461)                                                  !/*cs4c       22*/
  461 format(21h1  identical surfaces,83x,14hprint table 70//                   !/*cs4c       23*/
     & 3x,14hmaster surface,5x,18hidentical surfaces/)                          !/*cs4c       24*/
      do 463 j2=1,idne(lide+j3)                                                 !/*cs4c       25*/
      write(hp(l+1:l+10),'(i8,2h  )')nsf(lnsf+idne(lide+j3+j2))                 !/*cs4c       26*/
      if(kfq.ne.0)write(hp(l+9:l+10),'(1h.,i1)')kfq                             !/*cs4c       27*/
      if(l+20.gt.119)call begone(hp,l)                                          !/*cs4c       28*/
      if(l.eq.1)l=9                                                             !/*cs4c       29*/
  463 l=l+10                                                                    !/*cs4c       30*/
      if(l.gt.19)call begone(hp,l)                                              !/*cs4c       31*/
  464 j3=j3+idne(lide+j3)+1                                                     !/*cs4c       32*/
!                                                                               !/*cs4c       33*/
!        print out surface coefficients for any identical surfaces.             !/*cs4c       34*/
      do 469 j=1,idne(lide+1)                                                   !/*cs4c       35*/
      if(j.eq.1)j3=3                                                            !/*cs4c       36*/
      if(j.eq.1)write(iuo,465)                                                  !/*cs4c       37*/
  465 format(//3x,47hsurface coefficients for identical surfaces not,           !/*cs4c       38*/
     & 6h used.//9x,7hsurface,5x,34htrans  type   surface coefficients/)        !/*cs4c       39*/
      do 468 j4=2,idne(lide+j3)                                                 !/*cs4c       40*/
      js=idne(lide+j3+j4)                                                       !/*cs4c       41*/
      n=lsc(llsc+js+1)-lsc(llsc+js)                                             !/*cs4c       42*/
      do 466 i=1,n                                                              !/*cs4c       43*/
  466 tpp(i)=scf(lsc(llsc+js)+i)                                                !/*cs4c       44*/
      k=kst(lkst+js)                                                            !/*cs4c       45*/
      if(k.ge.5.and.k.le.15)tpp(n)=sqrt(tpp(n))                                 !/*cs4c       46*/
      if(k.ge.16.and.k.le.21.and.tpp(n).ne.0.)tpp(n)=sign(one,tpp(n))           !/*cs4c       47*/
      if(k.ge.16.and.k.le.21.and.tpp(n).eq.0..or.k.ge.24)n=n-1                  !/*cs4c       48*/
      hl=' '                                                                    !/*cs4c       49*/
      if(ksu(lksu+js).eq.-1)hl='refl.'                                          !/*cs4c       50*/
      if(ksu(lksu+js).eq.-2)hl='white'                                          !/*cs4c       51*/
      if(ksu(lksu+js).gt.0)hl='pbc  '                                           !/*cs4c       52*/
      ht=' '                                                                    !/*cs4c       53*/
      if(jtr(ljtr+js).ne.0)write(ht,'(i4)')jtr(ljtr+js)                         !/*cs4c       54*/
      j1=nsf(lnsf+js)                                                           !/*cs4c       55*/
      hq=' '                                                                    !/*cs4c       56*/
      if(kfq.ne.0)write(hq,'(1h.,i1)')kfq                                       !/*cs4c       57*/
  468 write(iuo,460)js,j1,hq,hl,ht,ksf(k),(tpp(i),i=1,n)                        !/*cs4c       58*/
  469 j3=j3+idne(lide+j3)+1                                                     !/*cs4c       59*/
!                                                                               !/*cs        195*/
!        check for unused surfaces.                                             !/*cs        196*/
  470 jp=0                                                                      !/*cs4a       10*/
      do 500 j=1,mxj                                                            !/*cs4a       11*/
      if(idns(lids+j).ne.0)go to 500                                            !/*cs4c       60*/
      if(ksm(lksm+j).ne.0)go to 500                                             !/*cs4c       61*/
      if(ksu(lksu+j).gt.0)jp=1                                                  !/*cs4a       12*/
      do 480 i=1,mlja                                                           !/*cs        198*/
  480 if(abs(lja(llja+i)).eq.j)go to 500                                        !/*cs        199*/
      do 490 ic=1,mxa                                                           !/*cs        200*/
      k=nsf(lnsf+j)+1000*ncl(lncl+ic)                                           !/*cs        201*/
      do 490 i=abs(lca(llca+ic)),abs(lca(llca+ic+1))-1                          !/*cs        202*/
      if(lja(llja+i).gt.1000000)go to 490                                       !/*cs        203*/
      if(nsf(lnsf+abs(lja(llja+i))).eq.k)go to 500                              !/*cs        204*/
  490 continue                                                                  !/*cs        205*/
      call erprnt(1,2,1,nsf(lnsf+j),0,0,0,0,                                    !/*cs        206*/
     & '7hsurface,i7,26h is not used for anything.')                            !/*cs        207*/
      ksu(lksu+j)=-3                                                            !/*cs4a       13*/
  500 continue                                                                  !/*cs        208*/
      if(mct.ge.0)write(iuo,'(//22h random number control,e25.15)')rngs         !/*cs4c2       1*/
!                                                                               !/*cs4a       14*/
!        process periodic boundary conditions.                                  !/*cs4a       15*/
      if(jp.ne.0)call ipbc                                                      !/*cs4a       16*/
!                                                                               !/*cs        209*/
!        print and check the temperatures of the cells.                         !/*cs        210*/
      if(mxe.eq.0.or.mcal.ne.0.or.mxt.eq.0)return                               !/*cs4b        2*/
!                                                                               !/*cs4b        3*/
!        check for zero temperatures in nonvoid and nonzero imp cells.          !/*cs4b        4*/
      do 506 ic=1,mxa                                                           !/*cs4b        5*/
      if(fim(lfim+1,ic).eq.0..or.mat(lmat+ic).eq.0)go to 506                    !/*cs4b        6*/
      do 503 k=1,mxt                                                            !/*cs4b        7*/
      if(tmp(ltmp+k+(ic-1)*mxt).gt.0.)go to 503                                 !/*cs4b        8*/
      call erprnt(1,1,2,k,ncl(lncl+ic),0,0,0,                                   !/*cs4b        9*/
     & '11htemperature,i3,9h for cell,i5,18h must not be zero.')                !/*cs4b       10*/
  503 continue                                                                  !/*cs4b       11*/
  506 continue                                                                  !/*cs4b       12*/
      if(ink(72).eq.0)return                                                    !/*cs4b       13*/
      write(iuo,510)                                                            !/*cs4a       18*/
  510 format(52h1  cell temperatures in mev for the free-gas thermal,           !/*cs4a       19*/
     & 19h neutron treatment.,32x,15h print table 72)                           !/*cs4a       20*/
      if(mxt.gt.1)go to 518                                                     !/*cs4a       21*/
!                                                                               !/*cs4a       22*/
!         if one time, check if all the cell temperatures are the same.         !/*cs4a       23*/
      tp=-huge                                                                  !/*cs4a       24*/
      do 511 ic=1,mxa                                                           !/*cs4a       25*/
      if(fim(lfim+1,ic).eq.0..or.mat(lmat+ic).eq.0)go to 511                    !/*cs4a       26*/
      if(tp.eq.-huge)tp=tmp(ltmp+ic)                                            !/*cs4a       27*/
      if(tp.ne.tmp(ltmp+ic))go to 518                                           !/*cs4a       28*/
  511 continue                                                                  !/*cs4a       29*/
      if(tp.ne.-huge)go to 514                                                  !/*cs4a       30*/
      write(iuo,512)                                                            !/*cs4a       31*/
  512 format(/46h   all materials are in zero importance cells.)                !/*cs4a       32*/
      return                                                                    !/*cs4a       33*/
  514 if(tp.le.0.)go to 518                                                     !/*cs4a       34*/
      write(iuo,516)tp                                                          !/*cs4a       35*/
  516 format(/52h   all non-zero importance cells with materials have,          !/*cs4a       36*/
     & 38h a temperature for thermal neutrons of,1pe11.4,5h mev.)               !/*cs4a       37*/
      return                                                                    !/*cs4a       38*/
!                                                                               !/*cs4a       39*/
!        print the complete table of cell temperatures.                         !/*cs4a       40*/
  518 do 580 k1=1,mxt,7                                                         !/*cs4a       41*/
      k2=min(k1+6,mxt)                                                          !/*cs        215*/
      write(iuo,520)(tth(ltth+k),k=k1,k2)                                       !/*cs4a       42*/
  520 format(/5x,5htime:,1p7e16.4)                                              !/*cs        217*/
      write(iuo,530)('tmp',k,k=k1,k2)                                           !/*cs4a       43*/
  530 format(/5x,4hcell,9x,a3,i2,6(11x,a3,i2))                                  !/*cs        219*/
      do 570 ic=1,mxa                                                           !/*cs        220*/
      if(fim(lfim+1,ic).eq.0..or.mat(lmat+ic).eq.0)go to 570                    !/*cs        221*/
!                                                                               !/*cs        222*/
!        print the temperatures of thermal cells.                               !/*cs        223*/
      write(iuo,540)ncl(lncl+ic),(tmp(ltmp+k+(ic-1)*mxt),k=k1,k2)               !/*cs4a       44*/
  540 format(i8,2x,1p7e16.4)                                                    !/*cs        226*/
  570 continue                                                                  !/*cs        234*/
  580 continue                                                                  !/*cs        235*/
      return                                                                    !/*cs        236*/
      end                                                                       !/*cs        237*/

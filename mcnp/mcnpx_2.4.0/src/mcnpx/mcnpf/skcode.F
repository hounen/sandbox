!#ident skcode.F from deck sk                                                   !/*sk          1*/
      subroutine skcode                                                         !/*sk          2*/
!        set up the fission source for a kcode problem.                         !/*sk          3*/
      use global_data                                                           !/*skx         1*/
      use imcn_data                                                             !/*skx         2*/
!                                                                               !/*skx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*skx         4*/
      implicit integer(kindi) (i-n)                                             !/*skx         5*/
!                                                                               !/*skx         6*/
      dimension kr(4)                                                           !/*skx         7*/
      character ht*10                                                           !/*sk4c        1*/
!                                                                               !/*sk          7*/
      swtm=1.                                                                   !/*sk          8*/
      itfxs=1                                                                   !/*sk          9*/
!                                                                               !/*sk         10*/
!        if the source is from sdef card, just create the srctp file.           !/*sk         11*/
    8 if(ksdef.eq.0)go to 10                                                    !/*sk4c       10*/
      call ksrctp(3)                                                            !/*sk         13*/
      nsa=nsrck                                                                 !/*sk         14*/
      ixak=1                                                                    !/*sk         15*/
      ist=1                                                                     !/*sk         16*/
      go to 200                                                                 !/*sk         17*/
!                                                                               !/*sk         18*/
!        if the source is not from the ksrc card, read the srctp file.          !/*sk         19*/
   10 if(nsrc.gt.0)go to 20                                                     !/*sk         20*/
      if(inqire(srctp).eq.0)return                                              !/*sk4a        3*/
      call ksrctp(2)                                                            !/*sk         21*/
      go to 30                                                                  !/*sk         22*/
!                                                                               !/*sk         23*/
!        if the source is from the ksrc card, create the srctp file.            !/*sk         24*/
   20 nsa=(nsrc+2)/3                                                            !/*sk         25*/
      ixak=1                                                                    !/*sk         26*/
      ist=ixak+nsa                                                              !/*sk         27*/
      call ksrctp(3)                                                            !/*sk         28*/
      mrl=msrk                                                                  !/*sk         29*/
!                                                                               !/*sk         30*/
!        find the cells in which the source points are located.                 !/*sk         31*/
   30 n0=nsa                                                                    !/*sk         32*/
      nsa=0                                                                     !/*sk         33*/
      n1=0                                                                      !/*sk         34*/
      jsu=0                                                                     !/*sk         35*/
      do 40 i=1,4                                                               !/*sk         36*/
   40 kr(i)=0                                                                   !/*sk         37*/
      kt=0                                                                      !/*sk         38*/
      uuu=.6                                                                    !/*sk         39*/
      vvv=.8                                                                    !/*sk         40*/
      www=0.                                                                    !/*sk         41*/
      do 120 n=1,n0                                                             !/*sk         42*/
      i1=lfso+6*(ixak-1)                                                        !/*sk         43*/
      i2=lfso+6*(ist-1)                                                         !/*sk         44*/
      xxx=fso(i1+1)                                                             !/*sk         45*/
      fso(i2+1)=fso(i1+1)                                                       !/*sk         46*/
      yyy=fso(i1+2)                                                             !/*sk         47*/
      fso(i2+2)=fso(i1+2)                                                       !/*sk         48*/
      zzz=fso(i1+3)                                                             !/*sk         49*/
      fso(i2+3)=fso(i1+3)                                                       !/*sk         50*/
      fso(i2+4)=fso(i1+4)                                                       !/*sk         51*/
      j1=1                                                                      !/*sk         52*/
      do 50 k=1,mxa                                                             !/*sk         53*/
      ju=0                                                                      !/*sk         54*/
      if(junf.ne.0)ju=abs(jun(ljun+k))                                          !/*sk         55*/
      if(ju.ne.0)go to 50                                                       !/*sk         56*/
      call chkcel(k,0,j)                                                        !/*sk         57*/
      if(j.ne.0)go to 50                                                        !/*sk         58*/
      i=k                                                                       !/*sk         59*/
      j1=min(4,4*abs(j1-1))                                                     !/*sk         60*/
   50 continue                                                                  !/*sk         61*/
      if(j1.ne.0)go to 100                                                      !/*sk         62*/
!                                                                               !/*sk         63*/
!        reject points in non-existent, zero-importance, void, and              !/*sk         64*/
!        ambiguous cells.                                                       !/*sk         65*/
      if(mat(lmat+i).eq.0.and.junf.eq.0)j1=3                                    !/*sk         66*/
      if(fim(lfim+1,i).eq.0)j1=2                                                !/*sk         67*/
      if(j1.ne.0)go to 100                                                      !/*sk         68*/
      if(junf.eq.0)go to 110                                                    !/*sk         69*/
      lev=0                                                                     !/*sk         70*/
      ic=i                                                                      !/*sk         71*/
   60 if(mat(lmat+ic).eq.0.and.mfl(lmfl+1,ic).eq.0)j1=3                         !/*sk         72*/
      if(fim(lfim+1,ic).eq.0)j1=2                                               !/*sk         73*/
      if(j1.ne.0)go to 100                                                      !/*sk         74*/
      if(mfl(lmfl+1,ic).eq.0)go to 110                                          !/*sk         75*/
      kdb=0                                                                     !/*sk4c       11*/
      if(lat(llat+1,ic).ne.0)call findel(ic)                                    !/*sk         76*/
      if(kdb.ne.0)j1=4                                                          !/*sk4c       12*/
      if(kdb.ne.0)go to 100                                                     !/*sk4c       13*/
      j=-mfl(lmfl+1,ic)                                                         !/*sk         77*/
      if(j.gt.0)go to 70                                                        !/*sk         78*/
      ju=-j                                                                     !/*sk         79*/
      call dunlev(ic)                                                           !/*sk         80*/
      if(mfl(lmfl+2,ic).eq.0)go to 80                                           !/*sk         81*/
      ic=mfl(lmfl+2,ic)                                                         !/*sk         82*/
      go to 60                                                                  !/*sk         83*/
   70 do 75 k=1,3                                                               !/*sk4a        4*/
   75 if(ijk(k).lt.laf(j+k,1).or.ijk(k).gt.laf(j+k,1)+laf(j+k,2)-1)j1=1         !/*skx        10*/
      if(j1.ne.0)go to 100                                                      !/*sk4a        6*/
      k=3+iii-laf(j+1,1)+laf(j+1,2)*(jjj-laf(j+2,1)+laf(j+2,2)*                 !/*sk4a        7*/
     & (kkk-laf(j+3,1)))                                                        !/*sk         85*/
      if(laf(j+1,k).eq.abs(jun(ljun+ic)))go to 110                              !/*sk         86*/
      if(laf(j+1,k).eq.0)j1=1                                                   !/*sk         87*/
      if(j1.ne.0)go to 100                                                      !/*sk         88*/
      ju=laf(j+1,k)                                                             !/*sk         89*/
      call dunlev(ic)                                                           !/*sk         90*/
      if(laf(j+2,k).eq.0)go to 80                                               !/*sk         91*/
      ic=laf(j+2,k)                                                             !/*sk         92*/
      go to 60                                                                  !/*sk         93*/
   80 j1=1                                                                      !/*sk         94*/
      do 90 k=1,mxa                                                             !/*sk         95*/
      if(abs(jun(ljun+k)).ne.ju)go to 90                                        !/*sk         96*/
      call chkcel(k,0,j)                                                        !/*sk         97*/
      if(j.ne.0)go to 90                                                        !/*sk         98*/
      ic=k                                                                      !/*sk         99*/
      j1=min(4,4*abs(j1-1))                                                     !/*sk        100*/
   90 continue                                                                  !/*sk        101*/
      if(j1.eq.0)go to 60                                                       !/*sk        102*/
  100 kr(j1)=kr(j1)+1                                                           !/*sk        103*/
      kt=kt+1                                                                   !/*sk        104*/
      go to 120                                                                 !/*sk        105*/
  110 fso(i2+5)=i                                                               !/*sk        106*/
      n1=n1+1                                                                   !/*sk        107*/
      ist=mod(ist,msrk)+1                                                       !/*sk        108*/
  120 ixak=mod(ixak,mrl)+1                                                      !/*sk        109*/
      if(n1.eq.0)call erprnt(1,1,0,0,0,0,0,0,                                   !/*sk        110*/
     & '31hthe entire source was rejected.')                                    !/*sk        111*/
      if(n1.eq.0)go to 160                                                      !/*sk        112*/
!                                                                               !/*sk        113*/
!        expand or contract the source to about the nominal size.               !/*sk        114*/
      rngb=aint(5d0**19*2d0**(-24))                                             !/*sk        115*/
      rngs=5d0**19-rngb*2d0**24                                                 !/*sk        116*/
      ranb=rngb                                                                 !/*sk        117*/
      rans=rngs                                                                 !/*sk        118*/
      do 150 n=1,n1                                                             !/*sk        119*/
      i1=lfso+6*(ixak-1)                                                        !/*sk        120*/
      k2=(rang()*n1+nsrck)/n1                                                   !/*sk        121*/
      do 140 k=1,k2                                                             !/*sk        122*/
      nsa=nsa+1                                                                 !/*sk        123*/
      i2=lfso+6*(ist-1)                                                         !/*sk        124*/
      do 130 m=1,5                                                              !/*sk        125*/
  130 fso(i2+m)=fso(i1+m)                                                       !/*sk        126*/
!                                                                               !/*sk        127*/
!        sample the energy if the point does not already have it.               !/*sk        128*/
      if(k.eq.k2.and.nsrc.eq.0)go to 140                                        !/*sk        129*/
      j=rang()*32.+1.                                                           !/*sk        130*/
      fso(i2+4)=fes(j)+rang()*(fes(j+1)-fes(j))                                 !/*sk        131*/
  140 ist=mod(ist,msrk)+1                                                       !/*sk        132*/
  150 ixak=mod(ixak,msrk)+1                                                     !/*sk        133*/
!                                                                               !/*sk        134*/
!        print the source parameters.                                           !/*sk        135*/
  160 if(ink(90).eq.0)go to 200                                                 !/*sk        136*/
      if(nsrc.gt.0)write(iuo,170)                                               !/*sk        137*/
  170 format(1h1,5x,30hinitial source from ksrc card.,68x,                      !/*sk        138*/
     & 14hprint table 90)                                                       !/*sk        139*/
      if(nsrc.eq.0)write(iuo,180)srctp                                          !/*sk        140*/
  180 format(1h1,5x,25hinitial source from file ,a10)                           !/*sk        141*/
      write(iuo,190)n0,kr,kt,n1,nsa,nsrck,rkk,ikz,mrkp                          !/*sk4a        8*/
  190 format(/26h original number of points,i30/                                !/*sk        143*/
     & 4x,22hpoints not in any cell,i24/                                        !/*sk        144*/
     & 4x,34hpoints in cells of zero importance,i12/                            !/*sk        145*/
     & 4x,20hpoints in void cells,i26/                                          !/*sk        146*/
     & 4x,25hpoints in ambiguous cells,i21/                                     !/*sk        147*/
     & 22h total points rejected,i34/17h points remaining,i39/                  !/*sk        148*/
     & 38h points after expansion or contraction,i18/                           !/*sk        149*/
     & 20h nominal source size,i36//26h initial guess for k(eff.),f30.6         !/*sk        150*/
     & //31h cycles to skip before tallying,i25//                               !/*sk4a        9*/
     & 41h number of keff cycles that can be stored,i15/)                       !/*sk4a       10*/
  200 if(kct.ne.0.and.kct.le.ikz.and.ntal.ne.0)call erprnt(1,2,0,0,0,0,         !/*sk        152*/
     & 0,0,'51hthis problem will quit before any tallies are made.')            !/*sk        153*/
      if(ecf(1).gt.0.1)call erprnt(1,2,0,0,0,0,0,0,                             !/*sk4c       36*/
     & '49hcutn energy cutoff .gt.0.1 -- too high for kcode.')                  !/*sk4c       37*/
      if(itotnu.eq.0)itotnu=1                                                   !/*skx        15*/
      ht=' prompt '                                                             !/*sk4c       39*/
      if(itotnu.eq.1)ht='  total '                                              !/*sk4c       40*/
      write(iuo,210)ht                                                          !/*sk4c       41*/
      write(jtty,210)ht                                                         !/*sk4c       42*/
  210 format(a8,34hfission nubar data are being used.)                          !/*sk4c       43*/
  220 if(ikz.gt.0)nskk=nsa                                                      !/*sk        159*/
      if(nsa.eq.0)call expirx(0,'skcode','no source points in kcode')           !/*sk4c       46*/
      wt0=(nsrck+zero)/nsa                                                      !/*sk        161*/
      rkpl(lrkp+18,1)=nsa                                                       !/*sk4a       11*/
      return                                                                    !/*sk        162*/
      end                                                                       !/*sk        163*/

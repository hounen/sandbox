!#ident sourcb.F from deck sz                                                   !/*sz          1*/
      subroutine sourcb                                                         !/*sz          2*/
!        start a particle from the general source.                              !/*sz          3*/
      use global_data                                                           !/*szx         1*/
!                                                                               !/*szx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*szx         3*/
      implicit integer(kindi) (i-n)                                             !/*szx         4*/
!                                                                               !/*szx         5*/
      character hc*5                                                            !/*sz          5*/
      dimension fb(maxv),ic(maxv),sf(3)                                         !/*szx         6*/
!                                                                               !/*sz          7*/
   10 do 20 i=1,3                                                               !/*sz          8*/
   20 sf(i)=0.                                                                  !/*sz          9*/
      do 30 i=1,maxv                                                            !/*szx         7*/
      do 30 j=1,3                                                               !/*sz         11*/
   30 sff(j,i)=srv(j,i)                                                         !/*sz         12*/
      icl=0                                                                     !/*sz         13*/
      jsu=0                                                                     !/*sz         14*/
      wgt=srv(1,17)                                                             !/*sz         15*/
      ipsc=101                                                                  !/*sz         16*/
      imd=0                                                                     !/*sz         17*/
      if(nsr.eq.71)nsa=nsa-1                                                    !/*sz         18*/
!                                                                               !/*sz         19*/
!        zero the frequency duplication block.                                  !/*sz         20*/
      do 40 i=1,msd                                                             !/*sz         21*/
      fdd(kfdd+1,i)=0.                                                          !/*sz         22*/
   40 fdd(kfdd+2,i)=0.                                                          !/*sz         23*/
!                                                                               !/*sz         24*/
!        sample each source variable in its turn.                               !/*sz         25*/
      ns=1                                                                      !/*sz4a        1*/
   45 iy=0                                                                      !/*sz4a        2*/
      id=0                                                                      !/*sz         27*/
      sff(1,15)=srv(1,15)                                                       !/*sz         28*/
      iax=abs(srv(1,12))+abs(srv(2,12))+abs(srv(3,12))                          !/*sz         29*/
      iv=abs(srv(1,13))+abs(srv(2,13))+abs(srv(3,13))                           !/*sz         30*/
      do 430 nn=ns,nord                                                         !/*sz4a        3*/
      if(nn.eq.nnpos)sw=wgt                                                     !/*sz4a        4*/
      n=ivord(nn)                                                               !/*sz         32*/
!                                                                               !/*sz         33*/
!        defaulted variable.                                                    !/*sz         34*/
      if(idefv(n).eq.0)go to(430,430,240,250,430,320)n                          !/*sz         35*/
!                                                                               !/*sz         36*/
!        explicitly defined variable.                                           !/*sz         37*/
      if(ivdis(n).eq.0)go to 180                                                !/*sz         38*/
!                                                                               !/*sz         39*/
!        independent variable.                                                  !/*sz         40*/
      if(ivdd(n).ne.n)go to 60                                                  !/*sz         41*/
      call smpsrc(sf,ivdis(n),ic(n),fb(n))                                      !/*sz         42*/
      if(ksd(lksd+6,ivdis(n)).eq.0)go to 160                                    !/*sz         43*/
   50 ld=sf(1)                                                                  !/*sz         44*/
      if(ld.eq.0)go to(430,430,240,250,430,320,                                 !/*sz         45*/
     & 430,430,430,430,430,430,430,430,430,430)n                                !/*sz         46*/
      call smpsrc(sf,ld,ib,fi)                                                  !/*sz         47*/
      if(ksd(lksd+6,ld).ne.0)go to 50                                           !/*sz         48*/
      go to 160                                                                 !/*sz         49*/
!                                                                               !/*sz         50*/
!        dependent variable.                                                    !/*sz         51*/
   60 kd=ivdis(n)                                                               !/*sz         52*/
      if(ksd(lksd+6,kd).ne.0)go to 120                                          !/*sz         53*/
      if(ksd(lksd+9,kd).ne.0)go to 80                                           !/*sz         54*/
      isic(n)=kd                                                                !/*sz         55*/
      js=ic(ivdd(n))                                                            !/*sz         56*/
      do 70 i=1,nvs(n)                                                          !/*sz         57*/
      sff(i,n)=spf(ksd(lksd+13,kd)+1,ksd(lksd+21,kd)*(js-1)+i)                  !/*sz         58*/
   70 if(ksd(lksd+5,kd).eq.0)sff(i,n)=sff(i,n)+fb(ivdd(n))*                     !/*sz         59*/
     & (spf(ksd(lksd+13,kd)+1,ksd(lksd+21,kd)*js+i)-sff(i,n))                   !/*sz         60*/
      go to 180                                                                 !/*sz         61*/
!                                                                               !/*sz         62*/
!        dependent variable with t option.                                      !/*sz         63*/
   80 do 90 js=1,ksd(lksd+4,kd)                                                 !/*sz         64*/
   90 if(spf(ksd(lksd+13,kd)+1,ksd(lksd+21,kd)*(js-1)+1).eq.                    !/*sz         65*/
     & sff(1,ivdd(n)))go to 100                                                 !/*sz         66*/
      go to(430,430,240,250,430,320,                                            !/*sz         67*/
     & 430,430,430,430,430,430,430,430,430,430)n                                !/*sz         68*/
  100 isic(n)=kd                                                                !/*sz         69*/
      do 110 i=1,nvs(n)                                                         !/*sz         70*/
  110 sff(i,n)=spf(ksd(lksd+13,kd)+1,ksd(lksd+21,kd)*(js-1)+1+i)                !/*sz         71*/
      go to 180                                                                 !/*sz         72*/
!                                                                               !/*sz         73*/
!        dependent variable with s or q option.                                 !/*sz         74*/
  120 j=ic(ivdd(n))                                                             !/*sz         75*/
      if(ksd(lksd+8,kd).eq.0)go to 140                                          !/*sz         76*/
      do 130 j=2,2*ksd(lksd+4,kd),2                                             !/*sz         77*/
  130 if(spf(ksd(lksd+13,kd)+1,j-1).ge.sff(1,ivdd(n)))go to 140                 !/*sz         78*/
      call expirx(1,'sourcb',                                                   !/*sz         79*/
     & 'inadequate entries on ds card with q option.')                          !/*sz         80*/
      return                                                                    !/*sz         81*/
  140 sf(1)=spf(ksd(lksd+13,kd)+1,j)                                            !/*sz         82*/
  150 ld=sf(1)                                                                  !/*sz         83*/
      if(ld.eq.0)go to(430,430,240,250,430,320,                                 !/*sz         84*/
     & 430,430,430,430,430,430,430,430,430,430)n                                !/*sz         85*/
      call smpsrc(sf,ld,ib,fi)                                                  !/*sz         86*/
      if(ksd(lksd+6,ld).ne.0)go to 150                                          !/*sz         87*/
!                                                                               !/*sz         88*/
!        define the value of the particular source variable.                    !/*sz         89*/
  160 do 170 i=1,nvs(n)                                                         !/*sz         90*/
  170 sff(i,n)=sf(i)                                                            !/*sz         91*/
  180 go to(190,230,240,250,260,320,                                            !/*sz         92*/
     & 270,280,290,430,430,300,310,430,430,430)n                                !/*sz         93*/
  190 icl=sff(1,1)                                                              !/*sz         94*/
      if(icl.ge.0)go to 430                                                     !/*sz         95*/
      j=0                                                                       !/*sz         96*/
      l1=kcp(-icl-1)                                                            !/*sz4b        1*/
      l2=kcp(-icl-2)                                                            !/*sz4b        2*/
      do 220 i=0,l1                                                             !/*sz4b        3*/
      iclp(1,i)=kcp(-icl+j)                                                     !/*sz         98*/
      if(iclp(1,i).lt.100000)go to 210                                          !/*sz         99*/
      sf(1)=iclp(1,i)-100000                                                    !/*sz4c        1*/
  200 ld=nint(sf(1))                                                            !/*sz4c        2*/
      call smpsrc(sf,ld,ib,fi)                                                  !/*sz4c        3*/
      if(ksd(lksd+6,ld).ne.0.and.sf(1).ne.0.)go to 200                          !/*sz4c        4*/
      iclp(1,i)=nint(sf(1))                                                     !/*sz4c        5*/
  210 iclp(2,i)=98765432                                                        !/*sz        105*/
      iclp(5,i)=1                                                               !/*sz4b        4*/
      if(kcp(-icl+j+1).ne.1000001)go to 215                                     !/*sz4b        5*/
      iclp(2,i)=kcp(-icl+j+2)                                                   !/*sz        107*/
      iclp(3,i)=kcp(-icl+j+3)                                                   !/*sz        108*/
      iclp(4,i)=kcp(-icl+j+4)                                                   !/*sz        109*/
      j=j+5                                                                     !/*sz        110*/
!                                                                               !/*sz4b        6*/
!        if necessary, sample a lattice element for the previous level.         !/*sz4b        7*/
  215 if(i.gt.l2+1)go to 220                                                    !/*sz4b        8*/
      if(i.eq.0)go to 218                                                       !/*sz4b        9*/
      if(iclp(1,i-1).eq.0.or.iclp(2,i-1).ne.98765432)go to 218                  !/*sz4b       10*/
      ji=iclp(1,i-1)                                                            !/*sz4b       11*/
      l=-mfl(lmfl+1,ji)                                                         !/*sz4b       12*/
      if(l.le.0)go to 218                                                       !/*sz4b       13*/
      iclp(5,i-1)=0                                                             !/*sz4b       14*/
  217 iclp(2,i-1)=laf(l+1,1)+min(laf(l+1,2)-1,int(rang()*laf(l+1,2)))           !/*sz4b       15*/
      iclp(3,i-1)=laf(l+2,1)+min(laf(l+2,2)-1,int(rang()*laf(l+2,2)))           !/*sz4b       16*/
      iclp(4,i-1)=laf(l+3,1)+min(laf(l+3,2)-1,int(rang()*laf(l+3,2)))           !/*sz4b       17*/
      isef(kise+1,ji)=isef(kise+1,ji)+1                                         !/*sz4b       18*/
      if(iclp(1,i).eq.0)go to 220                                               !/*sz4b       19*/
      ju=laf(l+1,3+iclp(2,i-1)-laf(l+1,1)+laf(l+1,2)*                           !/*sz4b       20*/
     & (iclp(3,i-1)-laf(l+2,1)+laf(l+2,2)*(iclp(4,i-1)-laf(l+3,1))))            !/*sz4b       21*/
      if(ju.eq.abs(jun(ljun+iclp(1,i))))go to 218                               !/*sz4b       22*/
!                                                                               !/*sz4b       23*/
!        rejection loop for lattice element sampling.                           !/*sz4b       24*/
      isef(kise+2,ji)=isef(kise+2,ji)+1                                         !/*sz4b       25*/
      m=isef(kise+1,ji)-isef(kise+2,ji)                                         !/*sz4b       26*/
      if(one*max(m,10).gt.srv(1,18)*isef(kise+1,ji))go to 217                   !/*sz4b       27*/
      go to 580                                                                 !/*sz4b       28*/
!                                                                               !/*sz4b       29*/
!        if necessary, sample a lattice element for the lowest level.           !/*sz4b       30*/
  218 if(l1.ne.l2.or.i.ne.l1)go to 220                                          !/*sz4b       31*/
      if(iclp(1,i).eq.0.or.iclp(2,i).ne.98765432)go to 220                      !/*sz4b       32*/
      ji=iclp(1,i)                                                              !/*sz4b       33*/
      l=-mfl(lmfl+1,ji)                                                         !/*sz4b       34*/
      if(l.le.0)go to 220                                                       !/*sz4b       35*/
      iclp(2,i)=laf(l+1,1)+min(laf(l+1,2)-1,int(rang()*laf(l+1,2)))             !/*sz4b       36*/
      iclp(3,i)=laf(l+2,1)+min(laf(l+2,2)-1,int(rang()*laf(l+2,2)))             !/*sz4b       37*/
      iclp(4,i)=laf(l+3,1)+min(laf(l+3,2)-1,int(rang()*laf(l+3,2)))             !/*sz4b       38*/
      iclp(5,i)=0                                                               !/*sz4b       39*/
      isef(kise+1,ji)=isef(kise+1,ji)+1                                         !/*sz4b       40*/
  220 j=j+1                                                                     !/*sz        111*/
      go to 430                                                                 !/*sz        112*/
  230 jsu=sff(1,2)                                                              !/*sz        113*/
      go to 430                                                                 !/*sz        114*/
  240 erg=sff(1,3)                                                              !/*sz        115*/
      go to 430                                                                 !/*sz        116*/
  250 tme=sff(1,4)                                                              !/*sz        117*/
      go to 430                                                                 !/*sz        118*/
  260 id=1                                                                      !/*sz        119*/
      go to 430                                                                 !/*sz        120*/
  270 xxx=sff(1,7)                                                              !/*sz        121*/
      iy=1                                                                      !/*sz        122*/
      go to 430                                                                 !/*sz        123*/
  280 yyy=sff(1,8)                                                              !/*sz        124*/
      iy=1                                                                      !/*sz        125*/
      go to 430                                                                 !/*sz        126*/
  290 zzz=sff(1,9)                                                              !/*sz        127*/
      iy=1                                                                      !/*sz        128*/
      go to 430                                                                 !/*sz        129*/
  300 iax=abs(sff(1,12))+abs(sff(2,12))+abs(sff(3,12))                          !/*sz        130*/
      go to 430                                                                 !/*sz        131*/
  310 iv=abs(sff(1,13))+abs(sff(2,13))+abs(sff(3,13))                           !/*sz        132*/
      go to 430                                                                 !/*sz        133*/
!                                                                               !/*sz        134*/
!        pos:  sample the position on a surface or within a covering.           !/*sz        135*/
  320 if(iy.ne.0)go to 400                                                      !/*sz        136*/
      xxx=sff(1,6)                                                              !/*sz        137*/
      yyy=sff(2,6)                                                              !/*sz        138*/
      zzz=sff(3,6)                                                              !/*sz        139*/
!                                                                               !/*sz        140*/
!        source on a surface.                                                   !/*sz        141*/
      if(jsu.eq.0)go to 370                                                     !/*sz        142*/
      k=kst(lkst+jsu)                                                           !/*sz4a        5*/
      if(k.lt.5)go to 350                                                       !/*sz        144*/
      i=lsc(llsc+jsu)                                                           !/*sz        145*/
!                                                                               !/*sz        146*/
!        sample spherical surface uniformly in area.                            !/*sz        147*/
      if(k.gt.9)go to 330                                                       !/*sz        148*/
      j=13-iv*7                                                                 !/*sz        149*/
      iv=1                                                                      !/*sz        150*/
      if(iax.ne.0)call rotas(sff(1,11),sff(1,12),sff(1,j),0,irt)                !/*sz4b       41*/
      if(iax.eq.0)call isos(sff(1,j),0)                                         !/*sz4b       42*/
      if(kdb.ne.0)return                                                        !/*sz        153*/
      if(k.eq.5)r=sqrt(scf(i+1))                                                !/*sz        154*/
      if(k.eq.6)r=sqrt(scf(i+4))                                                !/*sz        155*/
      if(k.ge.7)r=sqrt(scf(i+2))                                                !/*sz        156*/
      xxx=r*sff(1,j)                                                            !/*sz        157*/
      yyy=r*sff(2,j)                                                            !/*sz        158*/
      zzz=r*sff(3,j)                                                            !/*sz        159*/
      if(k.ge.7)gpblcm(k-6)=gpblcm(k-6)+scf(i+1)                                !/*sz        160*/
      if(k.ne.6)go to 420                                                       !/*sz        161*/
      xxx=xxx+scf(i+1)                                                          !/*sz        162*/
      yyy=yyy+scf(i+2)                                                          !/*sz        163*/
      zzz=zzz+scf(i+3)                                                          !/*sz        164*/
      go to 420                                                                 !/*sz        165*/
!                                                                               !/*sz        166*/
!        sample unskewed spheroid uniformly in area.                            !/*sz        167*/
  330 j=1                                                                       !/*sz        168*/
      if(scf(i+1).eq.scf(i+3))j=2                                               !/*sz        169*/
      if(scf(i+1).eq.scf(i+2))j=3                                               !/*sz        170*/
      a=abs(scf(i+j))-1.                                                        !/*sz        171*/
      b=1.+max(zero,a)                                                          !/*sz        172*/
  340 y=2.*rang()-1.                                                            !/*sz        173*/
      if(y**2*a+1..le.b*rang()**2)go to 340                                     !/*sz        174*/
      r=sqrt((1.-y**2)*abs(scf(i+7)))                                           !/*sz        175*/
      gpblcm(j)=scf(i+j+7)+y*sqrt(-scf(i+7)/scf(i+j))                           !/*sz        176*/
      ph=rang()*2.*pie                                                          !/*sz        177*/
      jd=j+1                                                                    !/*sz        178*/
      if(jd.eq.4)jd=1                                                           !/*sz        179*/
      gpblcm(jd)=scf(i+7+jd)+r*cos(ph)                                          !/*sz        180*/
      gpblcm(6-j-jd)=scf(i+13-j-jd)+r*sin(ph)                                   !/*sz        181*/
      if(iv.ne.0)go to 420                                                      !/*sz        182*/
      c=angl()                                                                  !/*sz        183*/
      sff(1,13)=ang(1)                                                          !/*sz        184*/
      sff(2,13)=ang(2)                                                          !/*sz        185*/
      sff(3,13)=ang(3)                                                          !/*sz        186*/
      iv=1                                                                      !/*sz        187*/
      go to 420                                                                 !/*sz        188*/
!                                                                               !/*sz        189*/
!        sample on a plane within a circle.                                     !/*sz        190*/
  350 if(sff(1,10).eq.0..and.iv.ne.0.)go to 430                                 !/*sz        191*/
      j=13-iv                                                                   !/*sz        192*/
      iv=1                                                                      !/*sz        193*/
      c=angl()                                                                  !/*sz        194*/
      sff(1,j)=ang(1)                                                           !/*sz        195*/
      sff(2,j)=ang(2)                                                           !/*sz        196*/
      sff(3,j)=ang(3)                                                           !/*sz        197*/
      if(sff(1,10).eq.0.)go to 430                                              !/*sz        198*/
      if(abs(sff(1,13)*sff(1,j)+sff(2,13)*sff(2,j)+                             !/*sz        199*/
     & sff(3,13)*sff(3,j)).gt..99)imd=imd+2                                     !/*sz        200*/
      ph=rang()*2.*pie                                                          !/*sz        201*/
      xp=sff(1,10)*cos(ph)                                                      !/*sz        202*/
      yp=sff(1,10)*sin(ph)                                                      !/*sz        203*/
      t1=sqrt(sff(1,j)**2+sff(2,j)**2)                                          !/*sz        204*/
      if(t1.eq.0.)go to 360                                                     !/*sz        205*/
      t2=1./t1                                                                  !/*sz        206*/
      xxx=xxx+t2*(sff(1,j)*sff(3,j)*xp-sff(2,j)*yp)                             !/*sz        207*/
      yyy=yyy+t2*(sff(2,j)*sff(3,j)*xp+sff(1,j)*yp)                             !/*sz        208*/
      zzz=zzz-t1*xp                                                             !/*sz        209*/
      go to 420                                                                 !/*sz        210*/
  360 xxx=xxx+xp                                                                !/*sz        211*/
      yyy=yyy+yp                                                                !/*sz        212*/
      go to 420                                                                 !/*sz        213*/
!                                                                               !/*sz        214*/
!        source in a cell.                                                      !/*sz        215*/
!        spherical covering.                                                    !/*sz        216*/
  370 if(iax.ne.0)go to 380                                                     !/*sz        217*/
      if(sff(1,10).eq.0..and.icl.gt.0.and.ivdis(6).eq.0)go to 420               !/*sz4c        6*/
      if(sff(1,10).eq.0.)go to 400                                              !/*sz4c        7*/
      call isos(tpp,0)                                                          !/*sz4b       44*/
      xxx=xxx+sff(1,10)*tpp(1)                                                  !/*sz        220*/
      yyy=yyy+sff(1,10)*tpp(2)                                                  !/*sz        221*/
      zzz=zzz+sff(1,10)*tpp(3)                                                  !/*sz        222*/
      go to 400                                                                 !/*sz        223*/
!                                                                               !/*sz        224*/
!        cylindrical covering.                                                  !/*sz        225*/
  380 zu=sff(1,12)*sff(1,11)                                                    !/*sz        226*/
      zv=sff(2,12)*sff(1,11)                                                    !/*sz        227*/
      zw=sff(3,12)*sff(1,11)                                                    !/*sz        228*/
      ph=rang()*2.*pie                                                          !/*sz        229*/
      xp=sff(1,10)*cos(ph)                                                      !/*sz        230*/
      yp=sff(1,10)*sin(ph)                                                      !/*sz        231*/
      t1=sqrt(sff(1,12)**2+sff(2,12)**2)                                        !/*sz        232*/
      if(t1.eq.0.)go to 390                                                     !/*sz        233*/
      t2=1./t1                                                                  !/*sz        234*/
      xxx=xxx+t2*(sff(1,12)*sff(3,12)*xp-sff(2,12)*yp)+zu                       !/*sz        235*/
      yyy=yyy+t2*(sff(2,12)*sff(3,12)*xp+sff(1,12)*yp)+zv                       !/*sz        236*/
      zzz=zzz-t1*xp+zw                                                          !/*sz        237*/
      go to 400                                                                 !/*sz        238*/
  390 xxx=xxx+xp                                                                !/*sz        239*/
      yyy=yyy+yp                                                                !/*sz        240*/
      zzz=zzz+zw                                                                !/*sz        241*/
!                                                                               !/*sz        242*/
!        reject if the point is not in the cell.                                !/*sz        243*/
  400 if(icl.eq.0)go to 420                                                     !/*sz        244*/
      if(icl.lt.0)go to 410                                                     !/*sz        245*/
      ji=icl                                                                    !/*sz        246*/
      call chkcel(ji,0,j)                                                       !/*sz        247*/
      isef(kise+1,ji)=isef(kise+1,ji)+1                                         !/*sz        248*/
      if(j.ne.0)go to 425                                                       !/*sz4b       45*/
      go to 420                                                                 !/*sz        250*/
  410 l=levchk(ji)                                                              !/*sz        251*/
      if(kdb.ne.0)return                                                        !/*sz        252*/
      if(l.gt.0)go to 425                                                       !/*sz4b       46*/
      if(l.lt.0)go to 600                                                       !/*sz4b       47*/
!                                                                               !/*sz        254*/
!        reject if the point is not in the cookie-cutter cell.                  !/*sz        255*/
  420 if(sff(1,14).eq.0.)go to 430                                              !/*sz        256*/
      ji=sff(1,14)                                                              !/*sz        257*/
      call chkcel(ji,0,j)                                                       !/*sz        258*/
      isef(kise+1,ji)=isef(kise+1,ji)+1                                         !/*sz        259*/
      if(j.eq.0)go to 430                                                       !/*sz4b       48*/
!                                                                               !/*sz4b       49*/
!        rejection loop for position sampling.                                  !/*sz4b       50*/
!        variable nnpos may be for pos or x, y, z.                              !/*sz4b       51*/
  425 isef(kise+2,ji)=isef(kise+2,ji)+1                                         !/*sz4b       52*/
      k=isef(kise+1,ji)-isef(kise+2,ji)                                         !/*sz4b       53*/
      wgt=sw                                                                    !/*sz4b       54*/
      ns=nnpos                                                                  !/*sz4b       55*/
      if(one*max(k,10).gt.srv(1,18)*isef(kise+1,ji))go to 45                    !/*sz4b       56*/
      go to 580                                                                 !/*sz4b       57*/
!                                                                               !/*sz        261*/
  430 continue                                                                  !/*sz        262*/
!                                                                               !/*sz        263*/
!        finish sampling the direction.                                         !/*sz        264*/
      if(id.ne.0)go to 450                                                      !/*sz        265*/
      if(jsu.ne.0)go to 440                                                     !/*sz        266*/
      call isos(uuu,lev)                                                        !/*sz4b       58*/
      go to 470                                                                 !/*sz        268*/
  440 sff(1,5)=sqrt(rang())                                                     !/*sz        269*/
  450 if(iv.ne.0)go to 460                                                      !/*sz        270*/
      call expirx(1,'sourcb','dir specified but vec absent.')                   !/*sz        271*/
      return                                                                    !/*sz        272*/
  460 call rotas(sff(1,5)*sign(one,sff(1,15)),sff(1,13),uuu,0,irt)              !/*sz4b       59*/
      if(kdb.ne.0)return                                                        !/*sz        274*/
      ipsc=10                                                                   !/*sz        275*/
      if(imd.eq.3.and.abs(sff(1,5)).eq.1.)ipsc=11                               !/*sz        276*/
!                                                                               !/*sz        277*/
!        default for cel:  find the cell that contains xyz.                     !/*sz        278*/
  470 if(icl.gt.0)go to 560                                                     !/*sz        279*/
      if(icl.lt.0)go to 550                                                     !/*sz        280*/
      if(junf.ne.0)go to 500                                                    !/*sz        281*/
      do 480 m=1,nlse                                                           !/*sz        282*/
      icl=lse(klse+m)                                                           !/*sz        283*/
      call chkcel(icl,2,j)                                                      !/*sz        284*/
  480 if(j.eq.0)return                                                          !/*sz        285*/
      do 490 icl=1,mxa                                                          !/*sz        286*/
      call chkcel(icl,2,j)                                                      !/*sz        287*/
  490 if(j.eq.0)go to 540                                                       !/*sz        288*/
      go to 530                                                                 !/*sz        289*/
  500 do 510 m=1,nlse                                                           !/*sz        290*/
      icl=lse(klse+m)                                                           !/*sz        291*/
      if(jun(ljun+icl).ne.0)go to 510                                           !/*sz        292*/
      call chkcel(icl,2,j)                                                      !/*sz        293*/
      if(j.ne.0)go to 510                                                       !/*sz        294*/
      if(mfl(lmfl+1,icl).ne.0)call levcel                                       !/*sz        295*/
      return                                                                    !/*sz        296*/
  510 continue                                                                  !/*sz        297*/
      do 520 icl=1,mxa                                                          !/*sz        298*/
      if(jun(ljun+icl).ne.0)go to 520                                           !/*sz        299*/
      call chkcel(icl,2,j)                                                      !/*sz        300*/
      if(j.eq.0)go to 540                                                       !/*sz        301*/
  520 continue                                                                  !/*sz        302*/
  530 call expirx(1,'sourcb','source point is not in any cell.')                !/*sz        303*/
      return                                                                    !/*sz        304*/
  540 nlse=nlse+1                                                               !/*sz        305*/
      lse(klse+nlse)=icl                                                        !/*sz        306*/
      if(junf.eq.0)return                                                       !/*sz        307*/
      if(mfl(lmfl+1,icl).eq.0)return                                            !/*sz        308*/
  550 call levcel                                                               !/*sz        309*/
      return                                                                    !/*sz        310*/
!                                                                               !/*sz        311*/
  560 if(krflg.eq.0)return                                                      !/*sz        312*/
      call chkcel(icl,2,j)                                                      !/*sz        313*/
      if(j.ne.0)call errprn(1,-1,0,zero,zero,' ',' ',                           !/*sz4c2       1*/
     & 'the source point is not in the source cell.')                           !/*sz4c2       2*/
      return                                                                    !/*sz        316*/
!                                                                               !/*sz        322*/
!        quit if efficiency of covering surface or cell is too low.             !/*sz        323*/
#ifdef MULTT                                                                    /*!szxk        1*/
      call thread_lockon(jlock)                                                 !/*szxk        2*/
#endif                                                                          /*!szxk        3*/
  580 write(iuo,590)k,isef(kise+1,ji),ncl(lncl+ji)                              !/*sz4b       60*/
  590 format(/5h only,i8,13h successes in,i10,21h tries in source cell,         !/*sz        326*/
     & i6)                                                                      !/*sz        327*/
      write(jtty,595)ic(1),ksd(lksd+1,ivdis(1)),hbln(1,1),xxx,yyy,zzz           !/*sz4c2       3*/
      write(iuo,595)ic(1),ksd(lksd+1,ivdis(1)),hbln(1,1),xxx,yyy,zzz            !/*sz4c2       4*/
  595 format(/6h entry,i5,6h on SI,i3,25h for independant variable,a3,          !/*sz4c2       5*/
     & 29h at the last sampled position,1p3e13.5,19h appears incorrect.)        !/*sz4c2       6*/
      write(hc,'(i5)')ncl(lncl+ji)                                              !/*sz        328*/
      call expirx(1,'sourcb',                                                   !/*sz        329*/
     & 'the sampling efficiency in source cell '//hc//' is too low.')           !/*sz        330*/
      return                                                                    !/*sz        331*/
!                                                                               !/*sz4b       61*/
!        quit if the source path is invalid.                                    !/*sz4b       62*/
  600 if(l.eq.-1)call expirx(1,'sourcb',                                        !/*sz4b       63*/
     & 'path above x,y,z level must be fully specified.')                       !/*sz4b       64*/
      if(l.eq.-2)call expirx(1,'sourcb',                                        !/*sz4b       65*/
     & 'invalid repeated structures source path.')                              !/*sz4b       66*/
      if(l.eq.-3)call expirx(1,'sourcb',                                        !/*sz4b       67*/
     & 'zero entry in source path could not be identified.')                    !/*sz4b       68*/
      if(l.eq.-4)call expirx(1,'sourcb',                                        !/*sz4b       69*/
     & 'zero entry in source path can not be a lattice.')                       !/*sz4b       70*/
      if(l.eq.-5)call expirx(1,'sourcb',                                        !/*sz4b       71*/
     & 'source is not in the specified lattice element.')                       !/*sz4b       72*/
      return                                                                    !/*sz4b       73*/
      end                                                                       !/*sz        332*/

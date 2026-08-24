!#ident track.F from deck tr                                                    !/*tr          1*/
      subroutine track(ih)                                                      !/*tr          2*/
!        find the next bounding surface of cell ih or of a higher-level         !/*tr          3*/
!        cell, and calculate its distance dls.                                  !/*tr          4*/
!        if ih>mxa, for a set of tally segmenting surfaces, just return         !/*tr          5*/
!        the intersections in dti.                                              !/*tr          6*/
!        this subroutine is used a lot.  some of the funny-looking              !/*tr          7*/
!        coding has been done that way for speed.                               !/*tr          8*/
      use global_data                                                           !/*trx         1*/
!                                                                               !/*trx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*trx         3*/
      implicit integer(kindi) (i-n)                                             !/*trx         4*/
!                                                                               !/*trx         5*/
      dimension dl(0:mxlv),jp(0:mxlv)                                           !/*tr         10*/
!                                                                               !/*tr         11*/
!        set up to do the cell in the current level.                            !/*tr         12*/
#ifdef PCDOS                                                                    /*!tr4a        1*/
      if(lockl)call pttyin                                                      !/*tr4a        2*/
#endif                                                                          /*!cpp       268*/
#if !defined(MULTT) && !defined(MULTP)                                          /*!trxk        1*/
      if(irup.ne.0)call qttyin(-1,'5hnps =,i9,5x,12hcollisions =,i6')           !/*tr4a        4*/
#endif                                                                          /*!cpp       269*/
      ll=lev                                                                    !/*tr         13*/
      ic=ih                                                                     !/*tr         14*/
!                                                                               !/*tr         15*/
!        find the distance to boundary, dl, in this cell.                       !/*tr         16*/
   10 dl(ll)=huge                                                               !/*tr         17*/
      nlt=0                                                                     !/*tr         18*/
!                                                                               !/*tr         19*/
!        calculate all intersections with all surfaces of this cell.            !/*tr         20*/
      j2=abs(lca(llca+ic))                                                      !/*tr         21*/
      il=j2-lca(llca+ic)                                                        !/*tr         22*/
      j3=abs(lca(llca+ic+1))-1                                                  !/*tr         23*/
      j1=j2-1                                                                   !/*tr         24*/
   20 j1=j1+1                                                                   !/*tr         25*/
      if(j1.gt.j3)go to 320                                                     !/*tr         26*/
!                                                                               !/*tr         27*/
!        ignore logical operators and duplicate surfaces.                       !/*tr         28*/
      if(lcaj(klcj+j1).lt.0)go to 20                                            !/*tr         29*/
      j=abs(lja(llja+j1))                                                       !/*tr         30*/
      k=kst(lkst+j)                                                             !/*tr4a        5*/
      if(k.le.4.and.j.eq.jsu)go to 20                                           !/*tr         32*/
      i=lsc(llsc+j)+1                                                           !/*tr         33*/
      d1=-1.                                                                    !/*tr         34*/
      d2=-1.                                                                    !/*tr         35*/
      tk=0.                                                                     !/*tr         36*/
      t9=1.e10                                                                  !/*tr         37*/
      go to(30,40,40,40,50,60,70,80,90,100,110,120,130,140,150,160,             !/*tr         38*/
     & 170,180,190,200,210,220,230,240,240,240)k                                !/*tr         39*/
!                                                                               !/*tr         40*/
! >>>>>  planes:  p, px, py, pz                                                 !/*tr         41*/
!        if jsu<0 check for coincident surface at higher level.                 !/*tr4b        1*/
   30 t1=uuu*scf(i)+vvv*scf(i+1)+www*scf(i+2)                                   !/*tr         42*/
      if(t1.eq.0.)go to 20                                                      !/*tr         43*/
      d1=(scf(i+3)-xxx*scf(i)-yyy*scf(i+1)-zzz*scf(i+2))/t1                     !/*tr         44*/
      if(d1.le.0.)go to 20                                                      !/*tr         45*/
      if(jsu.ge.0)go to 300                                                     !/*tr4b        2*/
      if(d1.gt.coincd)go to 300                                                 !/*tr4b        3*/
      if(ksc(lksc+j).ne.ksc(lksc-jsu))go to 300                                 !/*tr4b        4*/
      if(t1*lja(llja+j1).gt.0..or.il.ne.0)go to 20                              !/*tr4b        5*/
      go to 300                                                                 !/*tr         46*/
   40 if(gpblcm(k+2).eq.0.)go to 20                                             !/*tr         47*/
      d1=(scf(i)-gpblcm(k-1))/gpblcm(k+2)                                       !/*tr         48*/
      if(d1.le.0.)go to 20                                                      !/*tr         49*/
      if(jsu.ge.0)go to 300                                                     !/*tr4b        6*/
      if(d1.gt.coincd)go to 300                                                 !/*tr4b        7*/
      if(ksc(lksc+j).ne.ksc(lksc-jsu))go to 300                                 !/*tr4b        8*/
      if(gpblcm(k+2)*lja(llja+j1).gt.0..or.il.ne.0)go to 20                     !/*tr4b        9*/
      go to 300                                                                 !/*tr         50*/
!                                                                               !/*tr         51*/
! >>>>>  spheres:  so, s, sx, sy, sz                                            !/*tr         52*/
   50 a1=xxx*uuu+yyy*vvv+zzz*www                                                !/*tr         53*/
      b1=xxx**2+yyy**2+zzz**2-scf(i)                                            !/*tr         54*/
      go to 260                                                                 !/*tr         55*/
   60 t1=xxx-scf(i)                                                             !/*tr         56*/
      t2=yyy-scf(i+1)                                                           !/*tr         57*/
      t3=zzz-scf(i+2)                                                           !/*tr         58*/
      a1=t1*uuu+t2*vvv+t3*www                                                   !/*tr         59*/
      b1=t1**2+t2**2+t3**2-scf(i+3)                                             !/*tr         60*/
      go to 260                                                                 !/*tr         61*/
   70 t1=xxx-scf(i)                                                             !/*tr         62*/
      a1=t1*uuu+yyy*vvv+zzz*www                                                 !/*tr         63*/
      b1=t1**2+yyy**2+zzz**2-scf(i+1)                                           !/*tr         64*/
      go to 260                                                                 !/*tr         65*/
   80 t2=yyy-scf(i)                                                             !/*tr         66*/
      a1=xxx*uuu+t2*vvv+zzz*www                                                 !/*tr         67*/
      b1=xxx**2+t2**2+zzz**2-scf(i+1)                                           !/*tr         68*/
      go to 260                                                                 !/*tr         69*/
   90 t3=zzz-scf(i)                                                             !/*tr         70*/
      a1=xxx*uuu+yyy*vvv+t3*www                                                 !/*tr         71*/
      b1=xxx**2+yyy**2+t3**2-scf(i+1)                                           !/*tr         72*/
      go to 260                                                                 !/*tr         73*/
!                                                                               !/*tr         74*/
! >>>>>  cylinders:  c/x, c/y, c/z, cx, cy, cz                                  !/*tr         75*/
  100 t1=vvv**2+www**2                                                          !/*tr         76*/
      if(t1.eq.0.)go to 20                                                      !/*tr         77*/
      t1=1./t1                                                                  !/*tr         78*/
      t2=yyy-scf(i)                                                             !/*tr         79*/
      t3=zzz-scf(i+1)                                                           !/*tr         80*/
      a1=(t2*vvv+t3*www)*t1                                                     !/*tr         81*/
      b1=(t2**2+t3**2-scf(i+2))*t1                                              !/*tr         82*/
      go to 260                                                                 !/*tr         83*/
  110 t1=uuu**2+www**2                                                          !/*tr         84*/
      if(t1.eq.0.)go to 20                                                      !/*tr         85*/
      t1=1./t1                                                                  !/*tr         86*/
      t2=xxx-scf(i)                                                             !/*tr         87*/
      t3=zzz-scf(i+1)                                                           !/*tr         88*/
      a1=(t2*uuu+t3*www)*t1                                                     !/*tr         89*/
      b1=(t2**2+t3**2-scf(i+2))*t1                                              !/*tr         90*/
      go to 260                                                                 !/*tr         91*/
  120 t1=uuu**2+vvv**2                                                          !/*tr         92*/
      if(t1.eq.0.)go to 20                                                      !/*tr         93*/
      t1=1./t1                                                                  !/*tr         94*/
      t2=xxx-scf(i)                                                             !/*tr         95*/
      t3=yyy-scf(i+1)                                                           !/*tr         96*/
      a1=(t2*uuu+t3*vvv)*t1                                                     !/*tr         97*/
      b1=(t2**2+t3**2-scf(i+2))*t1                                              !/*tr         98*/
      go to 260                                                                 !/*tr         99*/
  130 t1=vvv**2+www**2                                                          !/*tr        100*/
      if(t1.eq.0.)go to 20                                                      !/*tr        101*/
      t1=1./t1                                                                  !/*tr        102*/
      a1=(yyy*vvv+zzz*www)*t1                                                   !/*tr        103*/
      b1=(yyy**2+zzz**2-scf(i))*t1                                              !/*tr        104*/
      go to 260                                                                 !/*tr        105*/
  140 t1=uuu**2+www**2                                                          !/*tr        106*/
      if(t1.eq.0.)go to 20                                                      !/*tr        107*/
      t1=1./t1                                                                  !/*tr        108*/
      a1=(xxx*uuu+zzz*www)*t1                                                   !/*tr        109*/
      b1=(xxx**2+zzz**2-scf(i))*t1                                              !/*tr        110*/
      go to 260                                                                 !/*tr        111*/
  150 t1=uuu**2+vvv**2                                                          !/*tr        112*/
      if(t1.eq.0.)go to 20                                                      !/*tr        113*/
      t1=1./t1                                                                  !/*tr        114*/
      a1=(xxx*uuu+yyy*vvv)*t1                                                   !/*tr        115*/
      b1=(xxx**2+yyy**2-scf(i))*t1                                              !/*tr        116*/
      go to 260                                                                 !/*tr        117*/
!                                                                               !/*tr        118*/
! >>>>>  cones:  k/x, k/y, k/z, kx, ky, kz                                      !/*tr        119*/
  160 t4=vvv**2+www**2-scf(i+3)*uuu**2                                          !/*tr        120*/
      if(abs(t4).gt.1.e-10)t9=1./t4                                             !/*tr        121*/
      t1=yyy-scf(i+1)                                                           !/*tr        122*/
      t2=zzz-scf(i+2)                                                           !/*tr        123*/
      t3=xxx-scf(i)                                                             !/*tr        124*/
      uu=uuu                                                                    !/*tr        125*/
      tk=scf(i+4)                                                               !/*tr        126*/
      a1=(t1*vvv+t2*www-t3*uuu*scf(i+3))*t9                                     !/*tr        127*/
      b1=(t1**2+t2**2-scf(i+3)*t3**2)*t9                                        !/*tr        128*/
      if(t9.ne.1.e10)go to 260                                                  !/*tr        129*/
      go to 250                                                                 !/*tr        130*/
  170 t4=uuu**2+www**2-scf(i+3)*vvv**2                                          !/*tr        131*/
      if(abs(t4).gt.1.e-10)t9=1./t4                                             !/*tr        132*/
      t1=xxx-scf(i)                                                             !/*tr        133*/
      t2=zzz-scf(i+2)                                                           !/*tr        134*/
      t3=yyy-scf(i+1)                                                           !/*tr        135*/
      uu=vvv                                                                    !/*tr        136*/
      tk=scf(i+4)                                                               !/*tr        137*/
      a1=(t1*uuu+t2*www-t3*vvv*scf(i+3))*t9                                     !/*tr        138*/
      b1=(t1**2+t2**2-scf(i+3)*t3**2)*t9                                        !/*tr        139*/
      if(t9.ne.1.e10)go to 260                                                  !/*tr        140*/
      go to 250                                                                 !/*tr        141*/
  180 t4=uuu**2+vvv**2-scf(i+3)*www**2                                          !/*tr        142*/
      if(abs(t4).gt.1.e-10)t9=1./t4                                             !/*tr        143*/
      t1=yyy-scf(i+1)                                                           !/*tr        144*/
      t2=xxx-scf(i)                                                             !/*tr        145*/
      t3=zzz-scf(i+2)                                                           !/*tr        146*/
      uu=www                                                                    !/*tr        147*/
      tk=scf(i+4)                                                               !/*tr        148*/
      a1=(t1*vvv+t2*uuu-t3*www*scf(i+3))*t9                                     !/*tr        149*/
      b1=(t1**2+t2**2-scf(i+3)*t3**2)*t9                                        !/*tr        150*/
      if(t9.ne.1.e10)go to 260                                                  !/*tr        151*/
      go to 250                                                                 !/*tr        152*/
  190 t4=vvv**2+www**2-scf(i+1)*uuu**2                                          !/*tr        153*/
      if(abs(t4).gt.1.e-10)t9=1./t4                                             !/*tr        154*/
      t3=xxx-scf(i)                                                             !/*tr        155*/
      uu=uuu                                                                    !/*tr        156*/
      tk=scf(i+2)                                                               !/*tr        157*/
      a1=(zzz*www+yyy*vvv-t3*uuu*scf(i+1))*t9                                   !/*tr        158*/
      b1=(zzz**2+yyy**2-scf(i+1)*t3**2)*t9                                      !/*tr        159*/
      if(t9.ne.1.e10)go to 260                                                  !/*tr        160*/
      go to 250                                                                 !/*tr        161*/
  200 t4=uuu**2+www**2-scf(i+1)*vvv**2                                          !/*tr        162*/
      if(abs(t4).gt.1.e-10)t9=1./t4                                             !/*tr        163*/
      t3=yyy-scf(i)                                                             !/*tr        164*/
      uu=vvv                                                                    !/*tr        165*/
      tk=scf(i+2)                                                               !/*tr        166*/
      a1=(xxx*uuu+zzz*www-t3*vvv*scf(i+1))*t9                                   !/*tr        167*/
      b1=(xxx**2+zzz**2-scf(i+1)*t3**2)*t9                                      !/*tr        168*/
      if(t9.ne.1.e10)go to 260                                                  !/*tr        169*/
      go to 250                                                                 !/*tr        170*/
  210 t4=uuu**2+vvv**2-scf(i+1)*www**2                                          !/*tr        171*/
      if(abs(t4).gt.1.e-10)t9=1./t4                                             !/*tr        172*/
      t3=zzz-scf(i)                                                             !/*tr        173*/
      uu=www                                                                    !/*tr        174*/
      tk=scf(i+2)                                                               !/*tr        175*/
      a1=(xxx*uuu+yyy*vvv-t3*www*scf(i+1))*t9                                   !/*tr        176*/
      b1=(xxx**2+yyy**2-scf(i+1)*t3**2)*t9                                      !/*tr        177*/
      if(t9.ne.1.e10)go to 260                                                  !/*tr        178*/
      go to 250                                                                 !/*tr        179*/
!                                                                               !/*tr        180*/
! >>>>>  special quadratic:  sq                                                 !/*tr        181*/
  220 t1=scf(i)*uuu**2+scf(i+1)*vvv**2+scf(i+2)*www**2                          !/*tr        182*/
      if(abs(t1).gt.1.e-10)t9=1./t1                                             !/*tr        183*/
      t2=xxx-scf(i+7)                                                           !/*tr        184*/
      t3=yyy-scf(i+8)                                                           !/*tr        185*/
      t4=zzz-scf(i+9)                                                           !/*tr        186*/
      t5=scf(i)*t2+scf(i+3)                                                     !/*tr        187*/
      t6=scf(i+1)*t3+scf(i+4)                                                   !/*tr        188*/
      t7=scf(i+2)*t4+scf(i+5)                                                   !/*tr        189*/
      a1=(t5*uuu+t6*vvv+t7*www)*t9                                              !/*tr        190*/
      b1=((t5+scf(i+3))*t2+(t6+scf(i+4))*t3+(t7+scf(i+5))*t4+                   !/*tr        191*/
     & scf(i+6))*t9                                                             !/*tr        192*/
      if(t9.ne.1.e10)go to 260                                                  !/*tr        193*/
      go to 250                                                                 !/*tr        194*/
!                                                                               !/*tr        195*/
! >>>>>  general quadratic:  gq                                                 !/*tr        196*/
  230 t1=scf(i)*uuu**2+scf(i+1)*vvv**2+scf(i+2)*www**2+                         !/*tr        197*/
     & (scf(i+3)*uuu+scf(i+4)*www)*vvv+scf(i+5)*uuu*www                         !/*tr        198*/
      if(abs(t1).gt.1.e-10)t9=1./t1                                             !/*tr        199*/
      a1=((2.*scf(i)*xxx+scf(i+6))*uuu+scf(i+3)*(xxx*vvv+yyy*uuu)+              !/*tr        200*/
     & (2.*scf(i+1)*yyy+scf(i+7))*vvv+scf(i+4)*(zzz*vvv+yyy*www)+               !/*tr        201*/
     & (2.*scf(i+2)*zzz+scf(i+8))*www+scf(i+5)*(xxx*www+zzz*uuu))*.5*t9         !/*tr        202*/
      b1=((scf(i)*xxx+scf(i+3)*yyy+scf(i+6))*xxx+                               !/*tr        203*/
     & (scf(i+1)*yyy+scf(i+4)*zzz+scf(i+7))*yyy+                                !/*tr        204*/
     & (scf(i+2)*zzz+scf(i+5)*xxx+scf(i+8))*zzz+scf(i+9))*t9                    !/*tr        205*/
      if(t9.ne.1.e10)go to 260                                                  !/*tr        206*/
      go to 250                                                                 !/*tr        207*/
!                                                                               !/*tr        208*/
! >>>>>  tori:  tx, ty, tz                                                      !/*tr        209*/
  240 call torus(ic,i,j,d1)                                                     !/*tr        210*/
      go to 290                                                                 !/*tr        212*/
!                                                                               !/*tr        213*/
!        calculate a single distance.  (rare case)                              !/*tr        214*/
  250 if(abs(a1).le.1.e-10.or.j.eq.jsu)go to 20                                 !/*tr        215*/
      d1=-.5*b1/a1                                                              !/*tr        216*/
      if(d1.le.0.)go to 20                                                      !/*tr        217*/
      go to 280                                                                 !/*tr        218*/
!                                                                               !/*tr        219*/
!        calculate a pair of distances.                                         !/*tr        220*/
  260 t1=a1**2-b1                                                               !/*tr        221*/
      if(t1.lt.0.)go to 20                                                      !/*tr        222*/
      if(j.ne.jsu)go to 270                                                     !/*tr        223*/
      d1=-2.*a1                                                                 !/*tr        224*/
      if(d1.le.0.)go to 20                                                      !/*tr        225*/
      go to 280                                                                 !/*tr        226*/
  270 t2=sqrt(t1)                                                               !/*tr        227*/
      d1=-a1+t2                                                                 !/*tr        228*/
      if(d1.le.0.)go to 20                                                      !/*tr        229*/
      d2=-a1-t2                                                                 !/*tr        230*/
!                                                                               !/*tr        231*/
!        reject the distance to the wrong sheet of a cone.                      !/*tr        232*/
  280 if(tk.eq.0.)go to 300                                                     !/*tr        233*/
      if(tk*(t3+d1*uu).lt.0.)d1=-1.                                             !/*tr        234*/
      if(tk*(t3+d2*uu).lt.0.)d2=-1.                                             !/*tr        235*/
      d1=max(d1,d2)                                                             !/*tr        236*/
  290 if(d1.le.0.)go to 20                                                      !/*tr        237*/
!                                                                               !/*tr        238*/
!        simple cell case:  select the smallest positive distance.              !/*tr        239*/
  300 if(il.ne.0)go to 310                                                      !/*tr        240*/
      if(d2.gt.0.)d1=d2                                                         !/*tr        241*/
      if(d1.ge.dl(ll))go to 20                                                  !/*tr        242*/
      jp(ll)=j                                                                  !/*tr        243*/
      dl(ll)=d1                                                                 !/*tr        244*/
      go to 20                                                                  !/*tr        245*/
!                                                                               !/*tr        246*/
!        complicated cell case:  put positive roots in dti.                     !/*tr        247*/
  310 nlt=nlt+1                                                                 !/*tr        248*/
      dti(nlt)=d1                                                               !/*tr        249*/
      iti(nlt)=j                                                                !/*tr        250*/
      if(d2.le.0..or.d1.eq.d2)go to 20                                          !/*tr        251*/
      nlt=nlt+1                                                                 !/*tr        252*/
      dti(nlt)=d2                                                               !/*tr        253*/
      iti(nlt)=j                                                                !/*tr        254*/
      go to 20                                                                  !/*tr        255*/
!                                                                               !/*tr        256*/
!        if complicated cell, find the distance to the cell boundary.           !/*tr        257*/
  320 if(il.eq.0.and.junf.eq.0)go to 460                                        !/*tr        258*/
      if(il.eq.0)go to 380                                                      !/*tr        259*/
      if(ih.gt.mxa)return                                                       !/*tr        260*/
      if(nlt.eq.0)go to 370                                                     !/*tr        261*/
  330 n1=1                                                                      !/*tr        262*/
      do 340 m=1,nlt                                                            !/*tr        263*/
  340 if(dti(m).lt.dti(n1))n1=m                                                 !/*tr        264*/
      dl(ll)=dti(n1)                                                            !/*tr        265*/
      if(dl(ll).eq.huge)go to 370                                               !/*tr        266*/
      do 350 j1=j2,j3                                                           !/*tr        268*/
  350 if(abs(lja(llja+j1)).eq.iti(n1))lgc(j1-j2+1)=1-lgc(j1-j2+1)               !/*tr        269*/
      dti(n1)=huge                                                              !/*tr        270*/
      if(lgeval(lgc,j3-j2+1).ne.0)go to 330                                     !/*tr        271*/
  360 jp(ll)=iti(n1)                                                            !/*tr        272*/
!                                                                               !/*tr        273*/
!        prepare to do the cell in the next higher level.                       !/*tr        274*/
!        set jsu=-jsu for higher levels to check coincidence surfaces.          !/*tr4b       10*/
  370 if(junf.eq.0)go to 460                                                    !/*tr        275*/
  380 if(jun(ljun+ic).le.0)go to 420                                            !/*tr        276*/
      if(ll.ne.lev)go to 400                                                    !/*tr        277*/
      do 390 i=1,6                                                              !/*tr        278*/
  390 udt(i,lev)=gpblcm(i)                                                      !/*tr        279*/
      js=jsu                                                                    !/*tr4b       11*/
      if(jsu.eq.0)go to 400                                                     !/*tr4b       12*/
      if(ksc(lksc+jsu).ne.0)jsu=-js                                             !/*tr4b       13*/
  400 ll=ll-1                                                                   !/*tr        280*/
      do 410 i=1,6                                                              !/*tr        281*/
  410 gpblcm(i)=udt(i,ll)                                                       !/*tr        282*/
      ic=udt(7,ll)                                                              !/*tr        283*/
      if(lca(llca+ic).lt.0)call chkcel(ic,3,j)                                  !/*tr4b       14*/
      go to 10                                                                  !/*tr        285*/
  420 if(ll.eq.lev)go to 440                                                    !/*tr        286*/
      do 430 i=1,6                                                              !/*tr        287*/
  430 gpblcm(i)=udt(i,lev)                                                      !/*tr        288*/
      jsu=js                                                                    !/*tr4b       15*/
!                                                                               !/*tr        289*/
!        find the level where the distance to boundary is least.                !/*tr        290*/
  440 levp=ll                                                                   !/*tr        291*/
      do 450 i=ll+1,lev                                                         !/*tr4b       16*/
      if(dl(i).ge.dl(levp))go to 450                                            !/*tr4b       17*/
      if(dl(i)+coincd.lt.dl(levp))go to 445                                     !/*tr4c        1*/
      if(ksc(lksc+jp(i)).eq.0)go to 445                                         !/*tr4b       19*/
      if(ksc(lksc+jp(i)).eq.ksc(lksc+jp(levp)))go to 450                        !/*tr4b       20*/
  445 levp=i                                                                    !/*tr4b       21*/
  450 continue                                                                  !/*tr4b       22*/
  460 dls=dl(levp)                                                              !/*tr        294*/
      jap=jp(levp)                                                              !/*tr        295*/
!                                                                               !/*tr        296*/
!        return with error indicator if no intersection was found.              !/*tr        297*/
      if(dls.eq.huge)kdb=2                                                      !/*tr        298*/
      return                                                                    !/*tr        299*/
      end                                                                       !/*tr        300*/

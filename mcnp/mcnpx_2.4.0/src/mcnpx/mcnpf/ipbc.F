!#ident ipbc.F from deck bc4a                                                   !/*bc4a        1*/
      subroutine ipbc                                                           !/*bc4a        2*/
!        initiation for periodic boundary conditions.                           !/*bc4a        3*/
      use global_data                                                           !/*bcx         1*/
      use imcn_data                                                             !/*bcx         2*/
!                                                                               !/*bcx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*bcx         4*/
      implicit integer(kindi) (i-n)                                             !/*bcx         5*/
!                                                                               !/*bcx         6*/
!                                                                               !/*bc4a        6*/
!        set up trf and ksu array pointers.                                     !/*bc4a        7*/
      mx=mxtr+1                                                                 !/*bc4a        8*/
      do 20 j1=1,mxj                                                            !/*bc4a        9*/
      if(ksu(lksu+j1).le.0)go to 20                                             !/*bc4a       10*/
      if(kst(lkst+j1).gt.4)call erprnt(1,1,1,nsf(lnsf+j1),0,0,0,0,              !/*bc4a       11*/
     & '7hsurface,i5,35h is nonplanar.  cannot be periodic.')                   !/*bc4a       12*/
      j2=namchg(2,ksu(lksu+j1))                                                 !/*bc4a       13*/
      if(j2.eq.0)call erprnt(1,1,2,nsf(lnsf+j1),ksu(lksu+j1),0,0,0,             !/*bc4a       14*/
     & '7hsurface,i5,32h cannot be periodic with surface,i5')                   !/*bc4a       15*/
      mxtr=mxtr+1                                                               !/*bc4a       16*/
      trf(ltrf+1,mxtr)=j2                                                       !/*bc4a       17*/
      ksu(lksu+j1)=mxtr                                                         !/*bc4a       18*/
!                                                                               !/*bc4a       19*/
!        load surface coefficients into trf(ltrf+4+i,k), trf(ltrf+4,k).         !/*bc4a       20*/
!        trf(ltrf+4+i,mxtr) is also the surface normal vector.                  !/*bc4a       21*/
      ix=lsc(llsc+j1)                                                           !/*bc4a       22*/
      if(kst(lkst+j1).eq.1)t1=1./sqrt(dotpro(scf(ix+1),scf(ix+1)))              !/*bc4a       23*/
      do 10 i=1,3                                                               !/*bc4a       24*/
      trf(ltrf+4+i,mxtr)=0.                                                     !/*bc4a       25*/
   10 if(kst(lkst+j1).eq.1)trf(ltrf+4+i,mxtr)=t1*scf(ix+i)                      !/*bc4a       26*/
      if(kst(lkst+j1).gt.1)trf(ltrf+3+kst(lkst+j1),mxtr)=1.                     !/*bc4a       27*/
      if(kst(lkst+j1).gt.1)trf(ltrf+4,mxtr)=scf(ix+1)                           !/*bc4a       28*/
      if(kst(lkst+j1).eq.1)trf(ltrf+4,mxtr)=t1*scf(ix+4)                        !/*bc4a       29*/
   20 continue                                                                  !/*bc4a       30*/
!                                                                               !/*bc4a       31*/
!        find tpp(i), periodic surface intersection and system vertical.        !/*bc4a       32*/
      jd=1                                                                      !/*bc4a       33*/
      do 60 m1=mx,mxtr                                                          !/*bc4a       34*/
      do 50 m2=m1+1,mxtr                                                        !/*bc4a       35*/
      call crspro(trf(ltrf+5,m1),trf(ltrf+5,m2),tpp(jd))                        !/*bc4a       36*/
      if(abs(dotpro(tpp(1),tpp(jd))).lt.1.e-6)go to 50                          !/*bc4a       37*/
      if(abs(abs(dotpro(tpp(jd),tpp(jd)))-1.).lt.1.e-6)go to 40                 !/*bc4a       38*/
      t1=1./sqrt(abs(dotpro(tpp(jd),tpp(jd))))                                  !/*bc4a       39*/
      do 30 i=jd,jd+2                                                           !/*bc4a       40*/
   30 tpp(i)=t1*tpp(i)                                                          !/*bc4a       41*/
   40 if(abs(abs(dotpro(tpp(1),tpp(jd)))-1.).gt.1.e-6)go to 360                 !/*bc4a       42*/
      jd=4                                                                      !/*bc4a       43*/
   50 continue                                                                  !/*bc4a       44*/
   60 continue                                                                  !/*bc4a       45*/
      if(jd.eq.1)go to 360                                                      !/*bc4a       46*/
!                                                                               !/*bc4a       47*/
!        check reciprocity.                                                     !/*bc4a       48*/
      db=huge                                                                   !/*bc4a       49*/
      dt=-huge                                                                  !/*bc4a       50*/
      do 100 j1=1,mxj                                                           !/*bc4a       51*/
      if(kst(lkst+j1).gt.4.or.ksu(lksu+j1).eq.-3)go to 100                      !/*bc4a       52*/
      if(ksu(lksu+j1).le.0)go to 70                                             !/*bc4a       53*/
      j2=nint(trf(ltrf+1,ksu(lksu+j1)))                                         !/*bc4a       54*/
      j3=nint(trf(ltrf+1,ksu(lksu+j2)))                                         !/*bc4a       55*/
      if(j1.ne.j3)call erprnt(1,1,2,nsf(lnsf+j2),nsf(lnsf+j1),0,0,0,            !/*bc4a       56*/
     & '7hsurface,i5,29h is not periodic with surface,i5')                      !/*bc4a       57*/
      go to 100                                                                 !/*bc4a       58*/
!                                                                               !/*bc4a       59*/
!        find dt and db, geometry top and bottom along vertical, tpp(i).        !/*bc4a       60*/
   70 ix=lsc(llsc+j1)                                                           !/*bc4a       61*/
      if(kst(lkst+j1).gt.1)go to 80                                             !/*bc4a       62*/
      t1=dotpro(scf(ix+1),scf(ix+1))                                            !/*bc4a       63*/
      t2=dotpro(scf(ix+1),tpp(1))                                               !/*bc4a       64*/
      if(abs(t2**2-t1).gt.1.e-6*t1)go to 100                                    !/*bc4a       65*/
      t3=sign(scf(ix+4)/sqrt(t1),t2)                                            !/*bc4a       66*/
      go to 90                                                                  !/*bc4a       67*/
   80 if(abs(abs(tpp(kst(lkst+j1)-1))-1.).gt.1.e-6)go to 100                    !/*bc4a       68*/
      t3=scf(ix+1)*tpp(kst(lkst+j1)-1)                                          !/*bc4a       69*/
   90 db=min(t3,db)                                                             !/*bc4a       70*/
      dt=max(t3,dt)                                                             !/*bc4a       71*/
  100 continue                                                                  !/*bc4a       72*/
      dh=.5*(dt+db)                                                             !/*bc4a       73*/
!                                                                               !/*bc4a       74*/
!        get trf(ltrf+7+i,k), perimeter vector in surface jsu                   !/*bc4a       75*/
!        orthogonal to normal, trf(ltrf+4+i,k), and vertical, tpp(i).           !/*bc4a       76*/
      do 270 jsu=1,mxj                                                          !/*bc4a       77*/
      if(ksu(lksu+jsu).le.0)go to 270                                           !/*bc4a       78*/
      k=ksu(lksu+jsu)                                                           !/*bc4a       79*/
      call crspro(tpp(1),trf(ltrf+5,k),trf(ltrf+8,k))                           !/*bc4a       80*/
      if(abs(dotpro(trf(ltrf+8,k),trf(ltrf+8,k))-1.).gt.1.e-6)go to 360         !/*bc4a       81*/
!                                                                               !/*bc4a       82*/
!        set 3rd vector in jsu, trf(ltrf+10+i,k), to vertical, tpp(i).          !/*bc4a       83*/
!        find tpp(i+3), midpoint in jsu between top and bottom.                 !/*bc4a       84*/
      do 110 i=1,3                                                              !/*bc4a       85*/
      trf(ltrf+10+i,k)=tpp(i)                                                   !/*bc4a       86*/
  110 tpp(i+3)=trf(ltrf+4+i,k)*trf(ltrf+4,k)+dh*tpp(i)                          !/*bc4a       87*/
!                                                                               !/*bc4a       88*/
!        find trf(ltrf+3,m), the distance of surface m from point               !/*bc4a       89*/
!        tpp(i+3) in perimeter direction trf(ltrf+7+i,k).                       !/*bc4a       90*/
      n=1                                                                       !/*bc4a       91*/
      do 120 m=mx,mxtr                                                          !/*bc4a       92*/
      trf(ltrf+3,m)=huge                                                        !/*bc4a       93*/
      t=dotpro(trf(ltrf+5,m),trf(ltrf+8,k))                                     !/*bc4a       94*/
      if(abs(t).ge.1.e-6)trf(ltrf+3,m)=(trf(ltrf+4,m)-                          !/*bc4a       95*/
     & dotpro(trf(ltrf+5,m),tpp(4)))/t                                          !/*bc4a       96*/
  120 if(trf(ltrf+3,m).lt.trf(ltrf+3,n))n=m                                     !/*bc4a       97*/
      if(trf(ltrf+3,n).eq.huge)call erprnt(1,1,0,0,0,0,0,0,                     !/*bc4a       98*/
     & '38hnot all periodic boundaries specified.')                             !/*bc4a       99*/
!                                                                               !/*bc4a      100*/
!        move along trf(ltrf+7+i,k) perimeter direction in surface jsu.         !/*bc4a      101*/
!        gpblcm(i) is the center of each facet.                                 !/*bc4a      102*/
!        check inward/outward jsu surface normal, gpblcm(3+i).                  !/*bc4a      103*/
!        set trf(ltrf+2,k) = +1/-1 for jsu normal in/out of cell.               !/*bc4a      104*/
      d2=huge                                                                   !/*bc4a      105*/
  130 d1=trf(ltrf+3,n)                                                          !/*bc4a      106*/
      trf(ltrf+3,n)=huge                                                        !/*bc4a      107*/
      do 140 m=mx,mxtr                                                          !/*bc4a      108*/
  140 if(trf(ltrf+3,m).lt.trf(ltrf+3,n))n=m                                     !/*bc4a      109*/
      if(trf(ltrf+3,n).eq.huge)go to 250                                        !/*bc4a      110*/
      dx=.5*(d1+trf(ltrf+3,n))                                                  !/*bc4a      111*/
      do 150 i=1,3                                                              !/*bc4a      112*/
      gpblcm(i)=tpp(i+3)+dx*trf(ltrf+7+i,k)                                     !/*bc4a      113*/
  150 gpblcm(i+3)=trf(ltrf+4+i,k)                                               !/*bc4a      114*/
      jt=0                                                                      !/*bc4a      115*/
      do 180 i1=1,mxa                                                           !/*bc4a      116*/
      if(junf.ne.0)jt=abs(jun(ljun+i1))                                         !/*bc4a      117*/
      if(jt.ne.0)go to 180                                                      !/*bc4a      118*/
      do 160 jk=abs(lca(llca+i1)),abs(lca(llca+i1+1))-1                         !/*bc4a      119*/
  160 if(abs(lja(llja+jk)).eq.jsu)go to 170                                     !/*bc4a      120*/
      go to 180                                                                 !/*bc4a      121*/
  170 call chkcel(i1,2,js)                                                      !/*bc4a      122*/
      if(js.eq.0)go to 190                                                      !/*bc4a      123*/
  180 continue                                                                  !/*bc4a      124*/
      go to 130                                                                 !/*bc4a      125*/
  190 do 200 i=1,3                                                              !/*bc4a      126*/
  200 gpblcm(i+3)=-gpblcm(i+3)                                                  !/*bc4a      127*/
      jt=0                                                                      !/*bc4a      128*/
      do 230 i2=1,mxa                                                           !/*bc4a      129*/
      if(junf.ne.0)jt=abs(jun(ljun+i2))                                         !/*bc4a      130*/
      if(jt.ne.0)go to 230                                                      !/*bc4a      131*/
      do 210 jk=abs(lca(llca+i2)),abs(lca(llca+i2+1))-1                         !/*bc4a      132*/
  210 if(abs(lja(llja+jk)).eq.jsu)go to 220                                     !/*bc4a      133*/
      go to 230                                                                 !/*bc4a      134*/
  220 call chkcel(i2,2,js)                                                      !/*bc4a      135*/
      if(js.eq.0)go to 240                                                      !/*bc4a      136*/
  230 continue                                                                  !/*bc4a      137*/
      go to 130                                                                 !/*bc4a      138*/
  240 iz=0                                                                      !/*bc4a      139*/
      if(fim(lfim+1,i1).ne.0..and.fim(lfim+1,i2).eq.0.)iz=1.                    !/*bc4a      140*/
      if(fim(lfim+1,i1).eq.0..and.fim(lfim+1,i2).ne.0.)iz=-1.                   !/*bc4a      141*/
      if(iz.eq.0)go to 130                                                      !/*bc4a      142*/
      if(d2.eq.huge)d2=d1                                                       !/*bc4a      143*/
      d3=trf(ltrf+3,n)                                                          !/*bc4a      144*/
      trf(ltrf+2,k)=iz                                                          !/*bc4a      145*/
      jz=max(-iz*i1,iz*i2)                                                      !/*bc4a      146*/
      trf(ltrf+17,k)=jz                                                         !/*bc4a      147*/
      if(junf.eq.0)go to 130                                                    !/*bc4a      148*/
      if(mfl(lmfl+1,jz).ne.0)call erprnt(1,1,1,ncl(lncl+jz),0,0,0,0,            !/*bc4a      149*/
     & '20hzero-importance cell,i5,18h cannot have fill.')                      !/*bc4a      150*/
      go to 130                                                                 !/*bc4a      151*/
  250 if(d2.eq.huge)call erprnt(1,1,1,nsf(lnsf+jsu),0,0,0,0,                    !/*bc4a      152*/
     & '16hperiodic surface,i5,33h must bound zero-importance cell.')           !/*bc4a      153*/
!                                                                               !/*bc4a      154*/
!        trf(ltrf+13+i,k) is center of facet jsu.                               !/*bc4a      155*/
      do 260 i=1,3                                                              !/*bc4a      156*/
  260 trf(ltrf+13+i,k)=tpp(i+3)+.5*(d2+d3)*trf(ltrf+7+i,k)                      !/*bc4a      157*/
  270 continue                                                                  !/*bc4a      158*/
!                                                                               !/*bc4a      159*/
!        convert trf array to translation/rotation matrix, r, where the         !/*bc4a      160*/
!        matrices r*trf(m1)=trf(m2).  thus r=trf(m2)*transpose(trf(m1)).        !/*bc4a      161*/
!        ss term forces m1 and m2 to be headed in and out of cell.              !/*bc4a      162*/
      do 320 m1=mx,mxtr                                                         !/*bc4a      163*/
      m2=ksu(lksu+nint(trf(ltrf+1,m1)))                                         !/*bc4a      164*/
      if(m2.gt.m1)go to 320                                                     !/*bc4a      165*/
      t1=trf(ltrf+17,m1)                                                        !/*bc4a      166*/
      trf(ltrf+17,m1)=trf(ltrf+17,m2)                                           !/*bc4a      167*/
      trf(ltrf+17,m2)=t1                                                        !/*bc4a      168*/
      is=nint(-trf(ltrf+2,m1)*trf(ltrf+2,m2))                                   !/*bc4a      169*/
      do 290 i=1,3                                                              !/*bc4a      170*/
      do 290 j=1,3                                                              !/*bc4a      171*/
      tpp(3*i+j)=0.                                                             !/*bc4a      172*/
      do 280 k=1,3                                                              !/*bc4a      173*/
      ss=max(2*k-5,is)                                                          !/*bc4a      174*/
  280 tpp(3*i+j)=tpp(3*i+j)+ss*trf(ltrf+1+3*k+j,m2)*trf(ltrf+1+3*k+i,m1)        !/*bc4a      175*/
  290 if(abs(tpp(3*i+j)).lt.1.e-12)tpp(3*i+j)=0.                                !/*bc4a      176*/
      do 310 i=1,3                                                              !/*bc4a      177*/
      do 300 j=1,3                                                              !/*bc4a      178*/
      trf(ltrf+3*i+j+1,m1)=tpp(3*i+j)                                           !/*bc4a      179*/
  300 trf(ltrf+3*i+j+1,m2)=tpp(3*j+i)                                           !/*bc4a      180*/
      trf(ltrf+1+i,m1)=trf(ltrf+13+i,m2)-dotpro(trf(ltrf+3*i+2,m2),             !/*bc4a      181*/
     & trf(ltrf+14,m1))                                                         !/*bc4a      182*/
      trf(ltrf+1+i,m2)=trf(ltrf+13+i,m1)-dotpro(trf(ltrf+3*i+2,m1),             !/*bc4a      183*/
     & trf(ltrf+14,m2))                                                         !/*bc4a      184*/
  310 continue                                                                  !/*bc4a      185*/
  320 continue                                                                  !/*bc4a      186*/
!                                                                               !/*bc4a      187*/
!        print periodic boundary transformation/rotation relations.             !/*bc4c2       1*/
      if(ink(70).eq.0)return                                                    !/*bc4a      189*/
      write(iuo,330)                                                            !/*bc4a      190*/
  330 format(29h1periodic boundary conditions,75x,14hprint table 70//           !/*bc4a      191*/
     & 13h surface pair,5x,31htransformation/rotation matrix./)                 !/*bc4a      192*/
      do 340 j=1,mxj                                                            !/*bc4a      193*/
  340 if(ksu(lksu+j).gt.0)write(iuo,350)nsf(lnsf+j),nsf(lnsf                    !/*bc4a      194*/
     & +nint(trf(ltrf+1,ksu(lksu+j)))),(trf(ltrf+i,ksu(lksu+j)),i=2,13)         !/*bc4a      195*/
  350 format(i6,i5,1p3e14.6,3x,3e14.6/11x,3e14.6,3x,3e14.6)                     !/*bc4a      196*/
      return                                                                    !/*bc4a      197*/
!                                                                               !/*bc4a      198*/
!        error exit.                                                            !/*bc4a      199*/
  360 call erprnt(1,1,0,0,0,0,0,0,                                              !/*bc4a      200*/
     & '48hno common periodic boundary intersection vector.')                   !/*bc4a      201*/
      return                                                                    !/*bc4a      202*/
      end                                                                       !/*bc4a      203*/

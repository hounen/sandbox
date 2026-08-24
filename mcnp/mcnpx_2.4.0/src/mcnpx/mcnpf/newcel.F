!#ident newcel.F from deck nc                                                   !/*nc          1*/
      subroutine newcel(cs)                                                     !/*nc          2*/
!        find the new cell, iap, into which the particle has crossed            !/*nc          3*/
!        from cell icl through surface jsu.  if cs .ne. 0, return the           !/*nc          4*/
!        angle between the track and the normal to surface jsu.                 !/*nc          5*/
      use global_data                                                           !/*ncx         1*/
!                                                                               !/*ncx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ncx         3*/
      implicit integer(kindi) (i-n)                                             !/*ncx         4*/
!                                                                               !/*ncx         5*/
!                                                                               !/*nc          7*/
!        shift to the higher level if one was found in track.                   !/*nc          8*/
      if(levp.eq.lev)go to 10                                                   !/*nc          9*/
      lev=levp                                                                  !/*nc         10*/
      xxx=udt(1,lev)                                                            !/*nc         11*/
      yyy=udt(2,lev)                                                            !/*nc         12*/
      zzz=udt(3,lev)                                                            !/*nc         13*/
      uuu=udt(4,lev)                                                            !/*nc         14*/
      vvv=udt(5,lev)                                                            !/*nc         15*/
      www=udt(6,lev)                                                            !/*nc         16*/
      icl=udt(7,lev)                                                            !/*nc         17*/
      iii=udt(8,lev)                                                            !/*nc         18*/
      jjj=udt(9,lev)                                                            !/*nc         19*/
      kkk=udt(10,lev)                                                           !/*nc         20*/
   10 if(cs.ne.0.)cs=angl()                                                     !/*nc         21*/
!                                                                               !/*nc         22*/
!        find surface crossed among the bounding surfaces of cell icl.          !/*nc         23*/
      do 20 i1=abs(lca(llca+icl)),abs(lca(llca+icl+1))-1                        !/*nc         24*/
   20 if(abs(lja(llja+i1)).eq.jsu)go to 30                                      !/*nc         25*/
      if(iovr.eq.2)kdb=1                                                        !/*nc4c        1*/
      if(iovr.eq.2)return                                                       !/*nc4c        2*/
      call expirx(1,'newcel',                                                   !/*nc         26*/
     & 'the surface crossed is not a surface of this cell.')                    !/*nc         27*/
      return                                                                    !/*nc         28*/
   30 j2=lcaj(klcj+i1)                                                          !/*nc         29*/
      j3=abs(lcaj(klcj+i1+1))-1                                                 !/*nc         30*/
!                                                                               !/*nc         31*/
!        move into the next lattice element if icl is a lattice.                !/*nc         32*/
      if(junf.eq.0)go to 40                                                     !/*nc         33*/
      if(lat(llat+1,icl).eq.0)go to 40                                          !/*nc         34*/
      call movlat(i1)                                                           !/*nc         35*/
      if(iap.eq.-12345)go to 380                                                !/*nc4a        1*/
      j1=j2-1                                                                   !/*nc         36*/
      go to 210                                                                 !/*nc         37*/
!                                                                               !/*nc         38*/
!        look for the new cell in the laj list.                                 !/*nc         39*/
!        after the first 50 histories, omit the cell check if the cell          !/*nc         40*/
!        is simple and alone on the other side and if krflg indicates           !/*nc         41*/
!        that no geometry error has just occurred.                              !/*nc         42*/
   40 if(j3.lt.j2)go to 70                                                      !/*nc         43*/
      if(laj(j2).gt.0)go to 50                                                  !/*nc         44*/
      iap=abs(laj(j2))                                                          !/*nc         45*/
      j1=j2                                                                     !/*nc         46*/
      if(npstc.gt.50.and.krflg.lt.2)go to 210                                   !/*nc         47*/
   50 iap=0                                                                     !/*nc         48*/
      do 60 j1=j2,j3                                                            !/*nc         49*/
      if(iap.eq.1000001.and.laj(j1).ne.1000002)go to 60                         !/*nc         50*/
      iap=abs(laj(j1))                                                          !/*nc         51*/
      if(iap.gt.1000000)go to 60                                                !/*nc         52*/
      call chkcel(iap,0,j)                                                      !/*nc         53*/
      if(j.eq.0)go to 210                                                       !/*nc         54*/
   60 continue                                                                  !/*nc         55*/
!                                                                               !/*nc         56*/
!        search the overflow region at the end of laj.                          !/*nc         57*/
   70 do 90 j1=klaj+mlaj+1,klaj+nlaj                                            !/*nc         58*/
      iap=laj(j1)                                                               !/*nc         59*/
      if(iap.eq.icl)go to 90                                                    !/*nc         60*/
      if(junf.eq.0)go to 80                                                     !/*nc         61*/
      if(abs(jun(ljun+iap)).ne.abs(jun(ljun+icl)))go to 90                      !/*nc         62*/
   80 call chkcel(iap,0,j)                                                      !/*nc         63*/
      if(j.eq.0)go to 210                                                       !/*nc         64*/
   90 continue                                                                  !/*nc         65*/
!                                                                               !/*nc         66*/
!        look for the new cell in the entire list of cells.                     !/*nc         67*/
      do 130 iap=1,mxa                                                          !/*nc         68*/
      if(iap.eq.icl)go to 130                                                   !/*nc         69*/
      if(junf.eq.0)go to 100                                                    !/*nc         70*/
      if(abs(jun(ljun+iap)).ne.abs(jun(ljun+icl)))go to 130                     !/*nc         71*/
  100 do 110 i4=abs(lca(llca+iap)),abs(lca(llca+iap+1))-1                       !/*nc         72*/
  110 if(abs(lja(llja+i4)).eq.jsu)go to 120                                     !/*nc         73*/
      go to 130                                                                 !/*nc         74*/
  120 call chkcel(iap,0,j)                                                      !/*nc         75*/
      if(j.eq.0)go to 140                                                       !/*nc         76*/
  130 continue                                                                  !/*nc         77*/
      iap=1                                                                     !/*nc4c        3*/
      kdb=1                                                                     !/*nc         78*/
      return                                                                    !/*nc         79*/
!                                                                               !/*nc         80*/
!        insert the new cell in laj and increment the lcaj pointers.            !/*nc         81*/
  140 if(nlaj.ge.mlaj)go to 180                                                 !/*nc         82*/
      j1=j3+1                                                                   !/*nc         83*/
      j3=j3+1                                                                   !/*nc         84*/
      do 150 iz=j1,klaj+nlaj                                                    !/*nc         85*/
      i=j1+klaj+nlaj-iz                                                         !/*nc         86*/
  150 laj(i+1)=laj(i)                                                           !/*nc         87*/
      nlaj=nlaj+1                                                               !/*nc         88*/
      laj(j1)=iap                                                               !/*nc         89*/
      do 160 j=i1,nlja                                                          !/*nc         90*/
  160 lcaj(klcj+j+1)=sign(abs(lcaj(klcj+j+1))+1,lcaj(klcj+j+1))                 !/*nc         91*/
!                                                                               !/*nc         92*/
!        mark the new other-side cell if it is simple and alone.                !/*nc         93*/
      if(lca(llca+iap).lt.0)go to 210                                           !/*nc         94*/
      do 170 j=1,nlja                                                           !/*nc         95*/
  170 if((j.lt.lca(llca+iap).or.j.ge.abs(lca(llca+iap+1))).and.                 !/*nc         96*/
     & lja(llja+j).eq.lja(llja+i4))go to 210                                    !/*nc         97*/
      laj(j1)=-iap                                                              !/*nc         98*/
      go to 210                                                                 !/*nc         99*/
!                                                                               !/*nc        109*/
!        add the new cell to the overflow region at the end of laj.             !/*nc        110*/
  180 if(nlaj.eq.mlaj)call errprn(1,-1,0,zero,zero,' ',' ',                     !/*nc4c2       1*/
     & 'laj overflow.')                                                         !/*nc4a        4*/
      nlaj=nlaj+1                                                               !/*nc4a        5*/
      laj(klaj+nlaj)=iap                                                        !/*nc        112*/
!                                                                               !/*nc        113*/
!        move into the universe, if any, that fills the new cell.               !/*nc        114*/
  210 if(junf.eq.0)return                                                       !/*nc        115*/
      if(mfl(lmfl+1,iap).eq.0)return                                            !/*nc        116*/
      if(ksc(lksc+jsu).ne.0)jsu=-jsu                                            !/*nc4b        2*/
  215 j=-mfl(lmfl+1,iap)                                                        !/*nc4b        3*/
      if(j.gt.0)go to 220                                                       !/*nc        118*/
      ju=-j                                                                     !/*nc        119*/
      call dunlev(iap)                                                          !/*nc        120*/
      if(mfl(lmfl+2,iap).eq.0)go to 230                                         !/*nc        121*/
      iap=mfl(lmfl+2,iap)                                                       !/*nc        122*/
      go to 370                                                                 !/*nc        123*/
  220 n=3+iii-laf(j+1,1)+laf(j+1,2)*(jjj-laf(j+2,1)+laf(j+2,2)*                 !/*nc        124*/
     & (kkk-laf(j+3,1)))                                                        !/*nc        125*/
      if(laf(j+1,n).eq.abs(jun(ljun+iap)))go to 372                             !/*nc4b        4*/
      if(laf(j+1,n).eq.0)go to 380                                              !/*nc        127*/
      ju=laf(j+1,n)                                                             !/*nc        128*/
      call dunlev(iap)                                                          !/*nc        129*/
      if(laf(j+2,n).eq.0)go to 230                                              !/*nc        130*/
      iap=laf(j+2,n)                                                            !/*nc        131*/
      go to 370                                                                 !/*nc        132*/
!                                                                               !/*nc        133*/
!        look for the new cell in the parentheses in the laj list.              !/*nc        134*/
  230 if(j1.gt.klaj+mlaj)go to 250                                              !/*nc        135*/
      if(laj(j1+1).ne.1000001)go to 270                                         !/*nc4b        5*/
      do 240 j4=j1+2,j3-1                                                       !/*nc        136*/
      if(laj(j4).eq.1000002)go to 270                                           !/*nc        137*/
      iap=abs(laj(j4))                                                          !/*nc        138*/
      if(jun(ljun+iap).ne.ju)go to 240                                          !/*nc        139*/
      call chkcel(iap,0,j)                                                      !/*nc        140*/
      if(j.eq.0)go to 370                                                       !/*nc        141*/
  240 continue                                                                  !/*nc        142*/
!                                                                               !/*nc        143*/
!        search the overflow region at the end of laj.                          !/*nc        144*/
  250 do 260 i=klaj+mlaj+1,klaj+nlaj                                            !/*nc        145*/
      iap=laj(i)                                                                !/*nc        146*/
      if(jun(ljun+iap).ne.ju)go to 260                                          !/*nc        147*/
      call chkcel(iap,0,j)                                                      !/*nc        148*/
      if(j.eq.0)go to 370                                                       !/*nc        149*/
  260 continue                                                                  !/*nc        150*/
!                                                                               !/*nc        151*/
!        add a pair of parentheses if necessary.                                !/*nc        152*/
  270 if(laj(j1+1).eq.1000001.and.j1.ne.j3)go to 300                            !/*nc        153*/
      if(nlaj+1.ge.mlaj)go to 300                                               !/*nc        154*/
      do 280 iz=j1+1,klaj+nlaj                                                  !/*nc        155*/
      i=j1+1+klaj+nlaj-iz                                                       !/*nc        156*/
  280 laj(i+2)=laj(i)                                                           !/*nc        157*/
      nlaj=nlaj+2                                                               !/*nc        158*/
      laj(j1+1)=1000001                                                         !/*nc        159*/
      laj(j1+2)=1000002                                                         !/*nc        160*/
      j3=j3+2                                                                   !/*nc        161*/
      j4=j1+2                                                                   !/*nc        162*/
      do 290 j=i1,nlja                                                          !/*nc        163*/
  290 lcaj(klcj+j+1)=sign(abs(lcaj(klcj+j+1))+2,lcaj(klcj+j+1))                 !/*nc        164*/
!                                                                               !/*nc        165*/
!        look for the new cell among all cells of the universe.                 !/*nc        166*/
  300 do 310 iap=1,mxa                                                          !/*nc        167*/
      if(jun(ljun+iap).ne.ju)go to 310                                          !/*nc        168*/
      call chkcel(iap,0,j)                                                      !/*nc        169*/
      if(j.eq.0)go to 320                                                       !/*nc        170*/
  310 continue                                                                  !/*nc        171*/
      iap=1                                                                     !/*nc4c        4*/
      kdb=1                                                                     !/*nc        172*/
      return                                                                    !/*nc        173*/
!                                                                               !/*nc        174*/
!        insert the new cell in laj and increment the lcaj pointers.            !/*nc        175*/
  320 if(nlaj+1.ge.mlaj)go to 350                                               !/*nc        176*/
      do 330 iz=j4,klaj+nlaj                                                    !/*nc        177*/
      i=j4+klaj+nlaj-iz                                                         !/*nc        178*/
  330 laj(i+1)=laj(i)                                                           !/*nc        179*/
      nlaj=nlaj+1                                                               !/*nc        180*/
      laj(j4)=iap                                                               !/*nc        181*/
      do 340 j=i1,nlja                                                          !/*nc        182*/
  340 lcaj(klcj+j+1)=sign(abs(lcaj(klcj+j+1))+1,lcaj(klcj+j+1))                 !/*nc        183*/
      j3=j3+1                                                                   !/*nc        184*/
      go to 370                                                                 !/*nc        185*/
!                                                                               !/*nc        194*/
!        add the new cell to the overflow region at the end of laj.             !/*nc        195*/
  350 if(nlaj.eq.mlaj)call errprn(1,-1,0,zero,zero,' ',' ',                     !/*nc4c2       2*/
     & 'laj overflow.')                                                         !/*nc4a        8*/
      nlaj=max(nlaj,mlaj)+1                                                     !/*nc4a        9*/
      laj(klaj+nlaj)=iap                                                        !/*nc        197*/
!                                                                               !/*nc        198*/
!        find the element if the cell is a lattice.                             !/*nc        199*/
  370 if(lat(llat+1,iap).ne.0)call findel(iap)                                  !/*nc        200*/
      if(mfl(lmfl+1,iap).ne.0)go to 215                                         !/*nc4b        7*/
!                                                                               !/*nc4b        8*/
!        find coincident planar surface at lowest level, if any.                !/*nc4b        9*/
  372 if(jsu.ge.0)return                                                        !/*nc4b       10*/
      jsu=-jsu                                                                  !/*nc4b       11*/
      do 375 i=abs(lca(llca+iap)),abs(lca(llca+iap+1))-1                        !/*nc4b       12*/
      j=abs(lja(llja+i))                                                        !/*nc4b       13*/
      if(j.gt.1000000)go to 375                                                 !/*nc4b       14*/
      if(ksc(lksc+jsu).ne.ksc(lksc+j))go to 375                                 !/*nc4b       15*/
      k=kst(lkst+j)                                                             !/*nc4b       16*/
      l=lsc(llsc+j)                                                             !/*nc4b       17*/
      if(k.eq.1)a=scf(l+1)*xxx+scf(l+2)*yyy+scf(l+3)*zzz-scf(l+4)               !/*nc4b       18*/
      if(k.ne.1)a=gpblcm(k-1)-scf(l+1)                                          !/*nc4b       19*/
      if(k.eq.1)b=scf(l+1)*uuu+scf(l+2)*vvv+scf(l+3)*www                        !/*nc4c        5*/
      if(k.ne.1)b=gpblcm(k+2)                                                   !/*nc4c        6*/
      if(abs(a).gt.coincd*abs(b))go to 375                                      !/*nc4c        7*/
      if(b*lja(llja+i).gt.0.)jsu=j                                              !/*nc4c        8*/
  375 continue                                                                  !/*nc4b       24*/
      return                                                                    !/*nc4b       25*/
!                                                                               !/*nc        202*/
!        track is exiting a finite lattice.  bad trouble if lev > 0.            !/*nc4a       10*/
!        real particles escape.  detector pseudoparticles get lost.             !/*nc4a       11*/
  380 kdb=1                                                                     !/*nc4a       12*/
      if(iovr.eq.2)return                                                       !/*nc4c        9*/
      iap=-12345                                                                !/*nc4a       13*/
      if(lev.ne.0)call expirx(1,'newcel','zero lattice element hit.')           !/*nc4a       14*/
      return                                                                    !/*nc        206*/
      end                                                                       !/*nc        207*/

!#ident axis.F from deck ax                                                     !/*ax          1*/
      subroutine axis(ie)                                                       !/*ax          2*/
!        find a symmetry axis defined by surfaces of the current cell.          !/*ax          3*/
!        returns ie=0 if successful or -2 if the cell is asymmetric.            !/*ax          4*/
      use global_data                                                           !/*axx         1*/
      use imcn_data                                                             !/*axx         2*/
!                                                                               !/*axx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*axx         4*/
      implicit integer(kindi) (i-n)                                             !/*axx         5*/
!                                                                               !/*axx         6*/
      dimension aa(3,3),dd(3),ff(6),ee(3,3),sc(10),bt(4,2)                      !/*ax          7*/
      data bt/1.,7*0./                                                          !/*ax          8*/
!                                                                               !/*ax          9*/
!        ij marks progress:  1=direction, 2=point, 3=both determined.           !/*ax         10*/
      ij=0                                                                      !/*ax         11*/
!                                                                               !/*ax         12*/
!        try each surface until a common symmetry axis is defined or            !/*ax         13*/
!        until it is clear that there is none.                                  !/*ax         14*/
      do 300 il=1,nsv                                                           !/*ax         15*/
      jt=ljsv(llsv+il)                                                          !/*ax         16*/
      kk=kst(lkst+jt)                                                           !/*ax4a        1*/
      do 10 i=1,10                                                              !/*ax         18*/
   10 sc(i)=scf(lsc(llsc+jt)+i)                                                 !/*ax         19*/
      go to(20,20,20,20,40,40,40,40,40,60,60,60,60,60,60,                       !/*ax         20*/
     & 60,60,60,60,60,60,80,100,60,60,60)kk                                     !/*ax         21*/
!                                                                               !/*ax         22*/
! >>>>>  planes:  p,px,py,pz                                                    !/*ax         23*/
   20 do 30 i=1,3                                                               !/*ax         24*/
      bt(1+i,2)=0.                                                              !/*ax         25*/
      if(kk.eq.1)bt(1+i,2)=sc(i)                                                !/*ax         26*/
   30 if(i.eq.kk-1)bt(1+i,2)=1.                                                 !/*ax         27*/
      go to 290                                                                 !/*ax         28*/
!                                                                               !/*ax         29*/
! >>>>>  spheres:  so,s,sx,sy,sz                                                !/*ax         30*/
   40 do 50 i=1,3                                                               !/*ax         31*/
      ff(i)=0.                                                                  !/*ax         32*/
      if(kk.eq.6)ff(i)=sc(i)                                                    !/*ax         33*/
   50 if(kk.gt.6.and.kk-6.eq.i)ff(i)=sc(1)                                      !/*ax         34*/
      go to 250                                                                 !/*ax         35*/
!                                                                               !/*ax         36*/
! >>>>>  cylinders, cones, and tori:  c/x,c/y,c/z,cx,cy,cz,                     !/*ax         37*/
!        k/x,k/y,k/z,kx,ky,kz,tx,ty,tz                                          !/*ax         38*/
   60 j=mod(kk-1+kk/24,3)+1                                                     !/*ax         39*/
      l=0                                                                       !/*ax         40*/
      do 70 i=1,3                                                               !/*ax         41*/
      bt(1+i,1)=0.                                                              !/*ax         42*/
      bt(1+i,2)=0.                                                              !/*ax         43*/
      if(i.ne.j)l=l+1                                                           !/*ax         44*/
      if(kk.lt.13.and.i.ne.j)bt(1+i,1)=sc(l)                                    !/*ax         45*/
   70 if(kk.gt.15.and.kk.lt.19.or.kk.ge.24)bt(1+i,1)=sc(i)                      !/*ax         46*/
      bt(1+j,2)=1.                                                              !/*ax         47*/
      go to 310                                                                 !/*ax         48*/
!                                                                               !/*ax         49*/
! >>>>>  special quadratic:  sq                                                 !/*ax         50*/
!        convert it into a general quadratic.                                   !/*ax         51*/
   80 s=sc(7)                                                                   !/*ax         52*/
      do 90 i=1,3                                                               !/*ax         53*/
      sc(6+i)=2.*(sc(3+i)-sc(i)*sc(7+i))                                        !/*ax         54*/
      s=s+sc(7+i)*(sc(i)*sc(7+i)-2.*sc(3+i))                                    !/*ax         55*/
   90 sc(3+i)=0.                                                                !/*ax         56*/
      sc(10)=s                                                                  !/*ax         57*/
!                                                                               !/*ax         58*/
! >>>>>  general quadratic:  gq                                                 !/*ax         59*/
!        check for plane and sphere special cases.                              !/*ax         60*/
  100 t0=0.                                                                     !/*ax         61*/
      do 110 i=1,6                                                              !/*ax         62*/
  110 t0=t0+abs(sc(i))                                                          !/*ax         63*/
      if(t0.gt.0.)go to 130                                                     !/*ax         64*/
      do 120 i=1,3                                                              !/*ax         65*/
  120 bt(1+i,2)=sc(6+i)                                                         !/*ax         66*/
      go to 290                                                                 !/*ax         67*/
  130 if(abs(sc(1)-sc(2))+abs(sc(2)-sc(3))+abs(sc(4))+abs(sc(5))+               !/*ax         68*/
     & abs(sc(6)).gt.t0*1.e-8)go to 150                                         !/*ax         69*/
      do 140 i=1,3                                                              !/*ax         70*/
  140 ff(i)=-.5*sc(6+i)/sc(1)                                                   !/*ax         71*/
      go to 250                                                                 !/*ax         72*/
!                                                                               !/*ax         73*/
!        determine rotation matrix, ee, and eigenvalues, dd.                    !/*ax         74*/
  150 do 160 i=1,3                                                              !/*ax         75*/
  160 aa(i,i)=sc(i)                                                             !/*ax         76*/
      aa(1,2)=.5*sc(4)                                                          !/*ax         77*/
      aa(2,1)=.5*sc(4)                                                          !/*ax         78*/
      aa(2,3)=.5*sc(5)                                                          !/*ax         79*/
      aa(3,2)=.5*sc(5)                                                          !/*ax         80*/
      aa(1,3)=.5*sc(6)                                                          !/*ax         81*/
      aa(3,1)=.5*sc(6)                                                          !/*ax         82*/
      call smevev(3,aa,dd,ee,ff,zero+1.e-8,je,3)                                !/*ax         83*/
      if(je.ne.0)go to 350                                                      !/*ax         84*/
!                                                                               !/*ax         85*/
!        find the symmetry axis and check for plane of 2 sheets.                !/*ax         86*/
      z2=1.e-8*(abs(dd(1))+abs(dd(2))+abs(dd(3)))                               !/*ax         87*/
      do 170 j=1,3                                                              !/*ax         88*/
      k=mod(j,3)+1                                                              !/*ax         89*/
  170 if(abs(dd(k)-dd(6-k-j)).le.z2)go to 180                                   !/*ax         90*/
      go to 350                                                                 !/*ax         91*/
  180 do 190 i=1,3                                                              !/*ax         92*/
  190 bt(1+i,2)=ee(i,j)                                                         !/*ax         93*/
      if(dd(k).le.z2)go to 290                                                  !/*ax         94*/
!                                                                               !/*ax         95*/
!        find the translation vector by transforming the surface into           !/*ax         96*/
!        the p coordinate system where q=ee*p+x0.                               !/*ax         97*/
!        the surface is then p*dd*p+(2*x0*aa+b)*ee*p+(x0*aa+b)*x0+c=0           !/*ax         98*/
      if(abs(dd(j)).le.z2)go to 230                                             !/*ax         99*/
!                                                                               !/*ax        100*/
!        cone, ellipsoid, or hyperboloid.  x0=-.5*b*ee*dd(i)*ee(t)              !/*ax        101*/
      do 200 i=1,3                                                              !/*ax        102*/
      do 200 l=1,3                                                              !/*ax        103*/
  200 aa(i,l)=ee(l,i)/dd(i)                                                     !/*ax        104*/
      do 220 i=1,3                                                              !/*ax        105*/
      do 210 l=1,3                                                              !/*ax        106*/
  210 ff(l)=ee(i,1)*aa(1,l)+ee(i,2)*aa(2,l)+ee(i,3)*aa(3,l)                     !/*ax        107*/
  220 bt(1+i,1)=-.5*(sc(7)*ff(1)+sc(8)*ff(2)+sc(9)*ff(3))                       !/*ax        108*/
      go to 310                                                                 !/*ax        109*/
!                                                                               !/*ax        110*/
!        cylinder or paraboloid.  x0=-.5*b/dd(2)                                !/*ax        111*/
  230 do 240 i=1,3                                                              !/*ax        112*/
  240 bt(1+i,1)=-.5*sc(i+6)/dd(k)                                               !/*ax        113*/
      go to 310                                                                 !/*ax        114*/
!                                                                               !/*ax        115*/
!        sphere.  if point is unknown, set it up.                               !/*ax        116*/
  250 if(ij.eq.2)go to 270                                                      !/*ax        117*/
      do 260 i=1,3                                                              !/*ax        118*/
  260 bt(1+i,1)=ff(i)                                                           !/*ax        119*/
      ij=ij+2                                                                   !/*ax        120*/
      go to 300                                                                 !/*ax        121*/
!                                                                               !/*ax        122*/
!        if point is known, define axis by centers of the two spheres.          !/*ax        123*/
  270 z2=1.e-8*(abs(bt(2,1))+abs(bt(3,1))+abs(bt(4,1))+                         !/*ax        124*/
     & abs(ff(1))+abs(ff(2))+abs(ff(3)))                                        !/*ax        125*/
      do 280 i=1,3                                                              !/*ax        126*/
      bt(1+i,2)=bt(1+i,1)-ff(i)                                                 !/*ax        127*/
  280 if(abs(bt(1+i,2)).le.z2)bt(1+i,2)=0.                                      !/*ax        128*/
      if(abs(bt(2,2))+abs(bt(3,2))+abs(bt(3,2)).le.z2)go to 300                 !/*ax        129*/
!                                                                               !/*ax        130*/
  290 if(ij.ne.1)ij=ij+1                                                        !/*ax        131*/
  300 if(ij.eq.3)go to 310                                                      !/*ax        132*/
!                                                                               !/*ax        133*/
!        provide 2 arbitrary orthogonal base vectors to complete bbb.           !/*ax        134*/
  310 do 320 i=1,4                                                              !/*ax        135*/
      do 320 j=1,2                                                              !/*ax        136*/
  320 bbb(i,j)=bt(i,j)                                                          !/*ax        137*/
      t1=sqrt(bbb(2,2)**2+bbb(3,2)**2+bbb(4,2)**2)                              !/*ax        138*/
      if(t1.eq.0.)bbb(2,2)=1.                                                   !/*ax        139*/
      if(t1.eq.0.)t1=1.                                                         !/*ax        140*/
      do 330 i=2,4                                                              !/*ax        141*/
  330 bbb(i,2)=bbb(i,2)/t1                                                      !/*ax        142*/
      call arbobv(bbb(2,2),bbb(2,3),bbb(2,4),i)                                 !/*ax        143*/
!                                                                               !/*ax        144*/
!        return with ie=0 if a symmetry axis is defined.                        !/*ax        145*/
      if(dbcn(7).ne.0.)write(iuo,340)((bbb(i,j),j=1,4),i=1,4)                   !/*ax        146*/
  340 format(/47h new axis.  coordinate transformation:  q=bbb*p/               !/*ax        147*/
     & 9x,5hbbb =,1p4e20.12/(14x,4e20.12))                                      !/*ax        148*/
      ie=0                                                                      !/*ax        149*/
      return                                                                    !/*ax        150*/
!                                                                               !/*ax        151*/
!        return with ie=-2 if an asymmetric surface is found.                   !/*ax        152*/
  350 if(dbcn(7).ne.0.)write(iuo,360)nsf(lnsf+jt)                               !/*ax        153*/
  360 format(3x,7hsurface,i7,22h has no symmetry axis.)                         !/*ax        154*/
      ie=-2                                                                     !/*ax        155*/
      return                                                                    !/*ax        156*/
      end                                                                       !/*ax        157*/

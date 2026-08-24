!#ident psurf.F from deck ps                                                    !/*ps          1*/
      subroutine psurf                                                          !/*ps          2*/
!        translate point-defined surface mxj into a standard surface.           !/*ps          3*/
      use global_data                                                           !/*psx         1*/
      use imcn_data                                                             !/*psx         2*/
!                                                                               !/*psx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*psx         4*/
      implicit integer(kindi) (i-n)                                             !/*psx         5*/
!                                                                               !/*psx         6*/
      dimension c(2,3),a(4,4)                                                   !/*ps          6*/
      character hs(9)*24                                                        !/*ps          7*/
      data hs/'plane','cylinder','one-sheet cone','sphere','paraboloid',        !/*ps          8*/
     & 'hyperboloid of one sheet','hyperboloid of 2 sheets','ellipsoid',        !/*ps          9*/
     & 'two-sheet cone'/                                                        !/*ps         10*/
!                                                                               !/*ps         11*/
      ix=lsc(llsc+mxj)                                                          !/*ps         12*/
      if(m1c.eq.1)go to 170                                                     !/*ps         13*/
!                                                                               !/*ps         14*/
!        check for an acceptable number of input data items.                    !/*ps         15*/
      nc=nwc-1-m2c                                                              !/*ps         16*/
      if(nc.eq.2.or.nc.eq.4.or.nc.eq.6)go to 10                                 !/*ps         17*/
      call erprnt(2,1,0,0,0,0,0,0,'31hnumber of entries is incorrect.')         !/*ps         18*/
      return                                                                    !/*ps         19*/
!                                                                               !/*ps         20*/
!        make it three points if only one or two are given.                     !/*ps         21*/
   10 do 20 j=1,3                                                               !/*ps         22*/
      do 20 i=1,2                                                               !/*ps         23*/
   20 c(i,j)=scf(ix+i+2*(j-1))                                                  !/*ps         24*/
      do 30 i=(nc+2)/2,3                                                        !/*ps         25*/
      do 30 j=1,2                                                               !/*ps         26*/
   30 c(j,i)=c(j,i-1)                                                           !/*ps         27*/
      do 40 i=1,nc                                                              !/*ps         28*/
   40 scf(ix+i)=0.                                                              !/*ps         29*/
!                                                                               !/*ps         30*/
!        set up to detect special cases.                                        !/*ps         31*/
      t=sqrt(max((c(1,1)-c(1,2))**2+(c(2,1)-c(2,2))**2,(c(1,2)-c(1,3))          !/*ps         32*/
     & **2+(c(2,2)-c(2,3))**2,(c(1,3)-c(1,1))**2+(c(2,3)-c(2,1))**2))           !/*ps         33*/
      t1=min(c(1,1),c(1,2),c(1,3))                                              !/*ps         34*/
      t2=max(c(1,1),c(1,2),c(1,3))                                              !/*ps         35*/
!                                                                               !/*ps         36*/
!        plane if it is narrow or cylinder if it is horizontal.                 !/*ps         37*/
      ks=3                                                                      !/*ps         38*/
      if(max(abs(c(2,1)-c(2,2)),abs(c(2,2)-c(2,3)),abs(c(2,3)-c(2,1)))          !/*ps         40*/
     & .le.1.e-12*t)ks=2                                                        !/*cor4-1     56*/
      if(t2-t1.le.1.e-12*t)ks=1                                                 !/*cor4-1     57*/
      if(ks.eq.3)go to 50                                                       !/*ps         42*/
      m1c=m1c-36+11*ks                                                          !/*ps         43*/
      scf(ix+1)=((c(ks,1)+c(ks,2)+c(ks,3))/3.)**ks                              !/*ps         44*/
      go to 160                                                                 !/*ps         45*/
!                                                                               !/*ps         46*/
!        one-sheet cone if it is a straight line.                               !/*ps         47*/
   50 if(abs((c(1,2)-c(1,1))*(c(2,3)-c(2,2))-(c(2,2)-c(2,1))*                   !/*ps         48*/
     & (c(1,3)-c(1,2)))*1.e12.gt.t**2)go to 60                                  !/*ps         49*/
      m1c=m1c-8                                                                 !/*ps         50*/
      t1=c(1,1)+c(1,2)+c(1,3)                                                   !/*ps         51*/
      t2=c(2,1)+c(2,2)+c(2,3)                                                   !/*ps         52*/
      scf(ix+3)=(3.*(c(1,1)*c(2,1)+c(1,2)*c(2,2)+c(1,3)*c(2,3))-                !/*ps         53*/
     & t1*t2)/(3.*(c(1,1)**2+c(1,2)**2+c(1,3)**2)-t1**2)                        !/*ps         54*/
      scf(ix+2)=scf(ix+3)**2                                                    !/*ps         55*/
      scf(ix+1)=(t1-t2/scf(ix+3))/3.                                            !/*ps         56*/
      go to 160                                                                 !/*ps         57*/
!                                                                               !/*ps         58*/
!        check for a plane of two sheets.                                       !/*ps         59*/
   60 if(abs(c(2,1)-c(2,2)).le.1.e12*abs(c(1,1)-c(1,2)).and.                    !/*ps         60*/
     & abs(c(2,2)-c(2,3)).le.1.e12*abs(c(1,2)-c(1,3)).and.                      !/*ps         61*/
     & abs(c(2,3)-c(2,1)).le.1.e12*abs(c(1,3)-c(1,1)))go to 70                  !/*ps         62*/
      call erprnt(2,1,0,0,0,0,0,0,                                              !/*ps         63*/
     & '37hsurface would be two parallel planes.')                              !/*ps         64*/
      m1c=m1c-25                                                                !/*ps         65*/
      ks=0                                                                      !/*ps         66*/
      go to 160                                                                 !/*ps         67*/
!                                                                               !/*ps         68*/
!        find coefficients of symmetric gq equation by elimination.             !/*ps         69*/
!        r**2 + a(4,1)*x**2 + a(4,2)*x + a(4,3) = 0.                            !/*ps         70*/
   70 i1=1                                                                      !/*ps         71*/
      do 80 i=1,3                                                               !/*ps         72*/
      a(i,1)=c(1,i)**2                                                          !/*ps         73*/
      if(a(i,1).gt.a(i1,1))i1=i                                                 !/*ps         74*/
      a(i,2)=c(1,i)                                                             !/*ps         75*/
      a(i,3)=1.                                                                 !/*ps         76*/
   80 a(i,4)=-c(2,i)**2                                                         !/*ps         77*/
      i2=max(1,3-i1)                                                            !/*ps         78*/
      do 100 i=1,3                                                              !/*ps         79*/
      if(i.eq.i1)go to 100                                                      !/*ps         80*/
      do 90 j=2,4                                                               !/*ps         81*/
   90 a(i,j)=a(i,j)-a(i,1)*a(i1,j)/a(i1,1)                                      !/*ps         82*/
      if(abs(a(i,2)).gt.abs(a(i2,2)))i2=i                                       !/*ps         83*/
  100 continue                                                                  !/*ps         84*/
      i3=6-i1-i2                                                                !/*ps         85*/
      a(i3,3)=a(i3,3)-a(i3,2)*a(i2,3)/a(i2,2)                                   !/*ps         86*/
      a(4,3)=(a(i3,4)-a(i3,2)*a(i2,4)/a(i2,2))/a(i3,3)                          !/*ps         87*/
      a(4,2)=(a(i2,4)-a(i2,3)*a(4,3))/a(i2,2)                                   !/*ps         88*/
      a(4,1)=(a(i1,4)-a(i1,3)*a(4,3)-a(i1,2)*a(4,2))/a(i1,1)                    !/*ps         89*/
!                                                                               !/*ps         90*/
!        set up coefficients for simplest applicable standard surface.          !/*ps         91*/
!                                                                               !/*ps         92*/
!        sphere if x**2 coefficient is 1.                                       !/*ps         93*/
      if(abs(a(4,1)-1.)*1.e12.gt.1.)go to 120                                   !/*ps         94*/
      ks=4                                                                      !/*ps         95*/
!                                                                               !/*ps         96*/
!        sphere at origin if linear term is zero.                               !/*ps         97*/
      if(abs(a(4,2))*1.e12.gt.t)go to 110                                       !/*ps         98*/
      m1c=5                                                                     !/*ps         99*/
      scf(ix+1)=-a(4,3)                                                         !/*ps        100*/
      go to 160                                                                 !/*ps        101*/
!                                                                               !/*ps        102*/
!        sphere, not at origin.                                                 !/*ps        103*/
  110 m1c=m1c-20                                                                !/*ps        104*/
      scf(ix+1)=-.5*a(4,2)                                                      !/*ps        105*/
      scf(ix+2)=scf(ix+1)**2-a(4,3)                                             !/*ps        106*/
      go to 160                                                                 !/*ps        107*/
!                                                                               !/*ps        108*/
!        sq surface of some sort.                                               !/*ps        109*/
  120 do 130 i=1,10                                                             !/*ps        110*/
  130 scf(ix+11-i)=i/8                                                          !/*ps        111*/
!                                                                               !/*ps        112*/
!        paraboloid if it has no x**2 term.  let constant term be zero.         !/*ps        113*/
      if(abs(a(4,1))*1.e12.gt.1.)go to 140                                      !/*ps        114*/
      scf(ix+m1c-26)=0.                                                         !/*ps        115*/
      scf(ix+m1c-23)=.5*a(4,2)                                                  !/*ps        116*/
      scf(ix+m1c-19)=-a(4,3)/a(4,2)                                             !/*ps        117*/
      m1c=22                                                                    !/*ps        118*/
      ks=5                                                                      !/*ps        119*/
      go to 160                                                                 !/*ps        120*/
!                                                                               !/*ps        121*/
!        hyperboloid, ellipsoid, or cone.  let linear term be zero.             !/*ps        122*/
  140 scf(ix+m1c-26)=a(4,1)                                                     !/*ps        123*/
      scf(ix+7)=a(4,3)-.25*a(4,2)**2/a(4,1)                                     !/*ps        124*/
      scf(ix+m1c-19)=-.5*a(4,2)/a(4,1)                                          !/*ps        125*/
!                                                                               !/*ps        126*/
!        two-sheet cone if constant term is zero.                               !/*ps        127*/
      if(abs(scf(ix+7))*1.e12.gt.t**2.or.a(4,1).gt.0.)go to 150                 !/*ps        128*/
      scf(ix+1)=scf(ix+m1c-19)                                                  !/*ps        129*/
      scf(ix+2)=-a(4,1)                                                         !/*ps        130*/
      scf(ix+3)=0.                                                              !/*ps        131*/
      m1c=m1c-8                                                                 !/*ps        132*/
      ks=9                                                                      !/*ps        133*/
      go to 160                                                                 !/*ps        134*/
!                                                                               !/*ps        135*/
!        hyperboloid or ellipsoid.                                              !/*ps        136*/
  150 ks=6                                                                      !/*ps        137*/
      if(scf(ix+7).gt.0.)ks=7                                                   !/*ps        138*/
      if(a(4,1).gt.0.)ks=8                                                      !/*ps        139*/
!                                                                               !/*ps        140*/
!        check that all points are on the same sheet of a hyperboloid.          !/*ps        141*/
      if(scf(ix+m1c-19).lt.t2.and.scf(ix+m1c-19).gt.t1.and.ks.eq.7)             !/*ps        142*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*ps        143*/
     & '52hthe points are on different sheets of a hyperboloid.')               !/*ps        144*/
      m1c=22                                                                    !/*ps        145*/
!                                                                               !/*ps        146*/
!        print the shape of the translated surface.                             !/*ps        147*/
  160 if(ks.ne.0)write(iuo,'(1h+,94x,a24)')hs(ks)                               !/*ps        148*/
      return                                                                    !/*ps        149*/
!                                                                               !/*ps        150*/
!        convert a point-defined plane to a p-plane.                            !/*ps        151*/
  170 tpp(4)=0.                                                                 !/*ps        152*/
      do 180 i=1,3                                                              !/*ps        153*/
      j=mod(i,3)+1                                                              !/*ps        154*/
      k=6-i-j                                                                   !/*ps        155*/
      tpp(i)=scf(ix+j)*(scf(ix+k+3)-scf(ix+k+6))+scf(ix+j+3)*                   !/*ps        156*/
     & (scf(ix+k+6)-scf(ix+k))+scf(ix+j+6)*(scf(ix+k)-scf(ix+k+3))              !/*ps        157*/
  180 tpp(4)=tpp(4)+scf(ix+i)*(scf(ix+j+3)*scf(ix+k+6)-                         !/*ps        158*/
     & scf(ix+j+6)*scf(ix+k+3))                                                 !/*ps        159*/
!                                                                               !/*ps        160*/
!        check for the points being in line.  put the results in scf.           !/*ps        161*/
      xm=0.                                                                     !/*ps        162*/
      do 190 i=1,4                                                              !/*ps        163*/
      if(xm.eq.0..and.tpp(5-i).ne.0.)xm=1./tpp(5-i)                             !/*ps        164*/
  190 scf(ix+5-i)=tpp(5-i)*xm                                                   !/*ps        165*/
      if(xm.eq.0.)call erprnt(2,1,1,icn,0,0,0,0,                                !/*ps        166*/
     & '22hcannot convert surface,i5,12h to a plane.')                          !/*ps        167*/
      return                                                                    !/*ps        168*/
      end                                                                       !/*ps        169*/

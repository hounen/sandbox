!#ident dbmin.F from deck dm                                                    !/*dm          1*/
      function dbmin()                                                          !/*dm          2*/
!        find the minimum distance to all bounding surfaces of cell icl.        !/*dm          3*/
!        icl must be simple and have no sq, gq, or toroidal surfaces.           !/*dm          4*/
      use global_data                                                           !/*dmx         1*/
!                                                                               !/*dmx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*dmx         3*/
      implicit integer(kindi) (i-n)                                             !/*dmx         4*/
!                                                                               !/*dmx         5*/
!                                                                               !/*dm          6*/
      dbmin=huge                                                                !/*dm          7*/
      ic=icl                                                                    !/*dm4b        1*/
      x0=xxx                                                                    !/*dm4c2       1*/
      y0=yyy                                                                    !/*dm4c2       2*/
      z0=zzz                                                                    !/*dm4c2       3*/
      do 230 l=0,lev                                                            !/*dm4c2       4*/
      ll=lev-l                                                                  !/*dm4b        7*/
      if(ll.eq.lev)go to 6                                                      !/*dm4b        8*/
      xxx=udt(1,ll)                                                             !/*dm4b        9*/
      yyy=udt(2,ll)                                                             !/*dm4b       10*/
      zzz=udt(3,ll)                                                             !/*dm4b       11*/
      ic=udt(7,ll)                                                              !/*dm4b       12*/
    6 do 230 j1=abs(lca(llca+ic)),abs(lca(llca+ic+1))-1                         !/*dm4b       13*/
      if(lcaj(klcj+j1).lt.0)go to 230                                           !/*dm          9*/
      j=abs(lja(llja+j1))                                                       !/*dm         10*/
      k=kst(lkst+j)                                                             !/*dm4a        1*/
      i=lsc(llsc+j)+1                                                           !/*dm         12*/
      go to(10,20,20,20,30,40,50,60,70,80,90,100,110,120,130,                   !/*dm         13*/
     & 140,150,160,180,190,200)k                                                !/*dm         14*/
!                                                                               !/*dm         15*/
!        planes p,px,py,pz                                                      !/*dm         16*/
   10 d=abs(scf(i+3)-xxx*scf(i)-yyy*scf(i+1)-zzz*scf(i+2))/                     !/*dm         17*/
     & sqrt(scf(i)**2+scf(i+1)**2+scf(i+2)**2)                                  !/*dm         18*/
      go to 220                                                                 !/*dm         19*/
   20 d=abs(scf(i)-gpblcm(k-1))                                                 !/*dm         20*/
      go to 220                                                                 !/*dm         21*/
!                                                                               !/*dm         22*/
!        spheres so,s,sx,sy,sz                                                  !/*dm         23*/
   30 d=abs(sqrt(xxx**2+yyy**2+zzz**2)-sqrt(scf(i)))                            !/*dm         24*/
      go to 220                                                                 !/*dm         25*/
   40 d=abs(sqrt((xxx-scf(i))**2+(yyy-scf(i+1))**2+(zzz-scf(i+2))**2)-          !/*dm         26*/
     & sqrt(scf(i+3)))                                                          !/*dm         27*/
      go to 220                                                                 !/*dm         28*/
   50 d=abs(sqrt((xxx-scf(i))**2+yyy**2+zzz**2)-sqrt(scf(i+1)))                 !/*dm         29*/
      go to 220                                                                 !/*dm         30*/
   60 d=abs(sqrt(xxx**2+(yyy-scf(i))**2+zzz**2)-sqrt(scf(i+1)))                 !/*dm         31*/
      go to 220                                                                 !/*dm         32*/
   70 d=abs(sqrt(xxx**2+yyy**2+(zzz-scf(i))**2)-sqrt(scf(i+1)))                 !/*dm         33*/
      go to 220                                                                 !/*dm         34*/
!                                                                               !/*dm         35*/
!        cylinders c/x,c/y,c/z,cx,cy,cz                                         !/*dm         36*/
   80 d=abs(sqrt((yyy-scf(i))**2+(zzz-scf(i+1))**2)-sqrt(scf(i+2)))             !/*dm         37*/
      go to 220                                                                 !/*dm         38*/
   90 d=abs(sqrt((xxx-scf(i))**2+(zzz-scf(i+1))**2)-sqrt(scf(i+2)))             !/*dm         39*/
      go to 220                                                                 !/*dm         40*/
  100 d=abs(sqrt((xxx-scf(i))**2+(yyy-scf(i+1))**2)-sqrt(scf(i+2)))             !/*dm         41*/
      go to 220                                                                 !/*dm         42*/
  110 d=abs(sqrt(yyy**2+zzz**2)-sqrt(scf(i)))                                   !/*dm         43*/
      go to 220                                                                 !/*dm         44*/
  120 d=abs(sqrt(xxx**2+zzz**2)-sqrt(scf(i)))                                   !/*dm         45*/
      go to 220                                                                 !/*dm         46*/
  130 d=abs(sqrt(xxx**2+yyy**2)-sqrt(scf(i)))                                   !/*dm         47*/
      go to 220                                                                 !/*dm         48*/
!                                                                               !/*dm         49*/
!        cones k/x,k/y,k/z,kx,ky,kz                                             !/*dm         50*/
  140 t=sqrt((yyy-scf(i+1))**2+(zzz-scf(i+2))**2)                               !/*dm         51*/
      go to 170                                                                 !/*dm         52*/
  150 t=sqrt((xxx-scf(i))**2+(zzz-scf(i+2))**2)                                 !/*dm         53*/
      go to 170                                                                 !/*dm         54*/
  160 t=sqrt((xxx-scf(i))**2+(yyy-scf(i+1))**2)                                 !/*dm4a        2*/
  170 d=abs((abs(gpblcm(k-15)-scf(i+k-16))*sqrt(scf(i+3))-t)/                   !/*dm         56*/
     & sqrt(1.+scf(i+3)))                                                       !/*dm         57*/
      go to 220                                                                 !/*dm         58*/
  180 t=sqrt(yyy**2+zzz**2)                                                     !/*dm         59*/
      go to 210                                                                 !/*dm         60*/
  190 t=sqrt(xxx**2+zzz**2)                                                     !/*dm         61*/
      go to 210                                                                 !/*dm         62*/
  200 t=sqrt(xxx**2+yyy**2)                                                     !/*dm         63*/
  210 d=abs((abs(gpblcm(k-18)-scf(i))*sqrt(scf(i+1))-t)/                        !/*dm         64*/
     & sqrt(1.+scf(i+1)))                                                       !/*dm         65*/
!                                                                               !/*dm         66*/
!        take the closest distance.                                             !/*dm         67*/
  220 dbmin=min(d,dbmin)                                                        !/*dm         68*/
  230 continue                                                                  !/*dm         69*/
      xxx=x0                                                                    !/*dm4c2       5*/
      yyy=y0                                                                    !/*dm4c2       6*/
      zzz=z0                                                                    !/*dm4c2       7*/
      return                                                                    !/*dm         70*/
      end                                                                       !/*dm         71*/

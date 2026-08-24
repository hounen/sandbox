!#ident inter.F from deck in                                                    !/*in          1*/
      subroutine inter                                                          !/*in          3*/
#ifdef PLOT                                                                     /*!inx         1*/
!        find the intersections within the plot window of curve iqc,jsu         !/*in          4*/
!        with all other curves that bound cells bounded by jsu.                 !/*in          5*/
      use global_data                                                           !/*inx         2*/
      use plot_data                                                             !/*inx         3*/
!                                                                               !/*inx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*inx         5*/
      implicit integer(kindi) (i-n)                                             !/*inx         6*/
!                                                                               !/*inx         7*/
      dimension cf(5),qr(4),g(4),p(4),e(3,3,2),am(4,4),bb(4,4),tr(4,4)          !/*in          8*/
      data tr/1.,15*0./                                                         !/*in          9*/
      c(i)=coe(lcoe+i,iqc,jsu)                                                  !/*in         10*/
      d(i)=coe(lcoe+i,iq,js)                                                    !/*in         11*/
      q(i,j)=qmx(lqmx+i,j,iqc,jsu)                                              !/*in         12*/
      r(i,j)=qmx(lqmx+i,j,iq,js)                                                !/*in         13*/
!                                                                               !/*in         14*/
      nxp=0                                                                     !/*in         15*/
!                                                                               !/*in         16*/
!        if ellipse, set up points 3., zero, and 3.-2.*pie.                     !/*in         17*/
      if(kfm(lkfm+jsu).ne.3)go to 10                                            !/*in         18*/
      call ptost(three,iqc,jsu,s,t)                                             !/*inx         8*/
      if(abs(s).gt.1..or.abs(t).gt.1.)go to 10                                  !/*in         20*/
      nxp=2                                                                     !/*in         21*/
      crs(lcrs+1)=3.-2.*pie                                                     !/*in         22*/
      crs(lcrs+2)=3.                                                            !/*in         23*/
      call ptost(zero,iqc,jsu,s,t)                                              !/*in         24*/
      if(abs(s).gt.1..or.abs(t).gt.1.)go to 10                                  !/*in         25*/
      nxp=3                                                                     !/*in         26*/
      crs(lcrs+3)=0.                                                            !/*in         27*/
!                                                                               !/*in         28*/
!        set up points where the curve crosses the window boundary.             !/*in         29*/
   10 do 30 i=1,2                                                               !/*in         30*/
      do 30 m=-1,1,2                                                            !/*in         31*/
      call sttop(i,zero+m,iqc,jsu,n,g(1),g(2),p(1),p(2))                        !/*in         32*/
      do 20 k=1,n                                                               !/*in         33*/
      if(abs(g(k)).gt.1.)go to 20                                               !/*in         34*/
      if(isheet(p(k),iqc,jsu).ne.0)go to 20                                     !/*in         35*/
      nxp=nxp+1                                                                 !/*in         36*/
      crs(lcrs+nxp)=p(k)                                                        !/*in         37*/
   20 continue                                                                  !/*in         38*/
   30 continue                                                                  !/*in         39*/
!                                                                               !/*in         40*/
!        find the intersections with the other curves.                          !/*in         41*/
      do 360 js=1,mxj                                                           !/*in         42*/
      if(ncs(lncs+js).eq.0)go to 360                                            !/*in         43*/
      do 40 i=1,ncel                                                            !/*in         44*/
      ic=abs(lcl(llcl+i))                                                       !/*in         45*/
      do 40 j=abs(lca(llca+ic)),abs(lca(llca+ic+1))-1                           !/*in         46*/
   40 if(abs(lja(llja+j)).eq.js)go to 120                                       !/*in         47*/
      do 50 l=0,lev-1                                                           !/*in         48*/
      ic=udt(7,l)                                                               !/*in         49*/
      do 50 j=abs(lca(llca+ic)),abs(lca(llca+ic+1))-1                           !/*in         50*/
   50 if(abs(lja(llja+j)).eq.js)go to 60                                        !/*in         51*/
      go to 360                                                                 !/*in         52*/
   60 if(ncs(lncs+js).ge.0)go to 150                                            !/*in         53*/
      do 70 i=1,4                                                               !/*in         54*/
      do 70 j=1,4                                                               !/*in         55*/
   70 am(i,j)=amx(lamx+i,j,js)                                                  !/*in         56*/
      do 110 ll=l,lev-1                                                         !/*in         57*/
      j=-mfl(lmfl+1,int(udt(7,ll)))                                             !/*in         58*/
      if(j.lt.0)m=mfl(lmfl+3,int(udt(7,ll)))                                    !/*in         59*/
      if(j.gt.0)m=laf(j+3,int(3+udt(8,ll)-laf(j+1,1)+laf(j+1,2)*                !/*in         60*/
     & (udt(9,ll)-laf(j+2,1)+laf(j+2,2)*(udt(10,ll)-laf(j+3,1)))))              !/*in         61*/
      do 80 i=2,4                                                               !/*in         62*/
      tr(i,1)=trf(ltrf+13+i,m)                                                  !/*in         63*/
      do 80 j=2,4                                                               !/*in         64*/
   80 tr(i,j)=trf(ltrf+3*i+j-3,m)                                               !/*in         65*/
      if(ll.eq.lev-1)go to 100                                                  !/*in         66*/
      if(lat(llat+1,int(udt(7,ll+1))).eq.0)go to 100                            !/*in         67*/
      k=lat(llat+2,int(udt(7,ll+1)))                                            !/*in         68*/
      do 90 i=1,3                                                               !/*in         69*/
   90 tr(1+i,1)=tr(1+i,1)+udt(8,ll+1)*vcl(lvcl+i,1,k)+                          !/*in         70*/
     & udt(9,ll+1)*vcl(lvcl+i,2,k)+udt(10,ll+1)*vcl(lvcl+i,3,k)                 !/*in         71*/
  100 call matmpy(4,4,4,am,4,tr,4,bb,4,0)                                       !/*in         72*/
  110 call matmpy(4,4,4,tr,4,bb,4,am,4,4)                                       !/*in         73*/
      go to 140                                                                 !/*in         74*/
  120 if(ncs(lncs+js).ge.0)go to 150                                            !/*in         75*/
      do 130 i=1,4                                                              !/*in         76*/
      do 130 j=1,4                                                              !/*in         77*/
  130 am(i,j)=amx(lamx+i,j,js)                                                  !/*in         78*/
  140 call regula(js,am,0)                                                      !/*in         79*/
  150 do 350 iq=1,ncs(lncs+js)                                                  !/*in         80*/
      if(js.eq.jsu.and.iq.eq.iqc)go to 350                                      !/*in         81*/
!                                                                               !/*in         82*/
!        do the case of two straight lines.                                     !/*in         83*/
      if(max(kfm(lkfm+jsu),kfm(lkfm+js)).gt.1)go to 160                         !/*in         84*/
      b=d(5)*c(2)-d(2)*c(5)                                                     !/*in         85*/
      if(b.eq.0.)go to 350                                                      !/*in         86*/
      a=(d(5)*(d(1)-c(1))+d(2)*(c(4)-d(4)))/b                                   !/*in         87*/
      if(abs(c(1)+c(2)*a).gt.1..or.abs(c(4)+c(5)*a).gt.1.)go to 350             !/*in         88*/
      if(js.ne.jsu.and.(isheet(a,iqc,jsu).ne.0.or.isheet                        !/*in         89*/
     & ((c(5)*(d(1)-c(1))+c(2)*(c(4)-d(4)))/b,iq,js).ne.0))go to 350            !/*in         90*/
      nxp=nxp+1                                                                 !/*in         91*/
      crs(lcrs+nxp)=a                                                           !/*in         92*/
      go to 350                                                                 !/*in         93*/
!                                                                               !/*in         94*/
!        do the case where jsu is a line and js a quadratic.                    !/*in         95*/
  160 if(kfm(lkfm+jsu).ne.1)go to 180                                           !/*in         96*/
      cf(1)=r(2,2)*c(2)**2+2.*r(2,3)*c(2)*c(5)+r(3,3)*c(5)**2                   !/*in         97*/
      cf(2)=2.*(r(2,2)*c(1)*c(2)+r(2,3)*(c(4)*c(2)+c(1)*c(5))+                  !/*in         98*/
     & r(3,3)*c(4)*c(5)+r(1,2)*c(2)+r(1,3)*c(5))                                !/*in         99*/
      cf(3)=r(2,2)*c(1)**2+2.*r(2,3)*c(1)*c(4)+r(3,3)*c(4)**2+                  !/*in        100*/
     & 2.*r(1,2)*c(1)+2.*r(1,3)*c(4)+r(1,1)                                     !/*in        101*/
      call quad(cf,n,qr(1),qr(2))                                               !/*in        102*/
      do 170 i=1,n                                                              !/*in        103*/
      if(abs(c(1)+c(2)*qr(i)).gt.1..or.abs(c(4)+c(5)*qr(i)).gt.1.)              !/*in        104*/
     & go to 170                                                                !/*in        105*/
      if(isheet(qr(i),iqc,jsu).ne.0)go to 170                                   !/*in        106*/
      nxp=nxp+1                                                                 !/*in        107*/
      crs(lcrs+nxp)=qr(i)                                                       !/*in        108*/
  170 continue                                                                  !/*in        109*/
      go to 350                                                                 !/*in        110*/
!                                                                               !/*in        111*/
!        do the case where js is a line and jsu a quadratic.                    !/*in        112*/
  180 if(kfm(lkfm+js).ne.1)go to 220                                            !/*in        113*/
      cf(1)=q(2,2)*d(2)**2+2.*q(2,3)*d(2)*d(5)+q(3,3)*d(5)**2                   !/*in        114*/
      cf(2)=2.*(q(2,2)*d(1)*d(2)+q(2,3)*(d(4)*d(2)+d(1)*d(5))+                  !/*in        115*/
     & q(3,3)*d(4)*d(5)+q(1,2)*d(2)+q(1,3)*d(5))                                !/*in        116*/
      cf(3)=q(2,2)*d(1)**2+2.*q(2,3)*d(1)*d(4)+q(3,3)*d(4)**2+                  !/*in        117*/
     & 2.*q(1,2)*d(1)+2.*q(1,3)*d(4)+q(1,1)                                     !/*in        118*/
      call quad(cf,n,qr(1),qr(2))                                               !/*in        119*/
      do 210 i=1,n                                                              !/*in        120*/
      s=d(1)+d(2)*qr(i)                                                         !/*in        121*/
      t=d(4)+d(5)*qr(i)                                                         !/*in        122*/
      if(abs(s).gt.1..or.abs(t).gt.1.)go to 210                                 !/*in        123*/
      if(isheet(qr(i),iq,js).ne.0)go to 210                                     !/*in        124*/
      call sttop(1,s,iqc,jsu,m1,g(1),g(2),p(1),p(2))                            !/*in        125*/
      call sttop(2,t,iqc,jsu,m2,g(3),g(4),p(3),p(4))                            !/*in        126*/
      do 190 j=1,m1                                                             !/*in        127*/
      do 190 k=1,m2                                                             !/*in        128*/
  190 if((s-g(k+2))**2+(t-g(j))**2.lt.res**2)go to 200                          !/*in        129*/
      go to 210                                                                 !/*in        130*/
  200 nxp=nxp+1                                                                 !/*in        131*/
      crs(lcrs+nxp)=.5*(p(j)+p(k+2))                                            !/*in        132*/
  210 continue                                                                  !/*in        133*/
      go to 350                                                                 !/*in        134*/
!                                                                               !/*in        135*/
!        do the case where both curves are quadratics.                          !/*in        136*/
  220 if(abs(r(2,2)).gt.1.e-8*max(abs(r(2,3)),abs(r(3,3))).and.                 !/*in        137*/
     & abs(q(2,2)).gt.1.e-8*max(abs(q(2,3)),abs(q(3,3))))go to 240              !/*in        138*/
      k=1                                                                       !/*in        139*/
      if(abs(q(2,2))*max(abs(r(2,3)),abs(r(3,3))).lt.                           !/*in        140*/
     & abs(r(2,2))*max(abs(q(2,3)),abs(q(3,3))))k=2                             !/*in        141*/
      do 230 i=1,3                                                              !/*in        142*/
      do 230 j=1,3                                                              !/*in        143*/
      e(i,j,k)=q(i,j)                                                           !/*in        144*/
  230 e(i,j,3-k)=r(i,j)                                                         !/*in        145*/
      go to 260                                                                 !/*in        146*/
  240 do 250 i=1,3                                                              !/*in        147*/
      do 250 j=1,3                                                              !/*in        148*/
      e(i,j,1)=q(i,j)+r(i,j)                                                    !/*in        149*/
  250 e(i,j,2)=q(i,j)*r(2,2)-r(i,j)*q(2,2)                                      !/*in        150*/
  260 if(e(2,3,2).ne.0..or.e(1,2,2).ne.0.)go to 270                             !/*in        151*/
      cf(3)=e(3,3,2)                                                            !/*in        152*/
      cf(4)=2.*e(1,3,2)                                                         !/*in        153*/
      cf(5)=e(1,1,2)                                                            !/*in        154*/
      go to 310                                                                 !/*in        155*/
  270 cf(1)=e(2,2,1)*e(3,3,2)**2+4.*e(2,3,2)*                                   !/*in        156*/
     & (e(3,3,1)*e(2,3,2)-e(2,3,1)*e(3,3,2))                                    !/*in        157*/
      cf(2)=4.*(e(3,3,2)*(e(2,2,1)*e(1,3,2)-e(2,3,1)*e(1,2,2)-                  !/*in        158*/
     & e(1,2,1)*e(2,3,2))+2.*e(2,3,2)*(e(3,3,1)*e(1,2,2)+                       !/*in        159*/
     & e(1,3,1)*e(2,3,2)-e(2,3,1)*e(1,3,2)))                                    !/*in        160*/
      cf(3)=2.*(e(2,2,1)*e(3,3,2)*e(1,1,2)+2.*(e(2,2,1)*e(1,3,2)**2+            !/*in        161*/
     & e(2,3,2)*(e(1,1,1)*e(2,3,2)-e(2,3,1)*e(1,1,2))+e(1,2,2)*                 !/*in        162*/
     & (e(3,3,1)*e(1,2,2)-e(1,2,1)*e(3,3,2)))-4.*e(1,3,2)*(e(2,3,1)*            !/*in        163*/
     & e(1,2,2)+e(1,2,1)*e(2,3,2))+8.*e(1,3,1)*e(1,2,2)*e(2,3,2))               !/*in        164*/
      cf(4)=4.*(e(1,1,2)*(e(2,2,1)*e(1,3,2)-e(2,3,1)*e(1,2,2)-                  !/*in        165*/
     & e(1,2,1)*e(2,3,2))+2.*e(1,2,2)*(e(1,1,1)*e(2,3,2)+                       !/*in        166*/
     & e(1,3,1)*e(1,2,2)-e(1,2,1)*e(1,3,2)))                                    !/*in        167*/
      cf(5)=e(2,2,1)*e(1,1,2)**2+4.*e(1,2,2)*                                   !/*in        168*/
     & (e(1,1,1)*e(1,2,2)-e(1,2,1)*e(1,1,2))                                    !/*in        169*/
      if(abs(cf(1)).le.1.e-8*max(abs(cf(2)),abs(cf(3)),abs(cf(4)),              !/*in        170*/
     & abs(cf(5))))go to 290                                                    !/*in        171*/
      do 280 i=2,5                                                              !/*in        172*/
  280 cf(i)=cf(i)/cf(1)                                                         !/*in        173*/
      call quart(4,cf,qr,n,1)                                                   !/*in        174*/
      go to 320                                                                 !/*in        175*/
  290 if(abs(cf(2)).le.1.e-8*max(abs(cf(3)),abs(cf(4)),abs(cf(5))))             !/*in        176*/
     & go to 310                                                                !/*in        177*/
      do 300 i=3,5                                                              !/*in        178*/
  300 cf(i)=cf(i)/cf(2)                                                         !/*in        179*/
      call quart(3,cf(2),qr,n,1)                                                !/*in        180*/
      go to 320                                                                 !/*in        181*/
  310 call quad(cf(3),n,qr(1),qr(2))                                            !/*in        182*/
  320 do 340 i=1,n                                                              !/*in        183*/
      if(abs(qr(i)).gt.1.)go to 340                                             !/*in        184*/
      call sttop(2,qr(i),iqc,jsu,m1,g(1),g(2),p(1),p(2))                        !/*in        185*/
      call sttop(2,qr(i),iq,js,m2,g(3),g(4),p(3),p(4))                          !/*in        186*/
      do 330 j=1,m1                                                             !/*in        187*/
      do 330 k=1,m2                                                             !/*in        188*/
      if(abs(g(j)).gt.1..or.abs(g(j)-g(k+2)).gt.res)go to 330                   !/*in        189*/
      nxp=nxp+1                                                                 !/*in        190*/
      crs(lcrs+nxp)=p(j)                                                        !/*in        191*/
  330 continue                                                                  !/*in        192*/
  340 continue                                                                  !/*in        193*/
  350 if(nxp.ge.4*mxj+24)call expire(0,'inter',                                 !/*inx         9*/
     & 'too many intersections.  arrays crs and lsg overflow.')                 !/*in        195*/
  360 continue                                                                  !/*in        196*/
      if(nxp.le.1)return                                                        !/*in        197*/
!                                                                               !/*in        198*/
!        sort the intersections.                                                !/*in        199*/
      do 380 i=1,nxp-1                                                          !/*in        200*/
      do 370 j=i+1,nxp                                                          !/*in        201*/
      if(crs(lcrs+i).lt.crs(lcrs+j))go to 370                                   !/*in        202*/
      a=crs(lcrs+i)                                                             !/*in        203*/
      crs(lcrs+i)=crs(lcrs+j)                                                   !/*in        204*/
      crs(lcrs+j)=a                                                             !/*in        205*/
  370 continue                                                                  !/*in        206*/
  380 continue                                                                  !/*in        207*/
!                                                                               !/*in        208*/
!        delete any duplicate intersections.                                    !/*in        209*/
      p1=crs(lcrs+1)                                                            !/*in        210*/
      call ptost(p1,iqc,jsu,s1,t1)                                              !/*in        211*/
      i=1                                                                       !/*in        212*/
  390 p1=crs(lcrs+i+1)                                                          !/*in        213*/
      call ptost(p1,iqc,jsu,s2,t2)                                              !/*in        214*/
      if((s1-s2)**2+(t1-t2)**2.lt.res**2)go to 400                              !/*in        215*/
      s1=s2                                                                     !/*in        216*/
      t1=t2                                                                     !/*in        217*/
      i=i+1                                                                     !/*in        218*/
      go to 420                                                                 !/*in        219*/
  400 do 410 j=i+1,nxp-1                                                        !/*in        220*/
  410 crs(lcrs+j)=crs(lcrs+j+1)                                                 !/*in        221*/
      nxp=nxp-1                                                                 !/*in        222*/
  420 if(i.lt.nxp)go to 390                                                     !/*in        223*/
#endif                                                                          /*!inx        10*/
      return                                                                    !/*in        224*/
      end                                                                       !/*in        225*/

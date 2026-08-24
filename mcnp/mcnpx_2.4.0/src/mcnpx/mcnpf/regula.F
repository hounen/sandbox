!#ident regula.F from deck rg                                                   !/*rg          1*/
      subroutine regula(js,am,ls)                                               !/*rg          3*/
#ifdef PLOT                                                                     /*!rgx         1*/
!        calculate the coefficients coe and the matrix qmx for the              !/*rg          4*/
!        curves where the surface js intersects the plot plane.                 !/*rg          5*/
!        am is the amx matrix of surface js.                                    !/*rg          6*/
!        ls.ne.0 means accept straight lines outside the plot window.           !/*rg          7*/
      use global_data                                                           !/*rgx         2*/
      use plot_data                                                             !/*rgx         3*/
!                                                                               !/*rgx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*rgx         5*/
      implicit integer(kindi) (i-n)                                             !/*rgx         6*/
!                                                                               !/*rgx         7*/
      dimension aa(3,3),bb(4,4),cc(3,3),ct(6,2),dd(3,3),vc(2:4),am(4,4)         !/*rg         10*/
      data dd/1.,8*0./                                                          !/*rg         11*/
      c(i)=coe(lcoe+i,iq,js)                                                    !/*rg         12*/
      q(i,j)=qmx(lqmx+i,j,iq,js)                                                !/*rg         13*/
!                                                                               !/*rg         14*/
      ncs(lncs+js)=0                                                            !/*rg         15*/
      if(kst(lkst+js).gt.23)go to 170                                           !/*rg4a        1*/
      kfm(lkfm+js)=1                                                            !/*rg         17*/
      iq=1                                                                      !/*rg         18*/
!                                                                               !/*rg         19*/
!        transform the amx matrix into the qmx matrix.                          !/*rg         20*/
!        (1,x,y,z)t*amx*(1,x,y,z)=0     (1,s,t)t*qmx*(1,s,t)=0                  !/*rg         21*/
!        qmx=(pxx)t*amx*pxx                                        c g f        !/*rg         22*/
!        qmx contains the coefficients of the conic          qmx = g a h        !/*rg         23*/
!        a*s**2 + 2*h*s*t + b*t**2 + 2*g*s + 2*f*t + c = 0         f h b        !/*rg         24*/
      call matmpy(4,4,3,am,4,pxx,4,bb,4,0)                                      !/*rg         25*/
      call matmpy(3,4,3,pxx,4,bb,4,aa,3,4)                                      !/*rg         26*/
      do 10 i=1,3                                                               !/*rg         27*/
      do 10 j=1,3                                                               !/*rg         28*/
   10 qmx(lqmx+i,j,1,js)=aa(i,j)/extent(1)                                      !/*rg         29*/
!                                                                               !/*rg         30*/
!        scale qmx so that it corresponds to a plot window size 2x2.            !/*rg         31*/
      do 20 i=1,3                                                               !/*rg         32*/
      do 20 j=2,3                                                               !/*rg         33*/
      qmx(lqmx+i,j,1,js)=q(i,j)*extent(j-1)                                     !/*rg         34*/
   20 qmx(lqmx+j,i,1,js)=q(j,i)*extent(j-1)                                     !/*rg         35*/
!                                                                               !/*rg         36*/
!        one straight line if qmx is not a quadratic.                           !/*rg         37*/
!        reject it if it does not cross the plot window.                        !/*rg         38*/
      t=abs(q(1,2))+abs(q(1,3))                                                 !/*rg4b        1*/
      if(abs(q(2,2))+abs(q(2,3))+abs(q(3,3)).gt.res*t)go to 30                  !/*rg4b        2*/
      if(ls.ne.2.and.abs(q(1,1))+t.lt.1.e-6)call erprnt(1,2,1,                  !/*rg4c2       1*/
     & nsf(lnsf+js),0,0,0,0,'34hplot plane coincident with surface,i7')         !/*rg4c2       2*/
      if(abs(q(1,1)).gt.2.*t.and.ls.eq.0)return                                 !/*rg4b        5*/
      if(t.eq.0.)return                                                         !/*rg4b        6*/
      ncs(lncs+js)=1                                                            !/*rg         43*/
      a=.5*q(1,1)/(q(1,2)**2+q(1,3)**2)                                         !/*rg         44*/
      coe(lcoe+1,1,js)=-a*q(1,2)                                                !/*rg         45*/
      coe(lcoe+2,1,js)=q(1,3)                                                   !/*rg         46*/
      coe(lcoe+3,1,js)=0.                                                       !/*rg         47*/
      coe(lcoe+4,1,js)=-a*q(1,3)                                                !/*rg         48*/
      coe(lcoe+5,1,js)=-q(1,2)                                                  !/*rg         49*/
      coe(lcoe+6,1,js)=0.                                                       !/*rg         50*/
      return                                                                    !/*rg         51*/
!                                                                               !/*rg         52*/
!        clear unskewed coefficients.  calculate quadratic invariants.          !/*rg         53*/
   30 do 40 j=1,2                                                               !/*rg         54*/
      do 40 i=1,6                                                               !/*rg         55*/
   40 ct(i,j)=0.                                                                !/*rg         56*/
      qi=q(2,2)+q(3,3)                                                          !/*rg         57*/
      qj=q(2,2)*q(3,3)-q(2,3)**2                                                !/*rg         58*/
      qd=qj*q(1,1)+2.*q(1,2)*q(1,3)*q(2,3)-q(2,2)*q(1,3)**2-                    !/*rg         59*/
     & q(3,3)*q(1,2)**2                                                         !/*rg         60*/
      qk=q(1,3)**2+q(1,2)**2                                                    !/*rg         61*/
!                                                                               !/*rg         62*/
!        transform qmx into aa, which is the same conic unskewed and as         !/*rg         63*/
!        centered as possible.      aa=(dd)t*qmx*dd                             !/*rg         64*/
      ag=0.                                                                     !/*rg         65*/
      if(abs(q(2,3))+abs(q(2,2)-q(3,3)).ne.0.)                                  !/*rg         66*/
     & ag=.5*atan2(2.*q(2,3),q(2,2)-q(3,3))                                     !/*rg         67*/
      dd(2,2)=cos(ag)                                                           !/*rg         68*/
      dd(3,3)=dd(2,2)                                                           !/*rg         69*/
      dd(3,2)=sin(ag)                                                           !/*rg         70*/
      dd(2,3)=-dd(3,2)                                                          !/*rg         71*/
      if(abs(qj).gt.(res*qi)**2)go to 50                                        !/*rg         72*/
      t1=-(q(1,2)*dd(2,2)+q(1,3)*dd(3,2))/qi                                    !/*rg         73*/
      dd(2,1)=t1*dd(2,2)                                                        !/*rg         74*/
      dd(3,1)=t1*dd(3,2)                                                        !/*rg         75*/
      go to 60                                                                  !/*rg         76*/
   50 dd(2,1)=(-q(1,2)*q(3,3)+q(1,3)*q(2,3))/qj                                 !/*rg         77*/
      dd(3,1)=(-q(1,3)*q(2,2)+q(1,2)*q(2,3))/qj                                 !/*rg         78*/
   60 do 70 i=1,3                                                               !/*rg         79*/
      do 70 j=1,3                                                               !/*rg         80*/
   70 aa(i,j)=qmx(lqmx+i,j,1,js)                                                !/*rg         81*/
      call matmpy(3,3,3,aa,3,dd,3,cc,3,0)                                       !/*rg         82*/
      call matmpy(3,3,3,dd,3,cc,3,aa,3,4)                                       !/*rg         83*/
!                                                                               !/*rg         84*/
!        parabola or parallel lines if invariant qj is small enough.            !/*rg         85*/
      if(abs(qj).gt.(res*qi)**2)go to 100                                       !/*rg         86*/
      j=2                                                                       !/*rg         87*/
      if(abs(aa(3,3)).gt.abs(aa(2,2)))j=3                                       !/*rg         88*/
!                                                                               !/*rg         89*/
!        parallel lines if invariant qd is small enough.                        !/*rg         90*/
      if(abs(qd).gt.res**2*(abs(qi*qk)+abs(qi**2*q(1,1))))go to 90              !/*rg         91*/
      if(aa(1,1)*aa(j,j).gt.0.)return                                           !/*rg         92*/
      ncs(lncs+js)=2                                                            !/*rg         93*/
      do 80 i=1,2                                                               !/*rg         94*/
      ct(3*j-5,i)=(3-2*i)*sqrt(-aa(1,1)/aa(j,j))                                !/*rg         95*/
   80 ct(11-3*j,i)=1.                                                           !/*rg         96*/
      go to 140                                                                 !/*rg         97*/
!                                                                               !/*rg         98*/
!        parabola.                                                              !/*rg         99*/
   90 ncs(lncs+js)=1                                                            !/*rg        100*/
      kfm(lkfm+js)=2                                                            !/*rg        101*/
      ct(3*j-4,1)=1.                                                            !/*rg        102*/
      ct(10-3*j,1)=-.5*aa(1,1)/aa(1,5-j)                                        !/*rg        103*/
      ct(12-3*j,1)=-.5*aa(j,j)/aa(1,5-j)                                        !/*rg        104*/
      go to 140                                                                 !/*rg        105*/
!                                                                               !/*rg        106*/
!        ellipse if invariant qj > 0.                                           !/*rg        107*/
  100 if(qj.lt.0.)go to 110                                                     !/*rg        108*/
      if(qd*qi.ge.0.)return                                                     !/*rg        109*/
      if(max(aa(1,1)*aa(2,2),aa(1,1)*aa(3,3)).ge.0.)return                      !/*rg        110*/
      ct(3,1)=sqrt(-aa(1,1)/aa(2,2))                                            !/*rg        111*/
      ct(5,1)=sqrt(-aa(1,1)/aa(3,3))                                            !/*rg        112*/
      if(max(ct(3,1),ct(5,1)).lt.res)return                                     !/*rg        113*/
      ncs(lncs+js)=1                                                            !/*rg        114*/
      kfm(lkfm+js)=3                                                            !/*rg        115*/
      go to 140                                                                 !/*rg        116*/
!                                                                               !/*rg        117*/
!        intersecting lines if constant term is small enough.                   !/*rg        118*/
  110 if(max(-aa(1,1)/aa(2,2),-aa(1,1)/aa(3,3)).gt.res**2)go to 120             !/*rg        119*/
      ncs(lncs+js)=2                                                            !/*rg        120*/
      ct(2,1)=sqrt(aa(3,3)/(aa(3,3)-aa(2,2)))                                   !/*rg        121*/
      ct(2,2)=ct(2,1)                                                           !/*rg        122*/
      ct(5,1)=sqrt(aa(2,2)/(aa(2,2)-aa(3,3)))                                   !/*rg        123*/
      ct(5,2)=-ct(5,1)                                                          !/*rg        124*/
      go to 140                                                                 !/*rg        125*/
!                                                                               !/*rg        126*/
!        hyperbola.                                                             !/*rg        127*/
  120 ncs(lncs+js)=2                                                            !/*rg        128*/
      kfm(lkfm+js)=4                                                            !/*rg        129*/
      j=2                                                                       !/*rg        130*/
      if(aa(1,1)*aa(3,3).lt.0.)j=3                                              !/*rg        131*/
      ct(3*j-3,1)=sqrt(-aa(1,1)/aa(j,j))                                        !/*rg        132*/
      ct(11-3*j,1)=sqrt(aa(1,1)/aa(5-j,5-j))                                    !/*rg        133*/
      ct(3*j-3,2)=-ct(3*j-3,1)                                                  !/*rg        134*/
      ct(11-3*j,2)=ct(11-3*j,1)                                                 !/*rg        135*/
      do 130 j=1,3                                                              !/*rg        136*/
      do 130 i=1,3                                                              !/*rg        137*/
  130 qmx(lqmx+i,j,2,js)=q(i,j)                                                 !/*rg        138*/
!                                                                               !/*rg        139*/
!        transform unskewed coefficients into s,t coefficients.                 !/*rg        140*/
  140 do 150 iq=1,ncs(lncs+js)                                                  !/*rg        141*/
      do 150 i=1,3                                                              !/*rg        142*/
      do 150 k=1,2                                                              !/*rg        143*/
  150 coe(lcoe+i+3*k-3,iq,js)=dd(k+1,1)*dd(1,i)+dd(k+1,2)*ct(i,iq)+             !/*rg        144*/
     & dd(k+1,3)*ct(i+3,iq)                                                     !/*rg        145*/
!                                                                               !/*rg        146*/
!        calculate factored qmx matrices for line pairs.                        !/*rg        147*/
      if(kfm(lkfm+js).ne.1.or.ncs(lncs+js).ne.2)go to 240                       !/*rg        148*/
      do 160 iq=1,2                                                             !/*rg        149*/
      qmx(lqmx+1,1,iq,js)=2.*(c(2)*c(4)-c(5)*c(1))                              !/*rg        150*/
      do 160 k=1,2                                                              !/*rg        151*/
      qmx(lqmx+k+1,2,iq,js)=0.                                                  !/*rg        152*/
      qmx(lqmx+k+1,3,iq,js)=0.                                                  !/*rg        153*/
      qmx(lqmx+k+1,1,iq,js)=(3-2*k)*c(8-3*k)                                    !/*rg        154*/
  160 qmx(lqmx+1,k+1,iq,js)=q(k+1,1)                                            !/*rg        155*/
      go to 240                                                                 !/*rg        156*/
!                                                                               !/*rg        157*/
!        torus.                                                                 !/*rg        158*/
  170 kfm(lkfm+js)=3                                                            !/*rg        159*/
      k=kst(lkst+js)-22                                                         !/*rg4a        2*/
      i=mod(k-1,3)+2                                                            !/*rg        161*/
      j=9-k-i                                                                   !/*rg        162*/
      lc=lsc(llsc+js)                                                           !/*rg        163*/
      do 180 m=2,4                                                              !/*rg        164*/
  180 vc(m)=scf(lc+m-1)-pxx(m,1)                                                !/*rg        165*/
!                                                                               !/*rg        166*/
!        circles if the plot plane is normal to the torus axis.                 !/*rg        167*/
      if(abs(pxx(k,2))+abs(pxx(k,3)).ne.0.)go to 200                            !/*rg        168*/
      ca=scf(lc+6)**2-scf(lc+7)*vc(k)**2                                        !/*rg        169*/
      if(ca.le.0.)return                                                        !/*rg        170*/
      ca=sqrt(ca)                                                               !/*rg        171*/
      ncs(lncs+js)=2                                                            !/*rg        172*/
      if(scf(lc+4)-ca.lt.res*min(extent(1),extent(2)))ncs(lncs+js)=1            !/*rg        173*/
      do 190 iq=1,ncs(lncs+js)                                                  !/*rg        174*/
      coe(lcoe+1,iq,js)=(pxx(i,2)*vc(i)+pxx(j,2)*vc(j))/extent(1)               !/*rg        175*/
      coe(lcoe+2,iq,js)=0.                                                      !/*rg        176*/
      coe(lcoe+3,iq,js)=(scf(lc+4)+(3-2*iq)*ca)/extent(1)                       !/*rg        177*/
      coe(lcoe+4,iq,js)=(pxx(i,3)*vc(i)+pxx(j,3)*vc(j))/extent(2)               !/*rg        178*/
      coe(lcoe+5,iq,js)=(scf(lc+4)+(3-2*iq)*ca)/extent(2)                       !/*rg        179*/
  190 coe(lcoe+6,iq,js)=0.                                                      !/*rg        180*/
      go to 220                                                                 !/*rg        181*/
!                                                                               !/*rg        182*/
!        two ellipses if the plot plane contains the torus axis.                !/*rg        183*/
  200 if(pxx(k,4)**2*scf(lc+5).gt.res*min(extent(1),extent(2)).or.              !/*rg        184*/
     & (vc(i)*pxx(i,4)+vc(j)*pxx(j,4))**2.gt.res*min(extent(1),                 !/*rg        185*/
     & extent(2))*(scf(lc+4)+scf(lc+6)))return                                  !/*rg        186*/
      ncs(lncs+js)=2                                                            !/*rg        187*/
      do 210 iq=1,2                                                             !/*rg        188*/
      coe(lcoe+1,iq,js)=(pxx(2,2)*vc(2)+pxx(3,2)*vc(3)+                         !/*rg        189*/
     & pxx(4,2)*vc(4)+(3-2*iq)*pxx(k,3)*scf(lc+4))/extent(1)                    !/*rg        190*/
      coe(lcoe+2,iq,js)=-scf(lc+6)*pxx(k,3)/extent(1)                           !/*rg        191*/
      coe(lcoe+3,iq,js)=scf(lc+5)*pxx(k,2)/extent(1)                            !/*rg        192*/
      coe(lcoe+4,iq,js)=(pxx(2,3)*vc(2)+pxx(3,3)*vc(3)+                         !/*rg        193*/
     & pxx(4,3)*vc(4)-(3-2*iq)*pxx(k,2)*scf(lc+4))/extent(2)                    !/*rg        194*/
      coe(lcoe+5,iq,js)=scf(lc+6)*pxx(k,2)/extent(2)                            !/*rg        195*/
  210 coe(lcoe+6,iq,js)=scf(lc+5)*pxx(k,3)/extent(2)                            !/*rg        196*/
!                                                                               !/*rg        197*/
!        create the qmx matrices for the torus intersections.                   !/*rg        198*/
  220 do 230 iq=1,ncs(lncs+js)                                                  !/*rg        199*/
      qmx(lqmx+2,2,iq,js)=c(5)**2+c(6)**2                                       !/*rg        200*/
      qmx(lqmx+2,3,iq,js)=-c(5)*c(2)-c(6)*c(3)                                  !/*rg        201*/
      qmx(lqmx+3,2,iq,js)=q(2,3)                                                !/*rg        202*/
      qmx(lqmx+3,3,iq,js)=c(2)**2+c(3)**2                                       !/*rg        203*/
      qmx(lqmx+1,2,iq,js)=-q(2,3)*c(4)-q(2,2)*c(1)                              !/*rg        204*/
      qmx(lqmx+2,1,iq,js)=q(1,2)                                                !/*rg        205*/
      qmx(lqmx+1,3,iq,js)=-q(2,3)*c(1)-q(3,3)*c(4)                              !/*rg        206*/
      qmx(lqmx+3,1,iq,js)=q(1,3)                                                !/*rg        207*/
  230 qmx(lqmx+1,1,iq,js)=q(2,2)*c(1)**2+2.*q(2,3)*c(1)*c(4)+                   !/*rg        208*/
     & q(3,3)*c(4)**2-(c(6)*c(2)-c(5)*c(3))**2                                  !/*rg        209*/
!                                                                               !/*rg        210*/
!        reduce ncs if a curve fails to cross the plot window.                  !/*rg        211*/
  240 if(ls.eq.2)return                                                         !/*rg4c2       3*/
      do 290 iq=1,ncs(lncs+js)                                                  !/*rg4c2       4*/
      if(kfm(lkfm+js).ne.1.and.isheet(zero,iq,js).ne.0)go to 260                !/*rg        213*/
      do 250 i=1,2                                                              !/*rg        214*/
      do 250 m=-1,1,2                                                           !/*rg        215*/
      call sttop(i,zero+m,iq,js,n,g1,g2,p1,p2)                                  !/*rg        216*/
  250 if(n.eq.2.and.abs(g2).lt.1..or.n.ge.1.and.abs(g1).le.1.)go to 290         !/*rg        217*/
!                                                                               !/*rg        218*/
!        check for the case of an ellipse entirely inside the window.           !/*rg        219*/
      if(kfm(lkfm+js).eq.3.and.max(abs(c(1)+c(2)),                              !/*rg        220*/
     & abs(c(4)+c(5))).le.1.)go to 290                                          !/*rg        221*/
  260 ncs(lncs+js)=ncs(lncs+js)-1                                               !/*rg        222*/
      if(iq.ne.1.or.ncs(lncs+js).eq.0)go to 290                                 !/*rg        223*/
      do 270 i=1,6                                                              !/*rg        224*/
  270 coe(lcoe+i,1,js)=coe(lcoe+i,2,js)                                         !/*rg        225*/
      do 280 j=1,3                                                              !/*rg        226*/
      do 280 i=1,3                                                              !/*rg        227*/
  280 qmx(lqmx+i,j,1,js)=qmx(lqmx+i,j,2,js)                                     !/*rg        228*/
  290 continue                                                                  !/*rg        229*/
#endif                                                                          /*!rgx         8*/
      return                                                                    !/*rg        230*/
      end                                                                       !/*rg        231*/

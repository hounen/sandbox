!#ident contr.F from deck cf                                                    !/*cf          1*/
      subroutine contr(z,x,y,nx,ny,xw,yw)                                       !/*cfx         1*/
#ifdef MCPLOT                                                                   /*!cfx         2*/
!        draw contour lines on a rectangular mesh.                              !/*cf          4*/
!        z   nx by ny array defining a function of x and y.                     !/*cf          5*/
!        x   array of x-values.                                                 !/*cf          6*/
!        y   array of y-values.                                                 !/*cf          7*/
!        xw, yw   work storage for contour points, sized according to           !/*cf          8*/
!        the maximum number of points in a connected curve of a contour.        !/*cf          9*/
!        mc  work storage for flags.                                            !/*cf         10*/
      use global_data                                                           !/*cfx         3*/
      use mcplot_data                                                           !/*cfx         4*/
!                                                                               !/*cfx         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*cfx         6*/
      implicit integer(kindi) (i-n)                                             !/*cfx         7*/
!                                                                               !/*cfx         8*/
      dimension z(nx,ny),x(nx),y(ny),xw(*),yw(*)                                !/*cfx         9*/
      integer(kindi) :: mc(nx,ny)                                               !/*cfx        10*/
!                                                                               !/*cf         14*/
      ns=0                                                                      !/*cf         15*/
      do 310 n=1,nclev                                                          !/*cf         16*/
      cc=clev(n)                                                                !/*cf         17*/
      mc=0                                                                      !/*cfx        11*/
!                                                                               !/*cf         21*/
!        edge j=1.                                                              !/*cf         22*/
      ia=1                                                                      !/*cf         23*/
      i=1                                                                       !/*cf         24*/
   20 if((cc-z(i,1))*(cc-z(i+1,1)).gt.0.)go to 30                               !/*cf         25*/
      ic=i                                                                      !/*cf         26*/
      jc=1                                                                      !/*cf         27*/
      ib=-1                                                                     !/*cf         28*/
      in=1                                                                      !/*cf         29*/
      go to 220                                                                 !/*cf         30*/
   30 i=i+1                                                                     !/*cf         31*/
      if(i.le.nx-1)go to 20                                                     !/*cf         32*/
!                                                                               !/*cf         33*/
!        edge j=ny.                                                             !/*cf         34*/
      ia=2                                                                      !/*cf         35*/
      i=1                                                                       !/*cf         36*/
   40 if((cc-z(i,ny))*(cc-z(i+1,ny)).gt.0.)go to 50                             !/*cf         37*/
      ic=i                                                                      !/*cf         38*/
      jc=ny-1                                                                   !/*cf         39*/
      in=3                                                                      !/*cf         40*/
      ib=-1                                                                     !/*cf         41*/
      go to 260                                                                 !/*cf         42*/
   50 i=i+1                                                                     !/*cf         43*/
      if(i.le.nx-1)go to 40                                                     !/*cf         44*/
!                                                                               !/*cf         45*/
!        edge i=1.                                                              !/*cf         46*/
      ia=3                                                                      !/*cf         47*/
      j=1                                                                       !/*cf         48*/
   60 if((cc-z(1,j))*(cc-z(1,j+1)).gt.0.)go to 70                               !/*cf         49*/
      ic=1                                                                      !/*cf         50*/
      jc=j                                                                      !/*cf         51*/
      ib=-1                                                                     !/*cf         52*/
      in=4                                                                      !/*cf         53*/
      go to 280                                                                 !/*cf         54*/
   70 j=j+1                                                                     !/*cf         55*/
      if(j.le.ny-1)go to 60                                                     !/*cf         56*/
!                                                                               !/*cf         57*/
!        edge i=nx.                                                             !/*cf         58*/
      ia=4                                                                      !/*cf         59*/
      j=1                                                                       !/*cf         60*/
   80 if((cc-z(nx,j))*(cc-z(nx,j+1)).gt.0.)go to 90                             !/*cf         61*/
      ic=nx-1                                                                   !/*cf         62*/
      jc=j                                                                      !/*cf         63*/
      ib=-1                                                                     !/*cf         64*/
      in=2                                                                      !/*cf         65*/
      go to 240                                                                 !/*cf         66*/
   90 j=j+1                                                                     !/*cf         67*/
      if(j.le.ny-1)go to 80                                                     !/*cf         68*/
!                                                                               !/*cf         69*/
!        check horizontal segments.                                             !/*cf         70*/
      ia=5                                                                      !/*cf         71*/
      j=2                                                                       !/*cf         72*/
  100 i=1                                                                       !/*cf         73*/
  110 if((cc-z(i,j))*(cc-z(i+1,j)).gt.0.)go to 120                              !/*cf         74*/
      ic=i                                                                      !/*cf         75*/
      jc=j                                                                      !/*cf         76*/
      ib=-1                                                                     !/*cf         77*/
      in=1                                                                      !/*cf         78*/
      go to 220                                                                 !/*cf         79*/
  120 i=i+1                                                                     !/*cf         80*/
      if(i.le.nx-1)go to 110                                                    !/*cf         81*/
      j=j+1                                                                     !/*cf         82*/
      if(j.le.ny-1)go to 100                                                    !/*cf         83*/
      go to 310                                                                 !/*cf         84*/
!                                                                               !/*cf         85*/
!        determine starting point of vector.                                    !/*cf         86*/
  130 if(ib.gt.0)go to 140                                                      !/*cf         87*/
      c=0.                                                                      !/*cf         88*/
      if(z(i2,j2).ne.z(i1,j1))c=(cc-z(i1,j1))/(z(i2,j2)-z(i1,j1))               !/*cf         89*/
      xw(1)=c*(x(i2)-x(i1))+x(i1)                                               !/*cf         90*/
      yw(1)=c*(y(j2)-y(j1))+y(j1)                                               !/*cf         91*/
      x1=xw(1)                                                                  !/*cf         92*/
      y1=yw(1)                                                                  !/*cf         93*/
      ns=1                                                                      !/*cf         94*/
  140 ip=1                                                                      !/*cf         95*/
      if(z(i2,j2).ge.z(i1,j1))go to 170                                         !/*cf         96*/
!                                                                               !/*cf         97*/
!        second point in left side.                                             !/*cf         98*/
  150 if((cc-z(i1,j1))*(cc-z(i4,j4)).gt.0.)go to(160,190)ip                     !/*cf         99*/
      id=1                                                                      !/*cf        100*/
      c=1.                                                                      !/*cf        101*/
      if(z(i1,j1).ne.z(i4,j4))c=(cc-z(i1,j1))/(z(i4,j4)-z(i1,j1))               !/*cf        102*/
      x2=c*(x(i4)-x(i1))+x(i1)                                                  !/*cf        103*/
      y2=c*(y(j4)-y(j1))+y(j1)                                                  !/*cf        104*/
      go to 190                                                                 !/*cf        105*/
!                                                                               !/*cf        106*/
!        second point in opposite side.                                         !/*cf        107*/
  160 if((cc-z(i4,j4))*(cc-z(i3,j3)).gt.0.)go to(180,150)ip                     !/*cf        108*/
      id=0                                                                      !/*cf        109*/
      c=(cc-z(i4,j4))/(z(i3,j3)-z(i4,j4))                                       !/*cf        110*/
      x2=c*(x(i3)-x(i4))+x(i4)                                                  !/*cf        111*/
      y2=c*(y(j3)-y(j4))+y(j4)                                                  !/*cf        112*/
      go to 190                                                                 !/*cf        113*/
!                                                                               !/*cf        114*/
!        when z(i2,j2).ge.z(i1,j1).                                             !/*cf        115*/
  170 ip=2                                                                      !/*cf        116*/
!                                                                               !/*cf        117*/
!        second point in right side.                                            !/*cf        118*/
  180 if((cc-z(i3,j3))*(cc-z(i2,j2)).gt.0.)go to(190,160)ip                     !/*cf        119*/
      id=3                                                                      !/*cf        120*/
      c=1.                                                                      !/*cf        121*/
      if(z(i2,j2).ne.z(i3,j3))c=(cc-z(i2,j2))/(z(i3,j3)-z(i2,j2))               !/*cf        122*/
      x2=c*(x(i3)-x(i2))+x(i2)                                                  !/*cf        123*/
      y2=c*(y(j3)-y(j2))+y(j2)                                                  !/*cf        124*/
!                                                                               !/*cf        125*/
!        add the vector to the line to be plotted.                              !/*cf        126*/
  190 if(abs(x2-x1)+abs(y2-y1).eq.0.)go to 200                                  !/*cf        127*/
      ns=ns+1                                                                   !/*cf        128*/
      xw(ns)=x2                                                                 !/*cf        129*/
      yw(ns)=y2                                                                 !/*cf        130*/
      x1=x2                                                                     !/*cf        131*/
      y1=y2                                                                     !/*cf        132*/
  200 in=mod(in+id-1,4)+1                                                       !/*cf        133*/
      go to(210,230,250,270)in                                                  !/*cf        134*/
!                                                                               !/*cf        135*/
!        entering bottom side.                                                  !/*cf        136*/
  210 jc=jc+1                                                                   !/*cf        137*/
      if(jc.lt.ny)go to 220                                                     !/*cf        138*/
      mc(ic,ny)=abs(mc(ic,ny))                                                  !/*cf        139*/
      go to 290                                                                 !/*cf        140*/
  220 if(mc(ic,jc).gt.0)go to 290                                               !/*cf        141*/
      j1=jc                                                                     !/*cf        142*/
      i1=ic                                                                     !/*cf        143*/
      i2=i1+1                                                                   !/*cf        144*/
      j2=j1                                                                     !/*cf        145*/
      i3=i2                                                                     !/*cf        146*/
      j3=j1+1                                                                   !/*cf        147*/
      j4=j3                                                                     !/*cf        148*/
      i4=i1                                                                     !/*cf        149*/
      mc(ic,jc)=abs(mc(ic,jc))                                                  !/*cf        150*/
      ib=ib+1                                                                   !/*cf        151*/
      go to 130                                                                 !/*cf        152*/
!                                                                               !/*cf        153*/
!        entering right side.                                                   !/*cf        154*/
  230 ic=ic-1                                                                   !/*cf        155*/
      if(ic.gt.0)go to 240                                                      !/*cf        156*/
      mc(1,jc)=sign(2,mc(1,jc))                                                 !/*cf        157*/
      go to 290                                                                 !/*cf        158*/
  240 if(abs(mc(ic+1,jc)).eq.2)go to 290                                        !/*cf        159*/
      i1=ic+1                                                                   !/*cf        160*/
      j1=jc                                                                     !/*cf        161*/
      i2=i1                                                                     !/*cf        162*/
      j2=j1+1                                                                   !/*cf        163*/
      i3=ic                                                                     !/*cf        164*/
      j3=j2                                                                     !/*cf        165*/
      i4=i3                                                                     !/*cf        166*/
      j4=jc                                                                     !/*cf        167*/
      mc(i1,j1)=sign(2,mc(i1,j1))                                               !/*cf        168*/
      ib=ib+1                                                                   !/*cf        169*/
      go to 130                                                                 !/*cf        170*/
!                                                                               !/*cf        171*/
!        entering top side.                                                     !/*cf        172*/
  250 jc=jc-1                                                                   !/*cf        173*/
      if(jc.gt.0)go to 260                                                      !/*cf        174*/
      mc(ic,1)=abs(mc(ic,1))                                                    !/*cf        175*/
      go to 290                                                                 !/*cf        176*/
  260 if(mc(ic,jc+1).gt.0)go to 290                                             !/*cf        177*/
      j1=jc+1                                                                   !/*cf        178*/
      i1=ic+1                                                                   !/*cf        179*/
      i2=ic                                                                     !/*cf        180*/
      j2=j1                                                                     !/*cf        181*/
      i3=i2                                                                     !/*cf        182*/
      j3=jc                                                                     !/*cf        183*/
      i4=i1                                                                     !/*cf        184*/
      j4=j3                                                                     !/*cf        185*/
      mc(i2,j2)=abs(mc(i2,j2))                                                  !/*cf        186*/
      ib=ib+1                                                                   !/*cf        187*/
      go to 130                                                                 !/*cf        188*/
!                                                                               !/*cf        189*/
!        entering left side.                                                    !/*cf        190*/
  270 ic=ic+1                                                                   !/*cf        191*/
      if(ic.lt.nx)go to 280                                                     !/*cf        192*/
      mc(nx,jc)=sign(2,mc(nx,jc))                                               !/*cf        193*/
      go to 290                                                                 !/*cf        194*/
  280 if(abs(mc(ic,jc)).eq.2)go to 290                                          !/*cf        195*/
      i1=ic                                                                     !/*cf        196*/
      j1=jc+1                                                                   !/*cf        197*/
      i2=i1                                                                     !/*cf        198*/
      j2=jc                                                                     !/*cf        199*/
      i3=ic+1                                                                   !/*cf        200*/
      j3=j2                                                                     !/*cf        201*/
      i4=i3                                                                     !/*cf        202*/
      j4=j1                                                                     !/*cf        203*/
      mc(i2,j2)=sign(2,mc(i2,j2))                                               !/*cf        204*/
      ib=ib+1                                                                   !/*cf        205*/
      go to 130                                                                 !/*cf        206*/
!                                                                               !/*cf        207*/
!        draw the contour line.                                                 !/*cf        208*/
  290 if(ns.le.1)go to 300                                                      !/*cf        209*/
      call gsln(1+mod(n,2))                                                     !/*cf        210*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+1+mod(n,2)))                     !/*cf4a        2*/
      if(mod(n,2).ne.0)                                                         !/*cf        211*/
     & call gtx(real(xw(ns/2)),real(yw(ns/2)),char(ichar('a')+n-1))             !/*cf        212*/
      call plin(ns,xw,yw)                                                       !/*cf        213*/
      ns=0                                                                      !/*cf        214*/
  300 go to(30,50,70,90,120)ia                                                  !/*cf        215*/
  310 continue                                                                  !/*cf        216*/
      call gsln(1)                                                              !/*cf        217*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+1))                              !/*cf4a        3*/
#endif                                                                          /*!cfx        12*/
      return                                                                    !/*cf        218*/
      end                                                                       !/*cf        219*/

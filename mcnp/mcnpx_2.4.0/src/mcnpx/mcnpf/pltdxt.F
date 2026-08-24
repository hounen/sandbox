!#ident pltdxt.F from deck qx                                                   !/*qx          1*/
      subroutine pltdxt(nc,lp)                                                  !/*qx          3*/
#ifdef PLOT                                                                     /*!qxx         1*/
!        put points that define the arcs of the dxtran spheres into zst.        !/*qx          4*/
!        increment nc by the number of arcs generated.                          !/*qx          5*/
!        increment lp by the number of entries added to zst.                    !/*qx          6*/
      use allocate_memory                                                       !/*qxx         2*/
      use global_data                                                           !/*qxx         3*/
      use plot_data                                                             !/*qxx         4*/
!                                                                               !/*qxx         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*qxx         6*/
      implicit integer(kindi) (i-n)                                             !/*qxx         7*/
!                                                                               !/*qxx         8*/
      dimension cn(3),cr(8),g(2),p(2),lg(8)                                     !/*qx          9*/
!                                                                               !/*qx         10*/
      kfm(lkfm+mxj+1)=3                                                         !/*qx         11*/
      do 190 ip=1,mipt                                                          !/*qx         12*/
      do 180 ix=1,ndx(ip)                                                       !/*qx         13*/
!                                                                               !/*qx         14*/
!        set up the coefficients of the dxtran ellipse.                         !/*qx         15*/
      do 10 i=1,3                                                               !/*qx         16*/
   10 cn(i)=0.                                                                  !/*qx         17*/
      do 20 j=1,3                                                               !/*qx         18*/
      a=dxx(ip,j,ix)-origin(j)                                                  !/*qx         19*/
      do 20 i=1,3                                                               !/*qx         20*/
   20 cn(i)=cn(i)+a*basis(3*(i-1)+j)                                            !/*qx         21*/
      if(cn(3)**2.ge.dxx(ip,5,ix))go to 180                                     !/*qx         22*/
      r=sqrt(dxx(ip,5,ix)-cn(3)**2)                                             !/*qx         23*/
      if(cn(1)-r.ge.extent(1).or.cn(1)+r.le.-extent(1).or.                      !/*qx         24*/
     & cn(2)-r.ge.extent(2).or.cn(2)+r.le.-extent(2))go to 180                  !/*qx         25*/
      coe(lcoe+1,1,mxj+1)=cn(1)/extent(1)                                       !/*qx         26*/
      coe(lcoe+2,1,mxj+1)=0.                                                    !/*qx         27*/
      coe(lcoe+3,1,mxj+1)=r/extent(1)                                           !/*qx         28*/
      coe(lcoe+4,1,mxj+1)=cn(2)/extent(2)                                       !/*qx         29*/
      coe(lcoe+5,1,mxj+1)=r/extent(2)                                           !/*qx         30*/
      coe(lcoe+6,1,mxj+1)=0.                                                    !/*qx         31*/
!                                                                               !/*qx         32*/
!        set up 3, 0, 3-2*pie, and where it crosses the window boundary.        !/*qx         33*/
      np=0                                                                      !/*qx         34*/
      call ptost(three,1,mxj+1,s,t)                                             !/*qxx         9*/
      if(abs(s).gt.1..or.abs(t).gt.1.)go to 30                                  !/*qx         36*/
      np=2                                                                      !/*qx         37*/
      cr(1)=3.-2.*pie                                                           !/*qx         38*/
      cr(2)=3.                                                                  !/*qx         39*/
      call ptost(zero,1,mxj+1,s,t)                                              !/*qx         40*/
      if(abs(s).gt.1..or.abs(t).gt.1.)go to 30                                  !/*qx         41*/
      np=3                                                                      !/*qx         42*/
      cr(3)=0.                                                                  !/*qx         43*/
   30 do 50 i=1,2                                                               !/*qx         44*/
      do 50 m=-1,1,2                                                            !/*qx         45*/
      call sttop(i,zero+m,1,mxj+1,n,g(1),g(2),p(1),p(2))                        !/*qx         46*/
      do 40 k=1,n                                                               !/*qx         47*/
      if(abs(g(k)).gt.1.)go to 40                                               !/*qx         48*/
      np=np+1                                                                   !/*qx         49*/
      cr(np)=p(k)                                                               !/*qx         50*/
   40 continue                                                                  !/*qx         51*/
   50 continue                                                                  !/*qx         52*/
!                                                                               !/*qx         53*/
!        sort the intersections.                                                !/*qx         54*/
      do 70 i=1,np-1                                                            !/*qx         55*/
      do 60 j=i+1,np                                                            !/*qx         56*/
      if(cr(i).lt.cr(j))go to 60                                                !/*qx         57*/
      a=cr(i)                                                                   !/*qx         58*/
      cr(i)=cr(j)                                                               !/*qx         59*/
      cr(j)=a                                                                   !/*qx         60*/
   60 continue                                                                  !/*qx         61*/
   70 continue                                                                  !/*qx         62*/
!                                                                               !/*qx         63*/
!        delete any duplicate intersections.                                    !/*qx         64*/
      call ptost(cr(1),1,mxj+1,s1,t1)                                           !/*qx         65*/
      i=1                                                                       !/*qx         66*/
   80 call ptost(cr(i+1),1,mxj+1,s2,t2)                                         !/*qx         67*/
      if((s1-s2)**2+(t1-t2)**2.lt.res**2)go to 90                               !/*qx         68*/
      s1=s2                                                                     !/*qx         69*/
      t1=t2                                                                     !/*qx         70*/
      i=i+1                                                                     !/*qx         71*/
      go to 110                                                                 !/*qx         72*/
   90 do 100 j=i+1,np-1                                                         !/*qx         73*/
  100 cr(j)=cr(j+1)                                                             !/*qx         74*/
      np=np-1                                                                   !/*qx         75*/
  110 if(i.lt.np)go to 80                                                       !/*qx         76*/
!                                                                               !/*qx         77*/
!        determine whether each segment should be plotted.                      !/*qx         78*/
      lg(1)=0                                                                   !/*qx         79*/
      do 120 js=1,np-1                                                          !/*qx         80*/
      call ptost(.51729*cr(js)+.48271*cr(js+1),1,mxj+1,s,t)                     !/*qx         81*/
      lg(js+1)=2                                                                !/*qx         82*/
  120 if(abs(s).gt.1..or.abs(t).gt.1.)lg(js+1)=0                                !/*qx         83*/
!                                                                               !/*qx         84*/
!        put the points to be plotted into zst.                                 !/*qx         85*/
      do 170 js=1,np-1                                                          !/*qx         86*/
      if(lg(js+1).eq.0)go to 170                                                !/*qx         87*/
      if(lg(js+1).eq.lg(js))go to 130                                           !/*qx         88*/
      nc=nc+1                                                                   !/*qx         89*/
      la=lp                                                                     !/*qx         90*/
      if(size(zst).lt.(la+8)) then                                              !/*qxx        10*/
        zst => chg_mem (zst,la+5008,is)                                         !/*qxx        11*/
        if(is.ne.0)call expire(0,'pltdxt','chg_mem error')                      !/*qxx        12*/
      endif                                                                     !/*qxx        13*/
      zst(la)=0.                                                                !/*qx         92*/
      zst(la+1)=1                                                               !/*qx         93*/
      zst(la+2)=2                                                               !/*qx         94*/
      call ptost(cr(js),1,mxj+1,s1,t1)                                          !/*qx         95*/
      zst(lp+3)=s1*extent(1)                                                    !/*qx         96*/
      zst(lp+4)=t1*extent(2)                                                    !/*qx         97*/
      lp=lp+5                                                                   !/*qx         98*/
!                                                                               !/*qx         99*/
!        calculate enough points to define the segment.                         !/*qx        100*/
  130 p1=cr(js)                                                                 !/*qx        101*/
      call ptost(cr(js+1),1,mxj+1,se,te)                                        !/*qx        102*/
  140 p2=cr(js+1)                                                               !/*qx        103*/
      s2=se                                                                     !/*qx        104*/
      t2=te                                                                     !/*qx        105*/
  150 call ptost(.5*(p1+p2),1,mxj+1,sm,tm)                                      !/*qx        106*/
      if(((sm-s1)*(t2-t1)-(tm-t1)*(s2-s1))**2.le.res**2*                        !/*qx        107*/
     & ((t2-t1)**2+(s2-s1)**2))go to 160                                        !/*qx        108*/
      p2=.5*(p1+p2)                                                             !/*qx        109*/
      s2=sm                                                                     !/*qx        110*/
      t2=tm                                                                     !/*qx        111*/
      go to 150                                                                 !/*qx        112*/
  160 if(size(zst).lt.(lp+5)) then                                              !/*qxx        14*/
        zst => chg_mem (zst,lp+5005,is)                                         !/*qxx        15*/
        if(is.ne.0)call expire(0,'pltdxt','chg_mem error')                      !/*qxx        16*/
      endif                                                                     !/*qxx        17*/
      zst(lp)=s2*extent(1)                                                      !/*qx        114*/
      zst(lp+1)=t2*extent(2)                                                    !/*qx        115*/
      lp=lp+2                                                                   !/*qx        116*/
      zst(la+1)=zst(la+1)+1                                                     !/*qx        117*/
      p1=p2                                                                     !/*qx        118*/
      s1=s2                                                                     !/*qx        119*/
      t1=t2                                                                     !/*qx        120*/
      if(p2.ne.cr(js+1))go to 140                                               !/*qx        121*/
  170 continue                                                                  !/*qx        122*/
  180 continue                                                                  !/*qx        123*/
  190 continue                                                                  !/*qx        124*/
#endif                                                                          /*!qxx        18*/
      return                                                                    !/*qx        125*/
      end                                                                       !/*qx        126*/

!#ident splins.F from deck sh                                                   !/*sh          1*/
      subroutine splins                                                         !/*sh          3*/
#ifdef MCPLOT                                                                   /*!shx         1*/
!        draw a stineman spline line.                                           !/*sh          4*/
      use global_data                                                           !/*shx         2*/
      use mcplot_data                                                           !/*shx         3*/
!                                                                               !/*shx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*shx         5*/
      implicit integer(kindi) (i-n)                                             !/*shx         6*/
!                                                                               !/*shx         7*/
      dimension t(2)                                                            !/*sh          7*/
      a(i)=ab1(lab1+i)                                                          !/*sh          8*/
      o(i)=ord(lord+i)                                                          !/*sh          9*/
!                                                                               !/*sh         10*/
      m=max(1,199/(npt(1)-1))                                                   !/*sh         11*/
      do 50 i=1,npt(1)-1                                                        !/*sh         12*/
!                                                                               !/*sh         13*/
!        define segment slopes.                                                 !/*sh         14*/
      do 10 j=i,i+1                                                             !/*sh         15*/
      if(j.eq.1.or.j.eq.npt(1))go to 10                                         !/*sh         16*/
      f1=(a(j+1)-a(j))**2+(o(j+1)-o(j))**2                                      !/*sh         17*/
      f2=(a(j)-a(j-1))**2+(o(j)-o(j-1))**2                                      !/*sh         18*/
      fn=(o(j)-o(j-1))*f1+(o(j+1)-o(j))*f2                                      !/*sh         19*/
      fd=(a(j)-a(j-1))*f1+(a(j+1)-a(j))*f2                                      !/*sh         20*/
      t(j-i+1)=1.                                                               !/*sh         21*/
      if(fn.ne.0.)t(j-i+1)=huge*1e-10                                           !/*sh         22*/
      if(fd.ne.0.)t(j-i+1)=fn/fd                                                !/*sh         23*/
   10 continue                                                                  !/*sh         24*/
      if(i.ne.1)go to 20                                                        !/*sh         25*/
      s=(o(2)-o(1))/(a(2)-a(1))                                                 !/*sh         26*/
      t(1)=0.                                                                   !/*sh         27*/
      if(s*t(2).ne.0)t(1)=s+(abs(s)*(s-t(2)))/(abs(s)+abs(s-t(2)))              !/*sh         28*/
      if(s.gt.0..and.s.gt.t(2).or.s.lt.0..and.s.lt.t(2))t(1)=2.*s-t(2)          !/*sh         29*/
   20 if(i.ne.npt(1)-1)go to 30                                                 !/*sh         30*/
      s=(o(npt(1))-o(npt(1)-1))/(a(npt(1))-a(npt(1)-1))                         !/*sh         31*/
      t(2)=0.                                                                   !/*sh         32*/
      if(s*t(1).ne.0.)t(2)=s+(abs(s)*(s-t(1)))/(abs(s)+abs(s-t(1)))             !/*sh         33*/
      if(s.gt.0..and.s.gt.t(1).or.s.lt.0..and.s.lt.t(1))t(2)=2.*s-t(1)          !/*sh         34*/
!                                                                               !/*sh         35*/
!        tabulate and plot the segment.                                         !/*sh         36*/
   30 dl=(a(i+1)-a(i))/m                                                        !/*sh         37*/
      xcc(lxcc+1)=a(i)                                                          !/*sh         38*/
      ycc(lycc+1)=o(i)                                                          !/*sh         39*/
      do 40 j=2,m+1                                                             !/*sh         40*/
      xcc(lxcc+j)=xcc(lxcc+j-1)+dl                                              !/*sh         41*/
      ycc(lycc+j)=o(i)+(xcc(lxcc+j)-a(i))*(o(i+1)-o(i))/(a(i+1)-a(i))           !/*sh         42*/
      dy=o(i)+t(1)*(xcc(lxcc+j)-a(i))-ycc(lycc+j)                               !/*sh         43*/
      ey=o(i+1)+t(2)*(xcc(lxcc+j)-a(i+1))-ycc(lycc+j)                           !/*sh         44*/
      p=dy*ey                                                                   !/*sh         45*/
      if(p.eq.0.)go to 40                                                       !/*sh         46*/
      if(p.gt.0.)ycc(lycc+j)=ycc(lycc+j)+p/(dy+ey)                              !/*sh         47*/
      if(p.lt.0.)ycc(lycc+j)=ycc(lycc+j)+p*                                     !/*sh         48*/
     & (xcc(lxcc+j)-a(i)+xcc(lxcc+j)-a(i+1))/((dy-ey)*(a(i+1)-a(i)))            !/*sh         49*/
   40 continue                                                                  !/*sh         50*/
   50 call plin(m+1,xcc(lxcc+1),ycc(lycc+1))                                    !/*sh         51*/
#endif                                                                          /*!shx         8*/
      return                                                                    !/*sh         52*/
      end                                                                       !/*sh         53*/

!#ident plin.F from deck pz                                                     !/*pz          1*/
      subroutine plin(m,xr,yr)                                                  !/*pz          3*/
#ifdef MCPLOT                                                                   /*!pzx         1*/
!        draw a piecewise-linear line, with clipping.                           !/*pz          4*/
!        m = number of points in xr,yr.                                         !/*pz          5*/
      use global_data                                                           !/*pzx         2*/
      use mcplot_data                                                           !/*pzx         3*/
!                                                                               !/*pzx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pzx         5*/
      implicit integer(kindi) (i-n)                                             !/*pzx         6*/
!                                                                               !/*pzx         7*/
      dimension p(4),xr(m),yr(m)                                                !/*pz          8*/
!                                                                               !/*pz          9*/
      n=0                                                                       !/*pz         10*/
      do 40 ia=1,m-1                                                            !/*pz         11*/
      p(1)=xr(ia)                                                               !/*pz         12*/
      p(2)=xr(ia+1)                                                             !/*pz         13*/
      p(3)=yr(ia)                                                               !/*pz         14*/
      p(4)=yr(ia+1)                                                             !/*pz         15*/
      k=0                                                                       !/*pz         16*/
      l=1                                                                       !/*pz         17*/
      do 20 i=1,2                                                               !/*pz         18*/
      ka=i                                                                      !/*pz         19*/
      do 10 j=1,4                                                               !/*pz         20*/
      if(j.eq.3)ka=ka+2                                                         !/*pz         21*/
      if(l*(p(ka)-plim(j)).ge.0.)go to 10                                       !/*pz         22*/
      kb=ka+3-2*i                                                               !/*pz         23*/
      if(l*(p(kb)-plim(j)).le.0.)go to 30                                       !/*pz         24*/
      p(5-kb)=p(5-kb)+(p(5-ka)-p(5-kb))*(plim(j)-p(ka))/(p(kb)-p(ka))           !/*pz         25*/
      p(ka)=plim(j)                                                             !/*pz         26*/
      if(i.eq.2)k=1                                                             !/*pz         27*/
   10 l=-l                                                                      !/*pz         28*/
   20 continue                                                                  !/*pz         29*/
      n=n+1                                                                     !/*pz         30*/
      xrr(lxrr+n)=p(1)                                                          !/*pz         31*/
      xrr(lxrr+n+1)=p(2)                                                        !/*pz         32*/
      yrr(lyrr+n)=p(3)                                                          !/*pz         33*/
      yrr(lyrr+n+1)=p(4)                                                        !/*pz         34*/
      if(k.eq.0.and.ia.lt.m-1)go to 40                                          !/*pz         35*/
   30 if(n.ne.0)call gpl(n+1,xrr(lxrr+1),yrr(lyrr+1))                           !/*pz         36*/
      n=0                                                                       !/*pz         37*/
   40 continue                                                                  !/*pz         38*/
#endif                                                                          /*!pzx         8*/
      return                                                                    !/*pz         39*/
      end                                                                       !/*pz         40*/

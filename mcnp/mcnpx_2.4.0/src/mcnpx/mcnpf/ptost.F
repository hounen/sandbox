!#ident ptost.F from deck pj                                                    !/*pj          1*/
      subroutine ptost(p,iq,js,s,t)                                             !/*pj          3*/
#ifdef PLOT                                                                     /*!pjx         1*/
!        calculate the point s(p),t(p) from the parametric                      !/*pj          4*/
!        representation of curve iq of surface js.                              !/*pj          5*/
      use global_data                                                           !/*pjx         2*/
      use plot_data                                                             !/*pjx         3*/
!                                                                               !/*pjx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pjx         5*/
      implicit integer(kindi) (i-n)                                             !/*pjx         6*/
!                                                                               !/*pjx         7*/
      c(i)=coe(lcoe+i,iq,js)                                                    !/*pj          8*/
!                                                                               !/*pj          9*/
      go to(10,20,30,40)kfm(lkfm+js)                                            !/*pj         10*/
!                                                                               !/*pj         11*/
! >>>>>  straight line                                                          !/*pj         12*/
   10 s=c(1)+c(2)*p                                                             !/*pj         13*/
      t=c(4)+c(5)*p                                                             !/*pj         14*/
      return                                                                    !/*pj         15*/
!                                                                               !/*pj         16*/
! >>>>>  parabola                                                               !/*pj         17*/
   20 s=c(1)+c(2)*p+c(3)*p**2                                                   !/*pj         18*/
      t=c(4)+c(5)*p+c(6)*p**2                                                   !/*pj         19*/
      return                                                                    !/*pj         20*/
!                                                                               !/*pj         21*/
! >>>>>  ellipse                                                                !/*pj4c2       1*/
   30 r=p                                                                       !/*pj4b        1*/
!ccc     hp underflow bug correction.                                           !/*pj4b        2*/
      if(abs(p).lt.1.e-12)r=0.                                                  !/*pj4b        3*/
      a=sin(r)                                                                  !/*pj4b        4*/
      b=cos(r)                                                                  !/*pj4b        5*/
      s=c(1)+c(2)*a+c(3)*b                                                      !/*pj         25*/
      t=c(4)+c(5)*a+c(6)*b                                                      !/*pj         26*/
      return                                                                    !/*pj         27*/
!                                                                               !/*pj         28*/
! >>>>>  hyperbola                                                              !/*pj         29*/
   40 r=p                                                                       !/*pj4b        6*/
!ccc     hp underflow bug correction.                                           !/*pj4b        7*/
      if(abs(p).lt.1.e-12)r=0.                                                  !/*pj4b        8*/
      if(abs(p).gt.80.)r=sign(80.+zero,p)                                       !/*pj4b        9*/
      a=sinh(r)                                                                 !/*pj4b       10*/
      b=cosh(r)                                                                 !/*pj4b       11*/
      s=c(1)+c(2)*a+c(3)*b                                                      !/*pj         32*/
      t=c(4)+c(5)*a+c(6)*b                                                      !/*pj         33*/
#endif                                                                          /*!pjx         8*/
      return                                                                    !/*pj         34*/
      end                                                                       !/*pj         35*/

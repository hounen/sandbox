!#ident sttop.F from deck sg                                                    !/*sg          1*/
      subroutine sttop(mm,ar,iq,js,n,g1,g2,p1,p2)                               !/*sg          3*/
#ifdef PLOT                                                                     /*!sgx         1*/
!        calculate the values p1 and p2 of the curve parameter and the          !/*sg          4*/
!        values g1 and g2 of the other coordinate for the n points where        !/*sg          5*/
!        a line crosses curve iq of surface js.                                 !/*sg          6*/
!        if mm=1, the line is s=ar.  if mm=2, the line is t=ar.                 !/*sg          7*/
      use global_data                                                           !/*sgx         2*/
      use plot_data                                                             !/*sgx         3*/
!                                                                               !/*sgx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*sgx         5*/
      implicit integer(kindi) (i-n)                                             !/*sgx         6*/
!                                                                               !/*sgx         7*/
      dimension q(3)                                                            !/*sg         10*/
      c(i)=coe(lcoe+i,iq,js)                                                    !/*sg         11*/
!                                                                               !/*sg         12*/
      n=0                                                                       !/*sg         13*/
      g1=0.                                                                     !/*sg         14*/
      g2=0.                                                                     !/*sg         15*/
      p1=0.                                                                     !/*sg         16*/
      p2=0.                                                                     !/*sg         17*/
      k=3*mm-3                                                                  !/*sg         18*/
      go to(10,20,30,40)kfm(lkfm+js)                                            !/*sg         19*/
!                                                                               !/*sg         20*/
! >>>>>  straight line                                                          !/*sg         21*/
   10 if(c(2+k).eq.0.)return                                                    !/*sg         22*/
      n=1                                                                       !/*sg         23*/
      p1=(ar-c(1+k))/c(2+k)                                                     !/*sg         24*/
      g1=c(4-k)+c(5-k)*p1                                                       !/*sg         25*/
      return                                                                    !/*sg         26*/
!                                                                               !/*sg         27*/
! >>>>>  parabola                                                               !/*sg         28*/
   20 q(1)=c(3+k)                                                               !/*sg         29*/
      q(2)=c(2+k)                                                               !/*sg         30*/
      q(3)=c(1+k)-ar                                                            !/*sg         31*/
      call quad(q,n,p1,p2)                                                      !/*sg         32*/
      g1=c(4-k)+c(5-k)*p1+c(6-k)*p1**2                                          !/*sg         33*/
      g2=c(4-k)+c(5-k)*p2+c(6-k)*p2**2                                          !/*sg         34*/
      return                                                                    !/*sg         35*/
!                                                                               !/*sg         36*/
! >>>>>  ellipse                                                                !/*sg         37*/
   30 a=(ar-c(1+k))/sqrt(c(2+k)**2+c(3+k)**2)                                   !/*sg         38*/
      if(abs(a).gt.1.00001)return                                               !/*sgxl        1*/
      n=2                                                                       !/*sg         40*/
      b=asin(min(one,max(-one,a)))                                              !/*sg         41*/
      a=-atan2(c(3+k),c(2+k))                                                   !/*sg         42*/
      p1=mod(a+b-3.+4.*pie,2.*pie)+3.-2.*pie                                    !/*sg         43*/
      p2=mod(a-b-3.+3.*pie,2.*pie)+3.-2.*pie                                    !/*sg         44*/
      g1=c(4-k)+c(5-k)*sin(p1)+c(6-k)*cos(p1)                                   !/*sg         45*/
      g2=c(4-k)+c(5-k)*sin(p2)+c(6-k)*cos(p2)                                   !/*sg         46*/
      return                                                                    !/*sg         47*/
!                                                                               !/*sg         48*/
! >>>>>  hyperbola                                                              !/*sg         49*/
   40 a=(ar-c(1+k))**2+c(2+k)**2-c(3+k)**2                                      !/*sg         50*/
      if(a.lt.0.)return                                                         !/*sg         51*/
      if(c(3+k)+c(2+k).ne.0.)go to 50                                           !/*sg         52*/
      if(a.eq.0.)return                                                         !/*sg         53*/
      a=(c(3+k)-c(2+k))/(4.*a)                                                  !/*sg         54*/
      b=a                                                                       !/*sg         55*/
      go to 60                                                                  !/*sg         56*/
   50 a=abs(sqrt(a)/(c(3+k)+c(2+k)))                                            !/*sg         57*/
      b=(ar-c(1+k))/(c(3+k)+c(2+k))                                             !/*sg         58*/
   60 if(b+a.le.0.)return                                                       !/*sg         59*/
      n=1                                                                       !/*sg         60*/
      p1=log(b+a)                                                               !/*sg         61*/
      g1=c(4-k)+c(5-k)*sinh(p1)+c(6-k)*cosh(p1)                                 !/*sg         62*/
      if(b-a.le.0.)return                                                       !/*sg         63*/
      n=2                                                                       !/*sg         64*/
      p2=log(b-a)                                                               !/*sg         65*/
      g2=c(4-k)+c(5-k)*sinh(p2)+c(6-k)*cosh(p2)                                 !/*sg         66*/
#endif                                                                          /*!sgx         8*/
      return                                                                    !/*sg         67*/
      end                                                                       !/*sg         68*/

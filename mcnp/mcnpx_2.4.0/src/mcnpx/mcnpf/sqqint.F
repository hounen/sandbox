!#ident sqqint.F from deck sq                                                   !/*sq          1*/
      function sqqint(t1,t2,t3,x1,x2)                                           !/*sq          2*/
!        integrate the square root of a quadratic.                              !/*sq          3*/
!        sqqint=integral(sqrt(t1*x**2+t2*x+t3)) between x1 and x2.              !/*sq          4*/
      use global_data                                                           !/*sqx         1*/
      use imcn_data                                                             !/*sqx         2*/
!                                                                               !/*sqx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*sqx         4*/
      implicit integer(kindi) (i-n)                                             !/*sqx         5*/
!                                                                               !/*sqx         6*/
!                                                                               !/*sq          7*/
      z2=1.e-8*(abs(t1)+abs(t2)+abs(t3))                                        !/*sq          8*/
      if(abs(t1).gt.z2)go to 20                                                 !/*sq          9*/
      if(abs(t2).le.z2)go to 10                                                 !/*sq         10*/
      t4=max(zero,t3+t2*x1)                                                     !/*sq         11*/
      t5=max(zero,t3+t2*x2)                                                     !/*sq         12*/
      sqqint=2.*(t4**1.5-t5**1.5)/(3.*t2)                                       !/*sq         13*/
      return                                                                    !/*sq         14*/
   10 sqqint=sqrt(max(t3,zero))*(x1-x2)                                         !/*sq4b        1*/
      return                                                                    !/*sq         17*/
   20 t7=t2*t2-4.*t1*t3                                                         !/*sq         18*/
      if(abs(t7).gt.z2)go to 30                                                 !/*sq         19*/
      sqqint=.5*sqrt(abs(t1))*(x1*(x1+t2/t1)-x2*(x2+t2/t1))                     !/*sq         20*/
      return                                                                    !/*sq         21*/
   30 t4=max(zero,t3+x1*(t2+x1*t1))                                             !/*sq         22*/
      t5=max(zero,t3+x2*(t2+x2*t1))                                             !/*sq         23*/
      sqqint=.5*((x1+.5*t2/t1)*sqrt(t4)-(x2+.5*t2/t1)*sqrt(t5))                 !/*sq         24*/
      t8=sqrt(abs(t1))                                                          !/*sq         25*/
      t9=-t7/(8.*t8*t1)                                                         !/*sq         26*/
      if(t1.lt.0.)go to 40                                                      !/*sq         27*/
      sqqint=sqqint+t9*log(max(z2,sqrt(t4)+x1*t8+.5*t2/t8)/                     !/*sq         28*/
     & max(z2,sqrt(t5)+x2*t8+.5*t2/t8))                                         !/*sq         29*/
      return                                                                    !/*sq         30*/
   40 t6=-sqrt(abs(t7))                                                         !/*sq         31*/
      sqqint=sqqint+t9*(asin(max(-one,min(one,(2.*t1*x1+t2)/t6)))-              !/*sq         32*/
     & asin(max(-one,min(one,(2.*t1*x2+t2)/t6))))                               !/*sq         33*/
      return                                                                    !/*sq         34*/
      end                                                                       !/*sq         35*/

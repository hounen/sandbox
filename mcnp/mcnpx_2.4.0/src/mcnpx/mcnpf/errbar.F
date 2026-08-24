!#ident errbar.F from deck ea                                                   !/*ea          1*/
      subroutine errbar                                                         !/*ea          3*/
#ifdef MCPLOT                                                                   /*!eax         1*/
!        add error bars to a 2d plot.                                           !/*ea          4*/
      use global_data                                                           !/*eax         2*/
      use mcplot_data                                                           !/*eax         3*/
!                                                                               !/*eax         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*eax         5*/
      implicit integer(kindi) (i-n)                                             !/*eax         6*/
!                                                                               !/*eax         7*/
      real x(2),y(2)                                                            !/*ea          7*/
!                                                                               !/*ea          8*/
      do 20 i=1,npt(1)                                                          !/*ea          9*/
      if(erb(lerb+i).eq.0.)go to 20                                             !/*ea         10*/
      xv=ab1(lab1+i)                                                            !/*ea         11*/
      if(xv.lt.plim(1).or.xv.gt.plim(2))go to 20                                !/*ea4b        1*/
      yv=ord(lord+i)                                                            !/*ea         14*/
      if(abs(lax-3).eq.1)yv=10.**yv                                             !/*ea         15*/
      ys=sign(one,yv)                                                           !/*ea4c        1*/
      y(1)=max(yv*(1.-ys*erb(lerb+i)),xyzmn(2))                                 !/*ea4c        2*/
      y(2)=min(yv*(1.+ys*erb(lerb+i)),xyzmx(2))                                 !/*ea4c        3*/
      if(y(1).ge.y(2))go to 20                                                  !/*ea         18*/
      if(abs(lax-3).ne.1)go to 10                                               !/*ea         19*/
      y(1)=log10(y(1))                                                          !/*ea         20*/
      y(2)=log10(y(2))                                                          !/*ea         21*/
   10 x(1)=xv                                                                   !/*ea         22*/
      x(2)=xv                                                                   !/*ea         23*/
      call gpl(2,x,y)                                                           !/*ea         24*/
      yv=y(2)                                                                   !/*ea         25*/
      y(2)=y(1)                                                                 !/*ea         26*/
      d=.002*(plim(2)-plim(1))                                                  !/*ea         27*/
      x(1)=xv-d                                                                 !/*ea         28*/
      x(2)=xv+d                                                                 !/*ea         29*/
      if(y(1).gt.plim(3))call gpl(2,x,y)                                        !/*ea         30*/
      y(1)=yv                                                                   !/*ea         31*/
      y(2)=yv                                                                   !/*ea         32*/
      if(yv.lt.plim(4))call gpl(2,x,y)                                          !/*ea         33*/
   20 continue                                                                  !/*ea         34*/
#endif                                                                          /*!eax         8*/
      return                                                                    !/*ea         35*/
      end                                                                       !/*ea         36*/

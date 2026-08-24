!#ident plotcn.F from deck pn                                                   !/*pn          1*/
      subroutine plotcn                                                         !/*pn          3*/
#ifdef MCPLOT                                                                   /*!pnx         1*/
!        draw a contour plot.                                                   !/*pn          4*/
      use global_data                                                           !/*pnx         2*/
      use mcplot_data                                                           !/*pnx         3*/
!                                                                               !/*pnx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pnx         5*/
      implicit integer(kindi) (i-n)                                             !/*pnx         6*/
!                                                                               !/*pnx         7*/
      character hp*80                                                           !/*pn          7*/
      real x,y,s,t,yv                                                           !/*pn          8*/
      x(s)=plim(1)+(s-1.)*(plim(2)-plim(1))/7.                                  !/*pn          9*/
      y(t)=plim(3)+(t-1.-.6*lput)*(plim(4)-plim(3))/6.                          !/*pn         10*/
!                                                                               !/*pn         11*/
!        specify the window and the character expansion factor.                 !/*pn         12*/
      call gswn(1,x(0.),x(11.),y(0.),y(8.25))                                   !/*pn         13*/
      wnvp(1)=x(11.)-x(0.)                                                      !/*pn         14*/
      wnvp(2)=y(8.25)-y(0.)                                                     !/*pn         15*/
      call gschxp((x(1.)-x(0.))/(y(1.)-y(0.)))                                  !/*pn         16*/
!                                                                               !/*pn         17*/
!        draw the axes.                                                         !/*pn         18*/
      if(mscal.eq.0)go to 10                                                    !/*pn         19*/
      tk=.014*(plim(4)-plim(3))                                                 !/*pn4b        1*/
      g=0.                                                                      !/*pn         21*/
      if(mscal.eq.2)g=plim(4)-plim(3)                                           !/*pn         22*/
      call gxaxis(plim(1),plim(2),plim(3),plim(3),-tk,g,itik(1),0,1,            !/*pn4b        2*/
     & titles(1))                                                               !/*pn         24*/
      call gxaxis(plim(1),plim(2),plim(4),plim(4),tk,zero,itik(1),0,0,          !/*pn4b        3*/
     & ' ')                                                                     !/*pn         26*/
      tk=.012*(plim(2)-plim(1))                                                 !/*pn4b        4*/
      if(mscal.eq.2)g=plim(2)-plim(1)                                           !/*pn         28*/
      call gxaxis(plim(1),plim(1),plim(3),plim(4),-tk,g,itik(2),0,1,            !/*pn4b        5*/
     & titles(2))                                                               !/*pn         30*/
      call gxaxis(plim(2),plim(2),plim(3),plim(4),tk,zero,itik(2),0,0,          !/*pn4b        6*/
     & ' ')                                                                     !/*pn         32*/
!                                                                               !/*pn         33*/
!        write the titles.                                                      !/*pn         34*/
   10 call gschh(y(.18)-y(0.))                                                  !/*pn         35*/
      call gtx(x(1.5),y(7.7-7.2*lput),titles(5))                                !/*pn         36*/
      call gtx(x(1.5),y(7.3-7.2*lput),titles(6))                                !/*pn         37*/
      if(ititle(7).ne.0)call gtx(real(xst),real(yst),titles(7))                 !/*pn         38*/
!                                                                               !/*pn         39*/
!        calculate the contour levels.                                          !/*pn         40*/
      do 20 nclev=1,mclevs                                                      !/*pn         41*/
      clev(nclev)=(nclev-1)*contur(3)+contur(1)                                 !/*pn         42*/
      if(ipct.ne.0)clev(nclev)=.01*clev(nclev)*(xyzmx(3)-xyzmn(3))              !/*pn         43*/
   20 if(nclev*contur(3)+contur(1).gt.contur(2)*1.001)go to 30                  !/*pn         44*/
!                                                                               !/*pn         45*/
!        extract, scale, and plot sub-blocks.                                   !/*pn         46*/
   30 ibl(ifree(2),2)=icut(2)                                                   !/*pn         47*/
      call gschh(y(.14)-y(0.))                                                  !/*pn         48*/
   40 jb=min(jsbm,npt(2)+1-ibl(ifree(2),2))                                     !/*pn         49*/
      ibl(ifree(2),1)=ibl(ifree(2),2)                                           !/*pn         50*/
      ibl(ifree(2),2)=ibl(ifree(2),2)+jb-1                                      !/*pn         51*/
      ibl(ifree(1),2)=icut(1)                                                   !/*pn         52*/
   50 ib=min(isbm,npt(1)+1-ibl(ifree(1),2))                                     !/*pn         53*/
      ibl(ifree(1),1)=ibl(ifree(1),2)                                           !/*pn         54*/
      ibl(ifree(1),2)=ibl(ifree(1),2)+ib-1                                      !/*pn         55*/
      call exord(2)                                                             !/*pn         56*/
      do 60 i=1,ib*jb                                                           !/*pn         57*/
   60 ord(lord+i)=ord(lord+i)*scalf(1,3)+scalf(2,3)                             !/*pn         58*/
      call contr(ord(lord+1),ab1(lab1+ibl(ifree(1),1)),                         !/*pn         59*/
     & ab2(lab2+ibl(ifree(2),1)),ib,jb,xcc(lxcc+1),ycc(lycc+1))                 !/*pnx         8*/
      if(ibl(ifree(1),2).lt.npt(1))go to 50                                     !/*pn         62*/
      if(ibl(ifree(2),2).lt.npt(2))go to 40                                     !/*pn         63*/
!                                                                               !/*pn         64*/
!        write the contour level values to the legend area.                     !/*pn         65*/
      if(legend.eq.0)go to 90                                                   !/*pn         66*/
      yv=6.8+.6*lput                                                            !/*pn         67*/
      if(ipct.eq.0)go to 70                                                     !/*pn         68*/
      call gtx(x(8.3),y(yv),'percent')                                          !/*pn         69*/
      yv=yv-.5                                                                  !/*pn         70*/
   70 do 80 i=1,nclev                                                           !/*pn         71*/
      call gtx(x(8.3),y(yv),char(ichar('a')+i-1))                               !/*pn         72*/
      if(ipct.ne.0)write(hp,'(f6.1)')(i-1)*contur(3)+contur(1)                  !/*pn         73*/
      if(ipct.eq.0)write(hp,'(1pe11.3)')clev(i)                                 !/*pn         74*/
      call gtx(x(8.5),y(yv),hp(1:11))                                           !/*pn         75*/
   80 yv=yv-.3                                                                  !/*pn         76*/
!                                                                               !/*pn         77*/
!        return home and update the display.                                    !/*pn         78*/
   90 if(abs(koplot-2).ne.1)call gxhome(x(.05)+zero,y(8.15)+zero)               !/*pn4b        7*/
#endif                                                                          /*!pnx         9*/
      return                                                                    !/*pn         80*/
      end                                                                       !/*pn         81*/

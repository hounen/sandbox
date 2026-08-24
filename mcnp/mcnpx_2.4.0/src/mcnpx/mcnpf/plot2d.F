!#ident plot2d.F from deck px                                                   !/*px          1*/
      subroutine plot2d                                                         !/*px          3*/
#ifdef MCPLOT                                                                   /*!pxx         1*/
!        draw a 2d plot.                                                        !/*px          4*/
      use global_data                                                           !/*pxx         2*/
      use mcplot_data                                                           !/*pxx         3*/
!                                                                               !/*pxx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pxx         5*/
      implicit integer(kindi) (i-n)                                             !/*pxx         6*/
!                                                                               !/*pxx         7*/
      character ha*10,hl*10                                                     !/*px4b        1*/
      real x,y,s,t,xy(2,2)                                                      !/*px          8*/
      x(s)=plim(1)+(s-1.)*(plim(2)-plim(1))/7.                                  !/*px          9*/
      y(t)=plim(3)+(t-1.-.6*lput)*(plim(4)-plim(3))/6.                          !/*px         10*/
!                                                                               !/*px         11*/
      if(koplot.ge.2)go to 30                                                   !/*px         12*/
!                                                                               !/*px         13*/
!        specify the window and the character expansion factor.                 !/*px         14*/
      call gswn(1,x(0.),x(11.),y(0.),y(8.25))                                   !/*px         15*/
      wnvp(1)=x(11.)-x(0.)                                                      !/*px         16*/
      wnvp(2)=y(8.25)-y(0.)                                                     !/*px         17*/
      call gschxp((x(1.)-x(0.))/(y(1.)-y(0.)))                                  !/*px         18*/
!                                                                               !/*px         19*/
!        draw the axes.                                                         !/*px         20*/
      if(mscal.eq.0)go to 10                                                    !/*px         21*/
      a=.014*(plim(4)-plim(3))                                                  !/*px         22*/
      i=max(0,lax-2)                                                            !/*px         23*/
      g=0.                                                                      !/*px         24*/
      if(mscal.eq.2)g=plim(4)-plim(3)                                           !/*px         25*/
      call gxaxis(plim(1),plim(2),plim(3),plim(3),-a,g,itik(1),i,1,             !/*px         26*/
     & titles(1))                                                               !/*px         27*/
      call gxaxis(plim(1),plim(2),plim(4),plim(4),a,zero,itik(1),i,0,           !/*px         28*/
     & ' ')                                                                     !/*px         29*/
      a=.012*(plim(2)-plim(1))                                                  !/*px         30*/
      i=mod(lax-1,2)                                                            !/*px         31*/
      if(mscal.eq.2)g=plim(2)-plim(1)                                           !/*px         32*/
      call gxaxis(plim(1),plim(1),plim(3),plim(4),-a,g,itik(2),i,1,             !/*px         33*/
     & titles(2))                                                               !/*px         34*/
      call gxaxis(plim(2),plim(2),plim(3),plim(4),a,zero,itik(2),i,0,           !/*px         35*/
     & ' ')                                                                     !/*px         36*/
!                                                                               !/*px         37*/
!        write the titles.                                                      !/*px         38*/
   10 call gschh(y(.18)-y(0.))                                                  !/*px         39*/
      call gtx(x(1.5),y(7.7-7.2*lput),titles(5))                                !/*px         40*/
      call gtx(x(1.5),y(7.3-7.2*lput),titles(6))                                !/*px         41*/
      if(ititle(7).eq.0)go to 20                                                !/*px         42*/
      s=xst                                                                     !/*px         43*/
      t=yst                                                                     !/*px         44*/
      if(lax.ge.3.and.s.le.0..or.abs(lax-3).eq.1.and.t.le.0.)go to 20           !/*px         45*/
      if(lax.ge.3)s=log10(s)                                                    !/*px         46*/
      if(abs(lax-3).eq.1)t=log10(t)                                             !/*px         47*/
      call gtx(s,t,titles(7))                                                   !/*px         48*/
!                                                                               !/*px         49*/
!        write plot information to the legend area.                             !/*px         50*/
   20 if(legend.ne.0)call dplinf                                                !/*px         51*/
      lnstyl=0                                                                  !/*px         52*/
!                                                                               !/*px         53*/
!        plot the data.                                                         !/*px         54*/
   30 lnstyl=mod(lnstyl,max(4,min(6,mcolor)))+1                                 !/*px4a        1*/
      call gsln(lnstyl)                                                         !/*px         56*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+lnstyl))                         !/*px4a        2*/
      call gslwsc(100.*real(cthick))                                            !/*px         57*/
      if(kurv.eq.1)call hgram                                                   !/*px         58*/
      if(kurv.eq.2)call plin(npt(1),ab1(lab1+1),ord(lord+1))                    !/*px         59*/
      if(kurv.eq.3.and.tensn.eq.0.)call splins                                  !/*px         60*/
#ifdef DISSPLA                                                                  /*!px         61*/
      if(kurv.eq.3.and.tensn.ne.0.)call ratspl                                  !/*px         62*/
#endif                                                                          /*!cpp       317*/
      if(kurv.eq.4)call barplt                                                  !/*px         63*/
      call gslwsc(1.)                                                           !/*px         64*/
      call gsln(1)                                                              !/*px         65*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+1))                              !/*px4a        3*/
!                                                                               !/*px         66*/
!        add error bars if required.                                            !/*px         67*/
      if(noerbr.eq.0.and.(itfc.le.1.or.itfc.ge.41.and.itfc.le.43.or.            !/*px4a        4*/
     & itfc.ge.46.and.itfc.le.47.or.itfc.eq.52.or.itfc.eq.54.or.                !/*px4c        1*/
     & itfc.eq.64))call errbar                                                  !/*px4c        2*/
!                                                                               !/*px         69*/
!        annotate curve legend.                                                 !/*px         70*/
      if(legend.eq.0)go to 50                                                   !/*px         71*/
      hl=runtpe                                                                 !/*px         72*/
      if(ktfile.eq.2)hl=mctal                                                   !/*px         73*/
      if(itfc.gt.30)write(hl,'(5hkcode,i3)')itfc-30                             !/*px4a        6*/
      if(ititle(4).ne.0)hl=titles(4)                                            !/*px         75*/
      xy(1,1)=x(8.5)                                                            !/*px         76*/
      if(legend.ne.2)go to 40                                                   !/*px         77*/
      xy(1,1)=xlg                                                               !/*px         78*/
      if(lax.ge.3.and.xlg.le.0.)go to 50                                        !/*px         79*/
      if(lax.ge.3)xy(1,1)=log10(xlg)                                            !/*px         80*/
   40 xy(2,1)=xy(1,1)+x(1.)-x(0.)                                               !/*px         81*/
      if(kxsplt.ne.0)xy(2,1)=xy(1,1)+x(.35)-x(0.)                               !/*px4b        2*/
      if(kxsplt.eq.0.or.koplot.gt.1)go to 42                                    !/*px4b        3*/
      call gtx(xy(2,1)+x(.05)-x(0.),y(real(yval)),'  mt        xs')             !/*px4b        4*/
      yval=yval-.3                                                              !/*px4b        5*/
   42 xy(1,2)=y(real(yval))                                                     !/*px4b        6*/
      xy(1,2)=y(real(yval))                                                     !/*px         82*/
      xy(2,2)=xy(1,2)                                                           !/*px         83*/
      call gsln(lnstyl)                                                         !/*px         84*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+lnstyl))                         !/*px4a        7*/
      call gslwsc(100.*real(cthick))                                            !/*px         85*/
      call gpl(2,xy(1,1),xy(1,2))                                               !/*px         86*/
      call gslwsc(1.)                                                           !/*px         87*/
      call gsln(1)                                                              !/*px         88*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+1))                              !/*px4a        8*/
      call gschh(y(.14)-y(0.))                                                  !/*px         89*/
      if(kxsplt.eq.0)then                                                       !/*pxxl        1*/
         call gtx(xy(2,1)+x(.25)-x(0.),y(real(yval)),hl)                        !/*pxxl        2*/
      elseif(kxspmt.lt.1000)then                                                !/*pxxl        3*/
         write(ha,'(i3)')kxspmt                                                 !/*pxxl        4*/
         call gtx(xy(2,1)+x(.05)-x(0.),y(real(yval)),ha)                        !/*pxxl        5*/
         call gtx(xy(2,1)+x(.50)-x(0.),y(real(yval)),hl)                        !/*pxxl        6*/
      else                                                                      !/*pxxl        7*/
         write(ha,'(i5)')kxspmt                                                 !/*pxxl        8*/
         call gtx(xy(2,1),y(real(yval)),ha)                                     !/*pxxl        9*/
         call gtx(xy(2,1)+x(.50)-x(0.),y(real(yval)),hl)                        !/*pxxl       10*/
      endif                                                                     !/*pxxl       11*/
      yval=yval-.3                                                              !/*pxxl       12*/
!                                                                               !/*px         92*/
!        return home and update the display.                                    !/*px         93*/
   50 if(abs(koplot-2).ne.1)call gxhome(x(.05)+zero,y(8.15)+zero)               !/*px4b       14*/
#endif                                                                          /*!pxx         8*/
      return                                                                    !/*px         95*/
      end                                                                       !/*px         96*/

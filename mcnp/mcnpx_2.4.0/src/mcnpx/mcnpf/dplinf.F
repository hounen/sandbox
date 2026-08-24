!#ident dplinf.F from deck dp                                                   !/*dp          1*/
      subroutine dplinf                                                         !/*dp          3*/
#ifdef MCPLOT                                                                   /*!dpx         1*/
!        write information to the legend area of a 2d plot.                     !/*dp          4*/
      use global_data                                                           !/*dpx         2*/
      use mcplot_data                                                           !/*dpx         3*/
!                                                                               !/*dpx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*dpx         5*/
      implicit integer(kindi) (i-n)                                             !/*dpx         6*/
!                                                                               !/*dpx         7*/
      character ha*23,hb(mipt)*1                                                !/*dpx         8*/
      real x,y,s,t,yv                                                           !/*dp          8*/
      x(s)=plim(1)+(s-1.)*(plim(2)-plim(1))/7.                                  !/*dp          9*/
      y(t)=plim(3)+(t-1.-.6*lput)*(plim(4)-plim(3))/6.                          !/*dp         10*/
!                                                                               !/*dp         11*/
      mt=mtal+lpert*ntal                                                        !/*dp4b        1*/
      yv=7.+.6*lput                                                             !/*dp         12*/
      call gschh(y(.14)-y(0.))                                                  !/*dp         13*/
      if(mct.ge.0)then                                                          !/*dpx         9*/
         call gtx(x(8.5),y(yv),kod)                                             !/*dpx        10*/
         call gtx(x(10.),y(yv),ver)                                             !/*dpx        11*/
         call gtx(x(8.5),y(yv-.3),probid)                                       !/*dpx        12*/
      endif                                                                     !/*dpx        13*/
      yv=yv-.6                                                                  !/*dp         17*/
      if(kxsplt.ne.0)go to 100                                                  !/*dp4b        2*/
      if(itfc.gt.30)go to 20                                                    !/*dp4a        1*/
      if(jptal(ljpt+4,mtal).eq.0)call gtx(x(8.5),y(yv),'tally')                 !/*dp4b        3*/
      if(jptal(ljpt+4,mtal).ne.0)call gtx(x(8.5),y(yv),'tally*')                !/*dp4b        4*/
      write(ha,'(i4)')jptal(ljpt+1,mtal)                                        !/*dp         21*/
      call gtx(x(9.25),y(yv),ha(1:4))                                           !/*dp4b        5*/
      if(lpert.eq.0)go to 2                                                     !/*dp4b        6*/
      call gtx(x(9.85),y(yv),'pert')                                            !/*dp4b        7*/
      write(ha,'(i4)')iptb(lipb+1,lpert)                                        !/*dp4b        8*/
      call gtx(x(10.35),y(yv),ha(1:4))                                          !/*dp4b        9*/
    2 if(itfc.le.4)go to 5                                                      !/*dp4b       10*/
      if(hsb(nsp).gt.0.)go to 3                                                 !/*dp4a        3*/
      yv=yv-.3                                                                  !/*dp4a        4*/
      call gtx(x(8.5),y(yv),'score grid for f(-x)')                             !/*dp4a        5*/
    3 if(shsd(lshs+1,mt)+shsd(lshs+nsp,mt).eq.0)go to 5                         !/*dp4b       11*/
      yv=yv-.3                                                                  !/*dp4a        7*/
      call gtx(x(8.5),y(yv),'tallies outside grid')                             !/*dp4a        8*/
    5 continue                                                                  !/*dp4a        9*/
      n=0                                                                       !/*dpx        14*/
      do 7 i=1,mipt                                                             !/*dpx        15*/
      if(ktp(lktp+i,mtal).eq.0)go to 7                                          !/*dpx        16*/
      n=n+1                                                                     !/*dpx        17*/
      hb(n)=hip(i:i)                                                            !/*dpx        18*/
    7 continue                                                                  !/*dpx        19*/
      nl=(n+11)/12                                                              !/*dpx        20*/
      do 18 l=1,nl                                                              !/*dpx        21*/
      ha=' '                                                                    !/*dp         24*/
      yv=yv-.3                                                                  !/*dpx        22*/
      kf=12*(l-1)+1                                                             !/*dpx        23*/
      kl=min(12*l,n)                                                            !/*dpx        24*/
      m=-1                                                                      !/*dpx        25*/
      do 10 i=kf,kl                                                             !/*dpx        26*/
      m=m+2                                                                     !/*dpx        27*/
   10 ha(m:)=hb(i)                                                              !/*dpx        28*/
   18 call gtx(x(8.5),y(yv),ha(1:m))                                            !/*dpx        29*/
      yv=yv-.3                                                                  !/*dpx        30*/
   20 call gtx(x(8.5),y(yv),'nps')                                              !/*dp         34*/
      n=nps                                                                     !/*dp         35*/
      if(nsr.eq.6)n=np1                                                         !/*dp         36*/
      if(nsr.eq.6.and.nrrs.lt.nrss)n=npsr                                       !/*dp         37*/
      if(n.le.99999999)write(ha,'(i8)')n                                        !/*dp4c2       1*/
      if(n.le.99999999)call gtx(x(9.5),y(yv),ha(1:8))                           !/*dp4c2       2*/
      if(n.gt.99999999)write(ha,'(i12)')n                                       !/*dp4c        1*/
      if(n.gt.99999999)call gtx(x(9.5),y(yv),ha(1:12))                          !/*dp4c        2*/
      yv=yv-.3                                                                  !/*dp         40*/
      if(nonorm.ne.0.or.itfc.gt.0.and.itfc.le.5.or.itfc.eq.7.or.                !/*dp4a       10*/
     & itfc.eq.8.or.itfc.ge.18)go to 30                                         !/*dp4a       11*/
      call gtx(x(8.5),y(yv),'bin normed')                                       !/*dp         42*/
      yv=yv-.3                                                                  !/*dp         43*/
   30 if(ktfile.eq.1)call gtx(x(8.5),y(yv),'runtpe = '//runtpe)                 !/*dp         44*/
      if(ktfile.eq.2)call gtx(x(8.5),y(yv),'mctal = '//mctal)                   !/*dp         45*/
      write(ha,'(4hdump,i5)')knod                                               !/*dp         46*/
      if(ktfile.eq.1)call gtx(x(8.5),y(yv-.3),ha(1:9))                          !/*dp         47*/
      yv=yv-.6                                                                  !/*dp         48*/
      if(itfc.gt.30)go to 50                                                    !/*dp4a       12*/
      do 40 i=1,8                                                               !/*dp         50*/
      call gtx(x(8.5),y(yv),ibin(i:i)//'   '//ilbl(i))                          !/*dp         51*/
      ha='   *'                                                                 !/*dp         52*/
      if(ifree(1).ne.i.or.itfc.ne.0)write(ha,'(i4)')ibl(i,1)                    !/*dp         53*/
      call gtx(x(10.2),y(yv),ha(1:4))                                           !/*dp         54*/
      if(ibl(i,1).eq.iptal(lipt+i,4,mtal)-iptal(lipt+i,6,mtal))                 !/*dp4c        3*/
     & call gtx(x(10.8),y(yv),'t')                                              !/*dp4c        4*/
   40 yv=yv-.3                                                                  !/*dp         56*/
   50 yval=yv                                                                   !/*dp         57*/
      if(legend.ne.2)go to 60                                                   !/*dp         58*/
      if(abs(lax-3).eq.1.and.ylg.le.0.)go to 60                                 !/*dp         59*/
      a=ylg                                                                     !/*dp         60*/
      if(abs(lax-3).eq.1)a=log10(ylg)                                           !/*dp         61*/
      yval=1.+.6*lput+6.*(a-plim(3))/(plim(4)-plim(3))                          !/*dp         62*/
   60 if(dissf(1).eq.1.)go to 70                                                !/*dp         63*/
      write(ha,'(1pe7.0)')1./dissf(1)                                           !/*dp         64*/
      call gtx(x(7.5),y(.5+.6*lput),ha)                                         !/*dp         65*/
   70 if(dissf(2).eq.1.)return                                                  !/*dp         66*/
      write(ha,'(1pe7.0)')1./dissf(2)                                           !/*dp         67*/
      call gtx(x(0.),y(7.+.6*lput),ha)                                          !/*dp         68*/
      return                                                                    !/*dp         69*/
!                                                                               !/*dp4b       12*/
!        legend for xs plot.                                                    !/*dp4b       13*/
!        display mat, zaids, and mt.                                            !/*dp4b       14*/
  100 ha=titles(4)                                                              !/*dp4b       15*/
      call gtx(x(8.5),y(yv),ha)                                                 !/*dp4b       16*/
      yv=yv-.4                                                                  !/*dp4b       17*/
      if(kxspma.eq.0)go to 120                                                  !/*dp4b       18*/
      ha=' nuclides'                                                            !/*dp4b       19*/
      call gtx(x(8.5),y(yv),ha)                                                 !/*dp4b       20*/
      yv=yv-.3                                                                  !/*dp4b       21*/
      dv=min(4./kxsplt,.2)                                                      !/*dp4b       22*/
      do 110 j=1,kxsplt                                                         !/*dp4b       23*/
      call zaid(2,ha,ixl(lixl+1,kxspie(lxie+j)))                                !/*dp4b       24*/
      call gtx(x(8.5),y(yv),ha)                                                 !/*dp4b       25*/
  110 yv=yv-dv                                                                  !/*dp4b       26*/
  120 yval=yv-.3                                                                !/*dp4b       27*/
      if(legend.ne.2)return                                                     !/*dp4b       28*/
      if(abs(lax-3).eq.1.and.ylg.le.0.)return                                   !/*dp4b       29*/
      a=ylg                                                                     !/*dp4b       30*/
      if(abs(lax-3).eq.1)a=log10(ylg)                                           !/*dp4b       31*/
      yval=1.+.6*lput+6.*(a-plim(3))/(plim(4)-plim(3))                          !/*dp4b       32*/
#endif                                                                          /*!dpx        31*/
      return                                                                    !/*dp4b       33*/
      end                                                                       !/*dp         70*/

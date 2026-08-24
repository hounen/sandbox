!#ident extrct.F from deck ec                                                   !/*ec          1*/
      subroutine extrct                                                         !/*ec          3*/
#ifdef MCPLOT                                                                   /*!ecx         1*/
!        extract data for the plot.                                             !/*ec          4*/
      use global_data                                                           !/*ecx         2*/
      use mcplot_data                                                           !/*ecx         3*/
!                                                                               !/*ecx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*ecx         5*/
      implicit integer(kindi) (i-n)                                             !/*ecx         6*/
!                                                                               !/*ecx         7*/
!                                                                               !/*ec          8*/
      mt=mtal+lpert*ntal                                                        !/*ec4b        1*/
      if(koplot.ge.2)go to 20                                                   !/*ec          9*/
      do 10 i=1,3                                                               !/*ec         10*/
   10 dissf(i)=1.                                                               !/*ec         11*/
   20 do 30 i=1,8                                                               !/*ec         12*/
      ibl(i,1)=kbin(i,1)                                                        !/*ec         13*/
   30 ibl(i,2)=kbin(i,2)                                                        !/*ec         14*/
      if(nfree.eq.2)go to 360                                                   !/*ec         15*/
!                                                                               !/*ec         16*/
!        extract regular tally data for a 2d plot.                              !/*ec         17*/
      if(itfc.gt.30)go to 90                                                    !/*ec4a        1*/
      if(itfc.ne.0)go to 50                                                     !/*ec         19*/
      if(kxsplt.eq.0)go to 35                                                   !/*ec4b        2*/
      call abvals(lab1,1)                                                       !/*ec4b        3*/
      call exord(2)                                                             !/*ec4b        4*/
      xyzmn(3)=huge                                                             !/*ec4b        5*/
      xyzmx(3)=-huge                                                            !/*ec4b        6*/
      call exord(1)                                                             !/*ec4b        7*/
      ym=xyzmn(3)                                                               !/*ec4b        8*/
      yx=xyzmx(3)                                                               !/*ec4b        9*/
      yl=yx*1.e-17                                                              !/*ec4b       10*/
      if(yx.gt.0.)go to 32                                                      !/*ec4c        1*/
      write(jtty,'(14h xs max is <=0 )')                                        !/*ec4b       12*/
      yx=1.                                                                     !/*ec4b       13*/
      ym=1./huge                                                                !/*ec4b       14*/
      yl=1./huge                                                                !/*ec4b       15*/
   32 if(ym.gt.yl)go to 39                                                      !/*ec4b       16*/
      if(abs(lax-3).ne.1)go to 39                                               !/*ec4b       17*/
!                                                                               !/*ec4b       18*/
!        xs ordinate arrays has too many decades for log plot.                  !/*ec4b       19*/
!        make sure no value is less than 1e-17*max(ord).                        !/*ec4b       20*/
      write(jtty,'(41h some ordinates less than 1e-17*max(ord) )')              !/*ec4b       21*/
      if(ym.eq.0.)yl=1./huge                                                    !/*ec4c        2*/
      do 33 i=1,npt(1)                                                          !/*ec4b       23*/
   33 if(ord(lord+i).lt.yl)ord(lord+i)=yl                                       !/*ec4b       24*/
      go to 39                                                                  !/*ec4b       25*/
   35 call exord(2)                                                             !/*ec4b       26*/
      call abvals(lab1,1)                                                       !/*ec4b       27*/
   39 continue                                                                  !/*ec4b       28*/
      if(inform.ne.0)return                                                     !/*ec         22*/
!                                                                               !/*ec         23*/
!        relocate data to beginning of arrays.                                  !/*ec         24*/
      npt(1)=npt(1)-icut(1)+1                                                   !/*ec         25*/
      do 40 i=1,npt(1)                                                          !/*ec         26*/
      ab1(lab1+i)=ab1(lab1+i+icut(1)-1)                                         !/*ec         27*/
      ord(lord+i)=ord(lord+i+icut(1)-1)                                         !/*ec         28*/
   40 erb(lerb+i)=erb(lerb+i+icut(1)-1)                                         !/*ec         29*/
      go to 110                                                                 !/*ec         30*/
   50 if(itfc.le.5)go to 62                                                     !/*ec4a        2*/
      if(npt(1).lt.2)go to 115                                                  !/*ec4a        3*/
      if(itfc.eq.9)go to 54                                                     !/*ec4a        4*/
!                                                                               !/*ec4a        5*/
!        extract number or tally distribution of tally mtal.                    !/*ec4a        6*/
      k=mod(itfc-2,8)+1                                                         !/*ec4a        7*/
      nl=nhsd(lnhs+nsp+3,mt)-1                                                  !/*ec4b       29*/
      do 51 i=1,npt(1)                                                          !/*ec4a        9*/
      ab1(lab1+i)=.5*(hsb(nl+i)+hsb(nl+i-1))                                    !/*ec4a       10*/
      ord(lord+i)=nhsd(lnhs+nl+i,mt)                                            !/*ec4b       30*/
      if(itfc.eq.8)ord(lord+i)=shsd(lshs+nl+i,mt)                               !/*ec4b       31*/
!                                                                               !/*ec4a       13*/
!        normalize number or tally distribution data if required.               !/*ec4a       14*/
      if(itfc.eq.6.and.nonorm.eq.0)ord(lord+i)=ord(lord+i)/                     !/*ec4a       15*/
     & ((nps-nskk)*(hsb(nl+i)-hsb(nl+i-1)))                                     !/*ec4a       16*/
      if(itfc.ge.10.and.ord(lord+i).gt.0)ord(lord+i)=ord(lord+i)*               !/*ec4a       17*/
     & (shsd(lshs+nl+i,mt)/ord(lord+i))**k/(nps-nskk)                           !/*ec4b       32*/
      if(itfc.ge.10.and.itfc.le.17.and.nonorm.eq.0)ord(lord+i)=                 !/*ec4a       19*/
     & ord(lord+i)/(hsb(nl+i)-hsb(nl+i-1))                                      !/*ec4a       20*/
   51 continue                                                                  !/*ec4a       21*/
      ablo(1)=hsb(nl)                                                           !/*ec4a       22*/
      abhi(1)=hsb(nhsd(lnhs+nsp+4,mt))                                          !/*ec4b       33*/
      if(itfc.eq.6.or.itfc.ge.10.and.itfc.le.17)go to 75                        !/*ec4a       24*/
!                                                                               !/*ec4a       25*/
!        compute cumulative fractions for tfc n and tfc t.                      !/*ec4a       26*/
      if(itfc.eq.7)ord(lord+1)=ord(lord+1)+nhsd(lnhs+1,mt)                      !/*ec4b       34*/
      if(itfc.eq.8)ord(lord+1)=ord(lord+1)+shsd(lshs+1,mt)                      !/*ec4b       35*/
      do 52 i=2,npt(1)                                                          !/*ec4a       29*/
   52 ord(lord+i)=ord(lord+i)+ord(lord+i-1)                                     !/*ec4a       30*/
      if(itfc.ge.18)go to 75                                                    !/*ec4a       31*/
      if(itfc.eq.7)ord(lord+npt(1))=ord(lord+npt(1))+                           !/*ec4a       32*/
     & nhsd(lnhs+nsp,mt)                                                        !/*ec4b       36*/
      if(itfc.eq.8)ord(lord+npt(1))=ord(lord+npt(1))+                           !/*ec4a       34*/
     & shsd(lshs+nsp,mt)                                                        !/*ec4b       37*/
      do 53 i=1,npt(1)                                                          !/*ec4a       36*/
   53 ord(lord+i)=ord(lord+i)/ord(lord+npt(1))                                  !/*ec4a       37*/
      go to 75                                                                  !/*ec4a       38*/
!                                                                               !/*ec4a       39*/
!        create high tally tail pdf of history tally distributions.             !/*ec4a       40*/
   54 np=nhsd(lnhs+nsp+6,mt)                                                    !/*ec4b       38*/
      if(shsd(lshs+nsp+6,mt).eq.shsd(lshs+nsp+5+np,mt))go to 115                !/*ec4b       39*/
      ablo(1)=0.999*shsd(lshs+nsp+6,mt)                                         !/*ec4b       40*/
      abhi(1)=1.001*shsd(lshs+nsp+5+np,mt)                                      !/*ec4b       41*/
!                                                                               !/*ec4a       45*/
!        create 30 bins to display the np pdf tail points.                      !/*ec4a       46*/
      tl=log(ablo(1))                                                           !/*ec4a       47*/
      dt=(log(abhi(1))-tl)/30.                                                  !/*ec4a       48*/
      do 55 i=1,30                                                              !/*ec4a       49*/
      ord(lord+i)=0.                                                            !/*ec4a       50*/
   55 ab1(lab1+i)=exp(tl+i*dt)                                                  !/*ec4a       51*/
      j=1                                                                       !/*ec4a       52*/
      do 57 i=1,np                                                              !/*ec4a       53*/
      do 56 k=1,30                                                              !/*ec4a       54*/
   56 if(shsd(lshs+nsp+5+i,mt).le.ab1(lab1+k))go to 57                          !/*ec4b       42*/
   57 ord(lord+k)=ord(lord+k)+1.                                                !/*ec4a       56*/
      if(nonorm.ne.0)go to 59                                                   !/*ec4a       57*/
!                                                                               !/*ec4a       58*/
!        normalize the number in a tally bin by nps and bin width.              !/*ec4a       59*/
      ord(lord+1)=ord(lord+1)/((nps-nskk)*(ab1(lab1+1)-ablo(1)))                !/*ec4a       60*/
      do 58 i=2,30                                                              !/*ec4a       61*/
   58 ord(lord+i)=ord(lord+i)/((nps-nskk)*(ab1(lab1+i)-ab1(lab1+i-1)))          !/*ec4a       62*/
!                                                                               !/*ec4a       63*/
!        center abcissa values in the center of the bin.                        !/*ec4a       64*/
   59 do 61 i=30,2,-1                                                           !/*ec4a       65*/
   61 ab1(lab1+i)=.5*(ab1(lab1+i)+ab1(lab1+i-1))                                !/*ec4a       66*/
      ab1(lab1+1)=.5*(ab1(lab1+1)+ablo(1))                                      !/*ec4a       67*/
      go to 75                                                                  !/*ec4a       68*/
!                                                                               !/*ec4a       69*/
!        extract tally fluctuation chart data.                                  !/*ec4a       70*/
   62 do 60 i=1,20                                                              !/*ec4a       71*/
      if(npc(i).eq.0)go to 70                                                   !/*ec         34*/
      ab1(lab1+i)=npc(i)                                                        !/*ec         35*/
      t5=tfc(ltfc+itfc,i,mt)                                                    !/*ec4b       43*/
      if(itfc.ne.5)go to 63                                                     !/*ec4a       73*/
      t5=min(ten,t5)                                                            !/*ecx         8*/
      if(t5.le.1..and.t5.ne.0.)t5=10.                                           !/*ec4a       75*/
   63 ord(lord+i)=t5                                                            !/*ec4a       76*/
   60 erb(lerb+i)=tfc(ltfc+2,i,mt)                                              !/*ec4b       44*/
   70 npt(1)=i-1                                                                !/*ec         38*/
      if(npt(1).lt.2)go to 115                                                  !/*ec4a       77*/
      ablo(1)=0.                                                                !/*ec         40*/
      abhi(1)=ab1(lab1+npt(1))*2.-ab1(lab1+npt(1)-1)                            !/*ec         41*/
   75 do 80 i=1,8                                                               !/*ec4a       78*/
      ibl(i,1)=jtf(ljtf+i,mtal)                                                 !/*ec         43*/
   80 ibl(i,2)=ibl(i,1)                                                         !/*ec         44*/
      go to 110                                                                 !/*ec         45*/
!                                                                               !/*ec         46*/
!        extract kcode data.                                                    !/*ec         47*/
   90 i=mod(itfc,10)                                                            !/*ec         48*/
      ik=0                                                                      !/*ec4a       79*/
      if(itfc.lt.40)go to 95                                                    !/*ec4a       80*/
      kb=1                                                                      !/*ec4a       81*/
      if(itfc.gt.45)go to 92                                                    !/*ec4a       82*/
      i=2*i+4                                                                   !/*ec4a       83*/
      ik=ikz+1                                                                  !/*ec4a       84*/
      if(itfc.le.43)go to 96                                                    !/*ec4a       85*/
!                                                                               !/*ec4a       86*/
!        plot the prompt neutron removal lifetimes.                             !/*ec4b       45*/
      i=mod(itfc,10)                                                            !/*ec4a       88*/
      ik=ikz                                                                    !/*ec4a       89*/
      go to 96                                                                  !/*ec4a       90*/
!                                                                               !/*ec4a       91*/
!        define itfc locations: itfc=46 k(c/a/t) versus cycle number;           !/*ec4a       92*/
!        itfc=47, k(c/a/t) by skipped cycles; itfc=48,k(c/a/t) fom;             !/*ec4a       93*/
!        itfc=49, k(c/a/t) relative error.                                      !/*ec4a       94*/
   92 if(itfc.eq.47)go to 94                                                    !/*ec4a       95*/
      ik=ikz+3                                                                  !/*ec4a       96*/
      i=12                                                                      !/*ec4a       97*/
      if(itfc.eq.46)go to 96                                                    !/*ec4a       98*/
      i=19                                                                      !/*ec4a       99*/
      if(itfc.eq.49)i=13                                                        !/*ec4a      100*/
      go to 95                                                                  !/*ec4a      101*/
   94 if(kcz.gt.mrkp)go to 124                                                  !/*ec4a      102*/
      i=14                                                                      !/*ec4a      103*/
      ik=-1                                                                     !/*ec4a      104*/
      npt(1)=kcz-9                                                              !/*ec4a      105*/
      go to 98                                                                  !/*ec4a      106*/
!                                                                               !/*ec4a      108*/
!        set up points for combined keff or prompt lifetime estimators.         !/*ec4b       46*/
   95 kb=0                                                                      !/*ec4c       23*/
   96 npt(1)=npt(1)-ik                                                          !/*ec4a      110*/
   98 t1=0.                                                                     !/*ec4a      111*/
      ip=max(0,ik)                                                              !/*ec4a      112*/
   99 do 100 j=1,npt(1)                                                         !/*ec4c       24*/
      ab1(lab1+j)=ik+j                                                          !/*ec4a      113*/
      ord(lord+j)=rkpl(lrkp+i,j+ip)                                             !/*ec4a      114*/
      t1=t1+ord(lord+j)                                                         !/*ec4a      115*/
      if(itfc.eq.44.or.itfc.eq.45)ord(lord+j)=t1/j                              !/*ec4a      116*/
      if(itfc.eq.49.and.rkpl(lrkp+12,j+ip).ne.0.)ord(lord+j)=                   !/*ec4a      117*/
     & ord(lord+j)/rkpl(lrkp+12,j+ip)                                           !/*ec4a      118*/
      if(kb.eq.0)go to 100                                                      !/*ec4a      119*/
      erb(lerb+j)=0.                                                            !/*ec4a      120*/
      if(ord(lord+j).gt.0.)erb(lerb+j)=rkpl(lrkp+i+1,j+ip)/ord(lord+j)          !/*ec4a      121*/
      if(i.eq.22)erb(lerb+j)=rkpl(lrkp+23,j+ip)                                 !/*ec4c       25*/
      if(i.eq.34.and.rkpl(lrkp+34,j+ip).ne.0.)                                  !/*ec4c       26*/
     & erb(lerb+j)=rkpl(lrkp+35,j+ip)/rkpl(lrkp+34,j+ip)                        !/*ec4c       27*/
  100 continue                                                                  !/*ec4a      122*/
      ablo(1)=ik+1                                                              !/*ec4a      123*/
      abhi(1)=npt(1)+ik                                                         !/*ec4a      124*/
!                                                                               !/*ec         57*/
!        check for enough points and non-zero ordinates.                        !/*ec         58*/
  110 if(npt(1).gt.1)go to 130                                                  !/*ec         59*/
  115 write(jtty,120)                                                           !/*ec4a      125*/
  120 format(51h number of points to plot is less than 2.  no plot.)            !/*ec         61*/
      if(koplot.eq.1)koplot=0                                                   !/*ec4a      126*/
      inform=1                                                                  !/*ec         62*/
      return                                                                    !/*ec         63*/
  124 write(jtty,128)mrkp,kcz                                                   !/*ec4a      127*/
  128 format(31h too many keff cycles for plot./19h increase mrkp from,         !/*ec4a      128*/
     & i5,3h to,i5,15h on kcode card.)                                          !/*ec4a      129*/
      inform=1                                                                  !/*ec4a      130*/
      koplot=0                                                                  !/*ec4a      131*/
      return                                                                    !/*ec4a      132*/
  130 if(npt(1).gt.2.or.kurv.ne.3.or.tensn.ne.0.)go to 135                      !/*ec4a      133*/
      write(jtty,132)                                                           !/*ec4a      134*/
  132 format(51h two points are too few for cubic spline.  no plot.)            !/*ec4a      135*/
      if(koplot.eq.1)koplot=0                                                   !/*ec4a      136*/
      inform=1                                                                  !/*ec4a      137*/
      return                                                                    !/*ec4a      138*/
  135 do 140 i=1,npt(1)                                                         !/*ec4a      139*/
  140 if(ord(lord+i).ne.0.)go to 150                                            !/*ec         65*/
      write(jtty,'(34h ordinates are all zero.  no plot.)')                     !/*ec         66*/
      inform=1                                                                  !/*ec         67*/
      return                                                                    !/*ec         68*/
!                                                                               !/*ec         69*/
!        check log plot abscissa array for non-positive values.                 !/*ec         70*/
  150 if(lax.lt.3)go to 200                                                     !/*ec         71*/
      do 160 i=1,npt(1)                                                         !/*ec         72*/
  160 if(ab1(lab1+i).le.0.)go to 170                                            !/*ec         73*/
      if(ablo(1).gt.0.)go to 200                                                !/*ec         74*/
  170 if(koplot.ge.2)go to 180                                                  !/*ec         75*/
      lax=lax-2                                                                 !/*ec         76*/
      go to 200                                                                 !/*ec         77*/
  180 write(jtty,190)                                                           !/*ec         78*/
  190 format(50h coplot is impossible because some abscissa values,             !/*ec         79*/
     & 18h are not positive.)                                                   !/*ec         80*/
      inform=1                                                                  !/*ec         81*/
      koplot=0                                                                  !/*ec         82*/
      return                                                                    !/*ec         83*/
!                                                                               !/*ec         84*/
!        check log plot ordinate array for non-positive values.                 !/*ec         85*/
  200 if(abs(lax-3).ne.1)go to 260                                              !/*ec         86*/
      ap=huge                                                                   !/*ec         87*/
      am=huge                                                                   !/*ec         88*/
      ax=-huge                                                                  !/*ec         89*/
      do 210 i=1,npt(1)                                                         !/*ec         90*/
      am=min(am,ord(lord+i))                                                    !/*ec         91*/
      if(ord(lord+i).gt.0.)ap=min(ap,ord(lord+i))                               !/*ec         92*/
  210 ax=max(ax,ord(lord+i))                                                    !/*ec         93*/
      if(am.gt.0.)go to 260                                                     !/*ec         94*/
      if(ax.le.0.)go to 230                                                     !/*ec         95*/
      do 220 i=1,npt(1)                                                         !/*ec         96*/
  220 if(ord(lord+i).le.0.)ord(lord+i)=1./huge                                  !/*ec         97*/
      if(kurv.eq.3)kurv=2                                                       !/*ec         98*/
      go to 260                                                                 !/*ec         99*/
  230 if(koplot.ge.2)go to 240                                                  !/*ec        100*/
      lax=lax-1                                                                 !/*ec        101*/
      go to 260                                                                 !/*ec        102*/
  240 write(jtty,250)                                                           !/*ec        103*/
  250 format(51h coplot is impossible because there are no positive,            !/*ec        104*/
     & 17h ordinate values.)                                                    !/*ec        105*/
      inform=1                                                                  !/*ec        106*/
      koplot=0                                                                  !/*ec        107*/
      return                                                                    !/*ec        108*/
!                                                                               !/*ec        109*/
!        scale the data to be plotted.                                          !/*ec        110*/
  260 ablo(1)=(ablo(1)*scalf(1,1)+scalf(2,1))*dissf(1)                          !/*ec        111*/
      abhi(1)=(abhi(1)*scalf(1,1)+scalf(2,1))*dissf(1)                          !/*ec        112*/
      do 270 i=1,npt(1)                                                         !/*ec        113*/
  270 ab1(lab1+i)=(ab1(lab1+i)*scalf(1,1)+scalf(2,1))*dissf(1)                  !/*ec        114*/
      do 280 i=1,npt(1)                                                         !/*ec        115*/
  280 ord(lord+i)=(ord(lord+i)*scalf(1,2)+scalf(2,2))*dissf(2)                  !/*ec        116*/
      if(koplot.ge.2)go to 320                                                  !/*ec        117*/
!                                                                               !/*ec        118*/
!        determine axis limits.                                                 !/*ec        119*/
      if(jlim(1).eq.0)call gxlims(ablo(1),abhi(1),xyzmn(1),xyzmx(1),n,          !/*ec        120*/
     & max(0,lax-2))                                                            !/*ec        121*/
      if(jlim(2).ne.0)go to 300                                                 !/*ec        122*/
      a=huge                                                                    !/*ec        123*/
      b=-huge                                                                   !/*ec        124*/
      do 290 i=1,npt(1)                                                         !/*ec        125*/
      a=min(a,.95*ord(lord+i))                                                  !/*ec        126*/
  290 b=max(b,1.05*ord(lord+i))                                                 !/*ec        127*/
      if(abs(lax-3).eq.1)a=max(a,ap)                                            !/*ec        128*/
      call gxlims(a,b,xyzmn(2),xyzmx(2),n,mod(lax-1,2))                         !/*ec        129*/
  300 continue                                                                  !/*ec        130*/
!                                                                               !/*ec        131*/
#ifdef DISSPLA                                                                  /*!ec        132*/
      call disbug                                                               !/*ec        133*/
#endif                                                                          /*!cpp       310*/
!                                                                               !/*ec        134*/
!        print points if printpts option is on.                                 !/*ec        135*/
      if(iprpts.eq.0)go to 320                                                  !/*ec        136*/
!                                                                               !/*ec4b       47*/
!        print plot points to a file on unit iupx.                              !/*ec4c       62*/
      if(iprpts.ne.2)go to 305                                                  !/*ec4b       49*/
      write(iupx,310)(ab1(lab1+i),ord(lord+i),erb(lerb+i),i=1,npt(1))           !/*ec4b       50*/
      close(iupx)                                                               !/*ec4c       63*/
      inform=1                                                                  !/*ec4b       52*/
      return                                                                    !/*ec4b       53*/
  305 write(jtty,310)(ab1(lab1+i),ord(lord+i),erb(lerb+i),i=1,npt(1))           !/*ec4b       54*/
  310 format(/5x,12hplot points://13x,1hx,24x,1hy,19x,1he//                     !/*ec        138*/
     & (1pe25.10,e25.10,0pf13.5))                                               !/*ec        139*/
      inform=1                                                                  !/*ec        140*/
      return                                                                    !/*ec        141*/
!                                                                               !/*ec        142*/
!        convert data to log10 if required.                                     !/*ec        143*/
  320 if(lax.lt.3)go to 340                                                     !/*ec        144*/
      do 330 i=1,npt(1)                                                         !/*ec        145*/
  330 ab1(lab1+i)=log10(ab1(lab1+i))                                            !/*ec        146*/
      ablo(1)=log10(ablo(1))                                                    !/*ec        147*/
  340 if(abs(lax-3).ne.1)go to 410                                              !/*ec        148*/
      do 350 i=1,npt(1)                                                         !/*ec        149*/
  350 ord(lord+i)=log10(ord(lord+i))                                            !/*ec        150*/
      go to 410                                                                 !/*ec        151*/
!                                                                               !/*ec        152*/
!        get abscissa data and extreme ordinates for 3d or contour plot.        !/*ec        153*/
  360 call abvals(lab1,1)                                                       !/*ec        154*/
      call abvals(lab2,2)                                                       !/*ec        155*/
      if(inform.ne.0)return                                                     !/*ec        156*/
      xyzmn(3)=huge                                                             !/*ec        157*/
      xyzmx(3)=-huge                                                            !/*ec        158*/
      call exord(1)                                                             !/*ec        159*/
      if(xyzmx(3).gt.xyzmn(3))go to 380                                         !/*ec        160*/
      write(jtty,370)                                                           !/*ec        161*/
  370 format(38h ordinate data are constant.  no plot.)                         !/*ec        162*/
      inform=1                                                                  !/*ec        163*/
      return                                                                    !/*ec        164*/
!                                                                               !/*ec        165*/
!        scale the abscissa data and extreme ordinates.                         !/*ec        166*/
  380 ablo(1)=ablo(1)*scalf(1,1)+scalf(2,1)                                     !/*ec        167*/
      abhi(1)=abhi(1)*scalf(1,1)+scalf(2,1)                                     !/*ec        168*/
      do 390 i=1,npt(1)                                                         !/*ec        169*/
  390 ab1(lab1+i)=ab1(lab1+i)*scalf(1,1)+scalf(2,1)                             !/*ec        170*/
      ablo(2)=ablo(2)*scalf(1,2)+scalf(2,2)                                     !/*ec        171*/
      abhi(2)=abhi(2)*scalf(1,2)+scalf(2,2)                                     !/*ec        172*/
      do 400 i=1,npt(2)                                                         !/*ec        173*/
  400 ab2(lab2+i)=ab2(lab2+i)*scalf(1,2)+scalf(2,2)                             !/*ec        174*/
      xyzmx(3)=xyzmx(3)*scalf(1,3)+scalf(2,3)                                   !/*ec        175*/
      xyzmn(3)=xyzmn(3)*scalf(1,3)+scalf(2,3)                                   !/*ec        176*/
      if(jlim(1).eq.0)call gxlims(ablo(1),abhi(1),xyzmn(1),xyzmx(1),n,0)        !/*ec        177*/
      if(jlim(2).eq.0)call gxlims(ablo(2),abhi(2),xyzmn(2),xyzmx(2),n,0)        !/*ec        178*/
!                                                                               !/*ec        179*/
#ifdef DISSPLA                                                                  /*!ec        180*/
      call disbug                                                               !/*ec        181*/
#endif                                                                          /*!cpp       311*/
!                                                                               !/*ec        182*/
!        set up plot limits.                                                    !/*ec        183*/
  410 do 420 i=1,2                                                              !/*ec        184*/
      plim(2*i-1)=xyzmn(i)                                                      !/*ec        185*/
      plim(2*i)=xyzmx(i)                                                        !/*ec        186*/
      if(lax.ne.4.and.lax.ne.4-i.or.nfree.eq.2)go to 420                        !/*ec        187*/
      plim(2*i-1)=log10(plim(2*i-1))                                            !/*ec        188*/
      plim(2*i)=log10(plim(2*i))                                                !/*ec        189*/
  420 if(jlim(i).eq.0)itik(i)=0                                                 !/*ec        190*/
#endif                                                                          /*!ecx        15*/
      return                                                                    !/*ec        191*/
      end                                                                       !/*ec        192*/

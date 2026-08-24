!#ident tekdvr.F from deck tk                                                   !/*tk          1*/
      subroutine tekdvr                                                         !/*tk          3*/
#ifdef PLOT                                                                     /*!tkx         1*/
!        draw the picture.                                                      !/*tk          4*/
      use global_data                                                           !/*tkx         2*/
      use plot_data                                                             !/*tkx         3*/
!                                                                               !/*tkx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*tkx         5*/
      implicit integer(kindi) (i-n)                                             !/*tkx         6*/
!                                                                               !/*tkx         7*/
      character hl*42,hi(mclb)*25                                               !/*tkx         8*/
      real et(2),xz(100),yz(100),xy(2,2),xv,yd,yv                               !/*tk          8*/
      dimension np(mclb),nn(mclb)                                               !/*tk          9*/
      data hi/'cell names','importances','atom densities',                      !/*tk         10*/
     & 'mass densities','volumes','forced collisions','masses',                 !/*tk         11*/
     & 'photon production weights','material numbers','temperatures',           !/*tk         12*/
     & 'weight windows','exponential transforms',                               !/*tk         13*/
     & 'detector contributions','dxtran contributions','universes',             !/*tk         14*/
     & 'lattices','filling universes','fission turnoffs',                       !/*tk         15*/
     & 'activity, column'/                                                      !/*tk         16*/
      data np/0,1,3*0,1,4*0,1,1,0,1,4*0,1/,nn/9*0,1,1,0,1,5*0,1/                !/*tk         17*/
!                                                                               !/*tk         18*/
!        specify the window and the character expansion factor.                 !/*tk         19*/
#ifdef LAHEY                                                                    /*!tk4c        1*/
      call IGrAreaClear(0.0,0.0,1.0,1.0)                                        !/*tk4c        2*/
#endif                                                                          /*!cpp       163*/
      et(1)=extent(1)*(1.+.1*(1-jvp))                                           !/*tk         20*/
      et(2)=extent(2)*(1.+.1*(1-jvp))                                           !/*tk         21*/
      call gswn(1,-et(1)*(5-2*jvp)/3.,et(1),-et(2),et(2))                       !/*tk         22*/
      wnvp(1)=et(1)*(8-2*jvp)/3.                                                !/*tk         23*/
      wnvp(2)=et(2)*2.                                                          !/*tk         24*/
      call gschxp(et(1)/et(2))                                                  !/*tk         25*/
      if(mcolor.gt.2)call shade(lcolor)                                         !/*tk4a        1*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+1))                              !/*tk4b        1*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*tk4b        2*/
!                                                                               !/*tk         26*/
!        mark the center of the picture.                                        !/*tk         27*/
      do 10 i=1,2                                                               !/*tk         28*/
      xy(1,i)=-.011*et(i)                                                       !/*tk         29*/
      xy(2,i)=-xy(1,i)                                                          !/*tk         30*/
      xy(1,3-i)=0.                                                              !/*tk         31*/
      xy(2,3-i)=0.                                                              !/*tk         32*/
   10 call gpl(2,xy(1,1),xy(1,2))                                               !/*tk         33*/
!                                                                               !/*tk         34*/
!        draw scales if required.                                               !/*tk         35*/
      if(jvp.ne.0.or.jscal.eq.0)go to 20                                        !/*tk         36*/
      g=0.                                                                      !/*tk         37*/
      if(jscal.eq.2)g=2.*extent(2)                                              !/*tk         38*/
      call gxaxis(-extent(1),extent(1),-extent(2),-extent(2),                   !/*tk         39*/
     & -.02*extent(2),g,0,0,1,' ')                                              !/*tk         40*/
      call gxaxis(-extent(1),extent(1),extent(2),extent(2),                     !/*tk         41*/
     & .02*extent(2),zero,0,0,0,' ')                                            !/*tk         42*/
      if(jscal.eq.2)g=2.*extent(1)                                              !/*tk         43*/
      call gxaxis(-extent(1),-extent(1),-extent(2),extent(2),                   !/*tk         44*/
     & -.02*extent(1),g,0,0,1,' ')                                              !/*tk         45*/
      call gxaxis(extent(1),extent(1),-extent(2),extent(2),                     !/*tk         46*/
     & .02*extent(1),zero,0,0,0,' ')                                            !/*tk         47*/
!                                                                               !/*tk         48*/
!        draw the picture, one arc at a time.                                   !/*tk         49*/
   20 if(sclabl(1).ne.0.)call gschh(real(sch*sclabl(1))*et(2))                  !/*tk4a        2*/
      jd=0                                                                      !/*tk         51*/
      nlb=0                                                                     !/*tk         52*/
      lp=lzst+12                                                                !/*tk         53*/
      do 60 na=1,int(zst(lzst+1))                                               !/*tk         54*/
      mp=zst(lp+1)                                                              !/*tk         55*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+int(zst(lp+2))))                 !/*tk4b        3*/
      if(sclabl(1).ne.0.)call putlbl(lp)                                        !/*tk         56*/
!                                                                               !/*tk         57*/
!        draw the pieces of the arc.                                            !/*tk         58*/
      call gsln(int(zst(lp+2)))                                                 !/*tk         59*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+int(zst(lp+2))))                 !/*tk4a        3*/
      do 40 m=1,mp-1,99                                                         !/*tk         60*/
      n=min(100,mp-m+1)                                                         !/*tk         61*/
      do 30 i=1,n                                                               !/*tk         62*/
      xz(i)=zst(lp+2*(m+i)-1)                                                   !/*tk         63*/
   30 yz(i)=zst(lp+2*(m+i))                                                     !/*tk         64*/
   40 call gpl(n,xz,yz)                                                         !/*tk         65*/
      if(zst(lp+2).ne.3.or.jd.ne.0)go to 60                                     !/*tk         66*/
      cm=0.                                                                     !/*tk         67*/
      do 50 j=lp+5,lp+2*mp+1,2                                                  !/*tk         68*/
   50 cm=cm+sqrt(((zst(j)-zst(j-2))/et(1))**2+                                  !/*tk         69*/
     & ((zst(j+1)-zst(j-1))/et(2))**2)                                          !/*tk         70*/
      if(cm.gt.res)jd=1                                                         !/*tk         71*/
   60 lp=lp+2*mp+3                                                              !/*tk         72*/
      call gsln(1)                                                              !/*tk         73*/
!                                                                               !/*tk         74*/
!        put on cell labels if required.                                        !/*tk         75*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+3))                              !/*tk4a        5*/
      if(sclabl(2).ne.0.)call gschh(real(sch*sclabl(2))*et(2))                  !/*tk4a        6*/
      do 70 ic=1,int(zst(lzst+8))                                               !/*tk         77*/
      ar=zst(lp)                                                                !/*tk         78*/
      call fshort(ar,hl,n)                                                      !/*tk         79*/
      call gtx(real(zst(lp+1)),real(zst(lp+2)),hl(1:n))                         !/*tk         80*/
   70 lp=lp+3                                                                   !/*tk         81*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*tk4a        7*/
!                                                                               !/*tk         82*/
!        write the legend if the viewport is rectangular.                       !/*tk         83*/
      yv=.96*et(2)                                                              !/*tk         84*/
      yd=1.5*sch*et(2)                                                          !/*tk4a        8*/
#ifdef LAHEY                                                                    /*!tk4c        3*/
      if(jgf.eq.0)yd=yd*2.0                                                     !/*tk4c        4*/
#endif                                                                          /*!cpp       164*/
      call gschh(real(sch)*et(2))                                               !/*tk4a        9*/
      if(jvp.ne.0)go to 110                                                     !/*tk         87*/
      call getidt(idtm)                                                         !/*tk         88*/
      xv=-1.64*et(1)                                                            !/*tk         89*/
      if(mct.ge.0)call gtx(xv,yv,idtm)                                          !/*tkx         9*/
      i=33                                                                      !/*tk         91*/
      do 80 iz=1,33                                                             !/*tk         92*/
   80 if(aid(iz:iz).eq.' ')i=iz                                                 !/*tk         93*/
      j=i+32                                                                    !/*tk         94*/
      do 90 jz=i,i+32                                                           !/*tk         95*/
   90 if(aid(jz:jz).eq.' ')j=jz                                                 !/*tk         96*/
      if(j.lt.49)j=49                                                           !/*tk         97*/
      if(j-i.gt.32)i=j-32                                                       !/*tk         98*/
      call gtx(xv,yv-yd,aid(1:i-1))                                             !/*tk         99*/
      call gtx(xv,yv-2.*yd,aid(i:j-1))                                          !/*tk        100*/
      call gtx(xv,yv-3.*yd,aid(j:80))                                           !/*tk        101*/
      if(mct.ge.0)call gtx(xv,yv-4.*yd,'probid = '//probid)                     !/*tkx        10*/
      hl='basis:'                                                               !/*tk4c2       1*/
      if(basis(1).eq.0..and.basis(4).eq.0.)hl(10:11)='YZ'                       !/*tk4c2       2*/
      if(basis(2).eq.0..and.basis(5).eq.0.)hl(10:11)='XZ'                       !/*tk4c2       3*/
      if(basis(3).eq.0..and.basis(6).eq.0.)hl(10:11)='XY'                       !/*tk4c2       4*/
      call gtx(xv,yv-5.*yd,hl(1:11))                                            !/*tk4c2       5*/
      write(hl,'(1h(,f9.6,1h,,f9.6,1h,,f9.6,1h))')(basis(i),i=1,3)              !/*tk        104*/
      call gtx(xv,yv-6.*yd,hl(1:31))                                            !/*tk        105*/
      write(hl,'(1h(,f9.6,1h,,f9.6,1h,,f9.6,1h))')(basis(i),i=4,6)              !/*tk        106*/
      call gtx(xv,yv-7.*yd,hl(1:31))                                            !/*tk        107*/
      call gtx(xv,yv-8.*yd,'origin:')                                           !/*tk        108*/
      write(hl,'(1h(,f9.2,1h,,f9.2,1h,,f9.2,1h))')origin                        !/*tk        109*/
      call gtx(xv,yv-9.*yd,hl(1:31))                                            !/*tk        110*/
      write(hl,'(10hextent = (,f9.2,1h,,f9.2,1h))')extent                       !/*tk        111*/
      call gtx(xv,yv-10.*yd,hl(1:30))                                           !/*tk        112*/
      yv=yv-11.*yd                                                              !/*tk        113*/
      if(sclabl(2).eq.0.)go to 100                                              !/*tk        114*/
      m=sclabl(3)                                                               !/*tk        115*/
      hl=' '//hi(m)                                                             !/*tk        116*/
      if(np(m).ne.0)hl=' '//hnp(int(sclabl(4))/1000)//' '//hi(m)                !/*tk        117*/
      i=leng(hl)                                                                !/*tk        118*/
      n=mod(int(sclabl(4)),1000)                                                !/*tk        119*/
      if(m.eq.13)n=jptal(ljpt+1,n)                                              !/*tk        120*/
      if(nn(m).ne.0)write(hl(i+1:i+4),'(i4)')n                                  !/*tk        121*/
      call gtx(xv,yv,'cell labels are')                                         !/*tk        122*/
      call gtx(xv,yv-yd,hl(1:35))                                               !/*tkx        11*/
      yv=yv-2.*yd                                                               !/*tk        124*/
  100 if(jd.eq.0)go to 110                                                      !/*tk        125*/
      call gtx(xv,yv,'possible causes of dotted lines:')                        !/*tk        126*/
      call gtx(xv,yv-yd,' errors in the geometry.')                             !/*tk        127*/
      call gtx(xv,yv-2.*yd,' cookie-cutter cell in the source.')                !/*tk        128*/
      call gtx(xv,yv-3.*yd,' a problem plane coincident with')                  !/*tk        129*/
      call gtx(xv,yv-4.*yd,'  the plot plane.')                                 !/*tk        130*/
      yv=yv-5.*yd                                                               !/*tk        131*/
!                                                                               !/*tk        132*/
!        return home and update the display.                                    !/*tk        133*/
  110 xhom=-.33*(5-2*jvp)*et(1)                                                 !/*tk        134*/
      yhom=yv-yd                                                                !/*tk        135*/
#ifndef LAHEY                                                                   /*!tk4c        5*/
      call gxhome(xhom,yhom)                                                    !/*tk        136*/
#endif                                                                          /*!cpp       165*/
#endif                                                                          /*!tkx        12*/
      return                                                                    !/*tk        137*/
      end                                                                       !/*tk        138*/

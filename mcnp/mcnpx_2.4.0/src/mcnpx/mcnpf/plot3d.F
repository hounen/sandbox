!#ident plot3d.F from deck py                                                   !/*py          1*/
      subroutine plot3d                                                         !/*py          4*/
#if defined(MCPLOT) && defined(DISSPLA)                                         /*!pyx         1*/
!        draw a 3d plot.  requires disspla.  bypasses most of gks.              !/*py          5*/
      use global_data                                                           !/*pyx         2*/
      use mcplot_data                                                           !/*pyx         3*/
!                                                                               !/*pyx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pyx         5*/
      implicit integer(kindi) (i-n)                                             !/*pyx         6*/
!                                                                               !/*pyx         7*/
      use gkssim_data                                                           !/*pyxc        1*/
      dimension ib(20)                                                          !/*py          9*/
!                                                                               !/*py         10*/
!        set up for a 3d plot.                                                  !/*py         11*/
      call physor(1.,1.)                                                        !/*py         12*/
      call area2d(8.,6.)                                                        !/*py         13*/
      call height(.14)                                                          !/*py         14*/
      call reset(4hxnon)                                                        !/*py         15*/
      call reset(4hynon)                                                        !/*py         16*/
      call reset(4hxtic)                                                        !/*py         17*/
      call reset(4hytic)                                                        !/*py         18*/
      call vuangl(real(pht(1)),real(pht(2)),3.)                                 !/*py         19*/
      call volm3d(1.,1.,.5)                                                     !/*py         20*/
      call chrhol(titles(1),ib,0)                                               !/*py         21*/
      if(titles(1).ne.' ')call x3name(ib,leng(titles(1)))                       !/*py         22*/
      call chrhol(titles(2),ib,0)                                               !/*py         23*/
      if(titles(2).ne.' ')call y3name(ib,leng(titles(2)))                       !/*py         24*/
      call chrhol(titles(3),ib,0)                                               !/*py         25*/
      if(titles(3).ne.' ')call z3name(ib,leng(titles(3)))                       !/*py         26*/
      call graf3d(real(xyzmn(1)),4hscal,real(xyzmx(1)),real(xyzmn(2)),          !/*py         27*/
     & 4hscal,real(xyzmx(2)),real(xyzmn(3)),4hscal,real(xyzmx(3)))              !/*py         28*/
      lvd=3                                                                     !/*py         29*/
      call surtrn(4hboth)                                                       !/*py         30*/
!                                                                               !/*py         31*/
!        write the main title.                                                  !/*py         32*/
      call height(.26)                                                          !/*py         33*/
      do 10 i=5,6                                                               !/*py         34*/
      call chrhol(titles(i),ib,0)                                               !/*py         35*/
   10 call headin(ib,40,1.,2)                                                   !/*py         36*/
!                                                                               !/*py         37*/
!        extract, scale, and plot sub-blocks.                                   !/*py         38*/
      do 20 i=icut(1),npt(1)                                                    !/*py         39*/
      if(ab1(lab1+i).le.xyzmx(1))n1=i                                           !/*py         40*/
   20 if(ab1(lab1+icut(1)+npt(1)-i).ge.xyzmn(1))i1=icut(1)+npt(1)-i             !/*py         41*/
      do 30 i=icut(2),npt(2)                                                    !/*py         42*/
      if(ab2(lab2+i).le.xyzmx(2))n2=i                                           !/*py         43*/
   30 if(ab2(lab2+icut(2)+npt(2)-i).ge.xyzmn(2))i2=icut(2)+npt(2)-i             !/*py         44*/
      call height(.18)                                                          !/*py         45*/
      ibl(ifree(2),2)=i2                                                        !/*py         46*/
   40 jb=min(jsbm,n2+1-ibl(ifree(2),2))                                         !/*py         47*/
      ibl(ifree(2),1)=ibl(ifree(2),2)                                           !/*py         48*/
      ibl(ifree(2),2)=ibl(ifree(2),2)+jb-1                                      !/*py         49*/
      ibl(ifree(1),2)=i1                                                        !/*py         50*/
   50 jc=min(isbm,n1+1-ibl(ifree(1),2))                                         !/*py         51*/
      ibl(ifree(1),1)=ibl(ifree(1),2)                                           !/*py         52*/
      ibl(ifree(1),2)=ibl(ifree(1),2)+jc-1                                      !/*py         53*/
      call exord(2)                                                             !/*py         54*/
      do 60 i=1,jc*jb                                                           !/*py         55*/
   60 yrr(lyrr+i)=(ord(lord+i)*scalf(1,3)+scalf(2,3))*dissf(3)                  !/*py         56*/
      call surmat(yrr(lyrr+1),1,jc,1,jb,0)                                      !/*py         57*/
      if(ibl(ifree(1),2).lt.n1)go to 50                                         !/*py         58*/
      if(ibl(ifree(2),2).lt.n2)go to 40                                         !/*py         59*/
!                                                                               !/*py         60*/
!        return home.                                                           !/*py         61*/
      call height(.08)                                                          !/*py         62*/
      call messag(1h.,1,-.95,7.15)                                              !/*py         63*/
      call guwk(1,0)                                                            !/*py         64*/
#endif                                                                          /*!pyx         9*/
      return                                                                    !/*py         65*/
      end                                                                       !/*py         66*/

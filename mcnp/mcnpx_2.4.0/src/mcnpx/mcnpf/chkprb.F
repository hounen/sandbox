!#ident chkprb.F from deck cx                                                   !/*cx          1*/
      subroutine chkprb                                                         !/*cx          3*/
#ifdef MCPLOT                                                                   /*!cxx         1*/
!        check and complete the mcplot request definition.                      !/*cx          4*/
      use global_data                                                           !/*cxx         2*/
      use mcplot_data                                                           !/*cxx         3*/
!                                                                               !/*cxx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*cxx         5*/
      implicit integer(kindi) (i-n)                                             !/*cxx         6*/
!                                                                               !/*cxx         7*/
!                                                                               !/*cx          7*/
!        complain if there's no dump.                                           !/*cx          8*/
      if(ktfile.ne.0)go to 10                                                   !/*cx          9*/
      write(jtty,'(42h there is no runtpe or mctal dump on hand.)')             !/*cx         10*/
      inform=1                                                                  !/*cx         11*/
      return                                                                    !/*cx         12*/
   10 if(itfc.gt.30)go to 70                                                    !/*cx4a        1*/
      if(ntal.ne.0)go to 20                                                     !/*cx         14*/
      write(jtty,'(22h no tallies.  no plot.)')                                 !/*cx         15*/
      inform=1                                                                  !/*cx         16*/
      return                                                                    !/*cx         17*/
   20 ilbl(1)='cell'                                                            !/*cx         18*/
      if(jptal(ljpt+2,mtal).lt.4)ilbl(1)=' surface'                             !/*cx         19*/
      if(jptal(ljpt+2,mtal).eq.5)ilbl(1)='detector'                             !/*cx         20*/
      if(itfc.ne.0)go to 70                                                     !/*cx         21*/
!                                                                               !/*cx         22*/
!        define free variable ranges.                                           !/*cx         23*/
      do 40 i=1,nfree                                                           !/*cx         24*/
      k=ifree(i)                                                                !/*cx         25*/
      kbin(k,1)=1                                                               !/*cx         26*/
      kbin(k,2)=iptal(lipt+k,4,mtal)-1                                          !/*cx         27*/
      if(k.le.2)kbin(k,2)=iptal(lipt+k,3,mtal)                                  !/*cx         28*/
      npt(i)=kbin(k,2)-kbin(k,1)+1                                              !/*cx         29*/
      if(npt(i).ge.2)go to 40                                                   !/*cx         30*/
      write(jtty,30)                                                            !/*cx         31*/
   30 format(51h number of points to plot is less than 2.  no plot.)            !/*cx         32*/
      if(koplot.eq.1)koplot=0                                                   !/*cx4a        2*/
      inform=1                                                                  !/*cx         33*/
      return                                                                    !/*cx         34*/
   40 continue                                                                  !/*cx         35*/
!                                                                               !/*cx         36*/
!        set default bin numbers for fixed variables.                           !/*cx         37*/
      do 60 k=1,8                                                               !/*cx         38*/
      if(ifree(1).eq.k.or.ifree(nfree).eq.k)go to 60                            !/*cx         39*/
      kbin(k,1)=min(kbin(k,1),iptal(lipt+k,3,mtal))                             !/*cx         40*/
      if(kbin(k,1).ne.0)go to 50                                                !/*cx         41*/
      kbin(k,1)=1                                                               !/*cx         42*/
      if(k.gt.2.and.iptal(lipt+k,3,mtal).eq.iptal(lipt+k,4,mtal)-               !/*cx4c        1*/
     & iptal(lipt+k,6,mtal))kbin(k,1)=iptal(lipt+k,3,mtal)                      !/*cx4c        2*/
   50 kbin(k,2)=kbin(k,1)                                                       !/*cx         45*/
   60 continue                                                                  !/*cx         46*/
      if(nfree.eq.2)go to 130                                                   !/*cx         47*/
!                                                                               !/*cx         48*/
!        default to linlog except linlin for tfc or kcode.                      !/*cx         49*/
   70 if(lax.ne.0)go to 80                                                      !/*cx         50*/
      lax=2                                                                     !/*cx         51*/
      if(itfc.ne.0)lax=1                                                        !/*cx         52*/
      if(itfc.eq.7.or.itfc.eq.8)lax=3                                           !/*cx4a        3*/
      if(itfc.eq.6.or.itfc.ge.9.and.itfc.le.25)lax=4                            !/*cx4a        4*/
!                                                                               !/*cx         53*/
!        check that user-specified limits for log axes are positive.            !/*cx         54*/
   80 if((jlim(1).eq.0.or.lax.lt.3.or.xyzmn(1).gt.0.).and.                      !/*cx         55*/
     & (jlim(2).eq.0.or.abs(lax-3).ne.1.or.xyzmn(2).gt.0.))go to 100            !/*cx         56*/
      write(jtty,90)                                                            !/*cx         57*/
   90 format(34h non-positive limit on a log axis.)                             !/*cx         58*/
      inform=1                                                                  !/*cx         59*/
      return                                                                    !/*cx         60*/
!                                                                               !/*cx         61*/
!        no histogram for other than cosine, energy, time or some tfcs.         !/*cx4a        5*/
  100 if(ifree(1).ge.6.and.kurv.eq.0)kurv=1                                     !/*cx         63*/
      if(kurv.eq.0)kurv=2                                                       !/*cx         64*/
      if(kurv.ne.1)go to 120                                                    !/*cx         65*/
      if(ifree(1).lt.6.or.itfc.ne.0)go to 110                                   !/*cx         66*/
      if(ktfile.eq.1.or.iptal(lipt+ifree(1),2,mtal).eq.0)go to 120              !/*cx         67*/
  110 kurv=2                                                                    !/*cx         68*/
      if(itfc.ge.6.and.itfc.le.25)kurv=1                                        !/*cx4a        6*/
!                                                                               !/*cx         69*/
!        define the number of points for tfc and kcode.                         !/*cx         70*/
  120 if(itfc.gt.0.and.itfc.le.5)npt(1)=20                                      !/*cx4a        7*/
      if(itfc.ge.6.and.itfc.le.25)npt(1)=nhsd(lnhs+nsp+4,                       !/*cx4b        1*/
     & mtal+lpert*ntal)-nhsd(lnhs+nsp+3,mtal+lpert*ntal)+1                      !/*cx4b        2*/
      if(itfc.eq.9)npt(1)=30                                                    !/*cx4a       10*/
      if(itfc.gt.30)npt(1)=min(mrkp,kcz)                                        !/*cx4a       11*/
!                                                                               !/*cx         73*/
!        set up the main title and the labels for the axes.                     !/*cx         74*/
  130 call pltitl                                                               !/*cx         75*/
      if(ititle(1).eq.0)call axlabl(1)                                          !/*cx         76*/
      if(ititle(2).eq.0)call axlabl(2)                                          !/*cx         77*/
      if(nfree.eq.2.and.j3d.ne.0.and.ititle(3).eq.0)call axlabl(3)              !/*cx         78*/
#endif                                                                          /*!cxx         8*/
      return                                                                    !/*cx         79*/
      end                                                                       !/*cx         80*/

!#ident axlabl.F from deck ak                                                   !/*ak          1*/
      subroutine axlabl(m)                                                      !/*ak          3*/
#ifdef MCPLOT                                                                   /*!akx         1*/
!        define the label for an axis:  m=1 x, m=2 y, m=3 z                     !/*ak          4*/
      use global_data                                                           !/*akx         2*/
      use mcplot_data                                                           !/*akx         3*/
!                                                                               !/*akx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*akx         5*/
      implicit integer(kindi) (i-n)                                             !/*akx         6*/
!                                                                               !/*akx         7*/
      character hc(25)*18                                                       !/*ak4a        1*/
      data hc/'k (collision)','k (absorption)','k (trk length)',                !/*ak          8*/
     & 'pr rem life (col)','pr rem life (abs)','mean',                          !/*ak4b        1*/
     & 'error','fom','vov','tally pdf slope','tally density',                   !/*ak4b        2*/
     & 'cumulative number','cumulative tally','tail tally density',             !/*ak4b        3*/
     & 'first moment','second moment','third moment','fourth moment',           !/*ak4b        4*/
     & 'fifth moment','sixth moment','seventh moment','eighth moment',          !/*ak4b        5*/
     & 'tally frequency','tail tally number','k(col/abs/trk len)'/              !/*ak4a        7*/
!                                                                               !/*ak         10*/
!        tally plot x-axis label (or x-y axes for 3d plot.)                     !/*ak4b        6*/
      if(m.eq.3.or.nfree.eq.1.and.m.eq.2)go to 10                               !/*ak         11*/
      if(ifree(m).lt.6)titles(m)=ilbl(ifree(m))//' bin number'                  !/*ak         12*/
#ifdef RADIOG                                                                   /*!akx         8*/
      if(ifree(m).eq.4.and.jptal(ljpt+2,mtal).eq.5.and.                         !/*akx         9*/
     & jptal(ljpt+8,mtal).gt.2)titles(m)='s-axis  (cm)'                         !/*akx        10*/
#endif                                                                          /*!akx        11*/
      if(ifree(m).eq.6)titles(m)=ilbl(ifree(m))                                 !/*ak         13*/
#ifdef RADIOG                                                                   /*!akx        12*/
      if(ifree(m).eq.6.and.jptal(ljpt+2,mtal).eq.5.and.                         !/*akx        13*/
     & jptal(ljpt+8,mtal).gt.2)titles(m)='t-axis  (cm)'                         !/*akx        14*/
#endif                                                                          /*!akx        15*/
      if(ifree(m).eq.7)titles(m)=ilbl(ifree(m))//' (mev)'                       !/*ak         14*/
      if(ifree(m).eq.8)titles(m)=ilbl(ifree(m))//' (shakes)'                    !/*ak         15*/
      if(itfc.ne.0)titles(m)='nps'                                              !/*ak         16*/
      if(itfc.ge.6.and.itfc.le.25)titles(m)='nonzero tally'                     !/*ak4a        8*/
      if(itfc.gt.30)titles(m)='keff cycle number'                               !/*ak4a        9*/
      if(itfc.eq.47)titles(m)='number of inactive keff cycles'                  !/*ak4a       10*/
      if(itfc.eq.60)titles(m)='keff(use)'                                       !/*ak4c        1*/
      if(itfc.eq.66)titles(m)='k(col)'                                          !/*ak4c        5*/
      if(itfc.eq.67)titles(m)='k(abs)'                                          !/*ak4c        6*/
      if(itfc.eq.68)titles(m)='k(trk)'                                          !/*ak4c        7*/
      return                                                                    !/*ak         18*/
!                                                                               !/*ak         19*/
!        tally plot y-axis label (or z-axis for 3d plot.)                       !/*ak4b        7*/
   10 if(kxsplt.ne.0)go to 40                                                   !/*ak4b        8*/
      titles(m)='tally/particle'                                                !/*ak4b        9*/
!ccc                                                                            !/*ak4c        9*/
!                                                                               !/*akx        22*/
      if (nonorm.eq.0.or.jptal(ljpt+8,mtal).eq.5) then                          !/*akx        23*/
#ifdef RADIOG                                                                   /*!akx        24*/
         if(jptal(ljpt+2,mtal).eq.5.and.(ifree(1).eq.4.or.                      !/*akx        25*/
     &    ifree(1).eq.6))titles(m)='tally/cm**2/particle'                       !/*akx        26*/
         if(jptal(ljpt+2,mtal).ne.5.and.ifree(1).eq.6)                          !/*akx        27*/
     &    titles(m)='tally/steradian/particle'                                  !/*akx        28*/
#else                                                                           /*!akx        29*/
         if(ifree(1).eq.6)titles(m)='tally/steradian/particle'                  !/*akx        30*/
#endif                                                                          /*!akx        31*/
         if(ifree(1).eq.7)titles(m)='tally/mev/particle'                        !/*akx        32*/
         if(ifree(1).eq.8)titles(m)='tally/shake/particle'                      !/*akx        33*/
      endif                                                                     !/*akx        34*/
!                                                                               !/*akx        35*/
      if(itfc.eq.0)return                                                       !/*akx        36*/
      if(itfc.gt.30)go to 30                                                    !/*ak4a       12*/
      if(itfc.le.9)titles(m)=hc(itfc+5)                                         !/*ak4a       13*/
      if(itfc.ge.10.and.itfc.le.17.and.nonorm.eq.0)                             !/*ak4a       14*/
     & titles(m)='f(x)*x**n '//hc(itfc+5)                                       !/*ak4a       15*/
      if(itfc.ge.10.and.itfc.le.17.and.nonorm.ne.0)                             !/*ak4a       16*/
     & titles(m)='binned f*x**n '//hc(itfc+5)                                   !/*ak4a       17*/
      if(itfc.ge.18.and.itfc.le.25)titles(m)='cumulative '//hc(itfc-3)          !/*ak4a       18*/
      if(itfc.eq.6.and.nonorm.ne.0)titles(m)=hc(23)                             !/*ak4a       19*/
      if(itfc.eq.9.and.nonorm.ne.0)titles(m)=hc(24)                             !/*ak4a       20*/
      return                                                                    !/*ak4a       21*/
   30 if(itfc.le.35)titles(m)=hc(itfc-30)                                       !/*ak4a       22*/
      if(itfc.le.40)return                                                      !/*ak4a       23*/
      if(itfc.le.45)titles(m)='avg '//hc(itfc-40)                               !/*ak4a       24*/
      if(itfc.gt.45)titles(m)='avg '//hc(25)                                    !/*ak4a       25*/
      if(itfc.eq.48)titles(m)=hc(25)//' fom'                                    !/*ak4a       26*/
      if(itfc.eq.49)titles(m)=hc(25)//' relative error'                         !/*ak4a       27*/
      return                                                                    !/*ak4b       10*/
!                                                                               !/*ak4b       11*/
!        cross section plot y-axis label (units).                               !/*ak4b       12*/
   40 do 50 j=jlbl(1,kxsptp),jlbl(2,kxsptp)                                     !/*ak4b       13*/
   50 if(kxspmt.eq.klbl(j))go to 60                                             !/*ak4b       14*/
      j=jlbl(2,kxsptp)                                                          !/*ak4b       15*/
   60 titles(m)=hxspu(kxspu(j))                                                 !/*ak4b       16*/
#endif                                                                          /*!akx        41*/
      return                                                                    !/*ak         29*/
      end                                                                       !/*ak         30*/

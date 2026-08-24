!#ident pltscr.F from deck puu4c2                                               !/*puu4c2      1*/
      subroutine pltscr                                                         !/*puu4c2      3*/
#ifdef PLOT                                                                     /*!puux        1*/
!        put data from plot screen in klin.                                     !/*puu4c2      4*/
      use global_data                                                           !/*puux        2*/
      use plot_data                                                             !/*puux        3*/
!                                                                               !/*puux        4*/
      implicit real(kindr) (a-h,o-z)                                            !/*puux        5*/
      implicit integer(kindi) (i-n)                                             !/*puux        6*/
!                                                                               !/*puux        7*/
      real s1,s2,t1,t2                                                          !/*puu4c2      7*/
      character ha*10,hs*80                                                     !/*puu4c2      8*/
!                                                                               !/*puu4c2      9*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+5))                              !/*puu4c2     10*/
      hs='Enter Data>                 <'                                        !/*puu4c2     11*/
      call gtx(real(-1.82*extent(1)),real(-extent(2)),hs)                       !/*puu4c2     12*/
      if(mcolor.le.2)call guwk(1,0)                                             !/*puu4c2     13*/
      hs=' '                                                                    !/*puu4c2     14*/
      klin=' '                                                                  !/*puu4c2     15*/
      s1=-1.5*extent(1)                                                         !/*puu4c2     16*/
      t1=-extent(2)                                                             !/*puu4c2     17*/
      s2=-extent(1)                                                             !/*puu4c2     18*/
      t2=t1+.04*extent(2)                                                       !/*puu4c2     19*/
      call ginst(1,1,0,' ',1,s1,s2,t1,t2,29,1,hs)                               !/*puu4c2     20*/
      call grqst(1,1,ie,nn,klin)                                                !/*puu4c2     21*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*puu4c2     22*/
#endif                                                                          /*!puux        8*/
      return                                                                    !/*puu4c2     23*/
      end                                                                       !/*puu4c2     24*/
